library(tidyverse)
library(rtweet)
library(tidytext)
library(teamcolors)


wirfs = search_tweets('tristan wirfs AND draft OR NFL', n = 17999, type = 'recent', retryonratelimit = TRUE
                      ,include_rts = FALSE)

becton = search_tweets(c('mekhi becton AND draft OR NFL'), n = 17999, type = 'recent', retryonratelimit = TRUE
                       ,include_rts = FALSE)

thomas = search_tweets(c('andrew thomas AND draft OR NFL'), n = 17999, type = 'recent', retryonratelimit = TRUE
                       ,include_rts = FALSE)

jones = search_tweets(c('josh jones OR joshua jones AND draft OR NFL'), n = 17999, type = 'recent', retryonratelimit = TRUE
                      ,include_rts = FALSE)

wills = search_tweets(c('jedrick wills AND draft OR NFL'), n = 17999, type = 'recent', retryonratelimit = TRUE
                      ,include_rts = FALSE)

### wrangle data ####

wirfs = wirfs %>% 
  select(text) %>% 
  mutate(player = 'Tristan Wirfs'
         ,tweets = n()) %>% 
  rename('word' = 'text')

wills = wills %>% 
  select(text) %>% 
  mutate(player = 'Jedrick Wills'
         ,tweets = n()) %>% 
  rename('word' = 'text')

becton = becton %>% 
  select(text) %>% 
  mutate(player = 'Mekhi Becton'
         ,tweets = n()) %>% 
  rename('word' = 'text')

jones = jones %>% 
  select(text) %>% 
  mutate(player = 'Joshua Jones'
         ,tweets = n()) %>% 
  rename('word' = 'text')

thomas = thomas %>% 
  select(text) %>% 
  mutate(player = 'Andrew Thomas'
         ,tweets = n()) %>% 
  rename('word' = 'text')

players = bind_rows(becton, jones, thomas, wirfs, wills)

### tidy text processing ####

players = players %>% unnest_tokens(word, word, token = 'ngrams', n = 1)

players = players %>% anti_join(stop_words)

# remove non ascii characters #
players$word = iconv(players$word, "latin1", "ASCII", sub="")
players = subset(players, players$word != '')

# remove stop_words and custom_words #
players = players %>% anti_join(stop_words)
players$word = trimws(players$word)

#### summarize data ####

players_sentiment = players %>% inner_join(get_sentiments('afinn')) %>% 
  filter(value > 0) %>% 
  group_by(player) %>% 
  summarise(avg_sentiment = mean(value)
            ,sd_sentiment = sd(value)
            ,tweets = mean(tweets)) %>% 
  ungroup()

# players_pos_sentiment = players %>% inner_join(get_sentiments('afinn')) %>% filter(value > 0) %>%
#   group_by(player) %>% summarise(avg_sentiment = mean(value)
#                                        ,tot_sentiment = sum(value)
#                                       , count = n()) %>%
#   mutate(method = "AFINN") %>% 
#   ungroup()

## positive sentiment ##

plot = ggplot(data = players_sentiment, aes(x=reorder(player, avg_sentiment), y=avg_sentiment, group = player, fill = player)) +
  geom_bar(stat = 'identity')
  # geom_boxplot()

plot + 
  scale_fill_viridis_d() + 
  # geom_vline(xintercept = today_wk, linetype = "dashed", color = 'darkgray') +
  # geom_hline(yintercept = 0, linetype = 'dashed', color = 'black') +
  # geom_label(aes(x=today_wk-1.5, y=min_pos_sentiment+.15, label=today_wk), colour="darkgray", size=3.5, fontface = 'italic') +
  geom_text(aes(y = players_sentiment$avg_sentiment)
            , label = glue::glue('Avg = {round(players_sentiment$avg_sentiment,2)}\n'
                                ,'StDev = {round(players_sentiment$sd_sentiment,2)}\n'
                                 ,'({players_sentiment$tweets} tweets)')
            , hjust = -.1
            ,size = 3.25) +
  theme(panel.border = element_rect(colour = 'black', fill = 'transparent', size = .5)
        ,panel.background = element_blank()
        , plot.title = element_text(hjust = 0, size = 16)
        , plot.subtitle = element_text(hjust = 0)
        , axis.title.x = element_text(size = 12, face = 'italic' )
        ,axis.text.y = element_text(size = 14)
        , axis.title.y = element_text(size = 12, face = 'italic')
        , legend.position = "none", legend.text = element_text(size = 9), legend.title = element_blank()) +
  labs(title = "High-profile OT Prospects; 2020 NFL Combine", subtitle = "Positive Sentiment in Recent Tweets", 
       x = paste0("OT Prospect\n   "),
       y= "Positive Sentiment Score\n(Values from 0 to +5)"
       ,caption = 'Data: rtweet // Analysis: @brownalytics') +
  # scale_x_discrete(labels = rev(pos_sentiment$avg_sentiment)) +
  # annotation_custom(top_lt_anno) + annotation_custom(btm_lt_anno) +
  scale_y_continuous(limits = c(0,max(players_sentiment$avg_sentiment*1.5))) +
  coord_flip()
