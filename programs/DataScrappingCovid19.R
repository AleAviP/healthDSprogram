# ####Download packages###
# install.packages("tidyverse")
# install.packages("twitteR")
# install.packages("tidytext")
# install.packages("dplyr")
# install.packages("ggplot2")
# 
# ####Set up user###
# library(twitteR)
# 
# consumer_key <- ckey         #Add from your own Twitter accoung
# consumer_secret <- csecret   #Add from your own Twitter accoung
# access_token <- atoken       #Add from your own Twitter accoung
# access_secret <- asecret     #Add from your own Twitter accoung
# 
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

####Scrapping data###
library(tidyverse)
library(twitteR)
library(tidytext)
library(dplyr)
library(ggplot2)

# ####If you have the Twitter developed keys run this####
# fn_twitter <- searchTwitter("#Covid19",n=1000,lang="en")
# fn_twitter_df <- twListToDF(fn_twitter) #Convert to data frame

###### Get Twitter data url from Github######
urlfile<-'https://raw.githubusercontent.com/AleAviP/healthDSprogram/main/data/Twitter_Covid.RDS'

###### Read data and change format######
fn_twitter_df<-readRDS(url(urlfile))

tweet_words <- fn_twitter_df %>% select(id, text) %>% unnest_tokens(word,text)

####Plotting first words###
tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, 
                         n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity",fill="turquoise4") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) + xlab("")


####Plotting interesting words###
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", "rt", "amp","4yig9gzh5t","tn","sa",
                                "1","14","3","19","3.15","fyy2ceydhi","fakenews")))

tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words)

tweet_words_interesting %>% group_by(word) %>% 
  tally(sort=TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word,n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity",fill="turquoise4") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) + xlab("")

####Sentiment analysis###
nrc_word_counts <- tweet_words_interesting %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(word,sentiment,sort=TRUE)

nrc_word_counts %>% filter(!is.na(sentiment)) %>% 
  group_by(sentiment) %>% top_n(10) %>% summarise(n=n())

nrc_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  theme_minimal() +
  coord_flip()

data.s<-nrc_word_counts %>% filter(!is.na(sentiment)) %>% 
  group_by(sentiment) %>% top_n(10) %>% summarise(n=n())

ggplot(data.s,aes(x="", y=n, fill=sentiment)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()


bing_word_counts <- tweet_words_interesting %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word,sentiment,sort=TRUE)

data.s<-bing_word_counts %>% filter(!is.na(sentiment)) %>% 
  group_by(sentiment) %>% top_n(10) %>% summarise(n=n())

ggplot(data.s,aes(x="", y=n, fill=sentiment)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()
