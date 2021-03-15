# ####Download packages###
# install.packages("tidyverse")
# install.packages("twitteR")
# install.packages("tidytext")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("tm)
# 
# ####Set up user###
# Instrucciones para las llaves: https://bigcomputing.blogspot.com/
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
#fn_twitter <- searchTwitter("#8M",n=1000,lang="es")
fn_twitter <- searchTwitter("#8M",n=1000,lang="en")
fn_twitter_df <- twListToDF(fn_twitter) #Convert to data frame

tweet_words <- fn_twitter_df %>% select(id, text) %>% unnest_tokens(word,text)

####Plotting first words###
tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, 
                         n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity",fill="turquoise4") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) + xlab("")


####Plotting interesting words###
my_stop_words <-  stop_words %>% select(-lexicon) %>% bind_rows(stop_words,
                            data_frame(word = tm::stopwords("spanish"),
                                       lexicon = "custom")) %>% 
#  bind_rows(data.frame(word = c("https", "t.co", "rt", "amp","tn","así","sí","fbm7eplu8h")))
  bind_rows(data.frame(word = c("https", "t.co", "rt", "amp","tn","así","sí","fbm7eplu8h")))

tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words)

tweet_words_interesting %>% group_by(word) %>% 
  tally(sort=TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word,n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity",fill="turquoise4") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) + xlab("")

####Sentiment analysis###
df = tweet_words_interesting %>% count(word, sort=TRUE)

wordcloud::wordcloud(words = df$word, freq = df$n, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.1, 
          colors=RColorBrewer::brewer.pal(8, "Dark2"))
