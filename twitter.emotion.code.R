# Twitter sentiment analysis

# packages

library(purrr)
library(plyr)
library(stringr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(readr)
library(rtweet)
library(tidytext)
library(wordcloud2)
library(stopwords)
library(coreNLP)
library("tm")
library(textdata)
library(ggpubr)
library("twtools")
library("cld3")
#Twitter Sentiment Analysis / NLP

# Import a sentiment lexicon.

Lexicon <- read_delim(file = "Turkish-tr-NRC-VAD-Lexicon.txt", "\t", 
                      locale = locale(date_names = "tr", encoding = "UTF-8"))

# Twitter API settings

token <- create_token(...)

# Get English Tweets from a bilingual twitter account.

tweets_bilingual <- get_timeline("yuksel_gunal", n = 5000, include_rts = FALSE)

# Cleaning the data

tweets_bilingual$text <- removeNumbers(tweets_bilingual$text)

tweets_bilingual$text <- clean_tweets(tweets_bilingual$text)

# Create Turkish stop words

stop_turkish <- data.frame(word = stopwords::stopwords("tr", source = "stopwords-iso"), stringsAsFactors = FALSE)

# Get a list of words

tweets_clean <- tweets_bilingual %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(stop_turkish)
  

# Rename the column  

tweets_clean %<>% rename(Word = word) 

# Match the words in the data with the sentiment lexicon for English

en_sentiment <- tweets_clean %>%
  inner_join(Lexicon) %>%
  count(Word, Arousal, Valence, Dominance, sort = TRUE) %>%
  ungroup()

# Prepare the Turkish Lexicon

Lexicon %<>% rename(turkishword = "Turkish-tr")
TR_Lexicon <- Lexicon %>% select(-Word)
TR_Lexicon %<>% rename(Word = turkishword)
TR_Lexicon = TR_Lexicon[!duplicated(TR_Lexicon$Word),]


# Match the words in the data with the sentiment lexicon for Turkish

tr_sentiment <- tweets_clean %>%
  inner_join(TR_Lexicon) %>%
  count(Word, Arousal, Valence, Dominance, sort = TRUE) %>%
  ungroup()


# Form a word cloud

wordcloud2(tr_sentiment, size = 0.30)
wordcloud2(en_sentiment, size = 0.30)

# Get a boxplot for Arousal and Valence

mean(en_sentiment$Valence)
mean(tr_sentiment$Valence)
  
boxplot(en_sentiment$Arousal, tr_sentiment$Arousal,
        main = "Arousal in Bilingual Twitter Account (1197 Tweets)",
        names = c("English", "Turkish"))

# Plotting a histogram for Valence

p1 <- en_sentiment %>% 
  ggplot(aes(Arousal)) +
  geom_histogram(binwidth = 0.015)

p2 <- tr_sentiment %>% 
  ggplot(aes(Arousal)) +
  geom_histogram(binwidth = 0.015)

# Merging two plots
p3 <- ggarrange(p1, p2, 
          labels = c("En","Tur"),
          ncol = 1, nrow = 2)

# Another cool plot

p4 <- tr_sentiment %>%
  subset(Word != "thanking") %>% 
  subset(Valence > 0.05) %>% 
  group_by(Word) %>%
  top_n(10) %>% 
  ungroup() %>%
  mutate(Word = reorder(Word, Valence)) %>%
  ggplot(aes(Word, Valence)) +
  geom_point(show.legend = FALSE) +
  labs(title = "Most Negative Words in Turkish",
       y = "Valence",
       x = NULL) +
  coord_flip()


p5 <- en_sentiment %>%
  subset(Valence < 0.03) %>% 
  group_by(Word) %>%
  top_n(10) %>% 
  ungroup() %>%
  mutate(Word = reorder(Word, Valence)) %>%
  ggplot(aes(Word, Valence)) +
  geom_point(show.legend = FALSE) +
  labs(title = "Most Negative Words in English",
       y = "Valence",
       x = NULL) +
  coord_flip()

p6 <-  ggarrange(p5, p4, 
                 labels = c("En","Tur"),
                 ncol = 1, nrow = 2)

# Other Twitter stuff

simit_df <- search_tweets("simit", lang = "tr", n = 2000, include_rts = FALSE) 
simit_df <- simit_df %>% filter(!location %in% c("","Ýstanbul", "Türkiye", "istanbul", "Ankara", "Turkey", "she/her", "Istanbul"))


p4 <- plotsimit <- simit_df %>% 
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(8) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Yer",
       y = "Sayý",
       title = "Simit")


gevrek_df <- search_tweets("gevrek", lang = "tr", n = 2000, include_rts = FALSE)
gevrek_df <- gevrek_df %>% filter(!location %in% c("Ýstanbul ","Türkiye","istanbul", "Ankara", "malatya","she/her", "Topraðýn altý. ",
                                                 "TÜRKÝYE KIZILELMA","Türkiye-Istanbul","Malatya","", "Ýzmir", "izmir", "uzayda bir yerde","Ýstanbul","44","10000 yýl önceki Arslantepe", "izmir türkiye"))
p5 <- plotgevrek <- gevrek_df %>% 
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(7) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Yer",
       y = "Sayý",
       title = "Gevrek")

timeline <- get_my_timeline()

user <- lookup_users("onr_kls")
tweets <- userTimeline("onr_kls", n=50)



# Get a Twitter Wordcloud of someone's timeline

onurdf <- get_timeline("onr_kls", n=50, exclude_hashtags = TRUE, include_rts = TRUE)

# Cleaning

onurdf$text <- removeNumbers(onurdf$text)

onurdf$text <- clean_tweets(onurdf$text)

# Unnesting

onurtable <- onurdf %>% 
  unnest_tokens(word, text)

# Removing stop words

onurtable <- onurtable %>% 
  anti_join(stop_words) %>% 
  anti_join(stop_turkish)

# Count 

onurtable <- onurtable %>%
  count(word, sort = TRUE)

        
# Create a word cloud from my tweets 

wordcloud2(onurtable, size=0.5)

