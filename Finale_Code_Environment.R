
XR_1500 <- read.csv("~/Documents/Edinburgh/Big Data and Psych/XR_1500.csv", comment.char="#")
View(XR_1500)
F4F_1000 <- read.csv("~/Documents/Edinburgh/Big Data and Psych/F4F_1000.csv", comment.char="#")
View(F4F_1000)


# load all packages
install.packages ("xaringanthemer")
install.packages ("tidyverse")
install.packages ("patchwork")
install.packages ("ggmosaic")
install.packages ("kableExtra")
install.packages ("knitr")
install.packages ("performance")
install.packages ("see")
install.packages ("tidytext")
install.packages ("tidyverse")
install.packages ("stringr")
install.packages ("pandoc")
install.packages ("gutenbergr")
install.packages ("stringr")
install.packages ("topicmodels")
install.packages ("tm")
install.packages ("topicmodels")
library (topicmodels)
library(xaringanthemer)
library(tidyverse)
library(patchwork)
library(ggmosaic)
library(kableExtra)
library(knitr)
library(performance)
library(see)
library(tidytext)
library(janeaustenr)
library(tidyverse)
library(stringr)
library(pandoc)
library(gutenbergr)
library(tidyr)
library(dplyr)
library(stringr)
library(tm)


# CREATING FULL DATAFRAMES AND DATA CLEANING
# since each subreddit only has around 1000 posts each, comments will also be considered as posts
# comments were originally separated by '," ",' ',' or "," and merged together in one column per row 
#(i.e., all comments per post together)
# now,comments will be divided into separate rows and then added to the original posts to create bigger dataset


#FRIDAY'S FOR FUTURE (F4F)
#add comments column to normal posts

F4F_comments_df <- F4F_1000 %>%
  select(comments, id)
NewF4F2 <- F4F_comments_df %>%
  mutate(comments_post = strsplit(comments, "', \"|\", '|\",\"|', '", fixed = FALSE)) %>%
  unnest(comments_post) %>%
  select(id, comments, comments_post)
#removing empty rows, no entries are indicated by "[ ]"
NewF4F2 <- NewF4F2 %>%
  filter_all(all_vars(!grepl("\\[\\]", as.character(.)))) #observations decrease from 2453 to 2050
# remove the comments columns (which is actually the original text column) so there are only comments left 
NewF4F3 <- NewF4F2 %>%
  select(-comments)
#add subreddit column
F4F_comments <- NewF4F3 %>%
  mutate(subreddit = "FridaysForFuture")
#rename the comments_post column so the comments can be merged with the original posts
F4F_comments <- F4F_comments %>%
  rename(title = comments_post)
# merge both dataframes
merged_F4F2 <- bind_rows(
  F4F_comments %>% select(id, title, subreddit),
  F4F_1000 %>% select(id, title, subreddit)) #2959 entries 
#add body entries to normal posts
body_df_F4F <- data.frame(title = F4F_1000$body)
# remove empty rows or those that are filled with blank space
body_df_F4F <- body_df_F4F[!(apply(body_df_F4F, 1, function(row) all(is.na(row) | row == ""))), , drop = FALSE]
#add subreddits column
body_F4F_comments <- body_df_F4F %>%
  mutate(subreddit = "FridaysForFuture")
merged_F4F2 <- bind_rows(
  F4F_comments %>% select(id, title, subreddit),
  F4F_1000 %>% select(id, title, subreddit),
  body_F4F_comments %>% select (title, subreddit)) #3142 entries 

# CLEANING 
#remove columns that say [removed] [deleted]
merged_F4F2 <- merged_F4F2 %>%
  filter(!grepl("\\[removed\\]|\\[deleted\\]", title)) #now, 2895 observations
#make all letters lower case
merged_F4F2 <- merged_F4F2 %>%
  mutate(title = tolower(title))
# remove links 
merged_F4F2 <- merged_F4F2 %>%
  mutate(title = gsub("https?://\\S+|www\\.\\S+", "", title))
#removing all backslashes
# Remove backslashes from the "title" column
merged_F4F2$title <- gsub("\\", "", merged_F4F2$title, fixed = TRUE)
# Separate "nn" from other words and then delete "nn" in the "title" column
merged_F4F2$title <- gsub("([a-zA-Z])nn([a-zA-Z])", "\\1 \\2", merged_F4F2$title)
merged_F4F2$title <- gsub("nn", "", merged_F4F2$title)
#remove blank space
merged_F4F2 <- merged_F4F2 %>%
  mutate(title = str_squish(title))
#removing punctuation except single quotes (to not accidentally merge don't to dont)
merged_F4F2$title <- str_replace_all(merged_F4F2$title, "[[:punct:]&&[^']]", "")



# EXTINCTION REBELLION (XR)
XR_comments_df <- XR_1500 %>%
  select (comments, id)
NewXR <- XR_comments_df %>%
  mutate(comments_post = strsplit(comments, "', \"|\", '|\",\"|', '", fixed = FALSE)) %>%
  unnest(comments_post) %>%
  select(id, comments, comments_post)
# remove the comments columns (which is actually the original text column) so there are only comments left 
NewXR3 <- NewXR2 %>%
  select(-comments)
#add subreddit column
XR_comments <- NewXR3 %>%
  mutate(subreddit = "ExtinctionRebellion")
#rename the comments_post column so the comments can be merged with the original posts
XR_comments <- XR_comments %>%
  rename(title = comments_post)
# merge both dataframes
merged_XR <- bind_rows(
  XR_comments %>% select (id, title, subreddit),
  XR_1500 %>% select (id, title, subreddit))
#add body entries to normal posts
body_df_XR <- data.frame(title = XR_1500$body)
# remove empty rows or those that are filled with blank space
body_df_XR <- body_df_XR[!(apply(body_df_XR, 1, function(row) all(is.na(row) | row == ""))), , drop = FALSE]
#add subreddits column
body_XR_comments <- body_df_XR %>%
  mutate(subreddit = "ExtinctionRebellion")
merged_XR <- bind_rows(
  XR_comments %>% select(id, title, subreddit),
  XR_1500 %>% select(id, title, subreddit),
  body_XR_comments %>% select (title, subreddit)) #3142 entries 



# CLEANING 
#remove columns that say [removed] [deleted]
merged_XR <- merged_XR %>%
  filter(!grepl("\\[removed\\]|\\[deleted\\]", title)) #now, from 8210 observations to 7912
#make all letters lower case
merged_XR <- merged_XR %>%
  mutate(title = tolower(title))
# remove links 
merged_XR <- merged_XR %>%
  mutate(title = gsub("https?://\\S+|www\\.\\S+", "", title))
#removing all backslashes
merged_XR$title <- gsub("\\", "", merged_XR$title, fixed = TRUE)
# Separate "nn" from other words and then delete "nn" in the "title" column
merged_XR$title <- gsub("([a-zA-Z])nn([a-zA-Z])", "\\1 \\2", merged_XR$title)
merged_XR$title <- gsub("nn", "", merged_XR$title)
#remove blank space
merged_XR <- merged_XR %>%
  mutate(title = str_squish(title))
#removing punctuation except single quotes (to not accidentally merge don't to dont)
merged_XR$title <- str_replace_all(merged_XR$title, "[[:punct:]&&[^']]", "")
#remove empty rows
merged_XR2 <- merged_XR %>% filter(!is.na(title) & title != "") #removed data from 7912 to 7628


#merge both into one dataframe
merged_df <- bind_rows (
  merged_XR2 %>% select (id, title, subreddit),
  merged_F4F2 %>% select (id, title, subreddit))


# 1. Find most frequent words 
#F4F
# #1 unnest tokens
unnested_F4F <- merged_F4F2 %>%
  unnest_tokens(words,title)
unnested_F4F
# removing stop words for interesting output 
data(stop_words)
# rename words to word 
stop_words <- stop_words %>%
  rename("words" = "word")
unnested_F4F2 <- unnested_F4F %>%
  anti_join(stop_words)
unnested_F4F2 %>%
  unnest(words) %>%
  group_by(words) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(desc(count))




# graphical illustration

unnested_F4F2 %>%
  unnest(words) %>%
  group_by(words) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 90) %>%
  ggplot(aes(x = reorder(words, n), y = n)) +
  geom_col(fill = "lightgreen") +
  ggtitle("FridaysforFuture") +
  xlab(NULL) +
  coord_flip()



# XR
unnested_XR <- merged_XR2 %>%
  unnest_tokens(words,title)
# remove stop words
unnested_XR2 <- unnested_XR %>%
  anti_join(stop_words)
unnested_XR2 %>%
  unnest(words) %>%
  group_by(words) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(desc(count)) #170054 observations

#plot most frequent words 
unnested_XR2%>%
  unnest(words) %>%
  group_by(words) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 300) %>%
  ggplot(aes(x = reorder(words, n), y = n)) +
  geom_col(fill = "darkgreen") +
  ggtitle("ExtinctionRebellion") +
  xlab(NULL) +
  coord_flip()



#MERGING INTO ONE DATAFRAME
merged_df <- bind_rows (
  merged_XR2 %>% select (id, title, subreddit),
  merged_F4F2 %>% select (id, title, subreddit))
#assign unique ID to each entry
merged_df <- merged_df %>%
  mutate(unique_id = row_number())


# TOPIC MODELLING
# this code was inspired by 
#https://www.youtube.com/watch?v=3ozjwHWf-6E

merged_df_new <- merged_df%>%
  select (unique_id, title)

#adding dont to stopwords
new_stopword <- data.frame(words = "dont", lexicon = "SMART")
stop_words <- rbind(stop_words, new_stopword)
#make stopwords into vector so they can be removed
custom_stopwords <- stop_words$words
# Remove custom stop words
document <- Corpus(VectorSource(merged_df_new))
document <- tm_map(document, content_transformer(tolower))
document <- tm_map(document, removeNumbers)
document <- tm_map(document, removeWords, custom_stopwords)
document <- tm_map(document, removePunctuation, preserve_intra_word_dashes= TRUE)
document <- tm_map(document,stripWhitespace)

DTM <-DocumentTermMatrix(document)
# Remove documents with all zero entries
install.packages ("matrixStats")
library (matrixStats)
non_empty_docs <- rowSums(as.matrix(DTM)) > 0
DTM <- DTM[non_empty_docs, ]


# Create LDA model
Model_lda <- LDA(DTM, k = 2, control = list(seed = 1234))
Model_lda

beta_topics <- tidy(Model_lda,matrix = "beta")
beta_topics

library (broom)
#group by beta
beta_top_terms <- beta_topics%>%
  group_by(topic)%>%
  slice_max(beta, n=10)%>%
  ungroup()%>%
  arrange (topic, -beta)
beta_top_terms%>%
  mutate (term =reorder_within(term, beta, topic))%>%
  ggplot(aes(beta, term, fill= factor(topic))) +
  geom_col(show.legend= FALSE)+
  facet_wrap(~topic, scales= "free") + 
  scale_y_reordered()


# Sentiment Analysis 
# the following code was guided by https://www.youtube.com/watch?v=mQLXR_LaGes
#XR
library (tidyverse)
library (syuzhet)
library (tidytext)
#analyze sentiments based on syuzhet sentiment dictionary
emotions <- get_nrc_sentiment(merged_XR2$title)
emo_bar <- colSums(emotions)
emo_bar
emo_sum <- data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum
#barplot showing counts for each of eight different emotions and positive/negative rating
ggplot(emo_sum, aes(x = reorder(emotion, -count), y = count)) +
  geom_bar(stat = 'identity') +
  labs(x = "Emotion", y = "Count", title = "XR Emotion Distribution")


#now sentiment analysis using bing lexicon
bing_word_counts <- merged_XR2 %>%
  unnest_tokens(output = words, input = title) %>%
  ungroup() %>%
  inner_join(get_sentiments("bing"), by = c("words" = "word")) %>%
  group_by(words, sentiment) %>%
  summarize(count = n(), .groups = "drop", .by_group = TRUE) %>%
  arrange(desc(count))



# select top 10 words by sentiment 
bing_top_10_words_by_sentiment <- bing_word_counts %>%
  group_by(sentiment) %>%
  arrange(desc(count)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(words, count))


#barplot showing contribution of words to sentiment
bing_top_10_words_by_sentiment %>%
  ggplot(aes(word, count, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip() +
  ggtitle("Extinction Rebellion")


#F4F
emotions_F4F <- get_nrc_sentiment(merged_F4F2$title)
emo_bar_F4F <- colSums(emotions_F4F)
emo_bar_F4F
emo_sum_F4F <- data.frame(count=emo_bar_F4F, emotion=names(emo_bar_F4F))
emo_sum_F4F
#barplot showing counts for each of eight different emotions and positive/negative rating
ggplot (emo_sum_F4F, aes(x=reorder (emotion, -count), y=count))+
  geom_bar(stat='identity') +
  labs(x = "Emotion", y = "Count", title = "F4F Emotion Distribution")

#now sentiment analysis using bing lexicon
bing_word_counts_F4F <- merged_F4F2%>%unnest_tokens (output=words, input = title)%>%
  inner_join (get_sentiments("bing")) %>%
  count (words,sentiment, sort=TRUE)

# select top 10 words by sentiment 
bing_top_10_words_by_sentiment_F4F <- bing_word_counts_F4F %>%
  group_by (sentiment)%>%
  slice_max (order_by=n, n =10)%>%
  ungroup%>%
  mutate (word=reorder(word,n))
bing_top_10_words_by_sentiment_F4F

#barplot showing contribution of words to sentiment
bing_top_10_words_by_sentiment_F4F%>%
  ggplot(aes(word,n,fill=sentiment)) +
  geom_col(show.legend=FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs (y="Contribution to sentiment", x=NULL) +
  coord_flip()+
  ggtitle("Fridays for Future")




# for machine learning, create new dataframe with same number of entries for XR and F4F
# to avoid overfitting to XR



# reduce observations from merged_XR to 3100
# randomly select observations chosen
reduced_merged_XR <- merged_XR2 %>%
  slice(sample(1:n(), 3100))
#merge into new total df
machine_total_df <- bind_rows (
  reduced_merged_XR %>% select (id, title, subreddit),
  merged_F4F2 %>% select (id, title, subreddit))
machine_total_df <- machine_total_df %>%
  mutate(unique_id = row_number())

machine_total_df %>%
  ggplot(aes(subreddit)) +
  geom_bar()
table(machine_total_df$subreddit) #20 observations that belong to neither XR nor F4F
filtered_machine_total_df <- machine_total_df %>%
  filter(tolower(subreddit) %in% c("fridaysforfuture", "extinctionrebellion"))
table(filtered_machine_total_df$subreddit) 


environment_data_counts <- map_df(1:2, #Get both unigrams and bigrams by using map_df function
                      ~ unnest_tokens(filtered_machine_total_df, words, title, 
                                      token = "ngrams", n = .x))%>% #map_df is a function you can apply to whole dataframe, this case applied to unnest_tokens to the data frame (data_clean), column is called word, take words from text, token: indicate you want both unigrams and bigrams 
  anti_join(stop_words, by = "words")%>% # here it is counted up and sorted
  count(unique_id, words, sort = TRUE)

words_12 <- environment_data_counts %>% #words with at least 10 tweets bc of sparcity 
  group_by(words) %>%
  summarise(n = n()) %>% 
  filter(n >= 12) %>%
  select(words)


environment_dtm <- environment_data_counts %>%
  right_join(words_12, by = "words")%>% #words_10 is the dataframe
  bind_tf_idf(words, unique_id, n) %>%
  cast_dtm(unique_id, words, tf_idf)%>% #tf_idf is what is in the columns # cast_dtm makes into document matrix
  na.omit()


#create meta for disappeared posts
meta <- tibble(unique_id = as.numeric(dimnames(environment_dtm)[[1]])) %>%
  left_join(filtered_machine_total_df[!duplicated(filtered_machine_total_df$unique_id), ], by = "unique_id")

#creating training and testdata
library (caret)
set.seed(100)
trainIndex <- createDataPartition(meta$subreddit, p = 0.8, list = FALSE, times = 1)
#using createdatapartition function
#training set with 80 percent of the data 


data_df_train <- environment_dtm[trainIndex, ] %>% 
  as.matrix() %>% #first make it a matrix then make it a data frame
  as.data.frame()


data_df_test <- environment_dtm[-trainIndex, ] %>% 
  as.matrix() %>% 
  as.data.frame()

response_train <- meta$subreddit[trainIndex]

df_f_train <- cbind(response_train, data_df_train)

df_f_train <- df_f_train %>%
  na.omit()

train.control <- trainControl(method = "repeatedcv", #repeated cross-validation
                              n = 10, #10-folds
                              repeats = 3,
                              search = "grid", #search grid,
                              allowParallel = TRUE)

library(kernlab)

#Note we've seperated our predictors and dv into separate data frames. This is commmon when we have thousands of predictors!
svm_mod <- train(y = df_f_train[,1], x = df_f_train[, -1],
                 method = "svmLinearWeights2", #svmLinearweights2 is non-linear polynomal for sparse datasets
                 trControl = train.control,
                 tuneGrid = data.frame(cost = 0.01,  #tuneGrid specifies the number chosen
                                       Loss = 0, 
                                       weight = seq(0.5, 1.5, 0.1))) #between 0.5 and 1.5 with 0.1 incremental steps
svm_mod
confusionMatrix(svm_mod)
plot(svm_mod)
