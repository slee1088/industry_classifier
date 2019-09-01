

library(readr)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(gmodels)

set.seed(1234501)

# Windows-1252 or CP-1252 (code page 1252) is a single-byte character encoding of the Latin alphabet, 
# used by default in the legacy components of Microsoft Windows for English and some other Western languages (other languages use different default encodings).
data <- read_csv("seek_australia_sample.csv",locale = readr::locale(encoding = "windows-1252"))

data <- data %>% 
  select(job_description,category) %>%
  filter(!is.na(job_description)) %>%
  mutate(job_description=substring(job_description,1,2000))


categories <- data %>%
  group_by(category) %>%
  tally() %>%
  arrange(desc(n))

data_new <- data %>%
  mutate(flag=ifelse(category %in% c("Banking & Financial Services"),"Yes","No")) %>%
  mutate(no_chars=nchar(job_description))

as.character(data_new$job_description[[1]])

descriptions_corpus <- VCorpus(VectorSource(data_new$job_description))



descriptions_corpus_clean <- tm_map(descriptions_corpus,content_transformer(tolower))
descriptions_corpus_clean <- tm_map(descriptions_corpus_clean, removeNumbers)
descriptions_corpus_clean <- tm_map(descriptions_corpus_clean,removeWords, stopwords())
descriptions_corpus_clean <- tm_map(descriptions_corpus_clean, removePunctuation)
# descriptions_corpus_clean <- tm_map(descriptions_corpus_clean, stemDocument)
descriptions_corpus_clean <- tm_map(descriptions_corpus_clean, stripWhitespace)

inspect(descriptions_corpus_clean[1:2])

as.character(descriptions_corpus_clean[[1]])

descriptions_dtm <- DocumentTermMatrix(descriptions_corpus_clean)


 
train_sample <- sample(4725,3780)

descriptions_dtm_train <- descriptions_dtm[train_sample,]
descriptions_dtm_test <- descriptions_dtm[-train_sample,]

descriptions_train_labels <- data_flagged[train_sample,]$flag
descriptions_test_labels <- data_flagged[-train_sample,]$flag

prop.table(table(descriptions_train_labels))
prop.table(table(descriptions_test_labels))


# wordcloud(descriptions_corpus_clean, min.freq = 50, random.order = FALSE)


yes <- subset(data_new,flag=="Yes")
no <- subset(data_new,flag=="No")

# wordcloud(yes$job_description,max.words = 100,scale = c(3, 0.5),random.order = FALSE)
# wordcloud(no$job_description,max.words = 100,scale = c(3, 0.5),random.order = FALSE)

descriptions_freq_words <- findFreqTerms(descriptions_dtm_train, 50)[2:630]


descriptions_dtm_freq_train <- descriptions_dtm_train[ , descriptions_freq_words]
descriptions_dtm_freq_test <- descriptions_dtm_test[ , descriptions_freq_words]


convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

descriptions_train <- as.data.frame(apply(descriptions_dtm_freq_train, MARGIN = 2,
                   convert_counts))

descriptions_test <- as.data.frame(apply(descriptions_dtm_freq_test, MARGIN = 2,
                            convert_counts))

library(e1071)

descriptions_classifier <- naiveBayes(descriptions_train, as.factor(descriptions_train_labels),laplace=1)

descriptions_test_pred <- predict(descriptions_classifier, descriptions_test)



CrossTable(descriptions_test_pred, descriptions_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))



