library(tidyr)
library(rwhatsapp)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(lubridate)

file_name <- '/home/cksidharthan/Documents/Programming/R Programming/whatsapp-chat-analysis/bias_chat.txt'
chat_tbl <- rwa_read(file_name)

# Data Cleaning
chat_tbl <- na.omit(chat_tbl)
chat_tbl <- chat_tbl %>% select(time, author , text, emoji, emoji_name)
chat_tbl$hours <- format(as.POSIXct(chat_tbl$`time`, "%Y-%m-%d %H:%M:%S", tz = ""), format = "%H:%M")
chat_tbl$dates <- format(as.Date(chat_tbl$`time`,"%Y-%m-%d"), format = "%d/%m/%Y")
chat_tbl$day <- weekdays(as.POSIXct(chat_tbl$`dates`), abbreviate = F)
chat_tbl$month <- month.abb[month(as.Date(chat_tbl$`dates`))]

# Graph 1 - Messages Over Time
date_msg_count_tbl <- chat_tbl %>% select(time) %>% group_by(time) %>% mutate(count = n())
date_msg_count_tbl <- date_msg_count_tbl[order(date_msg_count_tbl$time),]
msg_over_time <- plot_ly(x = ~date_msg_count_tbl$time, y = ~date_msg_count_tbl$count, mode = 'lines', type = 'scatter')
msg_over_time %>% layout(title = "Number of Messages Over time",
                        xaxis = list(title = "Date"), yaxis = list (title = "Number of Messages"))

hrs_msg_tbl <- chat_tbl %>% select(hours) %>% group_by(hours) %>% mutate(count = n()) %>% sort(hours, decreasing = F)
plot_ly(x = ~hrs_msg_tbl$hours, y = ~hrs_msg_tbl$count, type = 'bar') %>%
  layout(title = "Number of Messages Over time", xaxis = list(title = "Date"), yaxis = list (title = "Number of Messages"))

# Graph 2 - Messages During days of Week
library(viridis)
library(hrbrthemes)

day_msg_tbl <- chat_tbl %>% select(day, month) %>% group_by(day) %>% mutate(count = n())

Noax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

# Need to add labels to this plot
plot_ly(x = day_msg_tbl$count, y = day_msg_tbl$day, type = 'bar', orientation = 'h') %>% layout(xaxis = Noax)

# Stacked Plot
ggplot(day_msg_tbl, aes(fill=month, y=count, x=day)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Messages over days of week") +
  theme_ipsum() + geom_text(aes(label=count)) + 
  xlab("") + ylab('')

# Most used words Analysis - Wordcloud
library(tidytext)
library(stringr)
library(textdata)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(tm)

text_corpus <- iconv(chat_tbl$text, to = "utf-8")
text_corpus <- Corpus(VectorSource(text_corpus))
text_corpus_cleaned <- tm_map(text_corpus, stripWhitespace)
text_corpus_cleaned <- tm_map(text_corpus_cleaned, tolower)
text_corpus_cleaned <- tm_map(text_corpus_cleaned, removePunctuation)
text_corpus_cleaned <- tm_map(text_corpus_cleaned, removeNumbers)
text_corpus_cleaned <- tm_map(text_corpus_cleaned, removeWords, stopwords("english"))
msg_text_matrix <- TermDocumentMatrix(text_corpus_cleaned)
msg_text_matrix <- as.matrix(msg_text_matrix)
text_words_count <- rowSums(msg_text_matrix)
text_words_count <- subset(text_words_count, text_words_count > 5)
wordcloud(words = names(text_words_count), freq = text_words_count, max.words = 100, random.order = T, min.freq = 10,
          colors = brewer.pal(8, "Dark2"), scale = c(4,1), rot.per = 0.3)

# Another Wordcloud
# text_token_tbl <- chat_tbl %>% select(text) %>% unnest_tokens(word, text)
# chat_word_count_tbl <- text_token_tbl  %>% count(word, sort = T)

text_words_count_df <- as.data.frame(as.table(text_words_count))
colnames(text_words_count_df) <- c('word', 'count')
wordcloud2(data=text_words_count_df, size=1.5, color='random-dark')

# Sentiment Analysis
chat_sentiment = get_nrc_sentiment(chat_tbl$text)
colSums(chat_sentiment)
barplot(colSums(chat_sentiment), las = 2, col = rainbow(10), ylab = "Count", main = "Sentiment Scores for Chat")

# Wordcloud - Messages by users
name_corpus <- iconv(chat_tbl$author, to = "utf-8")
name_corpus <- Corpus(VectorSource(name_corpus))
name_corpus_cleaned <- tm_map(name_corpus, stripWhitespace)
name_corpus_cleaned <- tm_map(name_corpus, removePunctuation)
name_corpus_cleaned <- tm_map(name_corpus_cleaned, tolower)
name_corpus_cleaned <- tm_map(name_corpus_cleaned, removeNumbers)
# Add words to below line to remove the words from the corpus
name_corpus_cleaned <- tm_map(name_corpus_cleaned, removeWords, c("bias", "ucc", 'crb'))
name_matrix <- TermDocumentMatrix(name_corpus_cleaned)
name_matrix <- as.matrix(name_matrix)
name_count <- rowSums(name_matrix)

wordcloud(words = names(name_count), freq = name_count, max.words = 50, random.order = F, min.freq = 40,
          colors = brewer.pal(8, "Dark2"), scale = c(4,2), rot.per = 0.2)

barplot(name_count, las = 2, col = rainbow(10), ylab = "Count", main = "Number of Messages by Users")

# Need to add
# Number of people removed from group
# Number of people added group
# Number of Emojis
