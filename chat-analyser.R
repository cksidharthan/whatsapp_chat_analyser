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
msg_over_time <- msg_over_time %>% layout(title = "Number of Messages Over time",
                                          xaxis = list(title = "Date"),
                                          yaxis = list (title = "Number of Messages"))
msg_over_time

# Graph 2 - Messages During days of Week
library(viridis)
library(hrbrthemes)

day_msg_tbl <- chat_tbl %>% select(day, month) %>% group_by(day) %>% mutate(count = n())
ggplot(day_msg_tbl, aes(fill=month, y=count, x=day)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Messages over days of week") +
  theme_ipsum() + geom_text(aes(label=count)) +
  xlab("") + ylab('') + ylim(0, max(day_msg_tbl$count, na.rm = TRUE))
