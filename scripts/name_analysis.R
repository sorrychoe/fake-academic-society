library(tidyverse)
library(readxl)
library(readr)
library(tidytext)
library(wordcloud2)

data(stop_words)

omics <- read_excel("data/omics_20190314.xlsx", sheet = "data")
wrl <- read_excel("data/World Research Library_20180921.xlsx", sheet = "data")
waset <- read_csv("data/waset.csv")

##데이터 전처리====

waset.data <- waset |> 
  select(Institution, Title, `저자`) |>
  unnest_tokens(authors, `저자`,token = "regex", pattern = ",")

omics.data <- omics |> 
  select(`기관명`, `논문명`, `저자명`) |> 
  unnest_tokens(authors, `저자명`,token = "regex", pattern = ",")

wrl.data <- wrl |> 
  select(`기관명`, `논문명`, `저자명`) |> 
  unnest_tokens(authors, `저자명`,token = "regex", pattern = ",")


str_trim(waset.data$authors) -> waset.data$authors
str_trim(omics.data$authors) -> omics.data$authors
str_trim(wrl.data$authors) -> wrl.data$authors


omics.data$authors <- gsub('\'', "", omics.data$authors)
wrl.data$authors <- gsub('\'', "", wrl.data$authors)

colnames(omics.data) <- c("Institution", "Title", "authors")
colnames(wrl.data) <- c("Institution", "Title", "authors")

## World Report Library and omics====

rbind(wrl.data, omics.data) -> data

data |> 
  group_by(Institution, authors) |> 
  tally() |> 
  arrange(desc(n)) |> 
  head(30) |> 
  ggplot(aes(x=reorder(authors,n), y=n, fill=Institution)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(legend.position = "top") +
  theme(legend.title = element_text(size = 8, face = "bold")) +
  theme(legend.text = element_text(size = 6, face = "italic"))


data |>
  group_by(Institution) |>
  count() |> 
  arrange(desc(n)) |> 
  head(20) |>
  ggplot(aes(x = n, y = reorder(Institution, n), fill = Institution)) + 
  geom_col()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")

## 전체 단어 분석 ====

all.data <- rbind(data, waset.data)

tokens <- tibble(text=all.data$Title) |> 
  unnest_tokens(word,text) 

text <- tokens %>%
  anti_join(stop_words)

words <- text |> 
  group_by(word) |> 
  tally() |> 
  arrange(desc(n))

wordcloud2(words)
