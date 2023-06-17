library(tidyverse)
library(readr)
library(tidytext)
library(wordcloud2)

waset <- read_csv("data/waset.csv")

summary(waset)

waset |>
  group_by(Institution) |>
  count() |>
  arrange(desc(n)) |>
  head(20) |>
  ggplot(aes(x = n, y = reorder(Institution,n), fill = Institution)) + 
  geom_col()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")

###=====
waset |> 
  select(Title, Institution, `저자`) |> 
  unnest_tokens(authors, `저자`,token = "regex", pattern = ",") -> name.saparate

name.saparate$authors <- gsub("and", "", name.saparate$authors)
str_trim(name.saparate$authors) -> name.saparate$authors

name.saparate |> 
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

###=====

tokens <- tibble(text=waset$Title) |> 
  unnest_tokens(word,text) 

data("stop_words")

text <- tokens %>%
  anti_join(stop_words)

words <- text |> 
  group_by(word) |> 
  tally() |> 
  arrange(desc(n))

wordcloud2(words)
