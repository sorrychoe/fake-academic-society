library(tidyverse)
library(readr)
library(tidytext)

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


waset |> 
  group_by(`Institution`, `저자`) |> 
  count() |> 
  arrange(desc(n)) |> 
  head(20) |> 
  ggplot(aes(x = n, y = reorder (`저자`, n), fill = `Institution`)) +
  geom_col() +
  xlab("")+
  ylab("")+
  theme(legend.position = "none")

waset |> 
  filter(`중복` == 0) |> 
  group_by(`저자`) |> 
  count() |> 
  arrange(desc(n)) |> 
  filter(n >= 4) |> 
  ggplot(aes(x=reorder(`저자`,n), y=n, fill=`저자`)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none")+
  xlab("") +
  ylab("")

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

text |> 
  count(word, sort=TRUE) |> 
  head(30) |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(x = n, y = word, fill = word)) +
  geom_col() +
  xlab("")+
  ylab("")+
  theme(legend.position = "none")
