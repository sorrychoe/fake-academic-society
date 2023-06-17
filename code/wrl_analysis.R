library(tidyverse)
library(readxl)
library(tidytext)

wrl <- read_excel("data/World Research Library_20180921.xlsx", sheet = "data")

summary(wrl)

wrl |>
  group_by(`기관명`) |>
  count() |> 
  arrange(desc(n)) |> 
  head(20) |>
  ggplot(aes(x = n, y = reorder(기관명,n), fill = 기관명)) + 
  geom_col()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")

wrl |>
  group_by(`기관명`, `저자명`) |>
  count() |> 
  arrange(desc(n)) |> 
  head(10) |>
  ggplot(aes(x = n, y = reorder(저자명,n), fill = 기관명)) + 
  geom_col()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")


##====

wrl |> 
  select(`논문명`, `기관명`, `저자명`) |> 
  unnest_tokens(authors, `저자명`,token = "regex", pattern = ",") -> name.saparate

str_trim(name.saparate$authors) -> name.saparate$authors

name.saparate |> 
  group_by(`기관명`, authors) |> 
  tally() |> 
  arrange(desc(n)) |>
  head(30) |> 
  ggplot(aes(x=reorder(authors,n), y=n, fill=`기관명`)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(legend.position = "top") +
  theme(legend.title = element_text(size = 8, face = "bold")) +
  theme(legend.text = element_text(size = 6, face = "italic"))

##====

tokens <- tibble(text=wrl$논문명) |> 
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
