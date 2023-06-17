library(tidyverse)
library(readxl)
library(readr)
library(tidytext)

omics <- read_excel("./data/omics_20190314.xlsx", sheet = "data")
wrl <- read_excel("./data/World Research Library_20180921.xlsx", sheet = "data")
waset <- read_csv("./data/waset.csv")

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

waset.data |> 
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
