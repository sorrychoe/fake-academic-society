library(tidyverse)
library(readr)

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


