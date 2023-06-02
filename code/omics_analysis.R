library(tidyverse)
library(readxl)

omics <- read_excel("data/omics_20190314.xlsx", sheet = "data")

summary(omics)

omics |>
  group_by(`기관명`) |>
  count() |> 
  arrange(desc(n)) |> 
  head(20) |>
  ggplot(aes(x = n, y = reorder(기관명,n), fill = 기관명)) + 
  geom_col()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")

