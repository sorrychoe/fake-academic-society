library(tidyverse)
library(readxl)

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
