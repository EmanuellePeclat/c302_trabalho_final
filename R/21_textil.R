library(tidyverse)

base <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/"
food  <- read_csv(paste0(base, "food_beverages.csv"))
text  <- read_csv(paste0(base, "textiles.csv"))
trans <- read_csv(paste0(base, "transport.csv"))

glimpse(food)    # colunas, tipos e primeiros valores
glimpse(text)
glimpse(trans)

summary(food)    # mínimo, máximo e quantidade de NAs por coluna
summary(text)
summary(trans)
