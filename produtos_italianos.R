
library(tidyverse)

# Option 2: Read directly from GitHub

food_beverages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/food_beverages.csv')
textiles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/textiles.csv')
transport <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/transport.csv')


food_text_trans <- food_beverages |> 
  inner_join(textiles, by = "Year") |> 
  inner_join(transport, by = "Year")
