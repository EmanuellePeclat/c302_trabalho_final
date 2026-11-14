## c302_trabalho_final "Italian industrial production" 
## Dataseet : https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-05-05/readme.md
Projeto final da materia de elementos

esses são os dados para abrir no R, pode ser pela opção 1 ou 2
# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-05-05')
## OR
tuesdata <- tidytuesdayR::tt_load(2026, week = 18)

food_beverages <- tuesdata$food_beverages
textiles <- tuesdata$textiles
transport <- tuesdata$transport

# Option 2: Read directly from GitHub

food_beverages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/food_beverages.csv')
textiles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/textiles.csv')
transport <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/transport.csv')

# Para juntar as tabelas
library(tidyverse)

food_text_trans <- food_beverages |> 
  inner_join(textiles, by = "Year") |> 
  inner_join(transport, by = "Year")

# Using Python
# Option 1: pydytuesday python library
## pip install pydytuesday

import pydytuesday

# Download files from the week, which you can then read in locally
pydytuesday.get_date('2026-05-05')

# Criar um backup
por emergencia criar uma copia