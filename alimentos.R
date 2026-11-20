
library(tidyverse)
library(dplyr)
library(ggplot2)

# Option 2: Read directly from GitHub

food_beverages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/food_beverages.csv')

#Tradução das colunas dos dados
comidas_bebidas <- food_beverages |> 
  rename(Ano = Year,
         Açúcar = Sugar,
         Glicose = Glucose,
         Substituto_café = Coffee_substitute,
         Oleo_de_sementes = Seed_oil,
         Etanol_1 = Ethyl_alcohol_1,
         Etanol_2 = Ethyl_alcohol_2,
         Cerveja = Beer) |> 
  mutate(Decada = paste0(floor(Ano /10)*10, "s")) |> 
  pivot_longer(
    cols = c(Açúcar, Glicose, Substituto_café, Oleo_de_sementes,
             Etanol_1, Etanol_2, Cerveja),
    names_to = "Variavel",
    values_to = "Valor") |>
  mutate(
    Aba = if_else(
      Variavel %in% c("Etanol_1", "Etanol_2", "Cerveja"),
      "Grupo 2", "Grupo 1")) |>
  relocate(Aba, Decada, Variavel)

#separação dos dados por aba
tabela_descritiva <- comidas_bebidas |>
  group_by(Aba, Decada, Variavel) |>
  summarise(
    n_obs   = sum(!is.na(Valor)),
    n_na    = sum(is.na(Valor)),
    media   = mean(Valor, na.rm = TRUE),
    dp      = sd(Valor, na.rm = TRUE),
    min     = min(Valor, na.rm = TRUE),
    q1      = quantile(Valor, probs = 0.25, na.rm = TRUE, names = FALSE),
    mediana = median(Valor, na.rm = TRUE),
    q3      = quantile(Valor, probs = 0.75, na.rm = TRUE, names = FALSE),
    max     = max(Valor, na.rm = TRUE),
    .groups = "drop") |>
  arrange(Aba, Decada, Variavel)

#plotar os dados para visualização
##



