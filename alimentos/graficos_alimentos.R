library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

  
#traduzir a library DT
options(DT.options = list(
  language = list(
    decimal = ",",
    thousands = ".",
    lengthMenu = "Mostrar _MENU_ registros por página",
    zeroRecords = "Nenhum registro encontrado",
    info = "Mostrando de _START_ até _END_ de _TOTAL_ registros",
    infoEmpty = "Mostrando 0 até 0 de 0 registros",
    infoFiltered = "(filtrado de _MAX_ registros no total)",
    search = "Buscar:",
    paginate = list(
      first = "Primeiro",
      last = "Último",
      `next` = "Próximo",
      previous = "Anterior"
    )
  )
))

# Dados diretos do GitHub: TidyTuesday 2026-19
food_beverages <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/food_beverages.csv",
  show_col_types = FALSE
)

# Variáveis nas colunas do csv, usadas para renomear
variaveis_prod <- c(
  "Açúcar", "Glicose", "Substituto_café", "Oleo_de_sementes",
  "Etanol_1", "Etanol_2", "Cerveja"
)

# Tradução das colunas e conversão de unidades:
# Glicose, Substituto_café e Oleo_de_sementes: quintal -> toneladas (x / 10)
comidas_bebidas <- food_beverages |>
  rename(
    Ano = Year,
    Açúcar = Sugar,
    Glicose = Glucose,
    Substituto_café = Coffee_substitute,
    Oleo_de_sementes = Seed_oil,
    Etanol_1 = Ethyl_alcohol_1,
    Etanol_2 = Ethyl_alcohol_2,
    Cerveja = Beer
  ) |>
  mutate(
    across(c(Glicose, Substituto_café, Oleo_de_sementes), ~ .x / 10)
  )

# Transformar em formato longo
comidas_bebidas_longer <- comidas_bebidas |>
  pivot_longer(
    cols = all_of(variaveis_prod),
    names_to = "Variavel",
    values_to = "Valor"
  ) |> 
  mutate(
    Decada = paste0(floor(Ano / 10) * 10, "s")
  )

# Estatística descritiva
cols_estat <- c("media", "dp", "min", "q1", "mediana", "q3", "max")

tabela_descritiva <- comidas_bebidas_longer |>
  group_by(Decada, Variavel) |>
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
    .groups = "drop"
  ) |>
  mutate(
    across(
      all_of(cols_estat),
      ~ if_else(is.nan(.x) | is.infinite(.x) | is.na(.x), NA_real_, .x)
    )
  ) |>
  arrange(Decada, Variavel)

#tabela no shiny
tabela_DT <- tabela_descritiva |>
  filter(n_obs > 0) |>
  mutate(across(c(media, dp, min, q1, mediana, q3, max), ~ round(.x, 2)))

anos_min <- min(comidas_bebidas_longer$Ano, na.rm = TRUE)
anos_max <- max(comidas_bebidas_longer$Ano, na.rm = TRUE)
decadas  <- sort(unique(na.omit(comidas_bebidas_longer$Decada)))

fmt_br <- function(x) {
  ifelse(
    is.na(x) | !is.finite(x),
    "NA",
    formatC(x, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
  )
}

