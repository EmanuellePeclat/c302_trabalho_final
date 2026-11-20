#Bibliotecas usadas

library(tidyverse)
library(dplyr)

#Dados diretos do GitHub: Tidytuesday 2026-19

food_beverages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/food_beverages.csv')

#Variáveis nas colunas do csv, usar para renomear
variaveis_prod<- c("Açúcar", "Glicose", "Substituto_café", "Oleo_de_sementes",
                   "Etanol_1", "Etanol_2", "Cerveja")

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
  mutate(Decada = paste0(floor(Ano /10)*10, "s")) 

#transformar em pivot_longer
comidas_bebidas_longer <- comidas_bebidas |>
  pivot_longer(cols = all_of(variaveis_prod),
               names_to = "Variavel",
               values_to = "Valor")


#estatistica descritiva
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
  arrange(Decada, Variavel)


anos_min <- min(comidas_bebidas$Ano, na.rm = TRUE)
anos_max <- max(comidas_bebidas$Ano, na.rm = TRUE)
decadas  <- sort(unique(comidas_bebidas$Decada))


#Juntar essas informçãoes no Shiny
library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "KPIs"),
  dashboardSidebar(
    menuItem(text = "Visão Geral e Décadas", tabName = "aba1_visao_geral", icon = icon("chart-line"))),
  dashboardBody(
    tabItem(tabName = "aba1_visao_geral",
      tags$style(HTML("
      #kpi_periodo .small-box {
        background-color: #ffffff !important;
        color: #222222 !important;
        border: 1px solid #d2d6de;
      }
      #kpi_periodo .small-box .icon-large {
        color: rgba(0, 0, 0, 0.15) !important;
      }
    ")),
    fluidRow(
      valueBoxOutput("kpi_linhas", width = 4),
      valueBoxOutput("kpi_periodo", width = 4),
      valueBoxOutput("kpi_variaveis", width = 4)
      )
    )
  )
)
server <- function(input, output){
  output$kpi_linhas <- renderValueBox({
    valueBox(
      value = format(nrow(food_beverages), big.mark = ".", decimal.mark = ","),
      subtitle = "Total de linhas na base",
      icon = icon("table"),
      color = "green"
    )
  })
  
  output$kpi_periodo <- renderValueBox({
    valueBox(
      value = paste0(anos_min, "-", anos_max),
      subtitle = paste0("Faixa de anos (", length(decadas), " décadas)"),
      icon = icon("calendar"),
      color = "olive"
    )
  })
  
  output$kpi_variaveis <- renderValueBox({
    valueBox(
      value = length(variaveis_prod),
      subtitle = "Variáveis analisadas",
      icon = icon("list"),
      color = "red"
    )
  })
  
}

shinyApp(ui = ui, server = server)



