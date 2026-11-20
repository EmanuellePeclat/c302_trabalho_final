library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)

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

tabela_DT <- tabela_descritiva |>
  filter(n_obs > 0) |>
  mutate(across(c(media, dp, min, q1, mediana, q3, max), ~ round(.x, 2)))

anos_min <- min(comidas_bebidas$Ano, na.rm = TRUE)
anos_max <- max(comidas_bebidas$Ano, na.rm = TRUE)
decadas  <- sort(unique(comidas_bebidas$Decada))

ggplot()

# ===================== Shiny ========================

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "KPIs"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Visão Geral e Décadas",
        tabName = "aba1_visao_geral",
        icon = icon("chart-line")
      )
    )
  ),
  dashboardBody(
    tags$style(HTML("
      #kpi_periodo .small-box {
        background-color: #ffffff !important;
        color: #222222 !important;
        border: 1px solid #d2d6de;
      }
      #kpi_periodo .small-box .icon-large {
        color: rgba(0, 0, 0, 0.15) !important;
      }
      #tabela_dados_brutos table.dataTable thead th,
      #tabela_DT table.dataTable thead th {
        background-color: #00a65a !important;
        color: #ffffff !important;
      }
      .btn-download-espaco {
        margin-bottom: 12px;
      }
    ")),
    tabItems(
      tabItem(
        tabName = "aba1_visao_geral",
        fluidRow(
          valueBoxOutput("kpi_linhas", width = 4),
          valueBoxOutput("kpi_periodo", width = 4),
          valueBoxOutput("kpi_variaveis", width = 4)
        ),
        fluidRow(
          box(
            title = "Base traduzida e convertida (comidas_bebidas)",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            div(
              class = "btn-download-espaco",
              downloadButton("baixar_comidas_bebidas", "Baixar comidas_bebidas (.csv)", class = "btn-success")
            ),
            DTOutput("tabela_dados_brutos")
          ),
          box(
            title = "Estatística descritiva por década e produto",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            div(
              class = "btn-download-espaco",
              downloadButton("baixar_descritiva", "Baixar estatística descritiva (.csv)", class = "btn-success")
            ),
            DTOutput("tabela_DT")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
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
  
  output$tabela_dados_brutos <- renderDT({
    cols_com_na <- c("Ano", variaveis_prod)
    
    render_na_bruto_js <- JS(
      "function(data, type, row, meta) {",
      "  if (type !== 'display') return data;",
      "  if (data === null || data === undefined || data === '') {",
      "    return 'NA';",
      "  }",
      "  if (typeof data === 'number' && Number.isNaN(data)) {",
      "    return 'NA';",
      "  }",
      "  return data;",
      "}"
    )
    
    targets_idx <- which(names(comidas_bebidas) %in% cols_com_na) - 1
    
    datatable(
      comidas_bebidas,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = targets_idx, render = render_na_bruto_js)
        )
      )
    )
  })
  
  output$tabela_DT <- renderDT({
    datatable(
      tabela_DT,
      rownames = FALSE,
      colnames = c(
        "Década", "Variável", "N", "NAs", "Média", "DP",
        "Mín", "Q1", "Mediana", "Q3", "Máx"
      ),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$baixar_comidas_bebidas <- downloadHandler(
    filename = function() {
      paste0("comidas_bebidas_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(comidas_bebidas, file)
    }
  )
  
  output$baixar_descritiva <- downloadHandler(
    filename = function() {
      paste0("estatistica_descritiva_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(tabela_DT, file)
    }
  )
}

shinyApp(ui = ui, server = server)
