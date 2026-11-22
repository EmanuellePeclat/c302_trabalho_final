library(shiny)
library(shinydashboard)
library(fresh)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(DT)

# Lendo os dados
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/textiles.csv"
if (!file.exists("textiles.csv")) download.file(url, "textiles.csv", mode = "wb", quiet = TRUE)
tex <- read_csv("textiles.csv", show_col_types = FALSE) %>% arrange(Year)

# Periodos de guerra
guerras <- tribble(
  ~nome, ~ini, ~fim, ~cor,
  "Guerra de Independência", 1866, 1866, "#8e7cc3",
  "Guerra Ítalo-Turca", 1911, 1912, "#e6a23c",
  "1ª Guerra Mundial", 1915, 1918, "#c0392b",
  "Guerra Ítalo-Etíope", 1935, 1936, "#d4a017",
  "2ª Guerra Mundial", 1940, 1945, "#7f1d1d"
) %>%
  mutate(periodo = ifelse(ini == fim, as.character(ini), paste0(ini, "-", fim)))

# marca as guerras nos graficos
bg_guerra <- function(p, ini, fim, show) {
  if (!show) return(p)

  shapes <- list()
  achou <- FALSE

  for (i in seq_len(nrow(guerras))) {
    g <- guerras[i, ]
    if (g$fim < ini || g$ini > fim) next
    achou <- TRUE

    x0 <- max(g$ini, ini) - 0.5
    x1 <- min(g$fim, fim) + 0.5

    # fundo
    shapes[[length(shapes) + 1]] <- list(
      type = "rect", xref = "x", x0 = x0, x1 = x1,
      yref = "paper", y0 = 0, y1 = 1,
      fillcolor = grDevices::adjustcolor(g$cor, alpha.f = 0.1),
      line = list(width = 0), layer = "below"
    )

    # faixa do topo (mostra o nome da guerra no hover)
    p <- p %>% add_trace(
      x = c(x0, x1, x1, x0, x0), y = c(0.93, 0.93, 1, 1, 0.93),
      yaxis = "y3", inherit = FALSE, type = "scatter", mode = "lines",
      fill = "toself", fillcolor = g$cor, line = list(width = 0),
      hoveron = "fills", name = g$nome,
      text = paste0("<b>", g$nome, "</b><br>", g$periodo), hoverinfo = "text",
      hoverlabel = list(bgcolor = g$cor, font = list(color = "white")),
      showlegend = FALSE
    )
  }

  if (!achou) return(p)

  p %>% layout(
    shapes = shapes,
    yaxis3 = list(overlaying = "y", range = c(0, 1), visible = FALSE, fixedrange = TRUE)
  )
}

# --- UI ---
ui <- dashboardPage(
  skin = "black",
  header = dashboardHeader(title = "Indústria Italiana", titleWidth = 260),
  sidebar = dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "tabs",
      menuItem("Indústria & Guerras", tabName = "industria", icon = icon("industry")),
      menuItem("Fibras & Substituição", tabName = "fibras", icon = icon("leaf"))
    ),
    div(style = "padding: 20px 15px;",
        sliderInput("anos", "Período:", min = min(tex$Year), max = max(tex$Year),
                    value = c(1890, max(tex$Year)), step = 1, sep = "", ticks = FALSE),
        checkboxInput("marcar_guerras", "Destacar guerras", TRUE),
        radioButtons("metrica_principal", "Métrica:",
                     choices = c("Fio e Tecido" = "ambos", "Fio" = "fio",
                                 "Tecido" = "tecido", "Seda" = "seda"),
                     selected = "ambos"),
        p(style = "color: #888; font-size: 11px; margin-top: 20px;", "Fonte: ISTAT / TidyTuesday")
    )
  ),
  body = dashboardBody(
    use_theme(create_theme(
      adminlte_color(light_blue = "#343a40"),
      adminlte_sidebar(dark_bg = "#1a1d21", dark_hover_bg = "#2b3035", dark_color = "#f8f9fa")
    )),
    tags$head(tags$style(HTML(".content-wrapper { background: #f4f6f9; } .box { border-radius: 6px; }
      .tabela-scroll { max-height: 420px; overflow: auto; }
      .tabela-scroll table { font-size: 12px; white-space: nowrap; }
      .tabela-scroll th { position: sticky; top: 0; background: #fff; z-index: 1; }"))),

    tabItems(
      tabItem(tabName = "industria",
        fluidRow(
          valueBoxOutput("vb_prod_fim", width = 4),
          valueBoxOutput("vb_maior_queda", width = 4),
          valueBoxOutput("vb_correlacao", width = 4)
        ),
        fluidRow(
          box(title = "Evolução da Produção", width = 8, status = "primary",
              plotlyOutput("p_producao", height = "350px")),
          box(title = "Variação Anual", width = 4, status = "danger",
              plotlyOutput("p_variacao", height = "350px"))
        ),
        fluidRow(
          box(title = "Eficiência (Tecido/Fio)", width = 6, status = "success",
              plotlyOutput("p_eficiencia", height = "330px")),
          box(title = "Queda e Recuperação", width = 6, status = "warning",
              plotlyOutput("p_indice_guerra", height = "330px"))
        ),
        fluidRow(
          box(title = "Dados do CSV (textiles.csv)", width = 12, status = "primary",
              div(class = "tabela-scroll", tableOutput("tabela_dt")))
        )
      ),
      tabItem(tabName = "fibras",
        fluidRow(
          valueBoxOutput("vb_algodao", width = 6),
          valueBoxOutput("vb_seda", width = 6)
        ),
        fluidRow(
          box(title = "Composição do Fio", width = 6, status = "info",
              plotlyOutput("p_composicao", height = "400px")),
          box(title = "Algodão x Seda", width = 6, status = "primary",
              plotlyOutput("p_seda_algodao", height = "400px"))
        )
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {

  d <- reactive({
    tex %>% filter(Year >= input$anos[1], Year <= input$anos[2])
  })

  # tabela
  output$tabela_dt <- renderTable({
    d() %>% mutate(Year = as.character(Year))
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
     digits = 0, na = "-", width = "100%",
     format.args = list(big.mark = ".", decimal.mark = ","))

  # aba industria
  output$vb_prod_fim <- renderValueBox({
    x <- d() %>% filter(!is.na(Total_yarn)) %>% arrange(Year)
    valueBox(paste0(round(tail(x$Total_yarn, 1)/1000), " mil t"),
             paste0("Fio total em ", tail(x$Year, 1)), icon = icon("chart-bar"), color = "blue")
  })

  output$vb_maior_queda <- renderValueBox({
    x <- d() %>% filter(!is.na(Total_yarn)) %>% arrange(Year) %>%
      mutate(v = (Total_yarn/lag(Total_yarn)-1)*100, cons = Year-lag(Year)==1) %>% filter(cons)
    pior <- x[which.min(x$v), ]
    valueBox(paste0(round(pior$v), "%"), paste0("Maior queda (", pior$Year, ")"),
             icon = icon("arrow-down"), color = "red")
  })

  output$vb_correlacao <- renderValueBox({
    x <- d() %>% filter(!is.na(Total_yarn), !is.na(Total_textiles))
    r <- cor(x$Total_yarn, x$Total_textiles, use = "complete.obs")
    valueBox(round(r, 2), "Correlação", icon = icon("link"), color = "green")
  })

  output$p_producao <- renderPlotly({
    x <- d()
    p <- plot_ly(x, x = ~Year)
    if (input$metrica_principal %in% c("ambos", "fio"))
      p <- p %>% add_lines(y = ~Total_yarn/1000, name = "Fio", line = list(color = "#2C3E50"))
    if (input$metrica_principal %in% c("ambos", "tecido"))
      p <- p %>% add_lines(y = ~Total_textiles/1000, name = "Tecido", line = list(color = "#18BC9C"))
    if (input$metrica_principal == "seda")
      p <- p %>% add_lines(y = ~Raw_silk, name = "Seda", line = list(color = "#7B5EA7"))

    p %>% layout(xaxis = list(title = ""), yaxis = list(title = "Volume"),
                 legend = list(orientation = "h", y = -0.2)) %>%
      bg_guerra(input$anos[1], input$anos[2], input$marcar_guerras) %>%
      config(displayModeBar = FALSE)
  })

  output$p_variacao <- renderPlotly({
    x <- d() %>% filter(!is.na(Total_yarn)) %>% arrange(Year) %>%
      mutate(v = (Total_yarn/lag(Total_yarn)-1)*100, cons = Year-lag(Year)==1) %>% filter(cons)
    cor_barra <- ifelse(x$v < 0, "#E74C3C", "#2ECC71")

    plot_ly(x, x = ~Year, y = ~v, type = "bar", marker = list(color = cor_barra)) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "%", range = c(-100, 100))) %>%
      bg_guerra(input$anos[1], input$anos[2], input$marcar_guerras) %>%
      config(displayModeBar = FALSE)
  })

  output$p_eficiencia <- renderPlotly({
    x <- d() %>% filter(!is.na(Total_yarn), Total_yarn > 0) %>% mutate(ef = Total_textiles/Total_yarn)
    plot_ly(x, x = ~Year, y = ~ef, type = "scatter", mode = "lines", line = list(color = "#E67E22")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Tecido / Fio", range = c(0, 1.8))) %>%
      bg_guerra(input$anos[1], input$anos[2], input$marcar_guerras) %>%
      config(displayModeBar = FALSE)
  })

  output$p_indice_guerra <- renderPlotly({
    x1 <- tex %>% filter(Year >= 1914, Year <= 1925) %>% mutate(idx = Total_yarn / tex$Total_yarn[tex$Year == 1914] * 100, g = "1ª Guerra")
    x2 <- tex %>% filter(Year >= 1939, Year <= 1950) %>% mutate(idx = Total_yarn / tex$Total_yarn[tex$Year == 1939] * 100, g = "2ª Guerra")
    x <- bind_rows(x1, x2) %>% filter(!is.na(idx))

    plot_ly(x, x = ~Year, y = ~idx, color = ~g, type = "scatter", mode = "lines+markers",
            colors = c("1ª Guerra" = "#c0392b", "2ª Guerra" = "#2980b9")) %>%
      layout(xaxis = list(title = "", range = c(1914, 1950)), yaxis = list(title = "Índice (Base 100)"),
             legend = list(orientation = "h", y = -0.2)) %>%
      bg_guerra(input$anos[1], input$anos[2], input$marcar_guerras) %>%
      config(displayModeBar = FALSE)
  })

  # aba fibras
  output$vb_algodao <- renderValueBox({
    x <- d() %>% filter(!is.na(Cotton_yarn), !is.na(Total_yarn))
    p <- x[which.max(x$Cotton_yarn/x$Total_yarn), ]
    valueBox(paste0(round(p$Cotton_yarn/p$Total_yarn*100), "%"),
             paste0("Pico do algodão (", p$Year, ")"), icon = icon("leaf"), color = "green")
  })

  output$vb_seda <- renderValueBox({
    x <- d() %>% filter(!is.na(Raw_silk))
    p <- x[which.max(x$Raw_silk), ]
    valueBox(paste0(round(p$Raw_silk), " t"), paste0("Pico da seda (", p$Year, ")"),
             icon = icon("scroll"), color = "purple")
  })

  output$p_composicao <- renderPlotly({
    x <- d() %>% filter(!is.na(Total_yarn), Total_yarn > 0) %>%
      transmute(Year, Algodão = Cotton_yarn/Total_yarn*100, Flock = Flock_yarn/Total_yarn*100, Outras = Other_yarn/Total_yarn*100) %>%
      pivot_longer(-Year, names_to = "Fibra", values_to = "Pct")

    plot_ly(x, x = ~Year, y = ~Pct, color = ~Fibra, type = "scatter", mode = "lines",
            stackgroup = "one", groupnorm = "percent",
            colors = c("Algodão" = "#2C3E50", "Flock" = "#E08E0B", "Outras" = "#A23B72")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "%", ticksuffix = "%"),
             legend = list(orientation = "h", y = -0.2)) %>%
      bg_guerra(input$anos[1], input$anos[2], input$marcar_guerras) %>%
      config(displayModeBar = FALSE)
  })

  output$p_seda_algodao <- renderPlotly({
    x <- d() %>% filter(!is.na(Cotton_yarn), !is.na(Raw_silk))
    plot_ly(x, x = ~Year) %>%
      add_lines(y = ~Cotton_yarn/1000, name = "Algodão", line = list(color = "#18BC9C"), yaxis = "y") %>%
      add_lines(y = ~Raw_silk, name = "Seda", line = list(color = "#7B5EA7", dash = "dot"), yaxis = "y2") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Algodão (mil t)"),
             yaxis2 = list(title = "Seda (t) log", overlaying = "y", side = "right", type = "log"),
             legend = list(orientation = "h", y = -0.2)) %>%
      bg_guerra(input$anos[1], input$anos[2], input$marcar_guerras) %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)