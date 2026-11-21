library(shiny)
library(bslib)      # layout bonito (barra lateral, cartões, caixas de valor)
library(tidyverse)

# =====================================================================
# PARTE 1 — DADOS (roda uma vez, quando o app abre)
# =====================================================================

# 1. Ler os dados
food_beverages <- read_csv("dados/food_beverages.csv")
textiles       <- read_csv("dados/textiles.csv")
transport      <- read_csv("dados/transport.csv")

# 2. Passar cada tabela para o formato longo e marcar o setor
food_longo <- food_beverages |>
  pivot_longer(-Year, names_to = "produto", values_to = "producao") |>
  mutate(setor = "Alimentos")

textiles_longo <- textiles |>
  pivot_longer(-Year, names_to = "produto", values_to = "producao") |>
  mutate(setor = "Têxteis")

transport_longo <- transport |>
  pivot_longer(-Year, names_to = "produto", values_to = "producao") |>
  mutate(setor = "Transporte")

# 3. Juntar as três tabelas numa só
dados <- bind_rows(food_longo, textiles_longo, transport_longo)

# 4. Tirar os anos sem dado e as colunas de total
dados <- dados |>
  filter(!is.na(producao)) |>
  filter(produto != "Total_yarn", produto != "Total_textiles")

# Uma cor fixa para cada setor (assim a cor não muda quando filtramos)
cores <- c("Alimentos" = "#D98B2B", "Têxteis" = "#7A5BA6", "Transporte" = "#2E7D9A")


# =====================================================================
# PARTE 2 — UI: a "cara" do app (o que aparece na tela)
# =====================================================================

ui <- page_sidebar(
  title = "Indústria italiana, Periodo 1861–1985",
  fillable = FALSE,   # deixa a página rolar

  # Cor principal do tema (usada nos checkboxes e no slider)
  theme = bs_theme(primary = "#1F2A3C"),

  # Liga o nosso arquivo de estilo (fica na pasta www/)
  tags$head(tags$link(rel = "stylesheet", href = "estilo.css")),

  # Barra lateral com os filtros
  sidebar = sidebar(
    checkboxGroupInput(
      inputId  = "setores",
      label    = "Setores:",
      choices  = c("Alimentos", "Têxteis", "Transporte"),
      selected = c("Alimentos", "Têxteis", "Transporte")
    ),
    sliderInput(
      inputId = "anos",
      label   = "Período:",
      min = 1861, max = 1985,
      value = c(1861, 1985),
      sep = ""            # mostra 1861, e não 1,861
    )
  ),

  # Linha 1: período e setores
  layout_columns(
    value_box(
      title = "Período",
      value = textOutput("caixa_periodo"),
      theme = "dark"
    ),
    value_box(
      title = "Setores",
      value = textOutput("caixa_setores"),
      theme = "dark"
    )
  ),

  # Linha 1b: um card para cada setor
  layout_columns(
    value_box(
      title = "Produtos de Alimentos",
      value = textOutput("caixa_alimentos"),
      textOutput("lista_alimentos"),          # nomes dos produtos (texto menor)
      theme = value_box_theme(bg = cores["Alimentos"], fg = "white")
    ),
    value_box(
      title = "Produtos Têxteis",
      value = textOutput("caixa_texteis"),
      textOutput("lista_texteis"),
      theme = value_box_theme(bg = cores["Têxteis"], fg = "white")
    ),
    value_box(
      title = "Produtos de Transporte",
      value = textOutput("caixa_transporte"),
      textOutput("lista_transporte"),
      theme = value_box_theme(bg = cores["Transporte"], fg = "white")
    )
  ),

  # Linha 2: gráfico de linhas
  card(
    card_header("A produção cresceu em cada setor?"),
    plotOutput("grafico_linhas", height = "350px")
  ),

  # Linha 3: dois gráficos lado a lado
  layout_columns(
    card(
      card_header("Desde quando cada produto é registrado?"),
      plotOutput("grafico_linha_tempo", height = "450px")
    ),
    card(
      card_header("Quais produtos mais cresceram? (último ano ÷ primeiro ano)"),
      plotOutput("grafico_crescimento", height = "450px")
    )
  ),

  # Linha 4: uma aba para cada setor, com cada produto no seu gráfico
  navset_card_tab(
    title = "Cada produto, separado por setor",
    nav_panel("Alimentos",  plotOutput("grafico_alimentos",  height = "500px")),
    nav_panel("Têxteis",    plotOutput("grafico_texteis",    height = "500px")),
    nav_panel("Transporte", plotOutput("grafico_transporte", height = "500px"))
  )
)


# =====================================================================
# PARTE 3 — SERVER: os cálculos e gráficos (refeitos a cada filtro)
# =====================================================================

server <- function(input, output, session) {

  # Dados filtrados pelo que a pessoa escolheu na barra lateral
  dados_filtrados <- reactive({
    validate(need(length(input$setores) > 0, "Escolha pelo menos um setor."))

    dados |>
      filter(setor %in% input$setores) |>
      filter(Year >= input$anos[1], Year <= input$anos[2])
  })

  # Resumo: uma linha por produto
  resumo <- reactive({
    dados_filtrados() |>
      group_by(setor, produto) |>
      summarise(
        inicio = min(Year),
        fim = max(Year),
        primeiro_valor = first(producao),
        ultimo_valor = last(producao),
        .groups = "drop"   # desfaz o agrupamento (evita aviso no console)
      ) |>
      mutate(crescimento = ultimo_valor / primeiro_valor)
  })

  # --- Cards de cima ---
  output$caixa_periodo <- renderText(paste(input$anos[1], "–", input$anos[2]))
  output$caixa_setores <- renderText(length(input$setores))   # quantos setores estão marcados

  # Produtos de UM setor que têm dado no período escolhido
  produtos_do_setor <- function(nome_setor) {
    dados |>
      filter(setor == nome_setor) |>
      filter(Year >= input$anos[1], Year <= input$anos[2]) |>
      distinct(produto) |>
      pull(produto)
  }

  # Número de produtos de cada setor
  output$caixa_alimentos  <- renderText(length(produtos_do_setor("Alimentos")))
  output$caixa_texteis    <- renderText(length(produtos_do_setor("Têxteis")))
  output$caixa_transporte <- renderText(length(produtos_do_setor("Transporte")))

  # Nomes dos produtos de cada setor, separados por vírgula
  output$lista_alimentos  <- renderText(paste(produtos_do_setor("Alimentos"),  collapse = ", "))
  output$lista_texteis    <- renderText(paste(produtos_do_setor("Têxteis"),    collapse = ", "))
  output$lista_transporte <- renderText(paste(produtos_do_setor("Transporte"), collapse = ", "))

  # --- Gráfico de linhas ---
  output$grafico_linhas <- renderPlot({
    ggplot(dados_filtrados(), aes(x = Year, y = producao, group = produto, color = setor)) +
      geom_line() +
      facet_wrap(~ setor) +
      scale_y_log10() +
      scale_color_manual(values = cores) +
      labs(x = "Ano", y = "Produção (escala log)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })

  # --- Linha do tempo ---
  output$grafico_linha_tempo <- renderPlot({
    ggplot(resumo(), aes(y = reorder(produto, -inicio), color = setor)) +
      geom_segment(aes(x = inicio, xend = fim, yend = produto), linewidth = 3) +
      scale_color_manual(values = cores) +
      labs(x = "Ano", y = NULL, color = "Setor") +
      theme_minimal(base_size = 14)
  })

  # --- Crescimento ---
  output$grafico_crescimento <- renderPlot({
    ggplot(resumo(), aes(x = crescimento, y = reorder(produto, crescimento), fill = setor)) +
      geom_col() +
      scale_x_log10() +
      scale_fill_manual(values = cores) +
      labs(x = "Quantas vezes cresceu (escala log)", y = NULL, fill = "Setor") +
      theme_minimal(base_size = 14)
  })

  # --- Abas por setor ---
  # Uma função que desenha os produtos de UM setor.
  # Assim escrevemos o gráfico uma vez e usamos 3 vezes.
  grafico_do_setor <- function(nome_setor) {
    dados |>
      filter(setor == nome_setor) |>
      filter(Year >= input$anos[1], Year <= input$anos[2]) |>
      ggplot(aes(x = Year, y = producao)) +
      geom_line(color = cores[nome_setor], linewidth = 0.8) +
      facet_wrap(~ produto, scales = "free_y") +   # cada produto com sua própria escala
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      labs(x = "Ano", y = "Produção") +
      theme_minimal(base_size = 13)
  }

  output$grafico_alimentos  <- renderPlot(grafico_do_setor("Alimentos"))
  output$grafico_texteis    <- renderPlot(grafico_do_setor("Têxteis"))
  output$grafico_transporte <- renderPlot(grafico_do_setor("Transporte"))
}


# =====================================================================
# PARTE 4 — Ligar a UI com o server
# =====================================================================
shinyApp(ui, server)
