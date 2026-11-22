# =====================================================================
# PÁGINA ALIMENTOS  —  VERSÃO CONVERTIDA (para comparar)
# =====================================================================
# Este arquivo é o shiny_alimentos.R + graficos_alimentos.R adaptados
# ao molde do projeto. Para usar: copie TUDO isto para R/20_alimentos.R
# (substituindo o conteúdo de lá).
#
# Também precisa adicionar em R/00_pacotes.R:
#     library(DT)
#     library(plotly)
#
# O QUE FOI TIRADO do código original:
#   - library(...)                 -> vão para R/00_pacotes.R
#   - read_csv() do GitHub         -> food_beverages já vem de R/01_dados.R
#   - dashboardPage / menuItem     -> o menu do projeto já existe (R/03_layout.R)
#   - box() / valueBox()           -> viraram card() / value_box() (bslib)
#   - CSS do .small-box            -> não existe mais (era do shinydashboard)
#   - shinyApp(ui, server)         -> o app.R já faz isso
#
# REGRA: tudo termina em _alimentos (objetos, inputs e outputs).
# =====================================================================

setor_alimentos <- "Alimentos"
cor_alimentos   <- unname(cores[setor_alimentos])   # verde-sálvia do projeto


# =====================================================================
# PARTE 1 — DADOS E GRÁFICOS   (veio de graficos_alimentos.R)
# =====================================================================

# tradução da tabela DT para português  (igual ao original)
options(DT.options = list(
  language = list(
    decimal = ",", thousands = ".",
    lengthMenu   = "Mostrar _MENU_ registros por página",
    zeroRecords  = "Nenhum registro encontrado",
    info         = "Mostrando de _START_ até _END_ de _TOTAL_ registros",
    infoEmpty    = "Mostrando 0 até 0 de 0 registros",
    infoFiltered = "(filtrado de _MAX_ registros no total)",
    search       = "Buscar:",
    paginate = list(first = "Primeiro", last = "Último",
                    `next` = "Próximo", previous = "Anterior")
  )
))

# (o read_csv do GitHub foi APAGADO: food_beverages já existe)

variaveis_alimentos <- c(
  "Açúcar", "Glicose", "Substituto_café", "Oleo_de_sementes",
  "Etanol_1", "Etanol_2", "Cerveja"
)

# tradução das colunas + conversão: quintal -> toneladas (x / 10)
comidas_bebidas_alimentos <- food_beverages |>
  rename(
    Ano = Year, Açúcar = Sugar, Glicose = Glucose,
    Substituto_café = Coffee_substitute, Oleo_de_sementes = Seed_oil,
    Etanol_1 = Ethyl_alcohol_1, Etanol_2 = Ethyl_alcohol_2, Cerveja = Beer
  ) |>
  mutate(across(c(Glicose, Substituto_café, Oleo_de_sementes), ~ .x / 10))

# formato longo + década
longo_alimentos <- comidas_bebidas_alimentos |>
  pivot_longer(all_of(variaveis_alimentos), names_to = "Variavel", values_to = "Valor") |>
  mutate(Decada = paste0(floor(Ano / 10) * 10, "s"))

# estatística descritiva por década e produto
cols_estat_alimentos <- c("media", "dp", "min", "q1", "mediana", "q3", "max")

descritiva_alimentos <- longo_alimentos |>
  group_by(Decada, Variavel) |>
  summarise(
    n_obs   = sum(!is.na(Valor)),
    n_na    = sum(is.na(Valor)),
    media   = mean(Valor, na.rm = TRUE),
    dp      = sd(Valor, na.rm = TRUE),
    min     = min(Valor, na.rm = TRUE),
    q1      = quantile(Valor, 0.25, na.rm = TRUE, names = FALSE),
    mediana = median(Valor, na.rm = TRUE),
    q3      = quantile(Valor, 0.75, na.rm = TRUE, names = FALSE),
    max     = max(Valor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(all_of(cols_estat_alimentos),
                ~ if_else(is.nan(.x) | is.infinite(.x) | is.na(.x), NA_real_, .x))) |>
  arrange(Decada, Variavel)

anos_min_alimentos <- min(longo_alimentos$Ano, na.rm = TRUE)
anos_max_alimentos <- max(longo_alimentos$Ano, na.rm = TRUE)
decadas_alimentos  <- sort(unique(na.omit(longo_alimentos$Decada)))

fmt_br_alimentos <- function(x) {
  ifelse(is.na(x) | !is.finite(x), "NA",
         formatC(x, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))
}


# ---------------------------------------------------------------------
# GRÁFICO 1: violino por década   (veio do server: output$grafico_violino)
# ---------------------------------------------------------------------
grafico_violino_alimentos <- function(variavel, log = FALSE) {
  d <- longo_alimentos |>
    filter(Variavel == variavel, !is.na(Decada), !is.na(Valor))
  
  if (isTRUE(log)) d <- d |> filter(Valor > 0)
  
  d <- d |> group_by(Decada) |> filter(n() >= 3) |> ungroup()
  
  validate(need(nrow(d) > 0, "Sem décadas com dados suficientes para esta variável."))
  
  resumo <- d |>
    group_by(Decada) |>
    summarise(
      n_obs = n(), media = mean(Valor), mediana = median(Valor), dp = sd(Valor),
      q1 = quantile(Valor, 0.25, names = FALSE), q3 = quantile(Valor, 0.75, names = FALSE),
      min_val = min(Valor), max_val = max(Valor), .groups = "drop"
    ) |>
    mutate(texto_hover = paste0(
      "<b>Década:</b> ", Decada, "<br>",
      "<b>N:</b> ", n_obs, "<br>",
      "<b>Média:</b> ", fmt_br_alimentos(media), "<br>",
      "<b>Mediana (Q2):</b> ", fmt_br_alimentos(mediana), "<br>",
      "<b>DP:</b> ", fmt_br_alimentos(dp), "<br>",
      "<b>Q1 – Q3:</b> ", fmt_br_alimentos(q1), " – ", fmt_br_alimentos(q3), "<br>",
      "<b>Mín – Máx:</b> ", fmt_br_alimentos(min_val), " – ", fmt_br_alimentos(max_val)
    ))
  
  niveis <- intersect(decadas_alimentos, unique(d$Decada))
  d      <- d |> left_join(resumo |> select(Decada, texto_hover), by = "Decada") |>
    mutate(Decada = factor(Decada, levels = niveis))
  resumo <- resumo |> mutate(Decada = factor(Decada, levels = niveis))
  
  p <- ggplot(d, aes(x = Decada, y = Valor, fill = Decada)) +
    geom_violin(aes(text = texto_hover), scale = "width", alpha = 0.65, colour = "grey30") +
    geom_boxplot(width = 0.12, fill = "white", outlier.size = 0.8, show.legend = FALSE) +
    geom_point(data = resumo, aes(x = Decada, y = mediana, text = texto_hover),
               inherit.aes = FALSE, shape = 23, size = 2.2, fill = "#111111", colour = "white") +
    scale_fill_viridis_d(option = "D", guide = "none") +
    labs(title = paste("Distribuição de", variavel, "por Década"), x = NULL, y = variavel) +
    tema_mix
  
  rotulo <- scales::label_number(big.mark = ".", decimal.mark = ",")
  p <- if (isTRUE(log)) p + scale_y_log10(labels = rotulo) else p + scale_y_continuous(labels = rotulo)
  
  ggplotly(p, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
}


# ---------------------------------------------------------------------
# GRÁFICO 2: série anual + LOESS   (veio do server: output$grafico_serie_loess)
# ---------------------------------------------------------------------
grafico_serie_alimentos <- function(variavel, janela) {
  ano_ini <- janela[1]
  ano_fim <- janela[2]
  
  d <- longo_alimentos |>
    filter(Variavel == variavel, !is.na(Ano), !is.na(Valor),
           Ano >= (ano_ini - 10), Ano <= ano_fim) |>
    mutate(texto_ponto = paste0("<b>Ano:</b> ", Ano, "<br>",
                                "<b>", variavel, ":</b> ", fmt_br_alimentos(Valor))) |>
    arrange(Ano)
  
  validate(need(nrow(d) > 0, "Sem observações válidas no período selecionado."))
  
  rotulo <- scales::label_number(big.mark = ".", decimal.mark = ",")
  
  p <- ggplot(d, aes(x = Ano, y = Valor)) +
    geom_segment(aes(xend = Ano, yend = 0, color = Valor, text = texto_ponto), linewidth = 0.8) +
    geom_smooth(method = "loess", se = FALSE, color = "#0F3A40", linewidth = 1) +
    coord_cartesian(xlim = c(ano_ini, ano_fim)) +
    scale_color_gradient(low = "#CE2B37", high = cor_alimentos, labels = rotulo) +
    labs(title = paste("Série Anual e Tendência LOESS:", variavel),
         x = "Ano", y = variavel, color = variavel) +
    tema_mix
  
  ggplotly(p, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
}


# =====================================================================
# PARTE 2 — TELA (UI)   (veio do  ui <- dashboardPage(...)  )
#   tabItem  -> nav_panel      box -> card      valueBox -> value_box
#   sidebarLayout -> layout_sidebar
# =====================================================================
ui_alimentos <- function() {
  pagina("alimentos", "Alimentos", "Produção italiana de alimentos e bebidas",
         
         # cor do cabeçalho das tabelas (era o CSS do original, com os IDs novos)
         tags$style(HTML(paste0(
           "#tabela_brutos_alimentos thead th, #tabela_descritiva_alimentos thead th {",
           " background-color: ", cor_alimentos, " !important; color: #fff !important; }"
         ))),
         
         navset_card_tab(
           
           # ------------ ABA 1 (era aba1_visao_geral) ------------
           nav_panel("Visão Geral e Décadas",
                     
                     layout_columns(
                       value_box("Linhas",    textOutput("kpi_linhas_alimentos"),    showcase = icon("table")),
                       value_box("Período",   textOutput("kpi_periodo_alimentos"),   showcase = icon("calendar")),
                       value_box("Variáveis", textOutput("kpi_variaveis_alimentos"), showcase = icon("list"))
                     ),
                     
                     layout_columns(
                       card(card_header("Base traduzida e convertida"),
                            downloadButton("baixar_base_alimentos", "Baixar base (.csv)"),
                            DTOutput("tabela_brutos_alimentos")),
                       card(card_header("Estatística descritiva por década e produto"),
                            downloadButton("baixar_descritiva_alimentos", "Baixar estatística (.csv)"),
                            DTOutput("tabela_descritiva_alimentos"))
                     ),
                     
                     card(card_header("Distribuição Histórica por Década"),
                          layout_sidebar(
                            sidebar = sidebar(
                              selectInput("variavel_alimentos", "Selecione o Produto:",
                                          choices = variaveis_alimentos, selected = "Cerveja"),
                              checkboxInput("log_alimentos", "Usar escala logarítmica no eixo Y", FALSE),
                              helpText("Décadas com menos de 3 observações não são exibidas.")
                            ),
                            plotlyOutput("violino_alimentos", height = "480px")
                          ))
           ),
           
           # ------------ ABA 2 (era aba2_his_war_beer) ------------
           nav_panel("História, Guerras e Cerveja",
                     card(card_header("Série Anual com Tendência LOESS"),
                          layout_sidebar(
                            sidebar = sidebar(
                              selectInput("variavel_serie_alimentos", "Selecione o Produto:",
                                          choices = variaveis_alimentos, selected = "Cerveja"),
                              sliderInput("janela_alimentos", "Janela de anos:",
                                          min = anos_min_alimentos, max = anos_max_alimentos,
                                          value = c(max(anos_min_alimentos, anos_max_alimentos - 25),
                                                    anos_max_alimentos),
                                          sep = "", step = 1)
                            ),
                            plotlyOutput("serie_alimentos", height = "480px")
                          ))
           )
         )
  )
}


# =====================================================================
# PARTE 3 — SERVER   (veio do  server <- function(input, output) {...} )
# =====================================================================
server_alimentos <- function(input, output, session) {
  
  # KPIs (eram valueBox; agora só o texto, o value_box está na UI)
  output$kpi_linhas_alimentos    <- renderText(format(nrow(food_beverages), big.mark = "."))
  output$kpi_periodo_alimentos   <- renderText(paste0(anos_min_alimentos, "–", anos_max_alimentos,
                                                      " (", length(decadas_alimentos), " décadas)"))
  output$kpi_variaveis_alimentos <- renderText(length(variaveis_alimentos))
  
  # tabela 1: base traduzida (NA aparece como "NA")
  output$tabela_brutos_alimentos <- renderDT({
    js_na <- JS(
      "function(data, type, row, meta) {",
      "  if (type !== 'display') return data;",
      "  if (data === null || data === undefined || data === '') return 'NA';",
      "  if (typeof data === 'number' && Number.isNaN(data)) return 'NA';",
      "  return data;",
      "}"
    )
    alvo <- which(names(comidas_bebidas_alimentos) %in% c("Ano", variaveis_alimentos)) - 1
    datatable(comidas_bebidas_alimentos, rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE,
                             columnDefs = list(list(targets = alvo, render = js_na))))
  })
  
  # tabela 2: estatística descritiva (2 casas, padrão BR)
  output$tabela_descritiva_alimentos <- renderDT({
    js_2dec <- JS(
      "function(data, type, row, meta) {",
      "  if (type !== 'display') {",
      "    return (data === null || data === undefined || data === '' || Number.isNaN(Number(data))) ? -Infinity : Number(data);",
      "  }",
      "  if (data === null || data === undefined || data === '') return 'NA';",
      "  var num = Number(data);",
      "  if (Number.isNaN(num) || !Number.isFinite(num)) return 'NA';",
      "  return num.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });",
      "}"
    )
    alvo <- which(names(descritiva_alimentos) %in% cols_estat_alimentos) - 1
    datatable(descritiva_alimentos, rownames = FALSE,
              colnames = c("Década", "Variável", "N", "NAs", "Média", "DP",
                           "Mín", "Q1", "Mediana", "Q3", "Máx"),
              options = list(pageLength = 10, scrollX = TRUE,
                             columnDefs = list(list(targets = alvo, render = js_2dec))))
  })
  
  # downloads
  output$baixar_base_alimentos <- downloadHandler(
    filename = function() paste0("comidas_bebidas_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(comidas_bebidas_alimentos, file)
  )
  output$baixar_descritiva_alimentos <- downloadHandler(
    filename = function() paste0("estatistica_descritiva_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(descritiva_alimentos, file)
  )
  
  # gráficos (1 linha cada — as funções estão na PARTE 1)
  output$violino_alimentos <- renderPlotly(
    grafico_violino_alimentos(input$variavel_alimentos, input$log_alimentos)
  )
  output$serie_alimentos <- renderPlotly(
    grafico_serie_alimentos(input$variavel_serie_alimentos, input$janela_alimentos)
  )
}
