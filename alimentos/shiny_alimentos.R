library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# ===================== Shiny ========================

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Produção Italiana"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Visão Geral e Décadas",
        tabName = "aba1_visao_geral",
        icon = icon("chart-line")
      ),
      menuItem(
        text = "História, Guerras e Cerveja",
        tabName = "aba2_his_war_beer",
        icon = icon("beer")
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
        ),
        fluidRow(
          box(
            title = "Distribuição dos Produtos por Década e Evolução de cada item ao longo do tempo",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  inputId = "variavel_selecionada",
                  label = "Selecione o Produto (Variável):",
                  choices = variaveis_prod,
                  selected = "Cerveja"
                ),
                checkboxInput(
                  inputId = "escala_log",
                  label = "Usar escala logarítmica no eixo Y",
                  value = FALSE
                ),
                helpText("Décadas com menos de 3 observações não são exibidas.")
              ),
              mainPanel(
                plotlyOutput(outputId = "grafico_violino", height = "480px")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "aba2_his_war_beer",
        fluidRow(
          box(
            title = "Média por Década (± desvio padrão) com Destaques de 1914–1918 e 1939–1945",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(outputId = "grafico_mm_guerras", height = "900px")
          )
        ),
        fluidRow(
          box(
            title = "Média histórica por década e produto (log1p)",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(outputId = "grafico_heat", height = "520px"),
            helpText(
              "Cor = log1p da média. Em cada célula: média e desvio padrão (±).",
              "Passe o mouse para ver N, NAs, média, DP, mín, Q1, mediana, Q3 e máx.",
              "Células em branco = década sem dados para a variável."
            )
          )
        ),
      )
    )
  )
)

server <- function(input, output) {
  output$kpi_linhas <- renderValueBox({
    valueBox(
      value = format(nrow(food_beverages), big.mark = ".", decimal.mark = ","),
      subtitle = "Linhas",
      icon = icon("table"),
      color = "green"
    )
  })
  
  output$kpi_periodo <- renderValueBox({
    valueBox(
      value = paste0(anos_min, "-", anos_max),
      subtitle = paste0("Faixa de anos (", length(decadas), " décadas)"),
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$kpi_variaveis <- renderValueBox({
    valueBox(
      value = length(variaveis_prod),
      subtitle = "Variáveis",
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
    idx_estat <- which(names(tabela_descritiva) %in% cols_estat) - 1
    
    render_2dec_ou_na <- JS(
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
    
    datatable(
      tabela_descritiva,
      rownames = FALSE,
      colnames = c(
        "Década", "Variável", "N", "NAs", "Média", "DP",
        "Mín", "Q1", "Mediana", "Q3", "Máx"
      ),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = idx_estat, render = render_2dec_ou_na)
        )
      )
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
      readr::write_csv(tabela_descritiva, file)
    }
  )
  
  output$grafico_violino <- renderPlotly({
    req(input$variavel_selecionada)
    
    d <- comidas_bebidas_longer |>
      filter(
        Variavel == input$variavel_selecionada,
        !is.na(Decada),
        !is.na(Valor)
      )
    
    if (isTRUE(input$escala_log)) {
      d <- d |> filter(Valor > 0)
    }
    
    d <- d |>
      group_by(Decada) |>
      filter(n() >= 3) |>
      ungroup()
    
    validate(need(nrow(d) > 0, "Sem décadas com dados suficientes para esta variável."))
    
    resumo_decada <- d |>
      group_by(Decada) |>
      summarise(
        n_obs   = n(),
        media   = mean(Valor, na.rm = TRUE),
        mediana = median(Valor, na.rm = TRUE),
        dp      = sd(Valor, na.rm = TRUE),
        q1      = quantile(Valor, probs = 0.25, na.rm = TRUE, names = FALSE),
        q3      = quantile(Valor, probs = 0.75, na.rm = TRUE, names = FALSE),
        min_val = min(Valor, na.rm = TRUE),
        max_val = max(Valor, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        texto_hover = paste0(
          "<b>Década:</b> ", Decada, "<br>",
          "<b>N:</b> ", n_obs, "<br>",
          "<b>Média:</b> ", fmt_br(media), "<br>",
          "<b>Mediana (Q2):</b> ", fmt_br(mediana), "<br>",
          "<b>DP:</b> ", fmt_br(dp), "<br>",
          "<b>Q1 – Q3:</b> ", fmt_br(q1), " – ", fmt_br(q3), "<br>",
          "<b>Mín – Máx:</b> ", fmt_br(min_val), " – ", fmt_br(max_val)
        )
      )
    
    niveis_decada <- intersect(decadas, unique(d$Decada))
    
    d <- d |>
      left_join(resumo_decada |> select(Decada, texto_hover), by = "Decada") |>
      mutate(Decada = factor(Decada, levels = niveis_decada))
    
    resumo_decada <- resumo_decada |>
      mutate(Decada = factor(Decada, levels = niveis_decada))
    
    p <- ggplot(d, aes(x = Decada, y = Valor, fill = Decada)) +
      geom_violin(
        aes(text = texto_hover),
        scale = "width",
        alpha = 0.65,
        colour = "grey30"
      ) +
      geom_boxplot(
        width = 0.12,
        fill = "white",
        outlier.size = 0.8,
        show.legend = FALSE
      ) +
      geom_point(
        data = resumo_decada,
        aes(x = Decada, y = mediana, text = texto_hover),
        inherit.aes = FALSE,
        shape = 23,
        size = 2.2,
        fill = "#111111",
        colour = "white"
      ) +
      scale_fill_viridis_d(option = "D", guide = "none") +
      labs(
        title = paste("Distribuição de", input$variavel_selecionada, "por Década"),
        x = NULL,
        y = input$variavel_selecionada
      ) +
      theme_minimal(base_size = 12)
    
    if (isTRUE(input$escala_log)) {
      p <- p + scale_y_log10(labels = scales::label_number(big.mark = ".", decimal.mark = ","))
    } else {
      p <- p + scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","))
    }
    
    ggplotly(p, tooltip = "text") |>
      layout(hoverlabel = list(align = "left"))
  })
  
  
  #=============== segunda aba ===================  
    limites_y_mm <- reactive({
      dados_decada |>
        summarise(
          ymin = min(valor_decada, na.rm = TRUE),
          ymax = max(valor_decada, na.rm = TRUE),
          .by = Variavel
        ) |>
        mutate(
          folga = (ymax - ymin) * 0.05,
          ymin  = ymin - folga,
          ymax  = ymax + folga
        ) |>
        select(Variavel, ymin, ymax)
    })
    
    periodos_guerra <- reactive({
      tibble(
        xmin   = c(1910, 1940) - 5,
        xmax   = c(1910, 1940) + 5,
        guerra = c("1910s (1914–1918)", "1940s (1939–1945)")
      ) |>
        crossing(limites_y_mm())
    })
    
    output$grafico_mm_guerras <- renderPlotly({
      eixo_x <- sort(unique(dados_decada$decada_num))
      
      dd <- dados_decada |>
        mutate(
          texto_hover = paste0(
            "<b>Década:</b> ", Decada, "<br>",
            "<b>Média:</b> ", fmt_br(valor_decada), "<br>",
            "<b>DP:</b> ", fmt_br(dp_decada), "<br>",
            "<b>Média ± DP:</b> ", fmt_br(inferior), " – ", fmt_br(superior), "<br>",
            "<b>N:</b> ", n_obs
          )
        )
      
      p <- ggplot() +
        geom_rect(
          data = periodos_guerra(),
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = guerra),
          alpha = 0.35,
          inherit.aes = FALSE
        ) +
        geom_errorbar(
          data = dd,
          aes(x = decada_num, ymin = inferior, ymax = superior, color = Variavel),
          width = 2, linewidth = 0.6, alpha = 0.8
        ) +
        geom_line(
          data = dd,
          aes(x = decada_num, y = valor_decada, color = Variavel, group = Variavel),
          linewidth = 0.9, alpha = 0.9
        ) +
        geom_point(
          data = dd,
          aes(x = decada_num, y = valor_decada, color = Variavel, text = texto_hover),
          size = 2, alpha = 0.95
        ) +
        facet_wrap(~ Variavel, scales = "free_y", ncol = 2) +
        scale_fill_manual(
          values = c("1910s (1914–1918)" = "#f39c12", "1940s (1939–1945)" = "#e74c3c"),
          name = "Período destacado"
        ) +
        scale_x_continuous(breaks = eixo_x, labels = paste0(eixo_x, "s")) +
        scale_y_continuous(
          labels = scales::label_number(big.mark = ".", decimal.mark = ",")
        ) +
        labs(
          x = "Década", y = "Média na década"
        ) +
        theme_dark() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom"
        ) +
        guides(color = "none")
      
      ggplotly(p, tooltip = "text") |>
        layout(hoverlabel = list(align = "left"))
    })
    output$grafico_heat <- renderPlotly({
      # fmt_br que aceita NA
      f <- function(x) ifelse(is.na(x), "NA", fmt_br(x))
      
      d <- tabela_descritiva |>
        filter(!is.na(Decada)) |>
        mutate(
          Decada   = factor(Decada, levels = decadas),
          Variavel = factor(Variavel, levels = variaveis_prod),
          z        = log1p(media),
          hover = paste0(
            "<b>Década:</b> ", Decada, "<br>",
            "<b>Variável:</b> ", Variavel, "<br>",
            "<b>N:</b> ", n_obs, " | <b>NAs:</b> ", n_na, "<br>",
            "<b>Média:</b> ", f(media), "<br>",
            "<b>DP:</b> ", f(dp), "<br>",
            "<b>Mín:</b> ", f(min), "<br>",
            "<b>Q1:</b> ", f(q1), "<br>",
            "<b>Mediana (Q2):</b> ", f(mediana), "<br>",
            "<b>Q3:</b> ", f(q3), "<br>",
            "<b>Máx:</b> ", f(max)
          ),
          rotulo = ifelse(
            is.na(media), "",
            paste0(f(media), "<br>±", f(dp))
          )
        )
      
      # texto branco nas células escuras, preto nas claras
      z_meio <- mean(range(d$z, na.rm = TRUE))
      d <- d |>
        mutate(cor_txt = ifelse(!is.na(z) & z > z_meio, "white", "black"))
      
      anotacoes <- lapply(seq_len(nrow(d)), function(i) {
        list(
          x = as.character(d$Decada[i]),
          y = as.character(d$Variavel[i]),
          text = d$rotulo[i],
          showarrow = FALSE,
          font = list(size = 10, color = d$cor_txt[i])
        )
      })
      
      plot_ly(
        data = d,
        x = ~Decada,
        y = ~Variavel,
        z = ~z,
        type = "heatmap",
        text = ~hover,
        hoverinfo = "text",
        colorscale = list(c(0, "#ffffcc"), c(0.5, "#fd8d3c"), c(1, "#800026")),
        colorbar = list(title = "log1p(média)")
      ) |>
        layout(
          xaxis = list(title = "Década", categoryorder = "array", categoryarray = decadas),
          yaxis = list(title = "Variável", autorange = "reversed"),
          annotations = anotacoes,
          hoverlabel = list(align = "left")
        )
    })
    
    limites_y_mm <- reactive({
      dados_decada |>
        summarise(
          ymin = min(valor_decada, na.rm = TRUE),
          ymax = max(valor_decada, na.rm = TRUE),
          .by = Variavel
        ) |>
        mutate(
          folga = (ymax - ymin) * 0.05,
          ymin  = ymin - folga,
          ymax  = ymax + folga
        ) |>
        select(Variavel, ymin, ymax)
    })
    
    periodos_guerra <- reactive({
      tibble(
        xmin   = c(1910, 1940) - 5,
        xmax   = c(1910, 1940) + 5,
        guerra = c("1910s (1914–1918)", "1940s (1939–1945)")
      ) |>
        crossing(limites_y_mm())
    })
}

shinyApp(ui = ui, server = server)