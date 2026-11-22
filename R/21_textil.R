# =====================================================================
# PÁGINA TÊXTIL
# =====================================================================
# Responsável: ____________
#
#   PARTE 1 — GRÁFICOS   uma função por gráfico (só desta página)
#   PARTE 2 — TELA (UI)  onde cada gráfico aparece
#   PARTE 3 — SERVER     liga a tela aos gráficos (1 linha cada)
#
# COMO COLOCAR UM GRÁFICO (3 passos) — tem um exemplo pronto abaixo,
# é só tirar os # das linhas marcadas com  #>
#   1. PARTE 1: escreva o gráfico como função:  grafico_xxx_textil <- function() { ggplot(...) }
#   2. PARTE 2: coloque  plotOutput("xxx_textil")  dentro de um card
#   3. PARTE 3: ligue    output$xxx_textil <- renderPlot(grafico_xxx_textil())
#
# REGRA: termine os nomes com _textil (funções E outputs), para não
# misturar com as outras páginas.
#
# O que já existe para usar (vem de R/00 a R/04):
#   dados        tabela: Year | produto | producao | setor
#   cores        cor de cada setor     ->  cores["Têxteis"]
#   tema_mix     estilo dos gráficos   ->  + tema_mix  no fim do ggplot
#   nome_pt()    nome em português     ->  nome_pt("Sugar") = "Açúcar"
#   numero_curto eixo com "10 mil"     ->  scale_y_continuous(labels = numero_curto)
# =====================================================================

setor_textil <- "Têxteis"      # nome do setor como está na tabela `dados`


# =====================================================================
# PARTE 1 — GRÁFICOS
# =====================================================================

# ---------------------------------------------------------------------
# EXEMPLO: produção de cada produto ao longo do tempo
# ---------------------------------------------------------------------
#> grafico_exemplo_textil <- function() {
#>   dados |>
#>     filter(setor == setor_textil) |>
#>     ggplot(aes(x = Year, y = producao)) +
#>     geom_line(color = cores[setor_textil]) +
#>     facet_wrap(~ produto, scales = "free_y", labeller = labeller(produto = nome_pt)) +
#>     scale_y_continuous(labels = numero_curto) +
#>     labs(x = NULL, y = "Produção") +
#>     tema_mix
#> }


# =====================================================================
# PARTE 2 — TELA (UI)
# =====================================================================
ui_textil <- function() {
  pagina("textil", "Têxtil", "Cada produto do setor têxtil",

         #> card(card_header("Produção de cada produto ao longo do tempo"),
         #>      plotOutput("exemplo_textil", height = "500px")),

         # (apague este card quando colocar o primeiro gráfico)
         card(card_header("Em construção"),
              p("Os gráficos desta página ainda vão ser feitos."))
  )
}


# =====================================================================
# PARTE 3 — SERVER
# =====================================================================
server_textil <- function(input, output, session) {

  #> output$exemplo_textil <- renderPlot(grafico_exemplo_textil())

}
