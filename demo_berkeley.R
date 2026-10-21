# =====================================================================
# Demonstração ao vivo · Dia 1 · Oficina RStudio (SNCT 2026)
# Berkeley, 1973: as mulheres eram barradas?
#
# Como usar: rode um bloco por vez (selecione e aperte Ctrl + Enter).
# Os passos 1 a 4 não precisam de pacote nem de arquivo: os dados já
# vêm com o R. O passo 5 precisa do pacote shiny.
# Requer R 4.0 ou mais novo.
# =====================================================================


# 1. No total, quem foi mais aprovado? ---------------------------------

# já vem no R: candidaturas de 1973
ucb <- UCBAdmissions

# soma os 6 departamentos
geral <- margin.table(ucb, c("Admit", "Gender"))
geral

# em porcentagem, dentro de cada gênero
round(100 * prop.table(geral, "Gender"))

# Esperado: homens 45% aprovados, mulheres 30%


# 2. E departamento por departamento? ----------------------------------

# % de aprovados por gênero, em cada departamento
taxa <- prop.table(ucb, c("Gender", "Dept"))
por_dep <- round(100 * taxa["Admitted", , ])
por_dep
barplot(por_dep, beside = TRUE,
        legend.text = c("Homens", "Mulheres"),
        col = c("steelblue", "orange"))

# Esperado: elas na frente em A, B, D e F (no A, 82% contra 62%)


# 3. Qual departamento aprova mais? ------------------------------------

# % de aprovados por departamento
dep <- margin.table(ucb, c("Admit", "Dept"))
round(100 * prop.table(dep, "Dept"))

# Esperado: A 64%, B 63%, C 35%, D 34%, E 25%, F 6%


# 4. Onde cada grupo se candidatou? ------------------------------------

# % das candidaturas por departamento
cand <- margin.table(ucb, c("Gender", "Dept"))
round(100 * prop.table(cand, "Gender"))

# Esperado: homens 31% no A e 21% no B (52% nos dois mais fáceis);
#           mulheres 6% no A e 1% no B (só 7%)


# 5. Dezenove linhas viram um app --------------------------------------

# install.packages("shiny")   # só uma vez, antes da aula

library(shiny)
ui <- fluidPage(
  selectInput("dep", "Departamento",
              c("Todos", "A", "B", "C", "D", "E", "F")),
  plotOutput("grafico")
)
server <- function(input, output) {
  output$grafico <- renderPlot({
    tab <- UCBAdmissions
    if (input$dep != "Todos") tab <- tab[, , input$dep]
    geral <- margin.table(tab, c("Admit", "Gender"))
    taxa <- prop.table(geral, "Gender")["Admitted", ]
    barplot(100 * taxa, ylim = c(0, 100),
            main = "% de aprovados",
            names.arg = c("Homens", "Mulheres"),
            col = c("steelblue", "orange"))
  })
}
shinyApp(ui, server)

# Para parar o app: botão vermelho Stop no Console (ou Esc)
