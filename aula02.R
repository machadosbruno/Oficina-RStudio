library(dplyr)
library(ggplot2)
devs <- read.csv('Y:/Oficina-RStudio/devs.csv')

# Quanto ganha um dev "típico"
mean(devs$salario)
median(devs$salario)
# Resumo do salário (6 números)
summary(devs$salario)

# Contagens e porcentagens
tabela <- table(devs$modelo_trabalho)
tabela
round(100 * prop.table(tabela), 1)

idades <- c(25, 27, 33, 41, 52, 47)
log(sqrt(exp(mean(idades))))

# Operador "pipe
idades |> mean() |> exp() |> sqrt() |> log()

# A mediana de cada modelo de trabalho
# Lê-se: “aplique a mediana ao salário,
# separado por modelo de trabalho”.
tapply(devs$salario, devs$modelo_trabalho, median)

# Pergunta: Qual a média salarial por tipo de trabalho

# Resumir por grupo - O mesmo resumo,
# escrito como uma frase
devs |>
  group_by(modelo_trabalho) |>
  summarise(
    pessoas = n(),
    media   = mean(salario),
    mediana = median(salario)
  )

# Filtrar, contar e ordernar
devs |>
  filter(modelo_trabalho == "Remoto") |>
  summarise(mediana = median(salario))

devs |>
  count(cargo, sort = TRUE)

# Exercicio
# 1. Mediana do salário de quem usa IA
devs |>
  filter(usa_ia == "Sim") |>
  summarise(mediana = median(salario))

# 2. Quantas pessoas em cada escolaridade,
# da maior para a menor
devs |>
  count(escolaridade, sort = TRUE)

# 3. Mediana do salário por cargo da maior para a menor
devs |>
  group_by(cargo) |>
    summarise(
      mediana = median(salario),
    ) |> 
    arrange(desc(mediana))

# Parte 2: Diferença real ou acaso
# Qual a chance de acertar tudo no chute?
choose(8, 4)
1 / choose(8, 4)

# Testes estatísticos
# Teste t: comparar duas médias
t.test(devs$salario[devs$modelo_trabalho == "Remoto"],
       devs$salario[devs$modelo_trabalho == "Presencial"])

devs_filtrado <- devs |>
  filter(modelo_trabalho == "Remoto" | modelo_trabalho == "Presencial")
t.test(salario ~modelo_trabalho, data = devs_filtrado)

# Tabela cruzada
tab_cruzada <- table(devs$modelo_trabalho, devs$usa_ia)
round(prop.table(tab_cruzada, 2)*100, 2)
# Testes qui-quadrado
chisq.test(tab_cruzada)

# A lógica de todo teste em três passos
# 1 Suponha que não há diferença:
#   tudo é acaso (a hipótese nula)
# 2 Calcule a chance de ver dados assim
#   nesse caso (o p-valor)
# 3 p pequeno (menor que 0,05): o acaso explica mal

# Remoto x Presencial
# 1. fica só com o remoto e o presencial

duas <- filter(devs, modelo_trabalho != "Híbrido")

#2. compara o salário médio dos dois grupos
# salario ~ modelo_trabalho "salário em função do modelo"
t.test(salario ~ modelo_trabalho, data = duas)

# quem trabalha remoto usa mais IA?
tab <- table(devs$modelo_trabalho, devs$usa_ia)
tab
chisq.test(tab)
# Gráfico de barras empilhadas
devs |> 
  ggplot(aes(x = modelo_trabalho, fill = usa_ia)) +
  geom_bar(position = "fill")

# Os três grupos usam IA quase igual
ggplot(devs, aes(x = modelo_trabalho,
         fill = usa_ia)) +
  geom_bar(position = "fill")

# Experiência e salário andam juntos?
cor(devs$experiencia,
    devs$salario)

# Quanto vale um ano de experiência
modelo <- lm(salario ~ experiencia,
           data = devs)
modelo

# A reta no meio da nuvem
ggplot(devs, aes(x = experiencia,
       y = salario)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm")

# summary(modelo): o relatório completo
summary(modelo)

# Exercício
# 1. Teste t: híbrido x presencial. A diferença é real?
hibXpres <- filter(devs, modelo_trabalho != "Remoto")
t.test(salario ~ modelo_trabalho, data = hibXpres)

# 2. Para 10 anos de experiência,
#    qual salário a reta prevê?
summary(modelo)
#    Multiplicando valor da coluna experiência por 10
8407.52 / 608.50 *10

# 3. Os mais jovens usam mais IA?
#    Qui-quadrado de faixa_etaria × usa_ia
etaXia <- table(devs$faixa_etaria, devs$usa_ia)
chisq.test(etaXia)
