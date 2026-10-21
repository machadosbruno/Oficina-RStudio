# Definindo as variáveis salário e cientista
salario <- 3500
salario * 12
cientista <- "Ada Lovelace"

# Trabalhando com vetores
idade <- c(16,17,16,18,37, NA)
mean(idade, na.rm = TRUE) # Média com remoção de NA
sd(idade) # Desvio Padrão
median(idade) # Mediana
min(idade) # Valor mínimo
max(idade) # Valor máximo
quantile(idade,0.25) # quartil q1
table(idade) # Gera tabela de frequência, que pode ser usada para calcular a moda

idade[idade >= 18]

curso <- c("A", "A", "B", "B", "B")
mean(curso) # da NA = Not Available
table(curso)

# Execício de altura
altura <- c(1.75, 1.60, 1.65, 1.70, 1.90)
mean(altura)
max(altura)
sum(altura > 1.70)


# Importando o banco de dados
devs <- read.csv("Y:/oficina/devs.csv")

str(devs)
head(devs$salario)
max(devs$salario)
table(devs$modelo_trabalho)

# Exercício devs
table(devs$cargo)
max(devs$experiencia)
sum(devs$usa_ia == "Sim", na.rm = TRUE)

# Parte 4 - Gráficos
library(ggplot2) # importanto a biblioteca

# Experiência X Salário
ggplot(devs, aes(x = experiencia, y = salario)) +  # 1 dados e eixos
  geom_point() +                                    # 2 a forma
  labs(title = "Mais experiência, mais salário?")   # 3 os títulos

# Distribuição de salário
ggplot(devs, aes(x = salario)) + geom_histogram()

# Modelo de trabalho X Salário
ggplot(devs, aes(x = modelo_trabalho,
        y = salario)) +
        geom_boxplot()

# Mais experiência, mais salário? (de novo???)
ggplot(devs, aes(x = experiencia,
        y = salario)) +
        geom_point()

# Pessoas X Cargo
ggplot(devs, aes(y = cargo)) +
        geom_bar()

# salário X modelo de trabalho colorido
ggplot(devs, aes(x = modelo_trabalho,
                 y = salario, fill = modelo_trabalho)) +
       geom_boxplot(show.legend = FALSE) +
       labs(title = "Salário por modelo",
            x = NULL, y = "R$ por mês") +
       theme_minimal()

# Exercícios Gráficos

# salário X modelo de trabalho colorido
ggplot(devs, aes(x = faixa_etaria,
                 y = salario, fill = faixa_etaria)) +
                 geom_boxplot(show.legend = FALSE) +
                 labs(title = "Salário por Faixa Etária",
                 x = NULL, y = "R$ por mês") +
                 theme_minimal()

# Pessoas X Usa IA
ggplot(devs, aes(y = usa_ia)) +
       geom_bar() +
       labs(title = "Uso de IA",
       x = "Quantidade de pessoas", y = "Resposta")

# Modelo de trabalho X Salário
ggplot(devs, aes(x = modelo_trabalho, y = salario, color=modelo_trabalho)) +  # 1 dados e eixos
  geom_point() +                                    # 2 a forma
  labs(title = "Distribuição de salário por modelo de trabalho",
       x = "Salário", y = "Modelo de trabalho")   # 3 os títulos
