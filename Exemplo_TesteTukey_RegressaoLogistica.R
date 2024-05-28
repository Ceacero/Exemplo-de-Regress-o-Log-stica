
# Exemplos de Regressão aplicados à base de dados iris


# Ativando pacote ggplot2 de visualização gráfica, dplyr de
# manipulação de dados e broom para pblackição a partir de modelos


library(ggplot2)
library(dplyr)
library(broom)


# Carregando base de dados que estudaremos

iris = iris


# Existe diferenca no comprimento das pétalas em cada espécie de flor?


# Vamos nos aproximar inicialmente do problema criando um gráfico.


# Criando grafico de boxplot para visualizar comportamento do comprimento
# das pétalas para cada espécie de flor


ggplot(iris, mapping = aes(x = Species, y = Petal.Length)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Comprimento de pétalas de diferentes espécies de flor",
       y = "Comprimento de pétala",
       x = "Espécie de flor") +
  theme_bw()


# Graficamente, parece haver diferencas entre os comprimentos de pétalas
# para cada espécie de flor. Vamos prosseguir com testes de hipóteses
# para poder fazer afirmações estatisticamente significantes.


# Para responder a pergunta proposta, podemos começar verificando se o
# comprimento de pétalas de cada espécie segue distribuição normal através
# de testes de hipóteses de Shapiro-Wilk, antes de seguir com analises mais profundas.


shapiro.test(iris$Petal.Length[iris$Species == "setosa"])

shapiro.test(iris$Petal.Length[iris$Species == "versicolor"])

shapiro.test(iris$Petal.Length[iris$Species == "virginica"])


# Sob nivel de significancia de 5%, nao rejeitamos as hipoteses nulas; isto é,
# de que a distribuição do comprimento de pétalas dentro de cada uma
# das espécies segue distribuição Normal.


# Tendo isso em mente, podemos seguir para o teste de ANOVA para comparação
# de médias de populações normais.

modelo = aov(iris$Petal.Length ~ iris$Species)

summary(modelo)


# Veja que o p-valor é extremamente baixo; ou seja, podemos inferir que
# existe diferenca estatisticamente significante entre a media de comprimento
# de pétala entre pelo menos um par de espécies de flores.


# Vamos realizar o teste de Tukey para verificar se a diferença estatisticamente
# significante é apenas entre a espécie setosa e as outras duas, ou se
# versicolor e virginica também possuem diferença estatisticamente significante.


TukeyHSD(modelo, ordeblack = TRUE)

plot(TukeyHSD(modelo, ordeblack = TRUE))


# Veja que nenhum dos intervalos de confiança se sobrepõem. Logo, há evidências
# para afirmar que o comprimento médio das pétalas é diferente em cada
# uma das espécies de flor.


# Vamos estudar agora a distribuicao do comprimento de pétala dentro de cada
# tipo de flor. Como nao rejeitamos a normalidade da distribuicao, vamos
# construir um grafico de histograma com a curva teorica da distribuicao Normal
# utilizando a média amostral e variância amostral.


# A comecar pela espécie setosa:


ggplot(mapping = aes(x = iris$Petal.Length[iris$Species == "setosa"])) +
  geom_histogram(fill = "steelblue",
                 color = "black",
                 aes(y = ..density..),
                 bins = 9) +
  labs(title = "Comprimento de pétalas de flores da espécie setosa",
       x = "Comprimento de pétalas",
       y = "Densidade de flores") +
  stat_function(fun = dnorm,
                args = list(mean = mean(iris$Petal.Length[iris$Species == "setosa"]),
                            sd = sd(iris$Petal.Length[iris$Species == "setosa"])),
                color = "black",
                aes(x = ..x..),
                size = 1)

# Agora, para a espécie versicolor:

ggplot(mapping = aes(x = iris$Petal.Length[iris$Species == "versicolor"])) +
  geom_histogram(fill = "steelblue",
                 color = "black",
                 aes(y = ..density..),
                 bins = 9) +
  labs(title = "Comprimento de pétalas de flores da espécie versicolor",
       x = "Comprimento de pétalas",
       y = "Densidade de flores") +
  stat_function(fun = dnorm,
                args = list(mean = mean(iris$Petal.Length[iris$Species == "versicolor"]),
                            sd = sd(iris$Petal.Length[iris$Species == "versicolor"])),
                color = "black",
                aes(x = ..x..),
                size = 1)

# E para a espécie virginica:

ggplot(mapping = aes(x = iris$Petal.Length[iris$Species == "virginica"])) +
  geom_histogram(fill = "steelblue",
                 color = "black",
                 aes(y = ..density..),
                 bins = 9) +
  labs(title = "Comprimento de pétalas de flores da espécie virginica",
       x = "Comprimento de pétalas",
       y = "Densidade de flores") +
  stat_function(fun = dnorm,
                args = list(mean = mean(iris$Petal.Length[iris$Species == "virginica"]),
                            sd = sd(iris$Petal.Length[iris$Species == "virginica"])),
                color = "black",
                aes(x = ..x..),
                size = 1)

# Agora com a ferramenta gráfica, em conjunto com os testes de hipóteses,
# podemos verificar que de fato a distribuição Normal representa bem os dados.


# Vamos estudar melhor como saber se uma flor é da especie versicolor
# ou virginica de acordo com o comprimento de petala. Para tal, vamos
# ajustar um modelo de regressao logistica.

# Criando base de dados apenas com as espécies versicolor e virginica:

irisAlternativa = iris |>
  filter(Species != "setosa")

# Criando uma variável dummy para representar as classes

irisAlternativa$Species_dummy <- as.numeric(irisAlternativa$Species == "virginica")

# Criando o gráfico de dispersão com a curva de ajuste de regressão logística

ggplot(irisAlternativa, aes(x = Petal.Length, y = Species_dummy)) +
  geom_point() +
  stat_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial"),
              size = 1.1, color = "darkblue") +
  labs(title = "Dispersão com a curva de ajuste de regressão logística",
       x = "Comprimento de pétala",
       y = "Probabilidade da espécie da flor ser virginica") +
  theme_bw()

# Com esse modelo de regressão logística, podemos avaliar a probabilidade
# de uma flor ser de uma das espécies a partir do comprimento de sua pétala.


# Criando o modelo agora fora do gráfico do ggplot2:

modelo2 = glm(Species_dummy ~ Petal.Length, 
              family = "binomial",
              data = irisAlternativa)


# Calculando, pelo modelo, qual a probabilidade de cada flor da base de dados
# ser da espécie virginica


newdata = data.frame(Petal.Length = 5.3)


predict(modelo2, 
        newdata = newdata,
        type = "response")


# Com isso, ajustamos um modelo de regressão logística útil para prever,
# no futuro, a espécie da flor caso só tenhamos acesso ao comprimento
# das pétalas da mesma.












