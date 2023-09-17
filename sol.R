#R.version.string "R version 4.2.1 (2022-06-23 ucrt)"

#Definindo diretório de trabalho
wd <- "C:/Users/Computador/Documents/Cursos/DSA/cientista_de_dados/projects/previsao_consumo_energia"
setwd(wd)
getwd()

#Instalação pacotes
if (system.file(package="readxl") == "") install.packages("readxl")
if (system.file(package="ggplot2") == "") install.packages("ggplot2")
if (system.file(package="tidyverse") == "") install.packages("tidyverse")
if (system.file(package="corrplot") == "") install.packages("corrplot")

#Carregando pacotes
library(readxl)
library(ggplot2)
library(tidyverse)
library(corrplot)

#Carregando dados
df <- read_excel('dataset/FEV-data-Excel.xlsx')
View(df)

### Limpeza dos dados ###

#Renomeando colunas
colnames(df) <- c(
  "name",
  "make",
  "model",
  "min_price_PLN",
  "engine_power_KM",
  "max_torque_Nm",
  "brake_type",
  "traction_type",
  "battery_capacity_kWh",
  "range_WLTP_km",
  "wheelbase_cm",
  "length_cm",
  "width_cm",
  "height_cm",
  "min_empty_weight_kg",
  "permissable_gross_weight_kg",
  "max_load_capacity_kg",
  "seats",
  "doors",
  "tire_size_in",
  "max_speed_kph",
  "boot_capacity_VDA_L",
  "acceleration_to_100_kph_s",
  "max_DC_charging_power_kW",
  "mean_energy_consumption_kWh_per_100_km"
)

## Tratamento de dados missing

#Verificando colunas com dados missing
unlist(lapply(df, function(x) sum(is.na(x))))

#remover observações com dados missing na variável alvo
df <- df[!is.na(df$mean_energy_consumption_kWh_per_100_km), ]
unlist(lapply(df, function(x) sum(is.na(x))))

#imputação dos dados

#sobraram apenas 4 variáveis sem dados e conseguimos as informações 
#facilmente com uma rápida pesquisa
#mercedes com freio a disco nas duas rodas e volume porta-malas: 
#https://www.ultimatespecs.com/br/carros-ficha-tecnica/Mercedes-Benz/120227/Mercedes-Benz-EQV-300-Lang.html
#aceleração mercedes: 
#https://ev-database.org/car/1240/Mercedes-EQV-300-Long
#aceleração nissan: 
#https://ev-database.org/car/1117/Nissan-e-NV200-Evalia
df[43, "brake_type"] <- "disc (front + rear)"
df[43, "boot_capacity_VDA_L"] <- 1030
df[43, "acceleration_to_100_kph_s"] <- 12.1
df[44, "acceleration_to_100_kph_s"] <- 14.0

any(is.na(df)) #sem dados missing

#Verificando tipos de dados
str(df)

#Convertendo variaveis catergóricas para factors
df$make <- as.factor(df$make)
df$brake_type <- as.factor(df$brake_type)
df$traction_type <- as.factor(df$traction_type)

### Análise exploratória ###

#Analise de dados
summary(df)

## Categoricas
# Marca
#Temos 19 marcas diferentes que não são distribuidas de forma balanceada 
#e não parecem ter uma relação clara com o consumo de energia
#Qtde de marcas
length(summary(df$make))

#qtde de observações por marca
barplot(
  sort(summary(df$make), decreasing = T), 
  las=2
)

#distribuição do consumo de energia por marca
df %>%
  ggplot( aes(x=make, y=mean_energy_consumption_kWh_per_100_km)) +
  geom_boxplot() +
  coord_flip()

# Freio
#2 tipos de freios, também desbalanceados, 
#carros com discos apenas na dianteira parecem 
#ter um consumo menor
#qtde de tipos de freio
length(summary(df$brake_type))

#Observações por tipo de freio
barplot(
  sort(summary(df$brake_type), decreasing = T),
  las=2
)

#consumo por tipo de freio
df %>%
  ggplot( aes(x=brake_type, y=mean_energy_consumption_kWh_per_100_km)) +
  geom_boxplot()

# Tração
# 3 tipos diferentes de tração e veiculos com tração nas 4 rodas parecem 
# ter consumo maior
# quantidade de tipo de tração
length(summary(df$traction_type))

#observações por tipo de tração
barplot(
  sort(summary(df$traction_type), decreasing = T),
  las=2
)

#consumo por tipo de tração
df %>%
  ggplot( aes(x=traction_type, y=mean_energy_consumption_kWh_per_100_km)) +
  geom_boxplot()


#plotando a distribuição das variáveis númericas
#encontramos outliers em algumas variáveis
col_numericas <- unlist(lapply(df, is.numeric), use.names = FALSE)

for (i in 1:length(names(df))) {
  if(col_numericas[i]) {
    hist(
      unlist(df[, i]),
      main = paste("Histogram of", colnames(df)[i])
    )
  }
}

#Verificando correlação entre atributos
cor_matrix <- cor(df[, col_numericas])

#Variável alvo possui forte correlação com todos os atributos com
#excessão de range, seats, doors
#Além disso o gráfico de correlação permite observar que as variáveis
#tem forte correlação entre si, apresentando multicolinearidade
corrplot(
  cor_matrix,
  method = 'color',
  addCoef.col = 'springgreen2', 
  tl.col = "black",
  tl.cex = 0.7,
  tl.pos = 'l',
  number.cex = 0.5
)

#Criando variável potência por peso
df$power_to_weight <- df$engine_power_KM / df$min_empty_weight_kg
any(is.na(df$power_to_weight))

col_numericas <- unlist(lapply(df, is.numeric), use.names = FALSE)
hist(
  unlist(df$power_to_weight),
  main = paste("Histogram of", "Power to weight")
)

#Com matriz de correlação podemos observar que a nova variável tem forte 
#correlação com várias outras relacionadas a potência, peso e aceleração
cor_matrix <- cor(df[, col_numericas])
corrplot(
  cor_matrix,
  method = 'color',
  addCoef.col = 'springgreen2', 
  tl.col = "black",
  tl.cex = 0.7,
  tl.pos = 'l',
  number.cex = 0.5
)

ggplot(df, aes(x=power_to_weight, y=mean_energy_consumption_kWh_per_100_km)) + 
  geom_point()


#Normalização
#Função para normalização utilizando método Min-Max
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

for (i in 1:length(names(df))) {
  if(col_numericas[i]) {
    df[, i] <- lapply(df[, i], min_max_norm)
  }
}

#Agora com os dados normalizados, vamos verificar estatisticamente se há
#diferença significativa nos freios e tração
## Tração
tracao_awd <- df$mean_energy_consumption_kWh_per_100_km[df$traction_type == '4WD']
tracao_rwd <- df$mean_energy_consumption_kWh_per_100_km[df$traction_type == '2WD (rear)']
tracao_fwd <- df$mean_energy_consumption_kWh_per_100_km[df$traction_type == '2WD (front)']

#teste de distribuição normal
#tração traseira não segue uma distribuição normal
shapiro.test(tracao_awd) #p-value: 0.5688
shapiro.test(tracao_rwd) #p-value: 0.8897
shapiro.test(tracao_fwd) #p-value: 1.738e-05

#verificando o histograma podemos ver 3 outliers
hist(tracao_fwd)

#Utilizando teste não paramétrico de variância Kruskal-Wallis
#p-value = 0.0001366, rejeitamos a hipotese nula, portanto há diferença 
#significativa no consumo entre pelo menos 1 tipo de tração em relação as outras
kruskal.test(mean_energy_consumption_kWh_per_100_km ~ traction_type,
             data = df)

##Freios
discos_quatro <- df$mean_energy_consumption_kWh_per_100_km[df$brake_type == 'disc (front + rear)']
disco_dianteira <- df$mean_energy_consumption_kWh_per_100_km[df$brake_type != 'disc (front + rear)']

#teste de distribuição normal
#disco somente na dianteira não segue uma distribuição normal
shapiro.test(discos_quatro) #p-value: 0.001911
shapiro.test(disco_dianteira) #p-value: 0.9765

#Utilizando teste de Wilcoxon para comparar os dois grupos de freio
#p-value: 0.07231, falhamos em rejeitar a hipostese nula, portanto,
#não há diferença significativa no consumo entre os tipos de freio
wilcox.test(mean_energy_consumption_kWh_per_100_km ~ brake_type,
            data=df)


### Regressão ###

# Primeiro modelo com todas as variáveis 
# R²: 0,9773 - Diversas variáveis com valor p maior que 0,05, 
# indicando baixa significancia estatisticas das variáveis
model <- lm(
  mean_energy_consumption_kWh_per_100_km ~ . 
  - name 
  - make
  - model,
  data = df
)
summary(model)

# Segundo modelo utilizando apenas variáveis com valor p menor que 0,05
# com objetivo de tornar o modelo mais generalista
# R²: 0,8943

model2 <- lm(
  mean_energy_consumption_kWh_per_100_km
  ~ traction_type
  + battery_capacity_kWh
  + range_WLTP_km
  + tire_size_in,
  data = df
)
summary(model2)

# Terceiro modelo utilizando relação de potencia por peso, dimensões do veiculo
# preço e tração
# R² 0.9113
# Modelo mais generalista sem alta correlação entre variáveis preditoras

model3 <- lm(
  mean_energy_consumption_kWh_per_100_km
  ~ traction_type
  + min_price_PLN
  + power_to_weight
  + range_WLTP_km
  + length_cm
  + width_cm
  + height_cm
  + tire_size_in,
  data = df
)

summary(model3)