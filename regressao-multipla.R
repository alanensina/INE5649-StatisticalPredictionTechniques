# Leitura dos dados
library(ggplot2)
dados <- read.table("/home/alan/Github/INE5649-StatisticalPredictionTechniques/carros.txt", # modificar o caminho
                    stringsAsFactors = T, # strings são fatores
                    header=T) # primeira linha do arquivo são os rótulos das variáveis
head(dados) # imprimir início do data frame

# Modelo de regressão linear múltipla
modeloX <- lm(valor ~ ano + quilometragem + motor + modelo, data=dados) 
coef(modeloX) # imprimir coeficientes do modelo

summary(modeloX)
confint(modeloX)
anova(modeloX)