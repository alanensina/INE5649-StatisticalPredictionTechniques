# Leitura dos dados
dados <- read.table("/home/alan/Downloads/carros.txt", # modificar o caminho
                    stringsAsFactors = T, # strings são fatores
                    header=T) # primeira linha do arquivo são os rótulos das variáveis
head(dados) # imprimir início do data frame

# Gráficos 2 a 2 das variáveis em estudo (cada variável explicativa versus variável resposta)
library(ggplot2)
ggplot(data=dados, 
       aes(x=ano,y=valor))+
  geom_point() +
  labs(x = 'Ano', y = 'Valor (reais)') +
  theme_minimal()

ggplot(data=dados, 
       aes(x=quilometragem,y=valor))+
  geom_point()+
  labs(x = 'Quilometragem', y = 'Valor (reais)') +
  theme_minimal()

dados$motor <- factor(dados$motor)
dados$modelo <- factor(dados$modelo)

ggplot(data=dados, 
       aes(x=motor,y=valor, fill = motor))+
  geom_boxplot(show.legend = F) +
  labs(x="Motor", y = "Valor") +
  theme_minimal()

ggplot(data=dados, 
       aes(x=modelo,y=valor, fill = modelo))+
  geom_boxplot(show.legend = F) +
  labs(x="Modelo", y = "Valor") +
  theme_minimal()

# Ajuste do modelo de regressão linear múltipla
modelo <- lm(valor ~ ano + quilometragem + motor + modelo, # após o '~' adicionamos todas as variáveis explicativas separadas por '+'
             data=dados) # nome do objeto onde encontram-se as variáveis
coef(modelo) # imprimir coeficientes do modelo

summary(modelo)

anova(modelo)
