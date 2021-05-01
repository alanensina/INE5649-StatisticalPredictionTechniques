# Leitura dos dados
library(ggplot2)
dados <- read.table("/home/alan/Github/INE5649-StatisticalPredictionTechniques/carros.txt", # modificar o caminho
                    stringsAsFactors = T, # strings são fatores
                    header=T) # primeira linha do arquivo são os rótulos das variáveis
head(dados) # imprimir início do data frame

# Histograma da variável resposta
ggplot(data=dados, aes(valor)) + 
  geom_histogram()

# Shapiro test de todas as variáveis
shapiro.test(dados$valor)
shapiro.test(dados$ano)
shapiro.test(dados$quilometragem)
shapiro.test(dados$motor)
shapiro.test(dados$modelo)

# Gráficos das variáveis em estudo (cada variável explicativa versus variável resposta)
# Análise regressão linear simples entre Valor e Ano
g1 = ggplot(data=dados, 
            aes(x=valor,y=ano))+
  geom_point() +
  labs(x = 'Valor', y = 'Ano') +
  theme_minimal()

modelo1 = lm(dados$valor~dados$ano)
coef(modelo1)
g1 + geom_smooth(method='lm', se=F)
summary(modelo1)
cor.test(dados$valor,dados$ano)

# Análise regressão linear simples entre Valor e Ano
g2 = ggplot(data=dados, 
            aes(x=valor,y=quilometragem))+
  geom_point()+
  labs(x = 'Valor', y = 'KM') +
  theme_minimal()

modelo2 = lm(dados$valor~dados$quilometragem)
coef(modelo2)
g2 + geom_smooth(method='lm', se=F)
summary(modelo2)
cor.test(dados$valor,dados$quilometragem)

# Leitura das variáveis indicadoras
dados$motor <- factor(dados$motor)
dados$modelo <- factor(dados$modelo)

# Criação do gráfico boxplot entre Valor e Motor
ggplot(data=dados, 
       aes(x=motor,y=valor, fill = motor))+
  geom_boxplot(show.legend = F) +
  labs(x="Motor", y = "Valor") +
  theme_minimal()

# Análise regressão linear simples entre Valor e Motor
modelo3 = lm(dados$valor~dados$motor)
coef(modelo3)
anova(modelo3)

# Criação do gráfico boxplot entre Valor e Modelo
ggplot(data=dados, 
       aes(x=modelo,y=valor, fill = modelo))+
  geom_boxplot(show.legend = F) +
  labs(x="Modelo", y = "Valor") +
  theme_minimal()

# Análise regressão linear simples entre Valor e Motor
modelo4 = lm(dados$valor~dados$modelo)
coef(modelo4)
anova(modelo4)