# Leitura dos dados
library(ggplot2)
dados <- read.table("/home/alan/Github/INE5649-StatisticalPredictionTechniques/carros.txt", # modificar o caminho
                    stringsAsFactors = T, # strings são fatores
                    header=T) # primeira linha do arquivo são os rótulos das variáveis
head(dados) # imprimir início do data frame

# Construção do diagrama de dispersão
dados <- data.frame(dados, # arrumando o data frame
                    xtr = log(dados$valor), # adicionando a coluna xtr com ln(x)
                    ytr = log(dados$ano)) # adicionando a coluna ytr com ln(y)

g2 <- ggplot(data = dados,
             aes(x = valor, 
                 y = ytr)) + #indicação de ln(y) como variável dependente
  geom_point() +
  labs(x = 'Valor', 
       y = 'logaritmo de Ano') +
  theme_minimal()
g2

# modelo de regressão linear simples
modelo2 <- lm(ytr ~ valor, # ln(y) ~ x
              data=dados)
summary(modelo2)

g2 + geom_smooth(method = 'lm', se=F)

ggplot(data = modelo2) + 
  geom_point(aes(x=.fitted, 
                 y=.stdresid)) +
  geom_hline(yintercept = 0) +
  labs(x = 'Valores preditos', 
       y = 'Resíduos padronizados') + 
  theme_minimal()

ggplot(data = modelo2, 
       aes(sample = .stdresid)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(x = 'Valores esperados pela normal', 
       y = 'Resíduos padronizados') +
  theme_minimal()

ggplot(data = modelo2) + 
  geom_histogram(aes(x = .stdresid),
                 bins = 5,
                 fill = 'lightgrey',
                 colour = 'black') + 
  labs(x = 'Resíduos padronizados', 
       y = 'Frequência') +
  theme_minimal()

shapiro.test(rstandard(modelo2))

#=== Y=\beta_0 X_i^(\beta_1)
# Construção do diagrama de dispersão
g3 <- ggplot(data = dados,
             aes(x = xtr, #indicação de ln(x) como variável independente
                 y = ytr)) + #indicação de ln(y) como variável dependente
  geom_point() + 
  labs(x = 'logaritmo da Valor', 
       y = 'logaritmo de Ano') +
  theme_minimal() 
g3

# modelo de regressão linear simples
modelo3 <- lm(ytr ~ xtr, # ln(y) ~ ln(x)
              data=dados)
summary(modelo3)

g3 + geom_smooth(method = 'lm', se=F)

ggplot(data = modelo3) + 
  geom_point(aes(x=.fitted, 
                 y=.stdresid)) +
  geom_hline(yintercept = 0) +
  labs(x = 'Valores preditos', 
       y = 'Resíduos padronizados') + 
  theme_minimal()

ggplot(data = modelo3, 
       aes(sample = .stdresid)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(x = 'Valores esperados pela normal', 
       y = 'Resíduos padronizados') +
  theme_minimal()

ggplot(data = modelo3) + 
  geom_histogram(aes(x = .stdresid),
                 bins = 5,
                 fill = 'lightgrey',
                 colour = 'black') + 
  labs(x = 'Resíduos padronizados', 
       y = 'Frequência') +
  theme_minimal()

shapiro.test(rstandard(modelo3))

# Predição para um novo valor: X=3
pred.tr <- predict(modelo3, 
                   newdata = data.frame(xtr = log(3)))
pred <- exp(pred.tr); pred # Valor predito
