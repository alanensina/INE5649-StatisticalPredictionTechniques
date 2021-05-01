# Leitura dos dados
library(ggplot2)
dados <- read.table("/home/alan/Github/INE5649-StatisticalPredictionTechniques/carros.txt", # modificar o caminho
                    stringsAsFactors = T, # strings são fatores
                    header=T) # primeira linha do arquivo são os rótulos das variáveis
head(dados) # imprimir início do data frame

g2 = ggplot(data=dados, 
            aes(x=valor,y=quilometragem))+
  geom_point()+
  labs(x = 'Valor', y = 'KM') +
  theme_minimal()

modelo2 = lm(dados$valor~dados$quilometragem)
coef(modelo2)
g2 + geom_smooth(method='lm', se=F)
summary(modelo2)

# gráfico de dispersão dos resíduos padronizados
ggplot(data = modelo2) + # função principal, utilizando como data o modelo ajustado
  geom_point(aes(x=.fitted, # valores preditos
                 y=.stdresid)) + # resíduos padronizados
  geom_hline(yintercept = 0) + # reta em y=0
  labs(x = 'Valores preditos', # nomeação dos eixos
       y = 'Resíduos padronizados') + 
  theme_minimal()

# qq plot
ggplot(data = modelo2, # utilizando como data o modelo ajustado
       aes(sample = .stdresid)) + # na estética utilizar os resíduos padronizados em sample
  stat_qq() + # construir os pontos do gráfico
  stat_qq_line() + # construir a linha do gráfico
  labs(x = 'Valores esperados pela normal', # nomeação dos eixos
       y = 'Resíduos padronizados') +
  theme_minimal()

# histograma dos resíduos padronizados
ggplot(data = modelo2) + # utilizando como data o modelo ajustado
  geom_histogram(aes(x = .stdresid), # resíduo padronizado
                 bins = 5, # quantidade de barras do histograma
                 fill = 'lightgrey', # preenchimento
                 colour = 'black') + # cor das bordas
  labs(x = 'Resíduos padronizados', # nomeação dos eixos
       y = 'Frequência')

# teste de Shapiro-Wilk
shapiro.test(rstandard(modelo2)) # a função rstandard() calcula os resíduos padronizados
