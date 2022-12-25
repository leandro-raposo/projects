
# Carregando a base Volcanodust - poeira sobre a poeira vulcânica entre 1500 e 1969
# Fonte: Hipel and Mcleod, 1994
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
plot.ts(volcanodustseries)

# Visualmente as flutuações aleatórias na série temporal são aproximadamente constantes em 
# tamanho ao longo do tempo, portanto, um modelo aditivo é provavelmente apropriado para descrever 
# essa série temporal. Além disso, a série temporal parece ser estacionária em média e variância, 
# pois seu nível e variância parecem ser aproximadamente constantes ao longo do tempo. Portanto, 
# não precisamos diferenciar esta série para ajustar um modelo ARIMA, mas podemos ajustar um modelo 
# ARIMA à série original (a ordem de diferenciação necessária, d, é zero aqui).

## Vamos avaliar o correlograma e correlograma parcial para defasagens 1-20 para investigar qual 
# modelo ARIMA usar:

acf(volcanodustseries, lag.max=20)             # plotando um correlograma
acf(volcanodustseries, lag.max=20, plot=FALSE) # captando os valores da autocorrelação

# As autocorrelações para as defasagens 1, 2 e 3 excedem os limites de significância, e que 
# as autocorrelações diminuem para zero após o atraso 3. As autocorrelações para as defasagens 
# 1, 2, 3 são positivas e diminuem em magnitude com o aumento lag (lag 1: 0,666, lag 2: 0,374, 
# lag 3: 0,162).

# A autocorrelação para defasagens 19 e 20 excede os limites de significância também, mas é provável 
# que seja devido ao acaso, uma vez que apenas excedem os limites de significância (especialmente 
# para defasagens 19), as autocorrelações para defasagens 4-18 não excedem os limites de significância, 
# e esperamos que 1 em cada 20 defasagens excedesse os limites de significância de 95% apenas 
# pelo acaso.

pacf(volcanodustseries, lag.max=20)
pacf(volcanodustseries, lag.max=20, plot=FALSE)

# A autocorrelação parcial no lag 1 é positiva e excede os limites de significância (0,666), enquanto a 
# autocorrelação parcial no lag 2 é negativa e também excede os limites de significância (-0,126). 
# As autocorrelações parciais diminuem para zero após o lag 2. Uma vez que o correlograma cai para zero 
# após o desfasamento 3, e o correlograma parcial é zero após o desfasamento 2, os seguintes modelos ARMA 
# são possíveis para a série temporal:
  
# Modelo ARMA (2,0), uma vez que o autocorrelograma parcial é zero após o lag 2, e o correlograma cai 
# para zero após o lag 3, e o correlograma parcial é zero após o lag 2
# Modelo ARMA (0,3), uma vez que o autocorrelograma é zero após o atraso 3, e o correlograma parcial 
# cai para zero (embora talvez de forma abrupta demais para que este modelo seja apropriado).
# Modelo misto ARMA (p, q), uma vez que o correlograma e o correlograma parcial diminuem para zero 
# (embora o correlograma parcial talvez diminua abruptamente para este modelo ser apropriado).
# Modelo ARMA (2,0) tem 2 parâmetros, o modelo ARMA (0,3) tem 3 parâmetros e o modelo ARMA (p, q) 
# tem pelo menos 2 parâmetros. Portanto, usando o princípio da parcimônia, o modelo ARMA (2,0) e o 
# modelo ARMA (p, q) são modelos candidatos igualmente bons.

# Modelo ARMA (2,0) é um modelo autoregressivo de ordem 2, ou modelo AR (2). Este modelo pode ser 
# escrito como: X_t - mu = (Beta1 * (X_t-1 - mu)) + (Beta2 * (Xt-2 - mu)) + Z_t, onde X_t é a série 
# temporal estacionária (série volcanodust), mu é a média das séries temporais X_t, Beta1 e Beta2 
# são parâmetros a serem estimados e Z_t é o ruído com média zero e variância constante.

# Modelo AR (autoregressivo) é geralmente usado para modelar uma série de tempo que mostra dependências 
# de longo prazo entre observações sucessivas. Intuitivamente, faz sentido que um modelo AR possa 
# ser usado para descrever a série temporal da base, já que esperaríamos que a poeira vulcânica e os 
# respectivos níveis em um ano afetassem aqueles em anos muito posteriores, uma vez que a poeira 
# são improváveis desaparecer rapidamente.

# Se o modelo ARMA (2,0) (com p = 2, q = 0) for usado para modelar a série temporal significaria que 
# um modelo ARIMA (2,0,0) pode ser usado ( com p = 2, d = 0, q = 0, onde d é a ordem de diferenciação
# necessária). Da mesma forma, se um modelo ARMA (p, q) misto é usado, onde p e q são ambos maiores que 
# zero, então um modelo ARIMA (p, 0, q) pode ser usado.

###### EXEMPLO 3 ###############################################################

# Vamos avaliar a previsão da base anterior - "volcanodust"

volcanodustseriesarima <- arima(volcanodustseries, order=c(2,0,0))
volcanodustseriesarima

# O modelo ARIMA (2,0,0) pode ser escrito como: X_t - mu = (Beta1 * (X_t-1 - mu)) + 
# (Beta2 * (Xt-2 - mu)) + Z_t, onde Beta1 e Beta2 são parâmetros a serem estimados. A saída da função 
# arima reporta que Beta1 e Beta2 são estimados como 0,7533 e -0,1268 aqui (dados como ar1 e ar2 na 
# saída).

# Com o ajuste do modelo ARIMA (2,0,0), podemos usar o modelo "forecast ()" para prever os valores 
# futuros do índice vulcânico. Os dados originais incluem os anos 1500-1969. Para fazer previsões 
# para os anos 1970-2000 (mais 31 anos):

volcanodustseriesforecasts <- forecast(volcanodustseriesarima, h=31)
volcanodustseriesforecasts

plot(volcanodustseriesforecasts) # visualização da série

# Atenção: o modelo previu valores negativos para o índice de poeira vulcânica, mas essa variável 
# só pode ter valores positivos! A razão é que as funções utilizadas não sabem que a variável só pode 
# assumir valores positivos. Claramente, o modelo preditivo atual não nos ajuda.

# Vamos investigar se os erros de previsão parecem estar correlacionados e se  são normalmente 
# distribuídos com média zero e variância constante. Para verificar as correlações entre erros de 
# previsão sucessivos, podemos fazer um correlograma e usar o teste Ljung-Box:

acf(volcanodustseriesforecasts$residuals, lag.max=20)
Box.test(volcanodustseriesforecasts$residuals, lag=20, type="Ljung-Box")

# O correlograma mostra que a autocorrelação no lag 20 excede os limites de significância. No entanto, 
# provavelmente se deve ao acaso, uma vez que esperaríamos que uma em 20 autocorrelações da amostra 
# excedesse os limites de significância de 95%. Além disso, o valor p para o teste Ljung-Box é 0,2, 
# indicando que há pouca evidência para autocorrelações diferentes de zero nos erros de previsão para 
# defasagens 1-20.

# Para verificar se os erros de previsão são normalmente distribuídos com média zero e variância 
# constante, fazemos um gráfico de tempo dos erros de previsão e um histograma:

plot.ts(volcanodustseriesforecasts$residuals)            # gráfico de tempos de erro de previsão
plotForecastErrors(volcanodustseriesforecasts$residuals)

# O gráfico de tempo dos erros de previsão mostra que os erros de previsão parecem ter uma variação 
# quase constante ao longo do tempo. No entanto, a série temporal de erros de previsão parece ter uma 
# média negativa, em vez de uma média zero. Podemos confirmar isso calculando o erro médio de previsão, 
# que acaba sendo cerca de -0,22:

# O histograma de erros de previsão mostra que embora o valor médio dos erros de previsão seja negativo, 
# a distribuição dos erros de previsão é inclinada para a direita em comparação com uma curva normal. 
# Portanto, parece que não podemos concluir que os erros de previsão são normalmente distribuídos 
# com média zero e variância constante! Portanto, é provável que o modelo ARIMA (2,0,0) para a série 
# temporal do índice de poeira vulcânica não seja o melhor modelo que poderíamos fazer e quase 
# definitivamente poderia ser melhorado!

###### EXEMPLO 4 ###############################################################

# Com a análise feita vamos propor um modelo (0,1,1)
kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1)) # fit an ARIMA(0,1,1) model
kingstimeseriesarima

# Com o modelo ARIMA (0,1,1) na série temporal estamos ajustando um modelo ARMA (0,1) à série 
# das primeiras diferenças. Um modelo ARMA (0,1) pode ser escrito X_t - mu = Z_t - (theta * Z_t-1), 
# onde theta é um parâmetro a ser estimado. A partir da saída da função R "arima ()", o valor 
# estimado de theta (dado como 'ma1' na saída R) é -0,7218 no caso do modelo ARIMA (0,1,1) ajustado à 
# série temporal de idades na morte de reis.

# Vamos agora prever as idades de morte dos próximos dez reis ingleses
kingstimeseriesforecasts <- forecast(kingstimeseriesarima, h=5)
kingstimeseriesforecasts

kingstimeseries # para verificar a idade da última observação

# A série inclui as idades de morte de 42 reis ingleses. A função forecast retornou a 
# previsão da idade de morte dos próximos cinco reis ingleses (reis 43-47), bem como 
# intervalos de predição de 80% e 95% para essas predições. A idade de morte do 42º rei inglês foi de 
# 56 anos (o último valor observado em nossa série temporal), e o modelo ARIMA fornece a idade prevista 
# para a morte dos próximos cinco reis como 67,8 anos.

# Para traçar as idades de morte observadas para os primeiros 42 reis, bem como as idades que 
# seriam previstas para esses 42 reis e para os próximos 5 reis usando nosso modelo ARIMA (0,1,1), 
# usamos:

plot(kingstimeseriesforecasts)

# Como no caso dos modelos de suavização exponencial seria interessante investigar se os erros 
# de previsão de um modelo ARIMA são normalmente distribuídos com média zero e variância constante, 
# e se existem correlações entre erros de previsão sucessivos.

acf(kingstimeseriesforecasts$residuals, lag.max=20) # avaliando o correlograma
Box.test(kingstimeseriesforecasts$residuals, lag=20, type="Ljung-Box") # teste de Ljung-Box

# Como o correlograma mostra que nenhuma das autocorrelações da amostra para defasagens 1-20 excede 
# os limites de significância, e o valor p para o teste Ljung-Box é 0,9, podemos concluir que há muito 
# pouca evidência para autocorrelações diferentes de zero no erros de previsão nas defasagens 1-20.

# Para investigar se os erros de previsão são normalmente distribuídos com média zero e variância 
# constante, podemos fazer um gráfico de tempo e histograma (com curva normal sobreposta) dos erros 
# de previsão:

plot.ts(kingstimeseriesforecasts$residuals) # avaliação dos resíduos
plotForecastErrors(kingstimeseriesforecasts$residuals)

# O gráfico de tempo dos erros de previsão dentro da amostra mostra que a variação dos erros 
# de previsão parece ser aproximadamente constante ao longo do tempo (embora talvez haja uma variação 
# ligeiramente maior para a segunda metade da série temporal). 
# O histograma da série temporal mostra que os erros de previsão são aproximadamente normalmente 
# distribuídos e a média parece ser próxima de zero. Portanto, é plausível que os erros de previsão 
# sejam normalmente distribuídos com média zero e variância constante. Uma vez que erros de previsão 
# sucessivos não parecem estar correlacionados, e os erros de previsão parecem ser normalmente 
# distribuídos com média zero e variância constante, o ARIMA (0,1,1) parece fornecer um modelo preditivo 
# adequado para as idades de morte de Reis ingleses.

###### EXEMPLO 5 ###############################################################

