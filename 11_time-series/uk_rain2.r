#################################################################################
#                          MODELAGEM E FORECAST                                 #
#################################################################################

# Transformações ###############################################################

## Antes de avaliarmos os modelos de previsão, vamos lembrar dois conceitos de 
# transformações:

# Diferenciação: usada para a série estacionária. 
## É a mudança da série, a diferença entre o valor e um valor anterior

autoplot(a10)
a10.diff <- diff(a10, 1)
autoplot(a10.diff) # Removeu a tendência
a10.diff2 <- diff(a10, 2)
autoplot(a10.diff2)
ndiffs(a10) # avaliar a quantidade de diferenciações necessárias

# BoxCox: se a diferenciação não for o bastante, pode ser usada a 
## transformação de BoxCox.

lambda <- BoxCox.lambda(a10)
a10.bc <- BoxCox(a10, lambda = lambda)
hist(a10) # antes
hist(a10.bc) # atual
autoplot(a10.bc)
ap1 <- autoplot(a10)
ap2 <- autoplot(a10.bc)
ap1 + ap2
ap1 / ap2

serie.final <- diff(a10.bc, 1)
autoplot(serie.final)

###############################################################################
### PREVISÃO COM ALISAMENTO EXPONENCIAL (EXPONENTIAL SMOOTHING)
###############################################################################

# Suavização Exponencial Simples

# Sendo possível descrever por meio do modelo aditivo com nível constante e sem sazonalidade.
# A suaviazação ocorre pelo parâmetro alfa entre 0 e 1. Sendo, 0 pouco peso nas observações
# mais recentes ao fazer previsões de valores futuros. 

###############################################################################

# Carregando a base chuva em Londres entre os anos de 1813-1912
# Fonte: Hipel and McLeod, 1994

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)

# A média permanece, quase, constante em aproximadamente em 25, o que indica o uso 
# de um modelo aditivo. 

# Vamos usar a função HoltWinters() para isso é preciso definir os parâmetros beta e gamma.
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts

# O valor estimado do parâmetro alfa é de 0.024. Como o valor é próximo a zero a previsão
# está baseada em observações recentes e menos recentes.Por default a previsão é feita apenas
# para o mesmo período avaliado na série temporal. Logo, entre os anos de 1813-1912.

rainseriesforecasts$fitted # avaliandos os valores estimados
plot(rainseriesforecasts)

# Como medida de previsão calculamos o erro da soma dos quadrados para os erros de previsão dentro
# da amostra. 

rainseriesforecasts$SSE # o valor do erro da soma dos quadrados
HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56) # utilizando o primeiro valor previsto

# Vamos prever um período além da série original
rainseriesforecasts2 <- forecast(rainseriesforecasts, h=8)
rainseriesforecasts2
plot(rainseriesforecasts2) # plotando o gráfico para verificar a previsão

# Os erros da previsão são calculados entre os valores observados menos os valores previstos
# Caso o modelo preditivo não possa ser aprimoraado, provavelmente não deve haver correlação entre os erros de
# previsão para as previsões sucessivas. Assim, outra técnica seria melhor empregada.

# Para avaliar usaremos o correlograma.
rainseriesforecasts2$residuals
acf(rainseriesforecasts2$residuals, lag.max = 20, na.action = na.pass)

# Vamos avaliar a significância por meio do teste Ljung-Box
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")

# Há pouca evidência de autocorrelação diferentes de zero nos erros de previsão dentro da amostra.
# Para garantir que seria o melhor modelo vamos verificar os erros de previsão são normalmente distribuídos
# ou seja, média e variância constante.

plot.ts(rainseriesforecasts2$residuals)

# Para avaliar vamos gerar um histograma
plotForecastErrors <- function(forecastErrors)
{
  forecastErrorsSd <- sd(x = forecastErrors,
                         na.rm = TRUE)
  forecastErrorsMin <- min(forecastErrors,
                           na.rm = TRUE) - forecastErrorsSd * 5
  forecastErrorsMax <- max(forecastErrors,
                           na.rm = TRUE) + forecastErrorsSd * 3
  forecastErrorsNorm <- rnorm(n = 10000,
                              mean = 0,
                              sd = forecastErrorsSd)
  binMin <- min(forecastErrorsMin, forecastErrorsNorm)
  binMax <- max(forecastErrorsMax, forecastErrorsNorm)
  binBreaks <- IQR(x = forecastErrors,
                   na.rm = TRUE) / 4
  bins <- seq(from = binMin,
              to = binMax,
              by = binBreaks)
  hist(x = forecastErrors,
       col = "#DCE319FF",
       freq = FALSE,
       breaks = bins)
  with(data = hist(x = forecastErrorsNorm,
                   plot = FALSE,
                   breaks = bins),
       expr = lines(x = mids,
                    y = density,
                    col = "#440154FF",
                    lwd = 3))
}

# plotando o histograma dos erros de previsão
plotForecastErrors(rainseriesforecasts2$residuals)

# O gráfico demonstra que a distribuição dos erros está centrada em zero e aproximadamente distribuída. 
# O teste Ljung-Box mostrou que há pouca evidência de autocorrelações diferentes de zero.
# O método de suavização exponencial simples fornece um modelo preditivo adequado



###### EXEMPLO 2 ###############################################################

