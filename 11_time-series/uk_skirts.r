
# Holt's Suavização Exponencial

# Usado quando é possível utilizar um modelo aditivo com acréscimo ou decréscimo na tendência e sazonalidade
# O método estima o nível e a inclinação no ponto de tempo atual e é controlada por dois parâmetros alfa (ponto atual)
# e beta para inclinação do componente da tendência no ponto do tempo atual.
# Alfa e beta terão valores entre 0 e 1, sendo que próximo a zero temos pouco peso nas previsões mais recentes.

###############################################################################

# Carregando a base skirts - diâmetro anual das saias femininas na bainha, de 1866 a 1911
# Fonte: McLeod, 1994

skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
# save(skirts, file = "skirts.RData")
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)

# É preciso configurar os parâmetros gama
skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts

# O valor do alpha foi de 0.83 e beta 1. Os valores são altos e indicam a estimativa do valor atual do nível,
# quando a inclinação do componente de tendência se baseiam principalmente em observações recentes da série.

skirtsseriesforecasts$SSE

# Assim, o nível e a inclinação mudam ao longo do tempo. O valor da soma dos erros quadrados é 16954.

plot(skirtsseriesforecasts) # atenção para lag antes dos dados observados na previsão.
skirtsseries

# para corrigir o nível do valor inicial, e a diferença entre a segunda e a primeira observação
HoltWinters(skirtsseries, gamma=FALSE, l.start=608, b.start=9) 

# Prevendo 19 pontos a mais que a série temporal
skirtsseriesforecasts2 <- forecast(skirtsseriesforecasts, h=19)

# linha azul representa com intervalos de predição de 80% com uma área sombreada em azul escuro e os 
# intervalos de predição de 95% com a área na cor clara 
plot(skirtsseriesforecasts2)

# skirtsseriesforecasts2 <- ts(skirts, start = 2021, frequency = 12) # definindo a frequência
# skirtsseriesforecasts2
# skirt.fcast <- hw(skirtsseriesforecasts2,h=19)

# Vamos avalair se o modelo preditivo pode ser melhorado ao verificar os erros de previsão na amostra
# mostram autocorrelações diferentes de zero nas defasagens de 1-20. 

acf(skirtsseriesforecasts2$residuals, lag.max=20, na.action = na.pass)

# O correlograma mostrou que a autocorrelação da amostra para os erros de previsão dentro da amostra no 
# defasamento 5 excede os limites de significância. Porém, é esperado que uma em cada 20 das autocorrelações 
# para os primeiros vinte atraso exceda o limite de significância de 95%. 

Box.test(skirtsseriesforecasts2$residuals, lag=20, type="Ljung-Box")
# O teste retornou um p valor de 0.47, indicando que há pouca evidência de autocorrelações diferentes de zero 
# nos erros de previsão dentro da amostra nas defasagens 1-20.

plot.ts(skirtsseriesforecasts2$residuals) # gerando um time plot

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
plotForecastErrors(skirtsseriesforecasts2$residuals) # gerando um histograma

# O gráfico demonstra que os erros de previsão têm uma variância quase constante ao longo do tempo. O histograma
# de erros de previsão mostra que é plausível que os erros de previsão sejam normalmente distribuídos com méda
# zero e variância constante.

# O Teste de Ljung-Box mostra que há pouca evidência de autocorrelações nos erros de previsão, enquanto que o 
# time plot e o histograma dos erros de previsão mostram que é plausível que os erros de previsão sejam
# normalmente distribuídos com média zero e variância constante. Logo, é possível concluir que a suavização
# exponencial de Holt fornece um modelo preditivo adequado para os parâmetros avaliados, e que provavelmente
# não pode ser melhorado. Além disso, significa que as suposições nas quais os intervalos de predições de 
# 80% e 95% são validas

###############################################################################

# Holt-Winters Suavização Exponencial

# Caso tenha uma série que pode ser descrita por meio de modelos aditivos, tendência crescente
# ou decrescente e sazonalidade, o uso da suavização exponencial de Holt-Winders é indicada 
# para previsões de curto prazo

# Estima o nível, inclinação e componente sazonal no ponto de tempo atual. A suavização é
# controlada por três parâmetros: alfa, beta e gama para estimar o nível, inclinação e o 
# componente de tendência e sazonal a partir do ponto atual. Os parâmetros variam entre 0 e 1.
# Valores próximos a 0 significam que é colocado relativamente pouco peso nas observações mais 
# recentes ao fazer as previsões.

###############################################################################

# Carregando a base Souvenir - venda de souvernirs entre Janeiro de 1987 a Dezembro de 1993
# Fonte: Wheelwright and Hyndman, 1998

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat") # carregando a base (caso não esteja carregada)
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))  # salvando o período de início, mês 01 de 1987

logsouvenirtimeseries <- log(souvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
souvenirtimeseriesforecasts

# Os valores estimados de alfa, beta e gama são 0.41, 0.00 e 0.95. O alfa é relativamente baixo
# indicando que a estimativa do nível no momento atual é baseada em observações no passado mais
# distante. O valor de beta indica que a estimativa da inclinação b do componente de tendência não
# é atualizado ao longo da série temporal e, em vez disso, é definida igual ao valor inicial. Assim,
# o nível muda bastante ao longo da série temporal, mas a inclinaçào do componente de tendência
# permanece praticamente a mesma. Já o valor gama é alto, indicandp que a estimativa do componente
# sazonal no momento atual é baseada apenas em observações recentes.

plot(souvenirtimeseriesforecasts)

# A técnica consegue prever os picos sazonais que ocorrem nos meses finais do ano.
# Vamos agora prever períodos que não estão na base, ou seja, de 1994 a 1998 (48 m)

souvenirtimeseriesforecasts2 <- forecast(souvenirtimeseriesforecasts, h=48)
plot(souvenirtimeseriesforecasts2)

# As previsões são mostradas com uma linha azul e as áreas sombreadas em cores claras e escuras
# mostram intervalos de previsão de 80% a 95%. Para avaliar se o modelo melhorado verificando se
# os erros de previsão na amostra mostram autocorrelações diferentes de zero nas defasagens 1-20, 
# vamos realizar o correlograma e o teste de Ljung e Box.

acf(souvenirtimeseriesforecasts2$residuals, lag.max=20, na.action = na.pass)
Box.test(souvenirtimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")

# O correlograma apresenta que as autocorrelações para os erros de previsão dentro da amostra
# não excedem os limites de significância para defasagem 1-20. O valor p para o teste Ljunb-Box
# e 0,6, indicando que há pouca evidência de autocorrelações diferentes de zero nas defasagens. 

# Os erros de previsão têm variância constante ao longo do tempo e são normalmente distribuídos
# com a média zero, fazendo um gráfico de tempo de erros de previsão e um histograma.

plot.ts(souvenirtimeseriesforecasts2$residuals)            # gerando um time plot

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
       col = "red",
       freq = FALSE,
       breaks = bins)
  with(data = hist(x = forecastErrorsNorm,
                   plot = FALSE,
                   breaks = bins),
       expr = lines(x = mids,
                    y = density,
                    col = "blue",
                    lwd = 2))
}
  
plotForecastErrors(souvenirtimeseriesforecasts2$residuals)  # gerando um histograma

# É compreensível que os erros de previsão tenham variação constante ao longo do tempo. A partir do histograma de erros 
# de previsão, os erros de previsão parecem ser normalmente distribuídos com média zero. Assim, há pouca evidência de 
# autocorrelação nas defasagens 1-20 para os erros de previsão, e os erros de previsão parecem ser normalmente distribuídos 
# com média zero e variância constante ao longo do tempo. Assim, a suavização exponencial de Holt-Winters fornece um modelo 
# preditivo adequado e que provavelmente não pode ser melhorado. Além disso, as suposições nas quais os intervalos de 
# predição foram baseados são provavelmente válidas.
