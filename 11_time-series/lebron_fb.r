# Passo a passo, Box e Jenkis (1976)
# 1: Plotar a série e examinar;
# 2: Diferenciar a série até ficar estacionária e fazer transformações, se necessário;
# 3: Usar séries diferenciadas para definir p e q;
# 4: Implementar o Arima nos dados originais;
# 5: Checar se é um bom modelo;
# 6: Usar o modelo para fazer previsões.

# Passo 1
autoplot(a10)
dec <- decompose(a10)
autoplot(dec)

# Passo 2 Uma diferenciação e transformação de BoxCox
autoplot(serie.final) # d = 1 e BC

# Passo 3
pacf(serie.final)  #p=1 e 1, função de autocorrelação parcial
# p = autoregressivo, valores anteriores
acf(serie.final)  #q=1 e 1, função de autocorrelação
ndiffs(a10) # para avaliar as diferenciações
nsdiffs(a10) # para avaliar diferenças sazonais
# d=1 e 1

# Passo 4
mod.arima <- Arima(a10, order = c(1, 1, 1), seasonal = c(1, 1, 1), lambda = lambda)
# usamos o lambda devido a transformação de BoxCox
summary(mod.arima)
autoplot(a10) + autolayer(mod.arima$fitted)

# modelo ficou melhor no início da série

# Passo 5
checkresiduals(mod.arima)

# Passo 6
prev.arima <- forecast(mod.arima, h = 12)
autoplot(prev.arima)

## Auto-Arima - automatiza os melhores parâmetros, p e q do Arima
# Passo a passo
# Passo 1: Executar Auto-Arima para verificar os melhores parâmetros;
# Passo 2: Implementar o Arima nos dados originais;
# Passo 3: Checar se é um bom modelo;
# Passo 4: Usar o modelo para fazer previsões.

# Passo 1
auto.arima(a10, lambda = lambda, trace = TRUE, approximation = FALSE)  #ARIMA(3,0,0)(2,1,1)

# Passo 2
mod.aa <- Arima(a10, order = c(3, 0, 0), seasonal = c(2, 1, 1), lambda = lambda,
                include.drift = TRUE)
# Passo 3
checkresiduals(mod.aa)

# Passo 4
prev.aa <- forecast(mod.aa, h = 12)
autoplot(prev.aa)

# PROPHET ######################################################################
# Detalhes podem ser acessados em: https://facebook.github.io/prophet/ #########
################################################################################

# Base - quantidade de visualizações no perfil do Wikipedia do Lebron James
# Fonte: https://pageviews.toolforge.org/

# Fases: 
# 1. Explorando os dados
# 2. Predições Básicas
# 3. Inspecionando Componentes do Modelo
# 4. Personalizando feriados e eventos

# 1. Explorando os dados
colnames(lebron) <- c("ds", "y")  # renomeando as colunas
head(lebron) # visualizando a base
lebron$y <- log10(lebron$y) # aplicando logaritmo base 10 na variável y
View(summary(lebron)) # explorando os dados
plot(y ~ ds, lebron, type = "l") # gráfico da série

# 2. Predições Básicas
m <- prophet(lebron)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)
plot(m, forecast) # visualização simples para compreensão

# Avaliação por valores brutos com o valor previsto por dia e intervalos de incerteza
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
tail(forecast) # previsão por componentes

# 3. Inspecionando Componentes do Modelo
prophet_plot_components(m, forecast)

####### Avaliação por ARIMA e Prophet ##########################################

# ARIMA ########################################################################

data(AirPassengers) 
AirPassengers
plot(AirPassengers, ylab="Passengers", type="o", pch =20) # visualizando a série

# Separação entre treino e teste (período de dois anos)
df_train<- window(AirPassengers, end = c(1958, 12))
df_test <- window(AirPassengers, start = c(1959, 01))

# A partir do fluxio de Box e Jenkins é possível concluir que a variância não é constante, 
# porque aumenta e muda com o tempo, portanto a transformação do log é necessária. Além disso, 
# esta série temporal não é estacionária em média, considerando a sazonalidade, portanto, a 
# diferença de sazonalidade é necessária.

ggtsdisplay(diff(log(AirPassengers), 12)) # avaliação da autocorrelação

# ACF e PACF sugerem um modelo auto regressivo de ordem 2 e um modelo MA de ordem 1. 
# Assim, o modelo ARIMA (2,0,0) (0,1,1) é selecionado e é treinado com o conjunto de treinamento. 
# Dois parâmetros são definidos: include.constant e lambda. O primeiro adiciona ao modelo a interceptação. 
# O outro, em vez disso, define a transformação do log.

arima_1 <- Arima(df_train, c(2, 0, 0), c(0, 1, 1), include.constant = TRUE, lambda = 0)

ggtsdisplay(arima_1$residuals)

# Não há uma autocorrelação automática significativa entre as defasagens. O modelo 
# pode prever os últimos dois anos.

arima_f <- forecast(arima_1, 24)
forecast(arima_1, 24) %>% autoplot()

# Vamos avaliar o modelo com o RMSE, MAE e MAPE.

# MAPE - erro absoluto do percentual da média
## Medida de precisão. Mede a precisão como uma porcentagem e pode ser calculado como o 
## erro percentual absoluto médio para cada périodo de tempo menos os valores reais
# divididos pelos valores reais.

# RMSE - raiz do erro quadrático da média
## Usado para avaliar a medida das diferenças entre os valores (amostra ou população) previstos
## por mum modelo ou um estimador e os valores observados

# MAE - erro absoluto da média
## Medida de erros entre observações emparelhadas que expressam o mesmo fenômeno

err = df_test - arima_f$mean
mape <- mean(abs(err) / (arima_f$mean+ err)) * 100
rmse <- sqrt(mean(err^2, na.rm = TRUE)) 
mae <- mean(abs(err), na.rm = TRUE) 
cbind(mape, rmse, mae)

# PROPHET ######################################################################

df_train = subset(air_passengers, ds < "1959-01-01")
df_test = subset(air_passengers, ds >= "1959-01-01")

# Como foi analisado antes, a sazonalidade não é constante no tempo, mas aumenta 
# com a tendência. Os modelos aditivos não são os melhores para lidar com essas 
# séries temporais. Mas com o Prophet podemos passar da sazonalidade aditiva para 
# a sazonalidade multiplicativa por meio do parâmetro "seasonality_mode".

m <- prophet(df_train,seasonality.mode = 'multiplicative')

# Vamos definir o período da previsão e a frequência (m, s, a)
future <- make_future_dataframe(m, 24, freq = 'm', include_history = F)
forecast <- predict(m, future)
plot(m, forecast)

# Para efeito de comparação, vamos avaliar o modelo com o RMSE, MAE e MAPE.

pred = forecast$yhat
err = df_test$y - forecast$yhat
mape <- mean(abs(err) / (pred+ err)) * 100
rmse <- sqrt(mean(err^2, na.rm = TRUE)) 
mae <- mean(abs(err), na.rm = TRUE) 
cbind(mape, rmse, mae)

# Para facilitar, vamos comparar os dois

## Modelo Arima
# mape     rmse      mae
# 2.356519 14.12564 10.62677

## Modelo Prophet
# mape     rmse      mae
# 5.463905 31.08188 25.89196
