# start = quando a série começa
# end = quando a série termina
# frequency = número de unidades por período

# Exemplo 1

vendas <- c(25, 35, 32, 39, 37, 40)
class(vendas) # tipo númerico não é usada em séreis temporais, é preciso ser "ts"
vendas <- ts (vendas)
vendas

vendas2 <- ts(vendas, start = 2021, frequency = 12) # definindo a frequência
vendas2
autoplot(vendas2, main = "Vendas", xlab = "Mês", ylab = "Unidades Vendidas")

# Exemplo 2 
serie = ts(c(100:231), start = c(2000,1), end = c(2010,12), frequency = 12)
serie
class(serie)
autoplot(serie)

plot.ts(serie, main = "Exemplo1")
AirPassengers
plot.ts(AirPassengers, main="Exemplo2")

ts (AirPassengers, frequency = 4, start = c(1959, 2)) # frequency 4 => Quarterly Data
ts (1:10, frequency = 12, start = 1990) # freq 12 => Monthly data. 
ts (AirPassengers, start=c(2009), end=c(2014), frequency=1) # Yearly Data

# LENDO DADOS DE SÉRIES TEMPORAIS
# Carregando a base Kings - idade da morte de sucessivos reis da Inglaterra
# Fonte: Hipel and Mcleod, 1994

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3) # ignora
# as 3 primeiras linhas da base
# save(kings, file = "kings.RData")
kings # avaliando a base
kingstimeseries <- ts(kings) # salvando os dados no formato de séries temporais (ST)
kingstimeseries # visualizando a série criada

# Carregando a base Births - número de récem nascidos em NY entre 1946 a 1959

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat") # carregando a base
#save(births, file = "births.RData")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1)) # salvando o período de início, mês 01 de 1946
birthstimeseries # visualizando a base

# Carregando a base Souvenir - venda de souvernirs entre Janeiro de 1987 a Dezembro de 1993
# Fonte: Wheelwright and Hyndman, 1998

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat") # carregando a base
#save(souvenir, file = "souvenir.RData")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))  # salvando o período de início, mês 01 de 1987
souvenirtimeseries # visualizando a base

###############################################################################
# Para avaliar quando iniciar a série temporal
start(USAccDeaths)
end(USAccDeaths)

#Avaliar graficamente várias séries
plot.ts(USAccDeaths)

###############################################################################
### PLOTANDO OS DADOS
###############################################################################

# LEMBRETE CONCEITUAL
# método aditivo - usar quando sazonalidade for constante (default)
# método multiplicativo - usar quando sazonalidade for crescente

plot.ts(kingstimeseries) # possibilidade de modelo aditivo
plot.ts(birthstimeseries) # possibilidade de modelo aditivo
plot.ts(souvenirtimeseries) # não parece ter a possibilidade de modelo aditivo
# A base souvernir tem as flutuações sazonais e aleatórias maiores ao longo do tempo

# Uma alterantiva para transforamr o dados seria calcular o LOG natural
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries) # modelo com LOG gera a possibilidade de modelo aditivo

#################################################################################
#                               CONCEITOS                                      #
#################################################################################

# Autocorrelação
# É a correlação da série com ela mesma. Em séries temporais, será a correlação
# entre a série e uma LAG (valores anteriores)

# Discussão sobre LAG

AirPassengers
lag(AirPassengers, -1)
lag(AirPassengers, -2)

# gerando lags para a série
tsData <- ts(AirPassengers)
tsData
laggedTS <- lag(tsData, 3)
myDf <- as.data.frame(tsData)
myDf <- slide(myDf, "x", NewVar = "xLag1", slideBy = -1)  # create lag1 variable
myDf <- slide(myDf, "x", NewVar = "xLead1", slideBy = 1)  # create lead1 variable
head(myDf)

# Séries com autocorrelação demonstra valores associados, ou seja, anteriores vão prever os próximos

fpp2::a10 # série disponível
acf(a10) # autocorrelação
pacf(a10) # autocorrelação parcial
ggtsdisplay(a10) # avaliação em visualização única

# Estacionariedade
# Série cujas características não mudam ao longo do tempo

# Ruído - série totalmente aleatória
# Característica: média e variância constante, sem tendência e sazonalidade
rb <- ts(rnorm(500))
autoplot(rb)

# Normalidade
# Distribuição normal tem forma de sino, com os dados distribuídos simetricamente
# ao redor da média

hist(rnorm(5000)) # expectativa e simétrica ao redor da média
hist(a10)
qqnorm(a10)
qqline(a10, col = "red")

################################################################################
#                               VISUALIZACAO GRÁFICA                            #
#################################################################################

# Formatando o gráfico
plot(USAccDeaths, xlab = 'Anos', ylab = 'Número de Mortes')
plot (USAccDeaths, type = 'o')

#Avaliando, e combinando, os gráficos em diferentes períodos
plot.ts(cbind(USAccDeaths,AirPassengers), main = 'Mortes x Transporte Aéreo', xlab = 'Anos')
plot.ts(cbind(USAccDeaths,AirPassengers), main = 'Mortes x Transporte Aéreo', xlab = 'Anos', nc=2) # lado a lado

# Agregando períodos
aggregate(USAccDeaths, nfrequency = 4, FUN = sum) # somas trimestrais
aggregate(USAccDeaths, nfreq = 1, FUN=mean) # médias anuais
plot(aggregate(USAccDeaths))
plot(aggregate(USAccDeaths, nfrequency = 4, FUN = sum))
monthplot(USAccDeaths, col.base =2, labels = month.abb)

# Visualizando uma janela temporal
janela = window(USAccDeaths, start=c(1973, 5), end = c(1975,7))
janela

diff(USAccDeaths) # diferença entre os meses
log(USAccDeaths) # logaritmo da série

# Análise da Autocorrelação (FAC) e Autocorrelação Parcial
# FCAp com defasagem 25:

a = acf(USAccDeaths, lag.max = 25)
a

p = pacf(USAccDeaths, lag.max = 25)
p

da = acf(diff(USAccDeaths), lag.max = 25) # diferença 
da

dp = pacf(diff(USAccDeaths), lag.max = 25)
dp

# Avaliar a sazonalidade
plot(stl(log(USAccDeaths), "periodic"))

###############################################################################
### DECOMPOSIÇÃO DE SÉRIES TEMPORAIS
###############################################################################

# Objetivo: separar os componentes, em geral, tendências e um componente irregular
# se for uma sére temporal sazonal, um componente sazonal

# Conceitos
# Soma da Tendência + Sazonalidade + Aleatório

# Tendência - padrão de crescimento ou decrescimento da série/média móvel centrada, coimputa variações cíclicas
# Sazonalidade - padrão de repetição em intervalos regulares
# Aleatório - não é tendência, nem sazonalidade, outros aspectos

# Modelo Aditivo
# Série = Tendência + Sazonalidade + Aleatório

# Modelo Multiplicativo
# Série = Tendência x Sazonalidade x Aleatório

# DECOMPOSIÇÃO DE SÉRIES NÃO SAZONAIS

# Componente de tendência e componente irregular. Envolve tentar separar a série temporal e estimar o componente de 
# tendência e o componente irregular

# Para estimar é comum usar o método de suaviazação, como o cáculo da média móvel simples da série temporal

kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3) # estimar com uma média móvel simples de ordem 3
plot.ts(kingstimeseriesSMA3)

# Ainda existem muitas flutuações, assim, vamos estimar com uma média superior

kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)

# DECOMPOSIÇÃO DE SÉRIES SAZONAIS

# Componentes de tendência, sazonalidade e componente irregular
# Objetivo será separar os componentes

# variações sazonais e aleatórias parecem ser constante ao longo do texto
plot.ts(birthstimeseries) # dados originais, picos no verão e no inverno
birthstimeseriescomponents <- decompose(birthstimeseries) # estimando os componentes
birthstimeseriescomponents$seasonal # obter os valores estimados do componentes sazonal
plot(birthstimeseriescomponents)

# AJUSTAMENTO SAZONAL
# É preciso que a série possa ser descrita como um modelo adicional
# É possível ajustar estimando o componente sazonal e substraindo o componente sazonal da série orignal

plot.ts(birthstimeseries) 
birthstimeseriescomponents <- decompose(birthstimeseries) # estimar o componente sazonal
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)

# A variação sazonal foi removida. Agora a série contém componentes de tendência e um componente irregular

#################################################################################
#                               TRANSFORMAÇÕES                                  #
#################################################################################

# Tornar séries normais e estacionárias

# Diferenciação - remove a tendênca da série, para ser estacionária
# Registro das mudanças das séries, diferença do segundo valor pelo primeiro valor

AirPassengers
diff(AirPassengers, 1) # diferença entre os meses
diff(AirPassengers, 20) # diferença entre os meses

autoplot(a10)
a10.diff <- diff(a10, 1)
autoplot(a10.diff) # Removeu a tendência
a10.diff2 <- diff(a10, 2)
autoplot(a10.diff2)
ndiffs(a10) # avaliar a quantidade de diferenciações necessárias

# Não foi sufuciente, então, 
# Transformação BoxCox (sazonalidade e distribuição normal)

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

# Teste de Dickey-Fuller Aumentado # teste de estacionariedade
# Se p valor < 0.05 indica que a série é estacionária
adf.test(serie.final) 
kpss.test(serie.final)

