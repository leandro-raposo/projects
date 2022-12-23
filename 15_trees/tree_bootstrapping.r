# Gerando os dados
set.seed(2360873)
L=1000
dados = rnorm(L)**2

# o quantil amostral é:
quantile(dados, .75)

# Definindo o número de amostras bootstrap
M=10000

# Inicializando um vetor que conterá as médias
estimativas <- vector(length=M)

# Realizar M amostras dos dados e calcular a média em cada uma delas
tempo_ini <- Sys.time()
for (i in 1:M){
  estimativas[i] <- quantile(sample(x = dados, size=L, replace=TRUE), .75)
}
tempo_fim <- Sys.time()

tempo_fim - tempo_ini

#Calcular os quantis
estimativas %>% 
  quantile(c(.025, .975)) %>% 
  round(3)

data.frame(estimativas) %>% head

p <- data.frame(estimativas) %>%
  ggplot( aes(x=estimativas, fill='q3')) +
  geom_histogram(bins=30, alpha=0.9) +
  ggtitle("Distribuição bootstrap do percentil 75%") +
  scale_fill_viridis_d(direction = -1, begin=0, end=.75) +
  theme_bw() + 
  theme(
    plot.title = element_text(size=12)
  )
p
