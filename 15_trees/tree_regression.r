# Árvore com variável resposta contínua

# Gerando os dados
# x é uma sequencia de valores entre 0 e 1
x <- seq(0,1, length.out=1000)

# y segue uma relação quadrática
a <- 0
b <- 10
c <- -10

y <- a + b*x + c*x**2 + rnorm(length(x), mean=0, sd=.1)

df <- data.frame(x, y)

p0 <- ggplot(df, aes(x,y)) + 
  geom_point(aes(colour='Observado')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.85, name = "Valor") +
  theme(legend.position="bottom",
        legend.spacing.x = unit(0, 'cm'))
p0

########################
# Construindo a árvore #
tree <- rpart(y~x, 
              data=df,
              control=rpart.control(maxdepth = 3, cp=0))

# Plotando a árvore
paleta = scales::viridis_pal(begin=.75, end=1)(20)
rpart.plot::rpart.plot(tree,
                       box.palette = paleta) # Paleta de cores

# Valores preditos
df['p'] = predict(tree, df)
df['r'] = df$y - df$p

# Valores esperados e observados
boost0_O_vs_E <- ggplot(df, aes(x,y)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,p, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados") +
  scale_y_continuous(name= "y") +
  scale_x_continuous(name= "x")

boost0_O_vs_E

# Gráfico de resíduos
boost0_res <- ggplot(df, aes(x,r)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Resíduo')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  labs(title="Gráfico de resíduos") +
  scale_y_continuous(name= "r") +
  scale_x_continuous(name= "x")
boost0_res

ggpubr::ggarrange(boost0_O_vs_E, boost0_res, 
          # labels = c("A", "B"),
          ncol = 2, nrow = 1)
