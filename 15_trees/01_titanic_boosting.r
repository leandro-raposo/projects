#      Classificar passageiros sobreviventes de acordo 
#      somente com variáveis do registro deles
titanic %>% head

# Analise descritiva
tmp <- titanic
tmp$survived <- as.integer(titanic$Survived=="Y")

##########################################
# Função para fazer a análise descritiva #
# Vamos avaliar a distribuição de sobreviventes por cada variável X
# Sumarizamos então y por categoria de X e montamos um gráfico de perfis

descritiva <- function(var){
  # Sumariza a taxa de sobreviventes por categoria da variável em análise
  tgc <- Rmisc::summarySE(tmp, measurevar="survived", groupvars=c(var))
  
  ggplot(tgc) + 
    # Plota o gráfico de barras com as frequências
    geom_bar(aes(x=tgc[,var], weight=N/891, fill=as.factor(tgc[,var]))) + 
    # Plota as barras de erro
    geom_errorbar(aes(x=tgc[,var], y=survived, ymin=survived-se, ymax=survived+se, colour='1'), width=.1) +
    # Plota as médias de cada grupo
    geom_point(aes(x=tgc[,var], y=survived, colour='1', group='1')) +
    # Plota as linhas que conectam as médias
    geom_line(aes(x=tgc[,var], y=survived, colour='1', group='1')) +
    # Escala de cores do gráfico de médias
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    # Escala de cores do gráfico de barras
    scale_fill_viridis_d(direction = -1, begin=.85, end=.95) +
    # Estética mais 'leve' do gráfico
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    # Remove a legenda
    theme(legend.position = "none") +
    # Rótulo dos eixos
    xlab(var) + ylab("Taxa de sobreviventes") + 
    # Marcas do eixo secundário
    scale_y_continuous(sec.axis = sec_axis(~.*891, name = "Frequencia"), labels = scales::percent)
}

descritiva("Sex")
descritiva("Pclass")
descritiva("Embarked")
descritiva("SibSp")
descritiva("Parch")

# Vamos categorizar as variáveis contínuas para analisar
tmp$cat_age <- quantcut(tmp$Age, 20)
descritiva("cat_age")

tmp$cat_fare <- quantcut(tmp$Fare, 10)
descritiva("cat_fare")

# Listagem das variáveis com algumas características
titanic %>% str

#############################################
# Vamos construir a árvore de classificação #
arvore <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                data=titanic,
                parms = list(split = 'gini'), # podemos trocar para  'information'
                method='class' # Essa opção indica que a resposta é qualitativa
)

#########################
# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot::rpart.plot(arvore,
                       box.palette = paleta) # Paleta de cores

##############################
# Avaliação básica da árvore #

# Predizendo com a árvore

# Probabilidade de sobreviver
prob = predict(arvore, titanic)

# Classificação dos sobreviventes
class = prob[,2]>.5
# Matriz de confusão
tab <- table(class, titanic$Survived)
tab

acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

# Árvore com variável resposta contínua

#####
# Gerando os dados
# x é uma sequencia de valores entre 0 e 1
x <- seq(0,1, length.out=1000)

# y segue uma relação quadrática
a <- 0
b <- 10
c <- -10

set.seed(2360873)
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
              control=rpart.control(maxdepth = 2, cp=0))

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

# Primeira iteração de boosting manual
tree1 <- rpart(r~x, 
               data=df,
               control=rpart.control(maxdepth = 2, cp=0))

df['p1'] = predict(tree1, df)  # Predito da árvore neste passo
df['P1'] = df$p + df$p1        # Predito do boosting (acumulado)
df['r1'] = df$r - df$p1        # resíduo do boosting


# Gráfico da primeira iteração de boosting manual
# O QUE O MODELO ESTÁ FAZENDO
boost1_r_vs_E <- ggplot(df, aes(x,r)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,p1, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Variável resposta neste passo") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "y") +
  scale_x_continuous(name= "x")


# O QUE ACONTECE COM O MODELO FINAL
boost1_O_vs_E <- ggplot(df, aes(x,y)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,P1, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Observado vs Esperado (final)") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "y") +
  scale_x_continuous(name= "x")

# Gráfico de resíduos
boost1_r <- ggplot(df, aes(x,r1)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Resíduo')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Gráfico de resíduos (final)") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "r") +
  scale_x_continuous(name= "x")

ggpubr::ggarrange(boost1_r_vs_E, boost1_O_vs_E, boost1_r,
                  # labels = c("A", "B"),
                  ncol = 3, nrow = 1)

#####
# Terceira iteração do boosting

tree2 <- rpart(r1~x, 
               data=df,
               control=rpart.control(maxdepth = 2, cp=0))

df['p2'] = predict(tree2, df) # predito da árvore tree2
df['P2'] = df$P1 + df$p2      # predito do boosting neste passo
df['r2'] = df$r1 - df$p2      # resíduo da árvore neste passo (resíduo do boosting)
# df['r2'] = df$y - df$P2     # O mesmo que a linha acima


# Gráfico da primeira iteração de boosting manual
# O QUE O MODELO ESTÁ FAZENDO
boost2_r_vs_E <- ggplot(df, aes(x,r1)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,p2, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Variável resposta neste passo") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "y(i)") +
  scale_x_continuous(name= "x")


# O QUE ACONTECE COM O MODELO FINAL
boost2_O_vs_E <- ggplot(df, aes(x,y)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,P2, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Observado vs Esperado (final)") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "y") +
  scale_x_continuous(name= "x")

# Gráfico de resíduos
boost2_r <- ggplot(df, aes(x,r2)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Resíduo')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Gráfico de resíduos (final)") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "r") +
  scale_x_continuous(name= "x")

boost2_O_vs_E

ggpubr::ggarrange(boost2_r_vs_E, boost2_O_vs_E, boost2_r,
                  # labels = c("A", "B"),
                  ncol = 3, nrow = 1)
