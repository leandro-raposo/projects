pacotes <- c("plotly","tidyverse","ggrepel","sjPlot","reshape2","knitr",
             "kableExtra","FactoMineR")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Análise de Correspondência Simples (ANACOR) - Abordagem Teórica ---------

#Carregando a base de dados
load(file = "perfil_investidor.RData")

#Observado os dados carregados
perfil_investidor %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Tabelas de frequências
summary(perfil_investidor)

#Criando uma tabela de contingências
tab <- table(perfil_investidor$perfil, 
             perfil_investidor$aplicacao)

tab

#Exemplo de uma tabela de contingências mais elegante
sjt.xtab(var.row = perfil_investidor$perfil,
         var.col = perfil_investidor$aplicacao)

#Exemplo de uma tabela de contingências mais elegante
sjt.xtab(var.row = perfil_investidor$perfil,
         var.col = perfil_investidor$aplicacao,
         show.exp = TRUE)

#Teste Qui-Quadrado
qui2 <- chisq.test(tab)
qui2

qui2$statistic # Valor calculado Qui2
qui2$parameter # Graus de liberdade
qui2$p.value # p-value
qui2$method
qui2$data.name
qui2$observed # Valores observados
qui2$expected # Valores esperados
qui2$observed - chi2$expected # Valores dos resíduos
qui2$residuals #Resíduos PADRONIZADOS
qui2$stdres #Resíduos PADRONIZADOS AJUSTADOS

#Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2$stdres) %>%
  rename(perfil = 1,
         aplicacao = 2) %>% 
  ggplot(aes(x = fct_rev(perfil), 
             y = aplicacao, 
             fill = Freq, 
             label = round(Freq,3))) +
  geom_tile() +
  geom_label(size = 3, fill = "white") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text())

#Decomposição da inércia principal total
It <- qui2$statistic/nrow(perfil_investidor)
It

#Construindo a matriz P
P <- 1/nrow(perfil_investidor) * tab
P

#Column profile
data.frame(tab) %>% 
  group_by(Var2) %>% 
  summarise(Var1 = Var1,
            Massas = Freq / sum(Freq)) %>% 
  dcast(Var1 ~ Var2) %>% 
  column_to_rownames("Var1") %>% 
  round(., digits = 3)

column_profile <- apply(tab, MARGIN = 1, FUN = sum) / nrow(perfil_investidor)
column_profile

#Row profiles
data.frame(tab) %>% 
  group_by(Var1) %>% 
  summarise(Var2 = Var2,
            Massas = Freq / sum(Freq)) %>% 
  dcast(Var1 ~ Var2) %>% 
  column_to_rownames("Var1") %>% 
  round(., digits = 3)

row_profile <- apply(tab, MARGIN = 2, FUN = sum) / nrow(perfil_investidor)
row_profile

#Matriz Dl
Dl <- diag(column_profile)
Dl

#Matriz Dc
Dc <- diag(row_profile)
Dc

#Matriz lc'
lc <- column_profile %o% row_profile
lc

#Matriz A
A <- diag(diag(Dl) ^ (-1/2)) %*% (P - lc) %*% diag(diag(Dc) ^ (-1/2))
A

#Curiosidade:
A_matriz <- qui2$residuals / sqrt(nrow(perfil_investidor))
A_matriz

#Matriz W
W_matriz <- t(A_matriz) %*% A_matriz
W_matriz

#Extraindo os eigenvalues da matriz W
eigenvalues <- eigen(W_matriz)
eigenvalues

sum(eigenvalues$values) #It
It

#Dimensionalidade dos dados
dimensoes <- min(nrow(A_matriz) - 1, ncol(A_matriz) - 1)
dimensoes

#Percentual da Inércia Total explicada
It_explicada <- eigenvalues$values[1:2] / It
It_explicada

#Cálculo das coordenadas do mapa perceptual

#Decomposição do valor singular da matriz A
decomp <- svd(x = A_matriz,
              nu = dimensoes,
              nv = dimensoes)

decomp

#Variável em linha - coordenada no eixo das abcissas
Xl_perfil <- diag((decomp$d[1]) * diag(diag(Dl)^(-1/2)) * decomp$u[,1])
Xl_perfil

#Variável em linha - coordenada no eixo das ordenadas
Yl_perfil <- diag((decomp$d[2]) * diag(diag(Dl)^(-1/2)) * decomp$u[,2])
Yl_perfil

#Variável em coluna - coordenada no eixo das abcissas
Xc_aplicacao <- diag((decomp$d[1]) * diag(diag(Dc)^(-1/2)) * decomp$v[,1])
Xc_aplicacao

#Variável em coluna - coordenada no eixo das ordenadas
Yc_aplicacao <- diag((decomp$d[2]) * diag(diag(Dc)^(-1/2)) * decomp$v[,2])
Yc_aplicacao


# Elaborando o mapa perceptual bidimensional ------------------------------

# Passo 1: Guardando as coordenadas, de cada categoria e de cada variável,  num 
# único objeto
coordenadas <- data.frame(Categorias = cbind(c(levels(perfil_investidor$perfil),
                                               levels(perfil_investidor$aplicacao))),
                          Dim1 = cbind(c(Xl_perfil, Xc_aplicacao)),
                          Dim2 = cbind(c(Yl_perfil, Yc_aplicacao)))

coordenadas

# Passo 2: Como iremos estratificar as categorias em função de cores distintas
# em função de qual variável elas pertencem, vamos criar uma coluna que faça
# essa identificação:
variaveis <- apply(perfil_investidor[,2:3],
                   MARGIN =  2,
                   FUN = function(x) nlevels(as.factor(x)))

variaveis

# Passo 3: Vamos juntar, o objeto variaveis ao objeto coordenadas:
coordenadas_final <- data.frame(coordenadas,
                                Variaveis = rep(names(variaveis), variaveis))

coordenadas_final

# Passo 4: Plotando o mapa perceptual bidimensional:
coordenadas_final %>% 
  rownames_to_column() %>% 
  rename(Category = 1) %>% 
  ggplot(aes(x = Dim1, y = Dim2, label = Categorias, color = Variaveis)) +
  geom_point() +
  geom_label_repel() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = paste("Dimension 1:", paste0(round(It_explicada[1] * 100, digits = 2), "%")),
       y = paste("Dimension 2:", paste0(round(It_explicada[2] * 100, digits = 2), "%"))) +
  scale_color_manual("Variable:",
                     values = c("darkorchid", "orange")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

#Repetindo o mapa de calor dos resíduos padronizados ajustados
data.frame(chi2$stdres) %>%
  rename(perfil = 1,
         aplicacao = 2) %>% 
  ggplot(aes(x = fct_rev(perfil), 
             y = aplicacao, 
             fill = Freq, 
             label = round(Freq,3))) +
  geom_tile() +
  geom_label(size = 3, fill = "white") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text())
