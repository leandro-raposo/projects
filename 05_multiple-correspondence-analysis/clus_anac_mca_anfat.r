pacotes <- c("plotly","tidyverse","ggrepel","sjPlot","reshape2","FactoMineR",
             "cabootcrs","knitr","kableExtra","gifski","gganimate","factoextra",
             "plot3D","viridis")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Para fins didáticos, vamos utilizar duas bases de dados já visitadas. 
# Primeiramente, estabeleceremos uma ACM e, depois, uma PCA. Por fim, faremos
# uma clusterização.

load("notasfatorial.RData")

# Apresentando os dados da base 'notasfatorial'
notasfatorial %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

load(file = "perfil_investidor_aplicacao.RData")

# Apresentando os dados da base 'perfil_investidor_aplicacao'
perfil_investidor_aplicacao %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Juntado as duas bases
base_dados <- notasfatorial %>% 
  left_join(perfil_investidor_aplicacao, by = "estudante")

# Apresentando a base de dados a ser utilizada
base_dados %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Vamos começar pela ACM:


# Estabelecendo a ACM -----------------------------------------------------

# 1. Verificando o teste Qui-Quadrado entre o cruzamentos de variáveis a serem
# considerados

# A) Perfil x Aplicação
tab_perfil_aplicacao <- table(perfil_investidor_aplicacao$perfil,
                              perfil_investidor_aplicacao$aplicacao)

qui2_perfil_aplicacao <- chisq.test(tab_perfil_aplicacao)
qui2_perfil_aplicacao

# B) Perfil x Estado Civil
tab_perfil_estadocivil <- table(perfil_investidor_aplicacao$perfil,
                                perfil_investidor_aplicacao$estado_civil)

tab_perfil_estadocivil

qui2_perfil_estadocivil <- chisq.test(tab_perfil_estadocivil)
qui2_perfil_estadocivil

# C) Aplicação x Estado Civil
tab_aplicacao_estadocivil <- table(perfil_investidor_aplicacao$aplicacao,
                                   perfil_investidor_aplicacao$estado_civil)

tab_aplicacao_estadocivil

# 2. A ACM
ACM <- MCA(base_dados[, 6:8], method = "Indicador")

# 3. Capiturando as coordenadas das observações em nossa base de dados
base_dados[c("D1","D2","D3","D4","D5")] <- data.frame(ACM$ind$coord)

# 4. Para facilitar o transcorrer do exercício, removeremos as variáveis
# categóricas originais, visto que suas coordenadas já as representam.
base_dados <- base_dados[,-c(6:8)]


# Estabelecendo uma PCA ---------------------------------------------------

# 1. Para a utilização do algoritmo prcomp(), o R exige a padronização dos
# dados. Não utilizaremos as coordenadas da ACM, mas já as estamos padronizando
# porque a subsequente clusterização a exigirá.
base_dados_std <- base_dados %>% 
  column_to_rownames("estudante") %>% 
  scale() %>% 
  data.frame()

# 2. A PCA
AFCP <- prcomp(base_dados_std[,1:4])

AFCP

# 3. Vamos considerar os fatores cujos eigenvalues se mostraram maiores do que
# 1. Assim, para salvá-los em nossa base de dados, podemos:

scores_fatoriais <- t(AFCP$rotation)/AFCP$sdev 

#Assumindo-se apenas o F1 e F2 como indicadores, calculam-se os scores 
#fatorias
score_D1 <- scores_fatoriais[1,]
score_D1

score_D2 <- scores_fatoriais[2,]
score_D2

F1 <- t(apply(base_dados_std[,1:4], 1, function(x) x * score_D1))
F2 <- t(apply(base_dados_std[,1:4], 1, function(x) x * score_D2))

F1
F2

F1 <- data.frame(F1) %>%
  mutate(fator1 = rowSums(.) * 1)

F1

F2 <- data.frame(F1) %>%
  mutate(fator2 = rowSums(.) * 1)

F2

base_dados_std[c("F1","F2")] <- cbind(F1$fator1, F2$fator2)

# 4. Por razões didáticas, excluiremos as variáveis métricas originais da base
# de dados:
base_dados_std <- base_dados_std[,-c(1:4)]


# Estabelecendo a Clusterização -------------------------------------------

# 1. Clustering
cluster_estudantes <- kmeans(base_dados_std, centers = 2)

# 2. Observando os resultados
fviz_cluster(cluster_estudantes, data = base_dados_std)

# 3. Uma outra maneira de enxergar os dados

# Vamos capturar as coordenadas do eixo Z:
plot <- fviz_cluster(cluster_estudantes, data = base_dados_std)

View(plot)

# Note que só as coordenadas dos eixos X e Y. Vamos "adaptar" o algoritmo
# fviz_cluster() para que ele nos retorne os valores do eixo Z:
fviz_cluster

fviz_cluster_adaptado(object = cluster_estudantes,
                      data = base_dados_std)

# Aparentemente, nada mudou, certo?
coordenadas <- fviz_cluster_adaptado(object = cluster_estudantes,
                                     data = base_dados_std)

View(coordenadas)

scatter3D(x = coordenadas$data$x, 
          y = coordenadas$data$y, 
          z = coordenadas$data$Dim.3, 
          zlim = c(-3,3),
          ylim = c(-3,3),
          xlim = c(-3,3),
          pch = 19,
          bty = "b2",
          colvar = as.numeric(coordenadas[["data"]][["cluster"]]),
          col = viridis(200))