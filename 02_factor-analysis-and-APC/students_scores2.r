pacotes <- c("plotly","tidyverse","knitr","kableExtra","PerformanceAnalytics",
             "factoextra","reshape2","psych","ggrepel")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Aplicação do algoritmo --------------------------------------------------

# Continuaremos a utilizar o primeiro exemplo, com dados da base notas fatorial

# Carregando a base de dados
load("notasfatorial.RData")

# O algoritmo prcomp(), do pacote psych, EXIGE que a a matriz de dados fornecida
# a ele já esteja padronizada pelo procedimento zscores:
notasfatorial_std <- notasfatorial %>% 
  column_to_rownames("estudante") %>% 
  scale() %>% 
  data.frame()

# Rodando a PCA:
afpc <- prcomp(notasfatorial_std)
summary(afpc)

# O objeto afpc possui os seguintes componentes:
afpc$sdev
afpc$rotation
afpc$center

# sdev: corresponde à raiz quadrada dos eigenvalues, ou seja, os desvios-padrão dos
# componentes principais.

# rotation: corresponde à matriz de tamanho jxj de eigenvectors, em que j 
# representa a quantidade de variáveis da base de dados.

# center: médias de cada variável utilizadas para após a padronização.

# scale: desvios-padrão de cada variável utilizadas para a padronização.

#Visualizando os pesos que cada variável tem em cada componente principal 
#obtido pela PCA
data.frame(afpc$rotation) %>%
  mutate(var = names(notasfatorial[2:5])) %>% 
  melt(id.vars = "var") %>%
  mutate(var = factor(var)) %>%
  ggplot(aes(x = var, y = value, fill = var)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~variable) +
  labs(x = NULL, y = NULL, fill = "Legenda:") +
  scale_fill_viridis_d() +
  theme_bw()

# Scree Plot - apenas ignorar os warnings
ggplotly(
  fviz_eig(X = afpc,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod3")
)

# Extraindo as Cargas Fatoriais
k <- sum((afpc$sdev ^ 2) > 1) #número de variáveis presentes na base de dados
cargas_fatoriais <- afpc$rotation[, 1:k] %*% diag(afpc$sdev[1:k])

# Visualizando as cargas fatoriais
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Visualizando as Comunalidades
data.frame(rowSums(cargas_fatoriais ^ 2)) %>%
  rename(comunalidades = 1) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Relatório das cargas fatoriais e das comunalidades
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Plotagem das Cargas Fatoriais
data.frame(cargas_fatoriais) %>%
  ggplot(aes(x = X1, y = X2)) +
  geom_point(color = "dodgerblue4") +
  geom_hline(yintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  labs(x = paste("F1", paste0("(",
                              round(summary(afpc)$importance[2,1] * 100,
                                    digits = 2),
                                   "%)")),
       y = paste("F2", paste0("(",
                              round(summary(afpc)$importance[2,2] * 100,
                                    digits = 2),
                              "%)"))) +
  theme_bw()

# Scores Fatoriais
scores_fatoriais <- t(afpc$rotation)/afpc$sdev 
colnames(scores_fatoriais) <- colnames(notasfatorial_std)

scores_fatoriais

scores_fatoriais %>%
  t() %>%
  data.frame() %>%
  rename(PC1 = 1,
         PC2 = 2) %>%
  select(PC1, PC2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)


# Proposta da construção de um ranking ------------------------------------

#Assumindo-se apenas o F1 e F2 como indicadores, calculam-se os scores 
#fatorias
score_D1 <- scores_fatoriais[1,]
score_D1

score_D2 <- scores_fatoriais[2,]
score_D2

#Estabelecendo o ranking dos indicadores assumido
F1 <- t(apply(notasfatorial_std, 1, function(x) x * score_D1))
F2 <- t(apply(notasfatorial_std, 1, function(x) x * score_D2))

F1
F2

#Na construção de rankings no R, devemos efetuar a multiplicação por -1, 
#visto que os scores fatoriais das observações mais fortes são, por padrão, 
#apresentados acompanhados do sinal de menos.
F1 <- data.frame(F1) %>%
  mutate(fator1 = rowSums(.) * 1)

F1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

F2 <- data.frame(F2) %>%
  mutate(fator2 = rowSums(.) * -1)

F2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Importando as colunas de fatores F1 e F2
notasfatorial["Fator1"] <- F1$fator1
notasfatorial["Fator2"] <- F2$fator2

#Criando um ranking pela soma ponderada dos fatores por sua variância
#compartilhada:

#Calculando a variância compartilhada
var_compartilhada <- (afpc$sdev ^ 2/sum(afpc$sdev ^ 2))
var_compartilhada

notasfatorial %>%
  mutate(pontuacao = Fator1 * var_compartilhada[1] +
           Fator2 * var_compartilhada[2]) -> notasfatorial

# Visualizando o ranking final
notasfatorial %>%
  arrange(desc(pontuacao)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
