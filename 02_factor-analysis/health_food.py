########################################
#
#   CHAMANDO BIBLIOTECAS IMPORTANTES
#
########################################

import pandas as pd
import numpy as np
import sklearn as sk

########################################
#
#         ANALISE DE CEREAIS
#
########################################

# Proposta de elaboração de um ranking de produtos "mais saudáveis"
load("cereais.RData")

# Apresentando a base de dados:
cereais %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Analisando as correlações entre variáveis da base de dados cereais
chart.Correlation(cereais[, 4:14], histogram = TRUE, pch = "+")

# Salvando a Matriz de Correlações -----------------------------------
rho_cereais <- cor(cereais[, 4:14])

# Construindo um mapa de calor a partir das correlações
rho_cereais %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 3)),
            size = 4) +
  labs(x = NULL,
       y = NULL,
       fill = "Correlações") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))

# O teste de efericidade de Bartlett --------------------------------------
cortest.bartlett(R = rho_cereais)

# O algoritmo prcomp(), do pacote psych, EXIGE que a a matriz de dados fornecida
# a ele já esteja padronizada pelo procedimento zscores:
cereais_std <- cereais %>% 
  select(-industria, -tipo) %>% 
  column_to_rownames("marca") %>% 
  scale() %>% 
  data.frame()

# Rodando a PCA
afpc_cereais <- prcomp(cereais_std)
summary(afpc_cereais)

# Sumarizando pontos importantes:
data.frame(eigenvalue = afpc_cereais$sdev ^ 2,
           var_compartilhada = summary(afpc_cereais)$importance[2,],
           var_cumulativa = summary(afpc_cereais)$importance[3,]) -> relatorio_cereais

relatorio_cereais %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Visualizando os pesos que cada variável tem em cada componente principal 
# obtido pela PCA
ggplotly(
  data.frame(afpc_cereais$rotation) %>%
    mutate(var = names(cereais[4:14])) %>%
    melt(id.vars = "var") %>%
    mutate(var = factor(var)) %>%
    ggplot(aes(x = var, y = value, fill = var)) +
    geom_bar(stat = "identity", color = "black") +
    facet_wrap(~variable) +
    labs(x = NULL, y = NULL, fill = "Legenda:") +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
)

# Scree Plot - apenas ignorar os warnings
ggplotly(
  fviz_eig(X = afpc_cereais,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod4")
)

#Extraindo as Cargas Fatoriais
k <- sum((afpc_cereais$sdev ^ 2) > 1) 
cargas_fatoriais <- afpc_cereais$rotation[, 1:k] %*% diag(afpc_cereais$sdev[1:k])

# Visualizando as cargas fatoriais
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3) %>%
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
         F2 = X2,
         F3 = X3) %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Scores Fatoriais
scores_fatoriais <- t(afpc_cereais$rotation)/afpc_cereais$sdev 
colnames(scores_fatoriais) <- colnames(cereais_std)

scores_fatoriais

scores_fatoriais %>%
  t() %>%
  data.frame() %>%
  rename(PC1 = 1,
         PC2 = 2,
         PC3 = 3) %>%
  select(PC1, PC2, PC3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)


# Proposta da construção de um ranking ------------------------------------

data.frame(cargas_fatoriais) %>% 
  ggplot() +
  geom_point(aes(x = X1, y = X2), color = "orange") +
  geom_text_repel(aes(x = X1, y = X2, label = names(cereais[4:14]))) +
  geom_hline(yintercept = 0, color = "darkorchid") +
  geom_vline(xintercept = 0, color = "darkorchid") +
  labs(x = "F1",
       y = "F2") +
  theme_bw()


#Assumindo-se apenas o F1 e F2 como indicadores, calculam-se os scores 
#fatorias
score_D1 <- scores_fatoriais[1,]
score_D1

score_D2 <- scores_fatoriais[2,]
score_D2

#Estabelecendo o ranking dos indicadores assumido
F1 <- t(apply(cereais_std, 1, function(x) x * score_D1))
F2 <- t(apply(cereais_std, 1, function(x) x * score_D2))

F1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

F2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Na construção de rankings no R, devemos efetuar a multiplicação por -1, 
#visto que os scores fatoriais das observações mais fortes são, por padrão, 
#apresentados acompanhados do sinal de menos.
F1 <- data.frame(F1) %>%
  mutate(fator1 = rowSums(.) * -1)

F1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

F2 <- data.frame(F2) %>%
  mutate(fator2 = rowSums(.) * 1)

F2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)



#Importando as colunas de fatores F1 e F2
cereais["fator1"] <- F1$fator1
cereais["fator2"] <- F2$fator2

#Criando um ranking pela soma ponderada dos fatores por sua variância
#compartilhada
cereais %>%
  mutate(pontuacao = fator1 * relatorio_cereais$var_compartilhada[1] +
           fator2 * relatorio_cereais$var_compartilhada[1]) -> cereais

#Visualizando o ranking final
cereais %>%
  arrange(desc(pontuacao)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

