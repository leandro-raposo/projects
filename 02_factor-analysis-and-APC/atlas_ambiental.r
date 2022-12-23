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
#         ANALISE DE ATLAS E RANKING
#
########################################

# Proposta de elaboração de um ranking e plotagem espacial ----------------
load("atlasambiental.RData")

# Apresentando a base de dados:
atlasambiental %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Analisando as correlações entre variáveis da base de dados atlasambiental
chart.Correlation(atlasambiental[, 3:11], histogram = TRUE, pch = "+")

# Salvando a Matriz de Correlações -----------------------------------
rho_atlas <- cor(atlasambiental[,3:11])

# Construindo um mapa de calor a partir das correlações
rho_atlas %>% 
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

# Construindo um mapa de calor 3D a partir das correlações

# Primeiro passo: salvando o mapa de calor 2D
plot3d_rho_atlas <- rho_atlas %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value, color = value),
            color = "black") +
  labs(x = NULL,
       y = NULL,
       fill = "Correlações") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(axis.text.x = element_text(size = 26, angle = 90),
        axis.text.y = element_text(size = 26),
        title = element_text(size = 18,face = "bold"),
        panel.border= element_rect(size = 4, color = "black", fill = NA))

plot3d_rho_atlas

# Segundo passo: visualizando o plot 3D
plot_gg(ggobj = plot3d_rho_atlas, 
        multicore = TRUE, 
        width = 6, 
        height = 6, 
        scale = 100, 
        background = "white",
        shadowcolor = "dodgerblue4")

# O teste de efericidade de Bartlett --------------------------------------
cortest.bartlett(R = rho_atlas)

# O algoritmo prcomp(), do pacote psych, EXIGE que a a matriz de dados fornecida
# a ele já esteja padronizada pelo procedimento zscores:
atlasambiental_std <- atlasambiental %>% 
  select(-cod_ibge) %>% 
  column_to_rownames("distritos") %>% 
  scale() %>% 
  data.frame()

# Rodando a PCA
afpc_atlas <- prcomp(atlasambiental_std)
summary(afpc_atlas)

# Sumarizando pontos importantes:
data.frame(eigenvalue = afpc_atlas$sdev ^ 2,
           var_compartilhada = summary(afpc_atlas)$importance[2,],
           var_cumulativa = summary(afpc_atlas)$importance[3,]) -> relatorio

relatorio %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Visualizando os pesos que cada variável tem em cada componente principal 
# obtido pela PCA
ggplotly(
  data.frame(afpc_atlas$rotation) %>%
    mutate(var = names(atlasambiental[3:11])) %>%
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
  fviz_eig(X = afpc_atlas,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod4")
)

#Extraindo as Cargas Fatoriais
k <- sum((afpc_atlas$sdev ^ 2) > 1) 
cargas_fatoriais <- afpc_atlas$rotation[, 1:k] %*% diag(afpc_atlas$sdev[1:k])

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
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, color = "darkorchid") +
  geom_vline(xintercept = 0, color = "darkorchid") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  labs(x = "F1",
       y = "F2") +
  theme_bw()

# Scores Fatoriais
scores_fatoriais <- t(afpc_atlas$rotation)/afpc_atlas$sdev 
colnames(scores_fatoriais) <- colnames(atlasambiental_std)

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

#Assumindo-se apenas o F1 como indicador, calculam-se os scores fatoriais:
score_D1 <- scores_fatoriais[1,]
score_D1

#Estabelecendo o ranking dos indicadores assumido
F1 <- t(apply(atlasambiental_std, 1, function(x) x * score_D1))

F1 %>%
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

#Importando a coluna referente ao fator F1:
atlasambiental["fator1"] <- F1$fator1

#Criando um ranking pela soma ponderada dos fatores por sua variância
#compartilhada
atlasambiental %>%
  mutate(pontuacao = fator1 * 
           relatorio$var_compartilhada[1]) -> atlasambiental

#Visualizando o ranking final
atlasambiental %>%
  arrange(desc(pontuacao)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Carregando o mapa
load(file = "mapa_sp.RData")

#Visualizando o mapa
tm_shape(mapa_sp) + 
  tm_borders()

#Acrescentando informações ao mapa
mapa_sp@data$COD_DIST <- as.numeric(mapa_sp@data$COD_DIST)

distritos_dados <- merge(mapa_sp,
                         atlasambiental,
                         by.x = "COD_DIST",
                         by.y = "cod_ibge")

#Plotando os rankings
tmap_mode("view") #modo interativo - para acionar o modo offline, basta 
#argumentar "plot"

tm_shape(distritos_dados) +
  tm_fill("pontuacao", midpoint = 0, palette = "RdBu", 
          style = "quantile", n = 10, legend.show = T) +
  tm_borders(alpha = 0.8) +
  tm_text("distritos") 