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
#         ANALISE DE LOJAS 
#
########################################

# Proposta de estudo de validação de constructos --------------------------
load("percepcao_lojas.RData")

# Questionário proposto
questionario <- image_read("questionário.png")

plot(questionario)

# Apresentando a base de dados:
percepcao_lojas %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Analisando as correlações entre variáveis da base de dados percepcao_lojas
chart.Correlation(percepcao_lojas, histogram = TRUE)

# Salvando a Matriz de Correlações -----------------------------------
rho_lojas <- cor(percepcao_lojas)

# Construindo um mapa de calor a partir das correlações
rho_lojas %>% 
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
plot3d_rho_lojas <- rho_lojas %>% 
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

plot3d_rho_lojas

# Segundo passo: visualizando o plot 3D
plot_gg(ggobj = plot3d_rho_lojas, 
        multicore = TRUE, 
        width = 3, 
        height = 3, 
        scale = 500, 
        background = "white",
        shadowcolor = "dodgerblue4")

# O teste de efericidade de Bartlett --------------------------------------
cortest.bartlett(R = rho_lojas)

# O algoritmo prcomp(), do pacote psych, EXIGE que a a matriz de dados fornecida
# a ele já esteja padronizada pelo procedimento zscores:
percepcao_lojas_std <- percepcao_lojas %>% 
  scale() %>% 
  data.frame()

# Rodando a PCA
afpc_lojas <- prcomp(percepcao_lojas_std)
summary(afpc_lojas)

# Sumarizando pontos importantes:
data.frame(eigenvalue = afpc_lojas$sdev ^ 2,
           var_compartilhada = summary(afpc_lojas)$importance[2,],
           var_cumulativa = summary(afpc_lojas)$importance[3,]) -> relatorio_lojas

relatorio_lojas %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Visualizando os pesos que cada variável tem em cada componente principal 
# obtido pela PCA
ggplotly(
  data.frame(afpc_lojas$rotation) %>%
    mutate(var = names(percepcao_lojas)) %>%
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

#Extraindo as Cargas Fatoriais
k <- sum((afpc_lojas$sdev ^ 2) > 1) 
cargas_fatoriais <- afpc_lojas$rotation[, 1:k] %*% diag(afpc_lojas$sdev[1:k])

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

# Note que, tanto as cargas fatoriais quanto a comunalidade da variável
# atendimento são relativamente baixas. Tal situação pode evidenciar a
# necessidade da extração de um terceiro fator, descaracterizando o 
# critério da raiz latente:

#Extraindo as Cargas Fatoriais para os 3 Fatores
k <- length(afpc_lojas$sdev[1:3])
cargas_fatoriais <- afpc_lojas$rotation[, 1:k] %*% diag(afpc_lojas$sdev[1:k])

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

# Note que a decisão de extração de três fatores, em detrimento da extração 
# com base no critério da raiz latente, aumenta as comunalidades das 
# variáveis, com destaque para a variável atendimento, agora correlacionada 
# mais fortemente com o terceiro fator.

# Comportamento das cargas fatoriais de forma 2D (usando F1 e F2)
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3) -> cargas_fatoriais

# ATENÇÃO! OS SINAIS NEGATIVOS PARA A PLOTAGEM DE F2, SERVEM ÚNICA E 
# EXCLUSIVAMENTE PARA INVERTER OS EIXOS DO GRÁFICO E PERMITIR A SUBSEQUENTE
# COMPARAÇÃO COM O PLOT 3D.
cargas_fatoriais %>% 
  ggplot(aes(x = -F2, y = F1)) +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, color = "darkorchid") +
  geom_vline(xintercept = 0, color = "darkorchid") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  theme_bw() 

# Comportamento das cargas fatoriais de forma 3D (usando F1, F2 e F3)
afpc_lojas_3D <- plot_ly()

afpc_lojas_3D <- add_trace(p = afpc_lojas_3D, 
                         x = cargas_fatoriais$F2, 
                         y = cargas_fatoriais$F3, 
                         z = cargas_fatoriais$F1,
                         mode = 'text', 
                         text = rownames(cargas_fatoriais),
                         textfont = list(color = "orange"), 
                         showlegend = FALSE)

afpc_lojas_3D <- layout(p = afpc_lojas_3D,
                        scene = list(xaxis = list(title = colnames(cargas_fatoriais)[2]),
                                     yaxis = list(title = colnames(cargas_fatoriais)[3]),
                                     zaxis = list(title = colnames(cargas_fatoriais)[1]),
                                     aspectmode = "data"))

afpc_lojas_3D