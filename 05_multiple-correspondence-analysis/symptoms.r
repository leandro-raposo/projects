pacotes <- c("plotly","tidyverse","ggrepel","reshape2","FactoMineR",
             "knitr","kableExtra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Estabeleça uma ACM e estude as associações existentes entre as variáveis
# disease, fever, itch e arthralgia.

# Carregando a base de dados
load("symptoms.RData")

# Apresentando os dados
symptoms %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Verificando as associações existentes entre as categorias das variáveis da
# base de dados, par a par

# A) Disease x Fever
tab_disease_fever <- table(symptoms$disease,
                           symptoms$fever)

qui2_disease_fever <- chisq.test(tab_disease_fever)
qui2_disease_fever

# B) Disease x Itch
tab_disease_itch <- table(symptoms$disease,
                          symptoms$itch)

tab_disease_itch

qui2_disease_itch <- chisq.test(tab_disease_itch)
qui2_disease_itch

# C) Disease x Arthralgia
tab_disease_arthralgia <- table(symptoms$disease,
                                symptoms$arthralgia)

tab_disease_arthralgia

qui2_disease_arthralgia <- chisq.test(tab_disease_arthralgia)
qui2_disease_arthralgia

# D) Fever x Itch
tab_fever_itch <- table(symptoms$fever,
                        symptoms$itch)

tab_fever_itch

qui2_fever_itch <- chisq.test(tab_fever_itch)
qui2_fever_itch

# E) Fever x Arthralgia
tab_fever_arthralgia <- table(symptoms$fever,
                              symptoms$arthralgia)

tab_fever_arthralgia

qui2_fever_arthralgia <- chisq.test(tab_fever_arthralgia)
qui2_fever_arthralgia

# F) Itch x Arthralgia
tab_itch_arthralgia <- table(symptoms$itch,
                             symptoms$arthralgia)

tab_itch_arthralgia

qui2_itch_arthralgia <- chisq.test(tab_itch_arthralgia)
qui2_itch_arthralgia

# Interpondo a ACM
ACM_symptoms <- MCA(symptoms[,2:5], method = "Indicador")

# Para estudarmos o percentual da inérica principal explicada por 
# dimensão, podemos:
categorias <- apply(symptoms[,2:5], 
                    MARGIN =  2, 
                    FUN = function(x) nlevels(as.factor(x)))

categorias

# Capturando as coordenadas das categorias
ACM_symptoms_mp <- data.frame(ACM_symptoms$var$coord, Variável = rep(names(categorias), categorias))

#Plotando o Mapa Perceptual das categorias:
ACM_symptoms_mp %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Dim.1, 
             y = Dim.2, 
             label = Categoria, 
             color = Variável, 
             fill = Variável,
             shape = Variável)) +
  geom_point() +
  geom_label_repel(color = "white") +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey") +
  labs(x = paste("Dimensão 1:", paste0(round(ACM_symptoms$eig[1,2], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(ACM_symptoms$eig[2,2], 2), "%"))) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

#Plotando o Mapa Perceptual das categorias e das observações:
ACM_symptoms_observacoes_df <- data.frame(ACM_symptoms$ind$coord)

ACM_symptoms_observacoes_df %>% 
  ggplot(aes(x = Dim.1, y = Dim.2, label = symptoms$id)) +
  geom_point(shape = 17, color = "#E76F5AFF", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_text_repel(max.overlaps = 600, size = 3) +
  geom_density2d(color = "gray80") +
  geom_label_repel(data = ACM_symptoms_mp, 
                   aes(x = Dim.1, y = Dim.2, 
                       label = rownames(ACM_symptoms_mp), 
                       fill = Variável), 
                   color = "white") +
  labs(x = paste("Dimensão 1:", paste0(round(ACM_symptoms$eig[,2][1], digits = 2), "%")),
       y = paste("Dimensão 2:", paste0(round(ACM_symptoms$eig[,2][2], digits = 2), "%"))) +
  scale_fill_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

# Mapa Perceptual 3D
ACM_symptoms_3D <- plot_ly()

# Adicionando as coordenadas
ACM_symptoms_3D <- add_trace(p = ACM_symptoms_3D,
                             x = ACM_symptoms_mp[,1],
                             y = ACM_symptoms_mp[,2],
                             z = ACM_symptoms_mp[,3],
                             mode = "text",
                             text = rownames(ACM_symptoms_mp),
                             textfont = list(color = "#440154FF"),
                             showlegend = FALSE)

# Adicionando as labels das dimensões
ACM_symptoms_3D <- layout(p = ACM_symptoms_3D,
                          scene = list(xaxis = list(title = colnames(ACM_symptoms_mp)[1]),
                                       yaxis = list(title = colnames(ACM_symptoms_mp)[2]),
                                       zaxis = list(title = colnames(ACM_symptoms_mp)[3]),
                                       aspectmode = "data"))

ACM_symptoms_3D
