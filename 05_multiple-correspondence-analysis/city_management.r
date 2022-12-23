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

# A base de dados a seguir diz respeito a uma pesquisa de opinião ocorrida
# a respeito do decorrer de 03 anos da gestão de dado prefeito.

# A vários eleitores foi proposta a afirmação 'Estou satisfeito com a gestão
# do prefeito!"

# Proponha uma ANACOR e discuta os resultados encontrados.

# Carregando a base de dados
load("gestao_municipal.RData")

# Apresentando os dados
gestao_municipal %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Estabelecendo uma tabela de contingências
tab_gestao <- table(gestao_municipal$avaliação,
                    gestao_municipal$ano)

tab_gestao

# Teste Qui-Quadrado
qui2_gestao <- chisq.test(tab_gestao)
qui2_gestao

# Apresentando o Mapa de Calor dos Resíduos Padronizados Ajustados
data.frame(qui2_gestao$stdres) %>%
  rename(avaliação = 1,
         ano = 2) %>% 
  ggplot(aes(x = ano, y = avaliação, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "#440154FF", 
                       mid = "white", 
                       high = "#FDE725FF",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none")

# Interpondo a ANACOR
anacor_gestao <- CA(tab_gestao)

# Mapa Perceptual Elegante
# Capturando todas as coordenadas num só objeto
ca_coordenadas <- rbind(anacor_gestao$row$coord, anacor_gestao$col$coord)
ca_coordenadas

# Capturando a quantidade de categorias por variável
id_var <- apply(gestao_municipal[,1:2],
                MARGIN =  2,
                FUN = function(x) nlevels(as.factor(x)))
id_var

# Juntando as coordenadas e as categorias capturadas anteriormente
ca_coordenadas_final <- data.frame(ca_coordenadas, 
                                   Variable = rep(names(id_var), id_var))

ca_coordenadas_final

# Mapa perceptual bidimensional

# Mapa perceptual elegante:
ca_coordenadas_final %>% 
  rownames_to_column() %>% 
  rename(Category = 1) %>% 
  ggplot(aes(x = Dim.1, 
             y = Dim.2, 
             label = Category, 
             fill = Variable,
             color = Variable,
             shape = Variable)) +
  geom_point(size = 2) +
  geom_label_repel(max.overlaps = 100,
                  size = 3,
                  color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = paste("Dimension 1:", paste0(round(anacor_gestao$eig[1,2], digits = 2), "%")),
       y = paste("Dimension 2:", paste0(round(anacor_gestao$eig[2,2], digits = 2), "%"))) +
  scale_fill_viridis_d(option = "cividis") +
  scale_color_viridis_d(option = "cividis") +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")
