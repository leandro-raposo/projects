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

load(file = "cpc_geral.RData")

#Observado os dados carregados
cpc_geral %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Tabelas de frequências
summary(cpc_geral)

#Criando uma tabela de contingências
tab <- table(cpc_geral$categoria, 
             cpc_geral$cpc)

tab

#Exemplo de uma tabela de contingências mais elegante
sjt.xtab(var.row = cpc_geral$categoria,
         var.col = cpc_geral$cpc,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)


#Teste Qui-Quadrado
qui2 <- chisq.test(tab)
qui2

#Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2$stdres) %>%
  rename(categoria = 1,
         cpc = 2) %>% 
  ggplot(aes(x = fct_rev(categoria), y = cpc, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "darkblue", 
                       mid = "white", 
                       high = "red",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text())

#Elaborando a ANACOR:
anacor <- CA(tab)

#Plotando o mapa perceptual de maneira mais elegante:

#Capturando todas as coordenadas num só objeto
ca_coordenadas <- rbind(anacor$row$coord, anacor$col$coord)
ca_coordenadas

#Capturando a quantidade de categorias por variável
id_var <- apply(cpc_geral[,c(4,6)],
                MARGIN =  2,
                FUN = function(x) nlevels(as.factor(x)))
id_var

#Juntando as coordenadas e as categorias capturadas anteriormente
ca_coordenadas_final <- data.frame(ca_coordenadas, 
                                   Variable = rep(names(id_var), id_var))

ca_coordenadas_final

#Mapa perceptual bidimensional

#Mapa perceptual elegante:
ca_coordenadas_final %>% 
  rownames_to_column() %>% 
  rename(Category = 1) %>% 
  ggplot(aes(x = Dim.1, y = Dim.2, label = Category, color = Variable)) +
  geom_point() +
  geom_label_repel() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = paste("Dimension 1:", paste0(round(anacor$eig[1,2], digits = 2), "%")),
       y = paste("Dimension 2:", paste0(round(anacor$eig[2,2], digits = 2), "%"))) +
  scale_color_manual("Variable:",
                     values = c("darkorchid", "orange")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

#Repetindo o mapa de calor dos resíduos padronizados ajustados
data.frame(qui2$stdres) %>%
  rename(categoria = 1,
         cpc = 2) %>% 
  ggplot(aes(x = fct_rev(categoria), y = cpc, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "darkblue", 
                       mid = "white", 
                       high = "red",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text())


# Elaborando o mapa perceptual tridimensional -----------------------------

# Capturando as coordenadas das categorias da variável disposta em linha
coordenadas_linhas <- anacor$row$coord
coordenadas_linhas

# Capturando as coordenadas das categorias da variável disposta em coluna
coordenadas_colunas <- anacor$col$coord
coordenadas_colunas

# Reservando um objeto que conterá nosso gráfico 3D
mapa_perceptual_3D <- plot_ly() 
mapa_perceptual_3D

# Inserindo as coordenadas das categorias da variável disposta em linha
mapa_perceptual_3D <- add_trace(mapa_perceptual_3D, 
                                x = coordenadas_linhas[,1], 
                                y = coordenadas_linhas[,2],
                                z = coordenadas_linhas[,3],
                                mode = "text", 
                                text = rownames(coordenadas_linhas),
                                textfont = list(color = "red"), 
                                showlegend = FALSE) 

mapa_perceptual_3D

# Inserindo as coordenadas das categorias da variável disposta em coluna
mapa_perceptual_3D <- add_trace(mapa_perceptual_3D, 
                                x = coordenadas_colunas[,1], 
                                y = coordenadas_colunas[,2], 
                                z = coordenadas_colunas[,3],
                                mode = "text", 
                                text = rownames(coordenadas_colunas),
                                textfont = list(color = "blue"), 
                                showlegend = FALSE) 

mapa_perceptual_3D

# Inserindo o nome dos eixos (Dimensão 1, Dimensão 2 e Dimensão 3)
mapa_perceptual_3D <- layout(mapa_perceptual_3D, 
                             scene = list(xaxis = list(title = colnames(coordenadas_linhas)[1]),
                                          yaxis = list(title = colnames(coordenadas_linhas)[2]),
                                          zaxis = list(title = colnames(coordenadas_linhas)[3]),
                                          aspectmode = "data"),
                             margin = list(l = 0, r = 0, b = 0, t = 0))

mapa_perceptual_3D

