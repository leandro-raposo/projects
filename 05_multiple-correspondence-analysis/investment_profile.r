pacotes <- c("plotly","tidyverse","ggrepel","sjPlot","knitr","kableExtra",
             "FactoMineR","cabootcrs")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Análise de Correspondênciasa Múltiplas (ACM) - Abordagem Teórica --------
# Carregando a base de dados
load(file = "perfil_investidor_aplicacao.RData")

# Apresentando os dados
perfil_investidor_aplicacao %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Não há como se estabelecer umas única tabela de contingência que abranja todas
# as variáveis presentes na base de dados. Assim, poderíamos utilizar a função
# table() ou a função sjt.xtab() para analisarmos as variáveis duas a duas:

# Perfil x Aplicação
sjt.xtab(var.row = perfil_investidor_aplicacao$perfil, 
         var.col = perfil_investidor_aplicacao$aplicacao,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Perfil x Estado Civil
sjt.xtab(var.row = perfil_investidor_aplicacao$perfil, 
         var.col = perfil_investidor_aplicacao$estado_civil,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)

# Aplicação x Estado Civil
sjt.xtab(var.row = perfil_investidor_aplicacao$aplicacao, 
         var.col = perfil_investidor_aplicacao$estado_civil,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)


# Estudo do Qui-Quadrado de cada tabela de contingências ------------------

# A) Perfil x Aplicação
tab_perfil_aplicacao <- table(perfil_investidor_aplicacao$perfil,
                              perfil_investidor_aplicacao$aplicacao)

tab_perfil_aplicacao

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

qui2_aplic_estad <- chisq.test(tab_aplicacao_estadocivil)
qui2_aplic_estad

# Para a ANACOR, juntamos os cruzamentos das categorias de DUAS variáveis 
# categóricas numa matriz de contingências. Nesse momento, porém, temos TRÊS
# variáveis categóricas. Como proceder?

# Matriz Binária e Matriz de Burt -----------------------------------------

# Para se estabelecer uma matriz binária:
matriz_binaria <- getindicator(Xinput = perfil_investidor_aplicacao[, 2:4])
matriz_binaria

CA(matriz_binaria)

# Para a matriz de Burt:
matriz_burt <- getBurt(Xinput = perfil_investidor_aplicacao[, 2:4])
matriz_burt

CA(matriz_burt)


# Rodando a ACM -----------------------------------------------------------
ACM <- MCA(perfil_investidor_aplicacao[, 2:4], method = "Indicador")


# O componte 'var$coord', presente no objeto ACM, contém as coordenadas 
# de cada categoria. 
ACM$var$coord

# Assim, temos as coordenadas:
round(ACM$var$coord, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# As inércias principais estão computadas no componente 'eig':
ACM$eig

# As coordenadas de cada observação estão no componente 'ind$coord' do objeto
# ACM:
ACM$ind$coord

# Para estudarmos o percentual da inérica principal explicada por 
# dimensão, podemos:
categorias <- apply(perfil_investidor_aplicacao[,2:4], 
                    MARGIN =  2, 
                    FUN = function(x) nlevels(as.factor(x)))

categorias

It <- (sum(categorias) - length(categorias)) / length(categorias)
It

sum(ACM$eig[,1])

It_explicada <- ACM$eig[,1] / sum(ACM$eig[,1])

data.frame(Dimensão = paste("Dimensão", 1:length(It_explicada)),
           Inércia_Total = It_explicada) %>%
  ggplot(aes(x = Dimensão, 
             y = Inércia_Total, 
             label = paste0(round(Inércia_Total,3)*100,"%"))) +
  geom_bar(stat = "identity",
           color = "#440154FF", 
           fill = "#287C8EFF") +
  geom_label(vjust = 2) +
  labs(title = paste("Inércia Total Explicada de",
                     paste0(sum(It_explicada) * 100),"%")) +
   theme_bw()

# Já o número de dimensões da ACM é dado por:
dimensoes <- sum(categorias) - length(categorias)
dimensoes

# O mapa perceptual -------------------------------------------------------

#Para o estabelecimento de um Mapa Perceptual, precisamos:

#1º Definir o número de categorias por variável
categorias <- apply(perfil_investidor_aplicacao[,2:4], 
                    MARGIN =  2, 
                    FUN = function(x) nlevels(as.factor(x)))

#2º transformar o objeto ACM em um data frame, levando-se em consideração quais 
#tipos de coordenadas se quer plotar. Neste exemplo, utilizaremos as coordenadas
#dadas pela matriz de binária
ACM_mp <- data.frame(ACM$var$coord, Variável = rep(names(categorias), categorias))

ACM_mp %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  mutate(Categoria = gsub("perfil.","", Categoria),
         Categoria = gsub("aplicacao.","", Categoria),
         Categoria = gsub("estado_civil.","", Categoria)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Plotando o Mapa Perceptual:
ACM_mp %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Dim.1, 
             y = Dim.2, 
             label = Categoria, 
             color = Variável, 
             shape = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey") +
  labs(x = paste("Dimensão 1:", paste0(round(ACM$eig[1,2], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(ACM$eig[2,2], 2), "%"))) +
  scale_color_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

#Plotando o Mapa Perceptual, evidenciando-se as coordenadas das observações:

#Criando um mapa perceptual mais elegante, com as posições relativas de cada
#observação

#1º Salvar as posições relativas de cada observação
ACM_observacoes_df <- data.frame(ACM$ind$coord)

#2º Utilizando a estrutura do mapa perceptual já estabelecido, vamos acrescentar 
#as novas informações presentes no objeto mca_observacoes_df

ACM_observacoes_df %>% 
  ggplot(aes(x = Dim.1, y = Dim.2, label = perfil_investidor_aplicacao$estudante)) +
  geom_point(shape = 17, color = "#E76F5AFF", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_text_repel(max.overlaps = 100, size = 3) +
  geom_density2d(color = "gray80") +
  geom_label_repel(data = ACM_mp, 
                   aes(x = Dim.1, y = Dim.2, 
                       label = rownames(ACM_mp), 
                       fill = Variável), 
                   color = "white") +
  labs(x = paste("Dimensão 1:", paste0(round(ACM$eig[,2][1], digits = 2), "%")),
       y = paste("Dimensão 2:", paste0(round(ACM$eig[,2][2], digits = 2), "%"))) +
  scale_fill_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

#E como criar um mapa perceptual 3D de uma ACM?
ACM_3D <- plot_ly()

# Adicionando as coordenadas
ACM_3D <- add_trace(p = ACM_3D,
                    x = ACM_mp[,1],
                    y = ACM_mp[,2],
                    z = ACM_mp[,3],
                    mode = "text",
                    text = rownames(ACM_mp),
                    textfont = list(color = "#440154FF"),
                    showlegend = FALSE)

# Adicionando as labels das dimensões
ACM_3D <- layout(p = ACM_3D,
                 scene = list(xaxis = list(title = colnames(ACM_mp)[1]),
                              yaxis = list(title = colnames(ACM_mp)[2]),
                              zaxis = list(title = colnames(ACM_mp)[3]),
                              aspectmode = "data"))

ACM_3D
