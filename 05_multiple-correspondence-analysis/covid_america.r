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

# Utilizando a ANACOR para demonstrar comportamentos temporais de forma animada
load("covid_america_weekly.RData")

# Apresentando os dados
covid_america_weekly %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# O primeiro passo é o de estabelecer, uma ANACOR para cada período de tempo.
# Aqui, estamos estabelecendo uma ANACOR para a semana 78 desde o primeiro
# caso de COVID-19 reportado no continente americano.

# Criando uma tabela de contingências
tab <- table(covid_america_weekly$country, 
             covid_america_weekly$lethality_Q5)

tab

# Teste Qui-Quadrado
qui2_covid <- chisq.test(tab)
qui2_covid

# Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2_covid$stdres) %>%
  rename(country = 1,
         let_q5 = 2) %>% 
  ggplot(aes(x = country, y = let_q5, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 3, angle = 90) +
  scale_fill_gradient2(low = "#440154FF", 
                       mid = "white", 
                       high = "#FDE725FF",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text(angle = 90))

# Elaborando a ANACOR:
anacor <- CA(tab)

# Plotando o mapa perceptual de maneira mais elegante:

# Capturando todas as coordenadas num só objeto
ca_coordenadas <- rbind(anacor$row$coord, anacor$col$coord)
ca_coordenadas

# Capturando a quantidade de categorias por variável
id_var <- apply(covid_america_weekly[,c(1,9)],
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
             color = Variable, 
             shape = Variable)) +
  geom_point(size = 2) +
  geom_text_repel(max.overlaps = 100,
                  size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = paste("Dimension 1:", paste0(round(anacor$eig[1,2], digits = 2), "%")),
       y = paste("Dimension 2:", paste0(round(anacor$eig[2,2], digits = 2), "%"))) +
  scale_color_viridis_d(option = "viridis") +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")


# Elaborando a animação em razão do transcorrer temporal ------------------

# A base de dados a ser carregada a seguir contém as coordenadas de todas as
# ANACOR feitas, desde a 2ª até a 78ª semana na América. Foram consideradas
# duas dimensões de análise. 
load("coords_covid_america_byweek.RData")

# Apresentando os dados
coords_covid_america_byweek %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

#Sobrepondo as coordenadas dos mapas perceptuais em um só plano
coords_covid_america_byweek %>%
  ggplot() +
  geom_point(aes(x = Dim.1, y = Dim.2, 
                 color = country %in% c("L1","L2","L3","L4","L5"), size = 3,
                 shape = country %in% c("L1","L2","L3","L4","L5"))) +
  geom_text_repel(aes(x = Dim.1, y = Dim.2, 
                      label = country),
                  max.overlaps = 3000) +
  scale_color_viridis_d() +
  labs(x = "Dimensão 1",
       y = "Dimensão 2") +
  theme(legend.position = "none") -> mapas_perceptuais  

#Definindo que a interação entre os mapas perceptuais se dará em razão do passar
#das semanas
mapa_animado <- mapas_perceptuais + transition_time(week) +
  enter_fade() +
  labs(title = "Week: {frame_time}") +
  exit_fade()

#Estabelecendo um fundo branco para os gráficos
theme_set(theme_bw())

#Resultado final
animate(mapa_animado, renderer = gifski_renderer(), fps = 1)
