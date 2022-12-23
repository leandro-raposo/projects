pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Carregando a base de dados
load(file = "corrupcao.RData")

##################################################################################
#                    OBSERVANDO OS DADOS CARREGADOS DA BASE corrupcao            #
##################################################################################
glimpse(corrupcao) #Visualização das observações e das especificações referentes
                   #às variáveis da base de dados

levels(glimpse(corrupcao$regiao)) #Observando os rótulos da variável regiao
table(corrupcao$regiao) #Tabela de frequências da variável regiao

#Estatísticas univariadas
summary(corrupcao)

#Estimando um modelo, erroneamente, com o problema da ponderação arbitrária
modelo_corrupcao <- lm(formula = cpi ~ as.numeric(regiao), 
                       data = corrupcao)

#Observando os parâmetros do modelo_corrupcao
summary(modelo_corrupcao)

#Calculando os intervalos de confiança

confint(modelo_corrupcao, level = 0.95) # siginificância 5%

#Plotando os fitted values do modelo_corrupcao considerando, PROPOSITALMENTE, a
#ponderação arbitrária, isto é, assumindo que a América do Sul vale 1; que a 
#Oceania vale 2; a Europa, 3; EUA e Canadá, 4; e Ásia, 5.
corrupcao %>%
  mutate(rotulo = paste(pais, cpi)) %>%
  ggplot(aes(x = as.numeric(regiao), y = cpi, label = rotulo)) +
  geom_point(color = "#FDE725FF") +
  stat_smooth(aes(color = "Fitted Values"),
              method = "lm", 
              formula = y ~ x) +
  labs(x = "Região",
       y = "Corruption Perception Index") +
  scale_x_discrete(labels = c("1" = "América do Sul", 
                                "2" = "Oceania", 
                                "3" = "Europa", 
                                "4" = "EUA e Canadá", 
                                "5" = "Ásia")) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  geom_text_repel() +
  theme_bw()

#################################################################################
#                            PROCEDIMENTO N-1 DUMMIES                           #
#################################################################################
#Dummizando a variável regiao. O código abaixo, automaticamente, fará: a) o
#estabelecimento de dummies que representarão cada uma das regiões da base de 
#dados; b)removerá a variável dummizada original; c) estabelecerá como categoria 
#de referência a dummy mais frequente.
corrupcao_dummies <- dummy_columns(.data = corrupcao,
                                   select_columns = "regiao",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = T)

#Visualizando a base de dados dummizada
corrupcao_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

##################################################################################
#                        ESTIMAÇÃO DO MODELO DE REGRESSÃO                        #
##################################################################################
#Modelagem com todas as variáveis
modelo_corrupcao_dummies <- lm(cpi ~ . - pais, corrupcao_dummies)

#Parâmetros do modelo_corrupcao_dummies
summary(modelo_corrupcao_dummies)

#Plotando o modelo_corrupcao_dummies de forma interpolada
my_plot3 <- 
corrupcao %>%
  mutate(rotulo = paste(pais, cpi)) %>%
  ggplot(aes(x = as.numeric(regiao), y = cpi, label = rotulo)) +
  geom_point(color = "#FDE725FF") +
  stat_smooth(aes(color = "Fitted Values"),
              method = "lm", 
              formula = y ~ bs(x, df = 4)) +
  labs(x = "Região",
       y = "Corruption Perception Index") +
  scale_x_discrete(labels = c("1" = "América do Sul", 
                                "2" = "Oceania", 
                                "3" = "Europa", 
                                "4" = "EUA e Canadá", 
                                "5" = "Ásia")) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  geom_text_repel() +
  theme_bw()
my_plot3

#Com GIF
ggsave("my_plot3.png")
my_plot3 <- image_read("my_plot3.png") #função do pacote magick

gif <- image_read("https://i.pinimg.com/originals/89/2e/09/892e09e6609a951fa45d6799cc3fa3f3.gif")

frames <- image_composite(my_plot3, gif, offset = "+750+30")

animation <- image_animate(frames, fps = 10) #função do pacote magick
image_scale(animation, "x550")
beep("treasure")
