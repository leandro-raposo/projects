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

#Listar os arquivos do nosso project
list.files()

#Carregando a base de dados
load(file = "dados/tempodist.RData")

#################################################################################
#                 OBSERVANDO OS DADOS CARREGADOS DO DATASET tempodist           #
#################################################################################
tempodist %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

#Visualizando as observações e as especificações referentes às variáveis do dataset
glimpse(tempodist) 

#Estatísticas univariadas
summary(tempodist)

#################################################################################
#                             GRÁFICO DE DISPERSÃO                              #
#################################################################################
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", se = F, size = 2) +
    xlab("Distância") +
    ylab("Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

#################################################################################
#            MODELAGEM DE UMA REGRESSÃO LINEAR SIMPLES PARA O EXEMPLO 01        #
#################################################################################
#Estimando o modelo
modelo_tempodist <- lm(formula = tempo ~ distancia,
                       data = tempodist)

#Observando os parâmetros do modelo_tempodist
summary(modelo_tempodist)

#Outras maneiras de apresentar os outputs do modelo
#função summ do pacote jtools
summ(modelo_tempodist, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_tempodist, scale = F, digits = 4)

#Salvando fitted values (variável yhat) e residuals (variável erro) no dataset
tempodist$yhat <- modelo_tempodist$fitted.values
tempodist$erro <- modelo_tempodist$residuals

#Visualizando a base de dados com as variáveis yhat e erro
tempodist %>%
  select(tempo, distancia, yhat, erro) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Gráfico didático para visualizar o conceito de R²
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", se = F, size = 2) +
    geom_hline(yintercept = 30, color = "grey50", size = .5) +
    geom_segment(aes(color = "Ychapéu - Ymédio", x = distancia, xend = distancia,
                     y = yhat, yend = mean(tempo)), size = 0.7, linetype = 2) +
    geom_segment(aes(color = "Erro = Y - Ychapéu", x = distancia, xend = distancia,
                     y = tempo, yend = yhat), size = 0.7, linetype = 3) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = c("#55C667FF", "grey50", "#440154FF")) +
    theme_classic()
)

#Cálculo manual do R²
R2 <- (sum((tempodist$yhat - mean(tempodist$tempo))^2))/
      ((sum((tempodist$yhat - mean(tempodist$tempo))^2)) + (sum((tempodist$erro)^2)))

round(R2, digits = 4)

#coeficiente de ajuste (R²) é a correlação ao quadrado
cor(tempodist[1:2])

#Modelo auxiliar para mostrar R² igual a 100% (para fins didáticos)
modelo_auxiliar <- lm(formula = yhat ~ distancia, #note que aqui o yhat é a dependente
                   data = tempodist)
summary(modelo_auxiliar)

#Gráfico mostrando o perfect fit
my_plot <- 
  ggplot(tempodist, aes(x = distancia, y = yhat)) +
  geom_point(color = "#39568CFF", size = 5) +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm", se = F, size = 2) +
  labs(x = "Distância",
       y = "Tempo") +
  scale_color_manual("Legenda:",
                     values = "grey50") +
  theme_cowplot()
my_plot

#Com JPEG
ggdraw() + #funções ggdraw, draw_image e draw_plot do pacote cowplot
  draw_image("https://cdn.pixabay.com/photo/2017/02/16/10/20/target-2070972_960_720.png",
             x = 0.075, y = -0.15, scale = .34) +
  draw_image("https://imagensemoldes.com.br/wp-content/uploads/2019/10/O-Show-da-Luna-Luna-PNG-08.png",
             x = -0.235, y = 0.25, scale = .37) +
  draw_plot(my_plot)


##Voltando ao nosso modelo original:
#Plotando o Intervalo de Confiança de 90%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.90,) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

#Plotando o Intervalo de Confiança de 95%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.95) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

#Plotando o Intervalo de Confiança de 99%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.99) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

#Plotando o Intervalo de Confiança de 99,999%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.99999) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

#Calculando os intervalos de confiança

confint(modelo_tempodist, level = 0.90) # siginificância 10%
confint(modelo_tempodist, level = 0.95) # siginificância 5%
confint(modelo_tempodist, level = 0.99) # siginificância 1%
confint(modelo_tempodist, level = 0.99999) # siginificância 0,001%

#Fazendo predições em modelos OLS - e.g.: qual seria o tempo gasto, em média, para
#percorrer a distância de 25km?
predict(object = modelo_tempodist,
        data.frame(distancia = 25))

#Caso se queira obter as predições com os IC
predict(object = modelo_tempodist,
        data.frame(distancia = 25),
        interval = "confidence", level = 0.95)


#####################################################################################
#     NOVA MODELAGEM PARA O EXEMPLO 01, COM NOVO DATASET QUE CONTÉM REPLICAÇÕES     #
#####################################################################################

# Quantas replicações de cada linha você quer? -> função slice
tempodistnovo <- tempodist %>%
  slice(rep(1:n(), each=3))

# Reestimando o modelo
modelo_tempodistnovo <- lm(formula = tempo ~ distancia,
                        data = tempodistnovo)

#Observando os parâmetros do modelo_tempodistnovo
summary(modelo_tempodistnovo)

#Calculando os novos intervalos de confiança
confint(modelo_tempodistnovo, level = 0.95) # siginificância 5%

#Plotando o Novo Gráfico com Intervalo de Confiança de 95%
#Note o estreitamento da amplitude dos intervalos de confiança!
ggplotly(
  ggplot(tempodistnovo, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.95) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

#PROCEDIMENTO ERRADO: ELIMINAR O INTERCEPTO QUANDO ESTE NÃO SE MOSTRAR
#ESTATISTICAMENTE SIGNIFICANTE
modelo_errado <- lm(formula = tempo ~ 0 + distancia,
                              data = tempodist)

#Observando os parâmetros do modelo_errado
summary(modelo_errado)

#Comparando os parâmetros do modelo_tempodist X modelo_errado
export_summs(modelo_tempodist, modelo_errado, scale = F, digits = 4)

#Gráfico didático para visualizar o viés decorrente de se eliminar erroneamente
#o intercepto em modelos regressivos
my_plot2 <-
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
  geom_point(color = "#39568CFF", size = 2.5) +
  geom_smooth(aes(color = "Fitted Values OLS"),
              method = "lm", se = F, size = 1.5) +
  geom_segment(aes(color = "Sem Intercepto",
                   x = min(distancia),
                   xend = max(distancia),
                   y = modelo_errado$coefficients[1]*min(distancia),
                   yend = modelo_errado$coefficients[1]*max(distancia)),
               size = 1.5) +
  labs(x = "Distância",
       y = "Tempo") +
  scale_color_manual("Legenda:",
                     values = c("grey50", "#1F968BFF")) +
  theme_cowplot()
my_plot2

#Com JPEG
ggdraw() +
  draw_image("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQKNf7Jk3b2LG23egCN7w7TW0275Vd2_lhYWHLlGGizplLYc74wLukF-EbOIB8YY8YB9L0&usqp=CAU",
             x = 0.065, y = -0.151, scale = .49) +
  draw_plot(my_plot2)

my_plot2
ggdraw() +
  draw_image("https://cdn.pixabay.com/photo/2014/04/03/00/36/mark-308835_960_720.png",
             x = -0.23, y = 0.24, scale = .25) +
  draw_image("https://i.pinimg.com/originals/38/13/f4/3813f4996821abb81b888c9a3f6d7c07.png",
             x = 0.07, y = -0.151, scale = .23) +
  draw_plot(my_plot2)
beep() #função do pacote beepr
