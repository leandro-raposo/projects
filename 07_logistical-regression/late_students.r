pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","lmtest","caret","pROC","ROCR","nnet","magick",
             "cowplot")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#  REGRESSÃO LOGÍSTICA BINÁRIA
#Estabelecendo uma função para a probabilidade de ocorrência de um evento
prob <- function(z){
  prob = 1 / (1 + exp(-z))
}

#Plotando a curva sigmóide teórica de ocorrência de um evento para um range
#do logito z entre -5 e +5
data.frame(z = -5:5) %>%
  ggplot() +
  stat_function(aes(x = z, color = "Prob. Evento"),
                fun = prob,
                size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "Logito z",
       y = "Probabilidade") +
  theme_bw()

# CARREGAMENTO DA BASE DE DADOS
load(file = "Atrasado.RData")

#Visualizando a base de dados
Atrasado %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Estatísticas descritivas univariadas da base de dados
summary(Atrasado)

#Tabela de frequências absolutas da variável 'atrasado'
table(Atrasado$atrasado) 

##############################################################################
#           EXEMPLO 01 - ESTIMAÇÃO DE UM MODELO LOGÍSTICO BINÁRIO            #
##############################################################################
modelo_atrasos <- glm(formula = atrasado ~ dist + sem, 
                      data = Atrasado, 
                      family = "binomial")

#Parâmetros do modelo_atrasos
summary(modelo_atrasos)
#Note que não há a explicitação do estatística geral do modelo,
#nem tampouco do valor de LL e dos intervalos de confiança.

#Extração do valor de Log-Likelihood (LL)
logLik(modelo_atrasos)

#Outras maneiras de apresentar os outputs do modelo
#função summ do pacote jtools
summ(modelo_atrasos, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_atrasos, scale = F, digits = 6)

#Fazendo predições para o modelo_atrasos. Exemplo: qual a probabilidade média
#de se chegar atrasado quando o trajeto tem 7 km e passa-se por 10 semáforos no percurso?
predict(object = modelo_atrasos, 
        data.frame(dist = 7, sem = 10), 
        type = "response")

##############################################################################
#               EXEMPLO 01 - CONSTRUÇÃO DE UMA MATRIZ DE CONFUSÃO            #
##############################################################################
# Adicionando os valores previstos de probabilidade da base de dados
Atrasado$phat <- modelo_atrasos$fitted.values

#Visualizando a base de dados com a variável 'phat'
Atrasado %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Matriz de confusão para cutoff = 0.5 (função confusionMatrix do pacote caret)
confusionMatrix(table(predict(modelo_atrasos, type = "response") >= 0.5,
                      Atrasado$atrasado == 1)[2:1, 2:1])

#Visualizando os principais indicadores desta matriz de confusão
data.frame(Sensitividade = confusionMatrix(table(predict(modelo_atrasos,
                                                         type = "response") >= 0.5,
                                          Atrasado$atrasado == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(modelo_atrasos,
                                                          type = "response") >= 0.5,
                                          Atrasado$atrasado == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(modelo_atrasos,
                                                    type = "response") >= 0.5,
                                          Atrasado$atrasado == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)

#Matriz de confusão para cutoff = 0.3
confusionMatrix(table(predict(modelo_atrasos, type = "response") >= 0.3,
                      Atrasado$atrasado == 1)[2:1, 2:1])

#Matriz de confusão para cutoff = 0.7
confusionMatrix(table(predict(modelo_atrasos, type = "response") >= 0.7,
                      Atrasado$atrasado == 1)[2:1, 2:1])

##############################################################################
#  EXEMPLO 01 - IGUALANDO OS CRITÉRIOS DE ESPECIFICIDADE E DE SENSITIVIDADE  #
##############################################################################
#Tentaremos estabelecer um critério que iguale a probabilidade de acerto
#daqueles que chegarão atrasados (sensitividade) e a probabilidade de acerto
#daqueles que não chegarão atrasados (especificidade).

#ATENÇÃO: o que será feito a seguir possui fins didáticos, apenas. DE NENHUMA
#FORMA o procedimento garante a maximização da acurácia do modelo!

#função prediction do pacote ROCR
predicoes <- prediction(predictions = modelo_atrasos$fitted.values, 
                        labels = Atrasado$atrasado) 
#a função prediction, do pacote ROCR, cria um objeto com os dados necessários
#para a futura plotagem da curva ROC.

#função performance do pacote ROCR
dados_curva_roc <- performance(predicoes, measure = "sens") 
#A função peformance(), do pacote ROCR, extrai do objeto 'predicoes' os 
#dados de sensitividade e de especificidade para a plotagem.

#Desejamos os dados da sensitividade e de especificidade. Então, devemos
#digitar os seguintes códigos::

sensitividade <- (performance(predicoes, measure = "sens"))@y.values[[1]] 

especificidade <- (performance(predicoes, measure = "spec"))@y.values[[1]]

#Extraindo os cutoffs:
cutoffs <- dados_curva_roc@x.values[[1]] 

#Até o momento, foram extraídos 3 vetores: 'sensitividade', 'especificidade' 
#e 'cutoffs'. Poder-se-ia plotar normalmente a partir daqui com a linguagem 
#base do R, mas demos preferência à ferramenta ggplot2. Assim, criamos um data 
#frame que contém os vetores mencionados.

dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)

#Visualizando o novo data frame dados_plotagem
dados_plotagem %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Plotando:
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

##############################################################################
#                       EXEMPLO 01 - CONSTRUÇÃO DA CURVA ROC                 #
##############################################################################
#função roc do pacote pROC
ROC <- roc(response = Atrasado$atrasado, 
           predictor = modelo_atrasos$fitted.values)

ggplotly(
  ggroc(ROC, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)
