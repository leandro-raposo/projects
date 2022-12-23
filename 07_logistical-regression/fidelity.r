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

# REGRESSÃO LOGÍSTICA BINÁRIA COM VARIÁVEIS EXPLICATIVAS QUANTI E QUALIS

# CARREGAMENTO DA BASE DE DADOS
load("dados_fidelidade.RData")

#Visualizando a base de dados dados_fidelidade
dados_fidelidade %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 16)

#Estatísticas Univariadas da Base de Dados
summary(dados_fidelidade)

#Tabela de frequências absolutas das variáveis qualitativas referentes aos
#atributos da loja na percepção dos consumidores
table(dados_fidelidade$atendimento)
table(dados_fidelidade$sortimento)
table(dados_fidelidade$acessibilidade)
table(dados_fidelidade$preço)

glimpse(dados_fidelidade)
#Note que as variáveis qualitativas já estão como fator (fct)

##############################################################################
#             EXEMPLO 03 - ESTIMAÇÃO DE UM MODELO LOGÍSTICO BINÁRIO          #
##############################################################################
modelo_fidelidade <- glm(formula = fidelidade ~ . - id, 
                         data = dados_fidelidade, 
                         family = "binomial")

#Parâmetros do modelo_fidelidade
summary(modelo_fidelidade)

#Outro modo de apresentar os outputs do modelo_fidelidade
summ(modelo_fidelidade, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_fidelidade, scale = F, digits = 6)

#Procedimento Stepwise
step_fidelidade <- step(object = modelo_fidelidade,
                        k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

#Parâmetros do modelo step_fidelidade
summary(step_fidelidade)
#Note que sem a dummização, o R consegue calcular corretamente os parâmetros,
#mas o procedimento Stepwise, quando aplicado, não surte efeitos!

##############################################################################
#                    EXEMPLO 03 -  PROCEDIMENTO N-1 DUMMIES                  #
##############################################################################
#Dummizando as variáveis atendimento, sortimento, acessibilidade e preço. O 
#código abaixo, automaticamente, fará: a) a dummização das variáveis originais;
#b)removerá as variáveis dummizadas originais; c) estabelecerá como categorias 
#de referência as categorias de label 1 de cada variável original.
fidelidade_dummies <- dummy_columns(.data = dados_fidelidade,
                                    select_columns = c("atendimento", 
                                                       "sortimento",
                                                       "acessibilidade", 
                                                       "preço"),
                                    remove_selected_columns = T,
                                    remove_first_dummy = T)

##############################################################################
#                      EXEMPLO 03 -  REESTIMANDO O MODELO                    #
##############################################################################
#Visualizando a base de dados fidelidade_dummies
fidelidade_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

modelo_fidelidade_dummies <- glm(formula = fidelidade ~ . -id, 
                                 data = fidelidade_dummies, 
                                 family = "binomial")

#Parâmetros do modelo_fidelidade_dummies
summary(modelo_fidelidade_dummies)

#Valor do LL do modelo_fidelidade_dummies
logLik(modelo_fidelidade_dummies)

#Procedimento Stepwise
step_fidelidade_dummies <- step(object = modelo_fidelidade_dummies,
                        k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

#Parâmetros do modelo step_fidelidade_dummies
summary(step_fidelidade_dummies)

#Outro modo de apresentar os outputs do modelo step_fidelidade_dummies
summ(step_fidelidade_dummies, confint = T, digits = 3, ci.width = .95)
export_summs(step_fidelidade_dummies, scale = F, digits = 6)

#Valor do LL do modelo step_fidelidade_dummies
logLik(step_fidelidade_dummies)

#Comparando os modelos step_fidelidade_dummies e modelo_fidelidade_dummies
#função lrtest do pacote lmtest
lrtest(modelo_fidelidade_dummies, step_fidelidade_dummies)

export_summs(modelo_fidelidade_dummies, step_fidelidade_dummies, scale = F,
             digits = 4)

##############################################################################
#              EXEMPLO 03 - CONSTRUÇÃO DE UMA MATRIZ DE CONFUSÃO             #
##############################################################################
confusionMatrix(
  table(predict(step_fidelidade_dummies, type = "response") >= 0.5, 
                      dados_fidelidade$fidelidade == "sim")[2:1, 2:1]
  )

##############################################################################
#  EXEMPLO 03 - IGUALANDO OS CRITÉRIOS DE ESPECIFICIDADE E DE SENSITIVIDADE  #
##############################################################################
#Analogamente ao realizado para o Exemplo 01, vamos estabelecer um critério
#que iguale a probabilidade de acerto daqueles que apresentarão fidelização ao
#estabelecimento varejista (sensitividade) e a probabilidade de acerto daqueles
#que não apresentarão fidelização (especificidade).

#ATENÇÃO: o que será feito a seguir possui fins didáticos, apenas. DE NENHUMA
#FORMA o procedimento garante a maximização da acurácia do modelo!

#função prediction do pacote ROCR
predicoes <- prediction(predictions = step_fidelidade_dummies$fitted.values, 
                        labels = dados_fidelidade$fidelidade) 
#a função prediction, do pacote ROCR, cria um objeto com os dados necessários
#para a futura plotagem da curva ROC.

#função performance do pacote ROCR
dados_curva_roc <- performance(predicoes, measure = "sens") 
#A função peformance(), do pacote ROCR, extraiu do objeto 'predicoes' os 
#dados de sensitividade, de sensibilidade e de especificidade para a plotagem.

#Porém, desejamos os dados da sensitividade, então devemos fazer o seguinte 
#ajuste:
sensitividade <- dados_curva_roc@y.values[[1]] 
#extraindo dados da sensitividade do modelo

especificidade <- performance(predicoes, measure = "spec") 
#extraindo os dados da especificidade, mas também há que se fazer um ajuste para a 
#plotagem:
especificidade <- especificidade@y.values[[1]]

cutoffs <- dados_curva_roc@x.values[[1]] 
#extraindo os cutoffs do objeto 'sensitividade'.

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
#                     EXEMPLO 03 - CONSTRUÇÃO DA CURVA ROC                   #
##############################################################################
ROC <- roc(response = dados_fidelidade$fidelidade, 
           predictor = step_fidelidade_dummies$fitted.values)

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
