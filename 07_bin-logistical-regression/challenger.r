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

# REGRESSÃO LOGÍSTICA BINÁRIA E PROCEDIMENTO STEPWISE

# CARREGAMENTO DA BASE DE DADOS
load("challenger.RData")

#Visualizando a base de dados challenger
challenger %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#desgaste: quantidade de vezes em que ocorreu stress térmico
#temperatura: temperatura de lançamento (graus ºF)
#pressão: pressão de verificação de vazamento (psi-libra-força por polegada ao quadrado)
#t: teste para o lançamento (id)

#Estatísticas univariadas descritivas da base de dados
summary(challenger)

##############################################################################
#             EXEMPLO 02 - ESTIMAÇÃO DE UM MODELO LOGÍSTICO BINÁRIO          #
##############################################################################

#Não há uma variável binária para servir como uma variável dependente, certo?
#Então vamos criá-la considerando a ocorrência de desgastes de peças como a
#ocorrência de um evento que chamaremos de 'falha':
challenger %>%
  mutate(falha = ifelse(desgaste > 0,
                        yes = "sim",
                        no = "não"),
         falha = factor(falha)) -> challenger

#Vamos observar as alterações na base de dados original:
challenger %>%
  select(desgaste, falha, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

#Estimando o modelo logístico binário
modelo_challenger <- glm(formula = falha ~ . -desgaste -t,
                         data = challenger,
                         family = "binomial")

#Parâmetros do modelo_default
summary(modelo_challenger)
#Note que não há a explicitação do estatística geral do modelo,
#nem tampouco do valor de LL e dos intervalos de confiança.

#Uma solução rápida para o caso pode ser a utilização da função summ do pacote jtools
summ(model = modelo_challenger, confint = T, digits = 4, ci.width = 0.95)
export_summs(modelo_challenger, scale = F, digits = 4)

#Procedimento Stepwise
step_challenger <- step(object = modelo_challenger,
                        k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

#Parâmetros do modelo step_challenger
summ(model = step_challenger, confint = T, digits = 4, ci.width = 0.95)

#Fazendo predições para o modelo step_challenger:
#Exemplo 1: qual a probabilidade média de falha a 70ºF (~21ºC)?
predict(object = step_challenger,
        data.frame(temperatura = 70),
        type = "response")

#Exemplo 2: qual a probabilidade média de falha a 77ºF (25ºC)?
predict(object = step_challenger,
        data.frame(temperatura = 77),
        type = "response")

#Exemplo 3: qual a probabilidade média de falha a 34ºF (~1ºC) - manhã do lançamento?
predict(object = step_challenger,
        data.frame(temperatura = 34),
        type = "response")

#Construção da sigmoide - probabilidade de evento em função da variável 'temperatura'
ggplotly(
  challenger %>% 
  mutate(phat = predict(object = step_challenger,
                        newdata = challenger,
                        type = "response"),
         falha = as.numeric(falha) - 1) %>% 
  ggplot() +
  geom_point(aes(x = temperatura, y = falha), color = "#95D840FF", size = 2) +
  geom_smooth(aes(x = temperatura, y = phat), 
              method = "glm", formula = y ~ x, 
              method.args = list(family = "binomial"), 
              se = F,
              color = "#440154FF", size = 2) +
  labs(x = "Temperatura",
       y = "Falha") +
  theme_bw()
)

#Nossa homenagem aos astronautas
image_scale(image_read("https://img.ibxk.com.br///2016/01/29/29182307148581.jpg?w=1200&h=675&mode=crop&scale=both"),
            "x320")
