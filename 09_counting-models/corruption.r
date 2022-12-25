pacotes <- c("plotly","tidyverse","knitr","kableExtra","reshape2","ggrepel",
             "fastDummies","lmtest","splines","jtools","questionr","MASS",
             "pscl","overdisp","magick","cowplot","beepr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# A DISTRIBUIÇÃO POISSON - PARTE CONCEITUAL
#Estabelecendo uma função da distribuição Poisson com lambda = 1
poisson_lambda1 <- function(m){
  lambda <- 1
  (exp(-lambda) * lambda ^ m) / factorial(m)
}

#Estabelecendo uma função da distribuição Poisson com lambda = 4
poisson_lambda4 <- function(m){
  lambda <- 4
  (exp(-lambda) * lambda ^ m) / factorial(m)
}

#Estabelecendo uma função da distribuição Poisson com lambda = 10
poisson_lambda10 <- function(m){
  lambda <- 10
  (exp(-lambda) * lambda ^ m) / factorial(m)
}

#Plotagem das funções estabelecidas anteriormente
data.frame(m = 0:20) %>%
  ggplot(aes(x = m)) +
  stat_function(fun = poisson_lambda1, size = 1.5,
                aes(color = "01")) +
  stat_function(fun = poisson_lambda4, size = 1.5,
                aes(color = "04")) +
  stat_function(fun = poisson_lambda10, size = 1.5,
                aes(color = "10")) +
  scale_color_viridis_d("Valores de" ~ lambda ~ "") +
  labs(y = "Probabilidades", x = "m") +
  theme_bw()


##############################################################################
#                      REGRESSÃO PARA DADOS DE CONTAGEM                      #
#                  CARREGAMENTO DA BASE DE DADOS corruption                  #
##############################################################################
#Fisman, R.; Miguel, E. Corruption, Norms, and Legal Enforcement:
#Evidence from Diplomatic Parking Tickets.
#Journal of Political Economy, v. 15, n. 6, p. 1020-1048, 2007.
#https://www.journals.uchicago.edu/doi/abs/10.1086/527495

load(file = "corruption.RData")

##############################################################################
#                   OBSERVAÇÃO DA BASE DE DADOS corruption                   #
##############################################################################
#Visualizando a base de dados
corruption %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 19)

glimpse(corruption) #Visualização das observações e das  especificações 
#referentes às variáveis da base de dados

#Estatísticas descritivas univariadas e tabela de frequências
summary(corruption)

#Tabela de frequências da variável dependente (função freq para gerar tabelas de
#frequência do pacote questionr)
freq(corruption$violations) %>%
  kable()%>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 19)

#Histograma da variável dependente
ggplotly(
  corruption %>%
    ggplot(aes(x = violations,
               fill = ..count..)) +
    geom_histogram(bins = round(2 * nrow(corruption) ^ (1 / 3)),
                   color = "black") +
    scale_fill_gradient("Contagem",
                        low = "#440154FF", 
                        high = "#FDE725FF") +
    labs(x = "Quantidade de violações de trânsito",
         y = "Frequência") +
    theme_bw()
)

#Diagnóstico preliminar para observação de eventual igualdade entre a média e
#a variância da variável dependente 'violations'
corruption %>%
  summarise(Média = mean(violations),
            Variância = var(violations)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 30)

#Comportamento das variáveis 'corruption' e 'violations' antes e depois do 
#início da vigência da lei
corruption %>%
  mutate(lnviolations = log(violations),
         lnviolations = ifelse(lnviolations == -Inf,
                               yes = 0, 
                               no = lnviolations)) %>%
  ggplot(aes(x = corruption, y = lnviolations)) +
  geom_point(color = "black") +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm",
              formula = y ~ splines::bs(x),
              se = FALSE, size = 2) +
  geom_text_repel(aes(label = code), # pacote ggrepel
                  size = 2,
                  color = "black",
                  max.overlaps = 100) +
  labs(y = "Violações de Trânsito em NY (logs)",
       x = "Índice de Corrupção dos Países") +
  scale_color_manual("Label:",
                     values = "gold") +
  facet_wrap(~post) +
  theme_bw()


################################################################################
#                        ESTIMAÇÃO DO MODELO POISSON                           #
################################################################################
#Estimação do modelo
modelo_poisson <- glm(formula = violations ~ staff + post + corruption,
                      data = corruption,
                      family = "poisson")

#Parâmetros do modelo_poisson
summary(modelo_poisson)

#Extração do valor de Log-Likelihood (LL)
logLik(modelo_poisson)

#Outra forma de visualização dos parâmetros - função summ do pacote jtools
summ(modelo_poisson, digits = 4, confint = T, ci.width = 0.95)
export_summs(modelo_poisson, scale = F, digits = 4)

#LR Test - função lrtest do pacote lmtest
#(likelihood ratio test para comparação de LL's entre modelos)
lrtest(modelo_poisson) #no caso, comparação com modelo nulo (somente com intercepto)

#Todas as variáveis preditoras se mostraram estatisticamente diferentes de zero,
#considerando-se um nível de significância de 5%, ceteris paribus. Porém, já se
#pode afirmar que a estimação Poisson é a mais adequada?

################################################################################
#            TESTE DE SUPERDISPERSÃO DE CAMERON E TRIVEDI (1990)               #
################################################################################
#CAMERON, A. C.; TRIVEDI, P. K. Regression-based tests for overdispersion in
#the Poisson model. Journal of Econometrics, v. 46, n. 3, p. 347-364, 1990.

#1º Passo: estimar um modelo Poisson;
#2º Passo: criar uma nova variável (Y*) utilizando os fitted values do modelo
#Poisson estimado anteriormente;
#3º Passo: estimar um modelo auxiliar OLS, com a variável Y* como variável
#dependente, os fitted values do modelo Poisson como única variável preditora e 
#sem o intercepto;
#4º Passo: Observar a significância do parâmetro beta.

#Adicionando os fitted values do modelo Poisson (lambda_poisson) à base de dados:
corruption$lambda_poisson <- modelo_poisson$fitted.values

#Criando a nova variável Y*:
attach(corruption)
corruption$ystar <- (((violations - lambda_poisson) ^ 2)
                     - violations) / lambda_poisson
detach(corruption)

#Estimando o modelo auxiliar OLS, sem o intercepto:
modelo_auxiliar <- lm(formula = ystar ~ 0 + lambda_poisson,
                      data = corruption)

#Observando os parâmetros do modelo_auxiliar
summary(modelo_auxiliar)

#Caso o p-value do parâmetro do lambda_poisson seja maior que 0.05,
#verifica-se a existência de equidispersão nos dados.
#Caso contrário, diagnostica-se a existência de superdispersão nos dados, fato
#que favorecerá a estimação de um modelo binomial negativo.

#Uma abordagem mais direta para a detecção da superdispersão pelo Teste de
#Cameron e Trivedi (1990) é por meio da utilização do algoritmo overdisp().
#Função overdisp do pacote overdisp
overdisp(x = corruption,
         dependent.position = 3,
         predictor.position = 4:6)

#Apenas para fins didáticos, caso considerássemos a estimação Poisson como a
#mais adequada, qual seria a quantidade média esperada de violações de trânsito
#para um país cujo corpo diplomático fosse composto por 23 membros, considerando
#o período anterior à vigência da lei e cujo índice de corrupção seja
#igual a 0.5?
predict(object = modelo_poisson, 
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),
        type = "response")

#Qual seria a quantidade média esperada de violações de trânsito para o mesmo
#país, porém agora considerando a vigência da lei?
predict(object = modelo_poisson, 
        newdata = data.frame(staff = 23, 
                             post = "yes", 
                             corruption = 0.5), 
        type = "response")


################################################################################
#              A DISTRIBUIÇÃO BINOMIAL NEGATIVA - PARTE CONCEITUAL             #
################################################################################

#Criando uma função da distribuição binomial negativa, com theta=2 e delta=2
#theta: parâmetro de forma da distribuição Poisson-Gama (binomial negativa)
#delta: parâmetro de taxa de decaimento da distribuição Poisson-Gama
bneg_theta2_delta2 <- function(m){
  theta <- 2
  delta <- 2
  ((delta ^ theta) * (m ^ (theta - 1)) * (exp(-m * delta))) / factorial(theta - 1)
}

#Criando uma função da distribuição binomial negativa, com theta=3 e delta=1
bneg_theta3_delta1 <- function(m){
  theta <- 3
  delta <- 1
  ((delta ^ theta) * (m ^ (theta - 1)) * (exp(-m * delta))) / factorial(theta - 1)
}

#Criando uma função da distribuição binomial negativa, com theta=3 e delta=0,5
bneg_theta3_delta05 <- function(m){
  theta <- 3
  delta <- 0.5
  ((delta ^ theta) * (m ^ (theta - 1)) * (exp(-m * delta))) / factorial(theta - 1)
}

#Plotagem das funções estabelecidas anteriormente
data.frame(m = 1:20) %>%
  ggplot(aes(x = m)) +
  stat_function(fun = bneg_theta2_delta2, 
                aes(color = "Theta igual a 2 e Delta igual a 2"),
                size = 1.5) +
  stat_function(fun = bneg_theta3_delta1, 
                aes(color = "Theta igual a 3 e Delta igual a 1"),
                size = 1.5) +
  stat_function(fun = bneg_theta3_delta05, 
                aes(color = "Theta igual a 3 e Delta igual a 0,5"),
                size = 1.5) +
  scale_color_viridis_d("Valores de " ~ theta ~ "e " ~ delta ~ "") +
  labs(y = "Probabilidades", x = "m") +
  theme_bw()


################################################################################
#                   ESTIMAÇÃO DO MODELO BINOMIAL NEGATIVO                      #
################################################################################
#Estimação do modelo binomial negativo pela função glm.nb do pacote MASS
#Modelo Binomial Negativo do Tipo 2 (NB2)
modelo_bneg <- glm.nb(formula = violations ~ staff + post + corruption,
                      data = corruption)

#Parâmetros do modelo_bneg
summary(modelo_bneg)

#Parâmetro de forma da distribuição binomial negativa
1 / modelo_bneg$theta #phi
modelo_bneg$theta

#Estatística z de Wald do parâmetro theta para verificação da
#significância estatística
modelo_bneg$theta / modelo_bneg$SE.theta  #maior que 1.96

#Extração do valor de Log-Likelihood (LL)
logLik(modelo_bneg)

#Parâmetros do modelo_bneg
summ(modelo_bneg, digits = 4, confint = T, ci.width = 0.95)
export_summs(modelo_bneg, scale = F, digits = 4)

#Comparando os modelos Poisson e Binomial Negativo
#modelo_poisson: linha 144 deste script!
export_summs(modelo_poisson, modelo_bneg, scale = F, digits = 4,
             model.names = c("POISSON","BNEG"))

data.frame(LL_Poisson = round(logLik(modelo_poisson), 1),
           LL_Bneg = round(logLik(modelo_bneg), 1)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

#Likelihoo-ratio test
lrtest(modelo_poisson, modelo_bneg)

#Gráfico para a comparação dos LL dos modelos Poisson e Binomial Negativo
my_plot <-
data.frame(Poisson = logLik(modelo_poisson),
           BNeg = logLik(modelo_bneg)) %>% 
  melt() %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_bar(aes(fill = factor(variable)), 
           stat = "identity",
           color = "black") +
  geom_text(aes(label = round(value, digits = 3)), 
            color = "black", 
            size = 4, 
            vjust = -0.5,
            angle = 90) +
  scale_fill_manual("Legenda:", values = c("#440154FF", "orange")) +
  coord_flip() +
  labs(x = "Estimação",
       y = "Log-Likelihood") +
  theme_cowplot()
my_plot

#Com JPEG
ggdraw() +
  draw_image("https://cdn.pixabay.com/photo/2016/08/21/18/48/emoticon-1610518_960_720.png",
             x = -0.12, y = 0.23, scale = .43) +
  draw_plot(my_plot)


#COMPARAÇÕES ENTRE AS PREVISÕES:
#Qual seria a quantidade média esperada de violações de trânsito para um país
#cujo corpo diplomático seja composto por 23 membros, considerando o período
#anterior à vigência da lei e cujo índice de corrupção seja igual 0.5?

#Modelo Poisson:
predict(object = modelo_poisson, #linha 144 deste script
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),
        type = "response")

#Modelo Binomial Negativo:
predict(object = modelo_bneg,
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),
        type = "response")


#Qual seria a quantidade média esperada de violações de trânsito para o mesmo
#país, porém agora considerando a vigência da lei?

#Modelo Poisson:
predict(object = modelo_poisson,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")

#Modelo Binomial Negativo:
predict(object = modelo_bneg,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")


#Adicionando os fitted values dos modelos estimados até o momento, para fins de 
#comparação:
corruption %>%
  mutate(fitted_poisson = modelo_poisson$fitted.values,
         fitted_bneg = modelo_bneg$fitted.values) %>% 
  dplyr::select(country, code, violations, fitted_poisson, 
                fitted_bneg) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 19)


#Fitted values dos modelos POISSON e BINOMIAL NEGATIVO, considerando,
#para fins didáticos, apenas a variável preditora 'staff':
corruption %>%
  ggplot() +
  geom_point(aes(x = staff, y = violations), alpha = 0.5, size = 2) +
  geom_smooth(aes(x = staff, y = modelo_poisson$fitted.values,
                  color = "POISSON"), se = F, size = 1.5) +
  geom_smooth(aes(x = staff, y = modelo_bneg$fitted.values,
                  color = "BNEG"), se = F, size = 1.5) + 
  scale_color_manual("Estimação:",
                     values = c("orange", "#440154FF")) +
  labs(x = "Number of Diplomats (staff)",
       y = "Unpaid Parking Violations (violations)") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


################################################################################
#      ESTIMAÇÕES MUITO PRÓXIMAS PARA POISSON E BNEG SEM SUPERDISPERSÃO!       #
################################################################################

#Para fins didáticos, vamos gerar novo dataset 'corruption2', com quantidades
#de violações de trânsito iguais, no máximo, a 3. Este procedimento poderá,
#eventualmente, eliminar o fenômeno da superdispersão nos dados da variável
#dependente e, consequentemente, tornar as estimações dos modelos POISSON e
#BINOMIAL NEGATIVO praticamente iguais.

#Gerando novo dataset 'corruption2' com violations <= 3
corruption2 <- corruption[which(corruption$violations <= 3),1:6]

#Histograma da variável dependente 'violations' no dataset 'corruption2'
ggplotly(
  corruption2 %>%
    ggplot(aes(x = violations,
               fill = ..count..)) +
    geom_histogram(bins = 4,
                   color = "black") +
    scale_fill_gradient("Contagem",
                        low = "#440154FF", 
                        high = "#FDE725FF") +
    labs(x = "Quantidade de violações de trânsito",
         y = "Frequência") +
    theme_bw()
)

#Diagnóstico preliminar para observação de eventual igualdade entre a média e
#a variância da variável dependente 'violations' no dataset 'corruption2'
corruption2 %>%
  summarise(Média = mean(violations),
            Variância = var(violations)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 30)

#Estimação do modelo_poisson2
modelo_poisson2 <- glm(formula = violations ~ staff + post + corruption,
                       data = corruption2,
                       family = "poisson")

#Parâmetros do modelo_poisson2
summary(modelo_poisson2)

#Teste de superdispersão no dataset 'corruption2'
overdisp(x = corruption2,
         dependent.position = 3,
         predictor.position = 4:6)

#Estimação do modelo_bneg2
modelo_bneg2 <- glm.nb(formula = violations ~ staff + post + corruption,
                       data = corruption2)

#Parâmetros do modelo_bneg2
summary(modelo_bneg2)

#Significância estatística do parâmetro de forma da distribuição
#binomial negativa para o modelo_bneg2
modelo_bneg2$theta / modelo_bneg2$SE.theta #menor que 1.96

#Comparando os parâmetros e os valores de LL de modelo_poisson2 e modelo_bneg2
export_summs(modelo_poisson2, modelo_bneg2, scale = F, digits = 4,
             model.names = c("POISSON2","BNEG2"))

data.frame(LL_Poisson2 = round(logLik(modelo_poisson2), 1),
           LL_Bneg2 = round(logLik(modelo_bneg2), 1)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

#Likelihoo-ratio test para a comparação entre modelo_poisson2 e modelo_bneg2
lrtest(modelo_poisson2, modelo_bneg2)


################################################################################
#        A DISTRIBUIÇÃO ZERO-INFLATED POISSON (ZIP) - PARTE CONCEITUAL         #
################################################################################

#LAMBERT, D. Zero-inflated Poisson regression, with an application to defects
#in manufacturing. Technometrics, v. 34, n. 1, p. 1-14, 1992.

#Exemplo de uma função da distribuição ZI Poisson, com lambda = 1 e plogit = 0,7
zip_lambda1_plogit07 <- function(m){
  lambda <- 1
  plogit <- 0.7
  ifelse(m == 0, 
         yes = (plogit) + ((1 - plogit) * exp(-lambda)),
         no = (1 - plogit) * ((exp(-lambda) * lambda ^ m) / factorial(m)))
}

#Comparando as distribuições Poisson, BNeg e ZIP
data.frame(m = 0:20) %>% 
  ggplot(aes(x = m)) +
  stat_function(fun = poisson_lambda1, size = 0.7, 
                aes(color = "Poisson - Lambda igual a 1")) +
  stat_function(fun = poisson_lambda4, size = 0.7, 
                aes(color = "Poisson - Lambda igual a 4")) +
  stat_function(fun = poisson_lambda10, size = 0.7, 
                aes(color = "Poisson - Lambda igual a 10")) +
  stat_function(fun = bneg_theta2_delta2, size = 0.7, 
                aes(color = "BNeg - Theta igual a 2 e Delta igual a 2")) +
  stat_function(fun = bneg_theta3_delta1, size = 0.7, 
                aes(color = "BNeg - Theta igual a 3 e Delta igual a 1")) +
  stat_function(fun = bneg_theta3_delta05, size = 0.7, 
                aes(color = "BNeg - Theta igual a 3 e Delta igual a 0,5")) +
  stat_function(fun = zip_lambda1_plogit07, size = 1.5, 
                aes(color = "ZIP - Lambda igual a 1 e plogit igual a 0,7")) +
  scale_color_viridis_d("Distribuição:") +
  labs(y = "Probabilidade", x = "m") +
  theme_bw()


################################################################################
#              ESTIMAÇÃO DO MODELO ZERO-INFLATED POISSON (ZIP)                 #
################################################################################
#VOLTANDO AO DATASET 'corruption'
#Estimação do modelo ZIP pela função zeroinfl do pacote pscl
modelo_zip <- zeroinfl(formula = violations ~ corruption + post + staff
                       | corruption,
                       data = corruption,
                       dist = "poisson")

#Parâmetros e LL do modelo_zip
summary(modelo_zip)
logLik(modelo_zip)


#Teste de Vuong:
#VUONG, Q. H. Likelihood ratio tests for model selection and non-nested
#hypotheses. Econometrica, v. 57, n. 2, p. 307-333, 1989.

vuong(m1 = modelo_poisson, #linha 144 deste script
      m2 = modelo_zip)

#Comparando os LL dos modelos Poisson e ZIP
data.frame(LL_Poisson = round(logLik(modelo_poisson), 1),
           LL_ZIP = round(logLik(modelo_zip), 1)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F,
                font_size = 30)

#Likelihoo-ratio test
lrtest(modelo_poisson, modelo_zip)

data.frame(Poisson = logLik(modelo_poisson),
           ZIP = logLik(modelo_zip),
           BNeg = logLik(modelo_bneg)) %>% 
  melt() %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_bar(aes(fill = factor(variable)), 
           stat = "identity",
           color = "black") +
  geom_text(aes(label = format(value, digts = 3)), 
            color = "black", 
            size = 4, 
            vjust = -0.5,
            angle = 90) +
  scale_fill_manual("Legenda:", values = c("#440154FF", "#453781FF", "orange")) +
  coord_flip() +
  labs(x = "Estimação",
       y = "Log-Likelihood") +
  theme_bw()


#COMPARAÇÕES ENTRE AS PREVISÕES:
#Supondo que considerássemos a estimação ZIP como a mais adequada, qual seria a 
#quantidade média esperada de violações de trânsito para um país cujo corpo 
#diplomático seja composto por 23 membros, considerando o período anterior à 
#vigência da lei e cujo índice de corrupção seja igual a 0.5?

#Modelo Poisson:
predict(object = modelo_poisson, #linha 144 deste script
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),
        type = "response")

#Modelo Binomial Negativo:
predict(object = modelo_bneg, #linha 275 deste script
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),
        type = "response")

#Modelo ZIP:
predict(object = modelo_zip,
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5), 
        type = "response")


#Qual seria a quantidade média esperada de violações de trânsito para o mesmo
#país ao se considerar o início da vigência da lei?

#Modelo Poisson:
predict(object = modelo_poisson,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")

#Modelo Binomial Negativo:
predict(object = modelo_bneg,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")

#Modelo ZIP:
predict(object = modelo_zip,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")


################################################################################
#   A DISTRIBUIÇÃO ZERO-INFLATED BINOMIAL NEGATIVA (ZINB) - PARTE CONCEITUAL   #
################################################################################

#Exemplo de uma função da distribuição ZI Binomial Negativa, com theta = 2,
#delta = 2, plogit = 0,7 e lambda_bneg = 2
zinb_theta2_delta2_plogit07_lambda2 <- function(m){
  theta <- 2
  delta <- 2
  plogit <- 0.7
  lambda_bneg <- 2
  ifelse(m == 0,
         yes = (plogit) + ((1 - plogit) * (((1) / (1 + 1/theta * lambda_bneg)) ^ theta)),
         no = (1 - plogit) * ((delta ^ theta) * (m ^ (theta - 1)) * 
                                (exp(-m * delta))) / factorial(theta - 1))
}

#Comparando as distribuições Poisson, BNeg, ZIP e ZINB
data.frame(m = 0:20) %>% 
  ggplot(aes(x = m)) +
  stat_function(fun = poisson_lambda1, size = 0.7, 
                aes(color = "Poisson - Lambda igual a 1")) +
  stat_function(fun = poisson_lambda4, size = 0.7, 
                aes(color = "Poisson - Lambda igual a 4")) +
  stat_function(fun = poisson_lambda10, size = 0.7, 
                aes(color = "Poisson - Lambda igual a 10")) +
  stat_function(fun = bneg_theta2_delta2, size = 0.7, 
                aes(color = "BNeg - Theta igual a 2 e Delta igual a 2")) +
  stat_function(fun = bneg_theta3_delta1, size = 0.7, 
                aes(color = "BNeg - Theta igual a 3 e Delta igual a 1")) +
  stat_function(fun = bneg_theta3_delta05, size = 0.7, 
                aes(color = "BNeg - Theta igual a 3 e Delta igual a 0,5")) +
  stat_function(fun = zip_lambda1_plogit07, size = 0.7, 
                aes(color = "ZI Poisson - Lambda igual a 1 e plogit igual a 0,7")) +
  stat_function(fun = zinb_theta2_delta2_plogit07_lambda2, size = 1.5, 
                aes(color = "ZINB - Theta igual a 2, Delta igual a 2 e plogit igual a 0,7")) +
  scale_color_viridis_d("Distribuição:") +
  labs(y = "Probabilidade", x = "m") +
  theme_bw()


################################################################################
#        ESTIMAÇÃO DO MODELO ZERO-INFLATED BINOMIAL NEGATIVO (ZINB)            #
################################################################################
#Estimação do modelo ZINB pela função zeroinfl do pacote pscl
modelo_zinb <- zeroinfl(formula = violations ~ corruption + post + staff
                        | corruption,
                        data = corruption,
                        dist = "negbin")

#Parâmetros e LL do modelo_zinb
summary(modelo_zinb)
logLik(modelo_zinb)
modelo_zinb$theta
1/modelo_zinb$theta #phi


#Teste de Vuong (1989)
vuong(m1 = modelo_bneg, #linha 275 deste script
      m2 = modelo_zinb)

#Comparando os LL dos modelos Bneg e ZINB
data.frame(LL_Bneg = round(logLik(modelo_bneg), 2),
           LL_ZINB = round(logLik(modelo_zinb), 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

#Likelihoo-ratio test
lrtest(modelo_bneg, modelo_zinb)

my_plot2 <-
data.frame(Poisson = logLik(modelo_poisson),
           ZIP = logLik(modelo_zip),
           Bneg = logLik(modelo_bneg),
           ZINB = logLik(modelo_zinb)) %>%
  melt() %>%
  ggplot(aes(x = variable, y = value)) +
  geom_bar(aes(fill = factor(variable)), 
           stat = "identity",
           color = "black") +
  geom_text(aes(label = format(value, digts = 3)),
            color = "black",
            size = 3.5,
            vjust = -0.5,
            angle = 90) +
  scale_fill_manual("Legenda:", values = c("#440154FF", "#453781FF",
                                           "orange", "#FDE725FF")) +
  coord_flip() +
  labs(x = "Estimação",
       y = "Log-Likelihood") +
  theme_cowplot()
my_plot2

#Com JPEG
ggdraw() +
  draw_image("https://i.pinimg.com/originals/4a/ac/99/4aac9978c444c55cd462fd92c8ac400e.png",
             x = -0.07, y = 0.244, scale = .43) +
  draw_plot(my_plot2)
beep("mario")


#COMPARAÇÕES ENTRE AS PREVISÕES:
#Supondo que considerássemos a estimação ZINB como a mais adequada, qual seria a 
#quantidade média esperada de violações de trânsito para um país cujo corpo 
#diplomático seja composto por 23 membros, considerando o período anterior à 
#vigência da lei e cujo índice de corrupção seja igual a 0.5?

#Modelo Poisson:
predict(object = modelo_poisson, #linha 144 deste script
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),
        type = "response")

#Modelo Binomial Negativo:
predict(object = modelo_bneg, #linha 275 deste script
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),
        type = "response")

#Modelo ZIP:
predict(object = modelo_zip, #linha 447 deste script
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5), 
        type = "response")

#Modelo ZINB:
predict(object = modelo_zinb,
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5), 
        type = "response")


#Qual seria a quantidade média esperada de violações de trânsito para o mesmo
#país, porém agora considerando a vigência da lei?

#Modelo Poisson:
predict(object = modelo_poisson,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")

#Modelo Binomial Negativo:
predict(object = modelo_bneg,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")

#Modelo ZIP:
predict(object = modelo_zip,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")

#Modelo ZINB:
predict(object = modelo_zinb,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5), 
        type = "response")


#Adicionando os fitted values dos modelos estimados para fins de comparação
corruption %>%
  mutate(fitted_poisson = modelo_poisson$fitted.values,
         fitted_bneg = modelo_bneg$fitted.values,
         fitted_zip = modelo_zip$fitted.values,
         fitted_zinb = modelo_zinb$fitted.values) %>% 
  dplyr::select(country, code, violations, fitted_poisson, 
                fitted_bneg, fitted_zip, fitted_zinb) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 13)


#Fitted values dos modelos POISSON, BNEG, ZIP e ZINB, considerando, para fins
#didáticos, a variável dependente 'violations' em função apenas da variável
#preditora 'staff'
ggplotly(
  corruption %>%
    ggplot() +
    geom_point(aes(x = staff, y = violations), alpha = 0.5, size = 2) +
    geom_smooth(aes(x = staff, y = modelo_poisson$fitted.values,
                    color = "POISSON"), se = F) +
    geom_smooth(aes(x = staff, y = modelo_bneg$fitted.values,
                    color = "BNEG"), se = F) +
    geom_smooth(aes(x = staff, y = modelo_zip$fitted.values,
                    color = "ZIP"), se = F) +
    geom_smooth(aes(x = staff, y = modelo_zinb$fitted.values,
                    color = "ZINB"), se = F) +
    scale_color_manual("Estimação:",
                       values = c("orange", "#440154FF", "#FDE725FF", "#453781FF")) +
    labs(x = "Number of Diplomats (staff)",
         y = "Unpaid Parking Violations (violations)") +
    theme(panel.background = element_rect("white"),
          panel.grid = element_line("grey95"),
          panel.border = element_rect(NA),
          legend.position = "bottom")
)
