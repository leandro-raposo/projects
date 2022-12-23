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

##################################################################################
#            DIAGNÓSTICO DE MULTICOLINEARIDADE EM MODELOS DE REGRESSÃO           #
##################################################################################

#Carregando a base de dados
load("salarios.RData")

#Estatísticas univariadas
summary(salarios)

##CORRELAÇÃO PERFEITA:
cor(salarios$rh1, salarios$econometria1)

salarios %>% select(2:4) %>% 
  correlation(method = "pearson") %>%
  plot()

modelo1 <- lm(formula = salario ~ rh1 + econometria1,
              data = salarios)

summary(modelo1)

##CORRELAÇÃO BAIXA:
cor(salarios$rh3, salarios$econometria3)

salarios %>% select(2,7,8) %>% 
  correlation(method = "pearson") %>%
  plot()

modelo3 <- lm(formula = salario ~ rh3 + econometria3,
              data = salarios)

summary(modelo3)

#Diagnóstico de multicolinearidade (Variance Inflation Factor e Tolerance)
ols_vif_tol(modelo3)
#função ols_vif_tol do pacote olsrr

##CORRELAÇÃO MUITO ALTA, PORÉM NÃO PERFEITA:
cor(salarios$rh2, salarios$econometria2)

salarios %>% select(2,5,6) %>% 
  correlation(method = "pearson") %>%
  plot()

modelo2 <- lm(formula = salario ~ rh2 + econometria2,
              data = salarios)

summary(modelo2)
ols_vif_tol(modelo2)
