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
#           DIAGNÓSTICO DE HETEROCEDASTICIDADE EM MODELOS DE REGRESSÃO           #
##################################################################################

#Carregando a base de dados
load(file = "saeb_rend.RData")

#Estatísticas univariadas
summary(saeb_rend)

#Tabela de frequências absolutas das variáveis 'uf' e rede'
table(saeb_rend$uf)
table(saeb_rend$rede)

#Plotando saeb em função de rendimento, com linear fit
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb)) +
    geom_point(size = 1, color = "#FDE725FF") +
    geom_smooth(method = "lm", 
                color = "grey40", se = F) +
    xlab("rendimento") +
    ylab("saeb") +
    theme_classic()
)

#Plotando saeb em função de rendimento, com destaque para rede escolar 
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = rede, shape = rede)) +
    geom_point(size = 1) +
    xlab("rendimento") +
    ylab("saeb") +
    scale_colour_viridis_d() +
    theme_classic()
)

#Plotando saeb em função de rendimento, com destaque para rede escolar e linear fits
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = rede, shape = rede)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = F) +
    xlab("rendimento") +
    ylab("saeb") +
    scale_colour_viridis_d() +
    theme_classic()
)

##################################################################################
#                       ESTIMAÇÃO DO MODELO DE REGRESSÃO E                       #
#                       DIAGNÓSTICO DE HETEROCEDASTICIDADE                       #                                                            
##################################################################################
#Estimação do modelo
modelosaeb <- lm(formula = saeb ~ rendimento,
                 data = saeb_rend)

summary(modelosaeb)

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(modelosaeb)
#função ols_test_breusch_pagan do pacote olsrr
#Presença de heterocedasticidade -> omissão de variável(is) explicativa(s) relevante(s)

#H0 do teste: ausência de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
#variáveis explicativas, o que indica omissão de variável relevante!

#################################################################################
#              PROCEDIMENTO N-1 DUMMIES PARA UNIDADES FEDERATIVAS               #
#################################################################################

saeb_rend_dummies_uf <- dummy_columns(.data = saeb_rend,
                                      select_columns = "uf",
                                      remove_selected_columns = T,
                                      remove_most_frequent_dummy = T)

##################################################################################
#             ESTIMAÇÃO DO MODELO DE REGRESSÃO MÚLTIPLA COM DUMMIES E            #
#                       DIAGNÓSTICO DE HETEROCEDASTICIDADE                       #
##################################################################################
#Modelo considerando as UF's 
modelosaeb_dummies_uf <- lm(formula = saeb ~ . -municipio -codigo -escola -rede,
                            data = saeb_rend_dummies_uf)

summary(modelosaeb_dummies_uf)

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(modelosaeb_dummies_uf)

#Plotando saeb em função de rendimento, com destaque para UFs e linear fits
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = uf, shape = uf)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = F) +
    xlab("rendimento") +
    ylab("saeb") +
    scale_colour_viridis_d() +
    theme_classic()
)
