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
#                    REGRESSÃO NÃO LINEAR MÚLTIPLA COM DUMMIES                   #
##################################################################################

#Carregando a base de dados
load(file = "planosaude.RData")

#Estatísticas univariadas
glimpse(planosaude)
summary(planosaude)

#Categorias da variável 'plano'
levels(factor(planosaude$plano))

#Tabela de frequências absolutas da variável 'plano'
table(planosaude$plano)

# correlacoes
chart.Correlation((planosaude[2:5]), histogram = TRUE)

# N-1 DUMMIES                           #
planosaude_dummies <- dummy_columns(.data = planosaude,
                                    select_columns = "plano",
                                    remove_selected_columns = T,
                                    remove_most_frequent_dummy = T)

#Visualizando a base de dados dummizada
planosaude_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 19)

##################################################################################
#                       ESTIMAÇÃO DA REGRESSÃO LINEAR MÚLTIPLA                   #
##################################################################################
#Modelagem com todas as variáveis
modelo_planosaude <- lm(despmed ~ . - id, planosaude_dummies)

#Parâmetros do modelo_planosaude
summary(modelo_planosaude)

##################################################################################
#                               PROCEDIMENTO STEPWISE                            #
##################################################################################

step_planosaude <- step(modelo_planosaude, k = 3.841459)

summary(step_planosaude)

##################################################################################
#            TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE        #
##################################################################################

#Teste de Shapiro-Francia
sf.test(step_planosaude$residuals) #função sf.test do pacote nortest

#Plotando os resíduos do modelo step_planosaude 
planosaude %>%
    mutate(residuos = step_planosaude$residuals) %>%
    ggplot(aes(x = residuos)) +
    geom_histogram(color = "white", 
                   fill = "#55C667FF", 
                   bins = 15,
                   alpha = 0.6) +
    labs(x = "Resíduos",
         y = "Frequências") + 
    theme_bw()

#Acrescentando uma curva normal teórica para comparação entre as distribuições
planosaude %>%
  mutate(residuos = step_planosaude$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_planosaude$residuals),
                            sd = sd(step_planosaude$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

#Kernel density estimation (KDE) - forma não-paramétrica para estimar a
#função densidade de probabilidade de uma variável aleatória
planosaude_dummies %>%
  ggplot() +
  geom_density(aes(x = step_planosaude$residuals), fill = "#55C667FF") +
  labs(x = "Resíduos do Modelo Stepwise",
       y = "Densidade") +
  theme_bw()

##################################################################################
#                        DIAGNÓSTICO DE HETEROCEDASTICIDADE                      #
##################################################################################

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(step_planosaude)
#função ols_test_breusch_pagan do pacote olsrr
#Presença de heterocedasticidade -> omissão de variável(is) explicativa(s) relevante(s)

#H0 do teste: ausência de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
#variáveis explicativas, o que indica omissão de variável relevante!

#Adicionando fitted values e resíduos do modelo 'step_planosaude'
#no dataset 'planosaude_dummies'
planosaude_dummies$fitted_step <- step_planosaude$fitted.values
planosaude_dummies$residuos_step <- step_planosaude$residuals

#Gráfico que relaciona resíduos e fitted values do modelo 'step_planosaude'
planosaude_dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step, y = residuos_step),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  theme_bw()

##################################################################################
#                              TRANSFORMAÇÃO DE BOX-COX                          #
##################################################################################
#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(planosaude$despmed)
lambda_BC

#Inserindo o lambda de Box-Cox na nova base de dados para a estimação de um
#novo modelo
planosaude_dummies$bcdespmed <- (((planosaude$despmed ^ lambda_BC$lambda) - 1) / 
                                      lambda_BC$lambda)

#Visualizando a nova variável na base de dados
planosaude_dummies %>%
  select(id, despmed, bcdespmed, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

#Estimando um novo modelo múltiplo com dummies
modelo_bc_planosaude <- lm(formula = bcdespmed ~ . -id -despmed -fitted_step
                           -residuos_step, 
                           data = planosaude_dummies)

#Parâmetros do modelo
summary(modelo_bc_planosaude)

#Aplicando o procedimento Stepwise
step_bc_planosaude <- step(modelo_bc_planosaude, k = 3.841459)

summary(step_bc_planosaude)

#Verificando a normalidade dos resíduos do modelo step_bc_planosaude
#Teste de Shapiro-Francia
sf.test(step_bc_planosaude$residuals) #função sf.test do pacote nortest

#Plotando os novos resíduos do modelo step_bc_planosaude com curva normal teórica
planosaude_dummies %>%
  mutate(residuos = step_bc_planosaude$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_bc_planosaude$residuals),
                            sd = sd(step_bc_planosaude$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

#Kernel density estimation (KDE)
planosaude_dummies %>%
  ggplot() +
  geom_density(aes(x = step_bc_planosaude$residuals), fill = "#440154FF") +
  labs(x = "Resíduos do Modelo Stepwise com Transformação de Box-Cox",
       y = "Densidade") +
  theme_bw()

#Diagnóstico de Heterocedasticidade para o Modelo Stepwise com Box-Cox
ols_test_breusch_pagan(step_bc_planosaude)

#Adicionando fitted values e resíduos do modelo 'step_bc_planosaude'
#no dataset 'planosaude_dummies'
planosaude_dummies$fitted_step_novo <- step_bc_planosaude$fitted.values
planosaude_dummies$residuos_step_novo <- step_bc_planosaude$residuals

#Gráfico que relaciona resíduos e fitted values do modelo 'step_bc_planosaude'
planosaude_dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step_novo, y = residuos_step_novo),
             color = "#440154FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise com Transformação de Box-Cox",
       y = "Resíduos do Modelo Stepwise com Transformação de Box-Cox") +
  theme_bw()
