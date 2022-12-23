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
#                             REGRESSÃO NÃO LINEAR MÚLTIPLA                      #
##################################################################################

#Carregando a base de dados
load(file = "empresas.RData")

#Estatísticas univariadas
summary(empresas)

##################################################################################
#                               ESTUDO DAS CORRELAÇÕES                           #
##################################################################################
#A função correlation do pacote correlation faz com que seja estruturado um
#diagrama interessante que mostra a inter-relação entre as variáveis e a
#magnitude das correlações entre elas
#Requer instalação e carregamento dos pacotes see e ggraph para a plotagem
empresas %>%
  correlation(method = "pearson") %>%
  plot()

#A função chart.Correlation do pacote PerformanceAnalytics apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
chart.Correlation((empresas[2:6]), histogram = TRUE)

#A função corr_plot do pacote metan também apresenta as distribuições
#das variáveis, scatters, valores das correlações e suas respectivas
#significâncias
empresas %>%
  corr_plot(retorno, disclosure, endividamento, ativos, liquidez,
            shape.point = 21,
            col.point = "black",
            fill.point = "#FDE725FF",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 4,
            minsize = 2,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "#440154FF",
            upper = "corr",
            lower = "scatter",
            diag.type = "density",
            col.diag = "#440154FF",
            pan.spacing = 0,
            lab.position = "bl")

##################################################################################
#     ESTIMANDO UM MODELO MÚLTIPLO COM AS VARIÁVEIS DA BASE DE DADOS empresas    #
##################################################################################
#Visualizando a base de dados
empresas %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

#Estimando a Regressão Múltipla
modelo_empresas <- lm(formula = retorno ~ . - empresa,
                      data = empresas)

#Parâmetros do modelo
summary(modelo_empresas)

#Note que o parâmetro da variável 'endividamento' não é estatisticamente
#significante ao nível de significância de 5% (nível de confiança de 95%)

##################################################################################
#                                 PROCEDIMENTO STEPWISE                          #
##################################################################################
#Aplicando o procedimento Stepwise, temos o seguinte código:
step_empresas <- step(modelo_empresas, k = 3.841459)

#De onde vem o argumento k = 3.841459?
qchisq(p = 0.05, df = 1, lower.tail = F)
round(pchisq(3.841459, df = 1, lower.tail = F),7)

summary(step_empresas)
#Este procedimento no R removeu a variável 'endividamento'. Note que a variável
#'disclosure' também acabou sendo excluída após o procedimento Stepwise, nesta
#forma funcional linear!

export_summs(step_empresas, scale = F, digits = 5)

#Parâmetros reais do modelo com procedimento Stepwise
confint(step_empresas, level = 0.95) # siginificância 5%
plot_summs(step_empresas, colors = "#440154FF") #função plot_summs do pacote ggstance

#Parâmetros padronizados
plot_summs(step_empresas, scale = TRUE, colors = "#440154FF")

#Adicionando a caracterização da distribição normal no IC de cada parâmetro beta
plot_summs(step_empresas, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#440154FF")

#Comparando os ICs dos betas dos modelos sem e com procedimento Stepwise
plot_summs(modelo_empresas, step_empresas, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#FDE725FF", "#440154FF"))

##################################################################################
#          TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE          #
#                               SHAPIRO-FRANCIA                                  #
##################################################################################
#Shapiro-Francia: n > 30
sf.test(step_empresas$residuals) #função sf.test do pacote nortest

#Plotando os resíduos do modelo step_empresas
empresas %>%
  mutate(residuos = step_empresas$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequência") + 
  theme_bw()

#Acrescentando uma curva normal teórica para comparação entre as distribuições
empresas %>%
  mutate(residuos = step_empresas$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_empresas$residuals),
                            sd = sd(step_empresas$residuals)),
                size = 2, color = "grey30") +
    scale_color_manual(values = "grey50") +
    labs(x = "Resíduos",
         y = "Frequência") +
  theme_bw()

##################################################################################
#                            TRANSFORMAÇÃO DE BOX-COX                            #
##################################################################################
#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(empresas$retorno) #função powerTransform do pacote car#
lambda_BC

#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
empresas$bcretorno <- (((empresas$retorno ^ lambda_BC$lambda) - 1) / 
                            lambda_BC$lambda)

#Visualizando a nova variável na base de dados
empresas %>%
  select(empresa, retorno, bcretorno, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

#Estimando um novo modelo múltiplo com variável dependente transformada por Box-Cox
modelo_bc <- lm(formula = bcretorno ~ . -empresa -retorno, 
                data = empresas)

#Parâmetros do modelo
summary(modelo_bc)

#Aplicando o procedimento Stepwise
step_modelo_bc <- step(modelo_bc, k = 3.841459)

summary(step_modelo_bc)
#Note que a variável 'disclosure' acaba voltando ao modelo na forma funcional não linear!

#Verificando a normalidade dos resíduos do modelo step_modelo_bc
sf.test(step_modelo_bc$residuals) #função sf.test do pacote nortest

#Plotando os novos resíduos do step_modelo_bc
empresas %>%
    mutate(residuos = step_modelo_bc$residuals) %>%
    ggplot(aes(x = residuos)) +
    geom_histogram(aes(y = ..density..),
                   color = "white",
                   fill = "#287D8EFF",
                   bins = 30,
                   alpha = 0.6) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(step_modelo_bc$residuals),
                              sd = sd(step_modelo_bc$residuals)),
                  size = 2, color = "grey30") +
    scale_color_manual(values = "grey50") +
    labs(x = "Resíduos",
         y = "Frequência") +
    theme_bw()

#Resumo dos dois modelos obtidos pelo procedimento Stepwise (linear e com Box-Cox)
#Função export_summs do pacote jtools
export_summs(step_empresas, step_modelo_bc, scale = F, digits = 6)

#Parâmetros reais do modelo com procedimento Stepwise e Box-Cox
confint(step_modelo_bc, level = 0.95) # siginificância 5%
plot_summs(step_modelo_bc, colors = "#287D8EFF") #função plot_summs do pacote ggstance

#Parâmetros padronizados
plot_summs(step_modelo_bc, scale = TRUE, colors = "#287D8EFF")

#Adicionando caracterização da distribição normal no IC de cada parâmetro beta
plot_summs(step_modelo_bc, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#287D8EFF")

#Comparando os ICs do betas dos modelos sem e com Transformação de Box-Cox
plot_summs(step_empresas, step_modelo_bc, scale = T, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#440154FF", "#287D8EFF"))

#Fazendo predições com o step_modelo_bc, e.g.: qual é o valor do retorno, em
#média, para disclosure igual a 50, liquidez igual a 14 e ativo igual a 4000,
#ceteris paribus?
predict(object = step_modelo_bc, 
        data.frame(disclosure = 50, 
                   liquidez = 14, 
                   ativos = 4000),
        interval = "confidence", level = 0.95)

#Não podemos nos esquecer de fazer o cálculo para a obtenção do fitted
#value de Y (retorno)
(((3.702015 * -0.02256414) + 1)) ^ (1 / -0.02256414)

#Salvando os fitted values dos modelos step_empresas e step_modelo_bc no
#dataset empresas
empresas$yhat_step_empresas <- step_empresas$fitted.values
empresas$yhat_step_modelo_bc <- (((step_modelo_bc$fitted.values*(lambda_BC$lambda))+
                                    1))^(1/(lambda_BC$lambda))

#Visualizando os dois fitted values no dataset
#modelos step_empresas e step_modelo_bc
empresas %>%
  select(empresa, retorno, yhat_step_empresas, yhat_step_modelo_bc) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

#Ajustes dos modelos: valores previstos (fitted values) X valores reais
empresas %>%
  ggplot() +
  geom_smooth(aes(x = retorno, y = yhat_step_empresas, color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = retorno, y = yhat_step_empresas),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = retorno, y = yhat_step_modelo_bc, color = "Stepwise Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = retorno, y = yhat_step_modelo_bc),
             color = "#287D8EFF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = retorno, y = retorno), method = "lm", 
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#287D8EFF", "#440154FF")) +
  labs(x = "Retorno", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")
