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

load(file = "paises.RData")

#Estatísticas univariadas
summary(paises)

#Gráfico 3D com scatter
scatter3d(cpi ~ idade + horas,
          data = paises,
          surface = F,
          point.col = "#440154FF",
          axis.col = rep(x = "black",
                         times = 3))

##################################################################################
#                               ESTUDO DAS CORRELAÇÕES                           #
##################################################################################
#Visualizando a base de dados
paises %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

#A função correlation do pacote correlation faz com que seja estruturado um
#diagrama interessante que mostra a inter-relação entre as variáveis e a
#magnitude das correlações entre elas
#Requer instalação e carregamento dos pacotes see e ggraph para a plotagem
paises %>%
  correlation(method = "pearson") %>%
  plot()

#A função chart.Correlation() do pacote PerformanceAnalytics apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
chart.Correlation((paises[2:4]), histogram = TRUE)

##################################################################################
#     ESTIMANDO UM MODELO MÚLTIPLO COM AS VARIÁVEIS DA BASE DE DADOS paises      #
##################################################################################
#Estimando a Regressão Múltipla
modelo_paises <- lm(formula = cpi ~ . - pais,
                    data = paises)

#Parâmetros do modelo
summary(modelo_paises)
confint(modelo_paises, level = 0.95) # siginificância de 5%

#Outro modo de apresentar os outputs do modelo - função summ do pacote jtools
summ(modelo_paises, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_paises, scale = F, digits = 5)

#Salvando os fitted values na base de dados
paises$cpifit <- modelo_paises$fitted.values

#Gráfico 3D com scatter e fitted values
scatter3d(cpi ~ idade + horas,
          data = paises,
          surface = T, fit = "linear",
          point.col = "#440154FF",
          axis.col = rep(x = "black",
                         times = 3))
