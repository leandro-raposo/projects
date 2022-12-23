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

#Carregando a base de dados
load(file = "bebes.RData")

#Estatísticas univariadas
summary(bebes)

#Gráfico de dispersão
ggplotly(
  bebes %>% 
    ggplot() +
    geom_point(aes(x = idade, y = comprimento),
               color = "grey20", alpha = 0.6, size = 2) +
    labs(x = "Idade em semanas",
         y = "Comprimento em cm") +
    theme_bw()
)

#Gráfico de dispersão com ajustes (fits) linear e não linear
ggplotly(
  bebes %>% 
    ggplot() +
    geom_point(aes(x = idade, y = comprimento),
               color = "grey20", alpha = 0.6, size = 2) +
    geom_smooth(aes(x = idade, y = comprimento),
                method = "lm", color = "#FDE725FF", se = F) +
    geom_smooth(aes(x = idade, y = comprimento),
                color = "#440154FF", se = F) +
    labs(x = "Idade em semanas",
         y = "Comprimento em cm") +
    theme_bw()
)

#Estimação do modelo OLS linear
modelo_linear <- lm(formula = comprimento ~ idade,
                    data = bebes)

summary(modelo_linear)

##################################################################################
#          TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE          #
#                               SHAPIRO-FRANCIA                                  #
##################################################################################
#Shapiro-Wilk: n <= 30
## shapiro.test(modelo_linear$residuals)

#Shapiro-Francia: n > 30
sf.test(modelo_linear$residuals) #função sf.test do pacote nortest

#Histograma dos resíduos do modelo OLS linear
bebes %>%
  mutate(residuos = modelo_linear$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_linear$residuals),
                            sd = sd(modelo_linear$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

##################################################################################
#                             TRANSFORMAÇÃO DE BOX-COX                           #
##################################################################################
#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(bebes$comprimento) #função powerTransform do pacote car#
lambda_BC

#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
bebes$bc_comprimento <- (((bebes$comprimento ^ lambda_BC$lambda) - 1) / 
                            lambda_BC$lambda)

#Estimando um novo modelo OLS com variável dependente transformada por Box-Cox
modelo_bc <- lm(formula = bc_comprimento ~ idade,
                data = bebes)

#Parâmetros do modelo
summary(modelo_bc)

#Comparando os parâmetros do modelo_linear com os do modelo_bc
#CUIDADO!!! OS PARÂMETROS NÃO SÃO DIRETAMENTE COMPARÁVEIS!
export_summs(modelo_linear, modelo_bc, scale = F, digits = 4)

#Repare que há um salto na qualidade do ajuste para o modelo não linear (R²)
data.frame("R²OLS" = round(summary(modelo_linear)$r.squared, 4),
           "R²BoxCox" = round(summary(modelo_bc)$r.squared, 4)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

#Teste de Shapiro-Francia para os resíduos do modelo_bc
sf.test(modelo_bc$residuals) #função sf.test do pacote nortest

#Histograma dos resíduos do modelo_bc
bebes %>%
  mutate(residuos = modelo_bc$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "gray90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_bc$residuals),
                            sd = sd(modelo_bc$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

#Fazendo predições com os modelos OLS linear e Box-Cox
#qual é o comprimento esperado de um bebê com 52 semanas de vida?
#Modelo OLS Linear:
predict(object = modelo_linear,
        data.frame(idade = 52),
        interval = "confidence", level = 0.95)

#Modelo Não Linear (Box-Cox):
predict(object = modelo_bc,
        data.frame(idade = 52),
        interval = "confidence", level = 0.95)
#Não podemos nos esquecer de fazer o cálculo para a obtenção do fitted
#value de Y (variável 'comprimento')
(((54251.12 * 2.659051) + 1)) ^ (1 / 2.659051)

#Salvando os fitted values dos dois modelos (modelo_linear e modelo_bc) no
#dataset 'bebes'
bebes$yhat_linear <- modelo_linear$fitted.values
bebes$yhat_modelo_bc <- (((modelo_bc$fitted.values*(lambda_BC$lambda))+
                                    1))^(1/(lambda_BC$lambda))

#Visualizando os fitted values dos dois modelos no dataset
bebes %>%
  select(idade, comprimento, yhat_linear, yhat_modelo_bc) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

#Ajustes dos modelos: valores previstos (fitted values) X valores reais
bebes %>%
  ggplot() +
  geom_smooth(aes(x = comprimento, y = yhat_linear, color = "OLS Linear"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = comprimento, y = yhat_linear),
             color = "#FDE725FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = comprimento, y = yhat_modelo_bc, color = "Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = comprimento, y = yhat_modelo_bc),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = comprimento, y = comprimento), method = "lm", 
              color = "gray30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#440154FF", "#FDE725FF")) +
  labs(x = "Comprimento", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
      panel.grid = element_line("grey95"),
      panel.border = element_rect(NA),
      legend.position = "bottom")


