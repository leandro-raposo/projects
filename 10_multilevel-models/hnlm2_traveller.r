#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","glmmTMB","lmtest",
             "caret","e1071","pROC","car","rgl","reshape2")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Carregamento da base de dados
load("base_turismo.RData")

#Visualizando a base de dados
base_turismo %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Estatísticas descritivas univariadas e tabelas de frequências
summary(base_turismo)

#Verificando o balanceamento dos dados quanto à quantidade de anos
base_turismo %>% 
  group_by(país) %>% 
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Modelo Multinível com Interceptos Aleatórios
modelo_turismo_random_intercepts <- glmmTMB(formula = turismo ~ idade + filhos
                                            + (1 | país),
                                            data = base_turismo,
                                            family = binomial,
                                            REML = TRUE)

#Parâmetros do modelo
summary(modelo_turismo_random_intercepts)

#Intervalos de confiança
confint(modelo_turismo_random_intercepts)

#LL
logLik(modelo_turismo_random_intercepts)

#Apresentando os interceptos aleatórios do modelo
ranef(modelo_turismo_random_intercepts)[["cond"]][["país"]] %>% 
  rownames_to_column("País") %>% 
  rename(v0j = 2) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Para observarmos graficamente o comportamento dos valores de v0j
ranef(modelo_turismo_random_intercepts)[["cond"]][["país"]] %>% 
  rownames_to_column("País") %>% 
  rename(v0j = 2) %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(País) %>% 
  ggplot(aes(label = round(v0j, digits = 3), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(País), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = País, y = 0), size = 3.1, color = "black") +
  coord_flip() +
  labs(x = "País",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("darkorchid","orange")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

#Curvas Sigmóides para países específicos (França, EUA, Japão, África do Sul e 
#Venezuela)
base_turismo %>%
  mutate(fitted_probs_hnlm2 = predict(object = modelo_turismo_random_intercepts, 
                                      type = "response")) %>%
  filter(país %in% c("França", "Estados Unidos", "Japão",
                     "África do Sul", "Venezuela")) %>%
  ggplot(aes(x = filhos, y = fitted_probs_hnlm2)) +
  geom_point(alpha = 0.4, size = 4, color = "bisque4") +
  geom_smooth(aes(group = país, color = país), 
              method = "lm", formula = y ~ splines::bs(x), se = F, size = 2) +
  scale_colour_viridis_d() +
  labs(x = "Quantidade de Filhos",
       y = "Probabilidade de Viajar",
       color = "Sigmóide") +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "right")

#Curvas Sigmóides para toda a base de dados
base_turismo %>% 
  mutate(fitted_probs_hnlm2 = predict(object = modelo_turismo_random_intercepts, 
                                      type = "response")) %>% 
  ggplot(aes(x = filhos, y = fitted_probs_hnlm2)) +
  geom_smooth(aes(group = país, color = país), 
              method = "lm", formula = y ~ splines::bs(x), se = F) +
  scale_colour_viridis_d() +
  facet_wrap(~país) +
  labs(y = "Fitted Probs",
       x = "Quantidade de Filhos") +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

#Plotagem tridimensional das probabilidades para os seis primeiros Municípios
base_exemplo1 <- base_turismo %>%
  mutate(fitted_hnlm2 = predict(object = modelo_turismo_random_intercepts, 
                                type = "response")) %>%
  mutate(país = as.numeric(país)) %>%
  filter(país %in% c(1:6))

scatter3d(base_exemplo1$idade, base_exemplo1$fitted_hnlm2, base_exemplo1$filhos, 
          groups=factor(base_exemplo1$país), 
          data = predicoes,
          fit = "smooth")

#Efetuando predições
predict(object = modelo_turismo_random_intercepts,
        type = "response",
        newdata = data.frame(idade = 50,
                             filhos = 1, 
                             país = "Brasil"))


#Estabelecendo um modelo GLM análogo ao modelo multinível
glm_turismo <- glm(formula = turismo ~ idade + filhos,
                   data = base_turismo,
                   family = "binomial")

#Parâmetros do modelo glm_turismo
summary(glm_turismo) 

#LR Test
lrtest(glm_turismo, modelo_turismo_random_intercepts)

#Comparação entre os LLs dois dois modelos
data.frame(GLM = logLik(glm_turismo),
           GLMM = logLik(modelo_turismo_random_intercepts)) %>%
  rename(`GLM` = 1,
         `GLMM` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("darkorchid","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Comparando os fitted values dos dois modelos
base_turismo %>% 
  mutate(fitted_probs_hnlm2 = predict(object = modelo_turismo_random_intercepts, 
                                      type = "response"),
         fitted_probs_glm = glm_turismo$fitted.values) %>% 
  select(país, turismo, fitted_probs_hnlm2, fitted_probs_glm, 
         everything()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Matrizes de confusão

#GLMM
confusionMatrix(table(predict(modelo_turismo_random_intercepts,
                              type = "response") >= 0.5,
                      base_turismo$turismo == "sim")[2:1, 2:1])

#GLM
confusionMatrix(table(predict(glm_turismo,
                              type = "response") >= 0.5,
                      base_turismo$turismo == "sim")[2:1, 2:1])

#Curvas ROC

#GLMM
roc_turismo_hnlm2 <- roc(response = base_turismo$turismo,
                         predictor = predict(object = modelo_turismo_random_intercepts,
                                             type = "response"))

ggplotly(
  ggroc(roc_turismo_hnlm2, color = "darkorchid", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color="orange", 
                 size = 0.2)+
    labs(x = "1 - Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:", 
                       round(roc_turismo_hnlm2$auc, 3), 
                       "|",
                       "Coeficiente de Gini", 
                       round((roc_turismo_hnlm2$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

#GLM
roc_turismo_glm <- roc(response = base_turismo$turismo, 
                       predictor = glm_turismo$fitted.values)

ggplotly(
  ggroc(roc_turismo_glm, color = "darkorchid", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color="orange", 
                 size = 0.2)+
    labs(x = "1 - Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:", 
                       round(roc_turismo_glm$auc, 3),
                       "|",
                       "Coeficiente de Gini", 
                       round((roc_turismo_glm$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

#Comparando as curvas ROC dos dois modelos estimados no mesmo gráfico
plot(roc_turismo_hnlm2, col = 1, lty = 2, main = "ROC Comparison")
plot(roc_turismo_glm, col = 4, lty = 3, add = TRUE)

roc.test(roc_turismo_glm,roc_turismo_hnlm2)


