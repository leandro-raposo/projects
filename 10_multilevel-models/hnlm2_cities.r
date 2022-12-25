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
load("base_default.RData")

#Verificando o balanceamento dos dados quanto à quantidade de município
base_default %>% 
  group_by(município) %>% 
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Modelo Multinível com Interceptos Aleatórios
modelo_default_random_intercepts <- glmmTMB(formula = default ~ idade + filhos
                                            + (1 | município),
                                            data = base_default,
                                            family = binomial,
                                            REML = TRUE)

#Parâmetros do modelo
summary(modelo_default_random_intercepts)

#Intervalos de confiança
confint(modelo_default_random_intercepts)

#LL
logLik(modelo_default_random_intercepts)

#Apresentando os interceptos aleatórios do modelo
ranef(modelo_default_random_intercepts)[["cond"]][["município"]] %>%
  rownames_to_column("País") %>%
  rename(v0j = 2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Para observarmos graficamente o comportamento dos valores de v0j
ranef(modelo_default_random_intercepts)[["cond"]][["município"]] %>% 
  rownames_to_column("Município") %>% 
  rename(v0j = 2) %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(Município) %>% 
  ggplot(aes(label = round(v0j, digits = 3), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(Município), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Município, y = 0), size = 3.1, color = "black") +
  coord_flip() +
  labs(x = "Município",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("darkorchid","orange")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")


#Curvas Sigmóides para os seis primeiros Municípios
ggplotly(
  base_default %>%
    mutate(fitted_hnlm2 = predict(object = modelo_default_random_intercepts,
                                  type = "response")) %>%
    filter(município %in% c(1:6)) %>%
    ggplot(aes(x = filhos, y = fitted_hnlm2)) +
    geom_point(alpha = 0.4 , size = 2, color = "bisque4") +
    geom_smooth(aes(group = município, color = município),
                method = "lm", formula = y ~ splines::bs(x), se = F) +
    scale_colour_viridis_d() +
    labs(x = "Quantidade de Filhos",
         y = "Probabilidade de Default",
         color = "Média Prevista por Município") +
    theme_bw()
)

#Curvas Sigmóides para toda a base de dados
ggplotly(
  base_default %>% 
    mutate(fitted_hnlm2 = predict(object = modelo_default_random_intercepts, 
                                  type = "response")) %>% 
    ggplot(aes(x = idade, y = fitted_hnlm2)) +
    geom_smooth(aes(group = município, color = município), 
                method = "lm", formula = y ~ splines::bs(x), se = F) +
    facet_wrap(~município)
)

#Plotagem tridimensional das probabilidades para os seis primeiros Municípios
base_exemplo2 <- base_default %>% 
  mutate(fitted_hnlm2 = predict(object = modelo_default_random_intercepts, 
                                type = "response")) %>% 
  filter(município %in% c(1:6))

scatter3d(base_exemplo2$idade, base_exemplo2$fitted_hnlm2, base_exemplo2$filhos, 
          groups=factor(base_exemplo2$município), 
          data = predicoes,
          fit = "smooth")


#Estabelecendo um modelo GLM análogo ao modelo multinível
glm_default <- glm(formula = default ~ idade + filhos,
                   data = base_default,
                   family = "binomial")

#Parâmetros do modelo glm_logit_default
summary(glm_default) 

#LR Test
lrtest(glm_default, modelo_default_random_intercepts)

#Comparação entre os LLs dois dois modelos
data.frame(GLM = logLik(glm_default),
           GLMM = logLik(modelo_default_random_intercepts)) %>%
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
base_default %>% 
  mutate(fitted_probs_hnlm2 = predict(object = modelo_default_random_intercepts, 
                                      type = "response"),
         fitted_probs_glm = glm_default$fitted.values) %>% 
  select(município, default, fitted_probs_hnlm2, fitted_probs_glm, 
         everything()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Matrizes de confusão

#GLMM
confusionMatrix(table(predict(modelo_default_random_intercepts,
                              type = "response") >= 0.5,
                      base_default$default == "sim")[2:1, 2:1])

#GLM
confusionMatrix(table(predict(glm_default,
                              type = "response") >= 0.5,
                      base_default$default == "sim")[2:1, 2:1])

#Curvas ROC

#GLMM
roc_default_hnlm2 <- roc(response = base_default$default,
                         predictor = predict(object = modelo_default_random_intercepts,
                                             type = "response"))

ggplotly(
  ggroc(roc_default_hnlm2, color = "darkorchid", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color="orange", 
                 size = 0.2)+
    labs(x = "1 - Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:", 
                       round(roc_default_hnlm2$auc, 3), 
                       "|",
                       "Coeficiente de Gini", 
                       round((roc_default_hnlm2$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

#GLM
roc_default_glm <- roc(response = base_default$default, 
                       predictor = glm_default$fitted.values)

ggplotly(
  ggroc(roc_default_glm, color = "darkorchid", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color="orange", 
                 size = 0.2)+
    labs(x = "1 - Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:", 
                       round(roc_default_glm$auc, 3), 
                       "|",
                       "Coeficiente de Gini", 
                       round((roc_default_glm$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

#Comparando as curvas ROC dos dois modelos estimados no mesmo gráfico
plot(roc_default_hnlm2, col = 1, lty = 2, main = "ROC Comparison")
plot(roc_default_glm, col = 4, lty = 3, add = TRUE)

roc.test(roc_default_glm,roc_default_hnlm2)
