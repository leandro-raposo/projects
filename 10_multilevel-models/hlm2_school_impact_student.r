#Pacotes utilizados
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra","rgl","car",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools")

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
load(file = "estudante_escola.RData")

#Visualização da base de dados
estudante_escola %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Estatísticas descritivas
summary(estudante_escola)

#Estudo sobre o desbalanceamento dos dados
estudante_escola %>% 
  group_by(escola) %>% 
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Desempenho médio dos estudantes por escola
estudante_escola %>%
  group_by(escola) %>%
  summarise(`desempenho médio` = mean(desempenho, na.rm = T)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

#Exploração visual do desempenho médio
estudante_escola %>%
  group_by(escola) %>%
  mutate(desempenho_medio = mean(desempenho, na.rm = TRUE)) %>% 
  ggplot() +
  geom_point(aes(x = escola, y = desempenho),color = "orange", alpha = 0.5, size = 2) +
  geom_line(aes(x = escola, y = desempenho_medio, 
                group = 1, color = "Desempenho Escolar Médio"), size = 1.5) +
  scale_colour_viridis_d() +
  labs(x = "Escola",
       y = "Desempenho Escolar") +
  theme(legend.title = element_blank(),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey"),
        panel.background = element_rect("white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (desempenho), com histograma
ggplotly(
  ggplot(estudante_escola, aes(x = desempenho)) +
    geom_density(aes(x = desempenho), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                   bins = 30) +
    theme_classic()
)

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (desempenho) por escola
ggplotly(
  ggplot(estudante_escola, aes(x = desempenho)) +
    geom_density(aes(color = escola, fill = escola), 
                 position = "identity", alpha = 0.3) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_classic()
)

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (desempenho), com histograma e por escola separadamente
#(função facet_wrap)
estudante_escola %>% 
  group_by(escola) %>% 
  mutate(linhas = 1:n()) %>% 
  mutate(x = unlist(density(desempenho, n = max(linhas))["x"]),
         y = unlist(density(desempenho, n = max(linhas))["y"])) %>%
  ggplot() +
  geom_area(aes(x = x, y = y, group = escola, fill = escola), color = "black", alpha = 0.3) +
  geom_histogram(aes(x = desempenho, y = ..density.., fill = escola), 
                 color = "black", position = 'identity', alpha = 0.1) +
  facet_wrap(~ escola) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw()

#Gráfico de desempenho x horas (OLS)
ggplotly(
  estudante_escola %>%
    ggplot(aes(x = horas, y = desempenho)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    scale_colour_viridis_d() +
    labs(x = "Quantidade Semanal de Horas de Estudo do Aluno",
         y = "Desempenho Escolar") +
    theme_bw()
)

#Gráfico de desempenho x horas por escola (visualização do contexto)
#NOTE QUE A PERSPECTIVA MULTINÍVEL NATURALMENTE CONSIDERA O COMPORTAMENTO
#HETEROCEDÁSTICO NOS DADOS!
ggplotly(
  estudante_escola %>%
    ggplot(aes(x = horas, y = desempenho, color = escola)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    guides(color = F) +
    scale_colour_viridis_d() +
    labs(x = "Quantidade Semanal de Horas de Estudo do Aluno",
         y = "Desempenho Escolar") +
    theme_bw()
)

#O gráfico a seguir apresenta uma plotagem sob a perspectiva de um modelo
#com equação única (ex.: OLS)
base_exemplo <- estudante_escola %>%
  filter(escola %in% c("1","2","3","4","5","6")) %>%
  mutate(escola = as.numeric(escola))

scatter3d(desempenho ~ horas + escola, #função scatter3d do pacote car
          data = base_exemplo,
          fit = "linear")

#Agora plotamos o mesmo gráfico, porém de forma tridimensional,
#considerando modelos distintos para as diferentes escolas. Plotamos
#apenas as 06 primeiras escolas em razão de uma limitação do algoritmo
scatter3d(desempenho ~ horas + escola,
          groups = factor(base_exemplo$escola),
          data = base_exemplo,
          fit = "linear",
          surface = T)


################################################################################
#                         ESTIMAÇÃO DO MODELO NULO HLM2                        #
################################################################################

#Estimação do modelo nulo (função lme do pacote nlme)
modelo_nulo_hlm2 <- lme(fixed = desempenho ~ 1, 
                        random = ~ 1 | escola,
                        data = estudante_escola,
                        method = "REML")

#Parâmetros do modelo
summary(modelo_nulo_hlm2)

#Verificando a funcionalidade da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_hlm2)


################################################################################
#                    COMPARAÇÃO DO HLM2 NULO COM UM OLS NULO                   #
################################################################################
#Para estimarmos o modelo OLS nulo, podemos comandar o seguinte
modelo_ols_nulo <- lm(formula = desempenho ~ 1, 
                      data = estudante_escola)

#Parâmetros do modelo OLS nulo
summary(modelo_ols_nulo)

#Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest
lrtest(modelo_ols_nulo, modelo_nulo_hlm2)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


################################################################################
#            ESTIMAÇÃO DO MODELO COM INTERCEPTOS ALEATÓRIOS HLM2               #
################################################################################

#Estimação do modelo com Interceptos Aleatórios
modelo_intercept_hlm2 <- lme(fixed = desempenho ~ horas,
                             random = ~ 1 | escola,
                             data = estudante_escola,
                             method = "REML")

#Parâmetros do modelo
summary(modelo_intercept_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_hlm2)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


################################################################################
#      ESTIMAÇÃO DO MODELO COM INTERCEPTOS E INCLINAÇÕES ALEATÓRIOS HLM2       #
################################################################################

#Estimação do modelo com Interceptos e Inclinações Aleatórios
modelo_intercept_inclin_hlm2 <- lme(fixed = desempenho ~ horas,
                                    random = ~ horas | escola,
                                    data = estudante_escola,
                                    method = "REML")

#Parâmetros do modelo
summary(modelo_intercept_inclin_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_inclin_hlm2)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


################################################################################
#                       ESTIMAÇÃO DO MODELO FINAL HLM2                         #
################################################################################

#Estimação do modelo final
modelo_final_hlm2 <- lme(fixed = desempenho ~ horas + texp + horas:texp,
                         random = ~ horas | escola,
                         data = estudante_escola,
                         method = "REML")

#Parâmetros do modelo
summary(modelo_final_hlm2)

#Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_final_hlm2)

#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4,
         `HLM2 Modelo Final` = 5) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3",
                               "deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


#Melhor visualização dos interceptos e das inclinações aleatórios por escola,
#para o modelo final HLM2

v_final <- data.frame(modelo_final_hlm2[["coefficients"]][["random"]][["escola"]]) %>%
  rename(v00 = 1,
         v10 = 2)
v_final$escola <- c(1:10)
v_final$escola <- as.factor(v_final$escola)

v_final %>% 
  select(escola, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Para observarmos graficamente o comportamento dos valores de v0j, ou seja,
#dos interceptos aleatórios por escola, podemos comandar
random.effects(modelo_final_hlm2) %>% 
  rename(v0j = 1) %>% 
  rownames_to_column("Escola") %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(Escola) %>% 
  ggplot(aes(label = format(v0j, digits = 2), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(Escola), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Escola, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Escola",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")


#Para observarmos graficamente o comportamento dos valores de v1j, ou seja
#das inclinações aleatórias por escola, podemos comandar
random.effects(modelo_final_hlm2) %>% 
  rename(v1j = 2) %>% 
  rownames_to_column("Escola") %>% 
  mutate(color_v1j = ifelse(v1j < 0, "A", "B"),
         hjust_v1j = ifelse(v1j > 0, 1.15, -0.15)) %>% 
  arrange(Escola) %>% 
  ggplot(aes(label = format(v1j, digits = 2), 
             hjust = hjust_v1j)) +
  geom_bar(aes(x = fct_rev(Escola), y = v1j, fill = color_v1j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Escola, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Escola",
       y = expression(nu[1][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

#Gerando os fitted values do modelo HLM2 Final
estudante_escola$hlm2_fitted <- predict(modelo_final_hlm2,
                                        estudante_escola)

# Visualizando os fitted values do modelo
#Visualizando os fitted values por estudante e por escola
predict(modelo_final_hlm2, level = 0:1) %>% 
  mutate(escola = gsub("^.*?\\/","",escola),
         escola = as.factor(as.numeric(escola)),
         desempenho = estudante_escola$desempenho,
         etjk = resid(modelo_final_hlm2)) %>% #função resid gera os termos etjk
  select(escola, desempenho, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

#Efetuando predições
#Exemplo: Quais os valores previstos de desempenho escolar, para dado
#aluno que estuda na escola "1", sabendo-se que ele estuda 11h semanais,
#e que a escola oferece tempo médio de experiência de seus professores
#igual a 3.6 anos?
predict(modelo_final_hlm2, level = 0:1,
        newdata = data.frame(escola = "1",
                             horas = 11,
                             texp = 3.6))

#Valores previstos do desempenho escolar em função da variável horas para o 
#modelo final HLM2 com interceptos e inclinações aleatórios
estudante_escola %>%
  mutate(fitted_escola = predict(modelo_final_hlm2, level = 1)) %>% 
  ggplot() +
  geom_point(aes(x = horas, y = fitted_escola)) +
  geom_smooth(aes(x = horas, y = fitted_escola, color = factor(escola)), 
              method = "lm", se = F) +
  scale_colour_viridis_d() +
  labs(x = "Quantidade Semanal de Horas de Estudo do Aluno",
       y = "Desempenho Escolar (Fitted Values)") +
  theme_bw()


################################################################################
#                       COMPARAÇÃO COM UM MODELO OLS                           #
################################################################################

#Elaborando um modelo OLS para fins de comparação
modelo_ols <- lm(formula = desempenho ~ horas + texp,
                 data = estudante_escola)

#Parâmetros
summary(modelo_ols)

#Comparando os LL dos modelos elaborados
data.frame(OLS = logLik(modelo_ols),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS` = 1,
         `HLM2 Modelo Final` = 2) %>%
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

#LR Test
lrtest(modelo_ols, modelo_final_hlm2)

#Comparando a aderência dos fitted values dos modelos estimados
#Gerando os fitted values do modelo OLS
estudante_escola$ols_fitted <- modelo_ols$fitted.values

#Plotagem
estudante_escola %>%
  ggplot() +
  geom_smooth(aes(x = desempenho, y = ols_fitted, color = "OLS"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y= hlm2_fitted, color = "HLM2 Final"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y = desempenho), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  geom_point(aes(x = desempenho, y = ols_fitted,
                 color = "OLS")) +
  geom_point(aes(x = desempenho, y = hlm2_fitted,
                 color = "HLM2 Final"))  +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue1","darkorchid")) +
  labs(x = "Desempenho", y = "Fitted Values") +
  theme_bw()


################################################################################
#                 COMPARAÇÃO COM UM MODELO OLS COM DUMMIES                     #
################################################################################

#Procedimento n-1 dummies para o contexto
estudante_escola_dummies <- dummy_cols(.data = estudante_escola,
                                       select_columns = "escola",
                                       remove_first_dummy = TRUE,
                                       remove_selected_columns = TRUE)

#Visualizando as dummies na nova base de dados 'estudante_escola_dummies'
estudante_escola_dummies %>%
  select(-hlm2_fitted,-ols_fitted, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 19)

#Modelo OLS com dummies
modelo_ols_dummies <- lm(formula = desempenho ~ horas + texp + escola_2 +
                           escola_3 + escola_4 + escola_5 + escola_6 +
                           escola_7 + escola_8 + escola_9 + escola_10,
                           data = estudante_escola_dummies)

#Parâmetros
summary(modelo_ols_dummies)

#Procedimento stepwise
modelo_ols_dummies_step <- step(object = modelo_ols_dummies,
                                step = qchisq(p = 0.05, df = 1,
                                              lower.tail = FALSE))

#Parâmetros do modelo OLS estimado com dummies por escola a partir do
#procedimento Stepwise
summary(modelo_ols_dummies_step)

#Comparando os LL dos modelos HLM2 Final, OLs e OLS com Dummies e Stepwise
data.frame(OLS = logLik(modelo_ols),
           OLS_Dummies_Step = logLik(modelo_ols_dummies_step),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS` = 1,
         `OLS com Dummies e Stepwise` = 2,
         `HLM2 Modelo Final` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                  values = c("darkorchid","maroon1","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#LR Test
lrtest(modelo_ols_dummies_step, modelo_final_hlm2)

#Comparação entre os parãmetros dos modelos (atente-se para a quantidade de
#parâmetros estimados em cada um deles!)
export_summs(modelo_ols_dummies_step, modelo_final_hlm2,
             model.names = c("OLS com Dummies", "HLM2 Final"))


#Comparando a aderência dos fitted values dos modelos HLM2 Final, OLS e
#OLS com Dummies e Stepwise
#Gerando os fitted values do modelo OLS com Dummies e Stepwise
estudante_escola$ols_step_fitted <- modelo_ols_dummies_step$fitted.values

#Gráfico para a comparação entre os fitted values dos modelos HLM2 Final, OLs e
#OLS com Dummies e Procedimento Stepwise
estudante_escola %>%
  ggplot() +
  geom_smooth(aes(x = desempenho, y = ols_step_fitted, color = "OLS com Dummies"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y= hlm2_fitted, color = "HLM2 Final"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y= ols_fitted, color = "OLS"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y = desempenho), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue1", "maroon1", "darkorchid")) +
  labs(x = "Desempenho", y = "Fitted Values") +
  theme_bw()


#Comparação entre os LLs de todos os modelos estimados neste exemplo
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           OLS = logLik(modelo_ols),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           OLS_Dummies_step = logLik(modelo_ols_dummies_step),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `OLS` = 3,
         `HLM2 com Interceptos Aleatórios` = 4,
         `OLS com Dummies e Stepwise` = 5,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 6,
         `HLM2 Modelo Final` = 7) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 5) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","darkorchid","bisque4",
                               "maroon1","bisque3","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())
