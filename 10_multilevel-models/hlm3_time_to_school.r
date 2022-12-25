pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra",
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

#Carregando a base de dados
load(file = "tempo_estudante_escola.RData")

#Visualização da base de dados
tempo_estudante_escola %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Estatísticas descritivas e tabelas de frequências
summary(tempo_estudante_escola)

#Estudo sobre o balanceamento dos dados em relação à quantidade de alunos 
#por período analisado
tempo_estudante_escola %>%
  rename(Mês = 3,
         `Quantidade de Alunos` = 2) %>% 
  group_by(Mês) %>% 
  summarise(`Quantidade de Alunos` = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Estudo sobre o desbalanceamento da quantidade de alunos aninhados em 
#escolas
tempo_estudante_escola %>% 
  rename(Escola = 1,
         `Quantidade de Alunos` = 2) %>% 
  group_by(Escola) %>% 
  summarise(`Quantidade de Alunos` = n()/4) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Gráfico da evolução temporal média do desempenho escolar dos estudantes
#(ajuste linear)
ggplotly(
  tempo_estudante_escola %>%
    ggplot(aes(x = mes, y = desempenho, group = 1, label = estudante)) +
    geom_point(color = "gold", size = 2, alpha = 0.2) +
    geom_smooth(color = "#440154FF", method = "lm", se = F, size = 2) +
    labs(x = "Mês",
         y = "Desempenho Escolar") +
    theme_bw()
)

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (desempenho), com histograma
ggplotly(
  ggplot(tempo_estudante_escola, aes(x = desempenho)) +
    geom_density(aes(x = desempenho), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                   bins = 30) +
    theme_classic()
)

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (desempenho) por escola
ggplotly(
  ggplot(tempo_estudante_escola, aes(x = desempenho)) +
    geom_density(aes(color = escola, fill = escola), 
                 position = "identity", alpha = 0.2) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_classic()
)

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (desempenho), com histograma e por escola separadamente
#(função facet_wrap)
tempo_estudante_escola %>% 
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

#Exploração visual da evolução temporal do desempenho dos 50 primeiros
#estudantes da base de dados (50 estudantes em razão da visualização no gráfico)
tempo_estudante_escola %>%
  filter(estudante %in% 1:50) %>% 
  ggplot(aes(group = estudante)) +
  geom_point(aes(x = mes, y = desempenho, color = estudante), size = 3) +
  geom_line(aes(x = mes, y = desempenho, color = estudante), size = 1) +
  guides(color = F) +
  scale_colour_viridis_d() +
  labs(x = "Mês",
       y = "Desempenho Escolar") +
  theme_bw()

#Gráfico da evolução temporal do desempenho médio por escola (ajustes lineares)
ggplotly(
  tempo_estudante_escola %>%
    ggplot(aes(x = mes, y = desempenho, color = escola)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point(size = 3, alpha = 0.2) +
    guides(color = F) +
    scale_colour_viridis_d() +
    labs(x = "Mês",
         y = "Desempenho Escolar") +
    theme_bw()
)


################################################################################
#                         ESTIMAÇÃO DO MODELO NULO HLM3                        #
################################################################################

#Estimação do modelo nulo (função lme do pacote nlme)
modelo_nulo_hlm3 <- lme(fixed = desempenho ~ 1,
                        random = list(escola = ~1, estudante = ~1),
                        data = tempo_estudante_escola,
                        method = "REML")

#Parâmetros do modelo
summary(modelo_nulo_hlm3)

#Erros-padrão do modelo por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_hlm3)


################################################################################
#                      COMPARAÇÃO DO HLM3 NULO COM UM OLS NULO                 #
################################################################################
#Para estimarmos o modelo OLS nulo, podemos comandar o seguinte
modelo_ols_nulo <- lm(formula = desempenho ~ 1,
                      data = tempo_estudante_escola)

#Parâmetros do modelo OLS nulo
summary(modelo_ols_nulo)

#Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest
lrtest(modelo_ols_nulo, modelo_nulo_hlm3)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM3_Nulo = logLik(modelo_nulo_hlm3)) %>%
  rename(`OLS Nulo` = 1,
         `HLM3 Nulo` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 7) +
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
#               ESTIMAÇÃO DO MODELO HLM3 COM TENDÊNCIA LINEAR E                #
#                    INTERCEPTOS E INCLINAÇÕES ALEATÓRIOS                      #
################################################################################

#Estimação do modelo com Tendência Linear e Interceptos e Inclinações Aleatórios
modelo_intercept_inclin_hlm3 <- lme(fixed = desempenho ~ mes,
                                    random = list(escola = ~mes, estudante = ~mes),
                                    data = tempo_estudante_escola,
                                    method = "REML")

#Parâmetros do modelo
summary(modelo_intercept_inclin_hlm3)

#Erros-padrão do modelo por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_inclin_hlm3)

#Função lrtest para comparar os LLs dos modelos
lrtest(modelo_nulo_hlm3, modelo_intercept_inclin_hlm3)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM3_Nulo = logLik(modelo_nulo_hlm3),
           HLM3_Inclinacoes_Aleatorios = logLik(modelo_intercept_inclin_hlm3)) %>%
  rename(`OLS Nulo` = 1,
         `HLM3 Nulo` = 2,
         `HLM3 com Int. e Incl. Aleat.` = 3) %>% 
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 6) +
  labs(title = "Comparação do Ganho de LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","coral4")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


################################################################################
#               ESTIMAÇÃO DO MODELO HLM3 COM TENDÊNCIA LINEAR,                 #
#                    INTERCEPTOS E INCLINAÇÕES ALEATÓRIOS                      #
#           E AS VARIÁVEIS 'ativ' DE NÍVEL 2 E 'text' DE NÍVEL 3               #
################################################################################

#Estimação do modelo com Tendência Linear, Interceptos e Inclinações Aleatórios
#e as variáveis 'ativ' de Nível 2 e 'texp' de Nível 3
modelo_completo_hlm3 <- lme(fixed = desempenho ~ mes + ativ + texp +
                              ativ:mes + texp:mes,
                            random = list(escola = ~mes, estudante = ~mes),
                            data = tempo_estudante_escola,
                            method = "REML")

#Parâmetros do modelo
summary(modelo_completo_hlm3)

#Erros-padrão do modelo por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_completo_hlm3)

#Função lrtest para comparar os LLs dos modelos
lrtest(modelo_intercept_inclin_hlm3, modelo_completo_hlm3)

#Visualização do ganho de LogLik entre as estimações:
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM3_Nulo = logLik(modelo_nulo_hlm3),
           HLM3_Inclinacoes_Aleatorios = logLik(modelo_intercept_inclin_hlm3),
           HLM3_Completo = logLik(modelo_completo_hlm3)) %>%
  rename(`OLS Nulo` = 1,
         `HLM3 Nulo` = 2,
         `HLM3 com Int. e Incl. Aleat.` = 3,
         `HLM3 Completo` = 4) %>% 
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 6) +
  labs(title = "Comparação do Ganho de LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","coral4","#440154FF")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


#Para acessarmos os valores de v0jk e v1jk (efeitos aleatórios de intercepto e 
#de inclinação no nível estudante, respectivamente) e de t00k e t10k (efeitos
#aleatórios de intercepto e de inclinação no nível escola, respectivamente),
#podemos proceder com os seguintes códigos:

#Nível estudante:
random.effects(modelo_completo_hlm3)[["estudante"]] %>% 
  rename(v0jk = 1,
         v1jk = 2) %>% 
  rownames_to_column("Estudante") %>% 
  mutate(Estudante = gsub("^.*?\\/","",Estudante)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Nível escola:
random.effects(modelo_completo_hlm3)[["escola"]] %>% 
  rename(t00k = 1,
         t10k = 2) %>% 
  rownames_to_column("Escola") %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Para observarmos graficamente o comportamento dos valores de v0jk, ou seja,
#dos interceptos aleatórios por estudante, podemos comandar:
ggplotly(
  random.effects(modelo_completo_hlm3)[["estudante"]] %>% 
    rename(v0jk = 1,
           v1jk = 2) %>% 
    rownames_to_column("Estudante") %>% 
    mutate(Estudante = gsub("^.*?\\/","",Estudante)) %>% 
    group_by(Estudante) %>% 
    summarise(v0jk = mean(v0jk)) %>% 
    ggplot(aes(x = fct_rev(Estudante), y = v0jk, label = Estudante)) +
    geom_bar(stat = "identity", color = "gold") +
    coord_flip() +
    labs(x = "Estudante",
         y = "v0jk") +
    theme(legend.title = element_blank(), 
          panel.background = element_rect("white"),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
)


#Para observarmos graficamente o comportamento dos valores de v1jk, ou seja,
#das inclinações aleatórias por estudante, podemos comandar:
ggplotly(
  random.effects(modelo_completo_hlm3)[["estudante"]] %>% 
    rename(v0jk = 1,
           v1jk = 2) %>% 
    rownames_to_column("Estudante") %>% 
    mutate(Estudante = gsub("^.*?\\/","",Estudante)) %>% 
    group_by(Estudante) %>% 
    summarise(v1jk = mean(v1jk)) %>% 
    ggplot(aes(x = fct_rev(Estudante), y = v1jk, label = Estudante)) +
    geom_bar(stat = "identity", color = "darkorchid4") + 
    coord_flip() +
    labs(x = "Estudante",
         y = "v1jk") +
    theme(legend.title = element_blank(), 
          panel.background = element_rect("white"),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
)

#Para observarmos graficamente o comportamento dos valores de t00k, ou seja,
#dos interceptos aleatórios por escola, podemos comandar:
random.effects(modelo_completo_hlm3)[["escola"]] %>% 
  rename(t00k = 1,
         t10k = 2) %>% 
  rownames_to_column("Escola") %>% 
  mutate(color_t00k = ifelse(t00k < 0, "A", "B"),
         color_t10k = ifelse(t10k < 0, "A", "B"),
         hjust_t00k = ifelse(t00k > 0, 1.15, -0.15),
         hjust_t10k = ifelse(t10k > 0, 1.15, -0.15)) %>% 
  arrange(Escola) %>% 
  ggplot(aes(label = round(t00k, digits = 3), 
             hjust = hjust_t00k)) +
  geom_bar(aes(x = fct_rev(Escola), y = t00k, fill = color_t00k),
           stat = "identity", color = "black") +
  geom_text(aes(x = Escola, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Escola",
       y = expression(t[0][0][k])) +
  scale_fill_manual(values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

#Para observarmos graficamente o comportamento dos valores de t10k, ou seja,
#das inclinações aleatórias por escola, podemos comandar:
random.effects(modelo_completo_hlm3)[["escola"]] %>% 
  rename(t00k = 1,
         t10k = 2) %>% 
  rownames_to_column("Escola") %>% 
  mutate(color_t00k = ifelse(t00k < 0, "A", "B"),
         color_t10k = ifelse(t10k < 0, "A", "B"),
         hjust_t00k = ifelse(t00k > 0, 1.15, -0.15),
         hjust_t10k = ifelse(t10k > 0, 1.15, -0.15)) %>% 
  arrange(Escola) %>% 
  ggplot(aes(label = round(t10k, digits = 3), 
             hjust = hjust_t10k)) +
  geom_bar(aes(x = fct_rev(Escola), y = t10k, fill = color_t10k),
           stat = "identity", color = "black") +
  geom_text(aes(x = Escola, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Escola",
       y = expression(t[1][0][k])) +
  scale_fill_manual(values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

# Visualizando os fitted values do modelo
#Visualizando os fitted values por estudante e por escola
predict(modelo_completo_hlm3, level = 0:2) %>% 
  mutate(estudante = gsub("^.*?\\/","",estudante),
         estudante = as.factor(as.numeric(estudante)),
         escola = as.factor(as.numeric(escola)),
         mes = tempo_estudante_escola$mes,
         desempenho = tempo_estudante_escola$desempenho,
         etjk = resid(modelo_completo_hlm3)) %>% #função resid gera os termos etjk
  rename("fitted fixed" = 3,
         "fitted escola" = 4,
         "fitted estudante" = 5) %>%
  select(escola, estudante, mes, desempenho, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 18)

#Efetuando predições
#Exemplo: Quais os valores previstos de desempenho escolar no primeiro mês (mes = 1)
#para o estudante "1" da escola "1", sabendo-se que esta escola oferece tempo médio
#de experiência de seus professores igual a 2 anos?
predict(modelo_completo_hlm3, level = 0:2,
        newdata = data.frame(escola = "1",
                             estudante = "1",
                             mes = 1,
                             ativ = c("não","sim"),
                             texp = 2))

#Gráfico com os valores previstos do desempenho escolar ao longo do tempo para os 47 
#primeiros estudantes da amostra (47 estudantes que estão na escola 1)
predict(modelo_completo_hlm3, level = 0:2) %>% 
  mutate(estudante = gsub("^.*?\\/","",estudante),
         estudante = as.factor(as.numeric(estudante)),
         mes = tempo_estudante_escola$mes) %>% 
  rename(fitted_fixed = 3,
         fitted_escola = 4,
         fitted_estudante = 5) %>% 
  right_join(tempo_estudante_escola, 
             by = c("escola","estudante","mes")) %>%
  filter(estudante %in% 1:47) %>%
  ggplot(aes(x = mes, y = fitted_estudante, color = estudante)) +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  geom_point(size = 4, alpha = 0.4) +
  guides(color = F) +
  scale_colour_viridis_d() +
  labs(x = "Mês",
       y = "Desempenho Escolar") +
  theme_bw()


################################################################################
#           FINALIZANDO... COMPARAÇÃO COM UM MODELO OLS COM DUMMIES            #
################################################################################
#Estimando um modelo OLS com as mesmas variáveis do modelo HLM3

#Procedimento n-1 dummies para escolas
base_dummizada <- dummy_cols(.data = tempo_estudante_escola,
                             select_columns = "escola",
                             remove_first_dummy = T,
                             remove_selected_columns = TRUE)

#Estimando um modelo OLS com as mesmas variáveis do modelo HLM3
modelo_ols_dummies <- lm(formula = desempenho ~ . + ativ:mes + texp:mes - estudante,
                         data = base_dummizada)

#Parâmetros do modelo ols_final
summary(modelo_ols_dummies)

#Procedimento stepwise
modelo_ols_dummies_step <- step(object = modelo_ols_dummies,
                                k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

#Parâmetros do modelo OLS final
summary(modelo_ols_dummies_step)

#Comparando os LL dos modelos HLM3 Completo e OLS com Dummies e Procedimento Stepwise
data.frame(OLS_Final = logLik(modelo_ols_dummies_step),
           HLM3_Completo = logLik(modelo_completo_hlm3)) %>%
  rename(`OLS Final com Stepwise` = 1,
         `HLM3 Completo` = 2) %>% 
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 6) +
  labs(title = "Comparação do Ganho de LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("orange","#440154FF")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Likelihood ratio test (lrtest) para comparação entre os modelos HLM3 Completo
#e OLS com Dummies e Procedimento Stepwise
lrtest(modelo_ols_dummies_step, modelo_completo_hlm3)

#Comparação entre os parãmetros dos modelos (atente-se para a quantidade de
#parâmetros estimados em cada um deles!)
export_summs(modelo_ols_dummies_step, modelo_completo_hlm3,
             model.names = c("OLS com Dummies", "HLM3 Completo"))

#Gráfico para a comparação entre os fitted values dos modelos HLM3 Completo e
#OLS com Dummies e Procedimento Stepwise
predict(modelo_completo_hlm3, level = 0:2) %>% 
  mutate(estudante = gsub("^.*?\\/","",estudante),
         estudante = as.factor(as.numeric(estudante)),
         mes = tempo_estudante_escola$mes) %>% 
  rename(fitted_fixed = 3,
         fitted_escola = 4,
         fitted_estudante = 5) %>% 
  right_join(tempo_estudante_escola, 
             by = c("escola","estudante","mes"))  %>% 
  mutate(fitted_ols = modelo_ols_dummies_step$fitted.values) %>% 
  ggplot() +
  geom_line(aes(x = desempenho, y = desempenho)) +
  geom_smooth(aes(x = desempenho, y = fitted_ols,
                  color = "OLS"), se = F, size = 2)  +
  geom_smooth(aes(x = desempenho, y = fitted_estudante,
                  color = "HLM3"), se = F, size = 2) +
  geom_point(aes(x = desempenho, y = fitted_ols,
                 color = "OLS"), shape = 1, size = 4, alpha = 4)  +
  geom_point(aes(x = desempenho, y = fitted_estudante,
                 color = "HLM3"), shape = 0, size = 4, alpha = 0.4) +
  labs(x = NULL,
       y = NULL) +
  scale_color_manual("Modelos:", values = c("#440154FF","orange")) +
  theme(legend.title = element_blank(), 
        panel.border = element_rect(NA),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey80"),
        legend.position = "bottom")