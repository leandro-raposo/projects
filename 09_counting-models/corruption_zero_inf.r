
pacotes <- c("plotly","tidyverse","knitr","kableExtra","reshape2","ggrepel",
             "lmtest","jtools","questionr","MASS","pscl","overdisp","glmmTMB")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# CARREGAMENTO DA BASE DE DADOS
load(file = "corruption.RData")

#Visualizando a base de dados
corruption %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 19)

glimpse(corruption) #Visualização das observações e das  especificações 
#referentes às variáveis da base de dados

#Estatísticas descritivas univariadas e tabela de frequências
summary(corruption)

#Tabela de frequências da variável dependente (função freq para gerar tabelas de
#frequência do pacote questionr)
freq(corruption$violations) %>%
  kable()%>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 19)

#Histograma da variável dependente
ggplotly(
  corruption %>%
    ggplot(aes(x = violations,
               fill = ..count..)) +
    geom_histogram(bins = round(2 * nrow(corruption) ^ (1 / 3)),
                   color = "black") +
    scale_fill_gradient("Contagem",
                        low = "#440154FF", 
                        high = "#FDE725FF") +
    labs(x = "Quantidade de violações de trânsito",
         y = "Frequência") +
    theme_bw()
)

#Diagnóstico preliminar para observação de eventual igualdade entre a média e
#a variância da variável dependente 'violations'
corruption %>%
  summarise(Média = mean(violations),
            Variância = var(violations)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 30)

#Comportamento das variáveis 'corruption' e 'violations' antes e depois do 
#início da vigência da lei
corruption %>%
  mutate(lnviolations = log(violations),
         lnviolations = ifelse(lnviolations == -Inf,
                               yes = 0, 
                               no = lnviolations)) %>%
  ggplot(aes(x = corruption, y = lnviolations)) +
  geom_point(color = "black") +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm",
              formula = y ~ splines::bs(x),
              se = FALSE, size = 2) +
  geom_text_repel(aes(label = code), # pacote ggrepel
                  size = 2,
                  color = "black",
                  max.overlaps = 100) +
  labs(y = "Violações de Trânsito em NY (logs)",
       x = "Índice de Corrupção dos Países") +
  scale_color_manual("Label:",
                     values = "gold") +
  facet_wrap(~post) +
  theme_bw()


################################################################################
#                        ESTIMAÇÃO DO MODELO POISSON                           #
################################################################################
#Estimação do modelo Poisson
modelo_poisson <- glm(formula = violations ~ staff + post + corruption,
                      data = corruption,
                      family = "poisson")

#Parâmetros e valor de Log-Likelihood (LL)
summary(modelo_poisson)
logLik(modelo_poisson)


################################################################################
#            TESTE DE SUPERDISPERSÃO DE CAMERON E TRIVEDI (1990)               #
################################################################################
#CAMERON, A. C.; TRIVEDI, P. K. Regression-based tests for overdispersion in
#the Poisson model. Journal of Econometrics, v. 46, n. 3, p. 347-364, 1990.

overdisp(x = corruption,
         dependent.position = 3,
         predictor.position = 4:6)


################################################################################
#                   ESTIMAÇÃO DO MODELO BINOMIAL NEGATIVO                      #
################################################################################
#Estimação do modelo binomial negativo pela função glm.nb do pacote MASS
#Modelo Binomial Negativo do Tipo 2 (NB2)
modelo_bneg <- glm.nb(formula = violations ~ staff + post + corruption,
                      data = corruption)

#Parâmetros e valor de Log-Likelihood (LL)
summary(modelo_bneg)
logLik(modelo_bneg)


################################################################################
#              ESTIMAÇÃO DO MODELO ZERO-INFLATED POISSON (ZIP)                 #
################################################################################
#Estimação do modelo ZIP pela função zeroinfl do pacote pscl
modelo_zip <- zeroinfl(formula = violations ~ corruption + post + staff
                       | corruption,
                       data = corruption,
                       dist = "poisson")

#Parâmetros e valor de Log-Likelihood (LL)
summary(modelo_zip)
logLik(modelo_zip)

#Teste de Vuong:
#VUONG, Q. H. Likelihood ratio tests for model selection and non-nested
#hypotheses. Econometrica, v. 57, n. 2, p. 307-333, 1989.

vuong(m1 = modelo_poisson,
      m2 = modelo_zip)


################################################################################
#        ESTIMAÇÃO DO MODELO ZERO-INFLATED BINOMIAL NEGATIVO (ZINB)            #
################################################################################
#Estimação do modelo ZINB pela função zeroinfl do pacote pscl
modelo_zinb <- zeroinfl(formula = violations ~ corruption + post + staff
                        | corruption,
                        data = corruption,
                        dist = "negbin")

#Parâmetros e valor de Log-Likelihood (LL)
summary(modelo_zinb)
logLik(modelo_zinb)

#Teste de Vuong (1989)
vuong(m1 = modelo_bneg,
      m2 = modelo_zinb)


################################################################################
#     ESTIMAÇÃO DO MODELO MULTINÍVEL ZERO-INFLATED BINOMIAL NEGATIVO (ZINB)    #
################################################################################
#Estimação do modelo ZINBm pela função glmmTMB do pacote glmmTMB

modelo_zinbm <- glmmTMB(formula = violations ~ corruption + post + staff
                        + (1 | country),
                        zi = ~ corruption,
                        family = nbinom2,
                        data = corruption)

#Parâmetros e valor de Log-Likelihood (LL)
summary(modelo_zinbm)
logLik(modelo_zinbm)

export_summs(modelo_poisson, modelo_bneg, modelo_zinbm,
             model.names = c("Poisson", "BNEG", "ZINB Multilevel"))


#Comparação entre os LLs de todos os modelos estimados neste exemplo
data.frame(Poisson = logLik(modelo_poisson),
           ZIP = logLik(modelo_zip),
           BNEG = logLik(modelo_bneg),
           ZINB = logLik(modelo_zinb),
           ZINBM = logLik(modelo_zinbm)) %>%
  rename(`Poisson` = 1,
         `ZIP` = 2,
         `BNEG` = 3,
         `ZINB` = 4,
         `ZINB Multilevel` = 5) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 5) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("darkorchid","maroon1","darkblue",
                               "deepskyblue3","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Teste de razão de verossimilhança
lrtest(modelo_zinb,modelo_zinbm)
