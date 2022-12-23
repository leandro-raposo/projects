pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","lmtest","caret","pROC","ROCR","nnet","magick",
             "cowplot")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#  REGRESSÃO LOGÍSTICA BINÁRIA
#Estabelecendo uma função para a probabilidade de ocorrência de um evento
prob <- function(z){
  prob = 1 / (1 + exp(-z))
}

#Plotando a curva sigmóide teórica de ocorrência de um evento para um range
#do logito z entre -5 e +5
data.frame(z = -5:5) %>%
  ggplot() +
  stat_function(aes(x = z, color = "Prob. Evento"),
                fun = prob,
                size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "Logito z",
       y = "Probabilidade") +
  theme_bw()

# REGRESSÃO LOGÍSTICA MULTINOMIAL

# CARREGAMENTO DA BASE DE DADOS
load(file = "AtrasadoMultinomial.RData")

#Visualizando a base de dados AtrasadoMultinomial
AtrasadoMultinomial %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Estatísticas descritivas univariadas da base de dados
summary(AtrasadoMultinomial)

##############################################################################
#           EXEMPLO 04 - ESTIMAÇÃO DE UM MODELO LOGÍSTICO MULTINOMIAL        #
##############################################################################
#Apontando a categoria de referência
AtrasadoMultinomial$atrasado <- relevel(AtrasadoMultinomial$atrasado, 
                                  ref = "não chegou atrasado")

#Estimação do modelo - função multinom do pacote nnet
modelo_atrasado <- multinom(formula = atrasado ~ dist + sem, 
                            data = AtrasadoMultinomial)

#Parâmetros do modelo_atrasado
summary(modelo_atrasado)

#LL do modelo_atrasado
logLik(modelo_atrasado)

#A função summ do pacote jtools não funciona para objetos de classe 'multinom'. Logo,
#vamos definir uma função Qui2 para se extrair a estatística geral do modelo:
Qui2 <- function(x) {
  maximo <- logLik(x)
  minimo <- logLik(update(x, ~1, trace = F))
  Qui.Quadrado <- -2*(minimo - maximo)
  pvalue <- pchisq(Qui.Quadrado, df = 1, lower.tail = F)
  df <- data.frame()
  df <- cbind.data.frame(Qui.Quadrado, pvalue)
  return(df)
}

#Estatística geral do modelo_atrasado
Qui2(modelo_atrasado)

#Na Regressão Logística Multinomial, o R quebra a lógica de relatórios que, 
#normalmente, oferece para os GLM. Também é preciso notar que a linguagem 
#básica  não consegue rodar esse tipo de regressão, sendo necessário o pacote
#nnet. Além do mais, não são fornecidas as estatísticas z de Wald, nem os
#p-values das variáveis da modelagem.

#Explicando a lógica do R para a Logística Multinomial:

#1 - Foram estabelecidas *labels* para as categorias da variável dependente: 
#'não chegou atrasado', 'chegou atrasado à primeira aula' e 'chegou atrasado à
#segunda aula';

#2 - Foi comandado que a categoria de referência seria a categoria 'não chegou
#atrasado', e isso explica o porquê dela não aparecer no relatório gerado;

#3 - O relatório é dividido em duas partes: 'Coefficients' e 'Std. Errors'. 
#Cada linha da seção 'Coefficients' informa um logito para cada categoria da
#variável dependente, com exceção da categoria de referência. Já a seção 
#'Std. Errors' informa o erro-padrão de cada parâmetro em cada logito.

#Para calcular as estatísticas z de Wald, há que se dividir os valores da 
#seção 'Coefficients' pelos valores da seção 'Std. Errors.' Assim, temos que:  

zWald_modelo_atrasado <- (summary(modelo_atrasado)$coefficients / 
                            summary(modelo_atrasado)$standard.errors)

zWald_modelo_atrasado

#Porém, ainda faltam os respectivos p-values. Assim, os valores das probabilidades 
#associadas às abscissas de uma distribuição normal-padrão é dada pela função
#pnorm(), considerando os valores em módulo - abs(). Após isso, multiplicamos 
#por dois os valores obtidos para considerar os dois lados da distribuição
#normal padronizada (distribuição bicaudal). Desta forma, temos que:
round((pnorm(abs(zWald_modelo_atrasado), lower.tail = F) * 2), 4)

#Fazendo predições para o modelo_atrasado Exemplo: qual a probabilidade média
#de atraso para cada categoria da variável dependente, se o indivíduo tiver 
#que percorrer 22km e passar por 12 semáforos.
predict(modelo_atrasado, 
        data.frame(dist = 22, sem = 12), 
        type = "probs")

predict(modelo_atrasado, 
        data.frame(dist = 22, sem = 12), 
        type = "class")

##############################################################################
#                   EXEMPLO 04 - A EFETIVIDADE GERAL DO MODELO               #
##############################################################################
#Adicionando as prováveis ocorrências de evento apontadas pela modelagem à 
#base de dados
AtrasadoMultinomial$predicao <- predict(modelo_atrasado, 
                                     newdata = AtrasadoMultinomial, 
                                     type = "class")

#Visualizando a nova base de dados AtrasadoMultinomial com a variável 'predicao'
AtrasadoMultinomial %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

attach(AtrasadoMultinomial)

#Criando uma tabela para comparar as ocorrências reais com as predições
EGM <- as.data.frame.matrix(table(atrasado, predicao))

#Visualizando a tabela EGM
EGM %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Eficiência global do modelo
acuracia <- (round((sum(diag(table(atrasado, predicao))) / 
                      sum(table(atrasado, predicao))), 2))

acuracia

##############################################################################
#                      EXEMPLO 04 - PLOTAGENS DAS PROBABILIDADES             #
##############################################################################

#Adicionando à base de dados as probabilidades em razão de cada categoria:
levels(AtrasadoMultinomial$atrasado)

AtrasadoMultinomial[c("não chegou atrasado",
                      "chegou atrasado à primeira aula",
                      "chegou atrasado à segunda aula")] <- modelo_atrasado$fitted.values

#Plotagem das smooth probability lines para a variável 'dist'
ggplotly(
  AtrasadoMultinomial %>% 
    dplyr::select(-predicao, - estudante) %>% 
    rename(y = 1) %>% 
    melt(id.vars = c("y","dist","sem"),
         value.name = "probabilidades") %>% 
    rename(categorias = variable) %>%
    mutate(categorias = factor(categorias,
                               levels = c("não chegou atrasado",
                                          "chegou atrasado à primeira aula",
                                          "chegou atrasado à segunda aula"))) %>% 
    ggplot() +
    geom_smooth(aes(x = dist, y = probabilidades, color = categorias), 
                method = "loess", formula = y ~ x, se = T) +
    labs(x = "Distância Percorrida",
         y = "Probabilidades",
         color = "Legenda:") +
    scale_color_viridis_d() +
    theme_bw()
)

#Plotagem das smooth probability lines para a variável 'sem'
ggplotly(
  AtrasadoMultinomial %>% 
    dplyr::select(-predicao, - estudante) %>% 
    rename(y = 1) %>% 
    melt(id.vars = c("y","dist","sem"),
         value.name = "probabilidades") %>% 
    rename(categorias = variable) %>%
    mutate(categorias = factor(categorias,
                               levels = c("não chegou atrasado",
                                          "chegou atrasado à primeira aula",
                                          "chegou atrasado à segunda aula"))) %>% 
    ggplot() +
    geom_smooth(aes(x = sem, y = probabilidades, color = categorias), 
                method = "loess", formula = y ~ x, se = T) +
    labs(x = "Semáforos no Percurso",
         y = "Probabilidades",
         color = "Legenda:") +
    scale_color_viridis_d() +
    theme_bw()
)

#Plotagem tridimensional para cada probabilidade de ocorrência de cada cada
#categoria da variável dependente

AtrasadoMultinomial$p0 <- AtrasadoMultinomial$`não chegou atrasado`
AtrasadoMultinomial$p1 <- AtrasadoMultinomial$`chegou atrasado à primeira aula`
AtrasadoMultinomial$p2 <- AtrasadoMultinomial$`chegou atrasado à segunda aula`


#p0 - Probabilidades de não chegar atrasado (função scatter3d do pacote car):
scatter3d(AtrasadoMultinomial$dist,AtrasadoMultinomial$p0,
          AtrasadoMultinomial$sem,
          groups = AtrasadoMultinomial$atrasado,
          data = AtrasadoMultinomial,
          fit = "smooth")

#Outro modo:
plot_ly(x = AtrasadoMultinomial$dist, 
        y = AtrasadoMultinomial$sem, 
        z = AtrasadoMultinomial$`não chegou atrasado`,
        type = "mesh3d",
        name = "ótimo",
        intensity = AtrasadoMultinomial$`não chegou atrasado`,
        colors = colorRamp(c("red","yellow","chartreuse3","lightblue","blue"))) %>% 
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")),
         title = "Categoria Não Chegou Atrasado")


#p1 - Probabilidades de chegar atrasado à primeira aula:
scatter3d(AtrasadoMultinomial$dist,AtrasadoMultinomial$p1,
          AtrasadoMultinomial$sem,
          groups = AtrasadoMultinomial$atrasado,
          data = AtrasadoMultinomial,
          fit = "smooth")

#Outro modo:
plot_ly(x = AtrasadoMultinomial$dist, 
        y = AtrasadoMultinomial$sem, 
        z = AtrasadoMultinomial$`chegou atrasado à primeira aula`,
        type = "mesh3d",
        name = "ótimo",
        intensity = AtrasadoMultinomial$`chegou atrasado à primeira aula`,
        colors = colorRamp(c("red", "yellow", "chartreuse3", "lightblue", "blue"))) %>% 
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")),
         title = "Categoria Chegou Atrasado à Primeira Aula")


#p2 - Probabilidades de chegar atrasado à segunda aula:
scatter3d(AtrasadoMultinomial$dist,AtrasadoMultinomial$p2,
          AtrasadoMultinomial$sem,
          groups = AtrasadoMultinomial$atrasado,
          data = AtrasadoMultinomial,
          fit = "smooth")

#Outro modo:
plot_ly(x = AtrasadoMultinomial$dist, 
        y = AtrasadoMultinomial$sem, 
        z = AtrasadoMultinomial$`chegou atrasado à segunda aula`,
        type = "mesh3d",
        name = "ótimo",
        intensity = AtrasadoMultinomial$`chegou atrasado à segunda aula`,
        colors = colorRamp(c("red", "yellow", "chartreuse3", "lightblue", "blue"))) %>% 
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")),
         title = "Categoria Chegou Atrasado à Segunda Aula")


#Visualização das sigmóides tridimensionais em um único gráfico:
naoatrasado <- plot_ly(x = AtrasadoMultinomial$dist, 
                       y = AtrasadoMultinomial$sem, 
                       z = AtrasadoMultinomial$`não chegou atrasado`,
                       type = "mesh3d",
                       name = "não chegou atrasado") %>%
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")))

atrasadoprimeira <- plot_ly(x = AtrasadoMultinomial$dist, 
                            y = AtrasadoMultinomial$sem, 
                            z = AtrasadoMultinomial$`chegou atrasado à primeira aula`,
                            type = "mesh3d",
                            name = "chegou atrasado à primeira aula") %>%
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")))

atrasadosegunda <- plot_ly(x = AtrasadoMultinomial$dist,
                           y = AtrasadoMultinomial$sem,
                           z = AtrasadoMultinomial$`chegou atrasado à segunda aula`,
                           type = "mesh3d",
                           name = "chegou atrasado à segunda aula") %>%
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")))

subplot(naoatrasado, atrasadoprimeira, atrasadosegunda)
