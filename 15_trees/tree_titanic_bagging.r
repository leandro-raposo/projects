# Buscar reprodutibilidade
set.seed(2360873)

# Gera 80% de 1's e 20% de 2's para separar as amostras
n <- sample(1:2, # vamos amostrar elementos do conjunto c(1,2)
            size=nrow(titanic), # O tamanho da amostragem é 891
            replace=TRUE, # Amostragem com reposição (de c(1,2))
            prob=c(0.8,0.2)) # A probabilidade de ser 1 é 80%, de ser 2 é 20%

######################################
# Dividir amostras de treino e teste #

# Amostra de treino: n==1 (os 80%)
treino <- titanic[n==1,]
# Amostra de teste: n==2 (os 20%)
teste <- titanic[n==2,]


######################################
# Treinar a Random Forest            #

# Semente aleatória para buscar a reprodutibilidade
set.seed(2360873)

# medir tempo de execução (iniciar o cronometro)
start_time <- Sys.time()

# Rodar o algoritmo
treino_rf <- randomForest::randomForest(
  Survived ~ ., 
  data = treino, 
  ntree = 50,
  mtry = 3, 
  importance = T)

# parar o cronometro
end_time <- Sys.time()
end_time - start_time


############################
# Avaliar o modelo         #

# Base de treino
avalia <- function(modelo, nome_modelo="modelo"){
  p_treino <- predict(modelo, treino, type='prob') # Probabilidade predita
  c_treino <- predict(modelo, treino)              # Classificação
  
  #Base de teste
  p_teste <- predict(modelo, teste, type='prob')
  c_teste <- predict(modelo, teste)
  
  # Data frame de avaliação (Treino)
  aval_treino <- data.frame(obs=treino$Survived, 
                               pred=c_treino,
                               Y = p_treino[,2],
                               N = 1-p_treino[,2]
  )
  
  # Data frame de avaliação (Teste)
  aval_teste <- data.frame(obs=teste$Survived, 
                              pred=c_teste,
                              Y = p_teste[,2],
                              N = 1-p_teste[,2]
  )
  
  tcs_treino <- caret::twoClassSummary(aval_treino, 
                                       lev=levels(aval_treino$obs))
  tcs_teste <- caret::twoClassSummary(aval_teste, 
                                      lev=levels(aval_teste$obs))
  ##########################
  # Curva ROC              #
  
  CurvaROC <- ggplot2::ggplot(aval_teste, aes(d = obs, m = Y, colour='1')) + 
    plotROC::geom_roc(n.cuts = 0, color="blue") +
    plotROC::geom_roc(data=aval_treino,
                      aes(d = obs, m = Y, colour='1'),
                      n.cuts = 0, color = "red") +
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    theme(legend.position = "none") +
    ggtitle(paste("Curva ROC | ", nome_modelo, " | AUC-treino=",
                  percent(tcs_treino[1]),
                  "| AUC_teste = ",
                  percent(tcs_teste[1]))
    )
  
  print('Avaliação base de treino')
  print(tcs_treino)
  print('Avaliação base de teste')
  print(tcs_teste)
  CurvaROC
}
avalia(treino_rf, nome_modelo="Random Forest")

###########################################
# Usando o Caret para fazer o grid-search #

tempo_ini <- Sys.time()

# number: number of folds for training
# repeats: keep the number for training

# O objeto gerado por trainControl vai controlar o algoritmo 
controle <- caret::trainControl(
  method='repeatedcv', # Solicita um K-Fold com repetições
  number=4, # Número de FOLDS (o k do k-fold)
  repeats=2, # Número de repetições
  search='grid', # especifica o grid-search
  summaryFunction = twoClassSummary, # Função de avaliação de performance
  classProbs = TRUE # Necessário para calcular a curva ROC
)

# agora vamos especificar o grid
grid <- base::expand.grid(.mtry=c(1:10))

# Vamos treinar todos os modelos do grid-search com cross-validation
gridsearch_rf <- caret::train(Survived ~ .,         # Fórmula (todas as variáveis)
                              data = treino,       # Base de dados
                              method = 'rf',        # Random-forest
                              metric='ROC',         # Escolhe o melhor por essa métrica
                              trControl = controle, # Parâmetros de controle do algoritmo
                              ntree=100,            # Numero de árvores
                              tuneGrid = grid)      # Percorre o grid especificado aqui

print(gridsearch_rf)
plot(gridsearch_rf)

tempo_fim <- Sys.time()
tempo_fim - tempo_ini


###################################
# Avaliar o modelo tunado         #
avalia(gridsearch_rf, nome_modelo='RF Tunado')
