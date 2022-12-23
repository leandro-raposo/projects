# XGBoosting
set.seed(2360873)

# parâmetros do trainControl
# number: number of folds or resampling iterations
# repeats: for repeated k-fold cross-validation, the number of complete sets of folds to compute
tempo_ini <- Sys.time()

controle <- caret::trainControl(
  "cv",
  number = 10,
  summaryFunction = twoClassSummary, # Função de avaliação de performance
  classProbs = TRUE # Necessário para calcular a curva ROC
)
# trainControl("cv", 
#              number = 10)

modelo <- caret::train(
  Survived ~., 
  data = treino, 
  method = "xgbTree",
  trControl = controle,
  tuneGrid = NULL,
  verbosity = 0)

modelo

tempo_fim <- Sys.time()
tempo_fim - tempo_ini

###################################
# Avaliar o XGBoosting            #
avalia(modelo, nome_modelo="XGBoosting")
