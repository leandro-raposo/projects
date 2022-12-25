#Pacotes utilizados
pacotes <- c("tidytext","ggplot2","dplyr","tibble","gutenbergr","wordcloud",
             "stringr","SnowballC","widyr","janeaustenr","lexiconPT",
             "tidyr","readxl","tm","e1071","gmodels","caret","reshape2",
             "stopwords")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
