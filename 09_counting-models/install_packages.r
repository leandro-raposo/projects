pacotes <- c("plotly","tidyverse","knitr","kableExtra","reshape2","ggrepel",
             "fastDummies","lmtest","splines","jtools","questionr","MASS",
             "pscl","overdisp","magick","cowplot","beepr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
