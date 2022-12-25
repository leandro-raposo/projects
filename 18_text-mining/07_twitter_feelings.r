library("readxl")
library("tm")
library("wordcloud")
library("e1071")
library("gmodels")
library("SnowballC")
library("caret")
library("dplyr")

# Pegando os dados
twitter <- read.csv2("twitter_validation.csv",sep = ",",header = FALSE)

twitter <- twitter %>% select(V3,V4) %>% filter(V3 == c('Positive','Negative'))

colnames(twitter) <- c("sentiment","text")


#Data Cleaning
#Train set
corpus = VCorpus(VectorSource(twitter$text)) 
corpus = tm_map(corpus, content_transformer(tolower)) 
corpus = tm_map(corpus, removeNumbers) 
corpus = tm_map(corpus, removePunctuation) 
corpus = tm_map(corpus, removeWords, stopwords("english")) 
corpus = tm_map(corpus, stemDocument) 
corpus = tm_map(corpus, stripWhitespace) 
as.character(corpus[[1]])

#Document Term Matrix - explicar conceito
dtm = DocumentTermMatrix(corpus) 
inspect(dtm) 
dim(dtm) 
dtm = removeSparseTerms(dtm, 0.999) 
inspect(dtm)

convert <- function(x) 
  {
y <- ifelse(x > 0, 1,0)
y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
y
  }  
    
datanaive = apply(dtm, 2, convert)

dataset = as.data.frame(as.matrix(datanaive))    
dataset$Class = factor(twitter$sentiment)
```

#Modelagem
#Train test Split

set.seed(31)
split = sample(2,nrow(dataset),prob = c(0.75,0.25),replace = TRUE)
train_set = dataset[split == 1,]
test_set = dataset[split == 2,] 
    
prop.table(table(train_set$Class))
prop.table(table(test_set$Class))

classifier_nb <- naiveBayes(train_set[1:1439], train_set$Class)

#Predição do test set
nb_pred = predict(classifier_nb, type = 'class', newdata =  test_set[1:1439])
confusionMatrix(nb_pred,test_set$Class)
