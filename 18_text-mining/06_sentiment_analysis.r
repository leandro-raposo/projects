library("gutenbergr")
library("dplyr")
library("tidytext")
library("ggplot2")
library("lexiconPT")
library("janeaustenr")
library("stringr")
library("tidyr")
library("wordcloud")

#Tipos de sentimentos
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#Forma de análise de sentimentos - inner join
#Conceito de Join - caso inner join
tidy_books <- austen_books() %>%  
  group_by(book) %>%
  unnest_tokens(word, text)

#Palavras mais comuns de prazer para análise de sentimentos
nrc_joy <- get_sentiments("nrc") %>%  filter(sentiment == "joy")

#Realizar inner join com o livro EMMA para entender sentimentos
book <- tidy_books %>%  filter(book == "Emma") %>%  inner_join(nrc_joy) %>%  count(word, sort = TRUE)

#usando o sistema bing para calcular o sentimento em polaridade
jane_austen_sentiment <- tidy_books %>% inner_join(get_sentiments("bing"))

persuasion <- jane_austen_sentiment %>% filter(book == "Persuasion")

persuasion <- persuasion %>% mutate(
    net_sentiment = ifelse(sentiment=="negative",-1,1))

sum(persuasion$net_sentiment)
  
#word cloud
library("reshape2")

mansfield <- tidy_books %>% filter(book == "Mansfield Park")

mansfield %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   scale=c(1,.5),
                   max.words = 100)
