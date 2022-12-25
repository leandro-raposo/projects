library("gutenbergr")
library("dplyr")
library("tidytext")
library("wordcloud")
library("stringr")
library("SnowballC")

textos <- gutenberg_metadata

textos$language == 'pt'

#Vamos usar os netos de camilo
livro <- gutenberg_download(c(33752))

#Mudamos o encoding para ver os acentos
#Discutir o que é encoding: https://blog.caelum.com.br/entendendo-unicode-e-os-character-encodings/amp/
Encoding(livro$text) <- "ASCII"

#vamos retirar os acentos
for (i in 1:nrow(livro))
{
  livro$text[i] <- iconv(livro$text[i], to = "ASCII//TRANSLIT")
}

#Unnest tokens
livro <- livro %>%  unnest_tokens(word, text) 

#Excluímos stop words em português - instalar stop words
livro <- livro %>% anti_join(get_stopwords(language = 'pt'))

#Word cloud - teste word e stem
livro_cont <- livro %>% select(word) %>% count(word, sort = TRUE)

pal <- brewer.pal(8,"Dark2")
livro_cont %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))
