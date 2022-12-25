library("gutenbergr")
library("dplyr")
library("tidytext")
library("wordcloud")
library("stringr")
library("SnowballC")
library("widyr")
library("janeaustenr")

textos <- gutenberg_metadata

#Vamos usar última ceia do dr fausto
livro <- gutenberg_download(c(35982))

#Mudamos o encoding para ver os acentos
#Discutir o que é encoding: https://blog.caelum.com.br/entendendo-unicode-e-os-character-encodings/amp/
Encoding(livro$text) <- "ASCII"

#vamos retirar os acentos
for (i in 1:nrow(livro))
{
  livro$text[i] <- iconv(livro$text[i], to = "ASCII//TRANSLIT")
}

#Unnest tokens - vamos fazer n gramas de 2 = bigrama
livro <- livro %>%  unnest_tokens(word, text, token = "ngrams", n = 2) 

#VAMOS CONTAR
cont <- livro %>% count(word, sort = TRUE)

#Correlação entre palavras - widyr
austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)
