library("gutenbergr")
library("dplyr")
library("tidytext")
library("wordcloud")
library("stringr")
library("SnowballC")

textos <- gutenberg_metadata

#Vamos usar perter pan e Moby Dick
livros <- gutenberg_download(c(15,16))

#Vamos retirar números - pode ser qualquer coisa
nums <- livros %>% filter(str_detect(text, "^[0-9]")) %>% select(text) 

livros <- livros %>%  anti_join(nums, by = "text")

#Vamos nos livrar das stop words e obter os tokens
livros <- livros %>%  unnest_tokens(word, text) %>%  anti_join(stop_words)

#Contar as palavras mais comuns por obra

#Moby Dick
moby <- livros %>% filter(gutenberg_id == 15) %>% count(word, sort = TRUE)

# define a paleta de cores
pal <- brewer.pal(8,"Dark2")

# word cloud
moby %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))

#Peter Pan
peter <- livros %>% filter(gutenberg_id == 16) %>% count(word, sort = TRUE)

# define a paleta de cores
pal <- brewer.pal(8,"Dark2")

# word cloud
peter %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))

#Stemming - vamos aplicar 
peter_stem <-  peter %>%  mutate(stem = wordStem(word)) 

peter_stem_count <- livros %>% filter(gutenberg_id == 16) %>% mutate(stem = wordStem(word)) %>% count(stem, sort = TRUE)
