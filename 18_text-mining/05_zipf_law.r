library("gutenbergr")
library("dplyr")
library("tidytext")
library("ggplot2")

textos <- gutenberg_metadata

#Vamos usar Othello e Contos do Norte
livros <- gutenberg_download(c(28526,28691))

#Unnest tokes para análise
book_words <- livros %>%
  unnest_tokens(word, text) %>%
  count(gutenberg_id, word, sort = TRUE)

#Avalia Lei de ZIPF 
#A lei de Zipf afirma que a frequência com que uma palavra aparece é inversamente proporcional a sua classificação.

#TF-IDF
#Não vamos pré-processar, a ideia é mostrar papel do tf-idf
books_tf_idf <- book_words %>% bind_tf_idf(word, gutenberg_id, n)

#Separa para ver palavras mais importantes por texto
othello_tf_idf <- books_tf_idf %>% filter(gutenberg_id == 28526)
contos_tf_idf <- books_tf_idf %>% filter(gutenberg_id == 28691)

#Realiza gráfico com palavras mais importantes
books_graph <- books_tf_idf %>% 
  group_by(gutenberg_id) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) 

books_graph %>% ggplot(aes(tf_idf, word, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free")
