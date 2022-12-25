#Iniciamos explicando tidytext

#Pacotes
library("dplyr")
library("tidytext")
library("ggplot2")
library("tibble")

st <- starwars

#Pequena introducao ao dplyr
filtro <- starwars %>%  filter(species == "Droid")

select <- starwars %>%  select(name, ends_with("color"))

imc <- starwars %>%   mutate(name, altura = height / 100) 

#Texto de Edgar Allan Poe - the black cat
text <- c("From my infancy I was noted for the docility and humanity of my disposition.",
      "My tenderness of heart was even so conspicuous as to make me the jest of my companions.",
      "There is something in the unselfish and self-sacrificing love of a brute, which goes directly to the heart of him who has had frequent occasion to test the paltry friendship and gossamer fidelity of mere Man.")

text

#Uso do formato tibble e explicacao
text_df <- tibble(line = 1:3, text = text)

#Explicao de token
#Abre funcao e explica unnest_tokens
#?unnest_tokens
df <-  text_df %>% unnest_tokens(word, text)

#Ja convertido em minuscula e tira pontuacao

#Stop Words:
stop_words

df_sem_stop_words <- df %>% anti_join(stop_words)

#Busca palavras mais comuns
conta <- df_sem_stop_words %>%  count(word, sort = TRUE) 


#grafico

#Para facilitar a visualização
df_sem_stop_words %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() 
