install.packages("rtweet")
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr") # alternative installation of the %>%
install.packages("ggplot2")
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr) # alternatively, this also loads %>%
library(rtweet)
library(ggplot2)

df_favorite<- rtweet::get_favorites("@raposo_le", n=3000)
users_favorite =
df_favorite %>% 
group_by(screen_name, user_id) %>%
summarise(
quant_fav= n()
)
users_favorite %>%
ungroup() %>%
top_n(15,quant_fav) %>%
mutate(screen_name = reorder(screen_name,quant_fav)) %>%
ggplot() + 
geom_col(aes(x= screen_name, y= quant_fav)) +
theme_light() +
coord_flip() +
labs(
title = "Gráfico da galera que eu mais favorito no twitter",
y = "Número de posts favoritados",
x= "Screen name da galera" )
