
#Pacotes
library("DBI")
library("odbc")
library("dplyr")
library("dbplyr")
library("RMySQL")

# Realiza uma conexão com o MySql
conn <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "world",
  host = "localhost",
  username = "root",
  password = "Mysql_2022") #seu password

#Primeira forma de obter dataframe com consulta
rs <- dbSendQuery(conn, "SELECT GNP,LifeExpectancy FROM world.country WHERE IndepYear IS NOT NULL;")
dataframe<-dbFetch(rs, n = -1)

#testes no dataframe com dplyr
DB_TEST <- dataframe %>% select(GNP)

DB_TEST2 <- dataframe %>% filter(LifeExpectancy > 50)

DB_TEST3 <- dataframe %>% summarise(media = mean(LifeExpectancy, na.rm = TRUE))

#Segunda forma de obter dataframe com consulta
dbListTables(conn)
table1 <- tbl(conn, "country") 
table1

table1 %>% select(GNP,LifeExpectancy)

q1 <- table1 %>% select(GNP,LifeExpectancy)

show_query(q1)

table1 %>% select(GNP,LifeExpectancy) %>% filter(LifeExpectancy > 50)

table2 <- table1 %>% select(GNP,LifeExpectancy) %>% filter(LifeExpectancy > 50) %>% collect()
