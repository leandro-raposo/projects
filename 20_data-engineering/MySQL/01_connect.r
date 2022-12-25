library("DBI")
library("odbc")
library("RMySQL")

#Verifica os driver odbc padrão
sort(unique(odbcListDrivers()[[1]]))

# Realiza uma conexão com o MySql
conn <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "world",
  host = "localhost",
  username = "root",
  password = "Mysql_2022") #seu password

#Métodos
#1 - lista Dbs
dbListTables(conn)
#2 - lista campos
dbListFields(conn,"city")
#3 - Read table
dbReadTable(conn,"city")
#3.1 - atribui dataframe
db <- dbReadTable(conn,"city")
#4 - segunda forma de obter dataframe com consulta
rs <- dbSendQuery(conn, "SELECT * FROM City;")
dataframe<-dbFetch(rs, n = -1)

str(dataframe)

#LIMPA RESULTADOS
dbClearResult(rs)

#terminou o trabalho?
dbDisconnect(conn)
