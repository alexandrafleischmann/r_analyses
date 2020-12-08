secret = xxxx

install.packages('RPostgres')
install.packages('DBI')


library(DBI)
library(RPostgres)



con <- dbConnect(RPostgres::Postgres(),
                 host = 'localhost',
                 port = 5432,
                 dbname = 'dvddatabase',
                 user = 'postgres',
                 password = secret
)


dbListTables(con)

df_address <- dbGetQuery(con, 'SELECT * FROM address')
head(df_address)
