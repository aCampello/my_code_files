################################################################################################################
## Database Connection Handler
##
## This script abstracts the database connection to Redshift and provides some additional capabilities
## It also handles authentication in a Git-friendly way (no passwords will be pushed to Git)
## All functions in this file will be created in the "db." namespace, "db.int." are internal functions
##
##
##
## To set-up the authenication, create .Rprofile file by typing  `file.edit("~/.Rprofile")`
## Enter the following details in this file and replace <<<>>> with your information
##    Sys.setenv(USER='<<<YOUR REDSHIFT USERNAME>>>')
##    Sys.setenv(PW='<<<YOUR REDSHIFT PASSWORD>>>')
##
## 
################################################################################################################

# Initialize the script
# -------------------------------------------------------------
# 1. Load / Install the packages
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
if (!is.installed("progress")){ install.packages("progress") }
if (!is.installed("RPostgreSQL")){ install.packages("RPostgreSQL") }

library(RPostgreSQL)
library(progress)

# -------------------------------------------------------------
# 2. Initalize the config

# Load the driver
db.drv <- dbDriver("PostgreSQL")

# Standard users to grant access when you create new tables (you can specific the names explicitly - for the lazy ones ;)
db.users_grant_access <- c("user1", "user2", "user3") ## example from phase 1 - "obauer", "mbuecker", "mjodlowski"


################################################################################################################
## Functions to abstract the database connection
# -------------------------------------------------------------
# 1. Connect to the Database
db.connect <- function(){
  if(!( exists("db.con") && isPostgresqlIdCurrent(db.con) )) {
    db.con <<- dbConnect(db.drv, host="cloud-dwh-analysis.cede9zhbujyu.eu-west-1.redshift.amazonaws.com", 
                         port="5439",
                         dbname="phxred", 
                         user=Sys.getenv("USER"),      # See above for config
                         password=Sys.getenv("PW")     # See above for config
                         #,memory.limit()
                         #ssl="true",
                         #sslfactory="com.amazon.redshift.ssl.NonValidatingFactory"
    )
  }
  
  return(db.con)
}

# -------------------------------------------------------------
# 2. Run Get Query
# sql: sql to run
db.query <- function(sql) {
  dbGetQuery(db.connect(), sql)
}

# -------------------------------------------------------------
# 3. Run Send Query
# sql: sql to run
db.sendQuery <- function(sql) {
  dbSendQuery(db.connect(), sql)
}

# -------------------------------------------------------------
# 4. Disconnect
db.disconnect <- function() {
  dbDisconnect(db.con)  
}

# -------------------------------------------------------------
# 5. Grant Access to users
# table_name: target_table in format schma.tablename 
# users: list of users in format c("abc", "bcd") 
db.grantAccess <- function(table_name, users=db.users_grant_access) {
  for (i in users) {    
    #optional ToDo speed up with one query for all users
    db.sendQuery(paste0("GRANT SELECT, INSERT, UPDATE ON ", table_name, " TO ", i , ";"))
  }
}

# -------------------------------------------------------------
# 6. Buld upload of data to RedShift 
# data: dataframe to upload
# table_name: target_table in format schma.tablename
# rows_per_insert: number of rows that are pushed to the database in one batch
# delete: if the existing table shall be deleted (if not, plz make sure that the table definitions are the same)
# In case the upload is too slow, increase the "rows_per_insert" value (writing one query takes app. the  same time)
db.writeBulk <- function(data, table_name, rows_per_insert=200000L, delete=TRUE) {
  flush.console()
  print("Bulk writing started!")
  if(delete) { 
    print("Delete old table, if it exists...")
    db.sendQuery(paste0("DROP TABLE IF EXISTS ",table_name, ";")) 
    print("done")
    print("(Re)create the schema....")
    db.int.createEmptyTable(table_name,data);
    print("done")
    print("Granting access to all users....")
    db.grantAccess(table_name)
    print("done")
  }
  print("Start writing the data...")
  db.int.insertBatch(table_name,data, rows_per_insert);
  print("Bulk writing finished")
}




################################################################################################################
## Internal Helper Functions
# -------------------------------------------------------------
# 1. Set the schema (not used right now)
db.int.setMainSchema <- function(schema) {
  dbGetQuery(db.connect(), paste0("SET search_path TO ", schema))
}

# -------------------------------------------------------------
# 2 Create an empty table based on the types of a specific dataframe
db.int.createEmptyTable <- function(table_name,data) {
  sql <- paste0("CREATE TABLE ",table_name," (",paste0(collapse=',','"',names(data),'" ',sapply(data[0,],postgresqlDataType)),");");
  db.sendQuery(sql);
  invisible();
}

# -------------------------------------------------------------
# 3 Push to Redshift in Batch
db.int.insertBatch <- function(table_name,data,size) {
  flush.console()
  if (nrow(data)==0L) return(invisible());
  cnt <- (nrow(data)-1L)%/%size+1L;
  pb <- progress_bar$new(format = "[:bar] :percent eta: :eta", total = cnt, clear = FALSE, width= 60, show_after=0)
  for (i in seq(0L,len=cnt)) {
    
    sql <- paste0("INSERT INTO ",table_name," VALUES (",do.call(paste,c(sep=',',collapse='),(',lapply(data[seq(i*size+1L,min(nrow(data),(i+1L)*size)),],db.int.shQuoteNull))),");");
    db.sendQuery(sql);
    pb$tick()
  }
  invisible()
}

# -------------------------------------------------------------
# 4 Transform strings for Redshift

db.int.shQuoteNull <- function(value) {
  return(ifelse(is.na(value), 'NULL', shQuote(value)))
}

# Select which libraries are needed for the program
packages <- c("odbc", "DBI", "data.table", "arules", "reshape", "parallel", "stringr", "tidyr", "bit64")

# Install required libraries if necessary
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
# Load the libraries
lapply(packages, require, character.only = TRUE)

# save data as table in Redshift
save_to_db <- function(results, dest){
  db.sendQuery(paste0("DROP TABLE IF EXISTS ", dest))
  db.int.createEmptyTable(dest,results);
  db.grantAccess(dest)
  db.int.insertBatch(dest,results, 200000L);
}
