# Get Oracle data from R using an existing SQL file
#
# Query data from Oracle with the Query in a SQL file(with formatting and inline comments intact)
#
#
#

# Load required libraries
packages_used = c("readxl","readr","dbplyr","infuser","RJDBC","ROracle","RODBC")
invisible(lapply(packages_used, require, character.only = TRUE))

#' Cleans-up text
#'
#' removes whitespaces, tabs, comments, formatting
#' @param x Line containing a string
#'
#' @return text without comments, whitespace, tabs
#'
#' @export
#'
#' @examples
#' LINECLEAN('The text after the comments will be removed --comment')
#'
LINECLEAN <- function(x) {
  x = gsub("\t+", " ", x, perl=TRUE); # remove all tabs
  x = gsub("^\\s+", "", x, perl=TRUE); # remove leading whitespace
  x = gsub("\\s+$", "", x, perl=TRUE); # remove trailing whitespace
  x = gsub("[ ]+", " ", x, perl=TRUE); # collapse multiple spaces to a single space
  x = gsub("[-]{2}.*$", "", x, perl=TRUE); # remove any comments
  x = gsub(";", "", x, perl=TRUE); # remove any semicolons
  x = gsub(":([A-Za-z_]+)","{{\\1}}",x, perl=TRUE) # replace sql query parameter ":parameter" to "{{parameter}}" (to use infuser to pass values - better readability)
  return(x)
}

#' Convert several lines of text to a single line
#'
#' @param PRETTYQUERY SQL file
#'
#' @return returns a single line of text
#'
#' @export
#'
#' @examples
#' ONELINEQ('C:/Users/CountQuery.SQL')
#'
ONELINEQ <- function(PRETTYQUERY) {
  A <- readLines(PRETTYQUERY, encoding = "UTF-8",warn = FALSE) # read in the query to a list of lines
  B <- lapply(A,LINECLEAN) # process each line
  C <- Filter(function(x) x != "",B) # remove blank and/or comment lines
  D <- paste(unlist(C),collapse=" ") # paste lines together into one-line string, spaces between.
  return(D)
}


#' Query with SQL file
#'
#' Query data from Oracle with the Query in a SQL file(with formatting and inline comments intact)
#'
#' @param DB_Host Host name of the Database
#' @param DB_Port Port id. By default the function takes 1521
#' @param DB_SID  SID of the database
#' @param Userid User id
#' @param Pwd Password
#' @param Queryfile SQL file with the Query and the inline comments
#' @param Query_args List with the values to be replaced for the parameter in SQL file
#'
#' @return Query result
#' @export
#'
#' @examples
#' Osqldata('DB_Host='hostname',DB_Port='1521',DB_SID='database',uid='userid',Pwd='password',
#' Queryfile='C:/Users/CountQuery.SQL')
#'
#'
Odata <- function (DB_Host,DB_Port=1521,DB_SID,Userid,Pwd,Queryfile,Query_args = NULL) {
  my_sql = ONELINEQ(Queryfile)
  # Make connection
  drv = dbDriver("Oracle")
  #DBname = "(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(HOST=DB_Host)(PORT=DB_Port))(CONNECT_DATA=(SID=DB_SID)))"
  DBname = paste0("(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(HOST=",
                  DB_Host,
                  ")(PORT=",
                  DB_Port,
                  "))(CONNECT_DATA=(SID=",
                  DB_SID,
                  ")))")
  con = dbConnect(drv, Userid, Pwd, dbname = DBname)
  # print(my_sql)
  # #my_sql = infuse(my_sql, Query_args, transform_function = dbplyr::build_sql,collapse_char = ",")  # build_sql function helps to escape characters
  # my_sql = infuse(my_sql, Query_args)  # build_sql function helps to escape characters

  params_supplied <- Query_args
  params_requested <- infuser::variables_requested(my_sql)
  variable_identifier=c("{{", "}}")
  default_char = "|"

  for(param in names(params_requested)){

    pattern <- paste0(variable_identifier[1],
                      "\\s*?",
                      param,
                      "\\s*?" ,
                      variable_identifier[2],
                      "|",  # or match with default in place
                      variable_identifier[1],
                      "\\s*?",
                      param,
                      "\\s*?\\",
                      default_char,
                      ".*?",
                      variable_identifier[2])

    if(param %in% names(params_supplied)){
      my_sql <- gsub(pattern,
                     gsub("\\(|\\)","",dbplyr::build_sql(params_supplied[[param]])),
                     my_sql,perl = TRUE)}
  }

  message("Query running ...")
  queryresult = dbGetQuery(con, my_sql)
  dbDisconnect(con)
  queryresult
}
