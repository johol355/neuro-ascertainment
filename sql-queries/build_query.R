# An R-script to create the function build_query()

# Usage:
# Two arguments are passed to the function build_query(). 

# 1. A vector of paths to the .sql-files to be read. This can be created
# by using for example:
# dirs <- c('path1','path2','path3',...) 
# and then passing the object dirs to 
# the function build_query. 

# 2. A string with the final SELECT-clause to query the database with.

build_query <- function(paths, final_sql_query){
  paste(paste(purrr::map(paths, ~readr::read_file(.)), collapse = ", "), final_sql_query)
}