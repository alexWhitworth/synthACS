write_calls_to_server <- function() {   
  #Create 'RODBC'
  library(RODBC)
  
  ch <- odbcConnect("phoenix1")
  
  old_table <- sqlQuery(ch, "SELECT * FROM bi_sandbox.dbo.call_projections")
  
  sqlQuery(ch, "DROP TABLE bi_sandbox.dbo.call_projections")
  
  sqlQuery(ch, "CREATE TABLE bi_sandbox.dbo.call_projections (Year varchar(20), Month varchar(20), response_date varchar(20), 
         Called Varchar(20), projected Varchar(20), Holiday varchar(20), Error varchar(20), RMSE_monthly varchar(20))")        
  
  a <-"INSERT INTO bi_sandbox.dbo.call_projections (Year, Month, response_date, Called, projected, Holiday, Error, RMSE_monthly) VALUES ("
  for(i in 1:nrow(x$full_projections)) { 
    sqlQuery(ch, paste(a, "'", x$full_projections[i, 1], "'",",", " ", "'", x$full_projections[i, 2], "'", ",",
                       "'", x$full_projections[i, 3], "'", ",", "'", x$full_projections[i, 4], "'", ",",
                       "'", x$full_projections[i, 5], "'", ",", "'", x$full_projections[i, 6], "'", ",",
                       "'", x$full_projections[i, 7], "'", ",", "'", x$full_projections[i, 8], "'", ")"))
  }
  
  new_table <- sqlQuery(ch, "SELECT * FROM bi_sandbox.dbo.call_projections")
  
  rm(ch)
  
  output <- list("old_table" = old_table, "new_table" = new_table)
  return(output)   
  
}