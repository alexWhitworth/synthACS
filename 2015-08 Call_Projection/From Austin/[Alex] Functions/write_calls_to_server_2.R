write_calls_to_server_2 <- function(data) {
  
  ch <- odbcConnect("phoenix_new")
  
  nrows <- nrow(data)
  
  a <-"INSERT INTO [BI_Sandbox].[dbo].[call_projection] (Year, Month, call_date, Holiday, mktg_direct_projection, customer_service_projection,
  DB_IVR_projection, DB_com_projection, Direct_Extension_projection, iUpdate_projection, Organic_projection, Paid_web_projection,
  other_projection, total_projection, mktg_direct, customer_service, DB_IVR, DB_com, Direct_Extension, iUpdate, Organic, Paid_web,
  other, all_calls, day_of_week, Error, Day, sd_Error, Average_Miss)
  VALUES ("
  for(i in 1:nrows) { 
    sqlQuery(ch, paste(a, "'", data$Year[i], "'",",", " ", "'", data$Month[i], "'", ",", " ", "'", data$call_date[i], 
                       "'",",", " ", "'", data$Holiday[i], 
                       "'",",", " ", "'", data$mktg_direct_projection[i], 
                       "'",",", " ", "'", data$customer_service_projection[i], 
                       "'",",", " ", "'", data$DB_IVR_projection[i], 
                       "'",",", " ", "'", data$DB_com_projection[i], 
                       "'",",", " ", "'", data$Direct_Extension_projection[i], 
                       "'",",", " ", "'", data$iUpdate_projection[i], 
                       "'",",", " ", "'", data$Organic_projection[i], 
                       "'",",", " ", "'", data$Paid_web_projection[i], 
                       "'",",", " ", "'", data$other_projection[i],    
                       "'",",", " ", "'", data$total_projection[i],    
                       "'",",", " ", "'", data$mktg_direct[i], 
                       "'",",", " ", "'", data$customer_service[i], 
                       "'",",", " ", "'", data$DB_IVR[i], 
                       "'",",", " ", "'", data$DB_com[i], 
                       "'",",", " ", "'", data$Direct_Extension[i], 
                       "'",",", " ", "'", data$iUpdate[i], 
                       "'",",", " ", "'", data$Organic[i], 
                       "'",",", " ", "'", data$Paid_web[i], 
                       "'",",", " ", "'", data$other[i],    
                       "'",",", " ", "'", data$all_calls[i], 
                       "'",",", " ", "'", data$day_of_week[i],
                       "'",",", " ", "'", data$Error[i],
                       "'",",", " ", "'", data$Day[i],
                       "'",",", " ", "'", data$sd_Error[i],
                       "'",",", " ", "'", data$Average_Miss[i],
                       
                       "'", ")"))
  }
  
  full_call_projections <- data    
  
  output <- list("full_call_projections" = full_call_projections, "nrows" = nrows)
  return(output)