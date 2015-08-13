#Final call projection script. Auomatically called weekly (Monday morning) by Windows Task Scheduler
#Bring in 'RODBC'
library(RODBC)

#Open channel to server
ch <- odbcConnect("phoenix1")

#First, call function which pulls all the data and runs the model to produce forecasts
load("/Users/ashelton/Documents/The Call Projection Model/R_workspace_FULL_call_projection_model_all_functions")

#Query calls for holiday adjustments
calls_holiday <- call_query_all_calls()

#Get All holiday dates to merge into call projection data

x <- weekly_call_projection_full_all_calls()

y <- write_calls_to_server()

save.image("/Users/ashelton/Documents/The Call Projection Model/last_model_run")

#Done!