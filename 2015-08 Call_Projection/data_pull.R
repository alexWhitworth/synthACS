#' @title Pull call data
#' @description Pull in inbound five9id call data from the appropriate databases / daily dump
#' @param channel A character string corresponding to the appropriate ODBC connection. Defaults to "c2g"
#' @param call_date A character string specifying subset \code{where call_date >= date}. Defaults to '1/01/2014'
#' @return a \code{data.frame} with call data
#' @export
pull_call_data <- function(channel= "c2g", call_date= '1/01/2014') {
  
  #Load 'RODBC'
  require(RODBC)
  require(data.table)
  ch <- odbcConnect(channel)
  
  query_txt <- paste("select ltrim(rtrim(category)) category, DNIS, campaign, TFN, five9did, 
    case when (call_date >=start_date or start_date is null) then description else 'Unassigned' end as 'Description',
    start_date, end_date, month(call_date)as month, call_date, sum(case when ((end_date is not NULL
    and call_date >=start_date and call_date <= end_date) OR (start_date is not null
    and call_date >= start_date and end_date is NULL) OR (end_date is null and start_date is null)
    OR (call_date <= end_date and start_date is null  and end_date is not null))
    AND ((campaign like 'IB - 800%' and SKILL like 'IB_Organic') OR (campaign not like 'IB - 800%'))
    then cast(calls as int) else 0 end) as mktg_call_count from sbs_cis_rpt2.dbo.CB_Daily_Call_Log_Data_Dump cd with(nolock)
    left join bi_sandbox.dbo.ib_ph_donotdelete ib with(nolock) on (cd.dnis = ib.tfn or cd.dnis = ib.five9did)
    where call_date >= '", call_date, "' and Call_Type = 'Inbound' and ANI not in 
    ('6107587400','5205842800','5205124901','8006273867')
    group by ltrim(rtrim(category)), DNIS, campaign, TFN, five9did, 
    case when (call_date >=start_date or start_date is null) then description else 'Unassigned' end , start_date, end_date,
    month(call_date), call_date order by TFN")
  
  called <- sqlQuery(ch, query_txt, stringsAsFactors= FALSE)
  close(ch)
  
  # Order calls by date and return
  called <- data.table(called)[order(call_date)]
  return(called)
}