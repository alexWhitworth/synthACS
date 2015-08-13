call_query_all <- function() {
  
  #Load 'RODBC'
  library(RODBC)
  
  #Setup a connection to phoenix
  ch <- odbcConnect("c2g")
  
  #Call query
  called <- sqlQuery(ch, "select ltrim(rtrim(category)) category,
                     DNIS,
                     campaign,
                     TFN,
                     five9did,
                     case when (call_date >=start_date
                     or start_date is null)
                     then description
                     else 'Unassigned'
                     end as 'Description',
                     start_date,
                     end_date,
                     month(call_date)as month,
                     call_date,
                     sum(case when ((end_date is not NULL
                     and call_date >=start_date
                     and call_date <= end_date)
                     OR (start_date is not null
                     and call_date >= start_date
                     and end_date is NULL)
                     OR (end_date is null
                     and start_date is null)
                     OR (call_date <= end_date
                     and start_date is null
                     and end_date is not null)
                     )
                     AND ((campaign like 'IB - 800%'
                     and SKILL like 'IB_Organic')
                     OR (campaign not like 'IB - 800%')
                     )
                     then cast(calls as int)
                     else 0
                     end) as mktg_call_count
                     from sbs_cis_rpt2.dbo.CB_Daily_Call_Log_Data_Dump cd with(nolock)
                     left join bi_sandbox.dbo.ib_ph_donotdelete ib with(nolock)
                     on (cd.dnis = ib.tfn or cd.dnis = ib.five9did)
                     where call_date >='1/01/2008'
                     and Call_Type = 'Inbound'
                     and ANI not in ('6107587400','5205842800','5205124901','8006273867')
                     and CATEGORY = 'Marketing Direct'
                     group by ltrim(rtrim(category)),
                     DNIS,
                     campaign,
                     TFN,
                     five9did,
                     case when (call_date >=start_date
                     or start_date is null)
                     then description
                     else 'Unassigned'
                     end ,
                     start_date,
                     end_date,
                     month(call_date),
                     call_date
                     order by TFN")
    
    #Order calls by date
    calls <- called[order(called$call_date), ]
    
    #Next, aggregate for called by date
    #calls_daily <- aggregate(called$mktg_call_count, by=list(called$call_date), FUN=sum, na.rm="TRUE")
    
    
    return(list("called" = called))
    
}