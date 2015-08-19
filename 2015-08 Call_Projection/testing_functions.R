
## Testing functions

rmse <- function(obs, pred) {sqrt(sum((obs - pred)^2, na.rm=T) / length(obs))}
mnAD <- function(obs, pred) {mean(abs(obs - pred), na.rm=T)}
wmnAD <- function(obs, pred) {
  wts <- log(abs(pred) + 2)
  return(1/ sum(wts, na.rm=T) * sum(wts * abs(obs - pred), na.rm=T))
}
mxAD <- function(obs, pred) {max(abs(obs - pred), na.rm=T)}


compute_acc <- function(projections.list, actuals.list) {
  p <- length(projections.list)
  
  acc.list <- replicate(6, matrix(NA, nrow= p, ncol= 4, dimnames= list(
    names(actuals.list), c("rmse", "mnAD", "wmnAD", "mxAD"))))
  dimnames(acc.list)[[3]] <- c("mkt_direct", "db_ivr", "dandb.com", "organic", "paid_etc", "iupdate")
  
  # don't keep the model type info
  projections.list <- lapply(projections.list, function(x) {x[[1]]})
  
  for (j in 1:p) { # j == months
    # mkt direct
    acc.list[j,,1] <- c(rmse(actuals.list[[j]]$act_mkt, projections.list[[j]]$mkt_direct),
                        mnAD(actuals.list[[j]]$act_mkt, projections.list[[j]]$mkt_direct),
                        wmnAD(actuals.list[[j]]$act_mkt, projections.list[[j]]$mkt_direct),
                        mxAD(actuals.list[[j]]$act_mkt, projections.list[[j]]$mkt_direct))
    # db_ivr
    acc.list[j,,2] <- c(rmse(actuals.list[[j]]$act_dbivr, projections.list[[j]]$db_ivr),
                        mnAD(actuals.list[[j]]$act_dbivr, projections.list[[j]]$db_ivr),
                        wmnAD(actuals.list[[j]]$act_dbivr, projections.list[[j]]$db_ivr),
                        mxAD(actuals.list[[j]]$act_dbivr, projections.list[[j]]$db_ivr))
    # dandb.com
    acc.list[j,,3] <- c(rmse(actuals.list[[j]]$act_dandb, projections.list[[j]]$dandb.com),
                        mnAD(actuals.list[[j]]$act_dandb, projections.list[[j]]$dandb.com),
                        wmnAD(actuals.list[[j]]$act_dandb, projections.list[[j]]$dandb.com),
                        mxAD(actuals.list[[j]]$act_dandb, projections.list[[j]]$dandb.com))
    
    # organic
    acc.list[j,,4] <- c(rmse(actuals.list[[j]]$act_org, projections.list[[j]]$organic),
                        mnAD(actuals.list[[j]]$act_org, projections.list[[j]]$organic),
                        wmnAD(actuals.list[[j]]$act_org, projections.list[[j]]$organic),
                        mxAD(actuals.list[[j]]$act_org, projections.list[[j]]$organic))
    
    # paid_etc
    acc.list[j,,5] <- c(rmse(actuals.list[[j]]$act_paid, projections.list[[j]]$paid_etc),
                        mnAD(actuals.list[[j]]$act_paid, projections.list[[j]]$paid_etc),
                        wmnAD(actuals.list[[j]]$act_paid, projections.list[[j]]$paid_etc),
                        mxAD(actuals.list[[j]]$act_paid, projections.list[[j]]$paid_etc))
    
    # iupdate
    acc.list[j,,6] <- c(rmse(actuals.list[[j]]$act_iupdate, projections.list[[j]]$iupdate),
                        mnAD(actuals.list[[j]]$act_iupdate, projections.list[[j]]$iupdate),
                        wmnAD(actuals.list[[j]]$act_iupdate, projections.list[[j]]$iupdate),
                        mxAD(actuals.list[[j]]$act_iupdate, projections.list[[j]]$iupdate))
  }
  
  return(acc.list)
}

extract_acc <- function(acc_list, name) {
  p <- length(acc_list)
  
  mat <- matrix(NA, nrow= length(acc_list), ncol= 3)
  colnames(mat) <- c("avg_rmse", "avg_mnAD", "avg_mxAD")
  
  for (i in 1:p) {
    mat[i,] <- apply(acc_list[[i]][,-3, name], 2, mean, na.rm=TRUE)
  }
  return(mat)
}

extract_acc_wrap <- function(all_acc_lists, name) {
  mat <- do.call("rbind", lapply(all_acc_lists, extract_acc, name= name))
  return(mat)
}

extract_acc_2xwrap <- function(all_acc_lists, name_vec) {
  len <- length(name_vec)
  l <- vector(mode= "list", length= len)
  for (j in 1:len) {
    l[[j]] <- extract_acc_wrap(all_acc_lists, name= name_vec[j])
  }
  names(l) <- name_vec
  return(l)
}