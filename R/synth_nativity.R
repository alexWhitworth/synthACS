
# @param agmee_dat a \code{list} equivalent to the output of \code{synth_data_emp}
# @param nativity_vec A vector containing counts of the total population 
# as well as counts disagregating by age and place of birth.
# should be: \code{unlist(<synth_data>$estimates$nativity[<row i>,])}
synth_data_nativ <- function(agmee_dat, nativity_vec) {
  dat <- agmee_dat[[1]]
  nativity_vec <- nativity_vec[-c(2:10)] # remove age bracket overall counts
  
  # 1. create hash table of age/gender ages to employment status ages
  age_ht <- data.frame(age= agmee_dat[[2]],
              nativity= c(rep("u18",2), "18_24", rep("25_34", 2), rep("35_44", 2), rep("45_54", 2),
                          "55_59", "60_64", rep("65_74", 2), rep("75up", 3)), stringsAsFactors = FALSE)
  # 2. create age buckets on which to condition
  ag_list <- split(dat, dat$age)
  
  # 3. Apply nativity status
  nat_levels <- c("born_state_residence", "born_other_state", "born_out_us", "foreigner")
  
  ag_list <- do.call("rbind", lapply(ag_list, nat_lapply, ht= age_ht, 
                                 v= nativity_vec, levels= nat_levels))
  
  ag_list <- factor_return(ag_list, prob_name= "p")
  return(list(ag_list, levels(ag_list$age)))
}


# helper function for synth_data_nativ. 
nat_lapply <- function(l, ht, v, levels) {
  if (nrow(l) < 1) return(l)
  l_age_comp <- ht[,2][which(l$age[1] == ht[,1])]
  comp <- v[which(grepl(l_age_comp, names(v)))]
  if (sum(comp) > 0) comp <- (comp / sum(comp)) 
  
  dat <- replicate(length(levels), l, simplify = FALSE)
  dat <- do.call("rbind", mapply(mapply_synth, dat= dat, prob_name= "p", attr_pct= comp, 
                                 attr_name= "nativity", level= levels,
                                 SIMPLIFY = FALSE))
  return(dat)
}

# helper function for synth_data_nativ. Internal to nat_lapply
# nat_mapply <- function(dat, comp, levels) {
#   dat <- data.frame(age= dat$age, gender= dat$gender, marital_status= dat$marital_status,
#                     edu_attain= dat$edu_attain, emp_status= dat$emp_status, 
#                     nativity= levels, p= dat$p * comp)
#   return(dat)
# }