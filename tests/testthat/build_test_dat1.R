
ca_geo <- geo.make(state= "CA", county="*")
ca_dat <- pull_synth_data(geography= ca_geo)
syn <- derive_synth_datasets(ca_dat, parallel= TRUE)
test_micro <- syn[[2]][[1]][[2]];
test_macro <- syn[[2]][[1]][[1]]; rm(syn)

g <- test_macro$age_by_sex[2:3]; names(test_macro) <- c("Male", "Female")
e <- round(tapply(test_micro$p, test_micro$edu_attain, sum) * test_macro$age_by_sex[1] ,0)
a <- unlist(test_macro$age_by_sex)[-c(1:3)]
a <- apply(cbind(a[1:16], a[17:32]), 1, sum); names(a) <- levels(test_micro$age)
n <- round(tapply(test_micro$p, test_micro$nativity, sum) * test_macro$age_by_sex[1] ,0)
r <- round(test_macro$pop_by_race[c(5,3,10,4,6,8,9)] * test_macro$age_by_sex[1] / 
             sum(test_macro$pop_by_race[c(5,3,10,4,6,8,9)]) ,0)
r[7] <- r[7]-1
names(r) <- levels(test_micro$race)
m <- round(tapply(test_micro$p, test_micro$marital_status, sum) * test_macro$age_by_sex[1] ,0)
m[1] <- m[1] + 1

save.image("C:/Github_projects/ACSpulls/synthACS/tests/testthat/acsdat.Rdata")
