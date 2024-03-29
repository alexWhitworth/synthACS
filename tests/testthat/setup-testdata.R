
## constraint lists
a <- c(under15 = 283597, `15_17` = 57266, `18_24` = 149083, `25_29` = 115003, 
       `30_34` = 115165, `35_39` = 115086, `40_44` = 113904, `45_49` = 113324, 
       `50_54` = 108417, `55_59` = 95216, `60_64` = 78938, `65_69` = 53435, 
       `70_74` = 38538, `75_79` = 29558, `80_84` = 23605, `85up` = 25001
)

e <- c(lt_hs = 363153, some_hs = 137643, hs_grad = 236052, some_col = 261491, 
       assoc_dec = 75493, ba_deg = 267756, grad_deg = 173548)

g <- c(Male = 742993, Female = 772143)

m <- c(never_mar = 732561, married = 535052, mar_apart = 70865, widowed = 62458, 
       divorced = 114200)

n <- c(born_other_state = 275116, born_out_us = 22418, born_state_residence = 753706, 
       foreigner = 463896)

r <- c(asian = 392080, `black, afr amer` = 181728, `hispanic, latino` = 332334, 
       `native amer` = 8279, `pacific islander` = 12409, `two or more races` = 82543, 
       `white alone` = 505763)

## local test-micro
# load("./tests/testdata/dat-par_sim_anneal.Rdata")

# test_micro <- syn[[1]]$synthetic_micro
# a_m <- all_geog_constraint_age(syn, 'synthetic')[[1]]
# ed_m <- all_geog_constraint_edu(syn, 'synthetic')[[1]]
# g_m <- all_geog_constraint_gender(syn, 'macro.table')[[1]]
# nat_m <- all_geog_constraint_nativity(syn, 'macro.table')[[1]]
# mar_m <- all_geog_constraint_marital_status(syn, 'synthetic')[[1]]
# r_m <- all_geog_constraint_race(syn, 'synthetic')[[1]]
# 
# save(test_micro, a_m, ed_m, g_m, nat_m, mar_m, r_m
#      , file= "./tests/testdata/dat-test_micro.xz", version= 3, compress= 'xz')