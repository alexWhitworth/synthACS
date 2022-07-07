library(data.table)
library(acs)
library(synthACS)
library(testthat)

# load(system.file("testdata", 'dat-acsdata.Rda', package= "synthACS"))
# ca_dat <- readRDS(file= system.file("tests", "testdata", 'dat-cadat.rds'
#                                     , package= "synthACS"))
# towork <- readRDS(file= system.file("tests", "testdata", 'dat-towork.rds'
#                                     , package= "synthACS"))
# test_macro <- readRDS(file= system.file("tests", "testdata", 'dat-testmacro.rds'
#                                         , package= "synthACS"))


# test_check("synthACS")

# LOAD TEST DATA FOR LOCAL TESTS
#------------------------------------------------
# below file is ~69 mb; saved on dropbox. Must port to local install
# load("./tests/testthat/dat-par_sim_anneal.Rdata")


# MAKE TEST DATA
#------------------------------------------------
# library(synthACS)
# ca_geo <- geo.make(state= 'CA', county= '*')
# ca_dat <- pull_synth_data(2012, 5, ca_geo)
# save.image("./synthACS/tests/testthat/acsdat.Rdata")
# towork.Rda == object transit_work from paper

