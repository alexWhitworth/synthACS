# synthACS

synthACS provides four main features. Firstly, it provides a wrapper
  to library(acs) to access numerous American Community Survey (ACS) base tables
  which may be of interest to many researchers. Secondly, it builds synthetic
  microdatasets of ACS data (pulled via API) at any specified geographic level with
  10 default individual attributes. Thirdly, synthACS provides funtionality for users
  to add additional ACS & non-ACS synthetic data-attributes to micro-datasets based on
  macro population characteristics. And finally, in addition to creating synthetic
  data, synthACS also conducts spatial microsimulation modeling (SMSM) (ie- optimally
  fits synthetic microdata to macrodata constraints) via simulated annealing.
  SMSM is conducted in parallel by default.
