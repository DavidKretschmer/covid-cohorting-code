################
### Packages ###
################

# Package installation: all packages can be installed through
#package_name <- "dplyr"
#install.packages(package_name)

# Specific versions of packages can be installed through
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_0.8.5.tar.gz"
#install.packages(packageurl, repos = NULL, type = "source")


###################################
### Packages for specific files ###
###################################

# Versions specified are versions which most testing was conducted for (on R 3.6.3 on MacOS Mojaves)


### Simulated data  (00_01_simulate_networks)
install.packages("sna") # version 2.5
install.packages("ergm") # version 3.10.4
install.packages("tidyverse") # version 1.3.0 (dplyr 0.8.5)


### Optimize cohorting (01_01_optimize_cohorting)
install.packages("doParallel") # version 1.0.15
install.packages("foreach") # version 1.5.0
install.packages("tidyverse") # version 1.3.0 (dplyr 0.8.5)

### Cross-cohort ties (02_01_cross_cohort_ties)
install.packages("ggridges") # version 0.5.2

### Transmission analysis (04_01_analysis_simple)
# install.packages("devtools") # if devtools not installed, install first to download ggh4x
devtools::install_github("teunbrand/ggh4x")


# q("no")