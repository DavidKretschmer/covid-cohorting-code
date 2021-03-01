# Set parameters
country                 <- "sim"
gamma                   <- .20  # dispersion of gamma distribution
susceptibility_vec      <- .25 # probability of infection upon contact
reps                    <- 2 # number of simulations
type_vec                <- "all" # no cohorting
pr_out_of_school_vec    <- .20 # probability of out-of-school contact
symptomatic_vec         <- c(0, 1) # subclinical vs. clinical case
inf_asymptomatic_vec    <- .50 # relative infectiousness of subclinical case 
share_high_risk_vec     <- .25 # share of high-risk contacts in classroom
low_risk_vec            <- .20 # weighting factor for risk given low-risk contact
mode_vec                <- "parallel" # parallel instruction (no cohorting)


path_top <-  getwd()

get_date_run <- paste0(substr(Sys.time(), 1, 10), "___", gsub(":", "-", substr(Sys.time(), 12, 19)), "_gamma", "_", gamma, "_susc", "_", susceptibility_vec, "_", country)
path_get_date_run <- paste0(path_top, "/", "secondary_attack_rates", "/", get_date_run)

class_transmission_file <- "05_01_secondary.R"
dir.create("secondary_attack_rates")

dir.create(paste0("secondary_attack_rates", "/", get_date_run))

file.copy(
    from = paste(path_top, "/", class_transmission_file, sep = ""),
			to = paste(path_get_date_run,  "/",class_transmission_file,sep=""))

source(paste(path_top, "/", class_transmission_file, sep = ""))

q("no")


