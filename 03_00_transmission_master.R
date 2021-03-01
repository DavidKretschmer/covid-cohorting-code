# Set parameters
country               <- "sim"
runs                  <- 2 # number of simulations
type_vec              <- c("all", "random", "gender", "chain", "minimal") # type of cohorting strategy
index_vec             <- c(  1,   2,   3) # index for simulations (1:n)
dispers_vec           <- c(.60, .30, .20) # dispersion of gamma distribution
susceptibility_vec    <- c(.05, .15, .25) # probability of infection upon contact
share_symptomatic_vec <- c(.80, .50, .20) # share clinical cases
pr_out_of_school_vec  <- .20 # probability of out-of-school contact
inf_asymptomatic_vec  <- .50 # relative infectiousness of subclinical case
share_high_risk_vec   <- .25 # share of high-risk contacts in classroom
low_risk_vec          <- .20 # weighting factor for risk given low-risk contact
mode_vec              <- c("sequential", "parallel") # type of cohort instruction: rota-system vs. parallel

results_type          <- "main"
results_folder        <- paste0("transmission", "_", results_type)
transmission_file     <- "03_01_transmission.R"
marker                <- .20

get_date_run          <- paste0(substr(Sys.time(), 1, 10), "___", gsub(":", "-", substr(Sys.time(), 12, 19)), "_", country, "_", marker)


dir.create(results_folder)
dir.create(paste0(results_folder, "/", get_date_run))


file.copy(
    from = transmission_file,
			to = paste0(results_folder, "/", get_date_run, "/", transmission_file)
    )

source(transmission_file)

# q("no")
