
# Set parameters
country <- "sim"
path_top <-  getwd()

get_date_run <- paste0(substr(Sys.time(), 1, 10), "___", gsub(":", "-", substr(Sys.time(), 12, 19)), "_", country)
path_get_date_run <- paste0(path_top, "/", "optimization_results", "/", get_date_run)

optimize_file <- "01_01_optimize_cohorting.R"
dir.create("optimization_results")
dir.create(paste0("optimization_results", "/", get_date_run))


file.copy(
    from = paste(path_top, "/", optimize_file, sep = ""),
			to = paste(path_get_date_run,  "/", optimize_file, sep=""))

source(paste(path_top, "/", optimize_file, sep = ""))


# q("no")


