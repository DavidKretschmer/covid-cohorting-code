# 1. System requirements

All analysis tested and run in R 3.6.3 and R 4.0.3 on MacOS Mojave and R 4.0.3 on Windows 10. All operating systems that support R 3.6.3 should be able to run the code. This should apply to all common operating systems.


# 2. Installation guide

R software can be downloaded from https://www.r-project.org/. Install time should be less than 5 minutes. The most frequently used user interface for R is RStudio (https://rstudio.com/), install time should be less than 5 minutes.
Packages for R software used in the code, including download instructions, are listed in script "00_00_packages.R". Total install time should not exceed 10 minutes. We recommend using version 0.8.5 of dplyr package because the code is much slower in more recent versions of the package.


# 3. Demo

Throughout, the instructions and scripts assume that the R path is at the top-level folder, i.e., "covid-cohorting-code" (e.g., in an R project or by command-line navigation). A .Rproj file is included. Total expected run time, if all steps are run, is about 30 minutes.

All of the scripts below are standalone. Because of path changes and the objects created, running R sessions should be closed before a new script is run (with q("no")).


## Step 1: Script: 00_01_simulate_networks.R

The original CILS4EU data used in the analysis cannot be shared for data protection reasons. The data can be requested from GESIS (https://dbk.gesis.org/dbksearch/sdesc2.asp?no=5353&db=e&doi=10.4232/cils4eu.5353.3.1.0) and further information is available at https://www.cils4.eu/. 

To allow researchers to assess our code, we provide simulated data that the code can be run with. These data have the same structure as the CILS4EU data such that all of the code can be run on these data. However, because the data are simulated on the basis of a simple model, the data cannot be used to replicate our substantive findings from the manuscript. The example data is provided as .RData file "cils_prepared_sim.RData" in the folder "data_prepared".

These data are generated in the script "00_01_simulate_networks.R". This script can be adjusted to the researcher's need. In its default shape, it generates data for ten simulated classrooms in each of the four countries under analysis, i.e., 40 classrooms in total. For 40 networks, the analyses described subsequently can be performed in reasonable amounts of time (about 30 minutes). If the number of networks is increased, runtime will increase accordingly. Examplary data "cils_prepared_sim.RData" has already been generated, so the researcher themselves does not have to run the script "00_01_simulate.networks.R". If the researcher runs the script "00_01_simulate_networks.R" and saves the data at the end of the script, the example data is overwritten.


## Step 2: Script: 01_00_optimize_master.R

For the simulated data, the script "01_01_optimize_cohorting.R" runs the algorithm that minimizes out-of-school cross-cohort contact. The script is executed via "01_00_optimize_master.R", which saves the results in specific subfolders in the folder "optimization_results". For the 40 classrooms in the example data, the optimization algorithm runs about 10 minutes. The resulting information on cohort allocation is saved in the file "cils_prepared_sim.RData". We provide a corresponding data file for the example data.


## Step 3: Descriptives: Scripts: 02_01_network_means.R, 02_02_network_graphs.R, and 02_03_cross_cohort_ties.R

These scripts generate descriptive results. "02_01_networks_means.R" provides descriptive statistics on the networks (mean number of ties etc.). "02_02_network_graphs.R" generates plots for example networks, comparable to Figure 1 (saved in folder "network_graph"). "02_03_cross_cohort_ties.R" provides an overview of the number of cross-cohort ties that result from the different cohorting strategies. The results, two figures (like Fig 4, Fig A1 in the manuscript), are saved in the folder "ties_results". 


## Step 4: 03_00_transmission_master.R

This script runs classroom-level transmission models, which are discussed throughout the results and methods sections of the manuscript. Parameters can be adjusted in "03_00_transmission_master.R". In the default version, the script runs about 10 minutes (longer for dplyr version > 0.8.5). It calls the script "03_01_transmission.R". It returns a subfolder in the folder "transmission_main". The name of the subfolder is given by the start time of the simulation run. Within the subfolder, the data set "res_all_data.RData" contains the results of the simulations. 

We provide one example subfolder for the example data. Additional subfolders can be added by running "03_00_transmission_master.R".

The script "03_00_transmission_master_teacher.R" runs the simulation model when including teachers. Output is saved in subfolders in the folder "transmission_teacher". We also include one example data set for these simulations.


### Step 5: Scripts: 04_01_analysis_main.R, 04_06_analysis_teacher.R

These scripts provide overview results for the simulations from the previous step. At the beginning of each file, the data are loaded by identifying the corresponding subfolders. By default, the data from the example subfolder mentioned above are loaded. Additional data can be added by referencing the corresponding subfolders once they have been generated by running "03_00_transmission_master.R" or "03_00_transmission_master_teacher.R". 

"04_01_analysis_main.R" generates Fig. 3 and Fig. 5 from the main text, i.e., results on the effect of cohorting strategies for specific parameter constellations, Extended Data Figure 1-2 (i.e., results across the entire parameter space), and Fig. B1-B4 and results by country (Supplementary Material B, Fig. B1-B2).  "04_01_analysis_teacher.R" generates results for the models including teachers, reported in Appendix F (Fig. F1-F2). The results are saved in subfolders "results" in "transmission_main" and "transmission_teacher"

Note that the simulations in the previous step are stochastic. Therefore, a large number of simulations is necessary to arrive at precise estimates. For the manuscript, simulations are repeated thousands of times. When running simulations with the default parameters in script "03_00_transmission_master.R", results will be highly stochastic and the figures will not necessarily look like those from the manuscript.

We also provides scripts "04_02_analysis_gender.R", "04_03_analysis_asymp.R", "04_04_analysis_risk.R", "04_04_analysis_prob.R" in folder "scripts_supplementary", which can be used to reproduce the results from Supplementary Material A, C, D, and E. Beforehand, however, corresponding models have to be estimated using "03_00_transmission_master".


### Step 6: Scripts 05_00_secondary_master.R and 05_02_secondary_analysis.R

"05_00_secondary_master" calls "05_01_secondary" and calculates secondary attack rates for given probabilities of infection upon contact and dispersion parameters of gamma distributions. Three example results for different probabilities of infection and different dispersion parameters are saved. "05_02_secondary_analysis.R" provides analyses of these results and saves the distribution of the number of infections and other information (such as secondary attack rate, 80% quantile, etc.).


# 4. Instructions for use

Running the software for the CILS4EU data follows the steps outlined above. Usually, results from multiple simulation runs have to be aggregated in Step 4. Otherwise, the analysis is identical.

Because of data protection regulation, we cannot share the original CILS4EU data. Therefore, the results from the manuscript cannot be reproduced with the material provided here. However, analogs of all quantitative results from the manuscript using the simulated data can be generated from the instructions above and the scripts provided. Note that these will differ from the results in the manuscript.


