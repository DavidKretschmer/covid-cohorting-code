
# Load relevant packages
library(foreach)
library(doParallel)
library(tidyverse)



# Load input data
load(paste0("data_prepared", "/", "cils_prepared_", country, ".RData"))

path_to_minimization <- paste0("optimization_results", "/", "all_minimal_groups_", country, ".RData")

load(path_to_minimization)

setwd("secondary_attack_rates")
setwd(get_date_run)

#save(all_minimal, file = "all_minimal_groups.RData")

# Register parallel backend
procs <- 2
registerDoParallel(procs)




#####################################
### Out-of-school contact network ###
#####################################

network_out_of_school <- function(
  ids,
  cils_class,
  network_indicator
) {


### Genereate edelist
ego_alter <- cils_class %>% 
  filter(youthid %in% (ids %>% pull(youthid))) %>% # select all IDs in classroom
  select( # select relevant network variables
    youthid,
    starts_with(paste0(network_indicator, "_")),
  ) %>%
  select(
    -starts_with(paste0(network_indicator, "_", "0"))
  ) %>% # transform to edgelist
  pivot_longer(
    cols = -youthid,
    values_to = "alterid"
  ) %>%
  mutate(
    alterid = as.character(alterid) %>% as.numeric() # ensure correct numeric format
  ) %>%
  filter(!is.na(alterid)) %>% # filter non-nominations
  filter(youthid != alterid) %>% # filter self-nomations
  filter(alterid %in% (ids %>% pull(youthid))) %>% # filter IDs in classroom
  select(-name) 

### Find all reciprocal relations, and drop one instance
### to avoid double-counting
recip_drop <- ego_alter %>% # when switching ego/alter ID, 
                            # only reciprocal relations are successfully merged
  inner_join(
    ego_alter,
    by = c("youthid" = "alterid", "alterid" = "youthid")
  ) %>% 
  filter(youthid > alterid)  %>% # select the instance to drop (not both!)
  mutate(recip_drop = TRUE)

ego_alter <- ego_alter %>% 
  left_join(
    recip_drop,
    by = c("youthid", "alterid")
  ) %>% 
  filter(is.na(recip_drop)) %>% # only keep those that do not have recip_drop == TRUE
  select(-recip_drop)

ego_alter <- ego_alter %>% rename(ego = youthid, alter = alterid)

return(ego_alter)

}




#################################################
### Secondary attack rate for given classroom ###
#################################################


classroom_secondary_attack <- function(
  classid_this,
  all_minimal,
  cils_class,
  network_indicator,
  susceptibility,
  symptomatic,
  inf_asymptomatic,
  pr_out_of_school,
  share_high_risk,
  low_risk,
  mode,
  reps
) {


# All students in class
ids <- all_minimal %>% 
  filter(classid == classid_this) %>%
  select(youthid)


# Obtain network of out-of-school contacts
network_out <- network_out_of_school(
  ids = ids,
  cils_class = cils_class,
  network_indicator = network_indicator
)

# Get all directed ties
network_contacts <- network_out %>% 
  bind_rows(
    network_out %>% 
    rename(alter = ego, ego = alter) %>%
    select(ego, alter)
  ) %>% 
  distinct(ego, alter) %>% 
  mutate(risk = 1)


ids <- ids %>% rename(ego = youthid)

# Get cohorts (no cohorting)
groups <- ids %>%
      select(ego) %>%
      mutate(group = 1)


# In-school classroom network 
network_school <- groups %>% 
  pull(ego) %>%
  map(~groups %>% mutate(alter = .x)) %>%
  bind_rows() %>% 
  left_join(
    groups %>% rename(group_alter = group),
    by = c("alter" = "ego")
  ) %>% # full edgelist of all contacts
  filter(group == group_alter) %>% # select only those in the same cohort
  filter(ego != alter) %>% # exclude self-ties
  select(ego, alter)


# Apply individual function to all students in class
classroom_sec <- groups %>% pull(ego) %>% as.vector() %>%
  map(
  single_student_simulation_rep,
  student_data = ids,
  network_contacts = network_contacts,
  network_school = network_school,
  groups = groups,
  susceptibility = susceptibility,
  symptomatic = symptomatic,
  inf_asymptomatic = inf_asymptomatic,
  pr_out_of_school = pr_out_of_school,
  share_high_risk = share_high_risk,
  low_risk = low_risk,
  mode = mode,
  reps = reps
  ) %>% 
  bind_rows() %>%
  mutate(
    classid = classid_this,
    susceptibility = susceptibility,
    symptomatic = symptomatic,
    inf_asymptomatic = inf_asymptomatic,
    pr_out_of_school = pr_out_of_school,
    share_high_risk = share_high_risk,
    low_risk = low_risk,
    mode = mode,
    reps = reps
    ) 


}



###############################################
### Secondary attack rate for given student ###
###############################################


single_student_simulation_rep <- function(
  student_id,
  student_data,
  network_contacts,
  network_school,
  groups,
  susceptibility,
  symptomatic,
  inf_asymptomatic,
  pr_out_of_school,
  share_high_risk,
  low_risk,
  mode,
  reps
) {

  # Pick the individual student
  ind_student_data <- student_data %>% filter(ego == student_id)

  # Apply function across repetitions
  res <- 1:reps %>%
      map(
      single_student_simulation,
      ind_student_data = ind_student_data,
      network_contacts = network_contacts,
      network_school = network_school,
      groups = groups,
      susceptibility = susceptibility,
      symptomatic = symptomatic,
      inf_asymptomatic = inf_asymptomatic,
      pr_out_of_school = pr_out_of_school,
      share_high_risk = share_high_risk,
      low_risk = low_risk,
      mode = mode
      ) %>%
      bind_rows()
  
}


###############################################################
### Secondary attack rate for given student for a given run ###
###############################################################



single_student_simulation <- function(
  ind_student_data,
  network_contacts,
  network_school,
  groups,
  susceptibility,
  symptomatic,
  inf_asymptomatic,
  pr_out_of_school,
  share_high_risk,
  low_risk,
  mode, 
  rep
) {


##############################
#### Disease trajectories ####
##############################

main_data <- ind_student_data %>% 
  mutate(
    symptomatic = symptomatic,# will student be symptomatic if exposed/infectious
    infectiousness_gamma = rgamma(
      n = 1, 
      shape = gamma, 
      scale = 1/gamma
      ),
    infectiousness = ifelse(
      test = symptomatic == 1, 
      yes = infectiousness_gamma, 
      no = infectiousness_gamma * inf_asymptomatic
    ),
    susceptibility = susceptibility, # fixed susceptibility (probability of exposed | encounter with infectious)
    serial = rgamma(
      n = 1, 
      shape = 4, 
      scale = 3/4
      ) %>% round(), # length of exposed until infectious
    inf_subcl = rgamma(
      n = 1, 
      shape = 4, 
      scale = 5/4
      ) %>% round(), # length of subclinial infectious period (asymptomatic)
    inf_precl = rgamma(
      n = 1, 
      shape = 4, 
      scale = 2.1/4
      ) %>% round(), # length of preclinical infectious period (symptomatic)
    inf_cl = rgamma(
      n = 1, 
      shape = 4, 
      scale = 2.9/4
      ) %>% round(), # length of clinical infectious period (symptomatic)
    inf_start = 1, # first day of being infectious
    inf_end = ifelse(symptomatic == 0, inf_subcl + 1, inf_precl + 1), # first day of not being infectious anymore
  ) %>%
  select(-serial, -symptomatic, -inf_subcl, -inf_precl, -inf_cl)


# Which group is present in first week in sequential setup?
week1_group <- rbinom(n = 1, size = 1, prob = .5)

# Which day of the week does the simulation start (1 = Monday)
start_day <- sample(1:7, 1)

# Track when students are present in classroom
time_tracking <- tibble(
  weekday = rep(1:7, 8),
  week = rep(1:8, each = 7),
  group = rep(c(rep(week1_group, 7), rep(1 - week1_group, 7)), 4) # groups are present in subsequent weeks
) %>%
filter(
  week > 1 | weekday >= start_day
) %>%
rowid_to_column("time") %>%
filter(time < 50) # restrict to seven weeks


# When both cohorts are instructed in parallel
if (mode == "parallel") {

group0_present <- time_tracking %>%
  filter(weekday %in% c(1, 2, 3, 4, 5)) %>% # Monday-Friday
  pull(time)

group1_present <- time_tracking %>%
  filter(weekday %in% c(1, 2, 3, 4, 5)) %>% # Monday-Friday
  pull(time)

}

# When both cohorts are instructed sequentially
if (mode == "sequential") {

group0_present <- time_tracking %>%
  filter(weekday %in% c(1, 2, 3, 4, 5) & group == 0) %>% # Monday-Friday when group 0 present
  pull(time)

group1_present <- time_tracking %>%
  filter(weekday %in% c(1, 2, 3, 4, 5) & group == 1) %>% # Monday-Friday when group 1 present
  pull(time)

}


# Which cohort does focal student belong to?
main <- main_data %>% 
  left_join(
    groups,
    by = "ego"
  )

# Stochastic risk network
risk_network <- network_school %>% 
  filter(ego > alter) %>% # only consider undirected ties
  mutate(
    risk = ifelse(
    test = runif(length(ego)) < share_high_risk, # is contact high- or low-risk
    yes =  1, 
    no = low_risk) 
)

risk_network <- risk_network %>% # ensure that ego-alter are included both ways
  bind_rows(risk_network %>% rename(ego = alter, alter = ego))

network_school <- risk_network


# Collect all meeting other students and all exposures/infections of other students
# for secondary attack rate
meet_all <- tribble(~ego, ~alter)
exposure_all <- tribble(~alter)


###########
### Run ###
###########

# For the period of infectiousness
for (time_i in 1:main_data$inf_end) {


##################################################
### Incrementing time and disease trajectories ###
#################################################


main <- main %>% mutate(time = time_i)

# Who do the infectious students meet at school, given school data (alters)
exposure_school <- main %>%
  filter((group == 0 & (time %in% group0_present)) | (group == 1 & (time %in% group1_present))) %>% # only encounters when infectious student present
  left_join(
    network_school,
    by = c("ego")
) 

# Who do the infectious students meet outside of school?
# Encounters are stochastic with a given probability
exposure_out <- main %>% 
left_join(
  network_contacts,
  by = "ego"
) %>%
filter(!is.na(alter)) %>%
mutate(
    met = runif(nrow(.)) < pr_out_of_school # does meeting (stochasticc) take place?
  ) %>% 
filter(met == TRUE) %>% 
select(-met) 



# Full exposure information (at school and outside)
exposure <- exposure_school %>%
  bind_rows(exposure_out)

# Add new meetings to all meetings
meet_all <- meet_all %>%
  bind_rows(exposure)


######################
### New Infections ###
######################

new_exposed <- exposure %>%
  group_by(alter) %>%
  summarize( # Probability of exposure is 1 - probability of not being exposed in any of the encounters,
            # which is the product of not being exposed in any single encounter
    pr_inf = 1 - prod((1 - risk * infectiousness * susceptibility))
  ) %>%
  mutate(
    infection = ifelse(runif(nrow(.)) < pr_inf, 1, 0) # draw random number according to exposure probability
  ) %>%
  filter(infection == 1) %>%
  select(alter)


# Add new exposues/infections due to focal student
exposure_all <- exposure_all %>%
  bind_rows(new_exposed)

}

# Calculate the number of studdents met and the number of students exposed/infected
tibble(
 number_exposed = exposure_all %>% distinct(alter) %>% nrow(),
 number_met = meet_all %>% distinct(alter) %>% nrow(),
 rep = rep
)

}





# Get all classids to iterate over
all_classids <-  all_minimal %>% 
  distinct(classid) %>% 
  pull(classid)



combinations_vec <- length(type_vec) * 
  length(susceptibility_vec) * 
  length(pr_out_of_school_vec) * 
  length(inf_asymptomatic_vec) * 
  length(share_high_risk_vec) * 
  length(low_risk_vec) * 
  length(mode_vec) * 
  reps *
  length(all_classids) /
  procs


# Print parameters for the simulation
options(width=200)
sink(file="parameters.txt")
paste0("type: ", country) %>% print()
paste0("type: ", paste(type_vec, collapse = ", ")) %>% print()
paste0("susceptibility: ", paste(susceptibility_vec, collapse = ", ")) %>% print()
paste0("pr_out_of_school: ", paste(pr_out_of_school_vec, collapse = ", ")) %>% print()
paste0("inf_asymptomatic: ", paste(inf_asymptomatic_vec, collapse = ", ")) %>% print()
paste0("share_high_risk: ", paste(share_high_risk_vec, collapse = ", ")) %>% print()
paste0("low_risk: ", paste(low_risk_vec, collapse = ", ")) %>% print()
paste0("mode: ", paste(mode_vec, collapse = ", ")) %>% print()
paste0("reps: ", paste(reps, collapse = ", ")) %>% print()
paste0("total simulations per node: ", combinations_vec) %>% print()
sink()

# Run the simulation across the entire parameter space


time_start <- Sys.time()

res_all <- foreach(susceptibility = susceptibility_vec, .combine = rbind) %:%
              foreach(pr_out_of_school = pr_out_of_school_vec, .combine = rbind) %:%
                foreach(symptomatic = symptomatic_vec, .combine = rbind) %:%
                foreach(inf_asymptomatic = inf_asymptomatic_vec, .combine = rbind) %:%
                foreach(share_high_risk = share_high_risk_vec, .combine = rbind) %:%
                foreach(low_risk = low_risk_vec, .combine = rbind) %:%
                  foreach(mode = mode_vec, .combine = rbind) %:%
                    foreach (classid_this = all_classids, 
                            .packages = c("tidyverse"),
                            .export = c("simulation_epidemic_cohorting"),
                            .combine = rbind) %dopar% {


                      classroom_secondary_attack(
                        classid_this = classid_this,
                        all_minimal = all_minimal,
                        cils_class = cils_class_w1,
                        network_indicator = "tos",
                        susceptibility = susceptibility,
                        symptomatic = symptomatic,
                        inf_asymptomatic = inf_asymptomatic,
                        pr_out_of_school = pr_out_of_school,
                        share_high_risk = share_high_risk,
                        low_risk = low_risk,
                        mode = mode,
                        reps = reps
                      )


}


time_end <- Sys.time()

# Save results

save(res_all, file = "res_all_data.RData")


# Update parameters with runtime
options(width=200)
sink(file="parameters.txt")
paste0("type: ", country) %>% print()
paste0("type: ", paste(type_vec, collapse = ", ")) %>% print()
paste0("susceptibility: ", paste(susceptibility_vec, collapse = ", ")) %>% print()
paste0("pr_out_of_school: ", paste(pr_out_of_school_vec, collapse = ", ")) %>% print()
paste0("inf_asymptomatic: ", paste(inf_asymptomatic_vec, collapse = ", ")) %>% print()
paste0("share_high_risk: ", paste(share_high_risk_vec, collapse = ", ")) %>% print()
paste0("low_risk: ", paste(low_risk_vec, collapse = ", ")) %>% print()
paste0("mode: ", paste(mode_vec, collapse = ", ")) %>% print()
paste0("reps: ", paste(reps, collapse = ", ")) %>% print()
paste0("total simulations per node: ", combinations_vec) %>% print()
paste0("time: ", difftime(time_end, time_start, units = "hours")) %>% print()
sink()

