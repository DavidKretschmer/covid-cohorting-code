
# Load relevant packages
library(foreach)
library(doParallel)
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)


# Load input data
load(paste0("data_prepared", "/", "cils_prepared_", country, ".RData"))

path_to_minimization <- paste0("optimization_results", "/", "all_minimal_groups_", country, ".RData")

load(path_to_minimization)

setwd(paste0(results_folder, "/", get_date_run))


# Register parallel backend
procs <- as.numeric(Sys.getenv("SLURM_JOB_CPUS_PER_NODE"))
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



########################################
### Within-school ties: random split ###
########################################

groups_in_school_random <- function(
  ids
  ) {

### Allocate IDs in classroom randomly to groups
cils_main_group <- ids %>%
  mutate( # random allocation, sample half the class for one group
    sam = list(sample(youthid, length(youthid)/2)),
    group_random = ifelse(youthid %in% unlist(sam), 1, 0)
  ) %>%
  select(youthid, group_random) %>%
  rename(ego = youthid)

return(cils_main_group)

}




########################################
### Within-school ties: gender split ###
########################################


groups_in_school_gender <- function(
  ids,
  cils_main
  ) {


### Allocate IDs in classroom by gender
cils_main_group <- ids %>%
  left_join(cils_main, by = "youthid") %>%
  mutate( # allocation by gender, ensuring equal group size
    random = rbinom(length(youthid), 1, .5), # for simplicity, impute missing gender data
    girl_help = case_when(
      is.na(girl) & random == 1~"Girl",
      is.na(girl) & random == 0~"Boy",
      TRUE~girl
    )
  ) %>%
  mutate(
    n_diff = sum(girl_help == "Girl", na.rm = TRUE) - sum(girl_help == "Boy", na.rm = TRUE), # gender imbalance

    group_gender_fix = ifelse(girl_help == "Girl", 1, 0), # girls are group 1 (fixed allocation)

    group_gender = ifelse(girl_help == "Girl", 1, 0), # girls are group 1 (full reallocation)
    to_group_0 = ifelse( # if more girls, reallocate some girls to group 0
      test = n_diff > 1, 
      yes = list(sample(youthid[girl_help == "Girl"], abs(n_diff / 2))), # draw random girls to reallocate
      no = list(c(""))
    ),
    to_group_1 = ifelse( # if more boys, reallocate some boys to group 1
      test = n_diff < -1, 
      yes = list(sample(youthid[girl_help == "Boy"], abs(n_diff / 2))), # draw random boys to reallocate
      no = list(c(""))
    ), # do the reallocation
    group_gender = ifelse(youthid %in% unlist(to_group_0), 0, group_gender),
    group_gender = ifelse(youthid %in% unlist(to_group_1), 1, group_gender),

    group_gender_rel = ifelse(girl_help == "Girl", 1, 0), # girls are group 1 (some reallocation)
    to_group_0_rel = ifelse( # if more girls, reallocate some girls to group 0
      test = n_diff > 5, 
      yes = list(sample(youthid[girl_help == "Girl"], abs((n_diff - 4) / 2))), 
      no = list(c(""))
    ),
    to_group_1_rel = ifelse( # if more boys, reallocate some boys to group 1
      test = n_diff < -5, 
      yes = list(sample(youthid[girl_help == "Boy"], abs((n_diff + 4) / 2))), 
      no = list(c(""))
    ), # do the reallocation
    group_gender_rel = ifelse(youthid %in% unlist(to_group_0_rel), 0, group_gender_rel),
    group_gender_rel = ifelse(youthid %in% unlist(to_group_1_rel), 1, group_gender_rel),
  ) %>%
  select(youthid, group_gender, group_gender_fix, group_gender_rel) %>%
  rename(ego = youthid)

return(cils_main_group)


}



#######################################
### Within-school ties: chain split ###
#######################################


groups_in_school_chain <- function(
  ids,
  network_out
)  {

  # Treat ties are directed (for choice)
  network_out <- network_out %>% 
  bind_rows(
    network_out %>% 
    rename(alter = ego, ego = alter) %>%
    select(ego, alter)
  )

  # Size of the chain-based group
  group_size <- ceiling(nrow(ids)/2)

  # Starting node
  start_id <- ifelse(
    test = nrow(network_out) > 0,
    yes = network_out %>% pull(ego) %>% sample(1), # draw random ID from out-ofs-school contact network
    no = ids %>% pull(youthid) %>% sample(1) # if no out-of-school network: random start
  )

  while( length(start_id) < group_size ) { # as long as the group is not yet to large

    seq_id <- network_out %>%
    filter(ego %in% start_id) %>%
    filter(!alter %in% start_id) %>%
    distinct(alter) %>%
    pull(alter)
    # get all of the group members' out-of-school contacts
    # not yet in the group

    if (length(seq_id) == 0) {

      seq_id <- ids %>% filter(!youthid %in% start_id) %>% pull(youthid) %>% sample(1)
      # if there are no new contacts, pick a random student for the group

    }

    if (length(start_id) + length(seq_id) <= group_size) {

      start_id <- c(start_id, seq_id)
      # if all new contacts fit into group, all are put into the group

    }  else {

      start_id <- c(start_id, sample(seq_id, size = group_size - length(start_id)))
      # if too many new contacts, select random subsample

    }

  }

  group_chain <- ids %>%
    mutate(group_chain = ifelse(youthid %in% start_id, 1, 0)) %>%
    rename(ego = youthid)

  return(group_chain)

}



################################################
################################################
### Simulate classroom transmission dynamics ###
################################################
################################################


epidemic_parameters <- function(
  student_data,
  dispers,
  susceptibility,
  share_symptomatic,
  inf_asymptomatic,
  mode
) {


##############################
#### Disease trajectories ####
##############################

main_data <- student_data %>% 
  bind_rows(tibble(ego = 100)) %>% # adding the teacher
  mutate(
    state = "S", # all start as susceptibility
    presence = "A", # all start as present in class
    quarantine_end = 0, # all start without any quarantine
    symptomatic = rbinom(
      n = length(ego), 
      size = 1, 
      prob = share_symptomatic), # will student be symptomatic if exposed/infectious
    infectiousness_gamma = rgamma(
      n = length(ego), 
      shape = dispers, 
      scale = 1 / dispers # baseline infectiousness
      ),
    infectiousness = ifelse(
      test = symptomatic == 1, 
      yes = infectiousness_gamma, 
      no = infectiousness_gamma * inf_asymptomatic), # relative infectiousness if asymptomatic
    susceptibility = susceptibility, # fixed susceptibility (probability of exposed | encounter with infectious)
    serial = rgamma(
      n = length(ego), 
      shape = 4, 
      scale = 3/4
      ) %>% round(), # length of exposed until infectious
    inf_subcl = rgamma(
      n = length(ego), 
      shape = 4, 
      scale = 5/4
      ) %>% round(), # length of subclinial infectious period (asymptomatic)
    inf_precl = rgamma(
      n = length(ego), 
      shape = 4, 
      scale = 2.1/4
      ) %>% round(), # length of preclinical infectious period (symptomatic)
    inf_cl = rgamma(
      n = length(ego), 
      shape = 4, 
      scale = 2.9/4
      ) %>% round(), # length of clinical infectious period (symptomatic)
    inf_start = serial + 1, # first day of being infectious
    inf_end = serial + ifelse(symptomatic == 0, inf_subcl + 1, inf_precl + inf_cl + 1), # first day of not being infectious anymore
    sympt_start = ifelse(symptomatic == 1, serial + inf_precl + 1, NA), # first day symptomatic
    disease_t = NA # time in disease trajectory
  ) %>%
  select(-serial, -inf_subcl, -inf_precl, -inf_cl, -infectiousness_gamma)


######################
### Initialization ###
######################

# Index exposed (random)
index <- sample(main_data %>% filter(ego != 100) %>% pull(ego), 1)

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

# Setting up data with index exposed
main <- main_data %>% 
  mutate(
    state = ifelse(ego %in% index, "E", state), # index is exposed
    disease_t = ifelse(ego %in% index, 0, NA) # index is in first period of disease
  )  


return(list(main = main, group0_present = group0_present, group1_present = group1_present, index = index))

}





simulation_epidemic_group <- function(
  ids,
  cils_main,
  network_data,
  all_minimal,
  type,
  pr_out_of_school,
  share_high_risk,
  low_risk,
  teacher_risk,
  main,
  group0_present,
  group1_present,
  index
) {


###########################
#### Cohort Allocation ####
###########################



  # Get cohorts for optimization procedure
  if (type == "minimal") {

    groups <- all_minimal %>% 
      filter(youthid %in% (ids %>% pull(youthid))) %>%
      select(youthid, group_min) %>%
      rename(ego = youthid, group = group_min)

  }    


  # Get cohorts for gender split with full reallocation
  if (type == "gender") {

    groups <- groups_in_school_gender(
      ids = ids,
      cils_main = cils_main
    ) %>%
    select(ego, group_gender) %>%
    rename(group = group_gender)

  }


  # Get cohorts for gender split without reallocation
  if (type == "gender_fix") {

    groups <- groups_in_school_gender(
      ids = ids,
      cils_main = cils_main
    ) %>%
    select(ego, group_gender_fix) %>%
    rename(group = group_gender_fix)

  }


  # Get cohorts for gender split with some reallocaton
  if (type == "gender_rel") {

    groups <- groups_in_school_gender(
      ids = ids,
      cils_main = cils_main
    ) %>%
    select(ego, group_gender_rel) %>%
    rename(group = group_gender_rel)

  }


  # Get setup without cohorting
  if (type == "all") {  

    groups <- ids %>%
      select(youthid) %>%
      rename(ego = youthid) %>%
      mutate(group = 1)

  }


  # Get cohorts for random cohorting (stochastic)
  if (type == "random") {

    groups <- groups_in_school_random(
        ids = ids
        ) %>%
        select(ego, group_random) %>%
        rename(group = group_random)

    }


  # Get cohorts for network chain cohorting (stochastic)
  if (type == "chain") {

    groups <- groups_in_school_chain(
        ids = ids,
        network_out = network_data
      ) %>%
      select(ego, group_chain) %>%
      rename(group = group_chain)
      

  }


main <- main %>% 
  left_join(
    groups,
    by = "ego"
  )





####################################
#### Out-of-school contact data ####
####################################


network_contacts <- network_data %>% 
  bind_rows(
    network_data %>% 
    rename(alter = ego, ego = alter) %>%
    select(ego, alter)
  ) %>% 
  distinct(ego, alter) %>% 
  mutate(risk = 1)
# ensure that ego-alter are included
# both ways



######################################################
#### Within-school contact according to cohorting ####
######################################################

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


risk_network <- risk_network %>%
  bind_rows(
    groups %>% mutate(alter = 100, risk = teacher_risk) %>% select(ego, alter, risk)
  )
# All students are in contact with teacher in school, with risk determiend by teacher_risk

network_school <- risk_network


# Start time
time_i <- 0

# Collect al of out-of-school expose instances (for quarantine)
exposure_out_all <- tribble(~ego, ~time, ~alter)



###########
### Run ###
###########

while (
  (main %>% 
    filter(presence == "A") %>%
    filter(state %in% c("E", "I")) %>% 
    nrow() > 0) &
  time_i < 50
  ) {
# The simulation runs as long as somebody is exposed or infectious and not in quarantine
# otherwise, no more infections or epidemic peters out (in quarantine). Simulation 
# ends after seven weeks

##################################################
### Incrementing time and disease trajectories ###
##################################################

time_i <- time_i + 1

# Move to different states (exposed -> infectious, infectious -> recovered)
main <- main %>%
  mutate(
    time = time_i, # move time forward by one
    disease_t = disease_t + 1, # move disease trajectory forward by one
    state = ifelse(state == "E" & disease_t >= inf_start, "I", state), # move exposed -> infectious
    state = ifelse(state == "I" & disease_t >= inf_end, "R", state), # move infectious -> recovered (not infectious)
    presence = ifelse(presence == "Q" & time > quarantine_end, "A", presence) # move out of quarantine if quarantine time has ended
  )



####################
### Quarantining ###
####################

# Check whether a student moves to quarantine (one day after symptom onset)
quarantine_case <- main %>% 
  filter(ego != 100) %>%
  filter(disease_t == sympt_start + 1 & !is.na(disease_t) & !is.na(sympt_start)) %>%
  distinct(ego, group)

# Check whether the teacher moves to quarantine (one day after symptom onset)
quarantine_teacher <- main %>% 
  filter(ego == 100) %>%
  filter(disease_t == sympt_start + 1 & !is.na(disease_t) & !is.na(sympt_start)) %>%
  nrow()

# Check quarantine-case out-of-school contacts in the last 14 days
quarantine_exposure_out <- exposure_out_all %>% 
  filter(time >= time_i - 14) %>%
  filter(ego %in% (quarantine_case %>% pull(ego))) %>%
  distinct(alter)

# Quarantine the newy symptomatic non-quarantined case's cohort in the last 14 days for 14 days
# Quarantine the newy symptomatic non-quarantined case's out-of-school contacts in the last 14 days for 14 days
main <- main %>%
  mutate(
    quarantine_end = ifelse(group %in% (quarantine_case %>% pull(group)) & presence == "A", time + 14, quarantine_end), # cohort quarantine
    quarantine_end = ifelse(ego %in% (quarantine_exposure_out %>% pull(alter)) & presence == "A", time + 14, quarantine_end), # out-of-school contact quarantine
    presence = ifelse(group %in% (quarantine_case %>% pull(group)) & presence == "A", "Q", presence), # cohort quarantine
    presence = ifelse(ego %in% (quarantine_exposure_out %>% pull(alter)) & presence == "A", "Q", presence) # out-of-school contact quarantine
  )

# Everybody quarantines if teacher is symptomatic
if (quarantine_teacher == 1) {
 
main <- main %>%
  mutate(
    quarantine_end = time + 14, # cohort quarantine
    presence = "Q", # cohort quarantine
  )

}

###################
### Interaction ###
###################
# (We only model interaction among infectious students)

# Determine the infectious non-quarantined students
infectious_students <- main %>% 
  filter(state == "I" & presence == "A")

# Who do the infectious students meet at school, given school data (alters)
exposure_school <- infectious_students %>%
  filter(ego != 100) %>%
  filter((group == 0 & (time %in% group0_present)) | (group == 1 & (time %in% group1_present))) %>% # only encounters when infectious student present
  left_join(
    network_school,
    by = c("ego")
) 
# (this includes exposure of teachers to infectious students) 

# If teacher is infectious, he/she interacts with all students present that day
exposure_to_teacher <- infectious_students %>% 
  filter(ego == 100) %>%
  left_join(
    main %>% 
      filter((group == 0 & (time %in% group0_present)) | (group == 1 & (time %in% group1_present))) %>%
      transmute(alter = ego, ego = 100, risk = teacher_risk),
    by = c("ego")
  )


exposure_school <- exposure_school %>%
  bind_rows(exposure_to_teacher)


# Who do the infectious students meet outside of school?
# Encounters are stochastic with a given probability
exposure_out <- infectious_students %>% 
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

# Collect all out of-school exposure over time
exposure_out_all <- exposure_out_all %>% bind_rows(exposure_out)

# Full exposure information (at school and outside)
exposure <- exposure_school %>%
  bind_rows(exposure_out)


######################
### New Infections ###
######################

new_exposed <- exposure %>%
  filter(alter %in% (main %>% filter(state == "S") %>% pull(ego))) %>% # only the susceptible can become exposed
  filter(alter %in% (main %>% filter(presence == "A") %>% pull(ego))) %>% # only the non-quarantined can become exposed
  mutate( # Probability of exposure is 1 - probability of not being exposed in any of the encounters,
            # which is the product of not being exposed in any single encounter
    pr_inf = risk * infectiousness * susceptibility
  ) %>%
  mutate(
    infection = ifelse(runif(nrow(.)) < pr_inf, 1, 0) # draw random number according to exposure probability
  ) %>%
  filter(infection == 1) %>%
  group_by(alter) %>%
  slice(1) %>%
  pull(alter)




# Update data according to newly exposed and start their disease trajectory
main <- main %>% 
  mutate(
    state = ifelse(ego %in% new_exposed, "E", state),
    disease_t = ifelse(ego %in% new_exposed, 0, disease_t)
  )  

# Start next period
}


######################################
### Summarizing results of the run ###
######################################



# How many cohorts? are affected
groups_affected <- main %>% 
  filter(ego != 100 & state %in% c("E", "I", "R")) %>%
  distinct(group) %>% 
  nrow()

# Is the teacher affected?
teacher_affected <- main %>% 
  filter(ego == 100 & state %in% c("E", "I", "R")) %>%
  nrow()

all_results <- main %>% 
  filter(ego != 100) %>%
  summarize(
    share_qua = mean(presence == "Q"), # share quarantined
    share_inf = mean(state %in% c("E", "I", "R") & ego != index), # share who have been infected
    n_inf = sum(state %in% c("E", "I", "R")  & ego != index), # number infected
    n = n()
  ) %>% 
  mutate(
    groups_affected = groups_affected,
    teacher_affected = teacher_affected,
    type = type
  )



all_results

}




################################################
################################################
### Simulate classroom transmission dynamics ###
### for specific parameter constellations and###
### cohorting strategies                     ###
################################################
################################################



simulation_epidemic_cohorting <- function(
  classid_this,
  cils_main,
  cils_class,
  susceptibility,
  share_symptomatic,
  inf_asymptomatic,
  pr_out_of_school,
  share_high_risk,
  low_risk,
  teacher_risk,
  network_indicator,
  mode,
  types,
  run,
  all_minimal,
  dispers
)  {


ids <- all_minimal %>% 
  filter(classid == classid_this) %>%
  select(youthid)


epidemic_parameters_run <- epidemic_parameters(
  student_data = ids %>% rename(ego = youthid),
  dispers = dispers, 
  susceptibility = susceptibility, 
  share_symptomatic = share_symptomatic,
  inf_asymptomatic = inf_asymptomatic,
  mode = mode
)


# Obtain network of out-of-school contacts
network_out <- network_out_of_school(
  ids = ids,
  cils_class = cils_class,
  network_indicator = network_indicator
)


res <- types %>% map(
  simulation_epidemic_group,
  ids = ids,
  cils_main = cils_main,
  network_data = network_out,
  all_minimal = all_minimal,
  pr_out_of_school = pr_out_of_school,
  share_high_risk = share_high_risk,
  low_risk = low_risk,
  teacher_risk = teacher_risk,
  main = epidemic_parameters_run$main,
  group0_present = epidemic_parameters_run$group0_present,
  group1_present = epidemic_parameters_run$group1_present,
  index = epidemic_parameters_run$index
) %>%
  bind_rows() %>%
    mutate(
        classid = classid_this,
        susceptibility = susceptibility,
        share_symptomatic = share_symptomatic,
        inf_asymptomatic = inf_asymptomatic,
        pr_out_of_school = pr_out_of_school,
        share_high_risk = share_high_risk,
        low_risk = low_risk,
        teacher_risk = teacher_risk,
        mode = mode,
        run = run,
        dispers = dispers
    ) 


# Return classroom-level results
res %>% select(type, classid, run, groups_affected, teacher_affected, share_inf, n_inf, share_qua, everything())

}

######################
######################
### Run Simulation ###
######################
######################


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
  length(teacher_risk_vec) * 
  length(mode_vec) * 
  runs *
  length(all_classids) /
  procs


# Print parameters for the simulation
options(width=200)
sink(file="parameters.txt")
paste0("type: ", country) %>% print()
paste0("type: ", paste(type_vec, collapse = ", ")) %>% print()
paste0("susceptibility: ", paste(susceptibility_vec, collapse = ", ")) %>% print()
paste0("pr_out_of_school: ", paste(pr_out_of_school_vec, collapse = ", ")) %>% print()
paste0("share_symptomatic: ", paste(share_symptomatic_vec, collapse = ", ")) %>% print()
paste0("dispers: ", paste(dispers_vec, collapse = ", ")) %>% print()
paste0("inf_asymptomatic: ", paste(inf_asymptomatic_vec, collapse = ", ")) %>% print()
paste0("share_high_risk: ", paste(share_high_risk_vec, collapse = ", ")) %>% print()
paste0("low_risk: ", paste(low_risk_vec, collapse = ", ")) %>% print()
paste0("teacher_risk: ", paste(teacher_risk_vec, collapse = ", ")) %>% print()
paste0("mode: ", paste(mode_vec, collapse = ", ")) %>% print()
paste0("runs: ", paste(runs, collapse = ", ")) %>% print()
paste0("total simulations per node: ", combinations_vec) %>% print()
sink()

# Run the simulation across the entire parameter space


time_start <- Sys.time()

res_all <-    foreach(index = index_vec, .combine = rbind) %:%
              foreach(pr_out_of_school = pr_out_of_school_vec, .combine = rbind) %:%
                foreach(inf_asymptomatic = inf_asymptomatic_vec, .combine = rbind) %:%
                foreach(share_high_risk = share_high_risk_vec, .combine = rbind) %:%
                foreach(low_risk = low_risk_vec, .combine = rbind) %:%
                foreach(teacher_risk = teacher_risk_vec, .combine = rbind) %:%
                foreach(run = 1:runs, .combine = rbind) %:%
                  foreach(mode = mode_vec, .combine = rbind) %:%
                    foreach (classid_this = all_classids, 
                            .packages = c("dplyr"),
                            .export = c("simulation_epidemic_cohorting"),
                            .combine = rbind) %dopar% {

                      simulation_epidemic_cohorting(
                        classid_this = classid_this,
                        cils_main = cils_main,
                        cils_class = cils_class_w1,
                        susceptibility = susceptibility_vec[index],
                        share_symptomatic = share_symptomatic_vec[index],
                        inf_asymptomatic = inf_asymptomatic,
                        pr_out_of_school = pr_out_of_school,
                        share_high_risk = share_high_risk,
                        low_risk = low_risk,
                        teacher_risk = teacher_risk,
                        network_indicator = "tos",
                        mode =  mode,
                        types = type_vec,                        
                        run = run,
                        all_minimal = all_minimal,
                        dispers = dispers_vec[index]
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
paste0("share_symptomatic: ", paste(share_symptomatic_vec, collapse = ", ")) %>% print()
paste0("dispers: ", paste(dispers_vec, collapse = ", ")) %>% print()
paste0("inf_asymptomatic: ", paste(inf_asymptomatic_vec, collapse = ", ")) %>% print()
paste0("share_high_risk: ", paste(share_high_risk_vec, collapse = ", ")) %>% print()
paste0("low_risk: ", paste(low_risk_vec, collapse = ", ")) %>% print()
paste0("teacher_risk: ", paste(teacher_risk_vec, collapse = ", ")) %>% print()
paste0("mode: ", paste(mode_vec, collapse = ", ")) %>% print()
paste0("runs: ", paste(runs, collapse = ", ")) %>% print()
paste0("total simulations per node: ", combinations_vec) %>% print()
paste0("time: ", difftime(time_end, time_start, units = "hours")) %>% print()
sink()

