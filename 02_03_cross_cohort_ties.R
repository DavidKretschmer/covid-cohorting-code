
# Load relevant packages
library(foreach)
library(doParallel)
library(ggridges)
library(tidyverse)


procs <- 2
registerDoParallel(procs)


#####################################
### Out-of-school contact network ###
#####################################

network_out_of_school <- function(
  ids = ids,
  cils_class = cils_class_w1,
  network_indicator = "bfs"
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

ego_alter <- ego_alter %>% rename(ego = youthid, alter = alterid) # final edgelist

return(ego_alter)

}


########################################
### Within-school ties: random split ###
########################################

groups_in_school_random <- function(
  ids = ids
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
  ids = ids,
  cils_main = cils_main
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
  ids = ids,
  network_out = network_out
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



##########################################
### Comparison of cohorting strategies ###
##########################################


classroom_cohorting_ties <- function(
  cils_main = cils_main,
  cils_class = cils_class_w1,
  classid_this = 101801,
  network_indicator = "bfs",
  run = 1,
  all_minimal = all_minimal
) {


ids <- all_minimal %>% 
  filter(classid == classid_this) %>%
  select(youthid)


### Calculate network of out-of-school ties
### (undirected ties) 
network_out <- network_out_of_school(
  ids = ids,
  cils_class = cils_class,
  network_indicator = network_indicator
)

### Random cohorting
random <- groups_in_school_random(
  ids = ids
)

### Gender-specific cohorting with and without
### reallocation for equal group size
gender <- groups_in_school_gender(
  ids = ids,
  cils_main = cils_main
)

### Out-of-school-tie chain cohorting
chain <- groups_in_school_chain(
  ids = ids,
  network_out = network_out
) 

### Optimized cohorting
minimal <- all_minimal %>% 
  filter(youthid %in% (ids %>% pull(youthid))) %>%
  select(youthid, group_min) %>%
  rename(ego = youthid)


### Merging information for all cohorting strategies
cils_main_group <- random %>%
  left_join(
    gender,
    by = "ego"
  ) %>%
  left_join(
    chain,
    by = "ego"
  ) %>%
  left_join(
    minimal,
    by = "ego"
  )

### Merging cohort information to the relationship data
### ego and alter cohort
ego_alter_cov <- network_out %>%
  left_join(
    cils_main_group, by = "ego",
  ) %>% 
  rename_at(colnames(cils_main_group[-1]), ~paste0("ego_", .)) %>%
  left_join(
    cils_main_group, by = c("alter" = "ego"),
    ) %>%
  rename_at(colnames(cils_main_group[-1]), ~paste0("alter_", .))



### Calculate cross-group ties under different scenarios
### and return all the information

ego_alter_cov %>%
  summarize(
    n_students = cils_main_group %>% nrow(),
    n_students_with_ties = length(unique(ego)),
    n_ties = n(),
    n_cross_group_random = sum(ego_group_random != alter_group_random),
    n_cross_group_gender = sum(ego_group_gender != alter_group_gender),
    n_cross_group_gender_rel = sum(ego_group_gender_rel != alter_group_gender_rel),
    n_cross_group_gender_fix = sum(ego_group_gender_fix != alter_group_gender_fix),
    n_cross_group_chain = sum(ego_group_chain != alter_group_chain),
    n_cross_group_min = sum(ego_group_min != alter_group_min, na.rm = TRUE),
  ) %>%
  mutate(
    classid = classid_this,
    run = run,
    network_indicator = network_indicator
  ) %>%
  select(classid, everything())

}


########################
### Run the analysis ###
########################



### Load all data sets and results from
### optimization routine

load("data_prepared/cils_prepared_sim.RData")
load("optimization_results/all_minimal_groups_sim.RData")

### Determine classrooms to be included in analysis
all_classids <-  all_minimal %>% 
  distinct(classid) %>% 
  pull(classid)

### Number of runs (for stochastic procedures, i.e.
### random allocation, gender allocation, chain allocation)
n_runs <- 10


### Get classroom-level analysis on cross-cohort ties
res_n_ties <- foreach(
  classid = all_classids,
  .packages = c("tidyverse"),
  .export = c("classroom_cohorting_ties"),
  .combine = rbind
  ) %dopar% {


# Repeated individual classroom-level analysis
classroom_cohorting_ties_repeat <- 1:n_runs %>% 
  map(
    classroom_cohorting_ties,
    cils_main = cils_main,
    cils_class = cils_class_w1,
    classid_this = classid,
    network_indicator = "tos",
    all_minimal = all_minimal
  ) %>% 
  bind_rows()

# Aggregation at the classroom level
classroom_cohorting_ties_average <- classroom_cohorting_ties_repeat %>% 
  pivot_longer(
    -c(classid, network_indicator, run),
    names_to = "scenario",
    values_to = "value"
  ) %>%
  group_by(classid, scenario) %>%
  summarize(
    n_type = mean(value, na.rm = TRUE)
  )

classroom_cohorting_ties_average

}


### Generate statistics and figures ###

# Prepare data for graphs
res_n_ties_prep <- res_n_ties %>%
  ungroup() %>%
  filter(!scenario %in% c("n_students", "n_students_with_ties")) %>%
  mutate(
    scenario_clean = 
      case_when(
        scenario == "n_ties"~"Total ties in network",
        scenario == "n_cross_group_min"~"Optimized cohorting",
        scenario == "n_cross_group_random"~"Random cohorting\n(average)",
        scenario == "n_cross_group_gender"~"Gender-split cohorting\n(average)",
        scenario == "n_cross_group_gender_fix"~"Gender split cohorting\n(unequal group size)",
        scenario == "n_cross_group_gender_rel"~"Gender split cohorting\n(some unequal group size)",
        scenario == "n_cross_group_chain"~"Network chain cohorting\n(average)"
    ),
    scenario_clean = fct_relevel(scenario_clean, "Optimized cohorting", "Network chain cohorting\n(average)", "Gender-split cohorting\n(average)", "Random cohorting\n(average)"),
    country = case_when(
      classid > 400000~"SW",
      classid > 300000~"NL",
      classid > 200000~"GE",
      classid > 100000~"EN",
      TRUE~NA_character_
    )
  )


# Mean number of cross-cohort-ties by cohorting strategy 
res_n_ties_prep %>%
  group_by(scenario_clean) %>%
  summarize(
    n_obs = n(),
    mean_ties = mean(n_type),
    median_ties = median(n_type)
  )


# Color hue for figure
color_values <- c(
    "Optimized cohorting" = "#d7191c",
    "Network chain cohorting\n(average)"  = "#fdae61",
     "Gender-split cohorting\n(average)" = "#abd9e9",
     "Random cohorting\n(average)" = "#2c7bb6",
     "Total ties in network" = "#a6611a"
  )



dir.create("ties_results")
setwd("ties_results")



# Distribution of number of cross-cohort-ties by cohorting stratety (Fig. 4)
res_n_ties_prep %>%
  bind_rows(res_n_ties_prep %>% mutate(country = "All Countries")) %>%
  filter(!scenario_clean %in% c("Gender split cohorting\n(unequal group size)", "Gender split cohorting\n(some unequal group size)")) %>%
  ggplot(aes(x = n_type, y = scenario_clean, color = scenario_clean, fill = scenario_clean)) +
  geom_density_ridges(stat = "binline", bins = 40, scale = 0.95, draw_baseline = FALSE, alpha = .5) +
  labs(
    x = "Number of cross-cohort out-of-school ties",
    y = "",
    color = "Cohorting strategy",
    fill = "Cohorting strategy"
  ) +
  scale_color_manual(values = color_values, guide = guide_legend(nrow = 2)) +
  scale_fill_manual(values = color_values, guide = guide_legend(nrow = 2)) +
  scale_x_continuous(breaks = seq(0, 120, 30), limits = c(0, 120)) +
  facet_grid(~country) +
  theme_classic() +
  theme(
    legend.position = "off",
    axis.text.x = element_text(size = 10, angle = 30, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    strip.text.x  = element_text(size = 12),
  ) 

ggsave("Fig-4-Cross-Cohort-Ties.jpg", width = 8, height = 4)





# Color hue for figure
color_values <- c(
  "Gender-split cohorting\n(equal cohort size)" = "#d7191c",
  "Gender-split cohorting\n(some unequal cohort size)" = "#fdae61",
  "Gender-split cohorting\n(unequal cohort size)" = "#2c7bb6"
)



# Distribution of number of cross-cohort-ties for different gender-split cohorting strateties (Fig. A1)
res_n_ties_prep %>%
  bind_rows(res_n_ties_prep %>% mutate(country = "All Countries")) %>%
  filter(scenario_clean %in% c("Gender-split cohorting\n(average)", "Gender split cohorting\n(unequal group size)", "Gender split cohorting\n(some unequal group size)")) %>%
  mutate(
    scenario_clean = ifelse(scenario_clean == "Gender-split cohorting\n(average)", "Gender-split cohorting\n(equal group size)", as.character(scenario_clean)),
    scenario_clean = fct_relevel(scenario_clean, "Gender-split cohorting\n(equal group size)")
  ) %>%
  ggplot(aes(x = n_type, y = scenario_clean, color = scenario_clean, fill = scenario_clean)) +
  geom_density_ridges(stat = "binline", bins = 40, scale = 0.95, draw_baseline = FALSE, alpha = .5) +
  labs(
    x = "Number of cross-cohort out-of-school ties",
    y = "",
    color = "Cohorting strategy",
    fill = "Cohorting strategy"
  ) +
  scale_color_manual(values = color_values, guide = guide_legend(nrow = 2)) +
  scale_fill_manual(values = color_values, guide = guide_legend(nrow = 2)) +
  scale_x_continuous(breaks = seq(0, 110, 20)) +
  facet_grid(~country) +
  theme_classic() +
  theme(
    legend.position = "off",
    axis.text.x = element_text(size = 10, angle = 30, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    strip.text.x  = element_text(size = 12),
  ) 
  
ggsave("Supp-A-Fig-1-Cross-Cohort-Ties-Gender.jpg", width = 9, height = 3)

# q("no")


