

# Load relevant packages
library(foreach)
library(doParallel)
library(tidyverse)




##################################
### Out-of-school network ties ###
##################################


network_out_of_school <- function(
  ids = ids,
  cils_class = cils_class_w1,
  network_indicator = "tos"
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


network_out_summary <- function(
  classid_this = 400101,
  cils_main = cils_main,
  cils_class = cils_class_w1,
  all_minimal = all_minimal,
  network_indicator = "tos"
) {


ids <- all_minimal %>% 
  filter(classid == classid_this) %>%
  select(youthid)

# Out-of-school contact network
 network_out <- network_out_of_school(
  ids = ids,
  cils_class = cils_class,
  network_indicator = network_indicator
)


# Include all one-sided ties
network_contacts <- network_out %>% 
  bind_rows(
    network_out %>% 
    rename(alter = ego, ego = alter) %>%
    select(ego, alter)
  )  


# Number of ties of all of those in the network data
n_ties_network <- network_contacts %>%
  group_by(ego) %>%
  summarize(
    n_ties = n()
  )

# Merging those with zero ties
n_ties_all <- ids %>%
  left_join(
    n_ties_network,
    by = c("youthid" = "ego")
  ) %>%
  mutate(
    n_ties = ifelse(is.na(n_ties), 0, n_ties)
  )

 
return(n_ties_all) 

}


load("data_prepared/cils_prepared_sim.RData")
load("optimization_results/all_minimal_groups_sim.RData")



# Number of classrooms and students
all_minimal %>% distinct(youthid)
all_minimal %>% distinct(classid, n_students)


# Generate all classroom-level out-of-school contact networks
res <- all_minimal %>% 
  distinct(classid) %>%
  pull(classid) %>%
  map(
    network_out_summary,
    cils_main = cils_main,
    cils_class = cils_class_w1,
    all_minimal = all_minimal,
    network_indicator = "tos"
  ) %>% 
  bind_rows() %>%
  rename(ego = youthid)



# Calculate mean number of out-of-school contacts
res %>%
  summarize( # aggregate across all individuals
    n_students = n(),
    ties_mean_trim = mean(n_ties, trim = .1),
    ties_mean = mean(n_ties),
    ties_median = median(n_ties),
    ties_max = max(n_ties),
    ties_min = min(n_ties),
    ties_05 = quantile(n_ties, probs = .05),
    ties_95 = quantile(n_ties, probs = .95),
  )


res %>% 
  ggplot(aes(x = n_ties)) + 
  geom_density() +
  theme_classic()

# q("no")