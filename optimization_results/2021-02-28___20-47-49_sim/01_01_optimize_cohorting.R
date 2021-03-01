
# Load packages
library(foreach)
library(doParallel)
library(tidyverse)


# Set up the cluster
procs <- 2 # as.numeric(Sys.getenv("SLURM_JOB_CPUS_PER_NODE"))
registerDoParallel(procs)

# Function to minimize the number of cross-group ties
# for each class (classid)
minimize_relations_class <- function(
  cils_main = cils_main,
  cils_class = cils_class_w1,
  classid_this = 100102,
  network_indicator = "tos",
  id_version = "small",
  optimize_cutoff = 26,
  randomize_number = 100000
) {


if (id_version == "small") {

ids <- cils_main %>% filter(classid == classid_this) %>%
  full_join(
    cils_class %>% 
      filter(classid == classid_this) %>% 
      select(youthid), 
    by =  "youthid") %>%
  pull(youthid)

}


if (id_version == "large") {

ids <- cils_main %>% 
  filter(classid == classid_this) %>%
  full_join(
    cils_class %>% 
      filter(classid == classid_this) %>% 
      select(
        youthid,
        starts_with(paste0(network_indicator, "_"))
      ), 
    by =  "youthid") %>%
  select(
    youthid, 
    starts_with(paste0(network_indicator, "_"))
  ) %>%
  select(
    -starts_with(paste0(network_indicator, "_", "0"))
  ) %>%
  mutate_all(~as.character(.) %>% as.numeric(.)) %>%
  pivot_longer(
    cols = everything()
  ) %>%
  filter(!is.na(value)) %>%
  distinct(value) %>%
  rename(youthid = value) %>%
  pull(youthid)

}

### Genereate edelist
ego_alter <- cils_class %>% 
  filter(youthid %in% ids) %>% # select relevant network variables
  select(
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
    alterid = as.character(alterid) %>% as.numeric()
  ) %>%
  filter(!is.na(alterid)) %>% # filter non-nominations
  filter(youthid != alterid) %>% # filter self-nomations
  filter(alterid %in% ids) %>%
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

### Simplify setup for fast processing
ego <- ego_alter %>% pull(youthid) # ego IDs
alter <- ego_alter %>% pull(alterid) # alter IDs



### Run the optimization procedure
i <- 1 # Start with the first combination
min_val <- NA # No info on minimum cross-group ties yet
comb_min_val <- NA

if (length(ids) < optimize_cutoff) { # If classroom size smaller than the cutoff: explicit optimization

### Get all possible combinations for forming group
combinations <- combn(x = ids, m = length(ids)/2)


while(
  i < ncol(combinations) & # while there are still combinations
  (is.na(min_val) | min_val > 0) # and no combination with zero cross-group ties has been found yet
) {

  val <- sum(ego %in% combinations[,i] != alter %in% combinations[,i]) # get number of cross-group ties
  comb_min_val <- ifelse(is.na(min_val) | val < min_val, i, comb_min_val) # save combination with fewest ties
  min_val <- ifelse(is.na(min_val) | val < min_val, val, min_val) # replace min_val if lowest so far
  i <- i + 1 

}

combs_of_minimal <- combinations[,comb_min_val] # optimal combination

} 


if (length(ids) >= optimize_cutoff) { # If classroom size reaches cutoff: get optimal from random draws

while(
  i <= randomize_number & # while there are still combinations
  (is.na(min_val) | min_val > 0) # and no combination with zero cross-group ties has been found yet
) {

  this_group_random <- sample(ids, length(ids)/2)

  val <- sum(ego %in% this_group_random != alter %in% this_group_random) # get number of cross-group ties
  comb_min_val <- ifelse(is.na(min_val) | val < min_val, list(this_group_random), comb_min_val)  # save combination with fewest ties
  min_val <- ifelse(is.na(min_val) | val < min_val, val, min_val) # replace min_val if lowest so far
  i <- i + 1 

}

combs_of_minimal <- comb_min_val[[1]] # optimal combination


}

tibble(youthid = ids) %>%
  mutate(
    classid = classid_this,
    n_students = length(ids),
    group_min = ifelse(youthid %in% combs_of_minimal, 1, 0),
    network_indicator = network_indicator,
    n_cross_group_min = min_val,
    optimize_cutoff = optimize_cutoff,
    randomize_number = randomize_number,
    id_version = id_version
  )



}


# Load the prepared data
load(paste0("data_prepared", "/", "cils_prepared", "_", country, ".RData"))

### Get all classids
all_classids <- cils_main %>%
group_by(classid) %>%
  summarize(
    n_students = n(),
    n_girl_miss = sum(is.na(girl))
  ) %>%
  filter(n_students >= 20) %>%
  filter(n_girl_miss == 0) %>%
  distinct(classid) %>% 
  pull(classid)



### Run algorithm across all class ids
all_minimal <- foreach(
  classid_i = all_classids,
  .packages = c("tidyverse"),
  .export = c("minimize_relations_class"),
  .combine = rbind
  ) %dopar% {

  minimize_relations_class(
    cils_main = cils_main,
    cils_class = cils_class_w1,
    classid_this = classid_i,
    network_indicator = "tos",
    id_version = "small",
    optimize_cutoff = 33,
    randomize_number = 1000000
  ) %>%
  mutate(classid = classid_i)

}



### Save results
save(all_minimal, file = paste0(path_get_date_run, "/", "all_minimal_groups", "_", country, ".RData"))


# q("no")
