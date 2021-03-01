
# Load relevant packages
library(foreach)
library(doParallel)
library(network)
library(ggnetwork)
library(tidyverse)

country <- "sim"

# Load input data
load(paste0("data_prepared", "/", "cils_prepared_", country, ".RData"))

path_to_minimization <- paste0("optimization_results", "/", "all_minimal_groups_", country, ".RData")

load(path_to_minimization)

dir.create("network_graph")
setwd("network_graph")



all_classids <-  all_minimal %>% 
  distinct(classid) %>% 
  pull(classid)



generate_network_graphs <- function(
  classid_this = all_classids[1],
  cils_main = cils_main,
  cils_class = cils_class_w1,
  all_minimal = all_minimal,
  network_indicator = "tos"
) {

ids <- all_minimal %>% 
  filter(classid == classid_this) %>%
  select(youthid)


#####################################
### Out-of-school contact network ###
#####################################

### Genereate edelist
ego_alter <- cils_class %>% 
  filter(youthid %in% (ids %>% pull(youthid))) %>% # select relevant network variables
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
  filter(alterid %in% (ids %>% pull(youthid))) %>%
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

network_out <- ego_alter


random <- ids %>%
  mutate( # random allocation, sample half the class for one group
    sam = list(sample(youthid, length(youthid)/2)),
    group_random = ifelse(youthid %in% unlist(sam), 1, 0)
  ) %>%
  select(youthid, group_random) %>%
  rename(ego = youthid)



### Gender-split cohorting
gender <- ids %>%
  left_join(cils_main, by = "youthid") %>%
  mutate( # allocation by gender, ensuring equal group size
    random = rbinom(length(youthid), 1, .5), # for simplicity, impute missing gender data for now
    girl_help = case_when(
      is.na(girl) & random == 1 ~"Girl",
      is.na(girl) & random == 0 ~"Boy",
      TRUE ~girl
    )
  ) %>%
  mutate(
    n_diff = sum(girl_help == "Girl", na.rm = TRUE) - sum(girl_help == "Boy", na.rm = TRUE), # gender imbalance

    group_gender_fix = ifelse(girl_help == "Girl", 1, 0),

    group_gender = ifelse(girl_help == "Girl", 1, 0), # girls are group 1
    to_group_0 = ifelse( # if more girls, reallocate some girls to group 0
      test = n_diff > 1, 
      yes = list(sample(youthid[girl_help == "Girl"], abs(n_diff / 2))), 
      no = list(c(""))
    ),
    to_group_1 = ifelse( # if more boys, reallocate some boys to group 1
      test = n_diff < -1, 
      yes = list(sample(youthid[girl_help == "Boy"], abs(n_diff / 2))), 
      no = list(c(""))
    ), # do the reallocation
    group_gender = ifelse(youthid %in% unlist(to_group_0), 0, group_gender),
    group_gender = ifelse(youthid %in% unlist(to_group_1), 1, group_gender),

    group_gender_rel = ifelse(girl_help == "Girl", 1, 0), # girls are group 1
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



### Network chain cohorting

# Treat ties are directed (for choice)
network_out_double <- network_out %>% 
  bind_rows(
    network_out %>% 
    rename(alter = ego, ego = alter) %>%
    select(ego, alter)
  )

  # Size of the chain-based group
  group_size <- ceiling(nrow(ids)/2)

  # Starting node
  start_id <- ifelse(
    test = nrow(network_out_double) > 0,
    yes = network_out_double %>% pull(ego) %>% sample(1),
    no = ids %>% pull(youthid) %>% sample(1) # if no out-of-school network: random start
  )

  while( length(start_id) < group_size ) { # as long as the group is not yet to large

    seq_id <- network_out_double %>%
    filter(ego %in% start_id) %>%
    filter(!alter %in% start_id) %>%
    distinct(alter) %>%
    pull(alter)
    # get all of the group members' out-of-school contacts
    # not yet in the group

    if (length(seq_id) == 0) {

      seq_id <- ids %>% filter(!youthid %in% start_id) %>% pull(youthid) %>% sample(1)
      # if there are no new contact, pick a random student for the group

    }

    if (length(start_id) + length(seq_id) <= group_size) {

      start_id <- c(start_id, seq_id)
      # if all new contacts fit into group, all are put into the group

    }  else {

      start_id <- c(start_id, sample(seq_id, size = group_size - length(start_id)))
      # if too many new contacts, select random subsample

    }

  }


chain <- ids %>%
    mutate(group_chain = ifelse(youthid %in% start_id, 1, 0)) %>%
    rename(ego = youthid)



### Optimized cohorting
minimal <- all_minimal %>% 
      filter(youthid %in% (ids %>% pull(youthid))) %>%
      select(youthid, group_min) %>%
      rename(ego = youthid, group = group_min)



### All ties in network 
in_network_ids <- network_out %>% 
pivot_longer(
  cols = everything()
) %>%
distinct(value) %>%
pull(value)

### Add students without any ties
added_edges <- ids %>% 
  filter(!youthid %in% in_network_ids) %>%
  rename(ego = youthid) %>%
  mutate(alter = ego) 


### Full network
nw_network_out <- network_out %>% 
  bind_rows(added_edges) %>%
  mutate_all(~as.character(.)) %>%
  arrange(ego)

nw <- as.network(nw_network_out, directed = FALSE) 

# Add group information to network

nw %v% "group_random" <- random %>% arrange(ego) %>% pull(group_random) %>% as.character()
nw %v% "group_gender" <- gender %>% arrange(ego) %>% pull(group_gender) %>% as.character()
nw %v% "group_gender_fix" <- gender %>% arrange(ego) %>% pull(group_gender_fix) %>% as.character()
nw %v% "group_gender_rel" <- gender %>% arrange(ego) %>% pull(group_gender_rel) %>% as.character()
nw %v% "group_chain" <- chain %>% arrange(ego) %>% pull(group_chain) %>% as.character()
nw %v% "group_minimal" <- minimal %>% arrange(ego) %>% pull(group) %>% as.character()


# Prepare network for plotting
network_for_plot <- ggnetwork(nw) %>%
  pivot_longer(
    -c(x, y, vertex.names, xend, yend),
    names_to = "type",
    values_to = "Group"
  ) %>%
  mutate(
    type = case_when(
      type == "group_random"~"Random cohorting",
      type == "group_gender"~"Gender-split cohorting",
      type == "group_gender_fix"~"Gender-split cohorting (unequal group size)",
      type == "group_gender_rel"~"Gender-split cohorting (some unequal group size)",
      type == "group_chain"~"Network chain cohorting",
      type == "group_minimal"~"Optimized cohorting"
    ) %>% fct_relevel("Random cohorting", "Gender-split cohorting", "Network chain cohorting", "Optimized cohorting")
  ) %>%
  mutate(classid = classid_this)


# network_for_plot %>%
#   filter(type %in% c("Random cohorting", "Gender-split cohorting", "Network chain cohorting", "Optimized cohorting")) %>%
#   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#   geom_edges() + 
#   geom_nodes(aes(color = Group), size = 2) +
#   theme_blank() + 
#     theme(
#     legend.position = "bottom"
#   ) +
#   facet_grid(~type)

return(network_for_plot)

}



all_network_plots <- all_classids[1:10] %>% 
  map(
    generate_network_graphs,
    cils_main = cils_main,
    cils_class = cils_class_w1,
    all_minimal = all_minimal,
    network_indicator = "tos"
) %>% bind_rows()



all_network_plots %>%
  filter(
    type %in% c("Random cohorting", "Gender-split cohorting", "Network chain cohorting", "Optimized cohorting"),
    classid %in% c(100004)
  ) %>%
  mutate(
    Group = ifelse(Group == 0, "Cohort 1", "Cohort 2")
  ) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() + 
  geom_nodes(aes(color = Group), size = 2.5) +
  labs(
    color = "",
    caption = "Note: Nodes represent students and ties among nodes represent out-of-school contacts with classmates.\nColors indicate the cohort to which students have been allocated. Cohorts have the same size."
  ) +
  theme_blank() + 
    theme(
    legend.position = "bottom",
    panel.spacing = unit(2, "lines"),
    plot.caption = element_text(size = 12, hjust = 0),
    strip.text.x = element_text(size = 12),
    legend.text = element_text(size = 12)

  ) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~type)

ggsave(filename = "Figure-1-Network-Example.jpg", width = 8, height = 6.8)
 


all_network_plots %>%
  filter(
    type %in% c("Random cohorting", "Gender-split cohorting", "Network chain cohorting", "Optimized cohorting")
  ) %>%
  mutate(
    Group = ifelse(Group == 0, "Cohort 1", "Cohort 2")
  ) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() + 
  geom_nodes(aes(color = Group), size = 1.5) +
  labs(
    color = ""
  ) +
  theme_blank() + 
    theme(
    legend.position = "bottom",
    panel.spacing = unit(2, "lines")
  ) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(classid~type)


# q("no")