
###############################################################
### Supplementary Material F:                               ###
### Differences by whether teachers are included in model   ###
###############################################################


##############################
### Load relevant packages ###
##############################

library(ggh4x)
library(ggpubr)
library(tidyverse)


############################
### Load all of the data ###
############################

color_values <- c(
    "Optimized cohorting" = "#d7191c",
    "Network chain cohorting"  = "#fdae61",
    "Gender-split cohorting" = "#abd9e9",
    "Random cohorting" = "#2c7bb6",
    "No cohorting" = "#a6611a"
  )


### Names of the results folders

names_est <- c(

  "transmission_main/2021-02-28___21-15-09_sim_0.2",
  "transmission_teacher/2021-02-28___21-15-07_sim_0.1"
  

)


# Load all data
res_complete <- tibble()

for (names in names_est) {

load(paste0(names, "/", "res_all_data.RData"))
res_complete <- res_complete %>% bind_rows(res_all)

}

dim(res_complete)

options(width = 200)

########################
### Prepare the data ###
########################

# Where to save the results?

setwd("transmission_teacher")

folder <- paste0("results")

dir.create(folder)
setwd(folder)


# Prepare the data
res_analysis <- res_complete %>%   
  mutate(
    share_qua = share_qua - .5 * share_symptomatic,
    groups_affected = groups_affected - 1,
    country = case_when(
      classid > 400000~"SW",
      classid > 300000~"NL",
      classid > 200000~"GE",
      classid > 100000~"EN",
      TRUE~NA_character_
    ),
    classid = as.factor(classid),
    mode = ifelse(mode == "parallel", "Same-day instruction", "Weekly rota-system"),
    susceptibility_num = susceptibility,
    susceptibility = paste0("Baseline probability of\ninfection upon contact: ", susceptibility),
    teacher = ifelse(is.na(teacher_affected), "No teacher", "Teacher"),
    share_subclinical_num = (1 - share_symptomatic) %>% round(2),
    share_subclinical = paste0("Prop.\nsubclinical:\n", 1 - share_symptomatic),
    scenario = case_when(
      susceptibility_num == .05 & share_subclinical_num == .2~"Transmission dynamics: low",
      susceptibility_num == .15 & share_subclinical_num == .5~"Transmission dynamics: medium",
      susceptibility_num == .25 & share_subclinical_num == .8~"Transmission dynamics: high",
      TRUE~NA_character_
    ) %>% fct_relevel("Transmission dynamics: low", "Transmission dynamics: medium"),
    type = case_when(
      type == "chain"~"Network chain cohorting",
      type == "random"~"Random cohorting",
      type == "gender"~"Gender-split cohorting",
      type == "minimal"~"Optimized cohorting",
      type == "all"~"No cohorting",
      TRUE~type
    ),
    type = fct_relevel(type, "No cohorting", "Random cohorting", "Gender-split cohorting", "Network chain cohorting")
  ) %>%
  filter(!is.na(scenario))


# Summarize data at classroom level
res_unclustered_classroom <- res_analysis %>% 
  group_by(classid, country, mode, type, teacher, inf_asymptomatic, susceptibility_num, susceptibility, share_subclinical_num, share_subclinical, pr_out_of_school, scenario, share_high_risk) %>%
  summarize(
    `Proportion infected` = mean(share_inf),
    `Excess proportion quarantined` = mean(share_qua),
    `Proportion of spread across cohorts` = mean(groups_affected),
    `Proportion teacher infected` = mean(teacher_affected)
    ) %>%
  pivot_longer(
    cols = c("Proportion infected", "Excess proportion quarantined", "Proportion of spread across cohorts", "Proportion teacher infected"),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  mutate(
    indicator = fct_relevel(indicator, "Proportion of spread across cohorts", "Excess proportion quarantined", "Proportion infected")
  )


# Summarize data across classroom and countries: model with teachers
res_unclustered_teacher <- res_unclustered_classroom %>%
ungroup() %>%
filter(teacher == "Teacher") %>%
group_by(indicator, mode, type, teacher, inf_asymptomatic, susceptibility_num, susceptibility, share_subclinical_num, share_subclinical, pr_out_of_school, scenario, share_high_risk) %>%
summarize(
  conf.low = t.test(value)$conf.int[1],
  conf.high = t.test(value)$conf.int[2],
  value = mean(value),
)

# Summarize data across classroom and countries: model without teachers (standard model)
res_unclustered_no_teacher <- res_unclustered_classroom %>%
ungroup() %>%
filter(teacher == "No teacher") %>%
filter(indicator != "Proportion teacher infected") %>%
group_by(indicator, mode, type, teacher, inf_asymptomatic, susceptibility_num, susceptibility, share_subclinical_num, share_subclinical, pr_out_of_school, scenario, share_high_risk) %>%
summarize(
  conf.low = t.test(value)$conf.int[1],
  conf.high = t.test(value)$conf.int[2],
  value = mean(value),
)


# Combining results with and without teachers
res_unclustered <- res_unclustered_teacher %>%
  bind_rows(res_unclustered_no_teacher) %>%
  ungroup() %>%
  mutate(
    type = fct_relevel(type, "No cohorting", "Random cohorting", "Gender-split cohorting", "Network chain cohorting")
  )



######################################
# No cohorting vs. random cohorting ##
######################################


color_values_no <- c(
    "Random cohorting:\nWeekly rota-system" = "#abd9e9",
    "Random cohorting:\nSame-day instruction" = "#2c7bb6",
    "No cohorting" = "#a6611a"
  )

res_unclustered %>%
  filter(!is.na(scenario)) %>%
  filter(indicator %in% c("Proportion infected", "Proportion teacher infected")) %>%
  filter(type %in% c("No cohorting", "Random cohorting")) %>% 
  mutate(
    type_helper = case_when(
      type == "No cohorting" & mode == "Same-day instruction"~"No cohorting",
      type == "Random cohorting" & mode == "Same-day instruction"~"Random cohorting:\nSame-day instruction",
      type == "Random cohorting" & mode == "Weekly rota-system"~"Random cohorting:\nWeekly rota-system",
      TRUE~NA_character_
    )
  ) %>%
  filter(!is.na(type_helper)) %>%
  ggplot(aes(x = type_helper, y = value, fill = type_helper, color = type_helper)) + 
  geom_col() + 
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), color = "black", alpha = .7, width = .3, size = .3) +
  labs(
    x = "Type of intervention",
    y = "Proportion infected",
    fill = "",
    color = "",
    caption = "Note: Proportions and 95% confidence intervals."
  ) +
  scale_color_manual(values = color_values_no) +
  scale_fill_manual(values = color_values_no) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_nested(indicator~scenario + teacher, scales = "free")

ggsave(filename = "Supp-F-Fig-1-Random-Cohorting-Teacher-Compare.jpg", width = 6, height = 4)





####################################
## Comparing cohorting strategies ##
####################################


add_quarantine <- res_unclustered %>%
  filter(
    !is.na(scenario),
    indicator == "Excess proportion quarantined",
    type == "Gender-split cohorting",
  ) %>%
  group_by(inf_asymptomatic, teacher, pr_out_of_school, mode, scenario, type, indicator) %>%
  summarize(
    mean_prob = .125,
    max_qua = max(value),
    label = paste0("+", mean((1 - share_subclinical_num)/2))
  )


res_unclustered %>%
  ungroup() %>%
  filter(!is.na(scenario)) %>%
  filter(type %in% c("Random cohorting", "Gender-split cohorting", "Network chain cohorting", "Optimized cohorting")) %>%  
  ggplot(aes(x = type, y = value, fill = type, color = type)) + 
  geom_col(width = 1, position = position_dodge(0.5)) + 
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), color = "black", alpha = .7, width = .3) +
  #geom_text(data = add_quarantine, aes(x = type, y = max_qua, label = label), color = "black", size = 2.5, vjust = -3, hjust = -1) + 
  labs(
    x = "Type of intervention",
    y = "",
    fill = "",
    color = "",
    caption = "Note: Proportions and 95% confidence intervals. Numbers above excess proportion quarantined indicate proportion to be added to obtain total proportion quarantined (+ 1/2 of Proportion clinical). "
  ) +
  scale_color_manual(values = color_values) +
  scale_fill_manual(values = color_values) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_nested(indicator~mode + scenario + teacher, scales = "free")


ggsave(filename = "Supp-F-Fig-2-Cohorting-Strategies-Teacher-Compare.jpg", width = 12, height = 10)


