###############################################################
### Supplementary Material A:                               ###
### Differences by gender-split cohorting approaches        ###
###############################################################



##############################
### Load relevant packages ###
##############################

library(ggh4x)
library(estimatr)
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(forcats)
library(ggpubr)
library(ggplot2)

setwd("transmission_gender")

############################
### Load all of the data ###
############################

color_values <- c(
    "Gender-split cohorting\n(equal cohort size)" = "#d7191c",
    "Gender-split cohorting\n(some unequal cohort size)" = "#fdae61",
    "Gender-split cohorting\n(unequal cohort size)" = "#2c7bb6"
  )


### Names of the results folders

names_est <- c(

"2021-02-27___20-49-50_ge_0.2",
"2021-02-27___20-49-52_en_0.2",
"2021-02-27___20-49-57_sw_0.2",
"2021-02-27___20-49-58_nl_0.2"

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
    share_subclinical_num = (1 - share_symptomatic) %>% round(2),
    share_subclinical = paste0("Prop.\nsubclinical:\n", 1 - share_symptomatic),
    scenario = case_when(
      susceptibility_num == .05 & share_subclinical_num == .2~"Transmission\ndynamics:\nlow",
      susceptibility_num == .15 & share_subclinical_num == .5~"Transmission\ndynamics:\nmedium",
      susceptibility_num == .25 & share_subclinical_num == .8~"Transmission\ndynamics:\nhigh",
      TRUE~NA_character_
    ) %>% fct_relevel("Transmission\ndynamics:\nlow", "Transmission\ndynamics:\nmedium"),
    type = case_when(
      type == "gender"~"Gender-split cohorting\n(equal cohort size)",
      type == "gender_rel"~"Gender-split cohorting\n(some unequal cohort size)",
      type == "gender_fix"~"Gender-split cohorting\n(unequal cohort size)",
      TRUE~type
    ),
    type = fct_relevel(type, "Gender-split cohorting\n(equal cohort size)", "Gender-split cohorting\n(some unequal cohort size)")
  )



# Summarize data at classroom level
res_unclustered_classroom <- res_analysis %>% 
  group_by(classid, country, mode, type, inf_asymptomatic, susceptibility_num, susceptibility, share_subclinical_num, share_subclinical, pr_out_of_school, scenario) %>%
  summarize(
    `Proportion infected` = mean(share_inf),
    `Excess proportion quarantined` = mean(share_qua),
    `Proportion of spread across cohorts` = mean(groups_affected),
    ) %>%
  pivot_longer(
    cols = c("Proportion infected", "Excess proportion quarantined", "Proportion of spread across cohorts"),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  mutate(
    indicator = fct_relevel(indicator, "Proportion of spread across cohorts", "Excess proportion quarantined")
  )


# Summarize data across classroom and countries
res_unclustered <- res_unclustered_classroom %>%
ungroup() %>%
group_by(indicator, mode, type, inf_asymptomatic, susceptibility_num, susceptibility, share_subclinical_num, share_subclinical, pr_out_of_school, scenario) %>%
summarize(
  conf.low = t.test(value)$conf.int[1],
  conf.high = t.test(value)$conf.int[2],
  value = mean(value)
)




####################################
## Comparing cohorting strategies ##
####################################


add_quarantine <- res_unclustered %>%
  filter(
    !is.na(scenario),
    indicator == "Excess proportion quarantined",
    type == "Gender-split cohorting\n(some unequal cohort size)",
  ) %>%
  group_by(mode, scenario, type, indicator) %>%
  summarize(
    mean_prob = .125,
    max_qua = max(value),
    label = paste0("+", mean((1 - share_subclinical_num)/2))
  )


res_unclustered %>%
  ungroup() %>%
  filter(!is.na(scenario)) %>%
  ggplot(aes(x = type, y = value, fill = type, color = type)) + 
  geom_col(width = 1, position = position_dodge(0.5)) + 
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), color = "black", alpha = .7, width = .3) +
  geom_text(data = add_quarantine, aes(x = type, y = max_qua, label = label), color = "black", size = 2.5, vjust = -2, hjust = .7) + 
  labs(
    x = "Type of intervention",
    y = "",
    fill = "",
    color = "",
    caption = "Note: Proportions and 95% confidence intervals. Numbers above excess proportion quarantined indicate 
    proportion to be added to obtain total proportion quarantined (+ 1/2 of Proportion clinical)." 
  ) +
  scale_color_manual(values = color_values) +
  scale_fill_manual(values = color_values) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_nested(indicator~mode + scenario, scales = "free")


ggsave(filename = "Supp-A-Fig-2-Cohorting-Strategies-Gender.jpg", width = 7, height = 10)


