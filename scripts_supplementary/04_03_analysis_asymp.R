###############################################################
### Supplementary Material C:                               ###
### Differences by infectiousness of subclinical infections ###
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

"transmission_inf_asymptomatic/2021-02-23___16-32-30_en_0.3",
"transmission_inf_asymptomatic/2021-02-23___16-32-34_ge_0.3",
"transmission_inf_asymptomatic/2021-02-23___16-32-34_nl_0.3",
"transmission_inf_asymptomatic/2021-02-23___16-32-38_sw_0.3",
"transmission_inf_asymptomatic/2021-02-23___16-33-05_ge_0.3",
"transmission_inf_asymptomatic/2021-02-23___16-52-15_sw_0.3",
"transmission_inf_asymptomatic/2021-02-23___17-09-56_nl_0.3",
"transmission_inf_asymptomatic/2021-02-23___17-09-57_en_0.3",

"transmission_main/2021-02-23___15-24-14_sw_0.2",
"transmission_main/2021-02-23___15-24-17_en_0.2",
"transmission_main/2021-02-23___15-24-17_ge_0.2",
"transmission_main/2021-02-23___15-24-44_en_0.2",
"transmission_main/2021-02-23___15-24-44_ge_0.2",
"transmission_main/2021-02-23___15-24-44_sw_0.2",
"transmission_main/2021-02-23___15-24-49_nl_0.2",
"transmission_main/2021-02-23___15-25-16_nl_0.2"

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

setwd("transmission_inf_asymptomatic")

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
    inf_asymptomatic = paste0("Infectiousness asymptomatic: ", inf_asymptomatic),
    share_subclinical_num = (1 - share_symptomatic) %>% round(2),
    share_subclinical = paste0("Prop.\nsubclinical:\n", 1 - share_symptomatic),
    scenario = case_when(
      susceptibility_num == .05 & share_subclinical_num == .2~"Transmission\ndynamics:\nlow",
      susceptibility_num == .15 & share_subclinical_num == .5~"Transmission\ndynamics:\nmedium",
      susceptibility_num == .25 & share_subclinical_num == .8~"Transmission\ndynamics:\nhigh",
      TRUE~NA_character_
    ) %>% fct_relevel("Transmission\ndynamics:\nlow", "Transmission\ndynamics:\nmedium"),
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
  group_by(classid, country, mode, type, inf_asymptomatic, susceptibility_num, susceptibility, share_subclinical_num, share_subclinical, pr_out_of_school, scenario, share_high_risk) %>%
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
group_by(indicator, mode, type, inf_asymptomatic, susceptibility_num, susceptibility, share_subclinical_num, share_subclinical, pr_out_of_school, scenario, share_high_risk) %>%
summarize(
  conf.low = t.test(value)$conf.int[1],
  conf.high = t.test(value)$conf.int[2],
  value = mean(value),
)





####################################
## Comparing cohorting strategies ##
####################################


add_quarantine <- res_unclustered %>%
  filter(
    !is.na(scenario),
    indicator == "Excess proportion quarantined",
    type == "Gender-split cohorting",
  ) %>%
  group_by(inf_asymptomatic, pr_out_of_school, mode, scenario, type, indicator) %>%
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
  geom_text(data = add_quarantine, aes(x = type, y = max_qua, label = label), color = "black", size = 2.5, vjust = -1.5, hjust = 0) + 
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
  facet_nested(indicator~inf_asymptomatic + mode + scenario, scales = "free")


ggsave(filename = "Supp-C-Fig-1-Cohorting-Strategies-Infectious.jpg", width = 16, height = 10)





#######################################
## No cohorting vs. random cohorting ##
#######################################


# color_values_no <- c(
#     "Random cohorting:\nWeekly rota-system" = "#abd9e9",
#     "Random cohorting:\nSame-day instruction" = "#2c7bb6",
#     "No cohorting" = "#a6611a"
#   )

# res_unclustered %>%
#   filter(!is.na(scenario)) %>%
#   filter(indicator %in% c("Proportion infected")) %>%
#   filter(type %in% c("No cohorting", "Random cohorting")) %>% 
#   mutate(
#     type_helper = case_when(
#       type == "No cohorting" & mode == "Same-day instruction"~"No cohorting",
#       type == "Random cohorting" & mode == "Same-day instruction"~"Random cohorting:\nSame-day instruction",
#       type == "Random cohorting" & mode == "Weekly rota-system"~"Random cohorting:\nWeekly rota-system",
#       TRUE~NA_character_
#     )
#   ) %>%
#   filter(!is.na(type_helper)) %>%
#   ggplot(aes(x = type_helper, y = value, fill = type_helper, color = type_helper)) + 
#   geom_col() + 
#   geom_errorbar(aes(ymax = conf.high, ymin = conf.low), color = "black", alpha = .7, width = .3, size = .3) +
#   labs(
#     x = "Type of intervention",
#     y = "Proportion infected",
#     fill = "",
#     color = "",
#     caption = "Note: Proportions and 95% confidence intervals."
#   ) +
#   scale_color_manual(values = color_values_no) +
#   scale_fill_manual(values = color_values_no) +
#   theme_classic() +
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank()
#   ) +
#   facet_nested(~inf_asymptomatic + scenario, scales = "free")

# ggsave(filename = "Supp-C-Fig-0-Random-Cohorting-Infectious.jpg", width = 8, height = 4)




