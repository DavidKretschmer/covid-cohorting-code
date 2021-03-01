###############################################################
### Supplementary Material E:                               ###
### Differences by probability of out-of-schol contacts     ###
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

"transmission_prob/2021-02-23___17-19-59_en_0.05",
"transmission_prob/2021-02-23___18-16-14_ge_0.05",
"transmission_prob/2021-02-23___18-16-17_sw_0.05",
"transmission_prob/2021-02-23___19-56-31_ge_0.05",
"transmission_prob/2021-02-23___20-05-29_en_0.05",
"transmission_prob/2021-02-23___20-05-29_nl_0.05",
"transmission_prob/2021-02-23___20-05-29_sw_0.05",
"transmission_prob/2021-02-24___22-11-33_nl_0.05",
"transmission_prob/2021-02-23___21-12-10_sw_0.1",
"transmission_prob/2021-02-23___21-12-12_nl_0.1",
"transmission_prob/2021-02-23___21-12-13_en_0.1",
"transmission_prob/2021-02-23___21-12-13_ge_0.1",
"transmission_prob/2021-02-23___21-35-44_ge_0.1",
"transmission_prob/2021-02-23___21-35-45_en_0.1",
"transmission_prob/2021-02-23___21-35-45_sw_0.1",
"transmission_prob/2021-02-23___21-59-53_nl_0.1",
#"transmission_prob/2021-02-23___21-47-25_ge_0.4",
#"transmission_prob/2021-02-23___21-59-54_en_0.4",
#"transmission_prob/2021-02-23___21-59-57_sw_0.4",
#"transmission_prob/2021-02-23___22-07-20_ge_0.4",
#"transmission_prob/2021-02-23___22-07-22_en_0.4",
#"transmission_prob/2021-02-23___22-41-06_sw_0.4",
#"transmission_prob/2021-02-23___22-45-30_nl_0.4",
#"transmission_prob/2021-02-23___22-48-56_nl_0.4",
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

setwd("transmission_prob")

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
    pr_out_of_school = paste0("Daily probability of contact: ", pr_out_of_school),
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


# Summarize data across classroom within a given country
res_unclustered <- res_unclustered_classroom %>%
ungroup() %>%
group_by(indicator, mode, type, inf_asymptomatic, susceptibility_num, susceptibility, share_subclinical_num, share_subclinical, pr_out_of_school, scenario) %>%
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
  group_by(pr_out_of_school, mode, scenario, type, indicator) %>%
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
    caption = "Note: Proportions and 95% confidence intervals. Numbers above excess proportion quarantined indicate proportion to be added to obtain total proportion quarantined (+ 1/2 of Proportion clinical)."
  ) +
  scale_color_manual(values = color_values) +
  scale_fill_manual(values = color_values) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_nested(indicator~pr_out_of_school + mode + scenario, scales = "free")


ggsave(filename = "Supp-E-Fig-1-Cohorting-Strategies-Contact.jpg", width = 16, height = 10)





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
#     caption = "Note: Results across entire parameter space are in Extended Data Figure 1."
#   ) +
#   scale_color_manual(values = color_values_no) +
#   scale_fill_manual(values = color_values_no) +
#   theme_classic() +
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank()
#   ) +
#   facet_nested(~pr_out_of_school + scenario, scales = "free")

# ggsave(filename = "Supp-E-Fig-0-Random-Cohorting-Contact.jpg", width = 10, height = 4)




