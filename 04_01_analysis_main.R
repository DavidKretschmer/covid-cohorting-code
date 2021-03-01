
###############################################################
### Main Analysis for Main Text,Extended Data Figures       ###
### and Supplementary Material A: County-specific results   ###
###############################################################


##############################
### Load relevant packages ###
##############################

library(ggh4x)
library(ggpubr)
library(tidyverse)

setwd("transmission_main")

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

  "2021-02-28___21-15-09_sim_0.2"

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
      type == "chain"~"Network chain cohorting",
      type == "random"~"Random cohorting",
      type == "gender"~"Gender-split cohorting",
      type == "minimal"~"Optimized cohorting",
      type == "all"~"No cohorting",
      TRUE~type
    ),
    type = fct_relevel(type, "No cohorting", "Random cohorting", "Gender-split cohorting", "Network chain cohorting")
  )


# Collect information on largest outbreaks
res_dist <- res_analysis %>% 
  filter(!is.na(scenario)) %>%
  filter(type != "No cohorting") %>%
  group_by(mode, type, inf_asymptomatic, susceptibility_num, susceptibility, share_subclinical_num, share_subclinical, pr_out_of_school, scenario) %>%
  mutate(
    `5% Largest Outbreaks` = quantile(share_inf, .95),
    `1% Largest Outbreaks` = quantile(share_inf, .99)
  ) %>%
  filter(share_inf > `5% Largest Outbreaks`) 


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


# Summarize data across classroom within a given country
res_unclustered_country <- res_unclustered_classroom %>%
ungroup() %>%
  group_by(indicator, country, mode, type, inf_asymptomatic, susceptibility_num, susceptibility, share_subclinical_num, share_subclinical, pr_out_of_school, scenario) %>%
  summarize(
    conf.low = t.test(value)$conf.int[1],
    conf.high = t.test(value)$conf.int[2],
    value = mean(value)
  )




#######################################
## No cohorting vs. random cohorting ##
#######################################


color_values_no <- c(
    "Random cohorting:\nWeekly rota-system" = "#abd9e9",
    "Random cohorting:\nSame-day instruction" = "#2c7bb6",
    "No cohorting" = "#a6611a"
  )


### Results across all classrooms and countries ###

res_unclustered %>%
  filter(!is.na(scenario)) %>%
  filter(indicator %in% c("Proportion infected")) %>%
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
    caption = "Note: Proportions and 95% confidence intervals. Results across entire parameter space
    are in Extended Data Figure 1."
    
  ) +
  scale_color_manual(values = color_values_no) +
  scale_fill_manual(values = color_values_no) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_nested(~scenario, scales = "free")

ggsave(filename = "Fig-3-Random-Cohorting.jpg", width = 5, height = 4)


### Extended Results across all classrooms and countries ###

res_unclustered %>%
  filter(indicator %in% c("Proportion infected")) %>%
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
  facet_nested(~susceptibility + share_subclinical, scales = "free")

ggsave(filename = "Ext-Fig-1-Random-Cohorting-Space.jpg", width = 10, height = 4)



### Results across all classrooms, by country ###

res_unclustered_country %>%
  filter(!is.na(scenario)) %>%
  filter(indicator %in% c("Proportion infected")) %>%
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
    caption = "Proportions and 95% confidence intervals."
  ) +
  scale_color_manual(values = color_values_no) +
  scale_fill_manual(values = color_values_no) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_nested(~scenario + country, scales = "free")

ggsave(filename = "Supp-A-Fig-1-Random-Cohorting-Countries.jpg", width = 8, height = 4)






####################################
## Comparing cohorting strategies ##
####################################

### Results across all classrooms and countries ###

# Get information on minimal share quarantined
add_quarantine <- res_unclustered %>%
  filter(
    !is.na(scenario),
    indicator == "Excess proportion quarantined",
    type == "Gender-split cohorting",
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
    caption = "Note: Proportions and 95% confidence intervals. Numbers above excess proportion quarantined indicate 
    proportion to be added to obtain total proportion quarantined (+ 1/2 of Proportion clinical). 
    Results across entire parameter space are in Extended Data Figure 2."
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


ggsave(filename = "Fig-5-Cohorting-Strategies.jpg", width = 7, height = 10)





### Extended Results across all classrooms and countries ###


add_quarantine <- res_unclustered %>%
  filter(
    indicator == "Excess proportion quarantined",
    type == "Gender-split cohorting",
  ) %>%
  group_by(mode, susceptibility, share_subclinical, type, indicator) %>%
  summarize(
    mean_prob = .125,
    max_qua = max(value),
    label = paste0("+", mean((1 - share_subclinical_num)/2))
  )



res_unclustered %>%
  ungroup() %>%
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
  facet_nested(indicator~mode + susceptibility + share_subclinical, scales = "free")


ggsave(filename = "Ext-Fig-2-Cohorting-Strategies-Space.jpg", width = 14, height = 10)




### Results across all classrooms, by country ###

add_quarantine <- res_unclustered_country %>%
  filter(
    !is.na(scenario),
    indicator == "Excess proportion quarantined",
    type == "Gender-split cohorting",
  ) %>%
  group_by(country, mode, scenario, type, indicator) %>%
  summarize(
    mean_prob = .125,
    max_qua = max(value),
    label = paste0("+", mean((1 - share_subclinical_num)/2))
  )

res_unclustered_country %>%
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
  facet_nested(mode + indicator~scenario + country, scales = "free")



ggsave(filename = "Supp-A-Fig-2-Cohorting-Strategies-Countries.jpg", width = 12, height = 16)





#####################################
## Distribution of Large Outbreaks ##
#####################################


res_dist %>% 
   mutate(quant = "5% Largest\nOutbreaks") %>%
  bind_rows(
    res_dist %>% 
      filter(share_inf > `1% Largest Outbreaks`) %>%
      mutate(quant = "1% Largest\nOutbreaks")
  ) %>%
  mutate(
    quant = fct_relevel(quant, "5% Largest\nOutbreaks")
  ) %>%
  ggplot(aes(x = share_inf, color = type, fill = type)) + 
  geom_density(alpha = .2) +
  facet_nested(quant~mode+scenario) +
  labs(
    x = "Proportion infected",
    y = "Density",
    fill = "",
    color = ""
  ) +
  lims(
    x = c(0, 1)
  ) + 
  scale_color_manual(values = color_values) +
  scale_fill_manual(values = color_values) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(filename = "Fig-6-Large-Outbreaks.jpg", width = 10, height = 5)




sink("All-Results-Summary.txt")
res_unclustered %>% 
  filter(!is.na(scenario)) %>%
  arrange(indicator, mode, scenario, type) %>% print(n = Inf)
sink()


