
##############################
### Load relevant packages ###
##############################

library(ggh4x)
library(dplyr)
library(tidyverse)

setwd("secondary_attack_rates")

############################
### Load all of the data ###
############################


### Names of the results folders<

names_est <- c(

  "2021-02-28___21-21-10_gamma_0.6_susc_0.05_sim",
  "2021-02-28___21-43-11_gamma_0.3_susc_0.15_sim",
  "2021-02-28___21-43-40_gamma_0.2_susc_0.25_sim"

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


dir.create("results")
setwd("results")

# Prepare the data
res_analysis <- res_complete %>% 
  mutate(
    secondary = number_exposed / number_met,
    country = case_when(
      classid > 400000~"SW",
      classid > 300000~"NL",
      classid > 200000~"GE",
      classid > 100000~"EN",
      TRUE~NA_character_
    ),
    classid = as.factor(classid),
    mode = ifelse(mode == "parallel", "Same-day instruction", "Weekly rota-system"),
    symptomatic = ifelse(symptomatic == 0, "subclinical", "clinical"),
    susceptibility_num = susceptibility,
    susceptibility = paste0("Baseline probability of\ninfection upon contact: ", susceptibility),
    scenario = case_when(
      susceptibility_num == .05~"Transmission dynamics:\nlow\n\nProbability of infection: 5%",
      susceptibility_num == .15~"Transmission dynamics:\nmedium\n\nProbability of infection: 15%",
      susceptibility_num == .25~"Transmission dynamics:\nhigh\n\nProbability of infection: 25%",
      TRUE~NA_character_
    ) %>% fct_relevel("Transmission dynamics:\nlow\n\nProbability of infection: 5%", "Transmission dynamics:\nmedium\n\nProbability of infection: 15%")
  )



# Information on infections
share_exp <- res_analysis %>%
  group_by(mode, symptomatic, susceptibility_num, susceptibility, pr_out_of_school) %>%
  summarize(
    mean_sec = mean(secondary, na.rm = TRUE),
    mean_inf = mean(number_exposed, na.rm = TRUE),
    num_inf = sum(number_exposed, na.rm = TRUE),
    quant80 = quantile(number_exposed, probs = .8),
    num_inf_80 = sum(number_exposed[number_exposed >= quant80], na.rm = TRUE),
    share_80s = num_inf_80 / num_inf,
    num_inf_80_str = sum(number_exposed[number_exposed > quant80], na.rm = TRUE),
    share_80s_str = num_inf_80_str / num_inf
  )

# Plotting number of infections
res_analysis %>%
  ggplot(aes(x = number_exposed)) + 
  geom_histogram(aes(y=..density..), bins = 30) + 
  geom_vline(data = share_exp, aes(xintercept = quant80)) +
  facet_grid(symptomatic~susceptibility, scales = "free") +
  labs(
    x = "Number infected",
    y = "Count"
  ) +
  theme_classic()

ggsave(filename = "Number-of-infections.jpg", width = 10, height = 7)


sink(file="Infections-Information.txt")
share_exp %>% print(n = Inf)
sink()

