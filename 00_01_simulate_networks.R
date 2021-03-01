####################################################
### Generate simulated network data for analysis ###
### Actual CILS data cannot be shared            ###
####################################################
 
# Load packages
library(sna)
library(ergm)
library(tidyverse)



simulated_classroom <- function(
  classid
) {

n_total <- sample(20:25, 1) # total number of students in class

girl <- rbinom(n_total, 1, .5) # who is girl and who is boy


network_start <- rgraph( # start with a random graph
  n = n_total,
  m = 1,
  tprob = .1
) %>% network()


set.vertex.attribute(network_start, "girl", girl) # add gender variable

ergm_fixed <- ergm(network_start~offset(edges) + # run ERGM with fixed parameters
  offset(mutual) +
  offset(transitive) +
  offset(nodematch("girl")),
  offset.coef = c(-3, .5, .05, .5) # low density, some reciprocity, some transitivity, some gender homophily
)

network_end <- simulate(ergm_fixed, nsim = 1) # simulate from the ERGM
# this should create random networks that have gender segregation, reciprocity, and transitivity
# as do empirically observed networks have                                        

set.vertex.attribute(network_end, "girl", girl) # add gender variable


cils_class_net <- as.edgelist(network_end) %>% # transform into edgelist along the lines of CILS data
  as_tibble() %>% 
  rename(youthid = "V1", alterid = "V2") %>%
  mutate_all(~as.numeric(.)) %>%
  group_by(youthid) %>%
  mutate(name = paste0("tos_", 1:n())) %>%
  pivot_wider(
    names_from = "name",
    values_from = "alterid"
  ) %>%
  ungroup() %>%
  mutate_all(~as.numeric(.))


cils_class <- tibble(youthid = 1:n_total) %>% # change ids to fit classroom ids
  left_join(cils_class_net, by = "youthid") %>%
  mutate_all(~. + 100 * classid) %>%
  mutate(classid = classid)


cils_main <- tibble(youthid = 1:n_total + 100 * classid, classid = classid, girl = girl) %>% # transofmr along the lines of CILS data
  mutate(girl = ifelse(girl == 1, "Girl", "Boy"))


return(list(cils_main = cils_main, cils_class = cils_class, network_end = network_end))

}


### Simulate ten networks for each country ###

all_main <- tibble()
all_class <- tibble()

for(i in c(100001:100010, 200001:200010, 300001:300010, 400001:400010)) { # country is indicated by leading number

  res <- simulated_classroom(classid = i)
  all_main <- all_main %>% bind_rows(res$cils_main)
  all_class <- all_class %>% bind_rows(res$cils_class)

}


# Rename along the lines of CILS data
cils_main <- all_main
cils_class_w1 <- all_class



# Save
# dir.create("data_prepared")
# save(cils_main, cils_class_w1, file = paste0("data_prepared", "/", "cils_prepared", "_", "sim", ".RData"))

# q("no")