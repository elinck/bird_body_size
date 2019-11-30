library(ape)
library(phangorn)
library(brms)
library(broom)

# set working directory
setwd("~/Dropbox/Bird_body_size-analysis/bird_body_size/")

# read in files
master.df <- read.csv("data/all_analysis_df.csv")
master.tree <- read.tree("data/all_analysis_phy.tree")
master.dist <- ape::vcv.phylo(master.tree) 

# basic model, all species, weighted by no. years
mod_all_noyr <- brm(
  slope_mass | weights(no_years) ~ slope_temp + starting_mass + lat + (1|phylo), data = master.df, 
  family = gaussian(), cov_ranef = list(phylo = master.dist),
  #control = list(max_treedepth = 15),#, adapt_delta = 0.99),
  prior = c(
    prior(normal(0, 10), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 5000, warmup = 2500
)

summary(mod_all_noyr)
mod_obj_noyr <- tidy(mod_all_noyr)
write.csv(mod_obj_noyr, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/model_all_test.csv")


# basic model, males, weighted by no. years
m.df <- read.csv("data/m_analysis_df.csv")
m.tree <- read.tree("data/m_analysis_phy.tree")
m.dist <- ape::vcv.phylo(m.tree) 

# basic model, males, weighted by no. years
mod_m_noyr <- brm(
  slope_mass | weights(no_years) ~ slope_temp + starting_mass + lat + (1|phylo), data = m.df, 
  family = gaussian(), cov_ranef = list(phylo = master.dist),
  #control = list(max_treedepth = 15),#, adapt_delta = 0.99),
  prior = c(
    prior(normal(0, 10), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 5000, warmup = 2500
)

summary(mod_m_noyr)
mod_obj_m_noyr <- tidy(mod_m_noyr)
write.csv(mod_obj_m_noyr, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/model_m_test.csv")

# basic model, females, weighted by no. years
f.df <- read.csv("data/f_analysis_df.csv")
f.tree <- read.tree("data/f_analysis_phy.tree")
f.dist <- ape::vcv.phylo(m.tree) 

# basic model, all species, weighted by no. years
mod_f_noyr <- brm(
  slope_mass | weights(no_years) ~ slope_temp + starting_mass + lat + (1|phylo), data = f.df, 
  family = gaussian(), cov_ranef = list(phylo = master.dist),
  control = list(max_treedepth = 15),#, adapt_delta = 0.99),
  prior = c(
    prior(normal(0, 5), "b"),
    prior(normal(0, 25), "Intercept"),
    prior(student_t(3, 0, 5), "sd"),
    prior(student_t(3, 0, 5), "sigma")
  ),
  sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 5000, warmup = 2500
)

summary(mod_f_noyr)
mod_obj_f_noyr <- tidy(mod_f_noyr)
write.csv(mod_obj_f_noyr, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/model_f_test.csv")

