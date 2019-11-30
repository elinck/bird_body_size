install.packages("ape", repo = "http://ftp.osuosl.org/pub/cran/");
install.packages("phangorn", repo = "http://ftp.osuosl.org/pub/cran/");
install.packages("brms", repo = "http://ftp.osuosl.org/pub/cran/");
install.packages("broom", repo = "http://ftp.osuosl.org/pub/cran/");

library(ape)
library(phangorn)
library(brms)
library(broom)

# commands to upload data, request node, etc
# cd /lustre/haven/user/elinck/body_size/
# qsub -I -A ACF-UTK0011 -l nodes=1,walltime=6:00:00
# scp ~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis* elinck@datamover1.nics.utk.edu:/lustre/haven/user/elinck/body_size/

# set working directory
setwd("/media/burke/bigMac/ethan/brms/")

# read in files
master.df <- read.csv("all_analysis_df.csv")
master.tree <- read.tree("all_analysis_phy.tree")
master.dist <- ape::vcv.phylo(master.tree) 

# basic model, all species, weighted by no. years
mod_all_noyr <- brm(
  slope_mass | weights(no_years) ~ slope_temp + starting_mass + lat + (1|phylo), data = master.df, 
  family = gaussian(), cov_ranef = list(phylo = master.dist),
  control = list(max_treedepth = 15),
  prior = c(
    prior(normal(0, 10), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 50000, warmup = 25000
)

summary(mod_all_noyr)
mod_obj_noyr <- tidy(mod_all_noyr)
write.csv(mod_obj_noyr, "model_all_test.csv")

# basic model, males, weighted by no. years
m.df <- read.csv("m_analysis_df.csv")
m.tree <- read.tree("m_analysis_phy.tree")
m.dist <- ape::vcv.phylo(m.tree) 

# basic model, males, weighted by no. years
mod_m_noyr <- brm(
  slope_mass | weights(no_years) ~ slope_temp + starting_mass + lat + (1|phylo), data = m.df, 
  family = gaussian(), cov_ranef = list(phylo = master.dist),
  control = list(max_treedepth = 15),#, adapt_delta = 0.99),
  prior = c(
    prior(normal(0, 10), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  ),
  sample_prior = TRUE, chains = 2, cores = 2, 
  iter = 50000, warmup = 25000
)

summary(mod_m_noyr)
mod_obj_m_noyr <- tidy(mod_m_noyr)
write.csv(mod_obj_m_noyr, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/model_m_test.csv")

# basic model, females, weighted by no. years
f.df <- read.csv("f_analysis_df.csv")
f.tree <- read.tree("f_analysis_phy.tree")
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
  iter = 50000, warmup = 25000
)

summary(mod_f_noyr)
mod_obj_f_noyr <- tidy(mod_f_noyr)
write.csv(mod_obj_f_noyr, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/model_f_test.csv")