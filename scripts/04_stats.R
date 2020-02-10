library(ape)
library(phangorn)
library(broom)
library(phytools)
library(nlme)

# set working directory
setwd("~/Dropbox/Bird_body_size-analysis/bird_body_size/")

# read in files
master.df <- read.csv("data/all_analysis_df.csv")
male.df <- read.csv("data/m_analysis_df.csv")
female.df <- read.csv("data/f_analysis_df.csv")
powdermill.df <- read.csv("data/powdermill_analysis_df.csv")
palo.df <- read.csv("data/palo_analysis_df.csv")
master.tree <- read.tree("data/all_analysis_phy.tree")
male.tree <- read.tree("data/m_analysis_phy.tree")
female.tree <- read.tree("data/f_analysis_phy.tree")
powdermill.tree <- read.tree("data/p_analysis_phy.tree")
palo.tree <- read.tree("data/palo_analysis_phy.tree")

# scale variables
master.df$starting_mass <- master.df$starting_mass/1000
master.df$lat <- master.df$lat/1000
master.df$sample_size<-sqrt(master.df$sample_size)

male.df$starting_mass <- male.df$starting_mass/1000
male.df$lat <- male.df$lat/1000
male.df$sample_size<-sqrt(male.df$sample_size)

female.df$starting_mass <- female.df$starting_mass/1000
female.df$lat <- female.df$lat/1000
female.df$sample_size<-sqrt(female.df$sample_size)

powdermill.df$starting_mass <- powdermill.df$starting_mass/1000
powdermill.df$lat <- powdermill.df$lat/1000
powdermill.df$sample_size<-sqrt(powdermill.df$sample_size)

### pgls approach

# create data frame, all birds
X1 <- cbind.data.frame(master.df$slope_mass, master.df$slope_temp,  master.df$slope_precip, master.df$starting_mass, master.df$lat)
colnames(X1) <- c("slope_mass","slope_temp", "slope_precip","starting_mass","lat")
species1 <- as.vector(as.character(master.df$species))
rownames(X1) <- species1

# get SE of sample size
SE1 <-setNames(master.df$se_mass, master.tree$tip.label)

# fit model, all birds
fit1<-pgls.SEy(slope_mass ~ slope_temp + slope_precip + starting_mass + lat, data=X1, se=SE1, tree=master.tree, method="ML")
fit1.sum <- summary(fit1)
fit1.tidy <- broom.mixed:::tidy.gls(fit1)
write.csv(fit1.tidy, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/mod_all_fit.csv")

# create data frame, males
X2 <- cbind.data.frame(male.df$slope_mass, male.df$slope_temp, male.df$starting_mass, male.df$lat)
colnames(X2) <- c("slope_mass","slope_temp","starting_mass","lat")
species2 <- as.vector(as.character(male.df$species))
rownames(X2) <- species2

# get SE of sample size
SE2 <-setNames(male.df$se_mass, male.tree$tip.label)

# fit model, males
fit2<-pgls.SEy(slope_mass ~ slope_temp + starting_mass + lat, data=X2, se=SE2, tree=male.tree, method="ML")
fit2.sum <- summary(fit2)
fit2.tidy <- broom.mixed:::tidy.gls(fit2)
write.csv(fit2.tidy, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/mod_male_fit.csv")

# create data frame, females
X3 <- cbind.data.frame(female.df$slope_mass, female.df$slope_temp, female.df$starting_mass, female.df$lat)
colnames(X3) <- c("slope_mass","slope_temp","starting_mass","lat")
species3 <- as.vector(as.character(female.df$species))
rownames(X3) <- species3

# get SE of sample size
SE3 <-setNames(female.df$se_mass, female.tree$tip.label)

# fit model, females
fit3<-pgls.SEy(slope_mass ~ slope_temp + starting_mass + lat, data=X3, se=SE3, tree=female.tree, method="ML")
fit3.sum <- summary(fit3)
fit3.tidy <- broom.mixed:::tidy.gls(fit3)
write.csv(fit3.tidy, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/mod_female_fit.csv")

# create data frame, palomarin
X4 <- cbind.data.frame(palo.df$slope_mass, palo.df$slope_temp, palo.df$starting_mass, palo.df$lat)
colnames(X4) <- c("slope_mass","slope_temp","starting_mass","lat")
species4 <- as.vector(as.character(palo.df$species))
rownames(X4) <- species4

# get SE of sample size
SE4 <-setNames(palo.df$se_mass, palo.tree$tip.label)

# fit model, palomarin
fit4<-pgls.SEy(slope_mass ~ slope_temp + starting_mass, data=X4, se=SE4, tree=palo.tree, method="ML")
fit4.sum <- summary(fit4)
fit4.tidy <- broom.mixed:::tidy.gls(fit4)
write.csv(fit4.tidy, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/mod_powdermill_fit.csv")

# create data frame, powdermill
X5 <- cbind.data.frame(powdermill.df$slope_mass, powdermill.df$slope_temp, powdermill.df$starting_mass, powdermill.df$lat)
colnames(X5) <- c("slope_mass","slope_temp","starting_mass","lat")
species5 <- as.vector(as.character(powdermill.df$species))
rownames(X5) <- species5

# get SE of sample size
SE5 <-setNames(powdermill.df$se_mass, powdermill.tree$tip.label)

# fit model, powdermill
fit5<-pgls.SEy(slope_mass ~ slope_temp + starting_mass, data=X5, se=SE5, tree=powdermill.tree, method="ML")
fit5.sum <- summary(fit5)
fit5.tidy <- broom.mixed:::tidy.gls(fit5)
write.csv(fit5.tidy, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/mod_powdermill_fit.csv")

# simple linear models to see if phylogeny matters
mod1 <- lm(slope_mass ~ slope_temp + slope_precip + starting_mass, master.df)
summary(mod1)
mod1.tidy <- broom::tidy(mod1)
write.csv(mod1.tidy, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/mod_master_fit_nophy.csv")
mod2 <- lm(slope_mass ~ slope_temp + slope_precip + starting_mass, male.df)
summary(mod2)
mod2.tidy <- broom::tidy(mod2)
write.csv(mod2.tidy, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/mod_male_fit_nophy.csv")
mod3 <- lm(slope_mass ~ slope_temp + slope_precip + starting_mass, female.df)
summary(mod3)
mod3.tidy <- broom::tidy(mod3)
write.csv(mod3.tidy, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/mod_female_fit_nophy.csv")
mod4 <- lm(slope_mass ~ slope_temp + slope_precip + starting_mass, palo.df)
summary(mod4)
mod4.tidy <- broom::tidy(mod4)
write.csv(mod4.tidy, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/mod_palo_fit_nophy.csv")
mod5 <- lm(slope_mass ~ slope_temp + slope_precip + starting_mass, powdermill.df)
summary(mod5)
mod5.tidy <- broom::tidy(mod5)
write.csv(mod5.tidy, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/mod_powdermill_fit_nophy.csv")

