library(ape)
library(metafor)
library(broom)
library(tidyverse)

# set wd
setwd('~/Dropbox/Bird_body_size-analysis/bird_body_size/data')

# load data
master.df <- read.csv("all_analysis_df.csv")
male.df <- read.csv("m_analysis_df.csv")
female.df <- read.csv("f_analysis_df.csv")
tarsus.df <- read.csv("tarsus_analysis_df.csv")

master.df$tropical<-0
male.df$tropical<-0
female.df$tropical<-0

master.df[abs(master.df$lat)<22,]$tropical <- 1
male.df[abs(male.df$lat)<22,]$tropical <- 1
female.df[abs(female.df$lat)<22,]$tropical <- 1

master.df$SITE <- NA
male.df$SITE <- NA
female.df$SITE <- NA
tarsus.df$SITE <- NA

master.df$SITE[master.df$lat %in% unique(master.df$lat)[1]] <- 'A'
master.df$SITE[master.df$lat %in% unique(master.df$lat)[2]] <- 'B'
master.df$SITE[master.df$lat %in% unique(master.df$lat)[3]] <- 'C'
master.df$SITE[master.df$lat %in% unique(master.df$lat)[4]] <- 'D'
master.df$SITE[master.df$lat %in% unique(master.df$lat)[5]] <- 'E'
master.df$SITE[master.df$lat %in% unique(master.df$lat)[6]] <- 'F'
master.df$SITE[master.df$lat %in% unique(master.df$lat)[7]] <- 'G'

male.df$SITE[male.df$lat %in% unique(male.df$lat)[1]] <- 'A'
male.df$SITE[male.df$lat %in% unique(male.df$lat)[2]] <- 'B'
male.df$SITE[male.df$lat %in% unique(male.df$lat)[3]] <- 'C'
male.df$SITE[male.df$lat %in% unique(male.df$lat)[4]] <- 'D'
male.df$SITE[male.df$lat %in% unique(male.df$lat)[5]] <- 'E'
male.df$SITE[male.df$lat %in% unique(male.df$lat)[6]] <- 'F'
male.df$SITE[male.df$lat %in% unique(male.df$lat)[7]] <- 'G'

female.df$SITE[female.df$lat %in% unique(female.df$lat)[1]] <- 'A'
female.df$SITE[female.df$lat %in% unique(female.df$lat)[2]] <- 'B'
female.df$SITE[female.df$lat %in% unique(female.df$lat)[3]] <- 'C'
female.df$SITE[female.df$lat %in% unique(female.df$lat)[4]] <- 'D'
female.df$SITE[female.df$lat %in% unique(female.df$lat)[5]] <- 'E'
female.df$SITE[female.df$lat %in% unique(female.df$lat)[6]] <- 'F'
female.df$SITE[female.df$lat %in% unique(female.df$lat)[7]] <- 'G'

## models w/o phylogeny
m1.all <- rma.mv(yi = slope_mass, V = se_mass^2, random = list(~1 | species), method="REML", data=master.df, control=list(optimizer="optim"))
m1.female <- rma.mv(yi = slope_mass, V = se_mass^2, random = list(~1 | species), method="REML", data=female.df, control=list(optimizer="optim"))
m1.male <- rma.mv(yi = slope_mass, V = se_mass^2, random = list(~1 | species), method="REML", data=male.df, control=list(optimizer="optim"))

m2.all <- rma.mv(yi = slope_mass, V = se_mass^2, mods = ~starting_mass + slope_temp + slope_precip + abs(lat) + slope_temp*abs(lat),  random = list(~1 | SITE, ~1 | species), method="REML", data=master.df, control=list(optimizer="optim"))
m2.female <- rma.mv(yi = slope_mass, V = se_mass^2, mods = ~starting_mass + slope_temp + slope_precip + abs(lat) + slope_temp*abs(lat), random = list(~1 | species), method="REML", data=female.df, control=list(optimizer="optim"))
m2.male <- rma.mv(yi = slope_mass, V = se_mass^2, mods = ~starting_mass + slope_temp + slope_precip + abs(lat) + slope_temp*abs(lat), random = list(~1 | species), method="REML", data=male.df, control=list(optimizer="optim"))

## export
m1.all.write <- tidy(m1.all)
m1.all.write$ci.lb <- m1.all$ci.lb
m1.all.write$ci.ub <- m1.all$ci.ub
write_csv(m1.all.write, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m1.all.csv")
m1.male.write <- tidy(m1.male)
m1.male.write$ci.lb <- m1.male$ci.lb
m1.male.write$ci.ub <- m1.male$ci.ub
write_csv(m1.male.write, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m1.male.csv")
m1.female.write <- tidy(m1.female)
m1.female.write$ci.lb <- m1.female$ci.lb
m1.female.write$ci.ub <- m1.female$ci.ub
write_csv(m1.female.write, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m1.female.csv")
m2.all.write <- tidy(m2.all)
m2.all.write$ci.lb <- m2.all$ci.lb
m2.all.write$ci.ub <- m2.all$ci.ub
write_csv(m2.all.write, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m2.all.csv")
m2.male.write <- tidy(m2.male)
m2.male.write$ci.lb <- m2.male$ci.lb
m2.male.write$ci.ub <- m2.male$ci.ub
write_csv(m2.male.write, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m2.male.csv")
m2.female.write <- tidy(m2.female)
m2.female.write$ci.lb <- m2.female$ci.lb
m2.female.write$ci.ub <- m2.female$ci.ub
write_csv(m2.female.write, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m2.female.csv")

## phylogenetically informed models
master.tree <- read.tree("all_analysis_phy.tree")
male.tree <- read.tree("m_analysis_phy.tree")
female.tree <- read.tree("f_analysis_phy.tree")
sex.tree <- read.tree("sex_analysis_phy.tree")

## all data
mast.tree <- compute.brlen(master.tree)
cor <- vcv(mast.tree, cor = T)

master.df$species <- as.character(master.df$species)
m3.all <- rma.mv(yi = slope_mass, V = se_mass^2,
                  mods = ~starting_mass + slope_temp + slope_precip + abs(lat) + slope_temp * abs(lat),
                  random = list(~1 | SITE, ~1 | species), R = list(species = cor),
                  method="REML", data=master.df,
                  control=list(optimizer="bobyqa"))

## export
m3.all.write <- tidy(m3.all)
m3.all.write$ci.lb <- m3.all$ci.lb
m3.all.write$ci.ub <- m3.all$ci.ub
write_csv(m3.all.write, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m3.all.csv")

## males alone
male.tree <- compute.brlen(male.tree)
cor <- vcv(male.tree, cor = T)

male.df$species <- as.character(male.df$species)
m3.male <- rma.mv(yi = slope_mass, V = se_mass^2,
				mods = ~starting_mass + slope_temp + slope_precip + tropical + tropical*slope_temp,
				random = list(~1 | SITE, ~1 | species), R = list(species = cor),
				method="REML", data=male.df,
				control=list(optimizer="optim"))

## export
m3.male.write <- tidy(m3.male)
m3.male.write$ci.lb <- m3.male$ci.lb
m3.male.write$ci.ub <- m3.male$ci.ub
write_csv(m3.male.write, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m3.male.csv")

## females alone
female.tree <- compute.brlen(female.tree)
cor <- vcv(female.tree, cor = T)

female.df$species <- as.character(female.df$species)
m3.female <- rma.mv(yi = slope_mass, V = se_mass^2,
				mods = ~starting_mass + slope_temp + slope_precip + tropical + tropical*slope_temp,
				random = list(~1 | SITE, ~1 | species), R = list(species = cor),
				method="REML", data=female.df,
				control=list(optimizer="bobyqa"))

## export
m3.female.write <- tidy(m3.female)
m3.female.write$ci.lb <- m3.female$ci.lb
m3.female.write$ci.ub <- m3.female$ci.ub
write_csv(m3.female.write, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m3.female.csv")

## combined sexes
cor <- vcv(sex.tree, cor = T)
female.df$Sex <- 'F'
male.df$Sex <- 'M'

combined.df <- rbind(female.df, male.df)
m4.combined <- rma.mv(yi = slope_mass, V = se_mass^2,
				mods = ~starting_mass + Sex + slope_precip + tropical + tropical*slope_temp,
				random = list(~1 | SITE, ~1 | species), R = list(species = cor),
				method="REML", data=combined.df)

## xg: I centered the starting mass to make the intercept more interpretable (corresponding to a bird with mean body size)				
m4.combined_SMass_centered <- rma.mv(yi = slope_mass, V = se_mass^2,
				mods = ~scale(starting_mass,scale=F) + Sex + slope_precip + tropical + tropical*slope_temp,
				random = list(~1 | SITE, ~1 | species), R = list(species = cor),
				method="REML", data=combined.df)				
				
m4.combined.write <- tidy(m4.combined)
m4.combined.write$ci.lb <- m4.combined$ci.lb
m4.combined.write$ci.ub <- m4.combined$ci.ub
write_csv(m4.combined.write, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m4.combined.csv")

m4.combined_centered.write <- tidy(m4.combined_SMass_centered)
m4.combined_centered.write$ci.lb <- m4.combined_SMass_centered$ci.lb
m4.combined_centered.write$ci.ub <- m4.combined_SMass_centered$ci.ub
write_csv(m4.combined_centered.write, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m4.combined.csv")

# combined sexes, no interaction 
m5.combined_SMass_centered <- rma.mv(yi = slope_mass, V = se_mass^2,
                                     mods = ~scale(starting_mass,scale=F) + Sex + slope_precip + tropical + slope_temp,
                                     random = list(~1 | SITE, ~1 | species), R = list(species = cor),
                                     method="REML", data=combined.df)				

m5.combined.write <- tidy(m5.combined_SMass_centered)
m5.combined.write$ci.lb <- m5.combined_SMass_centered$ci.lb
m5.combined.write$ci.ub <- m5.combined_SMass_centered$ci.ub
write_csv(m5.combined.write, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m5.combined.csv")

# combined sexes, interaction but not constituent terms: this is the model we focus on in the manuscript!
m6.combined_SMass_centered <- rma.mv(yi = slope_mass, V = se_mass^2,
                                     mods = ~scale(starting_mass,scale=F) + Sex + slope_precip + slope_temp + abs(lat):slope_temp,
                                     random = list(~1 | SITE, ~1 | species), R = list(species = cor),
                                     method="REML", data=combined.df)				

m6.combined.write <- tidy(m6.combined_SMass_centered)
m6.combined.write$ci.lb <- m6.combined_SMass_centered$ci.lb
m6.combined.write$ci.ub <- m6.combined_SMass_centered$ci.ub
write_csv(m6.combined.write, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m6.combined.csv")

# data summary
nrow(master.df) # 294 total records
master.df$species %>% unique() %>% length() # 240 unique species
nrow(male.df) # 153 total records
male.df$species %>% unique() %>% length() # 123 unique species
nrow(female.df) # 173 total records
female.df$species %>% unique() %>% length() # 142 unique species
nrow(combined.df) # 326 total records
combined.df$species %>% unique() %>% length() # 149 unique species

# get first year for each locality
for(i in unique(combined.df$SITE)){
  print(i)
  tmp <- combined.df[combined.df$SITE==i,]
  tmp$starting_year %>% min() %>% print()
}

# get last year for each locality
for(i in unique(combined.df$SITE)){
  print(i)
  tmp <- combined.df[combined.df$SITE==i,]
  tmp$ending_year %>% max() %>% print()
}

