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

male.df$SITE[male.df$lat %in% unique(male.df$lat)[1]] <- 'A'
male.df$SITE[male.df$lat %in% unique(male.df$lat)[2]] <- 'B'
male.df$SITE[male.df$lat %in% unique(male.df$lat)[3]] <- 'C'
male.df$SITE[male.df$lat %in% unique(male.df$lat)[4]] <- 'D'
male.df$SITE[male.df$lat %in% unique(male.df$lat)[5]] <- 'E'
male.df$SITE[male.df$lat %in% unique(male.df$lat)[6]] <- 'F'

female.df$SITE[female.df$lat %in% unique(female.df$lat)[1]] <- 'A'
female.df$SITE[female.df$lat %in% unique(female.df$lat)[2]] <- 'B'
female.df$SITE[female.df$lat %in% unique(female.df$lat)[3]] <- 'C'
female.df$SITE[female.df$lat %in% unique(female.df$lat)[4]] <- 'D'
female.df$SITE[female.df$lat %in% unique(female.df$lat)[5]] <- 'E'
female.df$SITE[female.df$lat %in% unique(female.df$lat)[6]] <- 'F'

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

## null models

# brazil
data.A <- master.df[master.df$SITE=="A",]
species.A <- data.A$species
tree.A <- keep.tip(master.tree, species.A)
tree.A.length <- compute.brlen(tree.A)
cor.A <- vcv(tree.A.length, cor = T)
null.A <- rma.mv(yi = slope_mass, 
                 V = se_mass^2, 
                 random = list(~1 | species), 
                 R = list(species = cor.A),
                 method="REML", 
                 data=data.A, 
                 control=list(optimizer="optim"))


# panama
data.B <- master.df[master.df$SITE=="B",]
species.B <- data.B$species
tree.B <- keep.tip(master.tree, species.B)
tree.B.length <- compute.brlen(tree.B)
cor.B <- vcv(tree.B.length, cor = T)
null.B <- rma.mv(yi = slope_mass, 
                 V = se_mass^2, 
                 random = list(~1 | species), 
                 R = list(species = cor.B),
                 method="REML", 
                 data=data.B, 
                 control=list(optimizer="optim"))

# guanica
data.C <- master.df[master.df$SITE=="C",]
species.C <- data.C$species
tree.C <- keep.tip(master.tree, species.C)
tree.C.length <- compute.brlen(tree.C)
cor.C <- vcv(tree.C.length, cor = T)
null.C <- rma.mv(yi = slope_mass, 
                 V = se_mass^2, 
                 random = list(~1 | species), 
                 R = list(species = cor.C),
                 method="REML", 
                 data=data.C, 
                 control=list(optimizer="optim"))

# palomarin
data.D <- master.df[master.df$SITE=="D",]
species.D <- data.D$species
tree.D <- keep.tip(master.tree, species.D)
tree.D.length <- compute.brlen(tree.D)
cor.D <- vcv(tree.D.length, cor = T)
null.D <- rma.mv(yi = slope_mass, 
                 V = se_mass^2, 
                 random = list(~1 | species), 
                 R = list(species = cor.D),
                 method="REML", 
                 data=data.D, 
                 control=list(optimizer="optim"))

# teton science school
data.E <- master.df[master.df$SITE=="E",]
species.E <- data.E$species
tree.E <- keep.tip(master.tree, species.E)
tree.E.length <- compute.brlen(tree.E)
cor.E <- vcv(tree.E.length, cor = T)
null.E <- rma.mv(yi = slope_mass, 
                 V = se_mass^2, 
                 random = list(~1 | species), 
                 R = list(species = cor.E),
                 method="REML", 
                 data=data.E, 
                 control=list(optimizer="optim"))

# waterfall glen
data.F <- master.df[master.df$SITE=="F",]
species.F <- data.F$species
tree.F <- keep.tip(master.tree, species.F)
tree.F.length <- compute.brlen(tree.F)
cor.F <- vcv(tree.F.length, cor = T)
null.F <- rma.mv(yi = slope_mass, 
                 V = se_mass^2, 
                 random = list(~1 | species), 
                 R = list(species = cor.F),
                 method="REML", 
                 data=data.F, 
                 control=list(optimizer="optim"))

# make df
row1 <- cbind.data.frame("brazil", as.numeric(null.A$beta), as.numeric(null.A$ci.lb), as.numeric(null.A$ci.ub))
colnames(row1) <- c("site", "intercept", "lb", "ub")
row2 <- cbind.data.frame("panama", as.numeric(null.B$beta), as.numeric(null.B$ci.lb), as.numeric(null.B$ci.ub))
colnames(row2) <- c("site", "intercept", "lb", "ub")
row3 <- cbind.data.frame("guanica", as.numeric(null.C$beta), as.numeric(null.C$ci.lb), as.numeric(null.C$ci.ub))
colnames(row3) <- c("site", "intercept", "lb", "ub")
row4 <- cbind.data.frame("palomarin", as.numeric(null.D$beta), as.numeric(null.D$ci.lb), as.numeric(null.D$ci.ub))
colnames(row4) <- c("site", "intercept", "lb", "ub")
row5 <- cbind.data.frame("teton science school", as.numeric(null.E$beta), as.numeric(null.E$ci.lb), as.numeric(null.E$ci.ub))
colnames(row5) <- c("site", "intercept", "lb", "ub")
row6 <- cbind.data.frame("waterfall glen", as.numeric(null.F$beta), as.numeric(null.F$ci.lb), as.numeric(null.F$ci.ub))
colnames(row6) <- c("site", "intercept", "lb", "ub")
intercept.df <- rbind.data.frame(row1,row2,row3,row4,row5,row6)

# write to file
write.csv(intercept.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/site_intercept.csv")

# data summary
nrow(master.df) # 287 total records
master.df$species %>% unique() %>% length() # 239 unique species
nrow(male.df) # 150 total records
male.df$species %>% unique() %>% length() # 123 unique species
nrow(female.df) # 170 total records
female.df$species %>% unique() %>% length() # 142 unique species
nrow(combined.df) # 320 total records
combined.df$species %>% unique() %>% length() # 149 unique species
