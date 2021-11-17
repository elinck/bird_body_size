### create plots for manuscript

library(ggplot2)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(ggspatial)
library(ggtree)
library(stringr)
library(tidyverse)
library(RColorBrewer)
library(plyr)
source("~/Dropbox/Bird_body_size-analysis/bird_body_size/scripts/00_functions.R")

# load brazil data
brazil.plotting.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_sex.csv")[-1]
brazil.plotting.sex <- brazil.plotting.sex[,c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period")]

# plot with sexes separate
b1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=brazil.plotting.sex[brazil.plotting.sex$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=brazil.plotting.sex[brazil.plotting.sex$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=brazil.plotting.sex[brazil.plotting.sex$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=brazil.plotting.sex[brazil.plotting.sex$sex %in% "F",]) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free_y") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic")) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/species_plots/brazil_mass_sex.png",res=300,width=18,height=18,units="in")
b1
dev.off()

# load full data
brazil.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered.csv")[-1]
brazil.plotting <- brazil.plotting[,c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period")]

# plot overall trend
b2 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=brazil.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=brazil.plotting) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free_y") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic")) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/species_plots/brazil_mass_all.png",res=300,width=18,height=18,units="in")
b2
dev.off()

# load panama data
panama.plotting.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered_sex.csv")[-1]
panama.plotting.sex <- panama.plotting.sex[,c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period")]

# plot with sexes separate
p1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=panama.plotting.sex[panama.plotting.sex$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=panama.plotting.sex[panama.plotting.sex$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=panama.plotting.sex[panama.plotting.sex$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=panama.plotting.sex[panama.plotting.sex$sex %in% "F",]) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free_y") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic")) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/species_plots/panama_mass_sex.png",res=300,width=18,height=18,units="in")
p1
dev.off()

# load full data
panama.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered.csv")[-1]
panama.plotting <- panama.plotting[,c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period")]

# plot overall trend
p2 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=panama.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=panama.plotting) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free_y") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic")) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/species_plots/panama_mass_all.png",res=300,width=18,height=18,units="in")
p2
dev.off()

# load guanica data
guanica.plotting.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered_sex.csv")[-1]
guanica.plotting.sex <- guanica.plotting.sex[,c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period")]

# plot with sexes separate
g1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=guanica.plotting.sex[guanica.plotting.sex$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=guanica.plotting.sex[guanica.plotting.sex$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=guanica.plotting.sex[guanica.plotting.sex$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=guanica.plotting.sex[guanica.plotting.sex$sex %in% "F",]) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free_y") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic")) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/species_plots/guanica_mass_sex.png",res=300,width=18,height=18,units="in")
g1
dev.off()

# load full data
guanica.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered.csv")[-1]
guanica.plotting <- guanica.plotting[,c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period")]

# plot overall trend
g2 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=guanica.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=guanica.plotting) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free_y") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic")) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/species_plots/guanica_mass_all.png",res=300,width=18,height=18,units="in")
g2
dev.off()

# load palo data
palo.plotting.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered_sex.csv")[-1]
palo.plotting.sex <- palo.plotting.sex[,c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period")]

# plot with sexes separate
pal1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=palo.plotting.sex[palo.plotting.sex$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=palo.plotting.sex[palo.plotting.sex$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=palo.plotting.sex[palo.plotting.sex$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=palo.plotting.sex[palo.plotting.sex$sex %in% "F",]) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free_y") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic")) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/species_plots/palo_mass_sex.png",res=300,width=18,height=18,units="in")
pal1
dev.off()

# load full data
palo.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered.csv")[-1]
palo.plotting <- palo.plotting[,c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period")]

# plot overall trend
pal2 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=palo.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=palo.plotting) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free_y") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic")) 

# save pdf
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/species_plots/palo_mass_all.png",res=300,width=18,height=18,units="in")
pal2
dev.off()

# load powdermill data
powdermill.plotting.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered_sex.csv")[-1]
powdermill.plotting.sex <- powdermill.plotting.sex[,c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period")]

# plot with sexes separate
pw1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=powdermill.plotting.sex[powdermill.plotting.sex$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=powdermill.plotting.sex[powdermill.plotting.sex$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=powdermill.plotting.sex[powdermill.plotting.sex$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=powdermill.plotting.sex[powdermill.plotting.sex$sex %in% "F",]) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free_y") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic")) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/species_plots/powdermill_mass_sex.png",res=300,width=18,height=18,units="in")
pw1
dev.off()

# load full data
powdermill.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered.csv")[-1]
powdermill.plotting <- powdermill.plotting[,c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period")]

# plot overall trend
pw2 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=powdermill.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=powdermill.plotting) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free_y") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic")) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/species_plots/powdermill_mass_all.png",res=300,width=18,height=18,units="in")
pw2
dev.off()

# load tss data
tss.plotting.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered_sex.csv")[-1]
tss.plotting.sex <- tss.plotting.sex[,c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period")]

# plot with sexes separate
tss1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=tss.plotting.sex[tss.plotting.sex$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=tss.plotting.sex[tss.plotting.sex$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=tss.plotting.sex[tss.plotting.sex$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=tss.plotting.sex[tss.plotting.sex$sex %in% "F",]) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free_y") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic")) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/species_plots/tss_mass_sex.png",res=300,width=18,height=18,units="in")
tss1
dev.off()

# load full data
tss.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered.csv")[-1]
tss.plotting <- tss.plotting[,c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period")]

# plot overall trend
tss2 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=tss.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=tss.plotting) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free_y") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic")) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/species_plots/tss_mass_all.png",res=300,width=18,height=18,units="in")
tss2
dev.off()

# load wate data
wate.plotting.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered_sex.csv")[-1]
wate.plotting.sex <- wate.plotting.sex[,c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period")]

# plot with sexes separate
wate1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=wate.plotting.sex[wate.plotting.sex$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=wate.plotting.sex[wate.plotting.sex$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=wate.plotting.sex[wate.plotting.sex$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=wate.plotting.sex[wate.plotting.sex$sex %in% "F",]) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free_y") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic")) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/species_plots/wate_mass_sex.png",res=300,width=18,height=18,units="in")
wate1
dev.off()

# load full data
wate.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered.csv")[-1]

# plot overall trend
wate2 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=wate.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=wate.plotting) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free_y") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic")) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/species_plots/wate_mass_all.png",res=300,width=18,height=18,units="in")
wate2
dev.off()

# make master data frame for size correlations
corr.brazil.df <- cbind.data.frame(brazil.plotting, rep("Brazil",nrow(brazil.plotting))) #panama.plotting, guanica.plotting, powdermill.plotting, palo.plotting, wate.plotting)
colnames(corr.brazil.df) <- c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period","site")
corr.panama.df <- cbind.data.frame(panama.plotting, rep("Panama",nrow(panama.plotting))) #panama.plotting, guanica.plotting, powdermill.plotting, palo.plotting, wate.plotting)
colnames(corr.panama.df) <- c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period","site")
corr.guanica.df <- cbind.data.frame(guanica.plotting, rep("Guanica",nrow(guanica.plotting))) #guanica.plotting, guanica.plotting, powdermill.plotting, palo.plotting, wate.plotting)
colnames(corr.guanica.df) <- c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period","site")
corr.palo.df <- cbind.data.frame(palo.plotting, rep("palo",nrow(palo.plotting))) #palo.plotting, palo.plotting, powdermill.plotting, palo.plotting, wate.plotting)
colnames(corr.palo.df) <- c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period","site")
corr.powdermill.df <- cbind.data.frame(powdermill.plotting, rep("Powdermill",nrow(powdermill.plotting))) #powdermill.plotting, powdermill.plotting, powdermill.plotting, powdermill.plotting, wate.plotting)
colnames(corr.powdermill.df) <- c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period","site")
corr.tss.df <- cbind.data.frame(tss.plotting, rep("TSS",nrow(tss.plotting))) #tss.plotting, tss.plotting, tss.plotting, tss.plotting, wate.plotting)
colnames(corr.tss.df) <- c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period","site")
corr.wate.df <- cbind.data.frame(wate.plotting, rep("Waterfall",nrow(wate.plotting))) #maps.plotting, maps.plotting, maps.plotting, maps.plotting, wate.plotting)
colnames(corr.wate.df) <- c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period","site")
corr.master.df <- rbind.data.frame(corr.brazil.df,corr.panama.df,corr.guanica.df,corr.palo.df,corr.powdermill.df,corr.tss.df,corr.wate.df)
corr.master.df$tarsus <- as.numeric(as.character(corr.master.df$tarsus))
corr.master.df$wing_length <- as.numeric(as.character(corr.master.df$wing_length))

# tarsus
corr.tarsus.df <- corr.master.df[!is.na(corr.master.df$tarsus),]
corr.tarsus.df <- corr.tarsus.df[!corr.tarsus.df$tarsus>90,]
corr.tarsus.df <-corr.tarsus.df[!corr.tarsus.df$tarsus<5,]
corr.tarsus.df$site <- as.factor(corr.tarsus.df$site)
corr.tarsus.df$site <- recode(corr.tarsus.df$site, brazil = "Brazil", panama = "Panama", guanica = "Guanica",
                            powdermill = "Powdermill", palo = "Palomarin", tss = "Teton Science School",
                            wate = "Waterfall Glen")


tarsus.mod <- lm(tarsus ~ mass, corr.tarsus.df)
tarsus.mod <- summary(tarsus.mod)
tarsus.r2 <- tarsus.mod$adj.r.squared
tarsus.annotate <- paste0("R^2=",round(tarsus.r2, digits = 4))

n1 <- ggplot(corr.tarsus.df, aes(x=mass,y=tarsus)) +
  theme_classic() +
  geom_point(pch=1,color="#007304",alpha=0.7) +
  annotate(geom = 'text', label = tarsus.annotate, x=90, y=Inf, hjust = 0, vjust = 1) +
  geom_smooth(method = "lm",color="black",linetype="dashed") +
  xlab("Mass (g)") +
  ylab("Tarsus (mm)")

# wing_length
corr.wing.df <- corr.master.df[!is.na(corr.master.df$wing_length),]
corr.wing.df$site <- droplevels(corr.wing.df$site)
corr.wing.df <- corr.wing.df[!corr.wing.df$wing_length<5,]
corr.wing.df <- corr.wing.df[!corr.wing.df$wing_length==0,]
corr.wing.df <-corr.wing.df[!corr.wing.df$wing_length>400,]
corr.wing.df$site <- as.factor(corr.wing.df$site)
corr.wing.df$site <- recode(corr.wing.df$site, brazil = "Brazil", panama = "Panama", guanica = "Guanica",
                       powdermill = "Powdermill", palo = "Palomarin", tss = "Teton Science School",
                       wate = "Waterfall Glen")

wing.mod <- lm(wing_length ~ mass, corr.wing.df)
wing.mod <- summary(wing.mod)
wing.r2 <- wing.mod$adj.r.squared
wing.annotate <- paste0("R^2=",round(wing.r2, digits = 4))

o1 <- ggplot(corr.wing.df, aes(x=mass,y=wing_length)) +
  theme_classic() +
  geom_point(pch=1,color="#cc8800",alpha=0.7) +
  annotate(geom = 'text', label = wing.annotate, x=90, y=Inf, hjust = 0, vjust = 1) +
  geom_smooth(method = "lm",color="black",linetype="dashed") +
  xlab("Mass (g)") +
  ylab("Wing Length (mm)")

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s1.pdf",width=8,height=4)
plot_grid(n1,o1,labels="AUTO",nrow=1)
dev.off()

# plot correlations between body size and tarsus and body size and wing size, faceted
p1 <- ggplot(corr.tarsus.df, aes(x=mass,y=tarsus)) +
  theme_classic() +
  geom_point(pch=1,color="#007304",alpha=0.7) +
  #annotate(geom = 'text', label = tarsus.annotate, x=90, y=Inf, hjust = 0, vjust = 1) +
  geom_smooth(method = "lm",color="black",linetype="dashed") +
  xlab("Mass (g)") +
  ylab("Tarsus (mm)") +
  facet_wrap(~site, scales="free")

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s2.pdf",width=8,height=4)
p1
dev.off()

# plot tarsus vs cube root mass
corr.tarsus.df$mass_cuberoot <- corr.tarsus.df$mass^(1/3) 
tarsus.mod.g <- lm(tarsus ~ mass_cuberoot, corr.tarsus.df[corr.tarsus.df$site=="Guanica",])
tarsus.mod.pl <- lm(tarsus ~ mass_cuberoot, corr.tarsus.df[corr.tarsus.df$site=="Palomarin",])
tarsus.mod.pm <- lm(tarsus ~ mass_cuberoot, corr.tarsus.df[corr.tarsus.df$site=="Panama",])
tarsus.mod.g <- summary(tarsus.mod.g)
tarsus.mod.pl <- summary(tarsus.mod.pl)
tarsus.mod.pm <- summary(tarsus.mod.pm)
tarsus.r2.g <- tarsus.mod.g$adj.r.squared
tarsus.r2.pl <- tarsus.mod.pl$adj.r.squared
tarsus.r2.pm <- tarsus.mod.pm$adj.r.squared
tarsus.annotate.g <- paste0("R^2=",round(tarsus.r2.g, digits = 4))
tarsus.annotate.pl <- paste0("R^2=",round(tarsus.r2.pl, digits = 4))
tarsus.annotate.pm <- paste0("R^2=",round(tarsus.r2.pm, digits = 4))
dat_text <- data.frame(
  label = c(tarsus.annotate.g, tarsus.annotate.pl, tarsus.annotate.pm),
  site   = c("Guanica", "Palomarin", "Panama")
)

tarsus_masscuberoot <- ggplot(corr.tarsus.df, aes(x=mass_cuberoot,y=tarsus)) +
  theme_classic() +
  geom_point(pch=1,color="#007304",alpha=0.7) +
  #annotate(geom = 'text', label = tarsus.annotate, x=90, y=Inf, hjust = 0, vjust = 1) +
  geom_smooth(method = "lm",color="black",linetype="dashed") +
  xlab("Cube root mass (g)") +
  ylab("Tarsus (mm)") +
  facet_wrap(~site) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = -Inf, y = -Inf, label = label),
    hjust   = -0.5,
    vjust   = -25)

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s3.pdf",width=8,height=4)
tarsus_masscuberoot
dev.off()

q1 <- ggplot(corr.wing.df, aes(x=mass,y=wing_length)) +
  theme_classic() +
  geom_point(pch=1,color="#cc8800",alpha=0.7) +
  #annotate(geom = 'text', label = tarsus.annotate, x=90, y=Inf, hjust = 0, vjust = 1) +
  geom_smooth(method = "lm",color="black",linetype="dashed") +
  xlab("Mass (g)") +
  ylab("Wing Length (mm)") +
  facet_wrap(~site, scales="free")

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s4.pdf",width=8,height=8)
q1
dev.off()

# plot histogram of records over months
months.df <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/months.csv")[-1]
months.df$country <- factor(months.df$country,levels=c("brazil","panama","puertorico","palo","maps","powdermill"))
levels(months.df$country) <- c("Brazil","Panama","Puerto Rico","Palo","TSS + Waterfall","Powdermill")

m1 <- ggplot(data = months.df, aes(x=month)) +
  geom_histogram() +
  theme_bw() +
  labs(y="Record Frequency", x="Month") +
  facet_wrap(~country, scales="free_y", ncol=1) +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s5.pdf",width=6,height=10)
m1
dev.off()

tarsus.mod <- list()
for(i in unique(corr.tarsus.df$site)){
  tmp <- corr.tarsus.df[corr.tarsus.df$site==i,]
  tmp <- tmp[!is.na(tmp$tarsus),]
  mod <- summary(lm(tarsus ~ mass, tmp))
  r2 <- mod$adj.r.squared
  slope <- mod$coefficients[2,1]
  tarsus.mod[[i]] <- cbind.data.frame(r2,slope)
}
tarsus.mod 

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 

wing_length.mod <- list()
for(i in unique(corr.wing.df$site)){
  tmp <- corr.wing.df[corr.wing.df$site==i,]
  tmp <- tmp[!is.na(tmp$wing_length),]
  mod <- summary(lm(wing_length ~ mass, tmp))
  r2 <- mod$adj.r.squared
  slope <- mod$coefficients[2,1]
  wing_length.mod[[i]] <- cbind.data.frame(r2,slope)
}
wing_length.mod

# $brazil
# r2     slope
# 1 0.7877454 0.7476284
# 
# $panama
# r2     slope
# 1 0.7555506 0.6488566
# 
# $guanica
# r2     slope
# 1 0.8990361 0.9447432
# 
# $palo
# r2     slope
# 1 0.7024471 0.9727165
# 
# $powdermill
# r2     slope
# 1 0.7491755 0.9245388
# 
# $tss
# r2     slope
# 1 0.8596383 0.9563985
# 
# $wate
# r2    slope
# 1 0.7438697 1.055556

# phylogeny circle plot
master.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis_df.csv")
master.tree <- read.tree("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis_phy.tree")

# write species list, phylogeny
write.csv(x = master.tree$tip.label, file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/circular_phylogeny_species.csv")
write.tree(phy = master.tree, file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/circular_phylogeny.tre")

# visualize
master.df <- master.df[!master.df$slope_mass>0.5,] # drop outliers to avoid skewing color scale
master.df <- master.df[!master.df$slope_mass<(-0.5),]
master.tree <- keep.tip(master.tree, master.df$species)
master.slope <- cbind.data.frame(master.df$species, master.df$slope_mass)
p1 <- ggtree(master.tree, layout='circular') %<+% master.slope 
p1$data$`master.df$slope_mass`[is.na(p1$data$`master.df$slope_mass`)] <- 0
p2 <- ggtree(master.tree, layout='circular',size=1.5) %<+% master.slope + 
  aes(color=p1$data$`master.df$slope_mass`,guides="test") +
  scale_color_distiller(palette = "BrBG") +
  #geom_tiplab(size=2, color="black") + # uncomment to see species
  #geom_text(aes(label=node), color="black", size = 1) + # uncomment for node assignment
  theme(legend.position="right") +
  labs(color=expression(paste("Slope ",Delta," mass")))  +
  geom_cladelabel(node=476, label="", angle=0, offset=2) +
  geom_cladelabel(node=473, label="", angle=0, offset=2) +
  geom_cladelabel(node=242, label="", angle=0, offset=2) +
  geom_cladelabel(node=463, label="", angle=0, offset=2) +
  geom_cladelabel(node=466, label="", angle=0, offset=2) +
  geom_cladelabel(node=257, label="", angle=0, offset=2) +
  geom_cladelabel(node=299, label="", angle=0, offset=2) +
  geom_cladelabel(node=328, label="", angle=0, offset=2) +
  geom_cladelabel(node=354, label="", angle=0, offset=2) +
  geom_cladelabel(node=291, label="", angle=0, offset=2) +
  geom_cladelabel(node=362, label="", angle=0, offset=2) +
  geom_cladelabel(node=367, label="", angle=0, offset=2) +
  geom_cladelabel(node=377, label="", angle=0, offset=2) +
  geom_cladelabel(node=385, label="", angle=0, offset=2) +
  geom_cladelabel(node=403, label="", angle=0, offset=2) +
  geom_cladelabel(node=427, label="", angle=0, offset=2) +
  geom_cladelabel(node=440, label="", angle=0, offset=2) +
  geom_cladelabel(node=316, label="", angle=0, offset=2) +
  geom_cladelabel(node=442, label="", angle=0, offset=2) +
  geom_cladelabel(node=397, label="", angle=0, offset=2) 
  
  

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/figure_1_components/phylogeny.pdf", width=8, height=8)
p2
dev.off()


# read model terms 
mod1 <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m6.combined.csv")
mod1$term <- mod1$term %>% as.factor()  
mod1$term <- mod1$term %>% recode(intercept="Intercept",
                                  `scale(starting_mass, scale = F)`="Starting Mass",
                                  SexM="Sex=M",
                                  slope_precip="Precip",
                                  tropical="Tropical",
                                  slope_temp="Temp",
                                  `tropical:slope_temp`="Tropical*Temp"
                                  )
# betas         
p3 <- ggplot(mod1, aes(x=term, y=estimate, color=term)) + 
  theme_classic() +
  scale_color_brewer(palette="Dark2") +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=.1) +
  geom_line() +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_point(pch=1,size=3) +
  theme(axis.text.x=element_blank()) +
  # geom_text(data=subset(mod1, mod1$term=="slope_temp"), y = -0.75, label = "***") +
  ylab(expression(Beta)) +
  xlab(element_blank()) +
  labs(color="Model Term") 
  #scale_color_manual(name = "Model Term",
  #                   labels = modlabs) +
  # theme(legend.position = "bottom")

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/figure_1_components/betas.pdf", width=5, height=3)
p3
dev.off()

# locality map
localities <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/latlong.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
p4 <- ggplot(data=world) +
  geom_sf() +  #fill="white"
  coord_sf(xlim = c(-125, -50), ylim = c(-10, 45)) +
  theme_bw() +
  geom_point(data=localities, aes(x=long,y=lat, fill=site, size=years), pch=21, color="black", alpha=0.5) + #stroke=1.25, 
  labs(fill="Site",size="Years of data") +
  scale_color_viridis_d() +
  xlab("Longitude") +
  ylab("Latitude")

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/figure_1_components/map.pdf", width=6, height=5)
p4
dev.off()

# figure 2: distribution of mass change plot
slope_dist <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis_df.csv")
slope_dist$site[slope_dist$lat %in% unique(slope_dist$lat)[1]] <- 'Brazil'
slope_dist$site[slope_dist$lat %in% unique(slope_dist$lat)[2]] <- 'Panama'
slope_dist$site[slope_dist$lat %in% unique(slope_dist$lat)[3]] <- 'Guanica'
slope_dist$site[slope_dist$lat %in% unique(slope_dist$lat)[4]] <- 'Palomarin'
slope_dist$site[slope_dist$lat %in% unique(slope_dist$lat)[5]] <- 'Teton'
slope_dist$site[slope_dist$lat %in% unique(slope_dist$lat)[6]] <- 'Powdermill'
slope_dist$site[slope_dist$lat %in% unique(slope_dist$lat)[7]] <- 'Waterfall'
slope_dist$site <- factor(slope_dist$site, levels=c("Brazil", "Panama", "Guanica", "Palomarin", 
                                                    "Powdermill", "Waterfall", "Teton"))
slope_dist$lat <- as.numeric(as.character(slope_dist$lat))
slope_dist_plot <- ggplot(data=slope_dist, aes(x=site, y=slope_mass)) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  geom_hline(yintercept = 0, size=0.65, linetype="dashed") +
  geom_boxplot(alpha=0.7, outlier.shape = NA, aes(fill=abs(lat)))+
  scale_fill_gradient(
    low = 'firebrick', high = 'dodgerblue') + 
  geom_jitter(pch=21, alpha=0.5, width=0.18) +
  ylab(expression(paste(Delta," Mass ", (g/yr^-1)))) +
  annotate("segment", x = 3, xend = 1, y = 0.25, yend = 0.25,
           colour = "black", size = 0.65, arrow = arrow(type = "closed", length = unit(0.25, "cm"))) +
  annotate("text", x = 2, y = 0.3, label = "Tropical", size = 4) +
  annotate("segment", x = 4, xend = 7, y = 0.25, yend = 0.25,
           colour = "black", size = 0.65, arrow = arrow(type = "closed", length = unit(0.25, "cm"))) +
  annotate("text", x = 5.5, y = 0.3, label = "Temperate", size=4) +
  xlab("Site") +
  ylim(-0.3,0.3) +
  labs(fill="abs(Latitude)")

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/fig2.pdf", width=7, height=5)
slope_dist_plot
dev.off()

# alternative version with tropical / temperate color-coding
slope_dist$tropical <- 0
slope_dist[abs(slope_dist$lat)<22,]$tropical <- 1
slope_dist$tropical <- as.factor(slope_dist$tropical)
slope_dist$site <- factor(slope_dist$site, levels=c("Brazil", "Panama", "Guanica", "Palomarin", 
                                                    "Powdermill", "Waterfall", "Teton"))
slope_dist_plot_alt <- ggplot(data=slope_dist, aes(x=site, y=slope_mass)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = 0, size=0.65, linetype="dashed") +
  geom_boxplot(alpha=0.7, outlier.shape = NA, aes(fill=site)) +
  geom_jitter(pch=21, alpha=0.5, width=0.18) +
  ylab(expression(paste(Delta," Mass ", (g/yr^-1)))) +
  annotate("segment", x = 3, xend = 1, y = 0.25, yend = 0.25,
           colour = "black", size = 0.65, arrow = arrow(type = "closed", length = unit(0.25, "cm"))) +
  annotate("text", x = 2, y = 0.3, label = "Tropical", size = 4) +
  annotate("segment", x = 4, xend = 7, y = 0.25, yend = 0.25,
           colour = "black", size = 0.65, arrow = arrow(type = "closed", length = unit(0.25, "cm"))) +
  annotate("text", x = 5.5, y = 0.3, label = "Temperate", size=4) +
  xlab("Site") +
  ylim(-0.3,0.3)

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/fig2_alt.pdf", width=6, height=5)
slope_dist_plot_alt
dev.off()

# write temp dfs
temp_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered.csv",
            site_name = "Brazil",
            months = "SUMMER",
            output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_temps.csv")
temp_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered.csv",
            site_name = "Panama",
            months = "SUMMER",
            output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_temps.csv")
temp_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered.csv",
            site_name = "Guanica",
            months = "SUMMER",
            output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_temps.csv")
temp_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered.csv",
            site_name = "Powdermill",
            months = "SUMMER",
            output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_temps.csv")
temp_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered.csv",
            site_name = "Palomarin",
            months = "SUMMER",
            output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_temps.csv")
temp_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered.csv",
            site_name = "Teton",
            months = "SUMMER",
            output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_temps.csv")
temp_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered.csv",
            site_name = "Waterfall",
            months = "SUMMER",
            output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_temps.csv")

# load files
brazil_temps <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_temps.csv")[-1]
panama_temps <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_temps.csv")[-1]
guanica_temps <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_temps.csv")[-1]
powdermill_temps <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_temps.csv")[-1]
palo_temps <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_temps.csv")[-1]
tss_temps <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_temps.csv")[-1]
wate_temps <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_temps.csv")[-1]

# subset columns; add ID col
brazil_temps <- brazil_temps[,c("year", "MAT", "species")]
brazil_temps$site <- "Brazil"
panama_temps <- panama_temps[,c("year", "MAT", "species")]
panama_temps$site <- "Panama"
guanica_temps <- guanica_temps[,c("year", "MAT", "species")]
guanica_temps$site <- "Guanica"
powdermill_temps <- powdermill_temps[,c("year", "MAT", "species")]
powdermill_temps$site <- "Powdermill"
palo_temps <- palo_temps[,c("year", "MAT", "species")]
palo_temps$site <- "Palomarin"
tss_temps <- tss_temps[,c("year", "MAT", "species")]
tss_temps$site <- "Teton"
wate_temps <- wate_temps[,c("year", "MAT", "species")]
wate_temps$site <- "Waterfall"

# merge
temp_df <- rbind.data.frame(brazil_temps, panama_temps, guanica_temps,
                            powdermill_temps, palo_temps, tss_temps, wate_temps)

# temp plot
temp1 <- ggplot(data=temp_df, aes(year, MAT))+
  theme_bw() +
  theme(legend.position="none",
        strip.background = element_blank()) +
  ylab("Mean monthly temperature (°C)") +
  xlab("Year") +
  stat_smooth(geom='line', alpha=0.25, method='lm', aes(color=species)) +
  geom_smooth(method='lm', se = FALSE, color="black") +
  facet_wrap(~site, scales="free_y")

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s6.pdf", width=6, height=5)
temp1
dev.off()

# get slopes of change in temp
for(i in unique(temp_df$site)){
  tmp <- temp_df[temp_df$site==i,]
  mod <- lm(tmp$MAT ~ tmp$year)
  slope <- as.numeric(mod$coefficients[2])
  change_yr <- max(tmp$year) - min(tmp$year)
  print(paste0(i, " ", slope, " ", change_yr, " ", (slope*change_yr)))
}

# single climate regression for each site, using summer data for temp sites. temperature to start...
temperate_sites <- c("Powdermill","Teton","Waterfall","Palomarin")
tropical_sites <- c("Brazil","Guanica","Panama")
all_temps.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_temps.csv")
all_temps.df <- all_temps.df %>%
  gather(site, "MAT", 3:10) 
all_temps.df <- all_temps.df[!all_temps.df$site=="Puerto.Rico",]
all_temps.df <- all_temps.df[!all_temps.df$site %in% temperate_sites,]
all_temps.df$site <- factor(all_temps.df$site)
all_temps.df$site <- revalue(all_temps.df$site, 
                             c("Brazil"="Brazil", "Guanica"="Guanica","Panama"="Panama"))
summer_temps.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/summer_temps.csv")
summer_temps.df <- summer_temps.df %>%
  gather(site, "MAT", 3:10) 
summer_temps.df <- summer_temps.df[!summer_temps.df$site=="Puerto.Rico",]
summer_temps.df <- summer_temps.df[!summer_temps.df$site %in% tropical_sites,]
summer_temps.df$site <- factor(summer_temps.df$site)
summer_temps.df$site <- revalue(summer_temps.df$site, c("Palomarin"="Palomarin",
                                "Powdermill"="Powdermill",
                                "Teton"="Teton","Waterfall"="Waterfall"))
final_temps.df <- rbind.data.frame(all_temps.df, summer_temps.df)
temp2 <- ggplot(final_temps.df, aes(x=year,y=MAT)) +
  theme_bw() +
  theme(legend.position="none",
        strip.background = element_blank()) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~ site, scales="free_y") +
  ylab("Mean annual temperature (°C)") +
  xlab("Year")

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s7.pdf", width=6, height=5)
temp2
dev.off()

# ...now, precip:
all_precip.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_precip.csv")
all_precip.df <- all_precip.df %>%
  gather(site, "MAP", 3:10) 
all_precip.df <- all_precip.df[!all_precip.df$site=="Puerto.Rico",]
all_precip.df <- all_precip.df[!all_precip.df$site %in% temperate_sites,]
all_precip.df$site <- factor(all_precip.df$site)
all_precip.df$site <- revalue(all_precip.df$site, 
                              c("Brazil"="Brazil", "Guanica"="Guanica","Panama"="Panama"))
summer_precip.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/summer_precip.csv")
summer_precip.df <- summer_precip.df %>%
  gather(site, "MAP", 3:10) 
summer_precip.df <- summer_precip.df[!summer_precip.df$site=="Puerto.Rico",]
summer_precip.df <- summer_precip.df[!summer_precip.df$site %in% tropical_sites,]
summer_precip.df$site <- factor(summer_precip.df$site)
summer_precip.df$site <- revalue(summer_precip.df$site, c("Palomarin"="Palomarin",
                                                          "Powdermill"="Powdermill",
                                                          "Teton"="Teton","Waterfall"="Waterfall"))
final_precip.df <- rbind.data.frame(all_precip.df, summer_precip.df)
precip2 <- ggplot(final_precip.df, aes(x=year,y=MAP)) +
  theme_bw() +
  theme(legend.position="none",
        strip.background = element_blank()) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~ site, scales="free_y") +
  ylab("Mean monthly precipitation (cm)") +
  xlab("Year")

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s8.pdf", width=6, height=5)
precip2
dev.off()

# get slopes of change in precip
for(i in unique(final_precip.df$site)){
  tmp <- final_precip.df[final_precip.df$site==i,]
  mod <- lm(tmp$MAP ~ tmp$year)
  slope <- as.numeric(mod$coefficients[2])
  change_yr <- max(tmp$year) - min(tmp$year)
  print(paste0(i, " ", slope, " ", change_yr, " ", (slope*change_yr)))
}

# write precip dfs
precip_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered.csv",
              site_name = "Brazil",
              months = "SUMMER",
              output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_precips.csv")
precip_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered.csv",
              site_name = "Panama",
              months = "SUMMER",
              output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_precips.csv")
precip_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered.csv",
              site_name = "Guanica",
              months = "SUMMER",
              output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_precips.csv")
precip_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered.csv",
              site_name = "Powdermill",
              months = "SUMMER",
              output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_precips.csv")
precip_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered.csv",
              site_name = "Palomarin",
              months = "SUMMER",
              output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_precips.csv")
precip_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered.csv",
              site_name = "Teton",
              months = "SUMMER",
              output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_precips.csv")
precip_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered.csv",
              site_name = "Waterfall",
              months = "SUMMER",
              output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_precips.csv")

# load files
brazil_precips <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_precips.csv")[-1]
panama_precips <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_precips.csv")[-1]
guanica_precips <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_precips.csv")[-1]
powdermill_precips <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_precips.csv")[-1]
palo_precips <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_precips.csv")[-1]
tss_precips <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_precips.csv")[-1]
wate_precips <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_precips.csv")[-1]

# subset columns; add ID col
brazil_precips <- brazil_precips[,c("year", "MAP", "species")]
brazil_precips$site <- "Brazil"
panama_precips <- panama_precips[,c("year", "MAP", "species")]
panama_precips$site <- "Panama"
guanica_precips <- guanica_precips[,c("year", "MAP", "species")]
guanica_precips$site <- "Guanica"
powdermill_precips <- powdermill_precips[,c("year", "MAP", "species")]
powdermill_precips$site <- "Powdermill"
palo_precips <- palo_precips[,c("year", "MAP", "species")]
palo_precips$site <- "Palomarin"
tss_precips <- tss_precips[,c("year", "MAP", "species")]
tss_precips$site <- "Teton"
wate_precips <- wate_precips[,c("year", "MAP", "species")]
wate_precips$site <- "Waterfall"

# merge
precip_df <- rbind.data.frame(brazil_precips, panama_precips, guanica_precips,
                              powdermill_precips, palo_precips, tss_precips, wate_precips)

# precip plot
precip1 <- ggplot(data=precip_df, aes(year, MAP))+
  theme_bw() +
  theme(legend.position="none",
        strip.background = element_blank()) +
  ylab("Mean monthly precipation (cm)") +
  xlab("Year") +
  stat_smooth(geom='line', alpha=0.25, method='lm', aes(color=species)) +
  geom_smooth(method='lm', se = FALSE, color="black") +
  facet_wrap(~site, scales="free_y")


pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s9.pdf", width=6, height=5)
precip1
dev.off()

# correlated temp and precip plot
climate_corr <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis_df.csv")
climate_corr$site[climate_corr$lat %in% unique(climate_corr$lat)[1]] <- 'Brazil'
climate_corr$site[climate_corr$lat %in% unique(climate_corr$lat)[2]] <- 'Panama'
climate_corr$site[climate_corr$lat %in% unique(climate_corr$lat)[3]] <- 'Guanica'
climate_corr$site[climate_corr$lat %in% unique(climate_corr$lat)[4]] <- 'Palomarin'
climate_corr$site[climate_corr$lat %in% unique(climate_corr$lat)[5]] <- 'Teton'
climate_corr$site[climate_corr$lat %in% unique(climate_corr$lat)[6]] <- 'Powdermill'
climate_corr$site[climate_corr$lat %in% unique(climate_corr$lat)[7]] <- 'Waterfall'

corr <- ggplot(data=climate_corr, aes(x=slope_temp, y=slope_precip, color=site)) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  geom_point() +
  ylab("Change mean monthly precipation") +
  xlab("Change mean monthly temperature") +
  labs(color="Site")
  
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s10.pdf", width=6, height=5)
corr
dev.off()

# evaluate guanica anomaly
precip.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/precip_data.csv")
regexp <- "[[:digit:]]+" #regex to extract year from column names
colnames(precip.df) <- c("site",str_extract(colnames(precip.df), regexp)[-1]) #rename columns
site <- as.vector(as.character(precip.df$site))

# load lat long data
latlong.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/latlong.csv")

# reformat
precip.df <- subset(precip.df, select = -c(site))
x <- as.data.frame(precip.df)
precip.df <- as.data.frame(lapply(split(as.list(x),
                                        f = colnames(x)),
                                  function(x) Reduce(`+`, x) / length(x)))
precip.df <- cbind.data.frame(site,precip.df)

# transpose 
precip.df <- precip.df %>%
  rownames_to_column %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) 
cols <- precip.df[1,]
cols[1] <- "year"
rownames(precip.df) <- NULL
colnames(precip.df) <- NULL
precip.df <- precip.df[-1,]
colnames(precip.df) <- cols

# tropics precip precip (all months)
all_precip.df <- precip.df
all_precip.df$year <- str_extract(all_precip.df$year, regexp) #rename columns
all_precip.df$year <- as.numeric(all_precip.df$year)
all_precip.df <- all_precip.df %>% mutate_if(is.character,as.numeric)
all_precip.df <- all_precip.df %>%
  group_by(year) %>% 
  summarise_at(vars("Powdermill","Teton","Waterfall",
                    "Panama","Brazil","Puerto Rico","Palomarin","Guanica"), mean)

# figure s12: distribution of starting mass
mass_dist <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis_df.csv")
mass_dist$site[mass_dist$lat %in% unique(mass_dist$lat)[1]] <- 'Brazil'
mass_dist$site[mass_dist$lat %in% unique(mass_dist$lat)[2]] <- 'Panama'
mass_dist$site[mass_dist$lat %in% unique(mass_dist$lat)[3]] <- 'Guanica'
mass_dist$site[mass_dist$lat %in% unique(mass_dist$lat)[4]] <- 'Palomarin'
mass_dist$site[mass_dist$lat %in% unique(mass_dist$lat)[5]] <- 'Powdermill'
mass_dist$site[mass_dist$lat %in% unique(mass_dist$lat)[6]] <- 'Waterfall'
mass_dist$site[mass_dist$lat %in% unique(mass_dist$lat)[7]] <- 'Teton'
mass_dist$site <- factor(mass_dist$site, levels=c("Brazil", "Panama", "Guanica", "Palomarin", 
                                                  "Powdermill", "Waterfall", "Teton"))
mass_dist$lat <- as.numeric(as.character(mass_dist$lat))
mass_dist_plot <- ggplot(data=mass_dist, aes(x=site, y=starting_mass)) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  geom_boxplot(alpha=0.7, aes(fill=abs(lat)))+
  geom_jitter(pch=21, alpha=0.5) +
  labs(fill="abs(Latitude)") +
  ylab("Starting mass (g)") +
  xlab("Site")

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s12.pdf", width=7, height=5)
mass_dist_plot
dev.off()
