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

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s1.pdf",width=6,height=10)
m1
dev.off()

# make master data frame for size correlations
corr.brazil.df <- cbind.data.frame(brazil.plotting, rep("brazil",nrow(brazil.plotting))) #panama.plotting, guanica.plotting, powdermill.plotting, palo.plotting, wate.plotting)
colnames(corr.brazil.df) <- c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period","site")
corr.panama.df <- cbind.data.frame(panama.plotting, rep("panama",nrow(panama.plotting))) #panama.plotting, guanica.plotting, powdermill.plotting, palo.plotting, wate.plotting)
colnames(corr.panama.df) <- c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period","site")
corr.guanica.df <- cbind.data.frame(guanica.plotting, rep("guanica",nrow(guanica.plotting))) #guanica.plotting, guanica.plotting, powdermill.plotting, palo.plotting, wate.plotting)
colnames(corr.guanica.df) <- c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period","site")
corr.palo.df <- cbind.data.frame(palo.plotting, rep("palo",nrow(palo.plotting))) #palo.plotting, palo.plotting, powdermill.plotting, palo.plotting, wate.plotting)
colnames(corr.palo.df) <- c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period","site")
corr.powdermill.df <- cbind.data.frame(powdermill.plotting, rep("powdermill",nrow(powdermill.plotting))) #powdermill.plotting, powdermill.plotting, powdermill.plotting, powdermill.plotting, wate.plotting)
colnames(corr.powdermill.df) <- c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period","site")
corr.tss.df <- cbind.data.frame(tss.plotting, rep("tss",nrow(tss.plotting))) #tss.plotting, tss.plotting, tss.plotting, tss.plotting, wate.plotting)
colnames(corr.tss.df) <- c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period","site")
corr.wate.df <- cbind.data.frame(wate.plotting, rep("wate",nrow(wate.plotting))) #maps.plotting, maps.plotting, maps.plotting, maps.plotting, wate.plotting)
colnames(corr.wate.df) <- c("species", "month", "year", "band_no", "sex","age","mass","wing_length","tarsus","period","site")
corr.master.df <- rbind.data.frame(corr.brazil.df,corr.panama.df,corr.guanica.df,corr.palo.df,corr.powdermill.df,corr.tss.df,corr.wate.df)
corr.master.df$tarsus <- as.numeric(as.character(corr.master.df$tarsus))
corr.master.df$wing_length <- as.numeric(as.character(corr.master.df$wing_length))

# tarsus
corr.tarsus.df <- corr.master.df[!is.na(corr.master.df$tarsus),]
corr.tarsus.df <- corr.tarsus.df[!corr.tarsus.df$tarsus>90,]
corr.tarsus.df <-corr.tarsus.df[!corr.tarsus.df$tarsus<5,]
corr.tarsus.df$site <- as.factor(corr.tarsus.df$site)
corr.tarsus.df$site <- recode(corr.tarsus.df$site, brazil = "brazil", panama = "panama", guanica = "guanica",
                            powdermill = "powdermill", palo = "palomarin", tss = "teton science school",
                            wate = "waterfall glen")


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
corr.wing.df$site <- recode(corr.wing.df$site, brazil = "brazil", panama = "panama", guanica = "guanica",
                       powdermill = "powdermill", palo = "palomarin", tss = "teton science school",
                       wate = "waterfall glen")

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

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s2.pdf",width=8,height=4)
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

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s3.pdf",width=8,height=4)
p1
dev.off()

q1 <- ggplot(corr.wing.df, aes(x=mass,y=wing_length)) +
  theme_classic() +
  geom_point(pch=1,color="#cc8800",alpha=0.7) +
  #annotate(geom = 'text', label = tarsus.annotate, x=90, y=Inf, hjust = 0, vjust = 1) +
  geom_smooth(method = "lm",color="black",linetype="dashed") +
  xlab("Mass (g)") +
  ylab("Tarsus (mm)") +
  facet_wrap(~site, scales="free")

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s4.pdf",width=8,height=8)
q1
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

# $panama
# r2     slope
# 1 0.5357902 0.1398531
# 
# $guanica
# r2     slope
# 1 0.7973442 0.2186975
# 
# $palo
# r2     slope
# 1 0.6610499 0.2736614

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
master.df <- master.df[!master.df$slope_mass>0.5,]
master.df <- master.df[!master.df$slope_mass<(-0.5),]
master.tree <- read.tree("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis_phy.tree")
master.tree <- keep.tip(master.tree, master.df$species)
master.slope <- cbind.data.frame(master.df$species, master.df$slope_mass)
p1 <- ggtree(master.tree, layout='circular') %<+% master.slope 
p1$data$`master.df$slope_mass`[is.na(p1$data$`master.df$slope_mass`)] <- 0
p2 <- ggtree(master.tree, layout='circular',size=1.5) %<+% master.slope + 
  aes(color=p1$data$`master.df$slope_mass`,guides="test") +
  scale_color_distiller(palette = "BrBG") +
  geom_tiplab(size=2) +
  # geom_text(aes(label=node)) +
  theme(legend.position="right") +
  labs(color=expression(paste("Slope ",Delta," mass"))) +
  geom_cladelabel(node=353, label="", angle=0, offset=2) +
  geom_cladelabel(node=289, label="", angle=0, offset=2) +
  geom_cladelabel(node=377, label="", angle=0, offset=2) +
  geom_cladelabel(node=393, label="", angle=0, offset=2) +
  geom_cladelabel(node=349, label="", angle=0, offset=2) +
  geom_cladelabel(node=228, label="", angle=0, offset=2) +
  geom_cladelabel(node=265, label="", angle=0, offset=2) +
  geom_cladelabel(node=259, label="", angle=0, offset=2) +
  geom_cladelabel(node=405, label="", angle=0, offset=2) +
  geom_cladelabel(node=216, label="", angle=0, offset=2) +
  geom_cladelabel(node=423, label="", angle=0, offset=2) +
  geom_cladelabel(node=420, label="", angle=0, offset=2) +
  geom_cladelabel(node=307, label="", angle=0, offset=2) +
  geom_cladelabel(node=338, label="", angle=0, offset=2) +
  geom_cladelabel(node=330, label="", angle=0, offset=2) +
  geom_cladelabel(node=330, label="", angle=0, offset=2) +
  geom_cladelabel(node=320, label="", angle=0, offset=2) +
  geom_cladelabel(node=325, label="", angle=0, offset=2) +
  geom_cladelabel(node=316, label="", angle=0, offset=2)

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/figure_1_components/phylogeny.pdf", width=8, height=8)
p2
dev.off()

  
mod1 <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/stats_with_fragments/m4.combined.old.csv")
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

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/figure_1_components/betas.pdf", width=4, height=3)
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

# site-specific intercept plot
intercept.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/site_intercept.csv")[-1]
s5 <- ggplot(intercept.df, aes(x=site, y=intercept, color=site)) + 
  theme_classic() +
  scale_color_brewer(palette="Set1") +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.1) +
  geom_line() +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_point(pch=1,size=3) +
  theme(axis.text.x=element_blank()) +
  # geom_text(data=subset(mod1, mod1$term=="slope_temp"), y = -0.75, label = "***") +
  ylab(expression("intercept")) +
  xlab(element_blank()) +
  labs(color="site") 

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s5.pdf", width=6, height=5)
s5
dev.off()

# write temp dfs
temp_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered.csv",
            site_name = "Brazil",
            output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_temps.csv")
temp_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered.csv",
            site_name = "Panama",
            output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_temps.csv")
temp_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered.csv",
            site_name = "Guanica",
            output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_temps.csv")
temp_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered.csv",
            site_name = "Powdermill",
            output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_temps.csv")
temp_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered.csv",
            site_name = "Palomarin",
            output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_temps.csv")
temp_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered.csv",
            site_name = "Teton",
            output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_temps.csv")
temp_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered.csv",
            site_name = "Waterfall",
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
brazil_temps$site <- "brazil"
panama_temps <- panama_temps[,c("year", "MAT", "species")]
panama_temps$site <- "panama"
guanica_temps <- guanica_temps[,c("year", "MAT", "species")]
guanica_temps$site <- "guanica"
powdermill_temps <- powdermill_temps[,c("year", "MAT", "species")]
powdermill_temps$site <- "powdermill"
palo_temps <- palo_temps[,c("year", "MAT", "species")]
palo_temps$site <- "palomarin"
tss_temps <- tss_temps[,c("year", "MAT", "species")]
tss_temps$site <- "teton"
wate_temps <- wate_temps[,c("year", "MAT", "species")]
wate_temps$site <- "waterfall"

# merge
temp_df <- rbind.data.frame(brazil_temps, panama_temps, guanica_temps,
                            powdermill_temps, palo_temps, tss_temps, wate_temps)

# temp plot
temp1 <- ggplot(data=temp_df, aes(year, MAT, color=species))+
  theme_bw() +
  theme(legend.position="none",
        strip.background = element_blank()) +
  ylab("mean annual temperature (°C)") +
  geom_smooth(method='lm', se = FALSE) +
  facet_wrap(~site, scales="free_y")


pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s6.pdf", width=6, height=5)
temp1
dev.off()

# write precip dfs
precip_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered.csv",
              site_name = "Brazil",
              output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_precips.csv")
precip_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered.csv",
              site_name = "Panama",
              output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_precips.csv")
precip_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered.csv",
              site_name = "Guanica",
              output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_precips.csv")
precip_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered.csv",
              site_name = "Powdermill",
              output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_precips.csv")
precip_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered.csv",
              site_name = "Palomarin",
              output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_precips.csv")
precip_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered.csv",
              site_name = "Teton",
              output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_precips.csv")
precip_change(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered.csv",
              site_name = "Waterfall",
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
brazil_precips <- brazil_precips[,c("year", "MAT", "species")]
brazil_precips$site <- "brazil"
panama_precips <- panama_precips[,c("year", "MAT", "species")]
panama_precips$site <- "panama"
guanica_precips <- guanica_precips[,c("year", "MAT", "species")]
guanica_precips$site <- "guanica"
powdermill_precips <- powdermill_precips[,c("year", "MAT", "species")]
powdermill_precips$site <- "powdermill"
palo_precips <- palo_precips[,c("year", "MAT", "species")]
palo_precips$site <- "palomarin"
tss_precips <- tss_precips[,c("year", "MAT", "species")]
tss_precips$site <- "teton"
wate_precips <- wate_precips[,c("year", "MAT", "species")]
wate_precips$site <- "waterfall"

# merge
precip_df <- rbind.data.frame(brazil_precips, panama_precips, guanica_precips,
                              powdermill_precips, palo_precips, tss_precips, wate_precips)

# precip plot
precip1 <- ggplot(data=precip_df, aes(year, MAT, color=species))+
  theme_bw() +
  theme(legend.position="none",
        strip.background = element_blank()) +
  ylab("mean monthly precipation (cm)") +
  geom_smooth(method='lm', se = FALSE) +
  facet_wrap(~site, scales="free_y")


pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/figures/s7.pdf", width=6, height=5)
precip1
dev.off()

