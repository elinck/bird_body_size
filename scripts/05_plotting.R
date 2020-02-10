### create plots for manuscript

library(ggplot2)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeo)
library(ggspatial)

# load brazil data
brazil.plotting.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_sex.csv")[-1]

# plot with sexes separate
b1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=brazil.plotting.sex[brazil.plotting.sex$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=brazil.plotting.sex[brazil.plotting.sex$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=brazil.plotting.sex[brazil.plotting.sex$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=brazil.plotting.sex[brazil.plotting.sex$sex %in% "F",]) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/brazil_mass_sex.png",res=300,width=18,height=18,units="in")
b1
dev.off()

# load full data
brazil.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered.csv")[-1]

# plot overall trend
b2 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=brazil.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=brazil.plotting) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/brazil_mass_all.png",res=300,width=18,height=18,units="in")
b2
dev.off()

# load panama data
panama.plotting.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered_sex.csv")[-1]

# plot with sexes separate
p1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=panama.plotting.sex[panama.plotting.sex$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=panama.plotting.sex[panama.plotting.sex$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=panama.plotting.sex[panama.plotting.sex$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=panama.plotting.sex[panama.plotting.sex$sex %in% "F",]) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/panama_mass_sex.png",res=300,width=18,height=18,units="in")
p1
dev.off()

# load full data
panama.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered.csv")[-1]

# plot overall trend
p2 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=panama.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=panama.plotting) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/panama_mass_all.png",res=300,width=18,height=18,units="in")
p2
dev.off()

# load guanica data
guanica.plotting.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered_sex.csv")[-1]

# plot with sexes separate
g1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=guanica.plotting.sex[guanica.plotting.sex$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=guanica.plotting.sex[guanica.plotting.sex$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=guanica.plotting.sex[guanica.plotting.sex$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=guanica.plotting.sex[guanica.plotting.sex$sex %in% "F",]) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/guanica_mass_sex.png",res=300,width=18,height=18,units="in")
g1
dev.off()

# load full data
guanica.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered.csv")[-1]

# plot overall trend
g2 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=guanica.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=guanica.plotting) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/guanica_mass_all.png",res=300,width=18,height=18,units="in")
g2
dev.off()

# load palo data
palo.plotting.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered_sex.csv")[-1]

# plot with sexes separate
pal1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=palo.plotting.sex[palo.plotting.sex$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=palo.plotting.sex[palo.plotting.sex$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=palo.plotting.sex[palo.plotting.sex$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=palo.plotting.sex[palo.plotting.sex$sex %in% "F",]) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/palo_mass_sex.png",res=300,width=18,height=18,units="in")
pal1
dev.off()

# load full data
palo.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered.csv")[-1]

# plot overall trend
pal2 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=palo.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=palo.plotting) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

# save pdf
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/palo_mass_all.png",res=300,width=18,height=18,units="in")
pal2
dev.off()

# load powdermill data
powdermill.plotting.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered_sex.csv")[-1]

# plot with sexes separate
pw1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=powdermill.plotting.sex[powdermill.plotting.sex$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=powdermill.plotting.sex[powdermill.plotting.sex$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=powdermill.plotting.sex[powdermill.plotting.sex$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=powdermill.plotting.sex[powdermill.plotting.sex$sex %in% "F",]) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/powdermill_mass_sex.png",res=300,width=18,height=18,units="in")
pw1
dev.off()

# load full data
powdermill.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered.csv")[-1]

# plot overall trend
pw2 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=powdermill.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=powdermill.plotting) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/powdermill_mass_all.png",res=300,width=18,height=18,units="in")
pw2
dev.off()

# load tss data
tss.plotting.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered_sex.csv")[-1]

# plot with sexes separate
tss1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=tss.plotting.sex[tss.plotting.sex$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=tss.plotting.sex[tss.plotting.sex$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=tss.plotting.sex[tss.plotting.sex$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=tss.plotting.sex[tss.plotting.sex$sex %in% "F",]) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/tss_mass_sex.png",res=300,width=18,height=18,units="in")
tss1
dev.off()

# load full data
tss.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered.csv")[-1]

# plot overall trend
tss2 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=tss.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=tss.plotting) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/tss_mass_all.png",res=300,width=18,height=18,units="in")
tss2
dev.off()

# load wate data
wate.plotting.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered_sex.csv")[-1]

# plot with sexes separate
wate1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=wate.plotting.sex[wate.plotting.sex$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=wate.plotting.sex[wate.plotting.sex$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=wate.plotting.sex[wate.plotting.sex$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=wate.plotting.sex[wate.plotting.sex$sex %in% "F",]) +
  theme_bw() +
  labs(y="Mass", x="Year") +
  facet_wrap(~species, scales="free") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/wate_mass_sex.png",res=300,width=18,height=18,units="in")
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
  facet_wrap(~species, scales="free") +
  theme(
    strip.background = element_blank(),
    legend.position="none",
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) 

# save png
png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/wate_mass_all.png",res=300,width=18,height=18,units="in")
wate2
dev.off()

# plot histogram of records over months
months.df <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/months.csv")[-1]
months.df$country <- factor(months.df$country,levels=c("brazil","panama","puertorico","palo","maps","powdermill"))
levels(months.df$country) <- c("Brazil","Panama","Puerto Rico","Palo","MAPS","Powdermill")

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

png("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/records_by_site_by_month.pdf",res=300,width=6,height=10,units="in")
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

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/mass_correlations.pdf",width=8,height=4)
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

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/tarsus_correlations_site.pdf",width=8,height=4)
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

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/wing_correlations_site.pdf",width=8,height=8)
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

# $panama
# r2     slope
# 1 0.5152546 0.1407675
# 
# $guanica
# r2     slope
# 1 0.7734583 0.2101806
# 
# $palo
# r2     slope
# 1 0.6660911 0.2467116

wing_length.mod <- list()
for(i in unique(corr.wing.df$site)){
  tmp <- corr.wing.df[corr.wing.df$site==i,]
  tmp <- tmp[!is.na(tmp$wing_length),]
  mod <- summary(lm(wing_length ~ mass, tmp))
  r2 <- mod$adj.r.squared
  slope <- mod$coefficients[2,1]
  wing_length.mod[[i]] <- cbind.data.frame(r2,slope)
}

# $brazil
# r2     slope
# 1 0.7862534 0.7463115
#
# $panama
# r2     slope
# 1 0.7445751 0.6497541
# 
# $guanica
# r2     slope
# 1 0.8903666 0.9034451
# 
# $palo
# r2    slope
# 1 0.8199822 0.945038
# 
# $powdermill
# r2     slope
# 1 0.7651781 0.9135968
# 
# $tss
# r2     slope
# 1 0.8845124 0.9380705
# 
# $wate
# r2      slope
# 1 -0.01569919 -0.1248166

# phylogeny 
master.df <- read.csv("data/all_analysis_df.csv")
master.df <- master.df[!master.df$slope_mass>0.5,]
master.df <- master.df[!master.df$slope_mass<(-0.5),]
master.tree <- read.tree("data/all_analysis_phy.tree")
master.tree <- keep.tip(master.tree, master.df$species)
master.slope <- cbind.data.frame(master.df$species, master.df$slope_mass)
p1 <- ggtree(master.tree, layout='circular') %<+% master.slope 
p1$data$`master.df$slope_mass`[is.na(p1$data$`master.df$slope_mass`)] <- 0
d <- ggimage::phylopic_uid(master.tree$tip.label)
p2 <- ggtree(master.tree, layout='circular') %<+% master.slope + 
  aes(color=I(p1$data$`master.df$slope_mass`),guides="test") +
  scale_color_gradient2(midpoint = mean(p1$data$`master.df$slope_mass`), low = "Blue", mid = "#9E9E9E",
                        high = "Red", space = "Lab" ) + 
  geom_cladelabel(node=535, "Columbidae", fontsize=2, hjust = -0.15) +
  geom_cladelabel(node=275, "Trochilidae", fontsize=2, hjust = -0.15) +
  geom_cladelabel(node=280, "Scolopacidae", fontsize=2, hjust = -0.15) +
  #geom_cladelabel(node=516, "Accipitridae", fontsize=2) +
  #geom_cladelabel(node=517, "Strigidae", fontsize=2) +
  geom_cladelabel(node=521, "Alcedinidae", fontsize=2, hjust = -0.15) +
  geom_cladelabel(node=522, "Momotidae", fontsize=2, hjust = -0.15) +
  geom_cladelabel(node=525, "Bucconidae", fontsize=2, hjust = -0.15) +
  geom_cladelabel(node=254, "Ramphastidae", fontsize=2, hjust = -1.1) +
  geom_cladelabel(node=288, "Tyrannidae", fontsize=2, hjust = -1.1) +
  geom_cladelabel(node=529, "Picidae", fontsize=2, hjust = -0.15) +
  geom_cladelabel(node=390, "Corvidae", fontsize=2, hjust = 1.1) +
  geom_cladelabel(node=441, "Parulidae", fontsize=2, hjust = -0.15) +
  geom_cladelabel(node=480, "Passerelidae", fontsize=2, hjust = -0.15) +
  geom_cladelabel(node=496, "Icteridae", fontsize=2, offset.text=1) +
  geom_cladelabel(node=9, "Falconidae", fontsize=2, hjust = -1.1) + 
  geom_cladelabel(node=321, "Pipridae", fontsize=2, hjust = -0.5) + 
  geom_cladelabel(node=317, "Tityridae", fontsize=2, hjust = -0.5) + 
  geom_cladelabel(node=328, "Thamnophilidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=79, "Conopohagidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=379, "Formicariidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=357, "Furnariidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=382, "Vireonidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=394, "Paridae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=121, "Psaltriparidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=122, "Sylviidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=398, "Hirundinidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=404, "Regulidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=133, "Sturnidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=409, "Mimidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=411, "Turdidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=435, "Sittidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=145, "Certhiidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=433, "Polioptidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=424, "Troglodytidae", fontsize=2, hjust = 1.1) + 
  geom_cladelabel(node=502, "Cardinalidae", fontsize=2, offset.text=1) + 
  geom_cladelabel(node=510, "Thraupidae", fontsize=2, hjust = 0.75, offset.text=1) +
  theme(legend.position="right") +
  labs(color=expression(paste("Slope ",Delta," mass")))
  
  
mod1 <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/mod_all_fit.csv")[,-1]
mod1 <- mod1[2:4,]

p3 <- ggplot(mod1, aes(x=term, y=estimate, colour=term)) + 
  theme_classic() +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=.1) +
  geom_line() +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_point(pch=1,size=3) +
  theme(axis.text.x=element_blank()) +
  geom_text(data=subset(mod1, mod1$term=="slope_temp"), y = -0.75, label = "***") +
  ylab(expression(Beta)) +
  xlab(element_blank()) +
  labs(color="Model Term") +
  scale_color_manual(name = "Model Term", labels = c("Latitude", "Temperature Change", "Starting Mass"),
                       values = c("#d8b365", "Black", "#5ab4ac")) #+
  #theme(legend.position = "bottom")

  
plot_grid(p2,p3, rel_widths = c(0.75,0.25),ncol=1)
pdf("~/Desktop/phylo_test.pdf", width=7, height=7)
p2
dev.off()

pdf("~/Desktop/slope_test.pdf", width=7, height=5)
p3
dev.off()

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

pdf("~/Desktop/map_test.pdf", width=6, height=5)
p4
dev.off()



  
