### create plots for manuscript

library(ggplot2)
library(cowplot)

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

# save pdf
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/brazil_mass_sex.pdf",width=12,height=12)
b1
dev.off()

# load full data
brazil.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_all.csv")[-1]

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

# save pdf
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/brazil_mass_all.pdf",width=16,height=16)
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

# save pdf
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/panama_mass_sex.pdf",width=12,height=12)
p1
dev.off()

# load full data
panama.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered_all.csv")[-1]

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

# save pdf
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/panama_mass_all.pdf",width=16,height=16)
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

# save pdf
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/guanica_mass_sex.pdf",width=12,height=12)
g1
dev.off()

# load full data
guanica.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered_all.csv")[-1]

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

# save pdf
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/guanica_mass_all.pdf",width=16,height=16)
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

# save pdf
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/palo_mass_sex.pdf",width=12,height=12)
pal1
dev.off()

# load full data
palo.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered_all.csv")[-1]

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
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/palo_mass_all.pdf",width=16,height=16)
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

# save pdf
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/powdermill_mass_sex.pdf",width=12,height=12)
pw1
dev.off()

# load full data
powdermill.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered_all.csv")[-1]

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

# save pdf
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/powdermill_mass_all.pdf",width=18,height=18)
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

# save pdf
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/tss_mass_sex.pdf",width=10,height=10)
tss1
dev.off()

# load full data
tss.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered_all.csv")[-1]

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

# save pdf
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/tss_mass_all.pdf",width=10,height=10)
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

# save pdf
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/wate_mass_sex.pdf",width=10,height=10)
wate1
dev.off()

# load full data
wate.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered_all.csv")[-1]

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

# save pdf
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/wate_mass_all.pdf",width=10,height=10)
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

pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/records_by_site_by_month.pdf",width=6,height=10)
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



  
