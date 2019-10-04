### create plots for manuscript

library(ggplot2)

# load brazil data
brazil.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered.csv")[-1]

# plot with sexes separate
b1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=brazil.plotting[brazil.plotting$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=brazil.plotting[brazil.plotting$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=brazil.plotting[brazil.plotting$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=brazil.plotting[brazil.plotting$sex %in% "F",]) +
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
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/brazil_mass_sex.pdf",width=10,height=10)
b1
dev.off()

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
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/brazil_mass_all.pdf",width=10,height=10)
b2
dev.off()

# load panama data
panama.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered.csv")[-1]

# plot with sexes separate
p1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=panama.plotting[panama.plotting$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=panama.plotting[panama.plotting$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=panama.plotting[panama.plotting$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=panama.plotting[panama.plotting$sex %in% "F",]) +
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
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/panama_mass_sex.pdf",width=10,height=10)
p1
dev.off()

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
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/panama_mass_all.pdf",width=10,height=10)
p2
dev.off()

# load puerto rico data
puertorico.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/puertorico_filtered.csv")[-1]

# plot overall trend
pr1 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=puertorico.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=puertorico.plotting) +
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
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/puertorico_mass_all.pdf",width=10,height=10)
pr1
dev.off()

# load palo data
palo.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered.csv")[-1]

# plot with sexes separate
pa1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=palo.plotting[palo.plotting$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=palo.plotting[palo.plotting$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=palo.plotting[palo.plotting$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=palo.plotting[palo.plotting$sex %in% "F",]) +
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
pa1
dev.off()

# plot overall trend
pa2 <- ggplot() +
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
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/palo_mass_all.pdf",width=12,height=12)
pa2
dev.off()

# load maps data
maps.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/maps_filtered.csv")[-1]

# plot with sexes separate
m1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=maps.plotting[maps.plotting$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=maps.plotting[maps.plotting$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=maps.plotting[maps.plotting$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=maps.plotting[maps.plotting$sex %in% "F",]) +
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
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/maps_mass_sex.pdf",width=12,height=12)
m1
dev.off()

# plot overall trend
m2 <- ggplot() +
  geom_point(aes(y=mass, x=year), shape=1, data=maps.plotting) +
  geom_smooth(aes(y=mass, x=year), method="lm", color="black", linetype="dashed", data=maps.plotting) +
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
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/maps_mass_all.pdf",width=12,height=12)
m2
dev.off()

# load powdermill data
powdermill.plotting <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered.csv")[-1]

# plot with sexes separate
pw1 <- ggplot() +
  geom_point(aes(y=mass, x=year), col="#0345fc", shape=1, data=powdermill.plotting[powdermill.plotting$sex %in% "M",]) +
  geom_smooth(aes(y=mass, x=year), col="#0345fc", fill="#0345fc", method="lm", data=powdermill.plotting[powdermill.plotting$sex %in% "M",]) +
  geom_point(aes(y=mass, x=year), col="#fc9803", shape=1, data=powdermill.plotting[powdermill.plotting$sex %in% "F",]) +
  geom_smooth(aes(y=mass, x=year), col="#fc9803", fill="#fc9803", method="lm", data=powdermill.plotting[powdermill.plotting$sex %in% "F",]) +
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
pdf("~/Dropbox/Bird_body_size-analysis/bird_body_size/plots/powdermill_mass_sex.pdf",width=18,height=18)
pw1
dev.off()

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
