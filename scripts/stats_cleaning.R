### statistics

library(ape)
library(phangorn)
library(stringr)
library(tidyverse)
library(brms)
library(ggtree)

# load temp data
temp.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/temp_data.csv")
regexp <- "[[:digit:]]+" #regex to extract year from column names
colnames(temp.df) <- c("site",str_extract(colnames(temp.df), regexp)[-1]) #rename columns
site <- as.vector(as.character(temp.df$site))

# load lat long data
latlong.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/latlong.csv")

# calculate annual mean
x <- as.data.frame(temp.df)
temp.df <- as.data.frame(lapply(split(as.list(x),f = colnames(x)),function(x) Reduce(`+`,x) / length(x)))
temp.df <- subset(temp.df, select = -c(site) )
temp.df <- cbind.data.frame(site,temp.df)

# transpose 
temp.df <- temp.df %>%
  rownames_to_column %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) 
cols <- temp.df[1,]
cols[1] <- "year"
rownames(temp.df) <- NULL
colnames(temp.df) <- NULL
temp.df <- temp.df[-1,]
colnames(temp.df) <- cols
temp.df$year <- str_extract(temp.df$year, regexp) #rename columns
temp.df$year <- as.numeric(temp.df$year)

# load phylogenies
# supertree <-read.tree("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/AllBirdsHackett1.tre")
# find maximum clade credibility tree
# supertree <- maxCladeCred(supertree)
# write.tree(supertree, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/birds_mcc.tre")

# load MCC tree
supertree <-read.tree("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/birds_mcc.tre")
supertree.species <- supertree$tip.label # save vector of species names

### all species w/o sex data

### brazil!

# load data 
brazil.all <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_all.csv")[-1]

# subset temp data, merge with df
brazil.temps <- cbind.data.frame(temp.df$year, temp.df$Brazil)
colnames(brazil.temps) <- c("year", "MAT")
brazil.temps$MAT <- as.numeric(as.character(brazil.temps$MAT))
brazil.all <- merge(brazil.all, brazil.temps, by.x="year", by.y="year", all.y=FALSE)

# loop to get mean mass change per species, males: 
brazil.delta <- list()
for(i in unique(brazil.all$species)){
  tmp <- brazil.all[brazil.all$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  brazil.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start)
}

brazil.mass.df <- do.call(rbind, brazil.delta)
rownames(brazil.mass.df) <- NULL
colnames(brazil.mass.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass")
brazil.mass.df$lat <- rep(latlong.df[latlong.df$site=="Brazil",]$lat, nrow(brazil.mass.df))
brazil.mass.df$long <- rep(latlong.df[latlong.df$site=="Brazil",]$long, nrow(brazil.mass.df))

### panama!

# load data 
panama.all <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered_all.csv")[-1]

# subset temp data, merge with df
panama.temps <- cbind.data.frame(temp.df$year, temp.df$Panama)
colnames(panama.temps) <- c("year", "MAT")
panama.temps$MAT <- as.numeric(as.character(panama.temps$MAT))
panama.all <- merge(panama.all, panama.temps, by.x="year", by.y="year", all.y=FALSE)


# loop to get mean mass change per species, males: 
panama.delta <- list()
for(i in unique(panama.all$species)){
  tmp <- panama.all[panama.all$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  panama.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start)
}

panama.mass.df <- do.call(rbind, panama.delta)
rownames(panama.mass.df) <- NULL
colnames(panama.mass.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass")
panama.mass.df$lat <- rep(latlong.df[latlong.df$site=="Panama",]$lat, nrow(panama.mass.df))
panama.mass.df$long <- rep(latlong.df[latlong.df$site=="Panama",]$long, nrow(panama.mass.df))

### guanica!

# load data 
guanica.all <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered_all.csv", fileEncoding="latin1")[-1]

# subset temp data, merge with df
guanica.temps <- cbind.data.frame(temp.df$year, temp.df$Guanica)
colnames(guanica.temps) <- c("year", "MAT")
guanica.temps$MAT <- as.numeric(as.character(guanica.temps$MAT))
guanica.all <- merge(guanica.all, guanica.temps, by.x="year", by.y="year", all.y=FALSE)

# loop to get mean mass change per species, males: 
guanica.delta <- list()
for(i in unique(guanica.all$species)){
  tmp <- guanica.all[guanica.all$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  guanica.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start)
}

guanica.mass.df <- do.call(rbind, guanica.delta)
rownames(guanica.mass.df) <- NULL
colnames(guanica.mass.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass")
guanica.mass.df$lat <- rep(latlong.df[latlong.df$site=="Guanica",]$lat, nrow(guanica.mass.df))
guanica.mass.df$long <- rep(latlong.df[latlong.df$site=="Guanica",]$long, nrow(guanica.mass.df))

### palo!

# load data 
palo.all <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered_all.csv")[-1]

# subset temp data, merge with df
palo.temps <- cbind.data.frame(temp.df$year, temp.df$Palo)
colnames(palo.temps) <- c("year", "MAT")
palo.temps$MAT <- as.numeric(as.character(palo.temps$MAT))
palo.all <- merge(palo.all, palo.temps, by.x="year", by.y="year", all.y=FALSE)

# loop to get mean mass change per species
palo.delta <- list()
for(i in unique(palo.all$species)){
  tmp <- palo.all[palo.all$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  palo.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start)
}

palo.mass.df <- do.call(rbind, palo.delta)
rownames(palo.mass.df) <- NULL
colnames(palo.mass.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass")
palo.mass.df$lat <- rep(latlong.df[latlong.df$site=="Palomarin",]$lat, nrow(palo.mass.df))
palo.mass.df$long <- rep(latlong.df[latlong.df$site=="Palomarin",]$long, nrow(palo.mass.df))

### powdermill!

# load data 
powdermill.all <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered_all.csv")[-1]

# subset temp data, merge with df
powdermill.temps <- cbind.data.frame(temp.df$year, temp.df$Powdermill)
colnames(powdermill.temps) <- c("year", "MAT")
powdermill.temps$MAT <- as.numeric(as.character(powdermill.temps$MAT))
powdermill.all <- merge(powdermill.all, powdermill.temps, by.x="year", by.y="year", all.y=FALSE)

# loop to get mean mass change per species
powdermill.delta <- list()
for(i in unique(powdermill.all$species)){
  tmp <- powdermill.all[powdermill.all$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  powdermill.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start)
}

powdermill.mass.df <- do.call(rbind, powdermill.delta)
rownames(powdermill.mass.df) <- NULL
colnames(powdermill.mass.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass")
powdermill.mass.df$lat <- rep(latlong.df[latlong.df$site=="Powdermill",]$lat, nrow(powdermill.mass.df))
powdermill.mass.df$long <- rep(latlong.df[latlong.df$site=="Powdermill",]$long, nrow(powdermill.mass.df))

### TSS!

# load data 
teton.all <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered_all.csv")[-1]

# subset temp data, merge with df
teton.temps <- cbind.data.frame(temp.df$year, temp.df$Teton)
colnames(teton.temps) <- c("year", "MAT")
teton.temps$MAT <- as.numeric(as.character(teton.temps$MAT))
teton.all <- merge(teton.all, teton.temps, by.x="year", by.y="year", all.y=FALSE)

# loop to get mean mass change per species
teton.delta <- list()
for(i in unique(teton.all$species)){
  tmp <- teton.all[teton.all$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  teton.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start)
}

teton.mass.df <- do.call(rbind, teton.delta)
rownames(teton.mass.df) <- NULL
colnames(teton.mass.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass")
teton.mass.df$lat <- rep(latlong.df[latlong.df$site=="Teton",]$lat, nrow(teton.mass.df))
teton.mass.df$long <- rep(latlong.df[latlong.df$site=="Teton",]$long, nrow(teton.mass.df))

### waterfall!

# load data 
waterfall.all <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered_all.csv")[-1]

# subset temp data, merge with df
waterfall.temps <- cbind.data.frame(temp.df$year, temp.df$Waterfall)
colnames(waterfall.temps) <- c("year", "MAT")
waterfall.temps$MAT <- as.numeric(as.character(waterfall.temps$MAT))
waterfall.all <- merge(waterfall.all, waterfall.temps, by.x="year", by.y="year", all.y=FALSE)

# loop to get mean mass change per species
waterfall.delta <- list()
for(i in unique(waterfall.all$species)){
  tmp <- waterfall.all[waterfall.all$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  waterfall.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start)
}

waterfall.mass.df <- do.call(rbind, waterfall.delta)
rownames(waterfall.mass.df) <- NULL
colnames(waterfall.mass.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass")
waterfall.mass.df$lat <- rep(latlong.df[latlong.df$site=="Waterfall",]$lat, nrow(waterfall.mass.df))
waterfall.mass.df$long <- rep(latlong.df[latlong.df$site=="Waterfall",]$long, nrow(waterfall.mass.df))

# merge all the damn dfs
master.df <- rbind.data.frame(brazil.mass.df, panama.mass.df, guanica.mass.df, palo.mass.df, teton.mass.df, powdermill.mass.df)
species.count <- as.data.frame(table(master.df$species)) # get table of records

# clean ids
master.df$species <- sub(" ", "_", master.df$species) # replace space with underscore

# fix taxonomy
master.df[grep("Dixiphia_pipra",master.df$species),]$species <-"Pipra_pipra" 
master.df[grep("Ceratopipra_erythrocephala",master.df$species),]$species <-"Pipra_erythrocephala"
master.df[grep("Percnostola_leucostigma",master.df$species),]$species <-"Schistocichla_leucostigma"
master.df[grep("Ceratopipra_mentalis",master.df$species),]$species <-"Pipra_mentalis"
master.df[grep("Geothlypis_formosa",master.df$species),]$species <-"Oporornis_formosus"
master.df[grep("Sporophila_funerea",master.df$species),]$species <-"Oryzoborus_funereus"
master.df[grep("Setophaga_discolor",master.df$species),]$species <-"Dendroica_discolor" 
master.df[grep("Setophaga_adelaidae",master.df$species),]$species <-"Dendroica_adelaidae"
master.df[grep("Setophaga_americana",master.df$species),]$species <-"Parula_americana"
master.df[grep("Turdus_plumbeus",master.df$species),]$species <-"Turdus_plumbeus"
master.df[grep("Zonotrichia_l. nuttalli",master.df$species),]$species <-"Zonotrichia_leucophrys" 
master.df[grep("Haemorhous_mexicanus",master.df$species),]$species <-"Carpodacus_mexicanus"
master.df[grep("Setophaga_townsendi",master.df$species),]$species <-"Dendroica_townsendi"
master.df[grep("Junco_h. oregonus",master.df$species),]$species <-"Junco_hyemalis"
master.df[grep("Setophaga_petechia",master.df$species),]$species <-"Junco_hyemalis"
master.df[grep("Spinus_pinus",master.df$species),]$species <-"Dendroica_petechia"
master.df[grep("Ixoreus_naevius",master.df$species),]$species <-"Zoothera_naevia"
master.df[grep("Cardellina_pusilla",master.df$species),]$species <-"Wilsonia_pusilla"
master.df[grep("Spinus_tristis",master.df$species),]$species <-"Oreothlypis_celata"
master.df[grep("Oreothlypis_celata",master.df$species),]$species <-"Vermivora_celata"
master.df[grep("Colaptes_a. cafer",master.df$species),]$species <-"Colaptes_auratus"
master.df[grep("Haemorhous_purpureus",master.df$species),]$species <-"Carpodacus_purpureus"
master.df[grep("Poecile_rufescens",master.df$species),]$species <-"Parus_rufescens"
master.df[grep("Geothlypis_tolmiei",master.df$species),]$species <-"Oporornis_tolmiei"
master.df[grep("Setophaga_coronata coronata",master.df$species),]$species <-"Dendroica_coronata"
master.df[grep("Troglodytes_pacificus",master.df$species),]$species <-"Troglodytes_troglodytes"
master.df[grep("Setophaga_coronata",master.df$species),]$species <-"Dendroica_coronata"
master.df[grep("Setophaga_magnolia",master.df$species),]$species <-"Dendroica_magnolia"
master.df[grep("Colaptes_a. auratus",master.df$species),]$species <-"Colaptes_auratus"
master.df[grep("Setophaga_pensylvanica",master.df$species),]$species <-"Dendroica_pensylvanica"
master.df[grep("Parkesia_motacilla",master.df$species),]$species <-"Seiurus_motacilla"
master.df[grep("Sphyrapicus_varius",master.df$species),]$species <-"Sphyrapicus_varius"
master.df[grep("Cardellina_canadensis",master.df$species),]$species <-"Wilsonia_canadensis"
master.df[grep("Sitta_carolinensis",master.df$species),]$species <-"Sitta_carolinensis"
master.df[grep("Setophaga_striata",master.df$species),]$species <-"Dendroica_striata"
master.df[grep("Setophaga_tigrina",master.df$species),]$species <-"Dendroica_tigrina"
master.df[grep("Geothlypis_philadelphia",master.df$species),]$species <-"Oporornis_philadelphia"
master.df[grep("Oreothlypis_peregrina",master.df$species),]$species <-"Vermivora_peregrina"
master.df[grep("Poecile_atricapillus",master.df$species),]$species <-"Parus_atricapillus"
master.df[grep("Oreothlypis_ruficapilla",master.df$species),]$species <-"Vermivora_ruficapilla"
master.df[grep("Setophaga_virens",master.df$species),]$species <-"Dendroica_virens"
master.df[grep("Setophaga_citrina",master.df$species),]$species <-"Wilsonia_citrina"
master.df[grep("Setophaga_palmarum palmarum",master.df$species),]$species <-"Dendroica_palmarum"
master.df[grep("Setophaga_caerulescens",master.df$species),]$species <-"Dendroica_caerulescens"
master.df[grep("Setophaga_fusca",master.df$species),]$species <-"Dendroica_fusca"
master.df[grep("Setophaga_castanea",master.df$species),]$species <-"Dendroica_castanea"
master.df[grep("Vermivora_cyanoptera",master.df$species),]$species <-"Vermivora_pinus"
master.df[grep("Picoides_villosus",master.df$species),]$species <-"Picoides_villosus"
master.df[grep("Piranga_olivacea",master.df$species),]$species <-"Piranga_olivacea"
master.df[grep("Pheucticus_ludovicianus",master.df$species),]$species <-"Pheucticus_ludovicianus"
master.df[grep("Toxostoma_rufum",master.df$species),]$species <-"Toxostoma_rufum"
master.df[grep("Sturnus_vulgaris",master.df$species),]$species <-"Sturnus_vulgaris"
master.df[grep("Sialia_sialis",master.df$species),]$species <-"Sialia_sialis"
master.df[grep("Icterus_galbula",master.df$species),]$species <-"Icterus_galbula"
master.df[grep("Gymnopithys_bicolor",master.df$species),]$species <-"Gymnopithys_leucaspis"
master.df[grep("Schiffornis_stenorhyncha",master.df$species),]$species <-"Schiffornis_turdina"
master.df[grep("Parkesia_noveboracensis",master.df$species),]$species <-"Seiurus_noveboracensis"
master.df[grep("Microbates_cenereiventris",master.df$species),]$species <-"Microbates_cinereiventris"
master.df[grep("Eucomitis_penicillata",master.df$species),]$species <- "Eucometis_penicillata"
master.df[grep("Cercomacroides_tyrannina",master.df$species),]$species <-"Cercomacra_tyrannina"
master.df[grep("Vireo_latimeri",master.df$species),]$species <-"Vireo_latimeri"
master.df[grep("Geotrygon_chrysia",master.df$species),]$species <-"Geotrygon_chrysia" 
master.df[grep("Zonotrichia_l. pugetensis",master.df$species),]$species <-"Zonotrichia_leucophrys"
master.df[grep("Melozone_crissalis",master.df$species),]$species <-"Pipilo_crissalis"
master.df[grep("Setophaga_occidentalis",master.df$species),]$species <-"Dendroica_occidentalis"
master.df[grep("Setophaga_nigrescens",master.df$species),]$species <-"Dendroica_nigrescens"
master.df[grep("Catharus_guttatus",master.df$species),]$species <-"Catharus_guttatus"
master.df[grep("Vermivora_chrysoptera",master.df$species),]$species <-"Vermivora_chrysoptera"
master.df[grep("Troglodytes_hiemalis",master.df$species),]$species <-"Troglodytes_troglodytes"
master.df[grep("Hirundo_rustica",master.df$species),]$species <-"Hirundo_rustica"
master.df[grep("Stelgidopteryx_serripennis",master.df$species),]$species <-"Stelgidopteryx_serripennis"
master.df[grep("Spizelloides_arborea",master.df$species),]$species <-"Spizella_arborea"
master.df[grep("Empidonax_alnorum/traillii",master.df$species),]$species <-"Empidonax_alnorum"
master.df[grep("Tringa_solitaria",master.df$species),]$species <-"Tringa_solitaria"
master.df[grep("Leptotila_cassinii",master.df$species),]$species <-"Leptotila_cassini"
master.df[grep("Certhiasomus_stictolaemus",master.df$species),]$species <-"Deconychura_stictolaema"
master.df[grep("Clibarnornis_rubiginosus",master.df$species),]$species <-"Automolus_rubiginosus"
master.df[grep("Philydor_erythrocercus",master.df$species),]$species <-"Philydor_erythrocercum"
master.df[grep("Tunchiornis_ochraceiceps",master.df$species),]$species <-"Hylophilus_ochraceiceps"
master.df[grep("Myiothlypis_rivularis",master.df$species),]$species <-"Phaeothlypis_rivularis"

# function for searching for taxonomy errors
#obj <- grep("Phaeothlypis", supertree.species)
#supertree.species[obj]

# manipulations
dups <- species.count[species.count$Freq>1,] # identify duplicates
master.df <- master.df[!duplicated(master.df$species),]

# add phylo column
master.df$phylo <- master.df$species

#write to file
write.csv(master.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis_df.csv")

# keep only tips in dataset
master.tree <- keep.tip(supertree, master.df$species)
write.tree(master.tree, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis_phy.tree")

# get phylogenetic distance matrix
master.dist <- ape::vcv.phylo(master.tree) 

# test plot
master.slope <- cbind.data.frame(master.df$species, master.df$slope_mass)
p1 <- ggtree(master.tree, layout='circular') %<+% master.slope 
p1$data$`master.df$slope_mass`[is.na(p1$data$`master.df$slope_mass`)] <- 0
#p1$data$`master.df$slope_mass` <- (p1$data$`master.df$slope_mass`+ abs(min(p1$data$`master.df$slope_mass`)) + 0.0001)
p2 <- ggtree(master.tree, layout='circular') %<+% master.slope + 
  aes(color=I(log(p1$data$`master.df$slope_mass`)),guides="test") +
  scale_colour_gradient2()

# simple linear regression
mod.test <- lm(slope_mass ~ slope_temp + starting_mass + lat, master.df)
summary(mod.test)

### brazil!

# load data 
brazil.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_sex.csv")[-1]

# subset temp data, merge with df
brazil.temps <- cbind.data.frame(temp.df$year, temp.df$Brazil)
colnames(brazil.temps) <- c("year", "MAT")
brazil.temps$MAT <- as.numeric(as.character(brazil.temps$MAT))
brazil.sex <- merge(brazil.sex, brazil.temps, by.x="year", by.y="year", all.y=FALSE)

### sex-specific data 

### brazil!

# load data 
brazil.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_sex.csv")[-1]

# subset temp data, merge with df
brazil.temps <- cbind.data.frame(temp.df$year, temp.df$Brazil)
colnames(brazil.temps) <- c("year", "MAT")
brazil.temps$MAT <- as.numeric(as.character(brazil.temps$MAT))
brazil.sex <- merge(brazil.sex, brazil.temps, by.x="year", by.y="year", all.y=FALSE)

# loop to get mean mass change per species, males: 
brazil.delta <- list()
for(i in unique(brazil.sex$species)){
  tmp <- brazil.sex[brazil.sex$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  sex <- tmp$sex
  brazil.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,sex)
}

brazil.mass.sex.df <- do.call(rbind, brazil.delta)
rownames(brazil.mass.sex.df) <- NULL
colnames(brazil.mass.sex.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass", "sex")
brazil.mass.sex.df$lat <- rep(latlong.df[latlong.df$site=="Brazil",]$lat, nrow(brazil.mass.sex.df))
brazil.mass.sex.df$long <- rep(latlong.df[latlong.df$site=="Brazil",]$long, nrow(brazil.mass.sex.df))

### panama!

# load data 
panama.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered_sex.csv")[-1]

# subset temp data, merge with df
panama.temps <- cbind.data.frame(temp.df$year, temp.df$Panama)
colnames(panama.temps) <- c("year", "MAT")
panama.temps$MAT <- as.numeric(as.character(panama.temps$MAT))
panama.sex <- merge(panama.sex, panama.temps, by.x="year", by.y="year", all.y=FALSE)


# loop to get mean mass change per species, males: 
panama.delta <- list()
for(i in unique(panama.sex$species)){
  tmp <- panama.sex[panama.sex$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  sex <- tmp$sex
  panama.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,sex)
}

panama.mass.sex.df <- do.call(rbind, panama.delta)
rownames(panama.mass.sex.df) <- NULL
colnames(panama.mass.sex.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass", "sex")
panama.mass.sex.df$lat <- rep(latlong.df[latlong.df$site=="Panama",]$lat, nrow(panama.mass.sex.df))
panama.mass.sex.df$long <- rep(latlong.df[latlong.df$site=="Panama",]$long, nrow(panama.mass.sex.df))

### guanica!

# load data 
guanica.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered_sex.csv",fileEncoding="latin1")[-1]

# subset temp data, merge with df
guanica.temps <- cbind.data.frame(temp.df$year, temp.df$Guanica)
colnames(guanica.temps) <- c("year", "MAT")
guanica.temps$MAT <- as.numeric(as.character(guanica.temps$MAT))
guanica.sex <- merge(guanica.sex, guanica.temps, by.x="year", by.y="year", all.y=FALSE)

# loop to get mean mass change per species, males: 
guanica.delta <- list()
for(i in unique(guanica.sex$species)){
  tmp <- guanica.sex[guanica.sex$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  sex <- tmp$sex
  guanica.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,sex)
}

guanica.mass.sex.df <- do.call(rbind, guanica.delta)
rownames(guanica.mass.sex.df) <- NULL
colnames(guanica.mass.sex.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass", "sex")
guanica.mass.sex.df$lat <- rep(latlong.df[latlong.df$site=="Guanica",]$lat, nrow(guanica.mass.sex.df))
guanica.mass.sex.df$long <- rep(latlong.df[latlong.df$site=="Guanica",]$long, nrow(guanica.mass.sex.df))

### palo!

# load data 
palo.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered_sex.csv")[-1]

# subset temp data, merge with df
palo.temps <- cbind.data.frame(temp.df$year, temp.df$Palo)
colnames(palo.temps) <- c("year", "MAT")
palo.temps$MAT <- as.numeric(as.character(palo.temps$MAT))
palo.sex <- merge(palo.sex, palo.temps, by.x="year", by.y="year", all.y=FALSE)

# loop to get mean mass change per species
palo.delta <- list()
for(i in unique(palo.sex$species)){
  tmp <- palo.sex[palo.sex$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared   
  sex <- tmp$sex
  palo.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,sex)
}

palo.mass.sex.df <- do.call(rbind, palo.delta)
rownames(palo.mass.sex.df) <- NULL
colnames(palo.mass.sex.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass", "sex")
palo.mass.sex.df$lat <- rep(latlong.df[latlong.df$site=="Palomarin",]$lat, nrow(palo.mass.sex.df))
palo.mass.sex.df$long <- rep(latlong.df[latlong.df$site=="Palomarin",]$long, nrow(palo.mass.sex.df))

### powdermill!

# load data 
powdermill.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered_sex.csv")[-1]

# subset temp data, merge with df
powdermill.temps <- cbind.data.frame(temp.df$year, temp.df$Powdermill)
colnames(powdermill.temps) <- c("year", "MAT")
powdermill.temps$MAT <- as.numeric(as.character(powdermill.temps$MAT))
powdermill.sex <- merge(powdermill.sex, powdermill.temps, by.x="year", by.y="year", all.y=FALSE)

# loop to get mean mass change per species
powdermill.delta <- list()
for(i in unique(powdermill.sex$species)){
  tmp <- powdermill.sex[powdermill.sex$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared   
  sex <- tmp$sex
  powdermill.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,sex)
}

powdermill.mass.sex.df <- do.call(rbind, powdermill.delta)
rownames(powdermill.mass.sex.df) <- NULL
colnames(powdermill.mass.sex.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass", "sex")
powdermill.mass.sex.df$lat <- rep(latlong.df[latlong.df$site=="Powdermill",]$lat, nrow(powdermill.mass.sex.df))
powdermill.mass.sex.df$long <- rep(latlong.df[latlong.df$site=="Powdermill",]$long, nrow(powdermill.mass.sex.df))

### TSS!

# load data 
teton.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered_sex.csv")[-1]

# subset temp data, merge with df
teton.temps <- cbind.data.frame(temp.df$year, temp.df$Teton)
colnames(teton.temps) <- c("year", "MAT")
teton.temps$MAT <- as.numeric(as.character(teton.temps$MAT))
teton.sex <- merge(teton.sex, teton.temps, by.x="year", by.y="year", all.y=FALSE)

# loop to get mean mass change per species
teton.delta <- list()
for(i in unique(teton.sex$species)){
  tmp <- teton.sex[teton.sex$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared   
  sex <- tmp$sex
  teton.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,sex)
}

teton.mass.sex.df <- do.call(rbind, teton.delta)
rownames(teton.mass.sex.df) <- NULL
colnames(teton.mass.sex.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass", "sex")
teton.mass.sex.df$lat <- rep(latlong.df[latlong.df$site=="Teton",]$lat, nrow(teton.mass.sex.df))
teton.mass.sex.df$long <- rep(latlong.df[latlong.df$site=="Teton",]$long, nrow(teton.mass.sex.df))

### waterfall!

# load data 
waterfall.sex <- read.csv(file = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered_sex.csv")[-1]

# subset temp data, merge with df
waterfall.temps <- cbind.data.frame(temp.df$year, temp.df$Waterfall)
colnames(waterfall.temps) <- c("year", "MAT")
waterfall.temps$MAT <- as.numeric(as.character(waterfall.temps$MAT))
waterfall.sex <- merge(waterfall.sex, waterfall.temps, by.x="year", by.y="year", all.y=FALSE)

# loop to get mean mass change per species
waterfall.delta <- list()
for(i in unique(waterfall.sex$species)){
  tmp <- waterfall.sex[waterfall.sex$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared   
  sex <- tmp$sex
  waterfall.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,sex)
}

waterfall.mass.sex.df <- do.call(rbind, waterfall.delta)
rownames(waterfall.mass.sex.df) <- NULL
colnames(waterfall.mass.sex.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass", "sex")
waterfall.mass.sex.df$lat <- rep(latlong.df[latlong.df$site=="Waterfall",]$lat, nrow(waterfall.mass.sex.df))
waterfall.mass.sex.df$long <- rep(latlong.df[latlong.df$site=="Waterfall",]$long, nrow(waterfall.mass.sex.df))

# merge all the damn dfs
master.sex.df <- rbind.data.frame(brazil.mass.sex.df, panama.mass.sex.df, guanica.mass.sex.df, palo.mass.sex.df, teton.mass.sex.df, powdermill.mass.sex.df)
species.count <- as.data.frame(table(master.sex.df$species)) # get table of records

# clean ids
master.sex.df$species <- sub(" ", "_", master.sex.df$species) # replace space with underscore

# fix taxonomy
master.sex.df[grep("Dixiphia_pipra",master.sex.df$species),]$species <-"Pipra_pipra" 
master.sex.df[grep("Ceratopipra_erythrocephala",master.sex.df$species),]$species <-"Pipra_erythrocephala"
master.sex.df[grep("Percnostola_leucostigma",master.sex.df$species),]$species <-"Schistocichla_leucostigma"
master.sex.df[grep("Ceratopipra_mentalis",master.sex.df$species),]$species <-"Pipra_mentalis"
master.sex.df[grep("Geothlypis_formosa",master.sex.df$species),]$species <-"Oporornis_formosus"
master.sex.df[grep("Sporophila_funerea",master.sex.df$species),]$species <-"Oryzoborus_funereus"
master.sex.df[grep("Setophaga_discolor",master.sex.df$species),]$species <-"Dendroica_discolor" 
master.sex.df[grep("Setophaga_adelaidae",master.sex.df$species),]$species <-"Dendroica_adelaidae"
master.sex.df[grep("Setophaga_americana",master.sex.df$species),]$species <-"Parula_americana"
master.sex.df[grep("Turdus_plumbeus",master.sex.df$species),]$species <-"Turdus_plumbeus"
master.sex.df[grep("Zonotrichia_l. nuttalli",master.sex.df$species),]$species <-"Zonotrichia_leucophrys" 
master.sex.df[grep("Haemorhous_mexicanus",master.sex.df$species),]$species <-"Carpodacus_mexicanus"
master.sex.df[grep("Setophaga_townsendi",master.sex.df$species),]$species <-"Dendroica_townsendi"
master.sex.df[grep("Junco_h. oregonus",master.sex.df$species),]$species <-"Junco_hyemalis"
master.sex.df[grep("Setophaga_petechia",master.sex.df$species),]$species <-"Junco_hyemalis"
master.sex.df[grep("Spinus_pinus",master.sex.df$species),]$species <-"Dendroica_petechia"
master.sex.df[grep("Ixoreus_naevius",master.sex.df$species),]$species <-"Zoothera_naevia"
master.sex.df[grep("Cardellina_pusilla",master.sex.df$species),]$species <-"Wilsonia_pusilla"
master.sex.df[grep("Spinus_tristis",master.sex.df$species),]$species <-"Oreothlypis_celata"
master.sex.df[grep("Oreothlypis_celata",master.sex.df$species),]$species <-"Vermivora_celata"
master.sex.df[grep("Colaptes_a. cafer",master.sex.df$species),]$species <-"Colaptes_auratus"
master.sex.df[grep("Haemorhous_purpureus",master.sex.df$species),]$species <-"Carpodacus_purpureus"
master.sex.df[grep("Poecile_rufescens",master.sex.df$species),]$species <-"Parus_rufescens"
master.sex.df[grep("Geothlypis_tolmiei",master.sex.df$species),]$species <-"Oporornis_tolmiei"
master.sex.df[grep("Setophaga_coronata coronata",master.sex.df$species),]$species <-"Dendroica_coronata"
master.sex.df[grep("Troglodytes_pacificus",master.sex.df$species),]$species <-"Troglodytes_troglodytes"
master.sex.df[grep("Setophaga_coronata",master.sex.df$species),]$species <-"Dendroica_coronata"
master.sex.df[grep("Setophaga_magnolia",master.sex.df$species),]$species <-"Dendroica_magnolia"
master.sex.df[grep("Colaptes_a. auratus",master.sex.df$species),]$species <-"Colaptes_auratus"
master.sex.df[grep("Setophaga_pensylvanica",master.sex.df$species),]$species <-"Dendroica_pensylvanica"
master.sex.df[grep("Parkesia_motacilla",master.sex.df$species),]$species <-"Seiurus_motacilla"
master.sex.df[grep("Sphyrapicus_varius",master.sex.df$species),]$species <-"Sphyrapicus_varius"
master.sex.df[grep("Cardellina_canadensis",master.sex.df$species),]$species <-"Wilsonia_canadensis"
master.sex.df[grep("Sitta_carolinensis",master.sex.df$species),]$species <-"Sitta_carolinensis"
master.sex.df[grep("Setophaga_striata",master.sex.df$species),]$species <-"Dendroica_striata"
master.sex.df[grep("Setophaga_tigrina",master.sex.df$species),]$species <-"Dendroica_tigrina"
master.sex.df[grep("Geothlypis_philadelphia",master.sex.df$species),]$species <-"Oporornis_philadelphia"
master.sex.df[grep("Oreothlypis_peregrina",master.sex.df$species),]$species <-"Vermivora_peregrina"
master.sex.df[grep("Poecile_atricapillus",master.sex.df$species),]$species <-"Parus_atricapillus"
master.sex.df[grep("Oreothlypis_ruficapilla",master.sex.df$species),]$species <-"Vermivora_ruficapilla"
master.sex.df[grep("Setophaga_virens",master.sex.df$species),]$species <-"Dendroica_virens"
master.sex.df[grep("Setophaga_citrina",master.sex.df$species),]$species <-"Wilsonia_citrina"
master.sex.df[grep("Setophaga_palmarum palmarum",master.sex.df$species),]$species <-"Dendroica_palmarum"
master.sex.df[grep("Setophaga_caerulescens",master.sex.df$species),]$species <-"Dendroica_caerulescens"
master.sex.df[grep("Setophaga_fusca",master.sex.df$species),]$species <-"Dendroica_fusca"
master.sex.df[grep("Setophaga_castanea",master.sex.df$species),]$species <-"Dendroica_castanea"
master.sex.df[grep("Vermivora_cyanoptera",master.sex.df$species),]$species <-"Vermivora_pinus"
master.sex.df[grep("Picoides_villosus",master.sex.df$species),]$species <-"Picoides_villosus"
master.sex.df[grep("Piranga_olivacea",master.sex.df$species),]$species <-"Piranga_olivacea"
master.sex.df[grep("Pheucticus_ludovicianus",master.sex.df$species),]$species <-"Pheucticus_ludovicianus"
master.sex.df[grep("Toxostoma_rufum",master.sex.df$species),]$species <-"Toxostoma_rufum"
master.sex.df[grep("Sturnus_vulgaris",master.sex.df$species),]$species <-"Sturnus_vulgaris"
master.sex.df[grep("Sialia_sialis",master.sex.df$species),]$species <-"Sialia_sialis"
master.sex.df[grep("Icterus_galbula",master.sex.df$species),]$species <-"Icterus_galbula"
master.sex.df[grep("Hirundo_rustica",master.sex.df$species),]$species <-"Hirundo_rustica"

# function for searching for taxonomy errors
#obj <- grep("Phaeothlypis", supertree.species)
#supertree.species[obj]

# manipulations
dups <- species.count[species.count$Freq>1,] # identify duplicates
master.sex.df <- master.sex.df[!duplicated(master.sex.df$species),]

# add phylo column
master.sex.df$phylo <- master.sex.df$species

#write to file
write.csv(master.sex.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/sex_analysis_df.csv")

# keep only tips in dataset
master.sex.tree <- keep.tip(supertree, master.sex.df$species)
write.tree(master.sex.tree, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/sex_analysis_phy.tree")

# get phylogenetic distance matrix
master.dist <- ape::vcv.phylo(master.sex.tree) 

# test plot
master.slope <- cbind.data.frame(master.sex.df$species, master.sex.df$slope_mass)
p1 <- ggtree(master.tree, layout='circular') %<+% master.slope 
p1$data$`master.sex.df$slope_mass`[is.na(p1$data$`master.sex.df$slope_mass`)] <- 0
#p1$data$`master.sex.df$slope_mass` <- (p1$data$`master.sex.df$slope_mass`+ abs(min(p1$data$`master.sex.df$slope_mass`)) + 0.0001)
p2 <- ggtree(master.tree, layout='circular') %<+% master.slope + 
  aes(color=I(log(p1$data$`master.sex.df$slope_mass`)),guides="test") +
  scale_colour_gradient2()

# simple linear regression
mod.test <- lm(slope_mass ~ slope_temp + starting_mass + lat, master.sex.df)
summary(mod.test)

# subset by sex
master.m.df <- master.sex.df[master.sex.df$sex=="M",]
master.f.df <- master.sex.df[master.sex.df$sex=="F",]
write.csv(master.m.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/m_analysis_df.csv")
write.csv(master.f.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/f_analysis_df.csv")
m.tree <- keep.tip(supertree, master.m.df$species)
write.tree(master.sex.tree, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/m_analysis_phy.tree")
f.tree <- keep.tip(supertree, master.f.df$species)
write.tree(master.sex.tree, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/f_analysis_phy.tree")

### write sex-blind data frames for wing length correlation

### brazil! 

brazil.delta.wl <- list()
brazil.all.wl <- brazil.all[!is.na(brazil.all$wing_length),]
for(i in unique(brazil.all.wl$species)){
  tmp <- brazil.all.wl[brazil.all.wl$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  mod.wl <- lm(wing_length ~ year, tmp) # simple regression for wing length change
  mod.wl.sum <- summary(mod.wl)
  slope.wl <- mod.wl.sum$coefficients[2,1] # get mass slope
  r2.wl <- mod.wl.sum$r.squared # get mass r-squared
  brazil.delta.wl[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,slope.wl,r2.wl)
}

brazil.wl.df <- do.call(rbind, brazil.delta.wl)
rownames(brazil.wl.df) <- NULL
colnames(brazil.wl.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass","slope_wl","variance_wl")
brazil.wl.df$lat <- rep(latlong.df[latlong.df$site=="Brazil",]$lat, nrow(brazil.wl.df))
brazil.wl.df$long <- rep(latlong.df[latlong.df$site=="Brazil",]$long, nrow(brazil.wl.df))

### Panama!

panama.delta.wl <- list()
panama.all.wl <- panama.all[!is.na(panama.all$wing_length),]
for(i in unique(panama.all.wl$species)){
  tmp <- panama.all.wl[panama.all.wl$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  mod.wl <- lm(wing_length ~ year, tmp) # simple regression for wing length change
  mod.wl.sum <- summary(mod.wl)
  slope.wl <- mod.wl.sum$coefficients[2,1] # get mass slope
  r2.wl <- mod.wl.sum$r.squared # get mass r-squared
  panama.delta.wl[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,slope.wl,r2.wl)
}

panama.wl.df <- do.call(rbind, panama.delta.wl)
rownames(panama.wl.df) <- NULL
colnames(panama.wl.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass","slope_wl","variance_wl")
panama.wl.df$lat <- rep(latlong.df[latlong.df$site=="Panama",]$lat, nrow(panama.wl.df))
panama.wl.df$long <- rep(latlong.df[latlong.df$site=="Panama",]$long, nrow(panama.wl.df)) 

### Guanica!

guanica.delta.wl <- list()
guanica.all.wl <- guanica.all[!is.na(guanica.all$wing_length),]
for(i in unique(guanica.all.wl$species)){
  tmp <- guanica.all.wl[guanica.all.wl$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  mod.wl <- lm(wing_length ~ year, tmp) # simple regression for wing length change
  mod.wl.sum <- summary(mod.wl)
  slope.wl <- mod.wl.sum$coefficients[2,1] # get mass slope
  r2.wl <- mod.wl.sum$r.squared # get mass r-squared
  guanica.delta.wl[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,slope.wl,r2.wl)
}

guanica.wl.df <- do.call(rbind, guanica.delta.wl)
rownames(guanica.wl.df) <- NULL
colnames(guanica.wl.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass","slope_wl","variance_wl")
guanica.wl.df$lat <- rep(latlong.df[latlong.df$site=="Guanica",]$lat, nrow(guanica.wl.df))
guanica.wl.df$long <- rep(latlong.df[latlong.df$site=="Guanica",]$long, nrow(guanica.wl.df))

### Powdermill!

powdermill.delta.wl <- list()
powdermill.all.wl <- powdermill.all[!is.na(powdermill.all$wing_length),]
for(i in unique(powdermill.all.wl$species)){
  tmp <- powdermill.all.wl[powdermill.all.wl$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  mod.wl <- lm(wing_length ~ year, tmp) # simple regression for wing length change
  mod.wl.sum <- summary(mod.wl)
  slope.wl <- mod.wl.sum$coefficients[2,1] # get mass slope
  r2.wl <- mod.wl.sum$r.squared # get mass r-squared
  powdermill.delta.wl[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,slope.wl,r2.wl)
}

powdermill.wl.df <- do.call(rbind, powdermill.delta.wl)
rownames(powdermill.wl.df) <- NULL
colnames(powdermill.wl.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass","slope_wl","variance_wl")
powdermill.wl.df$lat <- rep(latlong.df[latlong.df$site=="Powdermill",]$lat, nrow(powdermill.wl.df))
powdermill.wl.df$long <- rep(latlong.df[latlong.df$site=="Powdermill",]$long, nrow(powdermill.wl.df))

### Palomarin!

palo.delta.wl <- list()
palo.all.wl <- palo.all[!is.na(palo.all$wing_length),]
for(i in unique(palo.all.wl$species)){
  tmp <- palo.all.wl[palo.all.wl$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  mod.wl <- lm(wing_length ~ year, tmp) # simple regression for wing length change
  mod.wl.sum <- summary(mod.wl)
  slope.wl <- mod.wl.sum$coefficients[2,1] # get mass slope
  r2.wl <- mod.wl.sum$r.squared # get mass r-squared
  palo.delta.wl[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,slope.wl,r2.wl)
}

palo.wl.df <- do.call(rbind, palo.delta.wl)
rownames(palo.wl.df) <- NULL
colnames(palo.wl.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass","slope_wl","variance_wl")
palo.wl.df$lat <- rep(latlong.df[latlong.df$site=="Palomarin",]$lat, nrow(palo.wl.df))
palo.wl.df$long <- rep(latlong.df[latlong.df$site=="Palomarin",]$long, nrow(palo.wl.df))

### tss!

tss.delta.wl <- list()
tss.all.wl <- tss.all[!is.na(tss.all$wing_length),]
for(i in unique(tss.all.wl$species)){
  tmp <- tss.all.wl[tss.all.wl$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  mod.wl <- lm(wing_length ~ year, tmp) # simple regression for wing length change
  mod.wl.sum <- summary(mod.wl)
  slope.wl <- mod.wl.sum$coefficients[2,1] # get mass slope
  r2.wl <- mod.wl.sum$r.squared # get mass r-squared
  tss.delta.wl[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,slope.wl,r2.wl)
}

tss.wl.df <- do.call(rbind, tss.delta.wl)
rownames(tss.wl.df) <- NULL
colnames(tss.wl.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass","slope_wl","variance_wl")
tss.wl.df$lat <- rep(latlong.df[latlong.df$site=="Teton",]$lat, nrow(tss.wl.df))
tss.wl.df$long <- rep(latlong.df[latlong.df$site=="Teton",]$long, nrow(tss.wl.df))

### wate!

wate.delta.wl <- list()
wate.all.wl <- waterfall.all[!is.na(waterfall.all$wing_length),]
for(i in unique(wate.all.wl$species)){
  tmp <- wate.all.wl[wate.all.wl$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  mod.wl <- lm(wing_length ~ year, tmp) # simple regression for wing length change
  mod.wl.sum <- summary(mod.wl)
  slope.wl <- mod.wl.sum$coefficients[2,1] # get mass slope
  r2.wl <- mod.wl.sum$r.squared # get mass r-squared
  wate.delta.wl[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,slope.wl,r2.wl)
}

wate.wl.df <- do.call(rbind, wate.delta.wl)
rownames(wate.wl.df) <- NULL
colnames(wate.wl.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass","slope_wl","variance_wl")
wate.wl.df$lat <- rep(latlong.df[latlong.df$site=="Waterfall",]$lat, nrow(wate.wl.df))
wate.wl.df$long <- rep(latlong.df[latlong.df$site=="Waterfall",]$long, nrow(wate.wl.df))

# merge, drop duplicates, write to file
master.wl.df <- rbind.data.frame(brazil.wl.df, panama.wl.df, guanica.wl.df, palo.wl.df, tss.wl.df, wate.wl.df, powdermill.wl.df)
master.wl.df$species <- sub(" ", "_", master.wl.df$species) # replace space with underscore
dups <- species.count[species.count$Freq>1,] # identify duplicates
master.wl.df <- master.wl.df[!duplicated(master.wl.df$species),]
write.csv(master.wl.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/correlations_wl.csv")

### write sex-blind data frames for tarsus correlations 

### Panama!

panama.delta.tarsus <- list()
panama.all.tarsus <- panama.all[!is.na(panama.all$tarsus),]
for(i in unique(panama.all.tarsus$species)){
  tmp <- panama.all.tarsus[panama.all.tarsus$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  mod.tarsus <- lm(tarsus ~ year, tmp) # simple regression for wing length change
  mod.tarsus.sum <- summary(mod.tarsus)
  slope.tarsus <- mod.tarsus.sum$coefficients[2,1] # get mass slope
  r2.tarsus <- mod.tarsus.sum$r.squared # get mass r-squared
  panama.delta.tarsus[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,slope.tarsus,r2.tarsus)
}

panama.tarsus.df <- do.call(rbind, panama.delta.tarsus)
rownames(panama.tarsus.df) <- NULL
colnames(panama.tarsus.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass","slope_tarsus","variance_wl")
panama.tarsus.df$lat <- rep(latlong.df[latlong.df$site=="Panama",]$lat, nrow(panama.tarsus.df))
panama.tarsus.df$long <- rep(latlong.df[latlong.df$site=="Panama",]$long, nrow(panama.tarsus.df)) 


### Guanica!

guanica.delta.tarsus <- list()
guanica.all.tarsus <- guanica.all[!is.na(guanica.all$tarsus),]
for(i in unique(guanica.all.tarsus$species)){
  tmp <- guanica.all.tarsus[guanica.all.tarsus$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  mod.tarsus <- lm(tarsus ~ year, tmp) # simple regression for wing length change
  mod.tarsus.sum <- summary(mod.tarsus)
  slope.tarsus <- mod.tarsus.sum$coefficients[2,1] # get mass slope
  r2.tarsus <- mod.tarsus.sum$r.squared # get mass r-squared
  guanica.delta.tarsus[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,slope.tarsus,r2.tarsus)
}

guanica.tarsus.df <- do.call(rbind, guanica.delta.tarsus)
rownames(guanica.tarsus.df) <- NULL
colnames(guanica.tarsus.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass","slope_tarsus","variance_wl")
guanica.tarsus.df$lat <- rep(latlong.df[latlong.df$site=="Guanica",]$lat, nrow(guanica.tarsus.df))
guanica.tarsus.df$long <- rep(latlong.df[latlong.df$site=="Guanica",]$long, nrow(guanica.tarsus.df))

### Palomarin!

palo.delta.tarsus <- list()
palo.all.tarsus <- palo.all[!is.na(palo.all$tarsus),]
for(i in unique(palo.all.tarsus$species)){
  tmp <- palo.all.tarsus[palo.all.tarsus$species==i,]
  n <- nrow(tmp) # get sample size
  n.years <- length(unique(tmp$year)) # get number of years
  min.year <- min(tmp$year)
  mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
  mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
  mod.mass.sum <- summary(mod.mass)
  slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
  r2.mass <- mod.mass.sum$r.squared # get mass r-squared
  mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
  mod.temp.sum <- summary(mod.temp)
  slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
  r2.temp <- mod.mass.sum$r.squared # get temp r-squared
  mod.tarsus <- lm(tarsus ~ year, tmp) # simple regression for wing length change
  mod.tarsus.sum <- summary(mod.tarsus)
  slope.tarsus <- mod.tarsus.sum$coefficients[2,1] # get mass slope
  r2.tarsus <- mod.tarsus.sum$r.squared # get mass r-squared
  palo.delta.tarsus[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,slope.temp,r2.temp,mass.start,slope.tarsus,r2.tarsus)
}

palo.tarsus.df <- do.call(rbind, palo.delta.tarsus)
rownames(palo.tarsus.df) <- NULL
colnames(palo.tarsus.df) <- c("species","sample_size","no_years","slope_mass","variance_mass", "slope_temp", "variance_temp", "starting_mass","slope_tarsus","variance_wl")
palo.tarsus.df$lat <- rep(latlong.df[latlong.df$site=="Palomarin",]$lat, nrow(palo.tarsus.df))
palo.tarsus.df$long <- rep(latlong.df[latlong.df$site=="Palomarin",]$long, nrow(palo.tarsus.df))

# merge, drop duplicates, write to file
master.tarsus.df <- rbind.data.frame(panama.tarsus.df, guanica.tarsus.df, palo.tarsus.df)
master.tarsus.df$species <- sub(" ", "_", master.tarsus.df$species) # replace space with underscore
dups <- species.count[species.count$Freq>1,] # identify duplicates
master.tarsus.df <- master.tarsus.df[!duplicated(master.tarsus.df$species),]
write.csv(master.tarsus.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/correlations_tarsus.csv")


