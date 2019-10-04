### manipulate size data for regression and plotting ###

## brazil!

# load brazil data
brazil.full <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_raw.csv") 

# subset to relevant columns
brazil.df <- brazil.full[,c("Updated.Scientific.Name","Month","Year","Band.Number","Sex","Age","Mass")]

# subset to only known sex
brazil.df <- brazil.df[brazil.df$Sex %in% c("F","M"),] 

# subset to adults only
brazil.df <- brazil.df[brazil.df$Age %in% "AD",] 

# subset to only records with mass data
brazil.df <- brazil.df[!is.na(brazil.df$Mass),]

# order by month and year
brazil.df <- brazil.df[order(brazil.df$Year, brazil.df$Month),]

# drop duplicates
brazil.df <- brazil.df[!duplicated(brazil.df$Band.Number),]

# rename columns
colnames(brazil.df) <- c("species","month","year","band_no","sex","age","mass")

# make sure all species are represented by changing to character
brazil.df$species <- as.character(brazil.df$species)

# check out which years are represented
unique(brazil.df$year)

# create binary code for sampling period
brazil.df$period <- ifelse(brazil.df$year < 2000, "old", "new")

# create mini data frames by period for filtering
brazil.old <- brazil.df[brazil.df$period=="old",]
brazil.new <- brazil.df[brazil.df$period=="new",]

# drop old species without five individuals per time period
brazil.old.species <- as.data.frame(table(brazil.old$species))
colnames(brazil.old.species) <- c("species", "frequency")
brazil.sp.old.keep <- brazil.old.species[which(brazil.old.species$frequency > 4),]
brazil.sp.old.keep <- as.character(brazil.sp.old.keep$species)

# drop new species without five individuals per time period
brazil.new.species <- as.data.frame(table(brazil.new$species))
colnames(brazil.new.species) <- c("species", "frequency")
brazil.sp.new.keep <- brazil.new.species[which(brazil.new.species$frequency > 4),]
brazil.sp.new.keep <- as.character(brazil.sp.new.keep$species)

# create mini data frames by sex for filtering
brazil.old.m <- brazil.old[brazil.old$sex=="M",]
brazil.old.f <- brazil.old[brazil.old$sex=="F",]
brazil.new.m <- brazil.new[brazil.new$sex=="M",]
brazil.new.f <- brazil.new[brazil.new$sex=="F",]

# drop species without 3 male individuals per sex per time period
brazil.old.m.species <- as.data.frame(table(brazil.old.m$species))
colnames(brazil.old.m.species) <- c("species", "frequency")
brazil.sp.old.m.keep <- brazil.old.m.species[which(brazil.old.m.species$frequency > 2),]
brazil.sp.old.m.keep <- as.character(brazil.sp.old.m.keep$species)
brazil.new.m.species <- as.data.frame(table(brazil.new.m$species))
colnames(brazil.new.m.species) <- c("species", "frequency")
brazil.sp.new.m.keep <- brazil.new.m.species[which(brazil.new.m.species$frequency > 2),]
brazil.sp.new.m.keep <- as.character(brazil.sp.new.m.keep$species)

# drop species without 3 female individuals per sex per time period
brazil.old.f.species <- as.data.frame(table(brazil.old.f$species))
colnames(brazil.old.f.species) <- c("species", "frequency")
brazil.sp.old.f.keep <- brazil.old.f.species[which(brazil.old.f.species$frequency > 2),]
brazil.sp.old.f.keep <- as.character(brazil.sp.old.f.keep$species)
brazil.new.f.species <- as.data.frame(table(brazil.new.f$species))
colnames(brazil.new.f.species) <- c("species", "frequency")
brazil.sp.new.f.keep <- brazil.new.f.species[which(brazil.new.f.species$frequency > 2),]
brazil.sp.new.f.keep <- as.character(brazil.sp.new.f.keep$species)

# find overlapping set
brazil.sp.sex.keep <- intersect(brazil.sp.new.f.keep, brazil.sp.new.m.keep) 
brazil.sp.time.keep <- intersect(brazil.sp.old.f.keep, brazil.sp.old.m.keep)
brazil.sp.keep <- intersect(brazil.sp.sex.keep, brazil.sp.time.keep)

# subset major data frame
brazil.df <- brazil.df[which(brazil.df$species %in% brazil.sp.keep),]

# drop mass outliers (> 2 sd)
brazil.df <- brazil.df[abs(brazil.df$mass-mean(brazil.df$mass))< 2*sd(brazil.df$mass),]

# count number of species
unique(brazil.df$species) # 20

# write filtered data frame
write.csv(brazil.df, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered.csv")

## panama!

# load panama data
panama.full <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_raw.csv") 

# subset to relevant columns (no band number data)
panama.df <- panama.full[,c("Updated.Scientific.Name","Month","Year","Sex","Age","Mass")]

# subset to only known sex
panama.df <- panama.df[panama.df$Sex %in% c("F","M"),] 

# subset to adults only
panama.df <- panama.df[panama.df$Age %in% "AD",] 

# subset to only records with mass data
panama.df <- panama.df[!is.na(panama.df$Mass),]

# order by month and year
panama.df <- panama.df[order(panama.df$Year, panama.df$Month),]

# rename columns
colnames(panama.df) <- c("species","month","year","sex","age","mass")

# make sure all species are represented by changing to character
panama.df$species <- as.character(panama.df$species)

# check out which years are represented
unique(panama.df$year)

# create binary code for sampling period
panama.df$period <- ifelse(panama.df$year < median(panama.df$year), "old", "new")

# create mini data frames by period for filtering
panama.old <- panama.df[panama.df$period=="old",]
panama.new <- panama.df[panama.df$period=="new",]

# drop old species without five individuals per time period
panama.old.species <- as.data.frame(table(panama.old$species))
colnames(panama.old.species) <- c("species", "frequency")
panama.sp.old.keep <- panama.old.species[which(panama.old.species$frequency > 4),]
panama.sp.old.keep <- as.character(panama.sp.old.keep$species)

# drop new species without five individuals per time period
panama.new.species <- as.data.frame(table(panama.new$species))
colnames(panama.new.species) <- c("species", "frequency")
panama.sp.new.keep <- panama.new.species[which(panama.new.species$frequency > 4),]
panama.sp.new.keep <- as.character(panama.sp.new.keep$species)

# create mini data frames by sex for filtering
panama.old.m <- panama.old[panama.old$sex=="M",]
panama.old.f <- panama.old[panama.old$sex=="F",]
panama.new.m <- panama.new[panama.new$sex=="M",]
panama.new.f <- panama.new[panama.new$sex=="F",]

# drop species without 3 male individuals per sex per time period
panama.old.m.species <- as.data.frame(table(panama.old.m$species))
colnames(panama.old.m.species) <- c("species", "frequency")
panama.sp.old.m.keep <- panama.old.m.species[which(panama.old.m.species$frequency > 2),]
panama.sp.old.m.keep <- as.character(panama.sp.old.m.keep$species)
panama.new.m.species <- as.data.frame(table(panama.new.m$species))
colnames(panama.new.m.species) <- c("species", "frequency")
panama.sp.new.m.keep <- panama.new.m.species[which(panama.new.m.species$frequency > 2),]
panama.sp.new.m.keep <- as.character(panama.sp.new.m.keep$species)

# drop species without 3 female individuals per sex per time period
panama.old.f.species <- as.data.frame(table(panama.old.f$species))
colnames(panama.old.f.species) <- c("species", "frequency")
panama.sp.old.f.keep <- panama.old.f.species[which(panama.old.f.species$frequency > 2),]
panama.sp.old.f.keep <- as.character(panama.sp.old.f.keep$species)
panama.new.f.species <- as.data.frame(table(panama.new.f$species))
colnames(panama.new.f.species) <- c("species", "frequency")
panama.sp.new.f.keep <- panama.new.f.species[which(panama.new.f.species$frequency > 2),]
panama.sp.new.f.keep <- as.character(panama.sp.new.f.keep$species)

# find overlapping set
panama.sp.sex.keep <- intersect(panama.sp.new.f.keep, panama.sp.new.m.keep) 
panama.sp.time.keep <- intersect(panama.sp.old.f.keep, panama.sp.old.m.keep)
panama.sp.keep <- intersect(panama.sp.sex.keep, panama.sp.time.keep)

# subset major data frame
panama.df <- panama.df[which(panama.df$species %in% panama.sp.keep),]

# drop mass outliers (> 2 sd)
panama.df <- panama.df[abs(panama.df$mass-mean(panama.df$mass))< 2*sd(panama.df$mass),]

# count number of species
unique(panama.df$species) # 18

# write filtered data frame
write.csv(panama.df, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered.csv")

## puerto rico!

# load puertorico data
puertorico.full <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/puertorico_raw.csv") 

# subset to relevant columns; not enough good sex data
puertorico.df <- puertorico.full[,c("Updated.Scientific.Name","Month","Year","Band.Number","Age","Mass")]

# subset to adults only
puertorico.df <- puertorico.df[puertorico.df$Age %in% "AD",] 

# subset to only records with mass data
puertorico.df <- puertorico.df[!is.na(puertorico.df$Mass),] 

# order by month and year
puertorico.df <- puertorico.df[order(puertorico.df$Year, puertorico.df$Month),]

# drop duplicates
puertorico.df <- puertorico.df[!duplicated(puertorico.df$Band.Number),] # down to 1423

# rename columns
colnames(puertorico.df) <- c("species","month","year","band_no","age","mass")

# make sure all species are represented by changing to character
puertorico.df$species <- as.character(puertorico.df$species)

# check out which years are represented
unique(puertorico.df$year)

# create binary code for sampling period
puertorico.df$period <- ifelse(puertorico.df$year < median(puertorico.df$year), "old", "new")

# create mini data frames by period for filtering
puertorico.old <- puertorico.df[puertorico.df$period=="old",]
puertorico.new <- puertorico.df[puertorico.df$period=="new",]

# drop old species without 3 individuals per time period
puertorico.old.species <- as.data.frame(table(puertorico.old$species))
colnames(puertorico.old.species) <- c("species", "frequency")
puertorico.sp.old.keep <- puertorico.old.species[which(puertorico.old.species$frequency > 2),] # reduce stringency
puertorico.sp.old.keep <- as.character(puertorico.sp.old.keep$species)

# drop new species without 3 individuals per time period
puertorico.new.species <- as.data.frame(table(puertorico.new$species))
colnames(puertorico.new.species) <- c("species", "frequency")
puertorico.sp.new.keep <- puertorico.new.species[which(puertorico.new.species$frequency > 2),] # reduce stringency
puertorico.sp.new.keep <- as.character(puertorico.sp.new.keep$species)

# find overlapping set
puertorico.sp.keep <- intersect(puertorico.sp.new.keep, puertorico.sp.old.keep)

# subset major data frame
puertorico.df <- puertorico.df[which(puertorico.df$species %in% puertorico.sp.keep),]

# drop NA mass values again
puertorico.df <- puertorico.df[!is.na(puertorico.df$mass),]

# change mass to numeric
puertorico.df$mass <- as.numeric(as.character(puertorico.df$mass))

# drop mass outliers (> 2 sd)
puertorico.df <- puertorico.df[abs(puertorico.df$mass-mean(puertorico.df$mass))< 2*sd(puertorico.df$mass),]

# count number of species
unique(puertorico.df$species) # 15

# drop new species without five individuals per time period
puertorico.count <- as.data.frame(table(puertorico.df$species))
colnames(puertorico.count) <- c("species", "frequency")
puertorico.final.keep <- puertorico.count[which(puertorico.count$frequency > 10),]
puertorico.final.keep <- as.character(puertorico.final.keep$species)

# subset major data frame again 
puertorico.df <- puertorico.df[which(puertorico.df$species %in% puertorico.final.keep),]

# write filtered data frame
write.csv(puertorico.df, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/puertorico_filtered.csv")

## palo!

# load palo data
palo.full <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_raw.csv") 

# subset to relevant columns
palo.df <- palo.full[,c("Updated.Scientific.Name","Month","Year","Band.Number","Sex","Age","Mass")]

# subset to only known sex
palo.df <- palo.df[palo.df$Sex %in% c("F","M"),] 

# subset to adults only
palo.df <- palo.df[palo.df$Age %in% "AD",] 

# subset to only records with mass data
palo.df <- palo.df[!is.na(palo.df$Mass),]

# order by month and year
palo.df <- palo.df[order(palo.df$Year, palo.df$Month),]

# drop duplicates
palo.df <- palo.df[!duplicated(palo.df$Band.Number),]

# rename columns
colnames(palo.df) <- c("species","month","year","band_no","sex","age","mass")

# make sure all species are represented by changing to character
palo.df$species <- as.character(palo.df$species)

# check out which years are represented
unique(palo.df$year)

# create binary code for sampling period
palo.df$period <- ifelse(palo.df$year <= median(palo.df$year), "old", "new")

# create mini data frames by period for filtering
palo.old <- palo.df[palo.df$period=="old",]
palo.new <- palo.df[palo.df$period=="new",]

# drop old species without five individuals per time period
palo.old.species <- as.data.frame(table(palo.old$species))
colnames(palo.old.species) <- c("species", "frequency")
palo.sp.old.keep <- palo.old.species[which(palo.old.species$frequency > 4),]
palo.sp.old.keep <- as.character(palo.sp.old.keep$species)

# drop new species without five individuals per time period
palo.new.species <- as.data.frame(table(palo.new$species))
colnames(palo.new.species) <- c("species", "frequency")
palo.sp.new.keep <- palo.new.species[which(palo.new.species$frequency > 4),]
palo.sp.new.keep <- as.character(palo.sp.new.keep$species)

# drop mass outliers (> 2 sd)
palo.df <- palo.df[abs(palo.df$mass-mean(palo.df$mass))< 2*sd(palo.df$mass),]

# create mini data frames by sex for filtering
palo.old.m <- palo.old[palo.old$sex=="M",]
palo.old.f <- palo.old[palo.old$sex=="F",]
palo.new.m <- palo.new[palo.new$sex=="M",]
palo.new.f <- palo.new[palo.new$sex=="F",]

# drop species without 3 male individuals per sex per time period
palo.old.m.species <- as.data.frame(table(palo.old.m$species))
colnames(palo.old.m.species) <- c("species", "frequency")
palo.sp.old.m.keep <- palo.old.m.species[which(palo.old.m.species$frequency > 2),]
palo.sp.old.m.keep <- as.character(palo.sp.old.m.keep$species)
palo.new.m.species <- as.data.frame(table(palo.new.m$species))
colnames(palo.new.m.species) <- c("species", "frequency")
palo.sp.new.m.keep <- palo.new.m.species[which(palo.new.m.species$frequency > 2),]
palo.sp.new.m.keep <- as.character(palo.sp.new.m.keep$species)

# drop species without 3 female individuals per sex per time period
palo.old.f.species <- as.data.frame(table(palo.old.f$species))
colnames(palo.old.f.species) <- c("species", "frequency")
palo.sp.old.f.keep <- palo.old.f.species[which(palo.old.f.species$frequency > 2),]
palo.sp.old.f.keep <- as.character(palo.sp.old.f.keep$species)
palo.new.f.species <- as.data.frame(table(palo.new.f$species))
colnames(palo.new.f.species) <- c("species", "frequency")
palo.sp.new.f.keep <- palo.new.f.species[which(palo.new.f.species$frequency > 2),]
palo.sp.new.f.keep <- as.character(palo.sp.new.f.keep$species)

# find overlapping set
palo.sp.sex.keep <- intersect(palo.sp.new.f.keep, palo.sp.new.m.keep) 
palo.sp.time.keep <- intersect(palo.sp.old.f.keep, palo.sp.old.m.keep)
palo.sp.keep <- intersect(palo.sp.sex.keep, palo.sp.time.keep)

# subset major data frame
palo.df <- palo.df[which(palo.df$species %in% palo.sp.keep),]

# drop new species without five individuals per time period
palo.count <- as.data.frame(table(palo.df$species))
colnames(palo.count) <- c("species", "frequency")
palo.final.keep <- palo.count[which(palo.count$frequency > 10),]
palo.final.keep <- as.character(palo.final.keep$species)

# subset major data frame
palo.df <- palo.df[which(palo.df$species %in% palo.final.keep),]

# write filtered data frame
write.csv(palo.df, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered.csv")

## maps!

# load maps data
maps.full <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/maps_raw.csv") 

# subset to relevant columns
maps.df <- maps.full[,c("Updated.Scientific.Name","Month","Year","Band.Number","Sex","Age","Mass")]

# subset to only known sex
maps.df <- maps.df[maps.df$Sex %in% c("F","M"),] 

# subset to adults only
maps.df <- maps.df[maps.df$Age %in% "AD",] 

# subset to only records with mass data
maps.df <- maps.df[!is.na(maps.df$Mass),]

# also make sure mass != 0
maps.df <- maps.df[!maps.df$Mass==0,]

# order by month and year
maps.df <- maps.df[order(maps.df$Year, maps.df$Month),]

# drop duplicates
maps.df <- maps.df[!duplicated(maps.df$Band.Number),]

# rename columns
colnames(maps.df) <- c("species","month","year","band_no","sex","age","mass")

# make sure all species are represented by changing to character
maps.df$species <- as.character(maps.df$species)

# check out which years are represented
unique(maps.df$year)

# create binary code for sampling period
maps.df$period <- ifelse(maps.df$year < median(maps.df$year), "old", "new")

# create mini data frames by period for filtering
maps.old <- maps.df[maps.df$period=="old",]
maps.new <- maps.df[maps.df$period=="new",]

# drop old species without five individuals per time period
maps.old.species <- as.data.frame(table(maps.old$species))
colnames(maps.old.species) <- c("species", "frequency")
maps.sp.old.keep <- maps.old.species[which(maps.old.species$frequency > 4),]
maps.sp.old.keep <- as.character(maps.sp.old.keep$species)

# drop new species without five individuals per time period
maps.new.species <- as.data.frame(table(maps.new$species))
colnames(maps.new.species) <- c("species", "frequency")
maps.sp.new.keep <- maps.new.species[which(maps.new.species$frequency > 4),]
maps.sp.new.keep <- as.character(maps.sp.new.keep$species)

# create mini data frames by sex for filtering
maps.old.m <- maps.old[maps.old$sex=="M",]
maps.old.f <- maps.old[maps.old$sex=="F",]
maps.new.m <- maps.new[maps.new$sex=="M",]
maps.new.f <- maps.new[maps.new$sex=="F",]

# drop species without 3 male individuals per sex per time period
maps.old.m.species <- as.data.frame(table(maps.old.m$species))
colnames(maps.old.m.species) <- c("species", "frequency")
maps.sp.old.m.keep <- maps.old.m.species[which(maps.old.m.species$frequency > 2),]
maps.sp.old.m.keep <- as.character(maps.sp.old.m.keep$species)
maps.new.m.species <- as.data.frame(table(maps.new.m$species))
colnames(maps.new.m.species) <- c("species", "frequency")
maps.sp.new.m.keep <- maps.new.m.species[which(maps.new.m.species$frequency > 2),]
maps.sp.new.m.keep <- as.character(maps.sp.new.m.keep$species)

# drop species without 3 female individuals per sex per time period
maps.old.f.species <- as.data.frame(table(maps.old.f$species))
colnames(maps.old.f.species) <- c("species", "frequency")
maps.sp.old.f.keep <- maps.old.f.species[which(maps.old.f.species$frequency > 2),]
maps.sp.old.f.keep <- as.character(maps.sp.old.f.keep$species)
maps.new.f.species <- as.data.frame(table(maps.new.f$species))
colnames(maps.new.f.species) <- c("species", "frequency")
maps.sp.new.f.keep <- maps.new.f.species[which(maps.new.f.species$frequency > 2),]
maps.sp.new.f.keep <- as.character(maps.sp.new.f.keep$species)

# find overlapping set
maps.sp.sex.keep <- intersect(maps.sp.new.f.keep, maps.sp.new.m.keep) 
maps.sp.time.keep <- intersect(maps.sp.old.f.keep, maps.sp.old.m.keep)
maps.sp.keep <- intersect(maps.sp.sex.keep, maps.sp.time.keep)

# subset major data frame
maps.df <- maps.df[which(maps.df$species %in% maps.sp.keep),]

# drop mass outliers (> 2 sd)
maps.df <- maps.df[abs(maps.df$mass-mean(maps.df$mass))< 2*sd(maps.df$mass),]

# count number of species
unique(maps.df$species) # 28

# write filtered data frame
write.csv(maps.df, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/maps_filtered.csv")

## powdermill!

# load powdermill data
powdermill.full <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_raw.csv") 

# subset to relevant columns
powdermill.df <- powdermill.full[,c("Updated.Scientific.Name","Month","Day","Band.Number","Sex","Age","Mass")]

# rename bad columns 
colnames(powdermill.df) <- c("Updated.Scientific.Name","Year","Month","Band.Number","Sex","Age","Mass")

# subset to only known sex
powdermill.df <- powdermill.df[powdermill.df$Sex %in% c("F","M"),] 

# subset to adults only
powdermill.df <- powdermill.df[powdermill.df$Age %in% "AD",] 

# subset to only records with mass data
powdermill.df <- powdermill.df[!is.na(powdermill.df$Mass),]

# also make sure mass != 0
powdermill.df <- powdermill.df[!powdermill.df$Mass==0,]

# make mass into numeric
powdermill.df$Mass <- as.numeric(as.character(powdermill.df$Mass))

# make month into numeric
powdermill.df$Month <- as.numeric(as.character(powdermill.df$Month))

# order by month and year
powdermill.df <- powdermill.df[order(powdermill.df$Year, powdermill.df$Month),]

# drop missing month data
powdermill.df <- powdermill.df[!is.na(powdermill.df$Month),]

# drop duplicates
powdermill.df <- powdermill.df[!duplicated(powdermill.df$Band.Number),]

# rename columns
colnames(powdermill.df) <- c("species","year","month","band_no","sex","age","mass")

# make sure all species are represented by changing to character
powdermill.df$species <- as.character(powdermill.df$species)

# check out which years are represented
unique(powdermill.df$year)

# create binary code for sampling period
powdermill.df$period <- ifelse(powdermill.df$year < median(powdermill.df$year), "old", "new")

# create mini data frames by period for filtering
powdermill.old <- powdermill.df[powdermill.df$period=="old",]
powdermill.new <- powdermill.df[powdermill.df$period=="new",]

# drop old species without five individuals per time period
powdermill.old.species <- as.data.frame(table(powdermill.old$species))
colnames(powdermill.old.species) <- c("species", "frequency")
powdermill.sp.old.keep <- powdermill.old.species[which(powdermill.old.species$frequency > 4),]
powdermill.sp.old.keep <- as.character(powdermill.sp.old.keep$species)

# drop new species without five individuals per time period
powdermill.new.species <- as.data.frame(table(powdermill.new$species))
colnames(powdermill.new.species) <- c("species", "frequency")
powdermill.sp.new.keep <- powdermill.new.species[which(powdermill.new.species$frequency > 4),]
powdermill.sp.new.keep <- as.character(powdermill.sp.new.keep$species)

# create mini data frames by sex for filtering
powdermill.old.m <- powdermill.old[powdermill.old$sex=="M",]
powdermill.old.f <- powdermill.old[powdermill.old$sex=="F",]
powdermill.new.m <- powdermill.new[powdermill.new$sex=="M",]
powdermill.new.f <- powdermill.new[powdermill.new$sex=="F",]

# drop species without 3 male individuals per sex per time period
powdermill.old.m.species <- as.data.frame(table(powdermill.old.m$species))
colnames(powdermill.old.m.species) <- c("species", "frequency")
powdermill.sp.old.m.keep <- powdermill.old.m.species[which(powdermill.old.m.species$frequency > 2),]
powdermill.sp.old.m.keep <- as.character(powdermill.sp.old.m.keep$species)
powdermill.new.m.species <- as.data.frame(table(powdermill.new.m$species))
colnames(powdermill.new.m.species) <- c("species", "frequency")
powdermill.sp.new.m.keep <- powdermill.new.m.species[which(powdermill.new.m.species$frequency > 2),]
powdermill.sp.new.m.keep <- as.character(powdermill.sp.new.m.keep$species)

# drop species without 3 female individuals per sex per time period
powdermill.old.f.species <- as.data.frame(table(powdermill.old.f$species))
colnames(powdermill.old.f.species) <- c("species", "frequency")
powdermill.sp.old.f.keep <- powdermill.old.f.species[which(powdermill.old.f.species$frequency > 2),]
powdermill.sp.old.f.keep <- as.character(powdermill.sp.old.f.keep$species)
powdermill.new.f.species <- as.data.frame(table(powdermill.new.f$species))
colnames(powdermill.new.f.species) <- c("species", "frequency")
powdermill.sp.new.f.keep <- powdermill.new.f.species[which(powdermill.new.f.species$frequency > 2),]
powdermill.sp.new.f.keep <- as.character(powdermill.sp.new.f.keep$species)

# find overlapping set
powdermill.sp.sex.keep <- intersect(powdermill.sp.new.f.keep, powdermill.sp.new.m.keep) 
powdermill.sp.time.keep <- intersect(powdermill.sp.old.f.keep, powdermill.sp.old.m.keep)
powdermill.sp.keep <- intersect(powdermill.sp.sex.keep, powdermill.sp.time.keep)

# subset major data frame
powdermill.df <- powdermill.df[which(powdermill.df$species %in% powdermill.sp.keep),]

# drop NA mass values again
powdermill.df <- powdermill.df[!is.na(powdermill.df$mass),]

# drop mass outliers (> 2 sd)
powdermill.df <- powdermill.df[abs(powdermill.df$mass-mean(powdermill.df$mass))< 2*sd(powdermill.df$mass),]

# count number of species
unique(powdermill.df$species) # 20

# drop new species without five individuals per time period
powdermill.count <- as.data.frame(table(powdermill.df$species))
colnames(powdermill.count) <- c("species", "frequency")
powdermill.final.keep <- powdermill.count[which(powdermill.count$frequency > 10),]
powdermill.final.keep <- as.character(powdermill.final.keep$species)

# subset major data frame again
powdermill.df <- powdermill.df[which(powdermill.df$species %in% powdermill.final.keep),]

# write filtered data frame
write.csv(powdermill.df, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered.csv")

## make months df
brazil.mo <- as.data.frame(brazil.df[,c("month")])
brazil.mo$country <- rep("brazil", nrow(brazil.mo))
colnames(brazil.mo) <- c("month","country")
panama.mo <- as.data.frame(panama.df[,c("month")])
panama.mo$country <- rep("panama", nrow(panama.mo))
colnames(panama.mo) <- c("month","country")
puertorico.mo <- as.data.frame(puertorico.df[,c("month")])
puertorico.mo$country <- rep("puertorico", nrow(puertorico.mo))
colnames(puertorico.mo) <- c("month","country")
palo.mo <- as.data.frame(palo.df[,c("month")])
palo.mo$country <- rep("palo", nrow(palo.mo))
colnames(palo.mo) <- c("month","country")
maps.mo <- as.data.frame(maps.df[,c("month")])
maps.mo$country <- rep("maps", nrow(maps.mo))
colnames(maps.mo) <- c("month","country")
powdermill.mo <- as.data.frame(powdermill.df[,c("month")])
powdermill.mo$country <- rep("powdermill", nrow(powdermill.mo))
colnames(powdermill.mo) <- c("month","country")
months.df <- rbind(brazil.mo,panama.mo,puertorico.mo,palo.mo,maps.mo,powdermill.mo)

# write months df
write.csv(months.df, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/months.csv")

