### manipulate size data for regression and plotting ###

## brazil!

# load brazil data
brazil.full <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_raw.csv") 

# subset to relevant columns
brazil.df <- brazil.full[,c("Updated.Scientific.Name","Month","Year","Band.Number","Sex","Age","Mass","Wing.Length","Tarsus")]

# exclude juvs
brazil.df <- brazil.df[!brazil.df$Age %in% "JUNKV",] 

# ensure mass etc are numeric; drop NAs
brazil.df$Mass <- as.numeric(as.character(brazil.df$Mass))
brazil.df$Wing.Length <- as.numeric(as.character(brazil.df$Wing.Length))
brazil.df$Tarsus <- as.numeric(as.character(brazil.df$Tarsus))
brazil.df <- brazil.df[!is.na(brazil.df$Mass),]
brazil.df <- brazil.df[!brazil.df$Mass==0.0,]

# order by month and year
brazil.df <- brazil.df[order(brazil.df$Year, brazil.df$Month),]

# drop duplicates
brazil.df <- brazil.df[!duplicated(brazil.df$Band.Number),]

# rename columns
colnames(brazil.df) <- c("species","month","year","band_no","sex","age","mass","wing_length","tarsus")

# make sure all species are represented by changing to character
brazil.df$species <- as.character(brazil.df$species)
brazil.df <- brazil.df[!is.na(brazil.df$species),]

# check out which years are represented
unique(brazil.df$year)

# drop mass outliers for each species (> 2 sd)
temp.df <- list()
for(i in unique(brazil.df$species)){
  tmp <- brazil.df[brazil.df$species==i,]
  tmp.b <- tmp[abs(tmp$mass-mean(tmp$mass))< 4*sd(tmp$mass),]
  temp.df[[i]] <- tmp.b
}
brazil.df <- do.call(rbind, temp.df)
rownames(brazil.df) <- NULL

# create binary code for sampling period
brazil.df$period <- ifelse(brazil.df$year <= median(brazil.df$year, na.rm = TRUE), "old", "new")

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

# find shared set, subset data frame
brazil.sp.time.keep <- intersect(brazil.sp.old.keep, brazil.sp.new.keep)
brazil.df.all <- brazil.df[which(brazil.df$species %in% brazil.sp.time.keep),]

# how many records and species?
nrow(brazil.df.all) #14064
length(unique(brazil.df.all$species)) #73
table(brazil.df.all$species)

# write filtered data frame
write.csv(brazil.df.all, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_all.csv")

# subset to only known sex
brazil.df.s <- brazil.df.all[brazil.df.all$sex %in% c("F","M"),] 

# ensure mass etc are numeric; drop NAs
brazil.df.s$mass <- as.numeric(as.character(brazil.df.s$mass))
brazil.df.s <- brazil.df.s[!is.na(brazil.df.s$mass),]

# create mini data frames by sex for filtering
brazil.old <- brazil.df.s[brazil.df.s$period=="old",]
brazil.new <- brazil.df.s[brazil.df.s$period=="new",]
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

# subset sex df
brazil.df.sex <- brazil.df.s[which(brazil.df.s$species %in% brazil.sp.keep),]

# how many records and species?
nrow(brazil.df.sex) #6773
length(unique(brazil.df.sex$species)) #32

# write filtered data frame
write.csv(brazil.df.sex, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_sex.csv")

## panama!

# load panama data
panama.full <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_raw.csv") 

# subset to relevant columns
panama.df <- panama.full[,c("Updated.Scientific.Name","Month","Year","Band.Number","Sex","Age","Mass","Wing.Length","Tarsus")]

# exclude juvs
panama.df <- panama.df[!panama.df$Age %in% "JUV",] 

# ensure mass etc are numeric; drop NAs
panama.df$Mass <- as.numeric(as.character(panama.df$Mass))
panama.df$Wing.Length <- as.numeric(as.character(panama.df$Wing.Length))
panama.df$Tarsus <- as.numeric(as.character(panama.df$Tarsus))
panama.df <- panama.df[!is.na(panama.df$Mass),]
panama.df <- panama.df[!panama.df$Mass==0.0,]

# order by month and year
panama.df <- panama.df[order(panama.df$Year, panama.df$Month),]

# no band number data; no duplicates to drop

# rename columns
colnames(panama.df) <- c("species","month","year","band_no","sex","age","mass","wing_length","tarsus")

# make sure all species are represented by changing to character
panama.df$species <- as.character(panama.df$species)
panama.df <- panama.df[!is.na(panama.df$species),]

# check out which years are represented
unique(panama.df$year)

# drop mass outliers for each species (> 2 sd)
temp.df <- list()
for(i in unique(panama.df$species)){
  print(i)
  tmp <- panama.df[panama.df$species==i,]
  tmp.b <- tmp[abs(tmp$mass-mean(tmp$mass))< 4*sd(tmp$mass),]
  temp.df[[i]] <- tmp.b
}
panama.df <- do.call(rbind, temp.df)
rownames(panama.df) <- NULL

# create binary code for sampling period
panama.df$period <- ifelse(panama.df$year <= median(panama.df$year, na.rm = TRUE), "old", "new")

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

# find shared set, subset data frame
panama.sp.time.keep <- intersect(panama.sp.old.keep, panama.sp.new.keep)
panama.df.all <- panama.df[which(panama.df$species %in% panama.sp.time.keep),]

# how many records and species?
nrow(panama.df.all) #8605
length(unique(panama.df.all$species)) #57

# write filtered data frame
write.csv(panama.df.all, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered_all.csv")

# subset to only known sex
panama.df.s <- panama.df.all[panama.df.all$sex %in% c("F","M"),] 

# create mini data frames by sex for filtering
panama.old <- panama.df.s[panama.df.s$period=="old",]
panama.new <- panama.df.s[panama.df.s$period=="new",]
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

# subset sex df
panama.df.sex <- panama.df.s[which(panama.df.s$species %in% panama.sp.keep),]

# ensure mass etc are numeric; drop NAs
panama.df.sex$mass <- as.numeric(as.character(panama.df.sex$mass))
panama.df.sex <- panama.df.sex[!is.na(panama.df.sex$mass),]

# how many records and species?
nrow(panama.df.sex) #2437
length(unique(panama.df.sex$species)) #18

# write filtered data frame
write.csv(panama.df.sex, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered_sex.csv")

## guanica!

# load guanica data
guanica.full <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_raw.csv") 

# subset to relevant columns
guanica.df <- guanica.full[,c("Updated.Scientific.Name","Month","Year","Band.Number","Sex","Age","Mass","Wing.Length","Tarsus")]

# exclude juvs
guanica.df <- guanica.df[!guanica.df$Age %in% "JUV",] 

# ensure mass etc are numeric; drop NAs
guanica.df$Mass <- as.numeric(as.character(guanica.df$Mass))
guanica.df$Wing.Length <- as.numeric(as.character(guanica.df$Wing.Length))
guanica.df$Tarsus <- as.numeric(as.character(guanica.df$Tarsus))
guanica.df <- guanica.df[!is.na(guanica.df$Mass),]
guanica.df <- guanica.df[!guanica.df$Mass==0.0,]

# order by month and year
guanica.df <- guanica.df[order(guanica.df$Year, guanica.df$Month),]

# drop duplicates
guanica.df <- guanica.df[!duplicated(guanica.df$Band.Number),]

# rename columns
colnames(guanica.df) <- c("species","month","year","band_no","sex","age","mass","wing_length","tarsus")

# make sure all species are represented by changing to character
guanica.df$species <- as.character(guanica.df$species)
guanica.df <- guanica.df[!is.na(guanica.df$species),]

# check out which years are represented
unique(guanica.df$year)

# drop mass outliers for each species (> 2 sd)
temp.df <- list()
for(i in unique(guanica.df$species)){
  tmp <- guanica.df[guanica.df$species==i,]
  tmp.b <- tmp[abs(tmp$mass-mean(tmp$mass))< 4*sd(tmp$mass),]
  temp.df[[i]] <- tmp.b
}
guanica.df <- do.call(rbind, temp.df)
rownames(guanica.df) <- NULL

# create binary code for sampling period
guanica.df$period <- ifelse(guanica.df$year <= median(guanica.df$year, na.rm = TRUE), "old", "new")

# create mini data frames by period for filtering
guanica.old <- guanica.df[guanica.df$period=="old",]
guanica.new <- guanica.df[guanica.df$period=="new",]

# drop old species without five individuals per time period
guanica.old.species <- as.data.frame(table(guanica.old$species))
colnames(guanica.old.species) <- c("species", "frequency")
guanica.sp.old.keep <- guanica.old.species[which(guanica.old.species$frequency > 4),]
guanica.sp.old.keep <- as.character(guanica.sp.old.keep$species)

# drop new species without five individuals per time period
guanica.new.species <- as.data.frame(table(guanica.new$species))
colnames(guanica.new.species) <- c("species", "frequency")
guanica.sp.new.keep <- guanica.new.species[which(guanica.new.species$frequency > 4),]
guanica.sp.new.keep <- as.character(guanica.sp.new.keep$species)

# find shared set, subset data frame
guanica.sp.time.keep <- intersect(guanica.sp.old.keep, guanica.sp.new.keep)
guanica.df.all <- guanica.df[which(guanica.df$species %in% guanica.sp.time.keep),]

# how many records and species?
nrow(guanica.df.all) #5384
length(unique(guanica.df.all$species)) #24

# write filtered data frame
write.csv(guanica.df.all, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered_all.csv")

# subset to only known sex
guanica.df.s <- guanica.df.all[guanica.df.all$sex %in% c("F","M"),] 

# create mini data frames by sex for filtering
guanica.old <- guanica.df.s[guanica.df.s$period=="old",]
guanica.new <- guanica.df.s[guanica.df.s$period=="new",]
guanica.old.m <- guanica.old[guanica.old$sex=="M",]
guanica.old.f <- guanica.old[guanica.old$sex=="F",]
guanica.new.m <- guanica.new[guanica.new$sex=="M",]
guanica.new.f <- guanica.new[guanica.new$sex=="F",]

# drop species without 3 male individuals per sex per time period
guanica.old.m.species <- as.data.frame(table(guanica.old.m$species))
colnames(guanica.old.m.species) <- c("species", "frequency")
guanica.sp.old.m.keep <- guanica.old.m.species[which(guanica.old.m.species$frequency > 2),]
guanica.sp.old.m.keep <- as.character(guanica.sp.old.m.keep$species)
guanica.new.m.species <- as.data.frame(table(guanica.new.m$species))
colnames(guanica.new.m.species) <- c("species", "frequency")
guanica.sp.new.m.keep <- guanica.new.m.species[which(guanica.new.m.species$frequency > 2),]
guanica.sp.new.m.keep <- as.character(guanica.sp.new.m.keep$species)

# drop species without 3 female individuals per sex per time period
guanica.old.f.species <- as.data.frame(table(guanica.old.f$species))
colnames(guanica.old.f.species) <- c("species", "frequency")
guanica.sp.old.f.keep <- guanica.old.f.species[which(guanica.old.f.species$frequency > 2),]
guanica.sp.old.f.keep <- as.character(guanica.sp.old.f.keep$species)
guanica.new.f.species <- as.data.frame(table(guanica.new.f$species))
colnames(guanica.new.f.species) <- c("species", "frequency")
guanica.sp.new.f.keep <- guanica.new.f.species[which(guanica.new.f.species$frequency > 2),]
guanica.sp.new.f.keep <- as.character(guanica.sp.new.f.keep$species)

# find overlapping set
guanica.sp.sex.keep <- intersect(guanica.sp.new.f.keep, guanica.sp.new.m.keep) 
guanica.sp.time.keep <- intersect(guanica.sp.old.f.keep, guanica.sp.old.m.keep)
guanica.sp.keep <- intersect(guanica.sp.sex.keep, guanica.sp.time.keep)

# subset sex df
guanica.df.sex <- guanica.df.s[which(guanica.df.s$species %in% guanica.sp.keep),]

# how many records and species?
nrow(guanica.df.sex) #2381
length(unique(guanica.df.sex$species)) #15

# write filtered data frame
write.csv(guanica.df.sex, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered_sex.csv")

## palomino!

# load palo data
palo.full <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_raw.csv") 

# subset to relevant columns
palo.df <- palo.full[,c("Updated.Scientific.Name","Month","Year","Band.Number","Sex","Age","Mass","Wing.Length","Tarsus")]

# select only june & july
palo.df <- palo.df[!palo.df$Month %in% c(6,7),] 

# exclude juvs
palo.df <- palo.df[!palo.df$Age %in% "JUV",] 

# subset to only records with mass data
palo.df <- palo.df[!is.na(palo.df$Mass),]

# ensure mass etc are numeric; drop NAs
palo.df$Mass <- as.numeric(as.character(palo.df$Mass))
palo.df$Wing.Length <- as.numeric(as.character(palo.df$Wing.Length))
palo.df$Tarsus <- as.numeric(as.character(palo.df$Tarsus))
palo.df <- palo.df[!is.na(palo.df$Mass),]
palo.df <- palo.df[!palo.df$Mass==0.0,]

# order by month and year
palo.df <- palo.df[order(palo.df$Year, palo.df$Month),]

# drop duplicates
palo.df <- palo.df[!duplicated(palo.df$Band.Number),]

# rename columns
colnames(palo.df) <- c("species","month","year","band_no","sex","age","mass","wing_length","tarsus")

# make sure all species are represented by changing to character
palo.df$species <- as.character(palo.df$species)
palo.df <- palo.df[!is.na(palo.df$species),]

# check out which years are represented
unique(palo.df$year)

# drop mass outliers for each species (> 2 sd)
temp.df <- list()
for(i in unique(palo.df$species)){
  tmp <- palo.df[palo.df$species==i,]
  tmp.b <- tmp[abs(tmp$mass-mean(tmp$mass))< 4*sd(tmp$mass),]
  temp.df[[i]] <- tmp.b
}
palo.df <- do.call(rbind, temp.df)
rownames(palo.df) <- NULL

# create binary code for sampling period
palo.df$period <- ifelse(palo.df$year <= median(palo.df$year, na.rm = TRUE), "old", "new")

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

# find shared set, subset data frame
palo.sp.time.keep <- intersect(palo.sp.old.keep, palo.sp.new.keep)
palo.df.all <- palo.df[which(palo.df$species %in% palo.sp.time.keep),]

# how many records and species?
nrow(palo.df.all) #35366
length(unique(palo.df.all$species)) #65

# write filtered data frame
write.csv(palo.df.all, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered_all.csv")

# subset to only known sex
palo.df.s <- palo.df.all[palo.df.all$sex %in% c("F","M"),] 

# create mini data frames by sex for filtering
palo.old <- palo.df.s[palo.df.s$period=="old",]
palo.new <- palo.df.s[palo.df.s$period=="new",]
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

# subset sex df
palo.df.sex <- palo.df.s[which(palo.df.s$species %in% palo.sp.keep),]

# how many records and species?
nrow(palo.df.sex) #20617
length(unique(palo.df.sex$species)) #37

# write filtered data frame
write.csv(palo.df.sex, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered_sex.csv")

## powdermill!

# load powdermill data
powdermill.full <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_raw.csv") 

# correct column names
colnames(powdermill.full)[3] <- "Year"
colnames(powdermill.full)[4] <- "Month"
colnames(powdermill.full)[5] <- "Day"

# subset to relevant columns
powdermill.df <- powdermill.full[,c("Updated.Scientific.Name","Month","Year","Band.Number","Sex","Age","Mass","Wing.Length","Tarsus")]

# select only june & july
powdermill.df <- powdermill.df[!powdermill.df$Month %in% c(6,7),] 

# exclude juvs
powdermill.df <- powdermill.df[!powdermill.df$Age %in% "JUV",] 

# ensure mass etc are numeric; drop NAs
powdermill.df$Mass <- as.numeric(as.character(powdermill.df$Mass))
powdermill.df$Wing.Length <- as.numeric(as.character(powdermill.df$Wing.Length))
powdermill.df$Tarsus <- as.numeric(as.character(powdermill.df$Tarsus))
powdermill.df <- powdermill.df[!is.na(powdermill.df$Mass),]
powdermill.df <- powdermill.df[!powdermill.df$Mass==0.0,]

# order by month and year
powdermill.df <- powdermill.df[order(powdermill.df$Year, powdermill.df$Month),]

# drop bad year record
powdermill.df <- powdermill.df[!powdermill.df$Year<2000,]

# drop duplicates
powdermill.df <- powdermill.df[!duplicated(powdermill.df$Band.Number),]

# rename columns
colnames(powdermill.df) <- c("species","month","year","band_no","sex","age","mass","wing_length","tarsus")

# make sure all species are represented by changing to character
powdermill.df$species <- as.character(powdermill.df$species)
powdermill.df <- powdermill.df[!is.na(powdermill.df$species),]

# check out which years are represented
unique(powdermill.df$year)

# drop mass outliers for each species (> 2 sd)
temp.df <- list()
for(i in unique(powdermill.df$species)){
  tmp <- powdermill.df[powdermill.df$species==i,]
  tmp.b <- tmp[abs(tmp$mass-mean(tmp$mass))< 4*sd(tmp$mass),]
  temp.df[[i]] <- tmp.b
}
powdermill.df <- do.call(rbind, temp.df)
rownames(powdermill.df) <- NULL

# create binary code for sampling period
powdermill.df$period <- ifelse(powdermill.df$year <= median(powdermill.df$year,na.rm = TRUE), "old", "new")

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

# find shared set, subset data frame
powdermill.sp.time.keep <- intersect(powdermill.sp.old.keep, powdermill.sp.new.keep)
powdermill.df.all <- powdermill.df[which(powdermill.df$species %in% powdermill.sp.time.keep),]

# how many records and species?
nrow(powdermill.df.all) #79066
length(unique(powdermill.df.all$species)) #112

# write filtered data frame
write.csv(powdermill.df.all, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered_all.csv")

# subset to only known sex
powdermill.df.s <- powdermill.df.all[powdermill.df.all$sex %in% c("F","M"),] 

# create mini data frames by sex for filtering
powdermill.old <- powdermill.df.s[powdermill.df.s$period=="old",]
powdermill.new <- powdermill.df.s[powdermill.df.s$period=="new",]
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

# subset sex df
powdermill.df.sex <- powdermill.df.s[which(powdermill.df.s$species %in% powdermill.sp.keep),]

# how many records and species?
nrow(powdermill.df.sex) #50111
length(unique(powdermill.df.sex$species)) #70

# write filtered data frame
write.csv(powdermill.df.sex, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered_sex.csv")

# maps: TSS!

# load maps data
maps.full <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/maps_raw.csv") 

# subset to site TSS
tss.full <- maps.full[maps.full$Data.Set=="TSS",]

# subset to relevant columns
tss.df <- tss.full[,c("Updated.Scientific.Name","Month","Year","Band.Number","Sex","Age","Mass","Wing.Length","Tarsus")]

# select only june & july
tss.df <- tss.df[!tss.df$Month %in% c(6,7),] 

# exclude juvs
tss.df <- tss.df[!tss.df$Age %in% "JUV",] 

# ensure mass etc are numeric; drop NAs
tss.df$Mass <- as.numeric(as.character(tss.df$Mass))
tss.df$Wing.Length <- as.numeric(as.character(tss.df$Wing.Length))
tss.df$Tarsus <- as.numeric(as.character(tss.df$Tarsus))
tss.df <- tss.df[!is.na(tss.df$Mass),]
tss.df <- tss.df[!tss.df$Mass==0.0,]

# order by month and year
tss.df <- tss.df[order(tss.df$Year, tss.df$Month),]

# drop duplicates
tss.df <- tss.df[!duplicated(tss.df$Band.Number),]

# rename columns
colnames(tss.df) <- c("species","month","year","band_no","sex","age","mass","wing_length","tarsus")

# make sure all species are represented by changing to character
tss.df$species <- as.character(tss.df$species)
tss.df <- tss.df[!is.na(tss.df$species),]

# check out which years are represented
unique(tss.df$year)

# drop mass outliers for each species (> 2 sd)
temp.df <- list()
for(i in unique(tss.df$species)){
  tmp <- tss.df[tss.df$species==i,]
  tmp.b <- tmp[abs(tmp$mass-mean(tmp$mass))< 4*sd(tmp$mass),]
  temp.df[[i]] <- tmp.b
}
tss.df <- do.call(rbind, temp.df)
rownames(tss.df) <- NULL

# create binary code for sampling period
tss.df$period <- ifelse(tss.df$year <= median(tss.df$year, na.rm=TRUE), "old", "new")

# create mini data frames by period for filtering
tss.old <- tss.df[tss.df$period=="old",]
tss.new <- tss.df[tss.df$period=="new",]

# drop old species without five individuals per time period
tss.old.species <- as.data.frame(table(tss.old$species))
colnames(tss.old.species) <- c("species", "frequency")
tss.sp.old.keep <- tss.old.species[which(tss.old.species$frequency > 4),]
tss.sp.old.keep <- as.character(tss.sp.old.keep$species)

# drop new species without five individuals per time period
tss.new.species <- as.data.frame(table(tss.new$species))
colnames(tss.new.species) <- c("species", "frequency")
tss.sp.new.keep <- tss.new.species[which(tss.new.species$frequency > 4),]
tss.sp.new.keep <- as.character(tss.sp.new.keep$species)

# find shared set, subset data frame
tss.sp.time.keep <- intersect(tss.sp.old.keep, tss.sp.new.keep)
tss.df.all <- tss.df[which(tss.df$species %in% tss.sp.time.keep),]

# how many records and species?
nrow(tss.df.all) #445
length(unique(tss.df.all$species)) #13

# write filtered data frame
write.csv(tss.df.all, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered_all.csv")

# subset to only known sex
tss.df.s <- tss.df.all[tss.df.all$sex %in% c("F","M"),] 

# create mini data frames by sex for filtering
tss.old <- tss.df.s[tss.df.s$period=="old",]
tss.new <- tss.df.s[tss.df.s$period=="new",]
tss.old.m <- tss.old[tss.old$sex=="M",]
tss.old.f <- tss.old[tss.old$sex=="F",]
tss.new.m <- tss.new[tss.new$sex=="M",]
tss.new.f <- tss.new[tss.new$sex=="F",]

# drop species without 3 male individuals per sex per time period
tss.old.m.species <- as.data.frame(table(tss.old.m$species))
colnames(tss.old.m.species) <- c("species", "frequency")
tss.sp.old.m.keep <- tss.old.m.species[which(tss.old.m.species$frequency > 2),]
tss.sp.old.m.keep <- as.character(tss.sp.old.m.keep$species)
tss.new.m.species <- as.data.frame(table(tss.new.m$species))
colnames(tss.new.m.species) <- c("species", "frequency")
tss.sp.new.m.keep <- tss.new.m.species[which(tss.new.m.species$frequency > 2),]
tss.sp.new.m.keep <- as.character(tss.sp.new.m.keep$species)

# drop species without 3 female individuals per sex per time period
tss.old.f.species <- as.data.frame(table(tss.old.f$species))
colnames(tss.old.f.species) <- c("species", "frequency")
tss.sp.old.f.keep <- tss.old.f.species[which(tss.old.f.species$frequency > 2),]
tss.sp.old.f.keep <- as.character(tss.sp.old.f.keep$species)
tss.new.f.species <- as.data.frame(table(tss.new.f$species))
colnames(tss.new.f.species) <- c("species", "frequency")
tss.sp.new.f.keep <- tss.new.f.species[which(tss.new.f.species$frequency > 2),]
tss.sp.new.f.keep <- as.character(tss.sp.new.f.keep$species)

# find overlapping set
tss.sp.sex.keep <- intersect(tss.sp.new.f.keep, tss.sp.new.m.keep) 
tss.sp.time.keep <- intersect(tss.sp.old.f.keep, tss.sp.old.m.keep)
tss.sp.keep <- intersect(tss.sp.sex.keep, tss.sp.time.keep)

# subset sex df
tss.df.sex <- tss.df.s[which(tss.df.s$species %in% tss.sp.keep),]

# how many records and species?
nrow(tss.df.sex) #205
length(unique(tss.df.sex$species)) #5

# write filtered data frame
write.csv(tss.df.sex, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered_sex.csv")

## maps: WATE!

# load maps data
maps.full <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/maps_raw.csv") 

# subset to site wate
wate.full <- maps.full[maps.full$Data.Set=="WATE",]

# subset to relevant columns
wate.df <- wate.full[,c("Updated.Scientific.Name","Month","Year","Band.Number","Sex","Age","Mass","Wing.Length","Tarsus")]

# select only june & july
wate.df <- wate.df[!wate.df$Month %in% c(6,7),] 

# exclude juvs
wate.df <- wate.df[!wate.df$Age %in% "JUV",] 

# ensure mass etc are numeric; drop NAs
wate.df$Mass <- as.numeric(as.character(wate.df$Mass))
wate.df$Wing.Length <- as.numeric(as.character(wate.df$Wing.Length))
wate.df$Tarsus <- as.numeric(as.character(wate.df$Tarsus))
wate.df <- wate.df[!is.na(wate.df$Mass),]
wate.df <- wate.df[!wate.df$Mass==0.0,]

# order by month and year
wate.df <- wate.df[order(wate.df$Year, wate.df$Month),]

# drop duplicates
wate.df <- wate.df[!duplicated(wate.df$Band.Number),]

# rename columns
colnames(wate.df) <- c("species","month","year","band_no","sex","age","mass","wing_length","tarsus")

# make sure all species are represented by changing to character
wate.df$species <- as.character(wate.df$species)
wate.df <- wate.df[!is.na(wate.df$species),]

# check out which years are represented
unique(wate.df$year)

# drop mass outliers for each species (> 2 sd)
temp.df <- list()
for(i in unique(wate.df$species)){
  tmp <- wate.df[wate.df$species==i,]
  tmp.b <- tmp[abs(tmp$mass-mean(tmp$mass))< 4*sd(tmp$mass),]
  temp.df[[i]] <- tmp.b
}
wate.df <- do.call(rbind, temp.df)
rownames(wate.df) <- NULL

# create binary code for sampling period
wate.df$period <- ifelse(wate.df$year <= median(wate.df$year, na.rm = TRUE), "old", "new")

# create mini data frames by period for filtering
wate.old <- wate.df[wate.df$period=="old",]
wate.new <- wate.df[wate.df$period=="new",]

# drop old species without five individuals per time period
wate.old.species <- as.data.frame(table(wate.old$species))
colnames(wate.old.species) <- c("species", "frequency")
wate.sp.old.keep <- wate.old.species[which(wate.old.species$frequency > 4),]
wate.sp.old.keep <- as.character(wate.sp.old.keep$species)

# drop new species without five individuals per time period
wate.new.species <- as.data.frame(table(wate.new$species))
colnames(wate.new.species) <- c("species", "frequency")
wate.sp.new.keep <- wate.new.species[which(wate.new.species$frequency > 4),]
wate.sp.new.keep <- as.character(wate.sp.new.keep$species)

# find shared set, subset data frame
wate.sp.time.keep <- intersect(wate.sp.old.keep, wate.sp.new.keep)
wate.df.all <- wate.df[which(wate.df$species %in% wate.sp.time.keep),]

# how many records and species?
nrow(wate.df.all) #53
length(unique(wate.df.all$species)) #1

# write filtered data frame
write.csv(wate.df.all, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered_all.csv")

# subset to only known sex
wate.df.s <- wate.df.all[wate.df.all$sex %in% c("F","M"),] 

# create mini data frames by sex for filtering
wate.old <- wate.df.s[wate.df.s$period=="old",]
wate.new <- wate.df.s[wate.df.s$period=="new",]
wate.old.m <- wate.old[wate.old$sex=="M",]
wate.old.f <- wate.old[wate.old$sex=="F",]
wate.new.m <- wate.new[wate.new$sex=="M",]
wate.new.f <- wate.new[wate.new$sex=="F",]

# drop species without 3 male individuals per sex per time period
wate.old.m.species <- as.data.frame(table(wate.old.m$species))
colnames(wate.old.m.species) <- c("species", "frequency")
wate.sp.old.m.keep <- wate.old.m.species[which(wate.old.m.species$frequency > 2),]
wate.sp.old.m.keep <- as.character(wate.sp.old.m.keep$species)
wate.new.m.species <- as.data.frame(table(wate.new.m$species))
colnames(wate.new.m.species) <- c("species", "frequency")
wate.sp.new.m.keep <- wate.new.m.species[which(wate.new.m.species$frequency > 2),]
wate.sp.new.m.keep <- as.character(wate.sp.new.m.keep$species)

# drop species without 3 female individuals per sex per time period
wate.old.f.species <- as.data.frame(table(wate.old.f$species))
colnames(wate.old.f.species) <- c("species", "frequency")
wate.sp.old.f.keep <- wate.old.f.species[which(wate.old.f.species$frequency > 2),]
wate.sp.old.f.keep <- as.character(wate.sp.old.f.keep$species)
wate.new.f.species <- as.data.frame(table(wate.new.f$species))
colnames(wate.new.f.species) <- c("species", "frequency")
wate.sp.new.f.keep <- wate.new.f.species[which(wate.new.f.species$frequency > 2),]
wate.sp.new.f.keep <- as.character(wate.sp.new.f.keep$species)

# find overlapping set
wate.sp.sex.keep <- intersect(wate.sp.new.f.keep, wate.sp.new.m.keep) 
wate.sp.time.keep <- intersect(wate.sp.old.f.keep, wate.sp.old.m.keep)
wate.sp.keep <- intersect(wate.sp.sex.keep, wate.sp.time.keep)

# subset sex df
wate.df.sex <- wate.df.s[which(wate.df.s$species %in% wate.sp.keep),]

# how many records and species?
nrow(wate.df.sex) #53
length(unique(wate.df.sex$species)) #1

# write filtered data frame
write.csv(wate.df.sex, file="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered_sex.csv")

