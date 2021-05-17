# library
library(tidyverse)

# load function to clean data
source("~/Dropbox/Bird_body_size-analysis/bird_body_size/scripts/00_functions.R")

# load migration data, subset
migration <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/migration.csv")
migration <- migration %>% select(c(Updated.Scientific.Name, Dataset, Migratory.Status))

# merge with raw brazil data, export
mig_b <- migration %>% filter(Dataset=="Brazil")
brazil.raw <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_pre.csv")
brazil.merged <- merge(brazil.raw, mig_b, by.x="Updated.Scientific.Name", by.y="Updated.Scientific.Name") 
write.csv(brazil.merged, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_raw.csv")

# merge with raw panama data, export
mig_p <- migration %>% filter(Dataset=="Panama")
panama.raw <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_pre.csv")
panama.merged <- merge(panama.raw, mig_p, by.x="Updated.Scientific.Name", by.y="Updated.Scientific.Name") 
write.csv(panama.merged, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_raw.csv")

# merge with raw guanica data, export
mig_g <- migration %>% filter(Dataset=="Guanica")
guanica.raw <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_pre.csv")
guanica.merged <- merge(guanica.raw, mig_g, by.x="Updated.Scientific.Name", by.y="Updated.Scientific.Name") 
write.csv(guanica.merged, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_raw.csv")

# run data cleaning function on Brazil data
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_raw.csv",  
           sex="ALL", climate = "TROPICAL", drop_juveniles = TRUE, brazil=TRUE, drop_frags = FALSE,
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered.csv")

# run function on Panama data
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_raw.csv",  
           sex="ALL", climate = "TROPICAL", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered.csv")

# run function on Guanica data
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_raw.csv",  
           sex="ALL", climate = "TROPICAL", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered.csv")

# run function on Powdermill data
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_raw.csv",  
           sex="ALL", climate = "TEMPERATE", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered.csv")

# run function on Palomarin data
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_raw.csv",  
           sex="ALL", climate = "TEMPERATE", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered.csv")

# run function on Teton Science School data
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_raw.csv",  
           sex="ALL", climate = "TEMPERATE", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered.csv")

# run function on Waterfall Glen data
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_raw.csv",  
           sex="ALL", climate = "TEMPERATE", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered.csv")

# run function on Brazil data, males only
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_raw.csv",  
           sex="MALES", climate = "TROPICAL", drop_juveniles = TRUE, brazil=TRUE, drop_frags = FALSE,
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_males.csv")

# run function on Panama data, males only
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_raw.csv",  
           sex="MALES", climate = "TROPICAL", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered_males.csv")

# run function on Guanica data, males only
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_raw.csv",  
           sex="MALES", climate = "TROPICAL", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered_males.csv")

# run function on Powdermill data, males only
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_raw.csv",  
           sex="MALES", climate = "TEMPERATE", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered_males.csv")

# run function on Palomarin data, males only
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_raw.csv",  
           sex="MALES", climate = "TEMPERATE", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered_males.csv")

# run function on Teton Science School data, males only
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_raw.csv",  
           sex="MALES", climate = "TEMPERATE", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered_males.csv")

# run function on Waterfall Glen data, males 
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_raw.csv",  
           sex="MALES", climate = "TEMPERATE", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered_males.csv")

# run function on Brazil data, females only
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_raw.csv",  
           sex="FEMALES", climate = "TROPICAL", drop_juveniles = TRUE, brazil=TRUE,
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_females.csv")

# run function on Panama data, females only
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_raw.csv",  
           sex="FEMALES", climate = "TROPICAL", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered_females.csv")

# run function on Guanica data, females only
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_raw.csv",  
           sex="FEMALES", climate = "TROPICAL", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered_females.csv")

# run function on Powdermill data, females only
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_raw.csv",  
           sex="FEMALES", climate = "TEMPERATE", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered_females.csv")

# run function on Palomarin data, females only
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_raw.csv",  
           sex="FEMALES", climate = "TEMPERATE", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered_females.csv")

# run function on Teton Science School data, females only
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_raw.csv",  
           sex="FEMALES", climate = "TEMPERATE", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered_females.csv")

# run function on Waterfall Glen data, females only
clean_data(file_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_raw.csv",  
           sex="FEMALES", climate = "TEMPERATE", drop_juveniles = TRUE, 
           output_path = "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered_females.csv")


# load sex-specific dataframes
brazil.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered.csv")
panama.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered.csv")
guanica.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered.csv")
powdermill.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered.csv")
palo.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered.csv")
tss.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered.csv")
wate.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered.csv")
brazil.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_males.csv")
panama.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered_males.csv")
guanica.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered_males.csv")
powdermill.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered_males.csv")
palo.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered_males.csv")
tss.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered_males.csv")
wate.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered_males.csv")
brazil.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_females.csv")
panama.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered_females.csv")
guanica.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered_females.csv")
powdermill.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered_females.csv")
palo.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered_females.csv")
tss.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered_females.csv")
wate.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered_females.csv")

# find shared species list
brazil_sp <- intersect(brazil.males.df$species, brazil.females.df$species)
brazil_sex <- brazil.df[brazil.df$species %in% brazil_sp,]
write.csv(brazil_sex, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_sex.csv")
panama_sp <- intersect(panama.males.df$species, panama.females.df$species)
panama_sex <- panama.df[panama.df$species %in% panama_sp,]
write.csv(panama_sex, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered_sex.csv")
guanica_sp <- intersect(guanica.males.df$species, guanica.females.df$species)
guanica_sex <- guanica.df[guanica.df$species %in% guanica_sp,]
write.csv(guanica_sex, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered_sex.csv")
powdermill_sp <- intersect(powdermill.males.df$species, powdermill.females.df$species)
powdermill_sex <- powdermill.df[powdermill.df$species %in% powdermill_sp,]
write.csv(powdermill_sex, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered_sex.csv")
palo_sp <- intersect(palo.males.df$species, palo.females.df$species)
palo_sex <- palo.df[palo.df$species %in% palo_sp,]
write.csv(palo_sex, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered_sex.csv")
tss_sp <- intersect(tss.males.df$species, tss.females.df$species)
tss_sex <- tss.df[tss.df$species %in% tss_sp,]
write.csv(tss_sex, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered_sex.csv")
wate_sp <- intersect(wate.males.df$species, wate.females.df$species)
wate_sex <- wate.df[wate.df$species %in% wate_sp,]
write.csv(wate_sex, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered_sex.csv")
