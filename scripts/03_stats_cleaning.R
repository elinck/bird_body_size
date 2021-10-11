library(ape)
library(phangorn)
library(stringr)
library(tidyverse)
source("~/Dropbox/Bird_body_size-analysis/bird_body_size/scripts/00_functions.R")

# load temp data
temp.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/temp_data.csv")
regexp <- "[[:digit:]]+" #regex to extract year from column names
colnames(temp.df) <- c("site", str_extract(colnames(temp.df), regexp)[-1]) #rename columns
site <- as.vector(as.character(temp.df$site))

# load precip data
precip.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/precip_data.csv")
regexp <- "[[:digit:]]+" #regex to extract year from column names
colnames(precip.df) <- c("site",str_extract(colnames(precip.df), regexp)[-1]) #rename columns
site <- as.vector(as.character(precip.df$site))

# load lat long data
latlong.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/latlong.csv")

# reformat
temp.df <- subset(temp.df, select = -c(site))
x <- as.data.frame(temp.df)
temp.df <- as.data.frame(lapply(split(as.list(x),f = colnames(x)),
                                function(x) Reduce(`+`,x) / length(x)))
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

# june / july only
summer_temps.df <- temp.df[grep("\\d+\\.6|\\d+\\.7",temp.df$year),] # regex extracts months 6 and 7
summer_temps.df$year <- str_extract(summer_temps.df$year, regexp) #rename columns
summer_temps.df$year <- as.numeric(summer_temps.df$year)
summer_temps.df <- summer_temps.df %>% mutate_if(is.character,as.numeric)
summer_temps.df <- summer_temps.df %>%
  group_by(year) %>% 
  summarise_at(vars("Powdermill","Teton","Waterfall",
                    "Panama","Brazil","Puerto Rico","Palomarin","Guanica"), mean)

# tropics (all months)
all_temps.df <- temp.df
all_temps.df$year <- str_extract(all_temps.df$year, regexp) #rename columns
all_temps.df$year <- as.numeric(all_temps.df$year)
all_temps.df <- all_temps.df %>% mutate_if(is.character,as.numeric)
all_temps.df <- all_temps.df %>%
  group_by(year) %>% 
  summarise_at(vars("Powdermill","Teton","Waterfall",
                    "Panama","Brazil","Puerto Rico","Palomarin","Guanica"), mean)

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

# temperate precip (june / july only)
summer_precip.df <- precip.df[grep("\\d+\\.6|\\d+\\.7",precip.df$year),] # regex extracts months 6 and 7
summer_precip.df$year <- str_extract(summer_precip.df$year, regexp) #rename columns
summer_precip.df$year <- as.numeric(summer_precip.df$year)
summer_precip.df <- summer_precip.df %>% mutate_if(is.character,as.numeric)
summer_precip.df <- summer_precip.df %>%
  group_by(year) %>% 
  summarise_at(vars("Powdermill","Teton","Waterfall",
                    "Panama","Brazil","Puerto Rico","Palomarin","Guanica"), mean)

# tropics precip precip (all months)
all_precip.df <- precip.df
all_precip.df$year <- str_extract(all_precip.df$year, regexp) #rename columns
all_precip.df$year <- as.numeric(all_precip.df$year)
all_precip.df <- all_precip.df %>% mutate_if(is.character,as.numeric)
all_precip.df <- all_precip.df %>%
  group_by(year) %>% 
  summarise_at(vars("Powdermill","Teton","Waterfall",
                    "Panama","Brazil","Puerto Rico","Palomarin","Guanica"), mean)

# write climate data to visualize broad trends
write.csv(all_temps.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_temps.csv")
write.csv(all_precip.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_precip.csv")
write.csv(summer_temps.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/summer_temps.csv")
write.csv(summer_precip.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/summer_precip.csv")

# load phylogenies
# supertree <-read.tree("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/AllBirdsHackett1.tre")
# find maximum clade credibility tree
# supertree <- maxCladeCred(supertree)
# write.tree(supertree, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/birds_mcc.tre")

# load MCC tree
supertree <- read.tree("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/birds_mcc.tre")
supertree.species <- supertree$tip.label # save vector of species names

# run dataframe function for Brazil, sex-blind
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered.csv", site_name="Brazil", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_stats_all.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Panama, sex-blind
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered.csv", site_name="Panama", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_stats_all.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Guanica, sex-blind
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered.csv", site_name="Guanica", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_stats_all.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Powdermill, sex-blind
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered.csv", site_name="Powdermill", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_stats_all.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Palomarin, sex-blind
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered.csv", site_name="Palomarin", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_stats_all.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for TSS, sex-blind
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered.csv", site_name="Teton", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_stats_all.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Waterfall Glen, sex-blind
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered.csv", site_name="Waterfall", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_stats_all.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Brazil, males
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_males.csv", site_name="Brazil", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_stats_males.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Panama, males
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered_males.csv", site_name="Panama", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_stats_males.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Guanica, males
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered_males.csv", site_name="Guanica", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_stats_males.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Powdermill, males
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered_males.csv", site_name="Powdermill", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_stats_males.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Palomarin, males
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered_males.csv", site_name="Palomarin", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_stats_males.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for TSS, males
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered_males.csv", site_name="Teton", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_stats_males.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Waterfall Glen, males
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered_males.csv", site_name="Waterfall", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_stats_males.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Brazil, females
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_filtered_females.csv", site_name="Brazil", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_stats_females.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Panama, females
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_filtered_females.csv", site_name="Panama", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_stats_females.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Guanica, females
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_filtered_females.csv", site_name="Guanica", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_stats_females.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Powdermill, females
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_filtered_females.csv", site_name="Powdermill", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_stats_females.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Palomarin, females
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_filtered_females.csv", site_name="Palomarin", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_stats_females.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for TSS, females
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_filtered_females.csv", site_name="Teton", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_stats_females.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# run dataframe function for Waterfall Glen, females
make_stats_df(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_filtered_females.csv", site_name="Waterfall", output_path = 
                "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_stats_females.csv",
              months = "SUMMER",
              climate_regression_years = "SAMPLED")

# load data frames
brazil.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_stats_all.csv")
panama.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_stats_all.csv")
guanica.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_stats_all.csv")
powdermill.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_stats_all.csv")
palo.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_stats_all.csv")
tss.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_stats_all.csv")
wate.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_stats_all.csv")
brazil.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_stats_males.csv")
panama.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_stats_males.csv")
guanica.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_stats_males.csv")
powdermill.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_stats_males.csv")
palo.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_stats_males.csv")
tss.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_stats_males.csv")
wate.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_stats_males.csv")
brazil.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/brazil_stats_females.csv")
panama.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/panama_stats_females.csv")
guanica.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/guanica_stats_females.csv")
powdermill.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_stats_females.csv")
palo.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_stats_females.csv")
tss.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/tss_stats_females.csv")
wate.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/wate_stats_females.csv")

# merge sex-blind dataframes
master.df <- rbind.data.frame(brazil.df, panama.df, guanica.df, palo.df, tss.df, powdermill.df, wate.df)
species.count <- as.data.frame(table(master.df$species)) # get table of records

# merge males dataframes
master.males.df <- rbind.data.frame(brazil.males.df, panama.males.df, guanica.males.df, palo.males.df, tss.males.df, powdermill.males.df, wate.males.df)
species.count.males <- as.data.frame(table(master.males.df$species)) # get table of records

# merge female dataframes
master.females.df <- rbind.data.frame(brazil.females.df, panama.females.df, guanica.females.df, palo.females.df, tss.females.df, powdermill.females.df, wate.females.df)
species.count.females <- as.data.frame(table(master.females.df$species)) # get table of records

# clean ids
master.df$species <- sub(" ", "_", master.df$species) # replace space with underscore
master.males.df$species <- sub(" ", "_", master.males.df$species) # replace space with underscore
master.females.df$species <- sub(" ", "_", master.females.df$species) # replace space with underscore
powdermill.df$species <- sub(" ", "_", powdermill.df$species) # replace space with underscore
palo.df$species <- sub(" ", "_", palo.df$species) # replace space with underscore

# write temp dfs
write.csv(master.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis_df.csv")
write.csv(master.males.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/m_analysis_df.csv")
write.csv(master.females.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/f_analysis_df.csv")
write.csv(powdermill.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_analysis_df.csv")
write.csv(palo.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_analysis_df.csv") 

# clean up taxonomy (see 00_functions.R for details)
clean_taxonomy(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis_df.csv",
               output_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis_df.csv")
clean_taxonomy(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/m_analysis_df.csv",
               output_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/m_analysis_df.csv")
clean_taxonomy(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/f_analysis_df.csv",
               output_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/f_analysis_df.csv")
clean_taxonomy(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_analysis_df.csv",
               output_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_analysis_df.csv")
clean_taxonomy(file_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_analysis_df.csv",
               output_path="~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_analysis_df.csv")

# reload edited dfs
master.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis_df.csv",stringsAsFactors = FALSE)
master.males.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/m_analysis_df.csv",stringsAsFactors = FALSE)
master.females.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/f_analysis_df.csv",stringsAsFactors = FALSE)
powdermill.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_analysis_df.csv",stringsAsFactors = FALSE)
palo.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_analysis_df.csv",stringsAsFactors = FALSE)

# drop species in more than one site from all subsequent sites after the first
# master.df <- master.df %>% group_by(species) %>% filter(row_number() == 1)
# master.males.df <- master.males.df %>% group_by(species) %>% filter(row_number() == 1)
# master.females.df <- master.females.df %>% group_by(species) %>% filter(row_number() == 1)
# powdermill.df <- powdermill.df %>% group_by(species) %>% filter(row_number() == 1)
# palo.df <- palo.df %>% group_by(species) %>% filter(row_number() == 1)

# rewrite to csv
write.csv(master.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis_df.csv")
write.csv(master.males.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/m_analysis_df.csv")
write.csv(master.females.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/f_analysis_df.csv")
write.csv(powdermill.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/powdermill_analysis_df.csv")
write.csv(palo.df, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_analysis_df.csv")

# keep only tips in dataset
master.tree <- keep.tip(supertree, master.df$species)
write.tree(master.tree, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/all_analysis_phy.tree")
master.males.tree <- keep.tip(supertree, master.males.df$species)
write.tree(master.males.tree, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/m_analysis_phy.tree")
master.females.tree <- keep.tip(supertree, master.females.df$species)
write.tree(master.females.tree, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/f_analysis_phy.tree")
powdermill.tree <- keep.tip(supertree, powdermill.df$species)
write.tree(powdermill.tree, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/p_analysis_phy.tree")
palo.tree <- keep.tip(supertree, palo.df$species)
write.tree(palo.tree, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/palo_analysis_phy.tree")
sex.species <- union(master.males.df$species, master.females.df$species)
sex.tree <- keep.tip(supertree, sex.species)
write.tree(sex.tree, "~/Dropbox/Bird_body_size-analysis/bird_body_size/data/sex_analysis_phy.tree")
