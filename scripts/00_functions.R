# function to load and clean data
clean_data <- function(file_path, output_path, drop_juveniles=FALSE, drop_frags=FALSE,
                       sex=c("MALES","FEMALES","ALL"), 
                       climate=c("TEMPERATE","TROPICAL"), 
                       migratory_status=c("ALL","NON","SHORT"),
                       brazil=FALSE,
                       s_dev=4,
                       sample_size_parameter=5){
  
  # required package
  require(lubridate)
  
  # load data
  data.full <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding="latin1") 
  
  # subset to relevant columns
  if (climate=="TROPICAL" & brazil==FALSE){data.df <- data.full[,c("Updated.Scientific.Name","Month","Year","Band.Number",
                          "Sex","Age","Mass","Wing.Length","Tarsus","Migratory.Status")]}
  if (climate=="TROPICAL" & brazil==TRUE){data.df <- data.full[,c("Updated.Scientific.Name","Month","Year","Band.Number",
                                                                   "Sex","Age","Mass","Wing.Length","Tarsus","Migratory.Status","Date","Reserve.Code")]}
  if (climate=="TEMPERATE"){data.df <- data.full[,c("Updated.Scientific.Name","Month","Year","Band.Number",
                                                   "Sex","Age","Mass","Wing.Length","Tarsus","Migratory.Status")]}
  
  # drop non-primary forest sites in Brazil
  if (brazil==TRUE & drop_frags==TRUE){
    
    # Convert date to "date" using the package 'lubridate'
    data.df$Date <- mdy(data.df$Date)
    
    # Remove all dates before 2007 and after 2017
    # data.df <- data.df %>% filter(Date >= "2007-01-01")
    # data.df <- data.df %>% filter(Date <= "2017-12-31")
    
    # Convert reserves to factor and then preserve only those that are from primary forest (below)
    data.df$Reserve.Code <- as.factor(data.df$Reserve.Code)
    
    # Create a vector of the recent primary forest reserves
    reserves <- c("37","1001","1301","1501","1511","2501","3402","3501","3512") # 9 primary forest reserves
    data.df <- data.df[data.df$Reserve.Code %in% reserves, ] 
  }
  
  # deal with factor issues
  data.df$Band.Number <- as.numeric(as.character(data.df$Band.Number))
  data.df$Year <- as.numeric(as.character(data.df$Year))
  data.df$Mass <- as.numeric(as.character(data.df$Mass))
  data.df$Wing.Length <- as.numeric(as.character(data.df$Wing.Length))
  data.df$Tarsus <- as.numeric(as.character(data.df$Tarsus))
  
  # drop NAs
  data.df <- data.df[!is.na(data.df$Mass),]
  data.df <- data.df[!data.df$Mass==0.0,]
  
  # drop juvs if juveniles=TRUE (false is default)
  if (drop_juveniles==FALSE) {data.df <- data.df}
  if (drop_juveniles==TRUE)  {data.df <- data.df[!data.df$Age=="JUV",]}

  # select sex category
  if (sex=="ALL") {data.df <- data.df} 
  if (sex=="MALES") {data.df <- data.df[data.df$Sex=="M",]}
  if (sex=="FEMALES") {data.df <- data.df[data.df$Sex=="F",]}  
  
  # select climate category
  if (climate=="TROPICAL") {data.df <- data.df[data.df$Migratory.Status=="non",]} 
  if (climate=="TEMPERATE") {data.df <- data.df[data.df$Month %in% c(6,7),]}
  
  # select migratory status
  if (migratory_status=="ALL") {data.df <- data.df} 
  if (migratory_status=="NON") {data.df <- data.df[data.df$Migratory.Status=="non",]} 
  if (migratory_status=="SHORT") {data.df <- data.df[!data.df$Migratory.Status=="long",]} 
  
  # order by month and year, drop bad data
  data.df <- data.df[order(data.df$Year, data.df$Month),]

  # generate fake band numbers for missing data
  for(i in 1:nrow(data.df)){
    tmp <- data.df$Band.Number[i]
    if(is.na(tmp)==TRUE){data.df$Band.Number[i] <- sample(1:1000000,1)}
  }
  
  # drop duplicated bands numbers
  if(sum(is.na(data.df$Band.Number))!=nrow(data.df)) {data.df <- data.df[!duplicated(data.df$Band.Number),]}

  # rename columns
  colnames(data.df) <- c("species","month","year","band_no","sex","age","mass","wing_length","tarsus", "migratory_status")
  
  # make sure all species are represented by changing to character
  data.df$species <- as.character(data.df$species)
  data.df <- data.df[!is.na(data.df$species),]
  
  # check out which years are represented
  print("here are the unique years in this dataset:")
  print(unique(data.df$year))

  # drop mass outliers for each species (> 2 sd)
  temp.df <- list()
  for(i in unique(data.df$species)){
    tmp <- data.df[data.df$species==i,]
    tmp.b <- tmp[abs(tmp$mass-mean(tmp$mass))< s_dev*sd(tmp$mass),]
    temp.df[[i]] <- tmp.b
  }
  data.df <- do.call(rbind, temp.df)
  rownames(data.df) <- NULL
  
  # create binary code for sampling period
  data.df$period <- ifelse(data.df$year <= median(data.df$year, na.rm = TRUE), "old", "new")
  
  # create mini data frames by period for filtering
  data.old <- data.df[data.df$period=="old",]
  data.new <- data.df[data.df$period=="new",]
  
  # drop old species without five individuals per time period
  data.old.species <- as.data.frame(table(data.old$species))
  colnames(data.old.species) <- c("species", "frequency")
  data.sp.old.keep <- data.old.species[which(data.old.species$frequency >= sample_size_parameter),]
  data.sp.old.keep <- as.character(data.sp.old.keep$species)
  
  # drop new species without five individuals per time period
  data.new.species <- as.data.frame(table(data.new$species))
  colnames(data.new.species) <- c("species", "frequency")
  data.sp.new.keep <- data.new.species[which(data.new.species$frequency >= sample_size_parameter),]
  data.sp.new.keep <- as.character(data.sp.new.keep$species)
  
  # find shared set, subset data frame
  data.sp.time.keep <- intersect(data.sp.old.keep, data.sp.new.keep)
  data.df.all <- data.df[which(data.df$species %in% data.sp.time.keep),]

  # how many records and species?
  print(paste("there are " , nrow(data.df.all), " records"))
  print(paste("there are ", length(unique(data.df.all$species)), " species"))

  # write filtered data frame
  write.csv(data.df.all, file=output_path)
}

make_stats_df <- function(file_path, site_name, output_path, 
                          months=c("ALL","SUMMER"),
                          climate_regression_years=c("ALL","SAMPLED", "SITE")){
  
  # load data 
  data.all <- read.csv(file = file_path, fileEncoding="latin1")[-1]
  
  # pick climate dataset
  if(months=="ALL"){temp.df <- all_temps.df}
  if(months=="ALL"){precip.df <- all_precip.df}
  if(months=="SUMMER"){temp.df <- summer_temps.df}
  if(months=="SUMMER"){precip.df <- summer_precip.df}
  
  # subset temp data, merge with df
  data.temps <- cbind.data.frame(temp.df$year, temp.df[,colnames(temp.df)==site_name])
  data.precip <- cbind.data.frame(precip.df$year, precip.df[,colnames(precip.df)==site_name])
  colnames(data.temps) <- c("year", "MAT")
  colnames(data.precip) <- c("year", "MAP")
  data.temps$MAT <- as.numeric(as.character(data.temps$MAT))
  data.precip$MAP <- as.numeric(as.character(data.precip$MAP))
  
  # merge datasets
  data.all <- merge(data.all, data.temps, by.x="year", by.y="year", all.y=FALSE)
  data.all <- merge(data.all, data.precip, by.x="year", by.y="year", all.y=FALSE)
  
  # get general climate trends
  mod.temp.gen <- lm(MAT ~ year, data.all) # simple regression for temp change
  mod.temp.sum.gen <- summary(mod.temp.gen)
  slope.temp.gen <- mod.temp.sum.gen$coefficients[2,1] # get temp slope
  r2.temp.gen <- mod.temp.sum.gen$r.squared # get temp r-squared
  min.year <- min(data.all$year)
  max.year <- max(data.all$year)
  precip.tmp.gen <- data.precip[data.precip$year %in% seq(min.year, max.year),]
  mod.precip.gen <- lm(MAP ~ year, precip.tmp.gen) # simple regression for temp change
  mod.precip.sum.gen <- summary(mod.precip.gen)
  slope.precip.gen <- mod.precip.sum.gen$coefficients[2,1] # get temp slope
  r2.precip.gen <- mod.precip.sum.gen$r.squared # get temp r-squared
  start.temp.gen <- data.all[data.all$year==min.year,]$MAT %>% mean()
  end.temp.gen <- data.all[data.all$year==max.year,]$MAT %>% mean()
  start.precip.gen <- data.all[data.all$year==min.year,]$MAP %>% mean()
  end.precip.gen <- data.all[data.all$year==max.year,]$MAP %>% mean()
  
  # determine climate trends regardless of whether species is sampled
  if(climate_regression_years=="ALL"){

    # loop to get mean mass change per species
    data.delta <- list()
    for(i in unique(data.all$species)){
      tmp <- data.all[data.all$species==i,]
      n <- nrow(tmp) # get sample size
      n.years <- length(unique(tmp$year)) # get number of years
      min.year <- min(tmp$year)
      max.year <- max(tmp$year)
      start.temp <- tmp[tmp$year==min.year,]$MAT %>% mean()
      end.temp <- tmp[tmp$year==max.year,]$MAT %>% mean()
      start.precip <- tmp[tmp$year==min.year,]$MAP %>% mean()
      end.precip <- tmp[tmp$year==max.year,]$MAP %>% mean()
      mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
      mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
      mod.mass.sum <- summary(mod.mass)
      slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
      r2.mass <- mod.mass.sum$r.squared # get mass r-squared
      se.mass <- mod.mass.sum$coefficients[2,2] # get mass SE
      temp.tmp <- data.temps[data.temps$year %in% seq(min.year, max.year),]
      mod.temp <- lm(MAT ~ year, temp.tmp) # simple regression for temp change
      mod.temp.sum <- summary(mod.temp)
      slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
      r2.temp <- mod.temp.sum$r.squared # get temp r-squared
      precip.tmp <- data.precip[data.precip$year %in% seq(min.year, max.year),]
      mod.precip <- lm(MAP ~ year, precip.tmp) # simple regression for temp change
      mod.precip.sum <- summary(mod.precip)
      slope.precip <- mod.precip.sum$coefficients[2,1] # get temp slope
      r2.precip <- mod.precip.sum$r.squared # get temp r-squared
      data.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,se.mass,slope.temp,
                                          r2.temp,slope.precip,r2.precip,mass.start,
                                          min.year,max.year,
                                          start.temp,end.temp,
                                          start.precip,end.precip)
    }
    
  }
  
  # determine climate trends based on year species is sampled
  if(climate_regression_years=="SAMPLED"){
    
    # loop to get mean mass change per species
    data.delta <- list()
    for(i in unique(data.all$species)){
      tmp <- data.all[data.all$species==i,]
      n <- nrow(tmp) # get sample size
      n.years <- length(unique(tmp$year)) # get number of years
      min.year <- min(tmp$year)
      max.year <- max(tmp$year)
      start.temp <- tmp[tmp$year==min.year,]$MAT %>% mean()
      end.temp <- tmp[tmp$year==max.year,]$MAT %>% mean()
      start.precip <- tmp[tmp$year==min.year,]$MAP %>% mean()
      end.precip <- tmp[tmp$year==max.year,]$MAP %>% mean()
      mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
      mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
      mod.mass.sum <- summary(mod.mass)
      slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
      r2.mass <- mod.mass.sum$r.squared # get mass r-squared
      se.mass <- mod.mass.sum$coefficients[2,2] # get mass SE
      mod.temp <- lm(MAT ~ year, tmp) # simple regression for temp change
      mod.temp.sum <- summary(mod.temp)
      slope.temp <- mod.temp.sum$coefficients[2,1] # get temp slope
      r2.temp <- mod.temp.sum$r.squared # get temp r-squared
      mod.precip <- lm(MAP ~ year, tmp) # simple regression for temp change
      mod.precip.sum <- summary(mod.precip)
      slope.precip <- mod.precip.sum$coefficients[2,1] # get temp slope
      r2.precip <- mod.precip.sum$r.squared # get temp r-squared
      data.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,se.mass,slope.temp,
                                          r2.temp,slope.precip,r2.precip,mass.start,
                                          min.year,max.year,
                                          start.temp,end.temp,
                                          start.precip,end.precip)
    }
    
  }
  
  # determine climate trends based only on site
  if(climate_regression_years=="SITE"){
    
    # loop to get mean mass change per species
    data.delta <- list()
    for(i in unique(data.all$species)){
      tmp <- data.all[data.all$species==i,]
      n <- nrow(tmp) # get sample size
      n.years <- length(unique(tmp$year)) # get number of years
      min.year <- min(tmp$year)
      max.year <- max(tmp$year)
      mass.start <- mean(tmp[tmp$year==min.year,]$mass) # average mass for starting year
      mod.mass <- lm(mass ~ year, tmp) # simple regression for mass change
      mod.mass.sum <- summary(mod.mass)
      slope.mass <- mod.mass.sum$coefficients[2,1] # get mass slope
      r2.mass <- mod.mass.sum$r.squared # get mass r-squared
      se.mass <- mod.mass.sum$coefficients[2,2] # get mass SE
      data.delta[[i]] <- cbind.data.frame(i,n,n.years,slope.mass,r2.mass,se.mass,slope.temp.gen,
                                          r2.temp.gen,slope.precip.gen,r2.precip.gen,mass.start,
                                          min.year,max.year,
                                          start.temp.gen,end.temp.gen,
                                          start.precip.gen,end.precip.gen)
    }
    
  }
  
  # assemble dataframe
  data.mass.df <- do.call(rbind, data.delta)
  rownames(data.mass.df) <- NULL
  colnames(data.mass.df) <- c("species","sample_size","no_years","slope_mass","variance_mass","se_mass","slope_temp", "variance_temp",
                              "slope_precip", "variance_precip","starting_mass","starting_year","ending_year",
                              "starting_temp","ending_temp","starting_precip","ending_precip")
  data.mass.df$lat <- rep(latlong.df[latlong.df$site==site_name,]$lat, nrow(data.mass.df))
  data.mass.df$long <- rep(latlong.df[latlong.df$site==site_name,]$long, nrow(data.mass.df))
  
  write.csv(data.mass.df, file=output_path)
}

# function to match taxonomy to Jetz
clean_taxonomy <- function(file_path, output_path){
  
  # load data
  input_obj <- read.csv(file_path)
  
  # switch factor to character
  input_obj$species <- as.character(input_obj$species) 
  
  # fix taxonomy
  if("Dixiphia_pipra" %in% input_obj$species==TRUE) {input_obj[grep("Dixiphia_pipra",input_obj$species),]$species <-"Pipra_pipra"} 
  if("Ceratopipra_erythrocephala" %in% input_obj$species==TRUE) {input_obj[grep("Ceratopipra_erythrocephala",input_obj$species),]$species <-"Pipra_erythrocephala"}
  if("Percnostola_leucostigma" %in% input_obj$species==TRUE) {input_obj[grep("Percnostola_leucostigma",input_obj$species),]$species <-"Schistocichla_leucostigma"}
  if("Ceratopipra_mentalis" %in% input_obj$species==TRUE) {input_obj[grep("Ceratopipra_mentalis",input_obj$species),]$species <-"Pipra_mentalis"}
  if("Geothlypis_formosa" %in% input_obj$species==TRUE) {input_obj[grep("Geothlypis_formosa",input_obj$species),]$species <-"Oporornis_formosus"}
  if("Sporophila_funerea" %in% input_obj$species==TRUE) {input_obj[grep("Sporophila_funerea",input_obj$species),]$species <-"Oryzoborus_funereus"}
  if("Setophaga_discolor" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_discolor",input_obj$species),]$species <-"Dendroica_discolor" }
  if("Setophaga_adelaidae" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_adelaidae",input_obj$species),]$species <-"Dendroica_adelaidae"}
  if("Setophaga_americana" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_americana",input_obj$species),]$species <-"Parula_americana"}
  if("Turdus_plumbeus" %in% input_obj$species==TRUE) {input_obj[grep("Turdus_plumbeus",input_obj$species),]$species <-"Turdus_plumbeus"}
  if("Zonotrichia_l. nuttalli" %in% input_obj$species==TRUE) {input_obj[grep("Zonotrichia_l. nuttalli",input_obj$species),]$species <-"Zonotrichia_leucophrys" }
  if("Haemorhous_mexicanus" %in% input_obj$species==TRUE) {input_obj[grep("Haemorhous_mexicanus",input_obj$species),]$species <-"Carpodacus_mexicanus"}
  if("Setophaga_townsendi" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_townsendi",input_obj$species),]$species <-"Dendroica_townsendi"}
  if("Junco_h. oregonus" %in% input_obj$species==TRUE) {input_obj[grep("Junco_h. oregonus",input_obj$species),]$species <-"Junco_hyemalis"}
  if("Setophaga_petechia" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_petechia",input_obj$species),]$species <-"Junco_hyemalis"}
  if("Spinus_pinus" %in% input_obj$species==TRUE) {input_obj[grep("Spinus_pinus",input_obj$species),]$species <-"Dendroica_petechia"}
  if("Ixoreus_naevius" %in% input_obj$species==TRUE) {input_obj[grep("Ixoreus_naevius",input_obj$species),]$species <-"Zoothera_naevia"}
  if("Cardellina_pusilla" %in% input_obj$species==TRUE) {input_obj[grep("Cardellina_pusilla",input_obj$species),]$species <-"Wilsonia_pusilla"}
  if("Spinus_tristis" %in% input_obj$species==TRUE) {input_obj[grep("Spinus_tristis",input_obj$species),]$species <-"Oreothlypis_celata"}
  if("Oreothlypis_celata" %in% input_obj$species==TRUE) {input_obj[grep("Oreothlypis_celata",input_obj$species),]$species <-"Vermivora_celata"}
  if("Colaptes_a. cafer" %in% input_obj$species==TRUE) {input_obj[grep("Colaptes_a. cafer",input_obj$species),]$species <-"Colaptes_auratus"}
  if("Haemorhous_purpureus" %in% input_obj$species==TRUE) {input_obj[grep("Haemorhous_purpureus",input_obj$species),]$species <-"Carpodacus_purpureus"}
  if("Poecile_rufescens" %in% input_obj$species==TRUE) {input_obj[grep("Poecile_rufescens",input_obj$species),]$species <-"Parus_rufescens"}
  if("Geothlypis_tolmiei" %in% input_obj$species==TRUE) {input_obj[grep("Geothlypis_tolmiei",input_obj$species),]$species <-"Oporornis_tolmiei"}
  if("Setophaga_coronata coronata" %in% input_obj$species==TRUE){input_obj[grep("Setophaga_coronata coronata",input_obj$species),]$species <-"Dendroica_coronata"}
  if("Troglodytes_pacificus" %in% input_obj$species==TRUE) {input_obj[grep("Troglodytes_pacificus",input_obj$species),]$species <-"Troglodytes_troglodytes"}
  if("Setophaga_coronata" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_coronata",input_obj$species),]$species <-"Dendroica_coronata"}
  if("Setophaga_magnolia" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_magnolia",input_obj$species),]$species <-"Dendroica_magnolia"}
  if("Colaptes_a. auratus" %in% input_obj$species==TRUE) {input_obj[grep("Colaptes_a. auratus",input_obj$species),]$species <-"Colaptes_auratus"}
  if("Setophaga_pensylvanica" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_pensylvanica",input_obj$species),]$species <-"Dendroica_pensylvanica"}
  if("Parkesia_motacilla" %in% input_obj$species==TRUE) {input_obj[grep("Parkesia_motacilla",input_obj$species),]$species <-"Seiurus_motacilla"}
  if("Sphyrapicus_varius" %in% input_obj$species==TRUE) {input_obj[grep("Sphyrapicus_varius",input_obj$species),]$species <-"Sphyrapicus_varius"}
  if("Cardellina_canadensis" %in% input_obj$species==TRUE) {input_obj[grep("Cardellina_canadensis",input_obj$species),]$species <-"Wilsonia_canadensis"}
  if("Sitta_carolinensis" %in% input_obj$species==TRUE) {input_obj[grep("Sitta_carolinensis",input_obj$species),]$species <-"Sitta_carolinensis"}
  if("Setophaga_striata" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_striata",input_obj$species),]$species <-"Dendroica_striata"}
  if("Setophaga_tigrina" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_tigrina",input_obj$species),]$species <-"Dendroica_tigrina"}
  if("Geothlypis_philadelphia" %in% input_obj$species==TRUE) {input_obj[grep("Geothlypis_philadelphia",input_obj$species),]$species <-"Oporornis_philadelphia"}
  if("Oreothlypis_peregrina" %in% input_obj$species==TRUE) {input_obj[grep("Oreothlypis_peregrina",input_obj$species),]$species <-"Vermivora_peregrina"}
  if("Poecile_atricapillus" %in% input_obj$species==TRUE) {input_obj[grep("Poecile_atricapillus",input_obj$species),]$species <-"Parus_atricapillus"}
  if("Oreothlypis_ruficapilla" %in% input_obj$species==TRUE) {input_obj[grep("Oreothlypis_ruficapilla",input_obj$species),]$species <-"Vermivora_ruficapilla"}
  if("Setophaga_virens" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_virens",input_obj$species),]$species <-"Dendroica_virens"}
  if("Setophaga_citrina" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_citrina",input_obj$species),]$species <-"Wilsonia_citrina"}
  if("Setophaga_palmarum palmarum" %in% input_obj$species==TRUE){input_obj[grep("Setophaga_palmarum palmarum",input_obj$species),]$species <-"Dendroica_palmarum"}
  if("Setophaga_caerulescens" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_caerulescens",input_obj$species),]$species <-"Dendroica_caerulescens"}
  if("Setophaga_fusca" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_fusca",input_obj$species),]$species <-"Dendroica_fusca"}
  if("Setophaga_castanea" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_castanea",input_obj$species),]$species <-"Dendroica_castanea"}
  if("Vermivora_cyanoptera" %in% input_obj$species==TRUE) {input_obj[grep("Vermivora_cyanoptera",input_obj$species),]$species <-"Vermivora_pinus"}
  if("Picoides_villosus" %in% input_obj$species==TRUE) {input_obj[grep("Picoides_villosus",input_obj$species),]$species <-"Picoides_villosus"}
  if("Piranga_olivacea" %in% input_obj$species==TRUE) {input_obj[grep("Piranga_olivacea",input_obj$species),]$species <-"Piranga_olivacea"}
  if("Pheucticus_ludovicianus" %in% input_obj$species==TRUE) {input_obj[grep("Pheucticus_ludovicianus",input_obj$species),]$species <-"Pheucticus_ludovicianus"}
  if("Toxostoma_rufum" %in% input_obj$species==TRUE) {input_obj[grep("Toxostoma_rufum",input_obj$species),]$species <-"Toxostoma_rufum"}
  if("Sturnus_vulgaris" %in% input_obj$species==TRUE) {input_obj[grep("Sturnus_vulgaris",input_obj$species),]$species <-"Sturnus_vulgaris"}
  if("Sialia_sialis" %in% input_obj$species==TRUE) {input_obj[grep("Sialia_sialis",input_obj$species),]$species <-"Sialia_sialis"}
  if("Icterus_galbula" %in% input_obj$species==TRUE) {input_obj[grep("Icterus_galbula",input_obj$species),]$species <-"Icterus_galbula"}
  if("Gymnopithys_bicolor" %in% input_obj$species==TRUE) {input_obj[grep("Gymnopithys_bicolor",input_obj$species),]$species <-"Gymnopithys_leucaspis"}
  if("Schiffornis_stenorhyncha" %in% input_obj$species==TRUE) {input_obj[grep("Schiffornis_stenorhyncha",input_obj$species),]$species <-"Schiffornis_turdina"}
  if("Parkesia_noveboracensis" %in% input_obj$species==TRUE) {input_obj[grep("Parkesia_noveboracensis",input_obj$species),]$species <-"Seiurus_noveboracensis"}
  if("Microbates_cenereiventris" %in% input_obj$species==TRUE) {input_obj[grep("Microbates_cenereiventris",input_obj$species),]$species <-"Microbates_cinereiventris"}
  if("Eucomitis_penicillata" %in% input_obj$species==TRUE) {input_obj[grep("Eucomitis_penicillata",input_obj$species),]$species <- "Eucometis_penicillata"}
  if("Cercomacroides_tyrannina" %in% input_obj$species==TRUE) {input_obj[grep("Cercomacroides_tyrannina",input_obj$species),]$species <-"Cercomacra_tyrannina"}
  if("Vireo_latimeri" %in% input_obj$species==TRUE) {input_obj[grep("Vireo_latimeri",input_obj$species),]$species <-"Vireo_latimeri"}
  if("Geotrygon_chrysia" %in% input_obj$species==TRUE) {input_obj[grep("Geotrygon_chrysia",input_obj$species),]$species <-"Geotrygon_chrysia" }
  if("Zonotrichia_l. pugetensis" %in% input_obj$species==TRUE) {input_obj[grep("Zonotrichia_l. pugetensis",input_obj$species),]$species <-"Zonotrichia_leucophrys"}
  if("Melozone_crissalis" %in% input_obj$species==TRUE) {input_obj[grep("Melozone_crissalis",input_obj$species),]$species <-"Pipilo_crissalis"}
  if("Setophaga_occidentalis" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_occidentalis",input_obj$species),]$species <-"Dendroica_occidentalis"}
  if("Setophaga_nigrescens" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_nigrescens",input_obj$species),]$species <-"Dendroica_nigrescens"}
  if("Catharus_guttatus" %in% input_obj$species==TRUE) {input_obj[grep("Catharus_guttatus",input_obj$species),]$species <-"Catharus_guttatus"}
  if("Vermivora_chrysoptera" %in% input_obj$species==TRUE) {input_obj[grep("Vermivora_chrysoptera",input_obj$species),]$species <-"Vermivora_chrysoptera"}
  if("Troglodytes_hiemalis" %in% input_obj$species==TRUE) {input_obj[grep("Troglodytes_hiemalis",input_obj$species),]$species <-"Troglodytes_troglodytes"}
  if("Hirundo_rustica" %in% input_obj$species==TRUE) {input_obj[grep("Hirundo_rustica",input_obj$species),]$species <-"Hirundo_rustica"}
  if("Stelgidopteryx_serripennis" %in% input_obj$species==TRUE) {input_obj[grep("Stelgidopteryx_serripennis",input_obj$species),]$species <-"Stelgidopteryx_serripennis"}
  if("Spizelloides_arborea" %in% input_obj$species==TRUE) {input_obj[grep("Spizelloides_arborea",input_obj$species),]$species <-"Spizella_arborea"}
  if("Empidonax_alnorum/traillii" %in% input_obj$species==TRUE) {input_obj[grep("Empidonax_alnorum/traillii",input_obj$species),]$species <-"Empidonax_alnorum"}
  if("Tringa_solitaria" %in% input_obj$species==TRUE) {input_obj[grep("Tringa_solitaria",input_obj$species),]$species <-"Tringa_solitaria"}
  if("Leptotila_cassinii" %in% input_obj$species==TRUE) {input_obj[grep("Leptotila_cassinii",input_obj$species),]$species <-"Leptotila_cassini"}
  if("Certhiasomus_stictolaemus" %in% input_obj$species==TRUE) {input_obj[grep("Certhiasomus_stictolaemus",input_obj$species),]$species <-"Deconychura_stictolaema"}
  if("Clibarnornis_rubiginosus" %in% input_obj$species==TRUE) {input_obj[grep("Clibarnornis_rubiginosus",input_obj$species),]$species <-"Automolus_rubiginosus"}
  if("Philydor_erythrocercus" %in% input_obj$species==TRUE) {input_obj[grep("Philydor_erythrocercus",input_obj$species),]$species <-"Philydor_erythrocercum"}
  if("Tunchiornis_ochraceiceps" %in% input_obj$species==TRUE) {input_obj[grep("Tunchiornis_ochraceiceps",input_obj$species),]$species <-"Hylophilus_ochraceiceps"}
  if("Myiothlypis_rivularis" %in% input_obj$species==TRUE) {input_obj[grep("Myiothlypis_rivularis",input_obj$species),]$species <-"Phaeothlypis_rivularis"}
  if("Zonotrichia_l. gambelii" %in% input_obj$species==TRUE) {input_obj[grep("Zonotrichia_l. gambelii",input_obj$species),]$species <-"Zonotrichia_leucophrys" }
  if("Spinus_psaltria" %in% input_obj$species==TRUE) {input_obj[grep("Spinus_psaltria",input_obj$species),]$species <-"Carduelis_psaltria" }
  if("Poecile_gambeli" %in% input_obj$species==TRUE) {input_obj[grep("Poecile_gambeli",input_obj$species),]$species <-"Parus_gambeli" }
  if("Poecile_carolinensis" %in% input_obj$species==TRUE) {input_obj[grep("Poecile_carolinensis",input_obj$species),]$species <-"Parus_carolinensis" }
  if("Setophaga_cerulea" %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_cerulea",input_obj$species),]$species <-"Dendroica_cerulea" }
  if("Setophaga_palmarum hypochrysea " %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_palmarum hypochrysea ",input_obj$species),]$species <-"Dendroica_palmarum" }
  if("Setophaga_pinus " %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_pinus ",input_obj$species),]$species <-"Dendroica_pinus" }
  if("Contopus_cooperi" %in% input_obj$species==TRUE) {input_obj[grep("Contopus_cooperi",input_obj$species),]$species <-"Contopus_cooperi" }
  if("Turdus_plumbeus " %in% input_obj$species==TRUE) {input_obj[grep("Turdus_plumbeus ",input_obj$species),]$species <-"Turdus_plumbeus" }
  if("Vireo_latimeri " %in% input_obj$species==TRUE) {input_obj[grep("Vireo_latimeri ",input_obj$species),]$species <-"Vireo_latimeri" }
  if("Coereba_flaveola " %in% input_obj$species==TRUE) {input_obj[grep("Coereba_flaveola ",input_obj$species),]$species <-"Coereba_flaveola" }
  if("Geotrygon_chrysia " %in% input_obj$species==TRUE) {input_obj[grep("Geotrygon_chrysia ",input_obj$species),]$species <-"Geotrygon_chrysia" }
  if("Haemorhous_cassinii" %in% input_obj$species==TRUE) {input_obj[grep("Haemorhous_cassinii",input_obj$species),]$species <-"Carpodacus_cassinii" }
  if("Sitta_carolinensis " %in% input_obj$species==TRUE) {input_obj[grep("Sitta_carolinensis ",input_obj$species),]$species <-"Sitta_carolinensis" }
  if("Toxostoma_rufum " %in% input_obj$species==TRUE) {input_obj[grep("Toxostoma_rufum ",input_obj$species),]$species <-"Toxostoma_rufum" }
  if("Vermivora_chrysoptera " %in% input_obj$species==TRUE) {input_obj[grep("Vermivora_chrysoptera ",input_obj$species),]$species <-"Vermivora_chrysoptera" }
  if("Icterus_galbula " %in% input_obj$species==TRUE) {input_obj[grep("Icterus_galbula ",input_obj$species),]$species <-"Icterus_galbula" }
  if("Piranga_olivacea " %in% input_obj$species==TRUE) {input_obj[grep("Piranga_olivacea ",input_obj$species),]$species <-"Piranga_olivacea" }
  if("Sialia_sialis " %in% input_obj$species==TRUE) {input_obj[grep("Sialia_sialis ",input_obj$species),]$species <-"Sialia_sialis" }
  if("Pheucticus_ludovicianus " %in% input_obj$species==TRUE) {input_obj[grep("Pheucticus_ludovicianus ",input_obj$species),]$species <-"Pheucticus_ludovicianus" }
  if("Tringa_solitaria " %in% input_obj$species==TRUE) {input_obj[grep("Tringa_solitaria ",input_obj$species),]$species <-"Tringa_solitaria" }
  if("Setophaga_caerulescens " %in% input_obj$species==TRUE) {input_obj[grep("Setophaga_caerulescens ",input_obj$species),]$species <-"Dendroica_caerulescens" }
  if("Stelgidopteryx_serripennis " %in% input_obj$species==TRUE) {input_obj[grep("Stelgidopteryx_serripennis ",input_obj$species),]$species <-"Stelgidopteryx_serripennis" }
  if("Selaporus_(sp)"  %in% input_obj$species==TRUE) {input_obj[grep("Selaporus_(sp)",input_obj$species),]$species <-"Selasphorus_rufus" }
  if("Selaphorus_(sp)"  %in% input_obj$species==TRUE) {input_obj[grep("Selaphorus",input_obj$species),]$species <-"Selasphorus_rufus" }
  if("Spindalis_portoricensis " %in% input_obj$species==TRUE) {input_obj[grep("Spindalis_portoricensis ",input_obj$species),]$species <-"Spindalis_portoricensis" }
  if("Coereba_flaveola " %in% input_obj$species==TRUE) {input_obj[grep("Coereba_flaveola ",input_obj$species),]$species <-"Coereba_flaveola" }
  if("Picoides_villosus " %in% input_obj$species==TRUE) {input_obj[grep("Picoides_villosus",input_obj$species),]$species <-"Picoides_villosus" }
  if("Hirundo_rustica " %in% input_obj$species==TRUE) {input_obj[grep("Hirundo_rustica",input_obj$species),]$species <-"Hirundo_rustica" }
  if("Colaptes_a._cafer" %in% input_obj$species==TRUE) {input_obj[grep("Colaptes_a._cafer",input_obj$species),]$species <-"Colaptes_auratus" }
  if("Zonotrichia_l._nuttalli" %in% input_obj$species==TRUE) {input_obj[grep("Zonotrichia_l._nuttalli",input_obj$species),]$species <-"Zonotrichia_leucophrys" }
  if("Junco_h._oregonus" %in% input_obj$species==TRUE) {input_obj[grep("Junco_h._oregonus",input_obj$species),]$species <-"Junco_hyemalis" }
  if("Vermivora_chrysoptera x cyanoptera" %in% input_obj$species==TRUE) {input_obj[grep("Vermivora_chrysoptera x cyanoptera",input_obj$species),]$species <-"Vermivora_chrysoptera" }
  
  # write to file
  write.csv(input_obj, output_path)
}

# function to calculate temperature change for each site across sampled years for each species
temp_change <- function(file_path, site_name, months=c("ALL","SUMMER"), output_path){
  
  # required packages
  require(stringr, tidyverse)
  
  # load temp data
  temp.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/temp_data.csv")
  regexp <- "[[:digit:]]+" #regex to extract year from column names
  colnames(temp.df) <- c("site", str_extract(colnames(temp.df), regexp)[-1]) #rename columns
  site <- as.vector(as.character(temp.df$site))
  
  # calculate annual mean
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
  
  if(months=="SUMMER"){
    summer_temps.df <- temp.df[grep("\\d+\\.6|\\d+\\.7",temp.df$year),] # regex extracts months 6 and 7
    summer_temps.df$year <- str_extract(summer_temps.df$year, regexp) #rename columns
    summer_temps.df$year <- as.numeric(summer_temps.df$year)
    summer_temps.df <- summer_temps.df %>% mutate_if(is.character,as.numeric)
    summer_temps.df <- summer_temps.df %>%
      group_by(year) %>% 
      summarise_at(vars("Powdermill","Teton","Waterfall",
                        "Panama","Brazil","Puerto Rico","Palomarin","Guanica"), mean)
    temp.df <- summer_temps.df
  }
  
  
  if(months=="ALL"){
    all_temps.df <- temp.df
    all_temps.df$year <- str_extract(all_temps.df$year, regexp) #rename columns
    all_temps.df$year <- as.numeric(all_temps.df$year)
    all_temps.df <- all_temps.df %>% mutate_if(is.character,as.numeric)
    all_temps.df <- all_temps.df %>%
      group_by(year) %>% 
      summarise_at(vars("Powdermill","Teton","Waterfall",
                      "Panama","Brazil","Puerto Rico","Palomarin","Guanica"), mean)
    temp.df <- all_temps.df
  }

  # load data 
  data.all <- read.csv(file = file_path, fileEncoding="latin1")[-1]
  
  # subset temp and precip data, merge with df
  data.temps <- cbind.data.frame(temp.df$year, temp.df[,colnames(temp.df)==site_name])
  colnames(data.temps) <- c("year", "MAT")
  data.temps$MAT <- as.numeric(as.character(data.temps$MAT))
  data.all <- merge(data.all, data.temps, by.x="year", by.y="year", all.y=FALSE)
  
  # export file
  write.csv(data.all, output_path)
  
}

# function to calculate precip change for each site across sampled years for each species
precip_change <- function(file_path, site_name, months=c("ALL","SUMMER"), output_path){
  
  # required packages
  require(stringr, tidyverse)
  
  # load precip data
  precip.df <- read.csv("~/Dropbox/Bird_body_size-analysis/bird_body_size/data/precip_data.csv")
  regexp <- "[[:digit:]]+" #regex to extract year from column names
  colnames(precip.df) <- c("site", str_extract(colnames(precip.df), regexp)[-1]) #rename columns
  site <- as.vector(as.character(precip.df$site))
  
  # calculate annual mean
  precip.df <- subset(precip.df, select = -c(site))
  x <- as.data.frame(precip.df)
  precip.df <- as.data.frame(lapply(split(as.list(x),f = colnames(x)),
                                    function(x) Reduce(`+`,x) / length(x)))
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
  
  if(months=="SUMMER"){
    summer_precip.df <- precip.df[grep("\\d+\\.6|\\d+\\.7",precip.df$year),] # regex extracts months 6 and 7
    summer_precip.df$year <- str_extract(summer_precip.df$year, regexp) #rename columns
    summer_precip.df$year <- as.numeric(summer_precip.df$year)
    summer_precip.df <- summer_precip.df %>% mutate_if(is.character,as.numeric)
    summer_precip.df <- summer_precip.df %>%
      group_by(year) %>% 
      summarise_at(vars("Powdermill","Teton","Waterfall",
                        "Panama","Brazil","Puerto Rico","Palomarin","Guanica"), mean)
    precip.df <- summer_precip.df
  }
  
  if(months=="ALL"){
    all_precip.df <- precip.df
    all_precip.df$year <- str_extract(all_precip.df$year, regexp) #rename columns
    all_precip.df$year <- as.numeric(all_precip.df$year)
    all_precip.df <- all_precip.df %>% mutate_if(is.character,as.numeric)
    all_precip.df <- all_precip.df %>%
      group_by(year) %>% 
      summarise_at(vars("Powdermill","Teton","Waterfall",
                        "Panama","Brazil","Puerto Rico","Palomarin","Guanica"), mean)
    precip.df <- all_precip.df
  }
  
  # load data 
  data.all <- read.csv(file = file_path, fileEncoding="latin1")[-1]
  
  # subset precip and precip data, merge with df
  data.precips <- cbind.data.frame(precip.df$year, precip.df[,colnames(precip.df)==site_name])
  colnames(data.precips) <- c("year", "MAP")
  data.precips$MAP <- as.numeric(as.character(data.precips$MAP))
  data.all <- merge(data.all, data.precips, by.x="year", by.y="year", all.y=FALSE)
  
  # export file
  write.csv(data.all, output_path)
  
}
