########################################################################################
################################# AGGRA merge data  ####################################
########################################################################################

# objective: Merge fish data for all years and export as csv  
# Author: Jack Johnson (jackvjohnson@hotmail.com)
# Date created: July 2023
# Last edited: 

# clear workspace
rm(list=ls())

# libraries

library(tidyverse)

# working directories 

messy_wd <- "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/AGGRA/Data_messy"
tidy_wd <- "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/AGGRA/Data_tidy"

# read in messy fish data 

fish_1999_2021_df <- read.csv(file=file.path(messy_wd, "fish","AGRRA_fish_1999_2021.csv"))
fish_2022_df <- read.csv(file=file.path(messy_wd, "fish","AGRRA_fish_2022.csv"))

fish_2023_JVJ <- read.csv(file=file.path(messy_wd, "fish","AGRRA_fish_2023_JVJ.csv"))
fish_2023_ADC <- read.csv(file=file.path(messy_wd, "fish","AGRRA_fish_2023_ADC.csv"))
fish_2023_JLR <- read.csv(file=file.path(messy_wd, "fish","AGRRA_fish_2023_JLR.csv"))

# combine 2023 data 
fish_2023_df <- rbind(fish_2023_JVJ, fish_2023_JLR, fish_2023_ADC)

# remove columns >18 as they are empty
fish_2023_df <- fish_2023_df[,1:18]

# convert column headers for 2023 to lower case so they match other dataframes
current_names <- colnames(fish_2023_df)
lowercase_names <- tolower(current_names)
colnames(fish_2023_df) <- lowercase_names

# 2023 col 1 needs to match previous years as "date"
colnames(fish_2023_df)[1] <- "date"

# remove NAs
fish_2023_df <- na.omit(fish_2023_df)

# combine dataframes 
fish_df_ALL_YEARS <- rbind(fish_1999_2021_df, fish_2022_df, fish_2023_df)

# export csv of tidy fish data to tidy working directory 

write.csv(fish_df_ALL_YEARS, file=file.path(tidy_wd, "AGRRA_fish_1999_2023.csv"))

########################################################################################
################################### coral data  ########################################

# tidy workspace 
rm(fish_1999_2021_df, fish_2022_df,fish_2023_ADC, fish_2023_df, fish_2023_JLR, fish_2023_JVJ, fish_df_ALL_YEARS, current_names,lowercase_names)

# read in coral data sheets

coral_1999_2019_df <- read.csv(file=file.path(messy_wd, "benthic","Coral Master List 1999-2019_Longform.csv"))
coral_2019_df <- read.csv(file=file.path(messy_wd, "benthic","AAGRA_2019_Coral.csv"))

coral_2020_df <- read.csv(file=file.path(messy_wd, "benthic","AGRRA_2020_coral.csv"))
coral_2021_df <- read.csv(file=file.path(messy_wd, "benthic","AGRRA_2021_coral.csv"))

coral_2022_ggg <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_coral_GGG_2022.csv"))
coral_2022_am <-  read.csv(file=file.path(messy_wd, "benthic", "AGRRA_coral_AM_2022.csv"))
coral_2022_jlr <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_coral_JLR_2022.csv"))
coral_2022_zp <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_coral_ZP_2022.csv"))

coral_2023_llg <-  read.csv(file=file.path(messy_wd, "benthic", "AGRRA_coral_LLG_2023.csv"))
coral_2023_ke <-  read.csv(file=file.path(messy_wd, "benthic", "AGRRA_coral_KE_2023.csv"))
coral_2023_jlr <-  read.csv(file=file.path(messy_wd, "benthic", "AGRRA_coral_JLR_2023.csv"))
coral_2023_had <-  read.csv(file=file.path(messy_wd, "benthic", "AGRRA_coral_HAD_2023.csv"))
coral_2023_ggg <-  read.csv(file=file.path(messy_wd, "benthic", "AGRRA_coral_GGG_2023.csv"))

coral_2022 <- rbind(coral_2022_am, coral_2022_ggg, coral_2022_jlr, coral_2022_zp)

colnames(coral_2023_jlr)[5] <- "Depth..m."
coral_2023 <- rbind(coral_2023_ggg, coral_2023_had,coral_2023_ke, coral_2023_llg, coral_2023_jlr)
colnames(coral_2023)[5] <- "Depth"

coral_2020_2023_df <- rbind(coral_2020_df, coral_2021_df, coral_2022, coral_2023)

# tidy work space
rm(coral_2020_df, coral_2021_df, coral_2022, coral_2022_am, coral_2022_ggg, coral_2022_jlr, coral_2022_zp, coral_2023, coral_2023_ggg, coral_2023_had, coral_2023_jlr, coral_2023_ke, coral_2023_llg)

# choose key variables which match up between the two dataframes

summary(coral_1999_2019_df)
summary(coral_2020_2023_df) # why is there a 2025 in there 
count(coral_2020_2023_df, Year == "2025") # just the 1 record of 2025. Wonder if the coral is still there after 2023

# 1999 - 2019 keep; year, site, observer, transect, Depth, corr.cover, coral.spp. max.Diameter, Max.Width, Max.Height

coral_1999_2019_df <- select(coral_1999_2019_df, c(Year, Site, OBSERV, TRANSECT, Depth..m., Coral.Spp., Max...Diameter....cm., Max.Width..cm., Max.Height...cm.))

coral_1999_2019_df <- coral_1999_2019_df %>%
  mutate(transect = na_if(TRANSECT, ""))

coral_1999_2019_df <- fill(coral_1999_2019_df, transect)
coral_1999_2019_df <- fill(coral_1999_2019_df, Depth..m.)

# 2020 - 2023 keep; Year, Month, Day, Site, Dpeth, Transect, Observer, Species_Code, Coral_genus, Coral_spp. length, width, height, 

coral_2020_2023_df <- select(coral_2020_2023_df, c(Year, Month, Day, Site, Depth, Transect, Observer, Species_Code, Coral_genus, Coral_spp, length, width, height, coral_SA))
coral_2020_2023_df <- na.omit(coral_2020_2023_df)
names(coral_2020_2023_df)

# old df needs; month, day, genus, species. Remove TRANSECT
# old df also messed up for 2019 so read that in manually get rid of 2019 and import the raw csv if you can find it

coral_1999_2019_df <- subset(coral_1999_2019_df, !Year == 2019)
names(coral_1999_2019_df)
unique(coral_1999_2019_df$Coral.Spp.)

# add 2019 data with the raw csv 

names(coral_2019_df)
Year2019 <- rep(2019, 979)
coral_2019_df <- data.frame(Year = Year2019, coral_2019_df)
coral_2019_df <- select(coral_2019_df, c(Year, Site, OBSERV, TRANSECT, Depth_m, Coral_Spp., Max.._Diameter_cm, Max_Width_cm, Max_Height_cm))

coral_2019_df$transect <- coral_2019_df$TRANSECT

# Will have to edit this again later for entire dataframe but for ease edit 2019 colnames

colnames(coral_2019_df)[5] <- "Depth..m."
colnames(coral_2019_df)[6] <- "Coral.Spp."
colnames(coral_2019_df)[7] <- "Max...Diameter....cm."
colnames(coral_2019_df)[8] <- "Max.Width..cm."
colnames(coral_2019_df)[9] <- "Max.Height...cm."

coral_1999_2019_df <- rbind(coral_1999_2019_df, coral_2019_df)

# need to update codes to aggra codes 

coral_1999_2019_df$Coral.Spp. <- trimws(coral_1999_2019_df$Coral.Spp.)


df <- coral_1999_2019_df %>%
  mutate(new_code = recode(Coral.Spp., 
                           "Agaaga" = "AAGA",
                           "Porpor" = "PPOR", 
                           "Monfav" = "OFAV", 
                           "Monfra" = "OFRA",
                           "Monann" = "OANN", 
                           "Moncav" = "MCAV",
                           "Porast" = "PAST", 
                           "Dipstr" = "PSTR", 
                           "Milspp" = "MILL",
                           "Manare" = "MARE", 
                           "Sidsid" = "SSID",
                           "Colnat" = "CNAT", 
                           "Meamea" = "MMEA", 
                           "Diplab" = "DLAB", 
                           "Dicsto" = "DSTO", 
                           "Acrpal" = "APAL", 
                           "Unknow" = "NA",
                           "Eusfas" = "EFAS", 
                           "Mycspp" = "MYCE", 
                           "Acrcer" = "ACER", 
                           "Musang" = "MANG", 
                           "colnat" = "CNAT", 
                           "porpor" = "PPOR", 
                           "monann" = "OANN", 
                           "sidsid" = "SSID", 
                           "moncav" = "MCAV",
                           "agaaga" = "AAGA", 
                           "monfav" = "OFAV",
                           "diplab" = "DLAB", 
                           "porast" = "PAST",
                           "porspp" = "PPOR", 
                           "agagra" = "AAGA",
                           "dipstr" = "PSTR", 
                           "milspp" = "MILL", 
                           "monfra" = "OFRA",
                           "solbou" = "SBOU", 
                           "mycspp" = "MYCE", 
                           "eusspp" = "EFAS", 
                           "dicsto" = "DSTO", 
                           "?" = "NA", 
                           "acrcer" = "ACER", 
                           "mycfer" = "MFER", 
                           "eusfas" = "EFAS", 
                           "meamea" = "MMEA", 
                           "acrpal" = "APAL", 
                           "dipcli" = "PCLI", 
                           "manspp" = "MARE", 
                           "steint" = "SINT", 
                           "isospp" = "ISOP", 
                           "agaga" = "AAGA",
                           "myclam" = "MLAM", 
                           "Agaspp" = "AGAR", 
                           "Milalc" = "MALC",
                           "Solbou" = "SBOU",
                           "Milcom" = "MCOM", 
                           "Maddac" = "MDEC", 
                           "madspp" = "MADR", 
                           "Sidrad" = "SRAD",
                           "Dipcli" = "PCLI", 
                           "Maddec" = "MDEC", 
                           "lepspp" = "HCUC", 
                           "Favfra" = "FFRA", 
                           "Solspp" = "SBOU",
                           "favfra" = "FFRA",
                           "Isorig" = "IRIG", 
                           "unknown" = "NA", 
                           "Ocuspp" = "OCUL", 
                           "Steint" = "SINT", 
                           "Isospp" = "ISOP", 
                           "agaspp" = "AGAR", 
                           "agaten" = "ATEN", 
                           "porfur" = "PFUR",
                           "PP" = "PPOR",
                           "sidrad" = "SRAD", 
                           "monfavr" = "OFAV",
                           "pordiv" = "PDIV", 
                           "milcom" = "MCOM", 
                           "milalc" = "MALC", 
                           "Myclam" = "MLAM",
                           "agahum" = "AHUM", 
                           "AcrPal" = "APAL", 
                           "MilAlc" = "MALC",
                           "AgaAga" = "AAGA",
                           "MonFav" = "OFAV", 
                           "MonSpp" = "ORBI", 
                           "MonAnn" = "OANN", 
                           "DipStr" = "PSTR", 
                           "SidSid" = "SSID", 
                           "PorAst" = "PAST",
                           "MilCom" = "MCOM", 
                           "SteInt" = "SINT", 
                           "PorPor" = "PPOR",
                           "MonCav" = "MCAV",
                           "MadDec" = "MDEC", 
                           "PorFur" = "PFUR", 
                           "DipLab" = "DLAB",
                           "IsoRig" = "IRIG",
                           "MeaMea" = "MMEA",
                           "AgaSpp" = "AGAR",
                           "EusFas" = "EFAS", 
                           "MycLam" = "MLAM", 
                           "MonFra" = "OFRA", 
                           "MycAli" = "MALI", 
                           "SolBou" = "SBOU", 
                           "SidRad" = "SRAD", 
                           "HelCuc" = "HCUC", 
                           "CopNat" = "CNAT", 
                           "DIPSTR" = "PSTR", 
                           "MILALC" = "MALC", 
                           "MONANN" = "OANN", 
                           "MONFAV" = "OFAV", 
                           "PORAST" = "PAST",
                           "PORFUR" = "PFUR", 
                           "SIDSID" = "SSID", 
                           "AGAAGA" = "AAGA", 
                           "COLNAT" = "CNAT", 
                           "MILCOM" = "MCOM", 
                           "MONCAV" = "MCAV", 
                           "PORPOR" = "PPOR", 
                           "DIPLAB" = "DLAB", 
                           "MADFOR" = "MFOR", 
                           "ACRPAL" = "APAL",
                           "SID SID" = "SSID",
                           "ColNat" = "CNAT", 
                           "DicSto" = "DSTO", 
                           "ISORIG" = "IRIG", 
                           "SIDRAD" = "SRAD", 
                           "AGALAM" = "ALAM", 
                           "EUSFAS" = "EFAS", 
                           "MONFRA" = "OFRA", 
                           "MYCFER" = "MFER", 
                           "MYCLAM" = "MLAM", 
                           "STEINT" = "SINT", 
                           "MEAMEA" = "MMEA",
                           "MANARE" = "MARE", 
                           "MON" = "NA", 
                           "PORDIV" = "PDIV", 
                           "DICSTO" = "DSTO", 
                           "DIPCLI" = "PCLI", 
                           "ACRCER" = "ACER", 
                           "MUSANG" = "MANG", 
                           "manare" = "MARE", 
                           "madfor" = "MFOR", 
                           "leuspp" = "NA", 
                           "lepcuc" = "HCUC",
                           "isosin" = "ISIN", 
                           "madmir" = "MAUR", 
                           "agalam" = "ALAM", 
                           "maddec" = "MDEC", 
                           "monare" = "MARE", 
                           "porats" = "PAST",
                           "isorig" = "IRIG", 
                           "mycdan" = "NA", 
                           "sidbou" = "SBOU", 
                           "poast" = "PAST", 
                           "scospp" = "SCOL",
                           "milacl" = "MALC", 
                           "mycali" = "MALI", 
                           "aga" = "AGAR", 
                           "styspp" = "NA", 
                           "musspp" = "MANG", 
                           "scowel" = "SWEL", 
                           "scolac" = "SLAC", 
                           "madaur" = "MAUR",
                           " " = "NA", 
                           "N/A" = "NA", 
                           "AGAHUM" = "AHUM", 
                           "SOLBOU" = "SBOU", 
                           "AGAGRA" = "AAGA", 
                           "MONSPP" = "ORBI", 
                           "ISOSIN" = "ISIN", 
                           "AGASPP"= "AGAR", 
                           "MADAUR" = "MAUR", 
                           "PORSPP" = "PPOR", 
                           "MYCSPP" = "MYCE", 
                           "MADSPP" = "MADR",
                           "FAVFAR"= "FFRA", 
                           "AGAFRA" = "AFRA",
                           "FAVFRA" = "FFRA",
                           "CMAT" = "CNAT", 
                           "DIAB" = "DLAB", 
                           "PRNGLE CORAL" = "NA",
                           "LABARI" = "NA", 
                           "MYCALI" = "MALI", 
                           "MADDEC" = "MDEC", 
                           "AAGA" = "AAGA", 
                           "AHUM" = "AHUM", 
                           "MFRA" = "OFRA", 
                           "PFUR" = "PFUR", 
                           "PAST" = "PAST", 
                           "MFAV" = "OFAV",
                           "MANN" = "OANN",
                           "PPOR" = "PPOR", 
                           "SSID" = "SSID", 
                           "AFRA" = "AFRA", 
                           "DSTR" = "PSTR", 
                           "MFAC" = "NA",
                           "MMEA" = "MMEA", 
                           "MLAM" = "MLAM", 
                           "MCAV" = "MCAV", 
                           "EFAS" = "EFAS", 
                           "DLAB" = "DLAB",
                           "CNAT" = "CNAT", 
                           "AAFA" = "AAGA", 
                           "SINT" = "SINT",
                           "APAL" = "APAL", 
                           "PSTR" = "PSTR", 
                           "SBOU" = "SBOU", 
                           "DSTO" = "DSTO", 
                           "SRAD" = "SRAD", 
                           "PDIV" = "PDIV", 
                           "MDAN" = "MDAN", 
                           "SSOD" = "SSID", 
                           ",FAV" = "FFRA", 
                           "SSIS" = "SSID",
                           "MFER" = "MFER", 
                           "FFRA" = "FFRA", 
                           "MARE" = "MARE",
                           "OFRA" = "OFRA", 
                           "MFOR" = "MFER", 
                           "OFAV" = "OFAV", 
                           "SCUB" = "SCUB", 
                           "OANN" = "OANN", 
                           "MMIR" = "NA", 
                           "MFRO" = "NA", 
                           "ALAM" = "ALAM",
                           "ACER" ="ACER",
                           "Agar" = "AGAR", 
                           "AGAR" = "AGAR", 
                           "MALI" = "MALI", 
                           "MCOM" = "MCOM", 
                           "PCLI" = "PCLI", 
                           "past" = "PAST", 
                           "pstr" = "PSTR", 
                           "MALC" = "MALC", 
                           "DCLI" = "PCLI",
                           "AAG" = "AGAR", 
                           "OFAC" = "OFAV"))

# genus and species epiphets 
df <- df %>%
  mutate(full_name = recode(new_code, 
                            "AAGA" = "Agaricia_agaricites", 
                            "PPOR" = "Porites_porites", 
                            "OFAV" = "Orbicella_faveolata", 
                            "OFRA" = "Orbicella_franksi", 
                            "OANN" = "Orbicella_annularis",
                            "MCAV" = "Montastraea_cavernosa",
                            "PAST" = "Porites_astreoides", 
                            "PSTR" = "Pseudodiploria_strigosa", 
                            "MILL" = "Millepora", 
                            "MARE" = "Manicina_areolata", 
                            "SSID" = "Siderastrea_siderea", 
                            "CNAT" = "Colpophyllia_natans", 
                            "MMEA" = "Meandrina_meandrites",
                            "DLAB" = "Diploria_labyrinthiformis", 
                            "DSTO" = "Dichocoenia_stokesii", 
                            "APAL" = "Acropora_palmata", 
                            "NA" = "NA", 
                            "EFAS" = "Eusmilia_fastigiata", 
                            "MYCE" = "Mycetophyllia", 
                            "ACER" = "Acropora_cervicornis", 
                            "MANG" = "Mussa_angulosa", 
                            "SBOU" = "Solenastrea_bournoni",
                            "MFER" = "Mycetophyllia_ferox", 
                            "PCLI" = "Pseudodiploria_clivosa", 
                            "SINT" = "Stephanocoenia_intersepta", 
                            "ISOP" = "Isophyllia", 
                            "MLAM" = "Mycetophyllia_lamarckiana", 
                            "AGAR" = "Agaricia", 
                            "MALC" = "Millepora_alcicornis", 
                            "MCOM" = "Millepora_complanata", 
                            "MDEC" = "Madracis_decactis", 
                            "MADR" = "Madracis", 
                            "SRAD" = "Siderastrea_radians", 
                            "HCUC" = "Helioseris_cucullata",
                            "FFRA" = "Favia_fragum",
                            "IRIG" = "Isophyllia_rigida", 
                            "OCUL" = "Oculina", 
                            "ATEN" = "Agaricia_tenuifolia", 
                            "PFUR" = "Porites_furcata", 
                            "PDIV" = "Porites_divaricata", 
                            "AHUM" = "Agaraicia_humilis", 
                            "ORBI" = "Orbicella", 
                            "MALI" = "Mycetophyllia_lamarckiana",
                            "MFOR" = "Madracis_formosa", 
                            "ALAM" = "Agaricia_lamarcki",
                            "ISIN" = "Isophyllia_sinuosa", 
                            "MAUR" = "Madracis_auretenra", 
                            "SCOL" = "Scolymia", 
                            "SWEL" = "scolymia_wellsi", 
                            "SLAC" = "Scolymia_lacera", 
                            " " = "NA", 
                            "AFRA" = "Agaricia_fragilis", 
                            "SBOU" = "Solenastrea_bournoni", 
                            "MDAN" = "Meandrina_danae", 
                            "SCUB" = "Scolymia_cubensis"
  ))

table(df$full_name)

# Split the 'species_code' column into two columns 'genus' and 'species'
df <- separate(df, full_name, into = c("Coral_genus", "Coral_spp"), sep = "_", remove = FALSE)

# make new df structured to match 2020-2023
names(coral_2020_2023_df)

df2 <- data.frame(Year = df$Year)
df2$Month <- NA
df2$Day <- NA
df2$Site <- df$Site
df2$Depth <- df$Depth..m.
df2$Transect <- df$transect
df2$Observer <- df$OBSERV
df2$Species_Code <- df$new_code
df2$Coral_genus <- df$Coral_genus
df2$Coral_spp <- df$Coral_spp
df2$length <- df$Max...Diameter....cm.
df2$width <- df$Max.Width..cm.
df2$height <- df$Max.Height...cm.
df2$coral_SA <- NA

new_coral_df <- rbind(df2, coral_2020_2023_df)

write.csv(new_coral_df, file=file.path(tidy_wd, "AGRRA_coral_1999_2023.csv"))

########################################################################################
################################# algae/benthic  #######################################

# tidy work space

rm(coral_1999_2019_df, coral_2019_df, coral_2020_2023_df, df, df2, new_coral_df, Year2019)

algae_1999_2019 <- read.csv(file=file.path(messy_wd, "benthic", "Algae Master List 1999-2019_Longform.csv"))
benthic_2020 <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_2020_benthic.csv"))
benthic_2021 <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_2021_benthic.csv"))
benthic_2022_GGG <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_GGG_2022.csv"))
benthic_2022_AM <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_AM_2022.csv"))
benthic_2022_JLR <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_JLR_2022.csv"))
benthic_2022_ZP <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_ZP_2022.csv"))
benthic_2023_GGG <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_GGG_2023.csv"))
benthic_2023_HAD <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_HAD_2023.csv"))
benthic_2023_KE <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_KE_2023.csv"))
benthic_2023_LLG <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_benthic_LLG_2023.csv"))


test <- select(algae_1999_2019, c("Cructose", "Macro.Total", "Macro.Fleshy", "Macro.Calc", "Turf"))
test[is.na(test)] <- 0
test <- as.matrix(test)
test <- prop.table(test, margin=1)
test <- as.data.frame(test)

# Assuming your data frame is named 'df'
colnames(test) <- paste("prop", colnames(test), sep = "_")

algae_1999_2019 <- cbind(algae_1999_2019, test)
algae_1999_2019 <- algae_1999_2019[, -c(5:10)]
algae_1999_2019 <- na.omit(algae_1999_2019)

# proportion cover of each algae type per transect per site per year 
# dropping different types of macroalage because inconsistent throughout years 

df <- algae_1999_2019 %>%
  group_by(Year, Site, trasnect) %>%
  summarise(prop_CCA = mean(prop_Cructose), prop_macro = mean(prop_Macro.Total), prop_turf = mean(prop_Turf))

# check prop = 1
sum(df[1, 4:6], na.rm = TRUE)

# add on extra years

benthic_2020_2023 <- rbind(benthic_2020, benthic_2021, benthic_2022_GGG, benthic_2022_AM, benthic_2022_JLR,
                           benthic_2022_ZP, benthic_2023_GGG, benthic_2023_HAD, benthic_2023_KE, benthic_2023_LLG
)

table(benthic_2020_2023$Functional_Group)
# over 9000 empty rows, nice
benthic_2020_2023 <- na.omit(benthic_2020_2023)

# fucntional group names are a mess

benthic_2020_2023 <- benthic_2020_2023 %>%
  mutate(new_group = recode(Functional_Group, 
                            "CCA" = "percent_CCA",
                            "cca" = "percent_CCA", 
                            "cliona" = "NA",
                            "coral" = "hard_coral", 
                            "crustose corraline algae" = "percent_CCA", 
                            "crustose_corraline_algae" = "percent_CCA", 
                            "hard_coral" = "hard_coral", 
                            "macro_algae" = "percent_macro", 
                            "macroalgae" = "percent_macro", 
                            "other" = "other", 
                            "rock_turf_algae" = "percent_turf", 
                            "sand" = "sand", 
                            "soft_coral" = "soft_coral", 
                            "sponge" = "sponge", 
                            "turf" = "percent_turf",
                            "turf_algae" = "percent_turf"
  ))

table(benthic_2020_2023$new_group)

df_2020_2023 <- benthic_2020_2023 %>%
  group_by(Year, Site, Observer, Transect, new_group) %>% 
  summarise(percent_cover = mean(Perc_cover))

df_2020_2023 <- df_2020_2023 %>%
  pivot_wider(names_from = new_group, values_from = percent_cover)

# only focus on algae because of previous years data 
df_2020_2023 <- df_2020_2023[,-c(5, 9:13)]

df_2020_2023[is.na(df_2020_2023)] <- 0

names(df)
names(df_2020_2023)

# make prop table for these years

prop_2 <- df_2020_2023[,5:7]
prop_2[is.na(prop_2)] <- 0
prop_2 <- as.matrix(prop_2)
prop_2 <- prop.table(prop_2, margin = 1)
prop_2 <- as.data.frame(prop_2)

df_2020_2023 <- df_2020_2023[,1:4]
df_2020_2023 <- cbind(df_2020_2023, prop_2)
colnames(df_2020_2023)[5] <- "prop_CCA"
colnames(df_2020_2023)[6] <- "prop_macro"
colnames(df_2020_2023)[7] <- "prop_turf"

df$Observer <- NA
colnames(df)[3] <- "Transect"

df_1999_2023 <- rbind(df, df_2020_2023)

write.csv(df_1999_2023, file=file.path(tidy_wd, "AGRRA_algae_1999_2023.csv"))

rm(benthic_2020, benthic_2021, benthic_2022_GGG, benthic_2022_AM, benthic_2022_JLR,benthic_2022_ZP, benthic_2023_GGG, benthic_2023_HAD, benthic_2023_KE, benthic_2023_LLG, df, df_1999_2023, test, algae_1999_2019, benthic_2020_2023, df_2020_2023. prop_2)

########################################################################################
################################# coral recruits  ######################################

# owing to earlier yeats simply having n of recruits, and uncertaintiy regarding species id, make a tidy dataframe for the number of recruits per site, per transect, per year 

recruits_1999_2019 <- read.csv(file=file.path(messy_wd, "benthic", "Algae Master List 1999-2019_Longform.csv"))

recruits_2020 <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_2020_recruits.csv"))
recruits_2021 <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_2021_coral_recruits.csv"))
recruits_2022_GGG <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_recruits_GGG_2022.csv"))
recruits_2022_AM <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_recruits_AM_2022.csv"))
recruits_2022_ZP <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_recruits_ZP_2022.csv"))
recruits_2023_GGG <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_recruits_GGG_2023.csv"))
recruits_2023_HAD <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_recruits_HAD_2023.csv"))
recruits_2023_KE <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_recruits_KE_2023.csv"))
recruits_2023_LLG <- read.csv(file=file.path(messy_wd, "benthic", "AGRRA_recruits_LLG_2023.csv"))

# early data first 
recruits_1999_2019$Number.of.juvenile.corals <- as.numeric(recruits_1999_2019$Number.of.juvenile.corals)
df_1999_2019 <- recruits_1999_2019 %>%
  group_by(Year, Site, trasnect) %>%
  summarise(total_recruits = sum(Number.of.juvenile.corals))

# 2020-2023

recruits_2020_2023 <- rbind(recruits_2020, recruits_2021, recruits_2022_GGG, recruits_2022_AM, recruits_2022_ZP,
                            recruits_2023_GGG, recruits_2023_HAD, recruits_2023_KE, recruits_2023_LLG
)

df_2020_2023 <- recruits_2020_2023 %>%
  group_by(Year, Month, Date, Site, Observer, Transect) %>%
  summarise(total_recruits = n())

df_2020_2023 <- na.omit(df_2020_2023)

names(df_1999_2019)
names(df_2020_2023)

colnames(df_1999_2019)[3] <- "Transect"
df_1999_2019$Month <- NA
df_1999_2019$Date <- NA
df_1999_2019$Observer <- NA

df_1999_2023<- rbind(df_1999_2019, df_2020_2023)

write.csv(df_1999_2023, file=file.path(tidy_wd, "AGRRA_recruits_1999_2023.csv"))

rm(recruits_2020, recruits_2021, recruits_2022_GGG, recruits_2022_AM, recruits_2022_ZP,
   recruits_2023_GGG, recruits_2023_HAD, recruits_2023_KE, recruits_2023_LLG, df_1999_2019, df_1999_2023, df_2020, df_2020_2023, recruits_1999_2019, recruits_2020_2023, prop_2)
