#===============================================================================
# Script to wrangle growth data from various formats (mainly Tucson format) 
# into long format.
#-------------------------------------------------------------------------------

# load dependencies ------------------------------------------------------------
if (!existsFunction("%>%"))      library("tidyverse") # to process data
if (!existsFunction("readxl"))   library("readxl") # to read metadata
if (!existsFunction("read.rwl")) library("dplR") # to read chronology files

# change warnings to be errors to make sure everything is read properly --------
options(warn = 2)

# get metadata for all chronologies with file ----------------------------------
siteMetaData <- readxl::read_excel(
  col_names = TRUE, 
  col_types = c ("numeric","text","text","text","text","text","text",
                 "text","numeric","numeric","numeric","numeric","numeric",
                 "text","logical","text","text","text"),
  path = "../data/growth/chronologyData/siteMetaData.xlsx",
  na = "NA") %>% 
  filter(!is.na(file)) %>% 
  filter(source %in% c("ITRDB_clean", "NP", "JTM", "SW", "BG", "SP", "SF", "LD", 
                       "JH", "DB", "ITRDB", "MFFP")) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))
# NB: Still waiting for MG chronologies.

# loop over files and load each of them into a tibble --------------------------
for (i in 1:dim (siteMetaData)[1]) {
  
  # use source to figure out file path -----------------------------------------
  if (siteMetaData$source[i] == 'ITRDB_clean') {
    fPath <- '../data/growth/chronologyData/ITRDB_cleaned_Zhao2019/Cleaned datasets/itrdb-v713-cleaned-rwl/usa/'
  } else if (siteMetaData$source[i] == 'NP') {
    fPath <- '../data/growth/chronologyData/NeilPedersonCollection/'
  } else if (siteMetaData$source[i] == 'JTM') {
    fPath <- '../data/growth/chronologyData/JustinMaxwellCollection/'
  } else if (siteMetaData$source[i] == 'BG') {
    fPath <- '../data/growth/chronologyData/ISFORT/BenoitGendreau-Berthiaume/'
  } else if (siteMetaData$source[i] == 'SW') {
    fPath <- '../data/growth/chronologyData/ScottWarnerCollection/'
  } else if (siteMetaData$source[i] == 'SP') {
    fPath <- '../data/growth/chronologyData/SergePayetteCollection/Data_ACSA_SP.xlsx'
  } else if (siteMetaData$source[i] == 'SF') {
    fPath <- '../data/growth/chronologyData/ShawnFraverCollection/'
  } else if (siteMetaData$source[i] == 'LD') {
    fPath <- '../data/growth/chronologyData/LoicDOrangevilleCollection/'
  } else if (siteMetaData$source[i] == 'DB') {
    fPath <- '../data/growth/chronologyData/DanBishopCollection/'
  } else if (siteMetaData$source[i] == 'JH') {
    fPath <- '../data/growth/chronologyData/JustinHartCollection/'
  } else if (siteMetaData$source[i] == 'ITRDB') {
    fPath <- '../data/growth/chronologyData/ITRDB/'
  } else if (siteMetaData$source[i] == 'MFFP') {
    fPath <- '../data/growth/chronologyData/MFFP/ERS_ERR_RESEF_Cernes.csv'  
  }
  filename <- paste0(fPath, siteMetaData$file[i])
  #print (filename)
  
  # read the rwl or other file with raw measurements ---------------------------
  if ((siteMetaData$source[i] == 'SP') | 
      (siteMetaData$source[i] == "JH" & 
       substr(filename, nchar(filename)-2, nchar(filename)) == "xls")) {
    tmp <- suppressMessages(
      readxl::read_excel(col_names = TRUE, 
                         path = ifelse(siteMetaData$source[i] == "SP", 
                                       fPath, filename), # In this case this is the file name
                         sheet = ifelse(siteMetaData$source[i] == "SP",
                                        siteMetaData$file[i], "rwl"),
                         na = ifelse(siteMetaData$source[i] == "SP",
                                     "", "NA"))) 
    if (siteMetaData$source[i] == "SP") {
      tmp <- tmp[-c(1:3), ]
      names(tmp)[1] <- 'year'
    } else if (siteMetaData$source[i] == 'JH') {
      tmp <- tmp %>% relocate(year, .after = dim(tmp)[2])
    }
  } else if (siteMetaData$source[i] == 'MFFP') {
    tmp <- read_delim(file = fPath,
                      delim = ';',
                      col_names = c('site','p','tree','species','m','core','h',
                                    'year','rwEYSTI'),
                      col_types = cols(), skip = 1)
    
    # wrangle data
    tmp <- tmp %>% mutate(
      site = case_when(
        site ==  101 ~ 248, site ==  102 ~ 249, site ==  103 ~ 250,
        site ==  105 ~ 251, site ==  201 ~ 252, site ==  301 ~ 253,
        site ==  401 ~ 254, site ==  402 ~ 255, site ==  501 ~ 256,
        site ==  502 ~ 257, site ==  503 ~ 258, site ==  701 ~ 259,
        site ==  702 ~ 260, site ==  703 ~ 261, site ==  802 ~ 262,
        site == 1201 ~ 263, site == 1202 ~ 264, site == 1204 ~ 265,
        site == 1501 ~ 266, site == 1502 ~ 267)) %>%
      filter(site == i) %>%
      mutate(species = ifelse(species == 'ERS', 'ACSA', 'ACRU')) %>%
      dplyr::select(-c(p, m, h)) %>% 
      mutate(lat = siteMetaData$lat[i],
             lon = siteMetaData$lon[i]) %>% 
      relocate(year, site, lat, lon, species, tree, core, rwEYSTI) %>%
      mutate(rwEYSTI = rwEYSTI / 100)
  } else if (siteMetaData$source[i] != 'SP') {
    tmp <- suppressMessages(
      dplR::read.rwl(fname = filename, format = "tucson", 
                     header = ifelse(is.na(siteMetaData$header[i]), 
                                     TRUE, 
                                     siteMetaData$header[i])))
    # add year as a seperate row
    tmp$year <- rownames(tmp)
  }
  
  # pivot to longer format and add tree and core specifiers depending on format
  #--------------------------------------------------------------------------------------
  if (is.na(siteMetaData$labelFormat[i])) {
    temp <- tmp
  } else if (siteMetaData$labelFormat[i] == "TT-I") {
    temp <- pivot_longer(tmp, cols = 1:(dim (tmp)[2]-1), 
                         values_to = "rwEYSTI", names_sep = "-", 
                         names_to = c ("tree","core"))
  } else if (siteMetaData$labelFormat [i] %in% c ("SSTTI","SSSTTI","SSSSTTI",
                                                  "SSPPTTI","SSSPTTI",
                                                  "SSSPPTTI","SSSSSTTI",
                                                  "SSSXXTTI")) {
    temp <- pivot_longer(tmp, cols = 1:(dim(tmp)[2]-1), 
                         values_to = "rwEYSTI", names_sep = c(2,3),
                         names_prefix = siteMetaData$labelPrefix[i],
                         names_to = c("tree","core"))
    if (siteMetaData$site [i] == 97) {
      temp$core [temp$tree == "18"] <- "N2"
      temp$tree [temp$tree == "18"] <- "08"
    }
  } else if (siteMetaData$labelFormat[i] %in% c("SSPTTTI","TTTI","SSPTTI",
                                                "SSSTT")) {
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = "rwEYSTI", names_sep = c (3,4),
                          names_prefix = siteMetaData$labelPrefix [i],
                          names_to = c ("tree","core"))
    if (siteMetaData$site [i] %in% c (23:26)) temp$core <- "1"
  } else if (siteMetaData$labelFormat [i] %in% c ("SSSSTTII","SSSS-TT","SSS-TT",
                                                  "SS-TT")) {
    temp <- pivot_longer (tmp, cols = 1:(dim (tmp)[2]-1), 
                          values_to = "rwEYSTI", names_sep = c (2,4),
                          names_prefix = siteMetaData$labelPrefix [i],
                          names_to = c ("tree","core"))
  } else if (siteMetaData$labelFormat [i] %in% c ("SSS-TT-I")) {
    temp <- pivot_longer(tmp, cols = 2:(dim (tmp)[2]), 
                         values_to = "rwEYSTI", names_sep = "-",
                         names_prefix = siteMetaData$labelPrefix[i],
                         names_to = c("tree","core")) %>% 
      mutate(rwEYSTI = as.numeric(rwEYSTI) / 1000)
  } else if (siteMetaData$labelFormat [i] %in% c ("XTTT","XTTTT")) { # Loic"s format
    temp <- pivot_longer(tmp, cols = 1:(dim(tmp)[2]-1), 
                         values_to = "rwEYSTI",
                         names_prefix = siteMetaData$labelPrefix[i],
                         names_to = "tree")
  } else if (siteMetaData$labelFormat [i] %in% c ("SSSTTTTI")) {
    temp <- pivot_longer(tmp, cols = 1:(dim (tmp)[2]-1), 
                         values_to = "rwEYSTI", names_sep = c(4,5),
                         names_prefix = siteMetaData$labelPrefix[i],
                         names_to = c("tree","core"))
  } else if (siteMetaData$labelFormat [i] %in% c ("TTSSSSI")) {
    temp <- pivot_longer(tmp, cols = 1:(dim (tmp)[2]-1), 
                         values_to = "rwEYSTI", names_sep = c (3,8),
                         names_to = c ("tree","core")) %>%
      mutate(core = substr(core, 5, 5))
  }
  # delete row without ring width
  temp <- temp %>% filter (!is.na (rwEYSTI))
  
  # add incrementCoreID for SF and LD collections, which only had one core per tree
  if (siteMetaData$source[i] %in% c('SF','LD')) temp$core <- '1'
  
  # add site name before adding it to the rwYSTI tibble
  if(siteMetaData$source[i] != 'MFFP'){
    temp <- temp %>% dplyr::mutate (site = siteMetaData$site[i], 
                                    lat = siteMetaData$lat[i],
                                    lon = siteMetaData$lon[i],
                                    species = siteMetaData$species[i],
                                    .before = tree)
  }
    
  # initialise tibble 
  if (i == 1) {
    tempData <- temp
  # or add to it
  } else {
    tempData <- rbind (tempData, temp)
  }

  # print important metadata
  VERBOSE <- FALSE
  if (VERBOSE) {
    print (sprintf ('Site: %i', siteMetaData$site[i]))
    print (sprintf ('Start: %s', min(temp$year)))
    print (sprintf ('End: %s', max(temp$year)))
    print (sprintf ('nTrees: %i', n_groups(temp %>% group_by(tree))))
    print (sprintf ('nCores: %i', n_groups(temp %>% group_by(tree, core))))
    print (sprintf ('File name: %s', siteMetaData$file[i]))
    print (head (temp))
    print ('')
  }
  
  # delete temporary variables 
  rm (tmp, temp)
}

# change warnings back to be just that -----------------------------------------
options(warn = 1)

# add unique tree and core IDs for simulation to each row
#-------------------------------------------------------------------------------
tempData <- tempData %>% mutate (treeID = NA, incrementCoreID = NA)
tempData$treeID <- tempData %>% 
  group_by (site, tree) %>% 
  group_indices () %>% as.factor ()
max (as.numeric (levels (tempData$treeID))) # number of trees
tempData$incrementCoreID <- tempData %>% 
  group_by (site, tree, core) %>% 
  group_indices () %>% as.factor () 
max (as.numeric (levels (tempData$incrementCoreID))) # number of increment cores
length (unique (tempData$site)) # number of sites

# extract cardinal direction of the core, if available
#-------------------------------------------------------------------------------
tempData$core[which(tempData$core %in% c('n','N','N1','N2','N3'))] <- 'North'
tempData$core[which(tempData$core %in% c('e','E'))] <- 'East'
tempData$core[which(tempData$core %in% c('s','S','S1','S2','S3'))] <- 'South'
tempData$core[which(tempData$core %in% c('w','W'))] <- 'West'
tempData <- tempData %>% 
  mutate (cardinalDir = ifelse (core == 'North', 'North', 
                                ifelse (core == 'East', 'East', 
                                        ifelse (core == 'South', 'South', 
                                                ifelse (core == 'West', 'West', NA)))))

# arrange in chronological order -----------------------------------------------
tempData <- tempData %>% arrange(year)

# make sure year  and site are a integers
#-------------------------------------------------------------------------------
tempData <- tempData %>% mutate(year = as.integer (year))

# add cumulative sums of size and age at each point in time --------------------
tempData <- tempData %>% group_by(site, treeID, incrementCoreID) %>% 
  mutate(rw_cum = cumsum(rwEYSTI), min_age = year-min(year)+1)

# replace the mininum age for the older core with the younger ------------------
tempData <- tempData %>% group_by(year, treeID) %>% 
  mutate(min_age = max(min_age)) %>% ungroup()

# TR - I need to add this calculation eventually
# add approximation of missed length to the increment core with less years -----
#for (r in 1:dim(tempData)[1]){
#  
#}

# deselect cardinal direction for now and only keep incrementCoreID and treeID
#-------------------------------------------------------------------------------
tempData <- tempData %>% dplyr::select (-cardinalDir, -core, -tree)

# reorder in terms of model subscripts (i.e., 'E' for species, 'Y' for year, 
# 'S' for sites, 'T' for tree, and 'I' for increment core)
#-------------------------------------------------------------------------------
rwEYSTI <- tempData %>% 
  relocate (rwEYSTI, rw_cum, min_age, species, year, site, treeID, incrementCoreID, lat, lon)

# There is one value from Justin Hart sugar maple (ACSA PCT.xls) at -2,903 which 
# does not make sense and which I remove here for now --------------------------
rwEYSTI <- rwEYSTI [-which (rwEYSTI$rwEYSTI < 0), ]
# TR - Also need to check the really large numbers (i.e., > 8mm)
# Largest ring comes from Louis Duchesne plots with 21mm

# make histogram of ring widths
#-------------------------------------------------------------------------------
PLOT <- FALSE; if (PLOT) {
  png (file = '../fig/ringWidthsHist.png', width = 700, height = 400)
  par (mar = c (5, 6, 1, 5), mfrow = c (1, 1)) 
  hist (rwEYSTI$rwEYSTI [rwEYSTI$species == 'ACSA'], 
        main = '', xlab = 'Croissance radiale (mm)', ylab = "", las = 1, axes = FALSE,
        ylim = c (0, 80000), border = F,
        col = '#f3bd4833', breaks = seq (0, 21, by = 0.3))
  hist (rwEYSTI$rwEYSTI [rwEYSTI$species == 'ACRU'], add = TRUE, axes = FALSE,
        ylim = c (0, 80000), border = F, col = '#901c3b33', 
        breaks = seq (0, 21, by = 0.3))
  abline (v = max(rwEYSTI$rwEYSTI [rwEYSTI$species == "ACSA"]), 
          col = '#f3bd48', lwd = 2, lty = 2)
  #text(x = max(rwEYSTI$rwEYSTI [rwEYSTI$species == "ACSA"]),
  #     y = , labels = "max")
  abline (v = max(rwEYSTI$rwEYSTI [rwEYSTI$species == "ACRU"]), 
          col = '#901c3b', lwd = 2, lty = 2)
  axis (side = 1, seq (0, 21, 5))
  axis (side = 2, seq (0, 80000, 10000), las = 1)
  par (new = TRUE)
  plot (density (rwEYSTI$rwEYSTI [rwEYSTI$species == 'ACSA']), 
        col = '#f3bd48', lwd = 3, main = '', xlab = '', 
        ylab = '', axes = FALSE, ylim = c (0, 0.5))
  par (new = TRUE)
  plot (density (rwEYSTI$rwEYSTI [rwEYSTI$species == 'ACRU']), 
        col = '#901c3b', lwd = 3, main = '', xlab = '', 
        ylab = '', axes = FALSE, ylim = c (0, 0.5))
  axis (side = 4, seq (0, 0.5, 0.1), las = 1)
  
  mtext (side = 4, line = 4, text = 'Densité')
  mtext (side = 2, line = 4, text = 'Fréquence')
  dev.off ()
}

# average by tree
#-------------------------------------------------------------------------------
rwEYST <- rwEYSTI %>% group_by(species, year, site, treeID, lat, lon) %>%
  summarise(rwEYST = mean(rwEYSTI, na.rm = TRUE), 
            age = max(min_age),
            rw_cum = mean(rw_cum), # TR - this is not correct but better than nothing for the presentation on monday
            .groups = 'drop') %>% 
  relocate(rwEYST, rw_cum, age, species, year, site, treeID, lat, lon)

# maple growth statistics ------------------------------------------------------
rwEYST %>% dplyr::select(rwEYST) %>% unlist() %>% mean(., na.rm = TRUE)  # mean
rwEYST %>% dplyr::select(rwEYST) %>% unlist() %>% sd(., na.rm = TRUE)    # standard deviation
rwEYST %>% dplyr::select(rwEYST) %>% unlist() %>% range(., na.rm = TRUE) # range
rwEYSTI %>% group_by(treeID) 
rwEYSTI %>% group_by(incrementCoreID) 
rwEYST %>% dplyr::select(rwEYST) %>% count()                             # number of rings (averaged per trees)
rwEYSTI %>% dplyr::select(rwEYSTI) %>% count()                           # number of rings
rwEYSTI %>% filter(rwEYSTI == 0) %>% dplyr::select (rwEYSTI) %>% count() # number of missing rings
rwEYSTI %>% filter(rwEYSTI == 0) %>% dplyr::select (rwEYSTI) %>% count() / # frequency of missing rings
rwEYSTI %>% dplyr::select(rwEYSTI) %>% count()                        

# sugar maple 
rwEYST %>% filter (species == "ACSA") %>% dplyr::select(rwEYST) %>% unlist() %>% mean(., na.rm = TRUE) 
rwEYST %>% filter (species == "ACSA") %>% dplyr::select(rwEYST) %>% unlist() %>% sd(., na.rm = TRUE) 
rwEYST %>% filter (species == "ACSA") %>% dplyr::select(rwEYST) %>% unlist() %>% range(., na.rm = TRUE) 
rwEYST %>% filter (species == "ACSA") %>% dplyr::select(rwEYST) %>% count()
rwEYSTI %>% filter (species == "ACSA") %>% dplyr::select(rwEYSTI) %>% count()
rwEYSTI %>% filter(rwEYSTI == 0, species == "ACSA") %>% dplyr::select (rwEYSTI) %>% 
  count() / rwEYSTI %>% filter(species == "ACSA") %>% dplyr::select(rwEYSTI) %>% count()  

# red maple 
rwEYST %>% filter (species == "ACRU") %>% dplyr::select(rwEYST) %>% unlist() %>% mean(., na.rm = TRUE) 
rwEYST %>% filter (species == "ACRU") %>% dplyr::select(rwEYST) %>% unlist() %>% sd(., na.rm = TRUE) 
rwEYST %>% filter (species == "ACRU") %>% dplyr::select(rwEYST) %>% unlist() %>% range(., na.rm = TRUE) 
rwEYST %>% filter (species == "ACRU") %>% dplyr::select(rwEYST) %>% count()
rwEYSTI %>% filter (species == "ACRU") %>% dplyr::select(rwEYSTI) %>% count()
rwEYSTI %>% filter(rwEYSTI == 0, species == "ACRU") %>% dplyr::select (rwEYSTI) %>% 
  count() / rwEYSTI %>% filter(species == "ACRU") %>% dplyr::select(rwEYSTI) %>% count() 

# mean radial growth in Quebec -------------------------------------------------
# included sites: BG   - 62-131
#                 SP   - 132-158
#                 MFFP - 248:267
Quebec <- rwEYST %>% filter(site %in% c(62:158, 248:267))
Quebec %>% dplyr::select(rwEYST) %>% unlist() %>% mean(., na.rm = TRUE) 
Quebec %>% dplyr::select(rwEYST) %>% unlist() %>% sd(., na.rm = TRUE) 
Quebec %>% dplyr::select(rwEYST) %>% unlist() %>% range(., na.rm = TRUE) 
Quebec %>% dplyr::select(rwEYST) %>% count()
rwEYSTI %>% filter(site %in% c(62:158, 248:267)) %>% dplyr::select(rwEYSTI) %>% count()

# Sugar maple ------------------------------------------------------------------
Quebec %>% filter (species == "ACSA") %>% dplyr::select(rwEYST) %>% unlist() %>% mean(., na.rm = TRUE) 
Quebec %>% filter (species == "ACSA") %>% dplyr::select(rwEYST) %>% unlist() %>% sd(., na.rm = TRUE) 
Quebec %>% filter (species == "ACSA") %>% dplyr::select(rwEYST) %>% unlist() %>% range(., na.rm = TRUE) 
Quebec %>% filter (species == "ACSA") %>% dplyr::select(rwEYST) %>% count()
rwEYSTI%>% filter(site %in% c(62:158, 248:267), species == "ACSA") %>% dplyr::select(rwEYSTI) %>% count()
rwEYSTI%>% filter(site %in% c(62:158, 248:267), species == "ACSA") %>% dplyr::select(rwEYSTI) %>% filter (rwEYSTI == 0) %>% count() / 
rwEYSTI%>% filter(site %in% c(62:158, 248:267), species == "ACSA") %>% dplyr::select(rwEYSTI) %>% count()

# Red maple --------------------------------------------------------------------
Quebec %>% filter (species == "ACRU") %>% dplyr::select(rwEYST) %>% unlist() %>% mean(., na.rm = TRUE) 
Quebec %>% filter (species == "ACRU") %>% dplyr::select(rwEYST) %>% unlist() %>% sd(., na.rm = TRUE) 
Quebec %>% filter (species == "ACRU") %>% dplyr::select(rwEYST) %>% unlist() %>% range(., na.rm = TRUE) 
Quebec %>% filter (species == "ACRU") %>% dplyr::select(rwEYST) %>% count()
rwEYSTI %>% filter (species == "ACRU", site %in% c(62:158, 248:267)) %>% dplyr::select(rwEYSTI) %>% count()
rwEYSTI%>% filter(site %in% c(62:158, 248:267), species == "ACRU") %>% dplyr::select(rwEYSTI) %>% filter (rwEYSTI == 0) %>% count() / 
  rwEYSTI%>% filter(site %in% c(62:158, 248:267), species == "ACRU") %>% dplyr::select(rwEYSTI) %>% count()

# delete data before 1900 (no climate data avaiable) ---------------------------
rwEYST <- rwEYST %>% filter (year >= 1900)

# write file with ring widths --------------------------------------------------
write_csv(rwEYST, file = '../data/growth/chronologyData/RW_all_sites_1900-2020.csv')

# clean up ---------------------------------------------------------------------
rm (i, fPath, filename, PLOT, VERBOSE, Quebec, tempData)
#===============================================================================