#===============================================================================
# Script to wrangle daily simulated meteorological data for each site as 
# simulated by BioSIM and derive monthly climate variables.
#-------------------------------------------------------------------------------

# load dependencies
#-------------------------------------------------------------------------------
if (!existsFunction ('%>%')) library ('tidyverse') # to generally process data
if (!existsFunction ('as_date')) library ('lubridate') # to use as_date function
if (!existsFunction ('thornthwaite')) library ('SPEI') # to calculate the potential evapotranspiration
if (!existsFunction ('pdsi')) library ('scPDSI') # to calculate the scPDSI

# load ring width data from all sites ------------------------------------------
if (!exists('rwEYSTI')) source ('wrangleGrowthData.R')

# define directory for simulated climate data (BioSIM) -------------------------
dir <- '../data/climate/BioClim/'

# read summarised monthly data as generated by BioSIM --------------------------
tmp1 <- read_csv (paste0(dir, "Export (SummariseOutputs1900-1950).csv"),
                  col_types = cols())
tmp2 <- read_csv (paste0(dir, "Export (SummariseOutputs1951-1980).csv"),
                  col_types = cols()) %>% dplyr::select(1:40)
tmp3 <- read_csv (paste0(dir, "Export (SummariseOutputs1981-2020).csv"),
                  col_types = cols()) %>% dplyr::select(1:40)
tmp <- rbind(tmp1, tmp2, tmp3) %>% 
  dplyr::select(-c(Replication, P, Name, Country, State)) %>% 
  arrange(KeyID, Latitude, Longitude, Year, Month) %>% 
  mutate(date = paste(lubridate::month(as.numeric (Month), 
                                       label = TRUE, 
                                       abbr = TRUE), 
                      Year, sep = "-"))

# pivot data wide to format used by Drew's code --------------------------------
tas_m <- tmp %>% 
  pivot_wider (id_cols = 1:3,
               values_from = `Air Temperature`,
               names_from = date) %>% 
  rename (site = "KeyID") %>% dplyr::select (-Latitude, -Longitude)
tmin_m <- tmp %>% 
  pivot_wider (id_cols = 1:3,
               values_from = `Minimum Air Temperature`,
               names_from = date) %>% 
  rename (site = "KeyID") %>% dplyr::select (-Latitude, -Longitude)
tmax_m <- tmp %>% 
  pivot_wider (id_cols = 1:3,
               values_from = `Maximum Air Temperature`,
               names_from = date) %>% 
  rename (site = "KeyID") %>% dplyr::select (-Latitude, -Longitude)
pre_m <- tmp %>% 
  pivot_wider (id_cols = 1:3,
               values_from = `Total Precipitation`,
               names_from = date) %>% 
  rename (site = "KeyID") %>% dplyr::select (-Latitude, -Longitude)

# combine siteMetaData with climate data ---------------------------------------
tmin_m <- full_join (siteMetaData, tmin_m, by = c ("site"))
tmax_m <- full_join (siteMetaData, tmax_m, by = c ("site"))
tas_m <- full_join (siteMetaData, tas_m, by = c ("site"))
pre_m <- full_join (siteMetaData, pre_m, by = c ("site"))

# calculate monthly self-calibrating and standadr Palmer Drought Severity Index 
for (s in 1:dim(siteMetaData)[1]) {
  
  # estimate potential evapotransporation [mm] ---------------------------------
  PET <- thornthwaite(Tave = t(tas_m[s, 19:1470]), 
                      lat = siteMetaData$lat[s],
                      na.rm = TRUE)
  PET <- as.numeric(PET)
  
  # calculate self-calibrating Palmer Drought Severity Index -------------------
  scpdsi_tmp <- pdsi(P = unlist(pre_m[s, 19:1470]),
                     PE = PET,
                     sc = TRUE)
  pdsi_tmp <- pdsi(P = unlist(pre_m[s, 19:1470]),
                   PE = PET,
                   sc = FALSE)
  
  # append self-calibrated PDSI data -------------------------------------------
  if (s == 1) {
    pdsi_m <- as_tibble(t(pdsi_tmp$X))
    scpdsi_m <- as_tibble(t(scpdsi_tmp$X))
  } else {
    pdsi_m <- add_row(pdsi_m, as_tibble(t(pdsi_tmp$X)))
    scpdsi_m <- add_row(scpdsi_m, as_tibble(t(scpdsi_tmp$X)))
  }
}

# change names of monthly scPDSI data for consistency with temperature and 
# precipitation data -----------------------------------------------------------
names (scpdsi_m) <- paste (
  rep (c ("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), 120),
  rep (1900:2020, each = 12), sep = "-")
names (pdsi_m) <- paste (
  rep (c ("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), 120),
  rep (1900:2020, each = 12), sep = "-")

# add siteMetaData to sitePDSIData to have same format as siteTempData and 
# sitePrecData -----------------------------------------------------------------
scpdsi_m <- add_column (siteMetaData, scpdsi_m)
pdsi_m <- add_column (siteMetaData, pdsi_m)

# write processed climate variables --------------------------------------------
write_csv (tmin_m, "../data/climate/processed/tmin_m.csv")
write_csv (tas_m, "../data/climate/processed/tas_m.csv")
write_csv (tmax_m, "../data/climate/processed/tmax_m.csv")
write_csv (pre_m, "../data/climate/processed/pre_m.csv")
write_csv (scpdsi_m, "../data/climate/processed/scpdsi_m.csv")
write_csv (pdsi_m, "../data/climate/processed/pdsi_m.csv")

# clean-up ---------------------------------------------------------------------
rm (PDSI_tmp, scPDSI_tmp, tmp, tmp1, tmp2, tmp3, dir, PET, s)
#===============================================================================