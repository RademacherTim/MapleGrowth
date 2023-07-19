#===============================================================================
# Scriot to explore differences in sites climate and PDSI data to identify 
# droughts with the subsequent aim of looking at drought impacts on maple growth
#-------------------------------------------------------------------------------

# load dependencies ------------------------------------------------------------
if (!existsFunction ('%>%')) library ('tidyverse') # to generally process data

# load climate data ------------------------------------------------------------
if (!exists('sitePDSIData')) source ('wrangleBioSIMclimate.R')

# pivot meteorological data ----------------------------------------------------
tmp_pdsi <- pdsi_m %>% 
  dplyr::select(site, species, lat, lon, 19:1470) %>%
  pivot_longer(cols = 5:1456, names_sep = '-', names_to = c('month', 'year'), 
               values_to = 'pdsi') %>%
  filter(year > 1990 & year <= 2020) %>% 
  filter(month %in% c('May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct')) %>%
  group_by(site, species) %>%
  summarise(pdsi = mean(pdsi), lon = mean(lon), .groups = 'keep')

tmp_tmin <- tmin_m %>% 
  dplyr::select(site, species, lat, lon, 19:1470) %>%
  pivot_longer(cols = 5:1456, names_sep = '-', names_to = c('month', 'year'), 
               values_to = 'tmin') %>%
  filter(year > 1990 & year <= 2020) %>% 
  group_by(site, species) %>%
  summarise(tmin = mean(tmin), lon = mean(lon), .groups = 'keep')

tmp_tas <- tas_m %>% 
  dplyr::select(site, species, lat, lon, 19:1470) %>%
  pivot_longer(cols = 5:1456, names_sep = '-', names_to = c('month', 'year'), 
               values_to = 'tas') %>%
  filter(year > 1990 & year <= 2020) %>% 
  group_by(site, species) %>%
  summarise(tas = mean(tas), lon = mean(lon), .groups = 'keep')

tmp_tmax <- tmax_m %>% 
  dplyr::select(site, species, lat, lon, 19:1470) %>%
  pivot_longer(cols = 5:1456, names_sep = '-', names_to = c('month', 'year'), 
               values_to = 'tmax') %>%
  filter(year > 1990 & year <= 2020) %>% 
  group_by(site, species) %>%
  summarise(tmax = mean(tmax), lon = mean(lon), .groups = 'keep')

tmp_pre <- pre_m %>% 
  dplyr::select(site, species, lat, lon, 19:1470) %>%
  pivot_longer(cols = 5:1456, names_sep = '-', names_to = c('month', 'year'), 
               values_to = 'pre') %>%
  filter(year > 1990 & year <= 2020) %>% 
  group_by(site, species) %>%
  summarise(pre = sum(pre), lon = mean(lon), .groups = 'keep')

# summarise annual temperatures ------------------------------------------------
clm <- full_join(tmp_pdsi, tmp_pre, by = c('site', 'species', 'lon')) %>%
  full_join(., tmp_tmin, by = c('site', 'species', 'lon')) %>%
  full_join(., tmp_tas, by = c('site', 'species', 'lon')) %>%
  full_join(., tmp_tmax, by = c('site', 'species', 'lon'))

# determine region and dryness -------------------------------------------------
clm <- clm %>% 
  mutate(region = case_when(species == 'ACRU' ~ 21,
                            species == 'ACSA' & lon <  -80 ~ 24,
                            species == 'ACSA' & lon >= -80 ~ 25),
         dryness = case_when(
           pdsi < -1.0 ~ '#67000d',
           pdsi < -0.5 ~ '#ef3b2c',
           pdsi <  0.0 ~ '#fc9272',
           pdsi <  0.5 ~ '#eff3ff',
           pdsi <  1.0 ~ '#c6dbef',
           pdsi <  1.5 ~ '#9ecae1',
           pdsi <  2.0 ~ '#6baed6',
           pdsi <  2.5 ~ '#4292c6',
           pdsi <  3.0 ~ '#2171b5',
           pdsi <  3.5 ~ '#084594'))

# plot the climatic niche of each site -----------------------------------------
par(mar = c(5, 5, 1, 1))
plot(x = clm$tas, y = clm$pre,
     pch = 19, las = 1, xlim = c(0, 15), ylim = c(600, 1800), 
     col = 'white', cex = 2,
     xlab = expression(paste('Mean annual temperature (',degree,'C)', sep = '')),
     ylab = 'Total annual precipitation (mm)', axes = FALSE)
axis(side = 1)
axis(side = 2, las = 1)
# add red maple sites
points(x = clm$tas[clm$region == 21], y = clm$pre[clm$region == 21], pch = 21, 
       col = '#88888888', bg = clm$dryness, 
       lwd = 2, cex = 1.5)
# add western sugar maple sites
points(x = clm$tas[clm$region == 24], y = clm$pre[clm$region == 24], pch = 24, 
       col = '#88888888', bg = clm$dryness, lwd = 2, cex = 1.5)
# add eastern sugar maple sites
points(x = clm$tas[clm$region == 25], y = clm$pre[clm$region == 25], pch = 25, 
       col = '#88888888', bg = clm$dryness, lwd = 2, cex = 1.5)
legend(x = 2.5, y = 1800, bg = 'transparent', box.lty = 0, 
       legend = c('ACSA: West', 'ACSA: East', 'ACRU'), pch = c(24, 25, 21) , 
       col = '#88888888', lwd = 2, cex = 1.2, lty = 0)
legend(x = -0.3, y = 1800, box.lty = 0, title = 'Growing season PDSI',
       legend = c('        x <= -1.0', '-1.0 < x <= -0.5', '-0.5 < x <= 0.0', 
                  ' 0.0 < x <= 0.5', ' 0.5 < x <=  1.0', ' 1.0 < x <= 1.5', 
                  ' 1.5 < x <= 2.0', ' 2.0 < x <=  2.5', ' 2.5 < x <= 3.0', 
                  ' 3.0 < x <= 3.5'), 
       pch = 22, col = '#88888888', 
       pt.bg = c('#67000d', '#ef3b2c', '#fc9272', '#eff3ff', '#c6dbef', 
                 '#9ecae1', '#6baed6', '#4292c6', '#2171b5', '#084594'), 
       lwd = 2, lty = 0)
