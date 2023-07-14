# Script to calculate basic stats for the growth data
library('zoo')

# plot growth against estimated age of the tree --------------------------------
par (mar = c(5, 5, 1, 1))
plot(x = rwEYST$age[rwEYST$species == "ACSA"],
     y = rwEYST$rwEYST[rwEYST$species == "ACSA"],
     pch = 19, col = '#f3bd4811', axes = FALSE, ylim = c(0, 10), 
     xlab = 'Âge (année)', ylab = 'Croissance radiale (mm)')
points(x = rwEYST$age[rwEYST$species == "ACRU"],
       y = rwEYST$rwEYST[rwEYST$species == "ACRU"],
       pch = 19, col = '#901c3b11')
axis(side = 1)
axis(side = 2, las = 1)
modACSA <- lm(rwEYST$rwEYST[rwEYST$species == 'ACSA'] ~ 
                rwEYST$age[rwEYST$species == 'ACSA'])
summary(modACSA)
modACRU <- lm(rwEYST$rwEYST[rwEYST$species == 'ACRU'] ~ 
                rwEYST$age[rwEYST$species == 'ACRU'])
summary(modACRU)
abline(modACSA, lwd = 2, col = '#f3bd48')
abline(modACRU, lwd = 2, col = '#901c3b')
# Average effect of 1.6 microns per year less

# plot growth against estimated size of the tree -------------------------------
plot(x = rwEYST$rw_cum[rwEYST$species == "ACSA"] / 10 * 2,
     y = rwEYST$rwEYST[rwEYST$species == "ACSA"],
     pch = 19, col = '#f3bd4811', axes = FALSE, ylim = c(0, 10), 
     xlab = 'Diameter (cm)', ylab = 'Croissance radiale (mm)')
points(x = rwEYST$rw_cum[rwEYST$species == "ACRU"] / 10 * 2,
       y = rwEYST$rwEYST[rwEYST$species == "ACRU"],
       pch = 19, col = '#901c3b11')
axis(side = 1)
axis(side = 2, las = 1)
modACRU <- lm(rwEYST$rwEYST[rwEYST$species == 'ACRU'] ~ 
                rwEYST$age[rwEYST$species == 'ACRU'])
summary(modACRU)
abline(modACSA, lwd = 2, col = '#f3bd48')
abline(modACRU, lwd = 2, col = '#901c3b')
# TR - Need to add a mean growth for each size

# plot age against size --------------------------------------------------------
plot(x = rwEYST$age[rwEYST$species == "ACSA"],
     y = rwEYST$rw_cum[rwEYST$species == "ACSA"] / 10 * 2,
     pch = 19, col = '#f3bd4811', axes = FALSE, #ylim = c(0, 10), 
     ylab = 'Diameter (cm)', xlab = 'Âge (année)')
points(x = rwEYST$age[rwEYST$species == "ACRU"],
       y = rwEYST$rw_cum[rwEYST$species == "ACRU"] / 10 * 2,
       pch = 19, col = '#901c3b11')
axis(side = 1)
axis(side = 2, las = 1)
abline(a = 0, b = 0.05, lwd = 2, lty = 2, col = '#444444')
abline(a = 0, b = 0.8, lwd = 2, lty = 2, col = '#444444')

# plot mean growth by calendar year --------------------------------------------
plot(x = rwEYST$year[rwEYST$year <= 2016],
     y = rwEYST$rwEYST[rwEYST$year <= 2016], col = '#11111111', pch = 19,
     axes = FALSE, xlab = "Année", ylab = "Croissance radiale (mm)", 
     ylim=c(0, 9))
#points(x = rwEYST$year[rwEYST$species == "ACRU"],
#       y = rwEYST$rwEYST[rwEYST$species == "ACRU"], col = '#901c3b11', pch = 19)
lines(x = rwEYST %>% filter(species == "ACRU") %>% 
        filter(year <= 2007) %>% 
        group_by(year) %>% 
        summarise(RW = mean(rwEYST, na.rm = TRUE)) %>%
        select(year) %>% unlist(),
      y = rwEYST %>% filter(species == "ACRU") %>% 
        filter(year <= 2007) %>% 
        group_by(year) %>% 
        summarise(RW = mean(rwEYST, na.rm = TRUE)) %>%
        select(RW) %>% unlist(), 
      col = "#901c3b", lwd = 2)
lines(x = rwEYST %>% filter(species == "ACSA") %>% 
        filter(year <= 2016) %>% 
        group_by(year) %>% 
        summarise(RW = mean(rwEYST, na.rm = TRUE)) %>%
        select(year) %>% unlist(),
      y = rwEYST %>% filter(species == "ACSA") %>% 
        filter(year <= 2016) %>% 
        group_by(year) %>% 
        summarise(RW = mean(rwEYST, na.rm = TRUE)) %>%
        select(RW) %>% unlist(), 
      col = "#f3bd48", lwd = 2)
axis(side = 1)
axis(side = 2, las = 1)

# plot mean growth by calendar year --------------------------------------------
Quebec <- c(62:158, 248:267)
plot(x = rwEYST$year[rwEYST$year <= 2016 & rwEYST$site %in% Quebec],
     y = rwEYST$rwEYST[rwEYST$year <= 2016 & rwEYST$site %in% Quebec], 
     col = '#11111111', pch = 19,
     axes = FALSE, xlab = "Année", ylab = "Croissance radiale (mm)", 
     ylim=c(0, 9))
# Add growth for Quebec --------------------------------------------------------
lines(x = rwEYST %>% filter(species == "ACRU") %>% 
        filter(year <= 2020 & year > 1890) %>% 
        filter(site %in% Quebec) %>% 
        group_by(year) %>% 
        summarise(RW = mean(rwEYST, na.rm = TRUE)) %>%
        select(year) %>% unlist(),
      y = rwEYST %>% filter(species == "ACRU") %>% 
        filter(year <= 2020 & year > 1890) %>% 
        filter(site %in% Quebec) %>% 
        group_by(year) %>% 
        summarise(RW = mean(rwEYST, na.rm = TRUE)) %>%
        select(RW) %>% unlist(), 
      col = "#901c3b", lwd = 2)
lines(x = rwEYST %>% filter(species == "ACSA") %>% 
        filter(year <= 2018) %>% 
        filter(site %in% Quebec) %>% 
        group_by(year) %>% 
        summarise(RW = mean(rwEYST, na.rm = TRUE)) %>%
        select(year) %>% unlist(),
      y = rwEYST %>% filter(species == "ACSA") %>% 
        filter(year <= 2018) %>% 
        filter(site %in% Quebec) %>% 
        group_by(year) %>% 
        summarise(RW = mean(rwEYST, na.rm = TRUE)) %>%
        select(RW) %>% unlist(), 
      col = "#f3bd48", lwd = 2)
axis(side = 1)
axis(side = 2, las = 1)

# plot growth versus latitude --------------------------------------------------
plot(x = rwEYST$lat[rwEYST$species == "ACSA"], 
     y = rwEYST$rwEYST[rwEYST$species == "ACSA"], pch = 19, col = "#f3bd4811",
     xlab = "Latitude", ylab = "Croissance radiale (mm)", axes = FALSE, 
     xlim = c(35, 50), ylim=c(0, 20))
points(x = rwEYST$lat[rwEYST$species == "ACRU"], 
       y = rwEYST$rwEYST[rwEYST$species == "ACRU"], pch = 19, col = "#901c3b11",)
axis(side = 1, seq (35, 50, by = 5))
axis(side = 2, las = 1)
#abline(h = 5, col = "#f3bd48", lty = 2, lwd = 2)

# add previous year growth to the tibble ---------------------------------------
rwEYST <- rwEYST %>% group_by(treeID) %>% arrange(year) %>% 
  mutate(ac1 = lag(rwEYST, default = first(0))) %>% ungroup()

# plot growth of current versus previous year ----------------------------------
plot(x = rwEYST$ac1[rwEYST$species == 'ACSA' & rwEYST$ac1 != 0],
     y = rwEYST$rwEYST[rwEYST$species == 'ACSA' & rwEYST$ac1 != 0],
     pch = 19, col = '#f3bd4811', axes = FALSE, 
     xlim = c(0, 10), ylim = c(0, 10),
     xlab = 'Croissance radiale précédente (mm)', 
     ylab = 'Croissance radiale (mm)')
points(x = rwEYST$ac1[rwEYST$species == 'ACRU' & rwEYST$ac1 != 0],
       y = rwEYST$rwEYST[rwEYST$species == 'ACRU' & rwEYST$ac1 != 0],
       pch = 19, col = '#901c3b11')
axis (side = 1)
axis (side = 2, las = 1)
modACSA <- lm(rwEYST$rwEYST[rwEYST$species == 'ACSA' & rwEYST$ac1 != 0] ~ 
                rwEYST$ac1[rwEYST$species == 'ACSA' & rwEYST$ac1 != 0])
summary(modACSA)
modACRU <- lm(rwEYST$rwEYST[rwEYST$species == 'ACRU' & rwEYST$ac1 != 0] ~ 
                rwEYST$ac1[rwEYST$species == 'ACRU' & rwEYST$ac1 != 0])
summary(modACRU)
abline(a = 0, b = 1, lwd = 2, lty = 2, col = '#444444')
abline(modACSA, lwd = 2, col = '#f3bd48')
abline(modACRU, lwd = 2, col = '#901c3b')
