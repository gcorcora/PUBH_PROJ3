#PUBH8472 Project 3

#Question: did COVID-19 result in a lasting spatial shift in robbery locations in Washington, DC, from 2020-2024 compared to pre-COVID years?

#using time as a covariate? 
#or using two stage, comparing across three times?

set.seed(2577) #setting seed this time! yay!

#libraries
library(ggplot2)
library(spatstat)
library(tibble)
library(knitr)
library(sf)
library(tibble)
library(dplyr)
library(tidycensus)
library(nimble)
library(tidyverse)
library(cowplot)


#crime open database data
crime_2018 <- read.csv('/Users/gretchen/Desktop/PUBH8472/Crime_Incidents_in_2018.csv')
crime_2019 <- read.csv('/Users/gretchen/Desktop/PUBH8472/Crime_Incidents_in_2019.csv')
crime_2020 <- read.csv('/Users/gretchen/Desktop/PUBH8472/Crime_Incidents_in_2020.csv')
crime_2021 <- read.csv('/Users/gretchen/Desktop/PUBH8472/Crime_Incidents_in_2021.csv')
crime_2022 <- read.csv('/Users/gretchen/Desktop/PUBH8472/Crime_Incidents_in_2022.csv')
crime_2023 <- read.csv('/Users/gretchen/Desktop/PUBH8472/Crime_Incidents_in_2023.csv')
crime_2024 <- read.csv('/Users/gretchen/Desktop/PUBH8472/Crime_Incidents_in_2024.csv')
dc_shp <- st_read('/Users/gretchen/Desktop/PUBH8472/Washington_DC_Boundary/Washington_DC_Boundary.shp')

# common coordinate reference system to use for points and shapefile
crs_use <- 32100

#applying coord system
crime_2018_sf <- crime_2018 %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%  #X is longitude, Y is latitude
  st_transform(crs = crs_use)
crime_2019_sf <- crime_2019 %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%  #X is longitude, Y is latitude
  st_transform(crs = crs_use)
crime_2020_sf <- crime_2020 %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%  #X is longitude, Y is latitude
  st_transform(crs = crs_use)
crime_2021_sf <- crime_2021 %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%  #X is longitude, Y is latitude
  st_transform(crs = crs_use)
crime_2022_sf <- crime_2022 %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%  #X is longitude, Y is latitude
  st_transform(crs = crs_use)
crime_2023_sf <- crime_2023 %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%  #X is longitude, Y is latitude
  st_transform(crs = crs_use)
crime_2024_sf <- crime_2024 %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%  #X is longitude, Y is latitude
  st_transform(crs = crs_use)

#applying coord system to shapefiles
dc_shp <- dc_shp %>% 
  st_transform(crs = crs_use)

#creating window as shapefile boundaries
window <- as.owin(dc_shp)

#separate into pre-covid, covid (2020), and post-covid (2021-2024) - filter by burglary
crime_pre_covid <- bind_rows(crime_2018_sf, crime_2019_sf) %>%
  filter(OFFENSE == "ROBBERY")
crime_covid <- bind_rows(crime_2020_sf, crime_2021_sf) %>%
  filter(OFFENSE == "ROBBERY")
crime_post_covid <- bind_rows(crime_2022_sf, crime_2023_sf) %>%
  filter(OFFENSE == "ROBBERY")

#combine into one dataset
crime_pre_covid$timeline <- "pre-covid"
crime_covid$timeline <- "covid"
crime_post_covid$timeline <- "post-covid"

robbery_data <- bind_rows(crime_pre_covid, crime_covid, crime_post_covid)

robbery_data$timeline <- factor(
  robbery_data$timeline,
  levels = c("pre-covid", "covid", "post-covid")
)

#plotting crime - let's do three graphs side by side?
# plot
ggplot(dc_shp) + 
  geom_sf(fill = 'white') + 
  geom_sf(data = robbery_data, aes(color = timeline), size = 1, alpha = 0.6) + 
  scale_color_viridis_d(option = "D") +
  facet_wrap(~timeline) + 
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 12)
  )

robbery_wgs <- st_transform(robbery_data, crs = 4326)
st_write(robbery_wgs, "robbery_by_period.gpkg", layer = "robbery", delete_layer = TRUE)



window <- as.owin(dc_shp)

#make into point pattern
ppp_pre_covid <- as.ppp(st_geometry(crime_pre_covid), W = window)
#says are duplicates but is crimes occuring at same point
ppp_covid <- as.ppp(st_geometry(crime_covid), W = window)
ppp_post_covid <- as.ppp(st_geometry(crime_post_covid), W=window)

#deviations from spatial randomness
L_pre_covid <- Lest(ppp_pre_covid)

L_covid <- Lest(ppp_covid)

L_post_covid <- Lest(ppp_post_covid)

#graphs of L functions
par(mfrow = c(1, 3))
plot(L_pre_covid, main = 'Pre-Covid (2018-2019)')
plot(L_covid, main = 'Covid (2020-2021)')
plot(L_post_covid, main = 'Post-Covid (2022-2023)')

#envelopes
#only border correction so it doesn't take 24 hours to run
pre_envelope <- envelope(ppp_pre_covid, fun = Lest, nsim = 99, correction = "border", global = TRUE) #global avoids type1 error?
covid_envelope <- envelope(ppp_covid, fun = Lest, nsim = 99, correction = "border", global = TRUE)
post_envelope <- envelope(ppp_post_covid, fun = Lest, nsim = 99, correction = "border",global = TRUE)

par(mfrow = c(1, 3))
plot(pre_envelope, main = "Pre-COVID")
plot(covid_envelope, main = "COVID")
plot(post_envelope, main = "Post-COVID")

#let's compare densities
dens_pre <- density.ppp(ppp_pre_covid)
plot(dens_pre, main = "pre-covid")

dens_covid <- density.ppp(ppp_covid)
plot(dens_covid, main = "covid")

dens_post <- density.ppp(ppp_post_covid)
plot(dens_post, main = "post-covid")

par(mfrow = c(1, 3),  mar = c(12, 1, 2, 2))
plot(dens_pre, main = "Pre-COVID Robbery Density")
plot(dens_covid, main = "COVID Robbery Density")
plot(dens_post, main = "Post-COVID Robbery Density")

#comparing intensities
#jsd function calls kld
kld <- function (v1, v2, base = exp(1), normalize = TRUE) {
  non_zero_both = which(v1 > 0 & v2 > 0)
  5
  if (length(non_zero_both) == 0)
    stop(paste("vectors have no common entries with nonzero values-",
               "KLD cannot be computed"))
  v1_both = v1[non_zero_both]
  v2_both = v2[non_zero_both]
  if (normalize) {
    v1_both = v1_both/sum(v1_both)
    v2_both = v2_both/sum(v2_both)
  }
  return(sum(v1_both * (logb(v1_both, base) - logb(v2_both,
                                                   base))))
}

#jsd function
jsd <- function (v1, v2, base = 2, normalize = TRUE) {
  if (normalize) {
    v1 = v1/sum(v1)
    v2 = v2/sum(v2)
  }
  M = 0.5 * (v1 + v2)
  return(sqrt(0.5 * (kld(v1, M, base = base) +
                       kld(v2, M, base = base))))
}

#now put all on same scale into data frame?
all_densities <- data.frame(y = rep(dens_pre$yrow,
                                    times = dens_pre$dim[2]),
                            x = rep(dens_pre$xcol,
                                    each = dens_pre$dim[1]),
                            dens = c(dens_pre$v,
                                     dens_covid$v,
                                     dens_post$v),
                            pp = rep(c('pp_pre_covid', 'pp_covid', 'pp_post_covid'), each = length(dens_pre$v)))

#removing NAs
dens_pre_v <- dens_pre$v[!is.na(dens_pre$v)]
dens_covid_v <- dens_covid$v[!is.na(dens_covid$v)]
dens_post_v <- dens_post$v[!is.na(dens_post$v)]

#calculate JSD
pre_v_covid <- jsd(dens_pre_v, dens_covid_v)
pre_v_post <- jsd(dens_pre_v, dens_post_v)
covid_v_post <- jsd(dens_covid_v, dens_post_v)

#table
jsd_res <- tibble(
  Comparison = c("Pre-Covid v Covid",
                 "Pre-Covid v Post-Covid",
                 "Covid v Post-Covid"),
  JSD = c(pre_v_covid,
          pre_v_post,
          covid_v_post)
)
#print table
kable(jsd_res, digits = 4, caption = "Jensen-Shannon distance comparison")

#log relative risk - REMEMBER TO TALK ABOUT THIS ON LOG SCALE/ EXPONENTIATE!!! NOT RAW NUMS!
par(mfrow = c(1, 3),  mar = c(12, 1, 2, 2))

log_rel_pre_v_cov <- dens_pre 
log_rel_pre_v_cov$v <- log(dens_pre$v/dens_covid$v)

plot1 <- plot(log_rel_pre_v_cov,
     main = "Log-Rel Risk: Pre-COVID v COVID",
     col = rev(terrain.colors(256)), ###make sure to change coloring
     ribbon = TRUE)

log_rel_pre_v_post <- dens_pre 
log_rel_pre_v_post$v <- log(dens_pre$v/dens_post$v)

plot2 <- plot(log_rel_pre_v_post,
     main = "Log-Rel: Pre-COVID v Post-Covid",
     col = rev(terrain.colors(256)), ###make sure to change coloring
     ribbon = TRUE)

log_rel_cov_v_post <- dens_pre 
log_rel_cov_v_post$v <- log(dens_covid$v/dens_post$v)

plot3 <- plot(log_rel_cov_v_post,
     main = "Log-Rel Risk: COVID v Post-Covid",
     col = rev(terrain.colors(256)), ###make sure to change coloring
     ribbon = TRUE)
