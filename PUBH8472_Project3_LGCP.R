#PUBH8472_Project 3 take 2
#PUBH8472 Project 3

##This is the one that worked

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
crs_use <- 26985

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

dc_shp <- dc_shp %>% 
  st_transform(crs = crs_use)

window <- as.owin(dc_shp)

#separate into pre-covid, covid (2020), and post-covid (2021-2024) - filter by robbery
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

#for plotting in QGIS
robbery_wgs <- st_transform(robbery_data, crs = 4326)
st_write(robbery_wgs, "robbery_by_period.gpkg", layer = "robbery", delete_layer = TRUE)

window <- as.owin(dc_shp)

##read in covariates
#metro stations will be covariate
metro <- st_read("/Users/gretchen/Desktop/PUBH8472/Metro_Stations_in_DC/Metro_Stations_in_DC.shp")
metro <- st_transform(metro, crs = crs_use)
metro_matrix <- st_coordinates(metro)

business <- st_read("/Users/gretchen/Desktop/PUBH8472/Business_Improvement_Districts/Business_Improvement_Districts.shp")
business <- st_transform(business, crs = crs_use)
central_business <- st_read("/Users/gretchen/Desktop/PUBH8472/DDOT_Central_Business_District/DDOT_Central_Business_District.shp")
central_business <- st_transform(central_business, crs = crs_use)
census_shp <- st_read('/Users/gretchen/Desktop/PUBH8472/Census_Tracts_in_2020/Census_Tracts_in_2020.shp')
census_shp <- st_transform(census_shp, crs = crs_use)
census_pop <- read.csv('/Users/gretchen/Desktop/PUBH8472/Census_Tracts_in_2020.csv')

#merge
#merge didn't work because one is character other is double, fix it
census_shp$GEOID <- as.character(census_shp$GEOID)
census_pop$GEOID <- as.character(census_shp$GEOID)
census <- left_join(census_shp, census_pop, by = "GEOID")
#because in meters, not kiloms
census$pop_density <- census$POP100.x / (as.numeric(st_area(census)) / 1e6)


#let's set up a subset of just pre_covid and covid, one for covid and post covid, and one for pre and post covid
robbery_pre_post <- robbery_data %>%
  filter(timeline %in% c("pre-covid", "post-covid"))
robbery_covid_post <- robbery_data %>%
  filter(timeline %in% c("covid", "post-covid"))
robbery_pre_covid <- robbery_data %>%
  filter(timeline %in% c("pre-covid", "covid"))


#get knots and integration points
knots <- expand.grid(
  x = seq(window$xrange[1], window$xrange[2], length.out = 15),
  y = seq(window$yrange[1], window$yrange[2], length.out = 15)
)
knots <- knots[inside.owin(knots$x, knots$y, window), ]

integration_points <- expand.grid(
  x = seq(window$xrange[1], window$xrange[2], length.out = 50),
  y = seq(window$yrange[1], window$yrange[2], length.out = 50)
)
integration_points <- integration_points[inside.owin(integration_points$x, integration_points$y, window), ]

#scaling due to phi
phi <- 3 / 1000

knots$x <- knots$x / 1000
knots$y <- knots$y / 1000


int_sf <- st_as_sf(integration_points, coords = c("x", "y"), crs = crs_use)
integration_points$business_dist <- as.integer(lengths(st_intersects(int_sf, business)) > 0)
integration_points$central_business <- as.integer(lengths(st_intersects(int_sf, central_business)) > 0)
integration_points$dist_to_metro <- apply(st_coordinates(int_sf), 1, function(pt) {
  min(sqrt((pt[1] - metro_matrix[,1])^2 + (pt[2] - metro_matrix[,2])^2))
})

# Step 1: Try intersecting with a tiny buffer to catch border cases
census_matches <- st_intersects(st_buffer(int_sf, dist = 0.001), census)

# Step 2: Assign pop_density from intersected polygons
integration_points$pop_density <- sapply(census_matches, function(i) {
  if (length(i) == 0) NA_real_ else census$pop_density[i[1]]
})

# Step 3: Find which rows are still NA
na_idx <- which(is.na(integration_points$pop_density))

# Step 4: Use nearest polygon to fill in missing ones
if (length(na_idx) > 0) {
  nearest_index <- st_nearest_feature(int_sf[na_idx, ], census)
  integration_points$pop_density[na_idx] <- census$pop_density[nearest_index]
}

#integration points scaling
integration_points$x <- integration_points$x / 1000
integration_points$y <- integration_points$y / 1000

#pre and covid comparison
robbery_sf_1 <- st_as_sf(robbery_pre_covid, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = crs_use)

# Add covariates
robbery_sf_1$business_dist <- as.integer(lengths(st_intersects(robbery_sf_1, business)) > 0)
robbery_sf_1$central_business <- as.integer(lengths(st_intersects(robbery_sf_1, central_business)) > 0)

robbery_sf_1$dist_to_metro <- apply(st_coordinates(robbery_sf_1), 1, function(pt) {
  min(sqrt((pt[1] - metro_matrix[,1])^2 + (pt[2] - metro_matrix[,2])^2))
})

census_matches <- st_intersects(robbery_sf_1, census)
robbery_sf_1$pop_density <- sapply(census_matches, function(i) {
  ifelse(length(i) == 0, NA, census$pop_density[i])
})


#post and covid comparison
robbery_sf_2 <- st_as_sf(robbery_covid_post, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = crs_use)

# Add covariates
robbery_sf_2$business_dist <- as.integer(lengths(st_intersects(robbery_sf_2, business)) > 0)
robbery_sf_2$central_business <- as.integer(lengths(st_intersects(robbery_sf_2, central_business)) > 0)

robbery_sf_2$dist_to_metro <- apply(st_coordinates(robbery_sf_2), 1, function(pt) {
  min(sqrt((pt[1] - metro_matrix[,1])^2 + (pt[2] - metro_matrix[,2])^2))
})

census_matches <- st_intersects(robbery_sf_2, census)
robbery_sf_2$pop_density <- sapply(census_matches, function(i) {
  ifelse(length(i) == 0, NA, census$pop_density[i])
})

#pre and post comparison
robbery_sf_3 <- st_as_sf(robbery_pre_post, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = crs_use)

# Add covariates
robbery_sf_3$business_dist <- as.integer(lengths(st_intersects(robbery_sf_3, business)) > 0)
robbery_sf_3$central_business <- as.integer(lengths(st_intersects(robbery_sf_3, central_business)) > 0)

robbery_sf_3$dist_to_metro <- apply(st_coordinates(robbery_sf_3), 1, function(pt) {
  min(sqrt((pt[1] - metro_matrix[,1])^2 + (pt[2] - metro_matrix[,2])^2))
})

census_matches <- st_intersects(robbery_sf_3, census)
robbery_sf_3$pop_density <- sapply(census_matches, function(i) {
  ifelse(length(i) == 0, NA, census$pop_density[i])
})
nearest_idx_obs <- st_nearest_feature(robbery_sf_3, census)
robbery_sf_3$pop_density <- census$pop_density[nearest_idx_obs]


###MAYBE needed, maybe not - would then have to remake/erase X obs X int for others too
# #perhaps need to scale everything again because having problem with 0 sd
scale_cols <- function(x) scale(x)[,1]
robbery_sf_3$dist_to_metro <- scale_cols(robbery_sf_3$dist_to_metro)
robbery_sf_3$pop_density <- scale_cols(robbery_sf_3$pop_density)
integration_points$dist_to_metro <- scale_cols(integration_points$dist_to_metro)
integration_points$pop_density <- scale_cols(integration_points$pop_density)
X_obs_pre_post <- model.matrix(~ dist_to_metro + pop_density + business_dist + central_business, data = robbery_sf_3)
X_int <- model.matrix(~ dist_to_metro + pop_density + business_dist + central_business, data = integration_points)

#pre_v_covid
coords_obs_pre_covid <- st_coordinates(robbery_sf_1)/1000



# Design matrix for observed points
X_obs_pre_covid <- model.matrix(~ dist_to_metro + pop_density + business_dist + central_business, data = robbery_sf_1)

#post_v_covid
coords_obs_covid_post <- st_coordinates(robbery_sf_2)/1000

# Design matrix for observed points
X_obs_covid_post <- model.matrix(~ dist_to_metro + pop_density + business_dist + central_business, data = robbery_sf_2)

#pre_v_post
coords_obs_pre_post <- st_coordinates(robbery_sf_3)/1000

#verifying integration and knots look good
# Convert back to unscaled for plotting
knots_plot <- knots * 1000
integration_points_plot <- integration_points * 1000

plot(dc_shp$geometry)
points(integration_points_plot, col = 'blue', pch = 16, cex = 0.5)
points(knots_plot, col = 'red', pch = 3, cex = 0.7)


#knots distances
dists_knots <- as.matrix(dist(knots[, c("x", "y")]))

#pre_v_covid
dists_obs_knots_pre_covid <- as.matrix(proxy::dist(
  coords_obs_pre_covid,
  knots[, c("x", "y")]
))

###post_v_covid
dists_obs_knots_covid_post <- as.matrix(proxy::dist(
  coords_obs_covid_post,
  knots[, c("x", "y")]
))

#pre_v_post
dists_obs_knots_pre_post <- as.matrix(proxy::dist(
  coords_obs_pre_post,
  knots[, c("x", "y")]
))

#integration and knots distances
dists_int_knots <- as.matrix(proxy::dist(
  integration_points[, c("x", "y")],
  knots[, c("x", "y")]
))

#redefining here for easy issue fix
robbery_sf_joint <- robbery_sf_3
robbery_sf_joint$Y <- as.numeric(robbery_sf_joint$timeline == "post-covid")
coords_obs_joint <- st_coordinates(robbery_sf_joint) / 1000
X_obs_joint <- model.matrix(~ dist_to_metro + pop_density + business_dist + central_business, data = robbery_sf_joint)
pop_joint <- robbery_sf_joint$pop_density

dists_obs_knots_joint <- as.matrix(proxy::dist(
  coords_obs_joint,
  knots[, c("x", "y")]
))

#exponential
exp_corr <- function(dists, phi) {
  exp(-dists * phi)
}

# C matrix for observed points
Wstar_exp_cov <- exp_corr(dists_knots, phi = phi)
inv_Wstar_exp_cov <- solve(Wstar_exp_cov)
C_cross_obs_joint <- exp_corr(dists_obs_knots_joint, phi = phi)
C_cross_C_inv_obs_joint <- C_cross_obs_joint %*% inv_Wstar_exp_cov
C_cross_C_inv_obs_2_joint <- C_cross_C_inv_obs_joint

exp_cov <- nimbleFunction(
  run = function(dists = double(2), phi = double(0), sigma2 = double(0)) {
    returnType(double(2)) 
    nrow <- dim(dists)[1]
    ncol <- dim(dists)[2]
    result <- matrix(nrow = nrow, ncol = ncol, init = FALSE)
    for(i in 1:nrow) {
      for(j in 1:ncol) {
        result[i, j] <- sigma2 * exp(-dists[i,j] * phi)
      }
    }
    return(result)
  })

d_lgcp <- nimbleFunction(
  run = function(x = double(1), lambda_sA = double(1), lambda_s = double(1),
                 area = double(0), n_A = double(0), log = logical(0, default = 0)) {
    returnType(double())
    lambda_D <- area / n_A * sum(lambda_sA)
    if (log) {
      val <- -lambda_D + sum(log(lambda_s))
      return(val)
    } else {
      # not logged
      val <- exp(-lambda_D) * prod(lambda_s)
      return(val)
    }
  })

registerDistributions(list(
  d_lgcp = list(
    BUGSdist = "d_lgcp(lambda_sA, lambda_s, area, n_A)",
    types = c("value = double(1)",
              "lambda_sA = double(1)",
              "lambda_s = double(1)",
              "area = double(0)",
              "n_A = double(0)"),
    discrete = TRUE
  )
))

# nimble model
# model_code <- nimbleCode({
# 
#     s[1:n_points] ~ d_lgcp(lambda_sA = lambda_sA[1:n_A],
#                            lambda_s = lambda[1:n_points],
#                            area = area,
#                            n_A = n_A)
#     for (i in 1:n_points) {
#       # intensity function
#       log(lambda[i]) <- inprod(beta[1:p], x[i, 1:p]) + W_tilde_1[i] + log(pop[i])
#       # logistic model for marks using same covariates
#       Y[i] ~ dbin(prob = pi[i], size = 1)
#       logit(pi[i]) <- inprod(gamma[1:p], x[i, 1:p]) + W_tilde_2[i]
#     }
#     for (i in 1:n_A) {
#       # lambda at integration points
#       log(lambda_sA[i]) <- inprod(beta[1:p], x_A[i, 1:p]) + W_tilde_A[i] + log(pop_A[i])}
#   
#   Sigma_Wstar[1:n_knots, 1:n_knots] <- sigma2_1 * Wstar_exp_cov[1:n_knots, 1:n_knots]
#   Sigma_Wstar_2[1:n_knots, 1:n_knots] <- sigma2_2 * Wstar_exp_cov[1:n_knots, 1:n_knots]
#   
#   Wstar[1:n_knots] ~ dmnorm(zeros[1:n_knots], cov = Sigma_Wstar[1:n_knots, 1:n_knots])
#   Wstar_2[1:n_knots] ~ dmnorm(zeros[1:n_knots], cov = Sigma_Wstar_2[1:n_knots, 1:n_knots])
#   
#   W_tilde_1[1:n_points] <- (C_cross_obs[1:n_points, 1:n_knots] %*% inv_Wstar_exp_cov[1:n_knots, 1:n_knots]) %*% Wstar[1:n_knots]
#   W_tilde_A[1:n_A] <- (C_cross_int[1:n_A, 1:n_knots] %*% inv_Wstar_exp_cov[1:n_knots, 1:n_knots]) %*% Wstar[1:n_knots]
#   W_tilde_2[1:n_points] <- (C_cross_obs[1:n_points, 1:n_knots] %*% inv_Wstar_exp_cov[1:n_knots, 1:n_knots]) %*% Wstar_2[1:n_knots]
#   
#   for(i in 1:p) {
#     beta[i] ~ dnorm(0, sd = 2)
#     gamma[i] ~ dnorm(0, sd = 2)
#   }
#   
#   sigma2_1 ~ dinvgamma(2, 10)
#   sigma2_2 ~ dinvgamma(2, 10)
# })
model_code <- nimbleCode({
  # --- Point process model (LGCP) ---
  s[1:n_points] ~ d_lgcp(lambda_sA = lambda_sA[1:n_A],
                         lambda_s = lambda[1:n_points],
                         area = area,
                         n_A = n_A)
  
  for (i in 1:n_A) {
    log(lambda_sA[i]) <- inprod(beta[1:p], x_A[i, 1:p]) + W_tilde_A[i]
  }
  
  for (i in 1:n_points) {
    log(lambda[i]) <- inprod(beta[1:p], x[i, 1:p]) + W_tilde[i]
  }
  
  # --- Mark model (logistic regression on Y) ---
  for (i in 1:n_points) {
    logit(pi[i]) <- inprod(gamma[1:p], x[i, 1:p]) + W_tilde_2[i]
    Y[i] ~ dbin(prob = pi[i], size = 1)
  }
  
  # --- Spatial priors for two GPs (LGCP and mark model) ---
  Sigma1[1:n_knots, 1:n_knots] <- sigma2_1 * exp_D[1:n_knots, 1:n_knots]
  Sigma2[1:n_knots, 1:n_knots] <- sigma2_2 * exp_D[1:n_knots, 1:n_knots]
  
  W1[1:n_knots] ~ dmnorm(mean = zeros[1:n_knots], cov = Sigma1[1:n_knots, 1:n_knots])
  W2[1:n_knots] ~ dmnorm(mean = zeros[1:n_knots], cov = Sigma2[1:n_knots, 1:n_knots])
  
  # --- Interpolation of spatial effects ---
  W_tilde[1:n_points]    <- C_obs[1:n_points, 1:n_knots] %*% W1[1:n_knots]
  W_tilde_2[1:n_points]  <- C_obs[1:n_points, 1:n_knots] %*% W2[1:n_knots]
  W_tilde_A[1:n_A]       <- C_int[1:n_A, 1:n_knots] %*% W1[1:n_knots]
  
  # --- Priors ---
  for (j in 1:p) {
    beta[j] ~ dnorm(0, sd = 5)
    gamma[j] ~ dnorm(0, sd = 5)
  }
  
  sigma2_1 ~ dinvgamma(2, 2)
  sigma2_2 ~ dinvgamma(2, 2)
})


#precompute matrices
#knot distances
Wstar_exp_cov <- exp_corr(dists_knots, phi = phi)
inv_Wstar_exp_cov <- solve(Wstar_exp_cov)

robbery_ppp <- as.ppp(st_geometry(robbery_sf_joint), W = window)
robbery_ppp_km <- rescale(robbery_ppp, s = 1000, unitname = "km")

Wstar <- rnorm(nrow(knots))
Wstar_2 <- rnorm(nrow(knots))
C_cross_int <- exp_corr(dists_int_knots, phi = phi)

exp_D <- exp(-phi * dists_knots)


constants_list <- list(
  n_points = nrow(X_obs_pre_post),
  n_A = nrow(X_int),
  n_knots = nrow(knots),
  area = area.owin(window)/1e6,
  x = X_obs_pre_post,
  x_A = X_int,
  p = ncol(X_obs_pre_post),
  C_obs = exp_corr(dists_obs_knots_pre_post, phi),
  C_int = exp_corr(dists_int_knots, phi),
  exp_D = exp_D,
  zeros = rep(0, nrow(knots))
)
data_list <- list(
  Y = as.numeric(robbery_sf_3$timeline == "post-covid"),  # 1=post-covid, 0=pre-covid
  s = rep(1, constants_list$n_points)
)
# inits
#PUBH8472_Project 3 take 2
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
crs_use <- 26985

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

dc_shp <- dc_shp %>% 
  st_transform(crs = crs_use)

window <- as.owin(dc_shp)

#separate into pre-covid, covid (2020), and post-covid (2021-2024) - filter by robbery
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

#for plotting in QGIS
robbery_wgs <- st_transform(robbery_data, crs = 4326)
st_write(robbery_wgs, "robbery_by_period.gpkg", layer = "robbery", delete_layer = TRUE)

window <- as.owin(dc_shp)

##read in covariates
#metro stations will be covariate
metro <- st_read("/Users/gretchen/Desktop/PUBH8472/Metro_Stations_in_DC/Metro_Stations_in_DC.shp")
metro <- st_transform(metro, crs = crs_use)
metro_matrix <- st_coordinates(metro)

business <- st_read("/Users/gretchen/Desktop/PUBH8472/Business_Improvement_Districts/Business_Improvement_Districts.shp")
business <- st_transform(business, crs = crs_use)
central_business <- st_read("/Users/gretchen/Desktop/PUBH8472/DDOT_Central_Business_District/DDOT_Central_Business_District.shp")
central_business <- st_transform(central_business, crs = crs_use)
census_shp <- st_read('/Users/gretchen/Desktop/PUBH8472/Census_Tracts_in_2020/Census_Tracts_in_2020.shp')
census_shp <- st_transform(census_shp, crs = crs_use)
census_pop <- read.csv('/Users/gretchen/Desktop/PUBH8472/Census_Tracts_in_2020.csv')

#merge
#merge didn't work because one is character other is double, fix it
census_shp$GEOID <- as.character(census_shp$GEOID)
census_pop$GEOID <- as.character(census_shp$GEOID)
census <- left_join(census_shp, census_pop, by = "GEOID")
#because in meters, not kiloms
census$pop_density <- census$POP100.x / (as.numeric(st_area(census)) / 1e6)


#let's set up a subset of just pre_covid and covid, one for covid and post covid, and one for pre and post covid
robbery_pre_post <- robbery_data %>%
  filter(timeline %in% c("pre-covid", "post-covid"))
robbery_covid_post <- robbery_data %>%
  filter(timeline %in% c("covid", "post-covid"))
robbery_pre_covid <- robbery_data %>%
  filter(timeline %in% c("pre-covid", "covid"))


#get knots and integration points
knots <- expand.grid(
  x = seq(window$xrange[1], window$xrange[2], length.out = 15),
  y = seq(window$yrange[1], window$yrange[2], length.out = 15)
)
knots <- knots[inside.owin(knots$x, knots$y, window), ]

integration_points <- expand.grid(
  x = seq(window$xrange[1], window$xrange[2], length.out = 50),
  y = seq(window$yrange[1], window$yrange[2], length.out = 50)
)
integration_points <- integration_points[inside.owin(integration_points$x, integration_points$y, window), ]

#scaling due to phi
phi <- 3 / 1000

knots$x <- knots$x / 1000
knots$y <- knots$y / 1000


#adding covariates to integration points
int_sf <- st_as_sf(integration_points, coords = c("x", "y"), crs = crs_use)
integration_points$business_dist <- as.integer(lengths(st_intersects(int_sf, business)) > 0)
integration_points$central_business <- as.integer(lengths(st_intersects(int_sf, central_business)) > 0)
integration_points$dist_to_metro <- apply(st_coordinates(int_sf), 1, function(pt) {
  min(sqrt((pt[1] - metro_matrix[,1])^2 + (pt[2] - metro_matrix[,2])^2))
})

census_matches <- st_intersects(st_buffer(int_sf, dist = 0.001), census)

integration_points$pop_density <- sapply(census_matches, function(i) {
  if (length(i) == 0) NA_real_ else census$pop_density[i[1]]
})

na_idx <- which(is.na(integration_points$pop_density))

if (length(na_idx) > 0) {
  nearest_index <- st_nearest_feature(int_sf[na_idx, ], census)
  integration_points$pop_density[na_idx] <- census$pop_density[nearest_index]
}

#integration points scaling
integration_points$x <- integration_points$x / 1000
integration_points$y <- integration_points$y / 1000

#pre and covid comparison
robbery_sf_1 <- st_as_sf(robbery_pre_covid, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = crs_use)

# Add covariates
robbery_sf_1$business_dist <- as.integer(lengths(st_intersects(robbery_sf_1, business)) > 0)
robbery_sf_1$central_business <- as.integer(lengths(st_intersects(robbery_sf_1, central_business)) > 0)

robbery_sf_1$dist_to_metro <- apply(st_coordinates(robbery_sf_1), 1, function(pt) {
  min(sqrt((pt[1] - metro_matrix[,1])^2 + (pt[2] - metro_matrix[,2])^2))
})

census_matches <- st_intersects(robbery_sf_1, census)
robbery_sf_1$pop_density <- sapply(census_matches, function(i) {
  ifelse(length(i) == 0, NA, census$pop_density[i])
})


#post and covid comparison
robbery_sf_2 <- st_as_sf(robbery_covid_post, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = crs_use)

# Add covariates
robbery_sf_2$business_dist <- as.integer(lengths(st_intersects(robbery_sf_2, business)) > 0)
robbery_sf_2$central_business <- as.integer(lengths(st_intersects(robbery_sf_2, central_business)) > 0)

robbery_sf_2$dist_to_metro <- apply(st_coordinates(robbery_sf_2), 1, function(pt) {
  min(sqrt((pt[1] - metro_matrix[,1])^2 + (pt[2] - metro_matrix[,2])^2))
})

census_matches <- st_intersects(robbery_sf_2, census)
robbery_sf_2$pop_density <- sapply(census_matches, function(i) {
  ifelse(length(i) == 0, NA, census$pop_density[i])
})

#pre and post comparison
robbery_sf_3 <- st_as_sf(robbery_pre_post, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = crs_use)

# Add covariates
robbery_sf_3$business_dist <- as.integer(lengths(st_intersects(robbery_sf_3, business)) > 0)
robbery_sf_3$central_business <- as.integer(lengths(st_intersects(robbery_sf_3, central_business)) > 0)

robbery_sf_3$dist_to_metro <- apply(st_coordinates(robbery_sf_3), 1, function(pt) {
  min(sqrt((pt[1] - metro_matrix[,1])^2 + (pt[2] - metro_matrix[,2])^2))
})

census_matches <- st_intersects(robbery_sf_3, census)
robbery_sf_3$pop_density <- sapply(census_matches, function(i) {
  ifelse(length(i) == 0, NA, census$pop_density[i])
})
nearest_idx_obs <- st_nearest_feature(robbery_sf_3, census)
robbery_sf_3$pop_density <- census$pop_density[nearest_idx_obs]


###MAYBE needed, maybe not - would then have to remake/erase X obs X int for others too
# #perhaps need to scale everything again because having problem with 0 sd
scale_cols <- function(x) scale(x)[,1]
robbery_sf_3$dist_to_metro <- scale_cols(robbery_sf_3$dist_to_metro)
robbery_sf_3$pop_density <- scale_cols(robbery_sf_3$pop_density)
integration_points$dist_to_metro <- scale_cols(integration_points$dist_to_metro)
integration_points$pop_density <- scale_cols(integration_points$pop_density)
X_obs_pre_post <- model.matrix(~ dist_to_metro + pop_density + business_dist + central_business, data = robbery_sf_3)
X_int <- model.matrix(~ dist_to_metro + pop_density + business_dist + central_business, data = integration_points)

#pre_v_covid
coords_obs_pre_covid <- st_coordinates(robbery_sf_1)/1000



# Design matrix for observed points
X_obs_pre_covid <- model.matrix(~ dist_to_metro + pop_density + business_dist + central_business, data = robbery_sf_1)

#post_v_covid
coords_obs_covid_post <- st_coordinates(robbery_sf_2)/1000

# Design matrix for observed points
X_obs_covid_post <- model.matrix(~ dist_to_metro + pop_density + business_dist + central_business, data = robbery_sf_2)

#pre_v_post
coords_obs_pre_post <- st_coordinates(robbery_sf_3)/1000

#verifying integration and knots look good
# Convert back to unscaled for plotting
knots_plot <- knots * 1000
integration_points_plot <- integration_points * 1000

plot(dc_shp$geometry)
points(integration_points_plot, col = 'blue', pch = 16, cex = 0.5)
points(knots_plot, col = 'red', pch = 3, cex = 0.7)


#knots distances
dists_knots <- as.matrix(dist(knots[, c("x", "y")]))

#pre_v_covid
dists_obs_knots_pre_covid <- as.matrix(proxy::dist(
  coords_obs_pre_covid,
  knots[, c("x", "y")]
))

###post_v_covid
dists_obs_knots_covid_post <- as.matrix(proxy::dist(
  coords_obs_covid_post,
  knots[, c("x", "y")]
))

#pre_v_post
dists_obs_knots_pre_post <- as.matrix(proxy::dist(
  coords_obs_pre_post,
  knots[, c("x", "y")]
))

#integration and knots distances
dists_int_knots <- as.matrix(proxy::dist(
  integration_points[, c("x", "y")],
  knots[, c("x", "y")]
))

#redefining here for easy issue fix
robbery_sf_joint <- robbery_sf_3
robbery_sf_joint$Y <- as.numeric(robbery_sf_joint$timeline == "post-covid")
coords_obs_joint <- st_coordinates(robbery_sf_joint) / 1000
X_obs_joint <- model.matrix(~ dist_to_metro + pop_density + business_dist + central_business, data = robbery_sf_joint)
pop_joint <- robbery_sf_joint$pop_density

dists_obs_knots_joint <- as.matrix(proxy::dist(
  coords_obs_joint,
  knots[, c("x", "y")]
))

#exponential
exp_corr <- function(dists, phi) {
  exp(-dists * phi)
}

# C matrix for observed points
Wstar_exp_cov <- exp_corr(dists_knots, phi = phi)
inv_Wstar_exp_cov <- solve(Wstar_exp_cov)
C_cross_obs_joint <- exp_corr(dists_obs_knots_joint, phi = phi)
C_cross_C_inv_obs_joint <- C_cross_obs_joint %*% inv_Wstar_exp_cov
C_cross_C_inv_obs_2_joint <- C_cross_C_inv_obs_joint

exp_cov <- nimbleFunction(
  run = function(dists = double(2), phi = double(0), sigma2 = double(0)) {
    returnType(double(2)) 
    nrow <- dim(dists)[1]
    ncol <- dim(dists)[2]
    result <- matrix(nrow = nrow, ncol = ncol, init = FALSE)
    for(i in 1:nrow) {
      for(j in 1:ncol) {
        result[i, j] <- sigma2 * exp(-dists[i,j] * phi)
      }
    }
    return(result)
  })

d_lgcp <- nimbleFunction(
  run = function(x = double(1), lambda_sA = double(1), lambda_s = double(1),
                 area = double(0), n_A = double(0), log = logical(0, default = 0)) {
    returnType(double())
    lambda_D <- area / n_A * sum(lambda_sA)
    if (log) {
      val <- -lambda_D + sum(log(lambda_s))
      return(val)
    } else {
      # not logged
      val <- exp(-lambda_D) * prod(lambda_s)
      return(val)
    }
  })

#idk if this was actualy needed, was having issues without this
registerDistributions(list(
  d_lgcp = list(
    BUGSdist = "d_lgcp(lambda_sA, lambda_s, area, n_A)",
    types = c("value = double(1)",
              "lambda_sA = double(1)",
              "lambda_s = double(1)",
              "area = double(0)",
              "n_A = double(0)"),
    discrete = TRUE
  )
))

# nimble model
# model_code <- nimbleCode({
# 
#     s[1:n_points] ~ d_lgcp(lambda_sA = lambda_sA[1:n_A],
#                            lambda_s = lambda[1:n_points],
#                            area = area,
#                            n_A = n_A)
#     for (i in 1:n_points) {
#       # intensity function
#       log(lambda[i]) <- inprod(beta[1:p], x[i, 1:p]) + W_tilde_1[i] + log(pop[i])
#       # logistic model for marks using same covariates
#       Y[i] ~ dbin(prob = pi[i], size = 1)
#       logit(pi[i]) <- inprod(gamma[1:p], x[i, 1:p]) + W_tilde_2[i]
#     }
#     for (i in 1:n_A) {
#       # lambda at integration points
#       log(lambda_sA[i]) <- inprod(beta[1:p], x_A[i, 1:p]) + W_tilde_A[i] + log(pop_A[i])}
#   
#   Sigma_Wstar[1:n_knots, 1:n_knots] <- sigma2_1 * Wstar_exp_cov[1:n_knots, 1:n_knots]
#   Sigma_Wstar_2[1:n_knots, 1:n_knots] <- sigma2_2 * Wstar_exp_cov[1:n_knots, 1:n_knots]
#   
#   Wstar[1:n_knots] ~ dmnorm(zeros[1:n_knots], cov = Sigma_Wstar[1:n_knots, 1:n_knots])
#   Wstar_2[1:n_knots] ~ dmnorm(zeros[1:n_knots], cov = Sigma_Wstar_2[1:n_knots, 1:n_knots])
#   
#   W_tilde_1[1:n_points] <- (C_cross_obs[1:n_points, 1:n_knots] %*% inv_Wstar_exp_cov[1:n_knots, 1:n_knots]) %*% Wstar[1:n_knots]
#   W_tilde_A[1:n_A] <- (C_cross_int[1:n_A, 1:n_knots] %*% inv_Wstar_exp_cov[1:n_knots, 1:n_knots]) %*% Wstar[1:n_knots]
#   W_tilde_2[1:n_points] <- (C_cross_obs[1:n_points, 1:n_knots] %*% inv_Wstar_exp_cov[1:n_knots, 1:n_knots]) %*% Wstar_2[1:n_knots]
#   
#   for(i in 1:p) {
#     beta[i] ~ dnorm(0, sd = 2)
#     gamma[i] ~ dnorm(0, sd = 2)
#   }
#   
#   sigma2_1 ~ dinvgamma(2, 10)
#   sigma2_2 ~ dinvgamma(2, 10)
# })
model_code <- nimbleCode({
  # --- Point process model (LGCP) ---
  s[1:n_points] ~ d_lgcp(lambda_sA = lambda_sA[1:n_A],
                         lambda_s = lambda[1:n_points],
                         area = area,
                         n_A = n_A)
  
  for (i in 1:n_A) {
    log(lambda_sA[i]) <- inprod(beta[1:p], x_A[i, 1:p]) + W_tilde_A[i]
  }
  
  for (i in 1:n_points) {
    log(lambda[i]) <- inprod(beta[1:p], x[i, 1:p]) + W_tilde[i]
  }
  
  for (i in 1:n_points) {
    logit(pi[i]) <- inprod(gamma[1:p], x[i, 1:p]) + W_tilde_2[i]
    Y[i] ~ dbin(prob = pi[i], size = 1)
  }
  
  Sigma1[1:n_knots, 1:n_knots] <- sigma2_1 * exp_D[1:n_knots, 1:n_knots]
  Sigma2[1:n_knots, 1:n_knots] <- sigma2_2 * exp_D[1:n_knots, 1:n_knots]
  
  W1[1:n_knots] ~ dmnorm(mean = zeros[1:n_knots], cov = Sigma1[1:n_knots, 1:n_knots])
  W2[1:n_knots] ~ dmnorm(mean = zeros[1:n_knots], cov = Sigma2[1:n_knots, 1:n_knots])
  
  W_tilde[1:n_points]    <- C_obs[1:n_points, 1:n_knots] %*% W1[1:n_knots]
  W_tilde_2[1:n_points]  <- C_obs[1:n_points, 1:n_knots] %*% W2[1:n_knots]
  W_tilde_A[1:n_A]       <- C_int[1:n_A, 1:n_knots] %*% W1[1:n_knots]
  

  for (j in 1:p) {
    beta[j] ~ dnorm(0, sd = 5)
    gamma[j] ~ dnorm(0, sd = 5)
  }
  
  sigma2_1 ~ dinvgamma(2, 2)
  sigma2_2 ~ dinvgamma(2, 2)
})


#precompute matrices
#knot distances
Wstar_exp_cov <- exp_corr(dists_knots, phi = phi)
inv_Wstar_exp_cov <- solve(Wstar_exp_cov)

robbery_ppp <- as.ppp(st_geometry(robbery_sf_joint), W = window)
robbery_ppp_km <- rescale(robbery_ppp, s = 1000, unitname = "km")

Wstar <- rnorm(nrow(knots))
Wstar_2 <- rnorm(nrow(knots))
C_cross_int <- exp_corr(dists_int_knots, phi = phi)

exp_D <- exp(-phi * dists_knots)


constants_list <- list(
  n_points = nrow(X_obs_pre_post),
  n_A = nrow(X_int),
  n_knots = nrow(knots),
  area = area.owin(window)/1e6,
  x = X_obs_pre_post,
  x_A = X_int,
  p = ncol(X_obs_pre_post),
  C_obs = exp_corr(dists_obs_knots_pre_post, phi),
  C_int = exp_corr(dists_int_knots, phi),
  exp_D = exp_D,
  zeros = rep(0, nrow(knots))
)
data_list <- list(
  Y = as.numeric(robbery_sf_3$timeline == "post-covid"),  # 1=post-covid, 0=pre-covid
  s = rep(1, constants_list$n_points)
)
# inits
inits_list <- list(
  beta = rnorm(constants_list$p, 0, 4),
  gamma = rnorm(constants_list$p, 0, 4),
  sigma2_1 = runif(1, 0, 5),
  sigma2_2 = runif(1, 0, 5),
  W1 = rnorm(constants_list$n_knots),
  W2 = rnorm(constants_list$n_knots)
)

##This model is pre v post covid!
my_model <- nimbleModel(model_code,
                        data = data_list,
                        constants = constants_list,
                        inits = inits_list)
my_config <- configureMCMC(my_model)
my_config$addMonitors(c('lambda_sA', 'sigma2_1', 'sigma2_2', 'beta', 'gamma', 'pi'))
my_MCMC <- buildMCMC(my_config)
my_compiled <- compileNimble(my_model, my_MCMC)

system.time({
  samples <- runMCMC(my_compiled$my_MCMC,
                     nburnin = 20000,
                     niter = 70000,
                     thin = 10, #thinning because system ran out of memory
                     samplesAsCodaMCMC = TRUE,
                     nchains = 3)
})

summary(samples[,c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]', 'beta[5]',
                   'gamma[1]', 'gamma[2]', 'gamma[3]', 'gamma[4]', 'gamma[5]',
                   'sigma2_1', 'sigma2_2')])

#largely ended up converging at the end - took awhile though. sorry couldn't run more than this because my computer kept having memory issues
plot(samples[,c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]', 'beta[5]',
                'gamma[1]', 'gamma[2]', 'gamma[3]', 'gamma[4]', 'gamma[5]',
                'sigma2_1', 'sigma2_2')], density = F)

all_samples <- rbind(samples$chain1,
                     samples$chain2,
                     samples$chain3)

post_mean_lambda <- colMeans(all_samples[,grep('lambda_sA', colnames(all_samples))])
to_plot <- cbind.data.frame(integration_points, lambda = post_mean_lambda)

ggplot(to_plot) +
  geom_raster(aes(x = x * 1000, y = y * 1000, fill = lambda)) +  # back to meters
  scale_fill_viridis_c(name = "Posterior mean\nrobbery intensity") +
  geom_sf(data = dc_shp, fill = NA, color = "black", linewidth = 0.5) +
  #geom_sf(data = business, fill = NA, color = "green", linewidth = 0.6, linetype = "dashed") +
  geom_sf(data = central_business, fill = NA, color = "red", linewidth = 0.8) +
  coord_sf(crs = st_crs(dc_shp)) +
  theme_void() +
  labs(title = "Estimated Robbery Intensity (λ)",
       subtitle = "Highlighting Business and Central Business Districts")

post_ci_lambda <- apply(all_samples[,grep('lambda_sA', colnames(all_samples))], 2,
                        quantile, probs = c(0.025, 0.5, 0.975))
to_plot <- cbind.data.frame(integration_points,
                            lower = post_ci_lambda[1,],
                            median = post_ci_lambda[2,],
                            upper = post_ci_lambda[3,])
to_plot_long <- reshape(to_plot,
                        varying = c("lower", 'median', "upper"),
                        v.names = "value",
                        timevar = "percentile",
                        times = c("lower", 'median', "upper"),
                        new.row.names = 1:(nrow(integration_points)*3),
                        direction = "long")
dc_coords <- st_transform(dc_shp, crs = 26985) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON") %>%
  st_coordinates() %>%
  as.data.frame()

# st_coordinates() gives matrix with X, Y, and optional L1 (group id)
# Rename for ggplot
colnames(dc_coords)[1:2] <- c("long", "lat")

ggplot(to_plot_long) +
  geom_raster(aes(x = x * 1000, y = y * 1000, fill = value)) +
  geom_path(data = dc_coords, aes(x = long, y = lat, group = L1), 
            color = "black", linewidth = 0.4) +
  scale_fill_viridis_c(name = "λ") +
  facet_wrap(~percentile) +  # remove scales = "free"
  coord_fixed() +            # keep fixed aspect ratio
  theme_minimal()

post_ci_pi <- apply(all_samples[,grep('pi', colnames(all_samples))], 2,
                    quantile, probs = c(0.025, 0.5, 0.975))
robbery_sf_3 <- cbind.data.frame(robbery_sf_3,
                          lower = post_ci_pi[1,],
                          median = post_ci_pi[2,],
                          upper = post_ci_pi[3,])
robbery_sf_long <- robbery_sf_3 %>%
  pivot_longer(cols = c(lower, median, upper),
               names_to = "percentile",
               values_to = "value")
ggplot(robbery_sf_long, aes(geometry = geometry)) +
  geom_sf(aes(col = value)) +
  scale_color_viridis_c() +
  facet_wrap(~percentile) +
  theme_void()+
  labs(title = "Posterior Probabilities of Robbery",
       fill = "pi") +
  geom_sf(data = dc_shp, fill = adjustcolor('white', alpha = 0))

ggplot(robbery_sf_long, aes(geometry = geometry)) +
  geom_sf(aes(color = value), size = 0.8) +
  scale_color_viridis_c(name = "Post-COVID") +
  facet_wrap(~percentile) +
  geom_sf(data = metro, color = "black", shape = 21, fill = "black", size = 1, stroke = 0.8) +
  geom_sf(data = dc_shp, fill = adjustcolor('white', alpha = 0), color = "black", linewidth = 0.6) +
  theme_void() +
  labs(title = "Posterior Probability Robbery Occurred Post-COVID",
       subtitle = "Median and 95% Credible Interval")

# #### redefining things for other models --- pre v. covid ########## - didn't have time to run
# #redefining here for easy issue fix
# robbery_sf_joint <- robbery_sf_1
# robbery_sf_joint$Y <- as.numeric(robbery_sf_joint$timeline == "pre-covid")
# coords_obs_joint <- st_coordinates(robbery_sf_joint) / 1000
# X_obs_joint <- model.matrix(~ dist_to_metro + pop_density + business_dist + central_business, data = robbery_sf_joint)
# pop_joint <- robbery_sf_joint$pop_density
# 
# dists_obs_knots_joint <- as.matrix(proxy::dist(
#   coords_obs_joint,
#   knots[, c("x", "y")]
# ))
# 
# #exponential
# exp_corr <- function(dists, phi) {
#   exp(-dists * phi)
# }
# 
# Wstar_exp_cov <- exp_corr(dists_knots, phi = phi)
# inv_Wstar_exp_cov <- solve(Wstar_exp_cov)
# 
# # C matrix for observed points
# C_cross_obs_joint <- exp_corr(dists_obs_knots_joint, phi = phi)
# C_cross_C_inv_obs_joint <- C_cross_obs_joint %*% inv_Wstar_exp_cov
# C_cross_C_inv_obs_2_joint <- C_cross_C_inv_obs_joint
# 
# 
# constants_list <- list(
#   n_points = nrow(X_obs_pre_covid),
#   n_A = nrow(X_int),
#   n_knots = nrow(knots),
#   area = area.owin(window)/1e6,  # Area in km²
#   x = X_obs_pre_covid,
#   x_A = X_int,
#   p = ncol(X_obs_pre_covid),
#   pop = robbery_sf_1$pop_density,
#   pop_A = integration_points$pop_density,
#   Wstar_exp_cov = Wstar_exp_cov,
#   inv_Wstar_exp_cov = inv_Wstar_exp_cov,
#   C_cross_obs = C_cross_obs_joint,
#   C_cross_int = C_cross_int,
#   zeros = rep(0, nrow(knots))  # Only used for spatial effects
# )
# data_list <- list(
#   Y = as.numeric(robbery_sf_1$timeline == "pre-covid"),  # 1=pre-covid, 0=covid
#   s = 1:constants_list$n_points
# )
# 
# ##This model is pre v post covid!
# my_model <- nimbleModel(model_code,
#                         data = data_list,
#                         constants = constants_list,
#                         inits = inits_list)
# my_config <- configureMCMC(my_model)
# my_config$addMonitors(c('lambda_sA', 'pi'))
# my_MCMC <- buildMCMC(my_config)
# my_compiled <- compileNimble(my_model, my_MCMC)
# 
# system.time({
#   samples <- runMCMC(my_compiled$my_MCMC,
#                      nburnin = 20000,
#                      niter = 50000,
#                      samplesAsCodaMCMC = TRUE,
#                      nchains = 3)
# })
# 
# summary(samples[,c('beta[1]', 'beta[2]', 'beta[3]',
#                    'gamma[1]', 'gamma[2]', 'gamma[3]',
#                    'sigma2_1', 'sigma2_2')])



##stuff for building output table - with posterior estimates
param_names <- varnames(samples)
beta_names <- grep("^beta\\[", param_names, value = TRUE)
gamma_names <- grep("^gamma\\[", param_names, value = TRUE)
var_names <- grep("^sigma2", param_names, value = TRUE)

summary_coda <- summary(samples)

means <- summary_coda$statistics[, "Mean"]
sds   <- summary_coda$statistics[, "SD"]
qntls <- summary_coda$quantiles

summary_df <- data.frame(
  param = rownames(summary_coda$statistics),
  mean = means,
  sd = sds,
  q2.5 = qntls[, "2.5%"],
  q25  = qntls[, "25%"],
  q50  = qntls[, "50%"],
  q75  = qntls[, "75%"],
  q97.5 = qntls[, "97.5%"],
  group = ifelse(rownames(summary_coda$statistics) %in% beta_names, "Pre-COVID",
                 ifelse(rownames(summary_coda$statistics) %in% gamma_names, "Post-Pre", "Variance"))
)

summary_beta <- summarize_params(beta_names, "Pre-COVID")
summary_gamma <- summarize_params(gamma_names, "Post-Pre")
summary_var <- summarize_params(var_names, "Variance")


summary_df <- rbind(summary_beta, summary_gamma, summary_var)
summary_df$group <- factor(summary_df$group, levels = c("Pre-COVID", "Post-Pre", "Variance"))

param_labels <- c(
  "beta[1]" = "Intercept (Pre-COVID)",
  "beta[2]" = "Distance to Metro (Pre-COVID)",
  "beta[3]" = "Population Density (Pre-COVID)",
  "beta[4]" = "Business District (Pre-COVID)",
  "beta[5]" = "Central Business District (Pre-COVID)",
  "gamma[1]" = "Intercept (Post - Pre)",
  "gamma[2]" = "Distance to Metro (Post - Pre)",
  "gamma[3]" = "Population Density (Post - Pre)",
  "gamma[4]" = "Business District (Post - Pre)",
  "gamma[5]" = "Central Business District (Post - Pre)"
)

summary_df$label <- param_labels[summary_df$param]
summary_df$label[is.na(summary_df$label)] <- summary_df$param  # fallback

plot_df <- summary_df %>%
  filter(group != "Variance") %>%
  mutate(label = factor(label, levels = rev(param_labels)))

library(ggplot2)
ggplot(plot_df, aes(x = label, y = mean, fill = group)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  labs(
    title = "Posterior Estimates with 95% Credible Intervals",
    x = NULL, y = "Posterior Mean Estimate",
    fill = "Effect Type"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13)