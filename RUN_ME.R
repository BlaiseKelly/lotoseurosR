## import functions
source('functions.R')

## import model data and output raster
mod_rast <- import_surf_concs(species = c('o3','no2'), path = 'files/output', output_units = 'volume')

## create a time series plot of the full domain
p1 <- mod_plot(raster_list = mod_rast, file_path = 'plots/rmd/', variable = 'o3')

## generate a domain boundary for a country
sub_domain <- get_country_domain(countries = c('Greece'))

## background map should be larger than polygon, so buffer it by 200km
buff_dom <- st_buffer(st_transform(sub_domain, 28992), 200000)

## import basemap for plot using buffered domaion
bg <- basemaps::basemap_raster(ext=buff_dom, map_service = "carto", map_type = "light")

## generate tmap plot
tm_1 <- tm_shape(bg)+
  tm_rgb()+
  tm_shape(sub_domain) +
  tm_polygons(col = 'yellow', alpha = 0.5)

## save to output folder for markdown document to read
tmap_save(tm_1, "plots/rmd/tm1.png")

## or an interactive leaflet map
lon <- mean(st_coordinates(sub_domain)[,1])
lat <- mean(st_coordinates(sub_domain)[,2])
 
leaflet() %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
  setView(lon, lat, zoom = 4) %>%
  addPolygons(data = sub_domain, color = "black", weight = 1,
                            opacity = 1.0, fillOpacity = 0.6,
                            fillColor = "yellow")


## find observation sites for sub_domain
obs_sites <- find_aq_sites(raster_list = mod_rast, domain = sub_domain, type_def = c('background'), area_def = c('urban', 'rural', 'suburban'), species = c('o3', 'no2'))

## generate leaflet map of observations
plot_obs(sites_sf = obs_sites, variable = 'no2')

## get observation data and extract model data for sites and combine
mod_obs <- mod_obs_combine(raster_list = mod_rast, sites_sf = obs_sites, species = c('no2', 'o3'))

mod_obs_1 <- filter(mod_obs, species == 'o3')

## pick out one site and variable
site_var_1 <- mod_obs_1 %>% 
  filter(site == unique(mod_obs$site)[1]) %>% 
  melt(c('date', 'site', 'species'))

## plot
p2 <- scatterPlot(mod_obs_1, x = "mod", y = "obs", 
            pch = 21:22, cex = 1.6, 
            fill = c("tomato"), 
            col = "white", 
            linear = TRUE, 
            xlab = "modelled (ug/m3)", 
            ylab = "observed (ug/m3)")

## output as a png
filename <- paste0("plots/rmd/sp1.png")
png(filename, width=1500, height=1500, units="px", res=150)
plot(p2)
dev.off()

## density plot
p3 <- scatterPlot(mod_obs_1,
            x = "mod", y = "obs",
            method = "density",
            xlab = "modelled (ug/m3)", 
            ylab = "observed (ug/m3)")

## save as png
filename <- paste0("plots/rmd/sp2.png")
png(filename, width=1500, height=1500, units="px", res=150)
plot(p3)
dev.off()

## time variation
p4 <- timeVariation(site_var_1, pollutant = 'value', group = 'variable', ylab = 'o3 (ug/m3)')

## output as png
filename <- paste0("plots/rmd/tv.png")
png(filename, width=2000, height=1500, units="px", res=150)
plot(p4)
dev.off()

## import model data and output raster
mod_rast_all <- import_surf_concs(species = c('o3','no2', 'no', 'hno3', 'nh3', 'so2', 'co', 'n2o5', 'form', 
                                          'iso', 'pan', 'no3a_f', 'no3a_c', 'so4a_f', 'so4a_c', 'nh4a_f', 
                                          'ppm_f', 'ppm_c', 'ec_f', 'ec_c', 'pom_f', 'pom_c', 'na_ff', 'na_f', 
                                          'na_c', 'na_cc', 'na_ccc', 'dust_ff', 'dust_f', 'dust_c', 'dust_cc', 
                                          'dust_ccc', 'tpm25', 'tpm10', 'tnmvoc', 'tdust', 'tss'), path = 'files/output', output_units = 'mass')

## pick out one site
site_1 <- filter(obs_sites, site == 'gr0027a')

## drill through raster brick and pick out the hours for each species for location. It will return a data frame with species stacked on top of each other
mod_s_all <- mod_site(raster_list = mod_rast_all, sites_sf = site_1, species = c('o3','no2', 'no', 'hno3', 'nh3', 'so2', 'co', 'n2o5', 'form', 
                                                                                 'iso', 'pan', 'no3a_f', 'no3a_c', 'so4a_f', 'so4a_c', 'nh4a_f', 
                                                                                 'ppm_f', 'ppm_c', 'ec_f', 'ec_c', 'pom_f', 'pom_c', 'na_ff', 'na_f', 
                                                                                 'na_c', 'na_cc', 'na_ccc', 'dust_ff', 'dust_f', 'dust_c', 'dust_cc', 
                                                                                 'dust_ccc', 'tpm25', 'tpm10', 'tnmvoc', 'tdust', 'tss'))



## import temperature data from meteo file
meteo_rast <- import_surf_meteo(variable = 'temper')

## import wind speed and direction data
ws_rast <- import_surf_meteo(variable = 'wspd_surf')
wd_rast <- import_surf_meteo(variable = 'wdir_surf')

## generate a gif of wind speed and direction for the full model domain
ws_wd_plot(ws_rast = ws_rast, file_path = 'plots/rmd/', wd_rast = wd_rast, height = 200, width = 400, start_hr = 1, end_hr = 4)

## find noaa sites for a specific domain
find_noaa_domain(domain = sub_domain, html_out = TRUE)

## find nearest noaa sites to another site
noaa_df <- find_noaa_sites(sites = obs_sites, start_date = '2012', end_date = '2012')

## plot the meteo data as an animated gif
meteo_plot(meteo_rast, start_hr = 1, end_hr = 30)


## calculate model statistics for the model and observed data that has been joined together
ms_sites <- modStats(mod_obs, obs = "obs", mod = "mod",
                     type = c("site"))

ms_all <- modStats(mod_obs, obs = "obs", mod = "mod")
