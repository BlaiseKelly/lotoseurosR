library(dplyr)
library(purrr)
library(sf)
library(lubridate)
library(raster)
library(ncdf4)
library(stringr)
library(reshape2)
library(ncdf4)
library(saqgetr)
library(openair)
library(worldmet)
library(mapview)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(ggplot2)
library(gganimate)
library(pals)
library(tmap)
library(exactextractr)
library(threadr)

select <- dplyr::select

varient_df <- data.frame(le_specs = c("o3", "no2", "no", "nh3", "so2", 'hno3', "co", "tpm25", "tpm10"),
                         omrekenfactor = c(1e12*0.048/24.4,  1e12*0.046005/24.4, 1e12*0.03001/24.4, 1e12*0.017031/24.4, 1e12*0.0640628/24.4, 1e12*0.06301/24.4,1e12*0.02801/24.4, 1e9, 1e9),
                         tmap_nam = c("'O'[3]*' ('* mu*'g/m'^3*')'", "'NO'[2]*' ('* mu*'g/m'^3*')'","'NO ('* mu*'g/m'^3*')'",
                                   "'NH'[3]*' ('* mu*'g/m'^3*')'","'SO'[2]*' ('* mu*'g/m'^3*')'","'HNO'[3]*' ('* mu*'g/m'^3*')'",
                                   "'CO'*' ('* mu*'g/m'^3*')'", "'PM'[2.5]*' ('* mu*'g/m'^3*')'",
                                   "'PM'[10]*' ('* mu*'g/m'^3*')'"),
                         dg_nam = c("O<sub>3</sub> (&mu;g/m<sup>3</sup>)", "NO<sub>2</sub> (&mu;g/m<sup>3</sup>)", "NO (&mu;g/m<sup>3</sup>)","NH<sub>3</sub> (&mu;g/m<sup>3</sup>)",
                                    "SO<sub>2</sub> (&mu;g/m<sup>3</sup>)", "HNO<sub>3</sub> (&mu;g/m<sup>3</sup>)", "CO (&mu;g/m<sup>3</sup>)", "PM<sub>10</sub> (&mu;g/m<sup>3</sup>)", "PM<sub>2.5</sub> (&mu;g/m<sup>3</sup>)"))

import_surf_concs <- function(path = 'mod_files/output', pattern = "_conc-sfc_", write_out = TRUE, species,  output_crs = 4326, output_units = 'mass'){
  # 
  # path <- "files/output"
  # pattern = "_conc-sfc_"
  # species <- c('o3', 'no2')
  # output_units <- 'mass'
  # output_crs = 4326
  
  files_p <- list.files(path, pattern, full.names = TRUE)
  files <- files_p[!grepl('metout', files_p)] ## metout files are generated as part of defualt model run
  files <- files[grepl('.nc', files)]
  ## open one file to get species
  lefile <- nc_open(files[1])
  
  longitude <- ncvar_get(lefile, "longitude")
  latitude <- ncvar_get(lefile, "latitude")
  lon_res <- (longitude[2]-longitude[1])/2
  lat_res <- (latitude[2]-latitude[1])/2
  ##create min x and y
  x_min <- min(longitude)-lon_res
  x_max <- max(longitude)+lon_res
  y_min <- min(latitude)-lat_res
  y_max <- max(latitude)+lat_res
  ##determine number of lat and lon points
  n_y <- NROW(latitude)
  n_x <- NROW(longitude)
  
  ##work out number of x and y cells
  
  ##create a data frame to setup polygon generation
  df <- data.frame(X = c(x_min, x_max, x_max, x_min),
                   Y = c(y_max, y_max, y_min, y_min))
  
  ##generate a polygon of the area
  vgt_area <- df %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
    dplyr::summarise(data = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  nc_close(lefile)
  rstz <- list()
  for (f in files){
    
    lefile <- nc_open(f)
    
    TIME <- ncvar_get(lefile, "time", start = c(1), count = c(1))
    
    time_since <- str_sub(lefile$dim$time$units, 15, -14)
    
    d8_time <- paste0((lubridate::ymd(time_since) + lubridate::seconds(TIME)), " ", sprintf("%02d", seq(1:24)-1), ":00")
    d8 <- lubridate::ymd(lubridate::ymd(time_since) + lubridate::seconds(TIME))
    
    for (s in species){
    
    ## import variable, convert to brick and transpose
    var_in <- t(brick(ncvar_get(lefile, s, start = c(1,1,1,1), count = c(n_x, n_y, 1,24))))
    
    var_info <- filter(varient_df, le_specs == s)
    ##define the crs
    crs(var_in) <- 4326
    ## define the extent
    bb <- extent(vgt_area)
    extent(var_in) <- bb
    ## flip the domain
    r1 <- flip(var_in, direction = 'y')
    
    if(output_units == 'mass'){
    r1_out <- r1*10^9
    }
    if(output_units == 'volume'){
      r1_out <- r1*var_info$omrekenfactor
    }
    
    r1_out <- raster::projectRaster(r1_out, crs = output_crs)
    
    names(r1_out) <- d8_time
    
    nam_out <- paste0(s,"_", as.character(d8))
    
    rstz[[nam_out]] <- r1_out
    
    print(paste('importing ', f, s))
   

    }
    
  }

  return(rstz)
  
}


meteo_spec <- c('temper', 'wspd_surf', 'wdir_surf')
                         

import_surf_meteo <- function(path = 'mod_files/output', pattern = "_meteo_", variable = 'temper', write_out = FALSE,  output_crs = 4326){
  
  #path <- "output"
  #pattern = "_meteo_"
  
  files_p <- list.files(path, pattern, full.names = TRUE)
  files <- files_p[!grepl('metout', files_p)] ## metout files are generated as part of defualt model run
  files <- files[grepl('.nc', files)]
  ## open one file to get species
  lefile <- nc_open(files[1])
  
  longitude <- ncvar_get(lefile, "longitude")
  latitude <- ncvar_get(lefile, "latitude")
  lon_res <- (longitude[2]-longitude[1])/2
  lat_res <- (latitude[2]-latitude[1])/2
  ##create min x and y
  x_min <- min(longitude)-lon_res
  x_max <- max(longitude)+lon_res
  y_min <- min(latitude)-lat_res
  y_max <- max(latitude)+lat_res
  ##determine number of lat and lon points
  n_y <- NROW(latitude)
  n_x <- NROW(longitude)
  
  ##work out number of x and y cells
  
  ##create a data frame to setup polygon generation
  df <- data.frame(X = c(x_min, x_max, x_max, x_min),
                   Y = c(y_max, y_max, y_min, y_min))
  
  ##generate a polygon of the area
  vgt_area <- df %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
    dplyr::summarise(data = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  nc_close(lefile)
  rstz <- list()
  for (f in files){
      
      lefile <- nc_open(f)
      
      TIME <- ncvar_get(lefile, "time", start = c(1), count = c(1))
      
      time_since <- str_sub(lefile$dim$time$units, 15, -14)
      
      d8_time <- paste0((lubridate::ymd(time_since) + lubridate::seconds(TIME)), " ", sprintf("%02d", seq(1:24)-1), ":00")
      d8 <- lubridate::ymd(lubridate::ymd(time_since) + lubridate::seconds(TIME))
      
      var_dim <- NROW(dim(ncvar_get(lefile, variable)))
      
       lefile$var$wdir_surf$ndims
      if(var_dim == 4){
        var_in <- t(brick(ncvar_get(lefile, variable, start = c(1,1,1,1), count = c(n_x, n_y, 1, 24))))
      }
      
      if(var_dim == 3){
        var_in <- t(brick(ncvar_get(lefile, variable, start = c(1,1,1), count = c(n_x, n_y, 24))))
      }
      

      ##define the crs
      crs(var_in) <- 4326
      ## define the extent
      bb <- extent(vgt_area)
      extent(var_in) <- bb
      ## flip the domain
      r1 <- flip(var_in, direction = 'y')
      
      r1_out <- raster::projectRaster(r1, crs = output_crs)
      
      names(r1_out) <- d8_time
      
      nam_out <- as.character(d8)
      
      rstz[[nam_out]] <- r1_out
      
      print(paste('importing ', f, variable))
      
      
    }
  
  if(write_out == TRUE){
    
    dir.create('bricks/meteo', recursive = TRUE)
    ## convert to a terra geospatial raster and use terra writeRaster function to preserve layer names
    terra::writeRaster(terra::rast(brick(rstz)), filename=paste0("bricks/meteo/", variable, ".TIF"), overwrite = TRUE)
    
    #assign(s, s_rast)
    
    print(paste0("raster brick written to: bricks/meteo/", variable, ".TIF"))
  }
  
  return(brick(rstz))
}


# 
# plot_obs <- function{data_brick, variable}{
#   
#   ## set palette for site types
#   pal_g <- colorFactor("Set3", reverse = FALSE, domain = site_sums_sf$site_type)
#   
#   ##plot on a map
#   
#   for (c in cats){
#     
#     ## tryCatch({
#     cat <- get(c)
#     
#     ## create base base
#     m <- leaflet() %>% 
#       setView(lon, lat, zoom = 6) %>% 
#       addProviderTiles('CartoDB.Positron')
#     
#     ## find unique variables
#     variable_u <- unique(cat$variable_long)
#     
#     ## find how many variable colours are needed
#     n <- NROW(variable_u)
#     ## generate a palette of for all variables
#     palette <- distinctColorPalette(n)
#     
#     ## loop through the variables to generate ability to toggle between species
#     for (u in variable_u){
#       
#       p <- palette[which(variable_u == u)]
#       
#       df <- filter(cat, variable_long == u)
#       
#       m <- m %>% addCircleMarkers(data = df, fillColor = ~pal_g(site_type), color = 'black', weight = 1,
#                                   opacity = 1.0, fillOpacity = 1.0,
#                                   popup = paste("site code:", df$site, "<br>",
#                                                 "site name:", df$site_name, "<br>",
#                                                 "site type:", df$site_type, "<br>",
#                                                 "date start:", df$date_start.y, "<br>",
#                                                 "date end:", df$date_end.y, "<br>",
#                                                 "data source:", df$data_source.y, "<br>",
#                                                 "sampling process:", df$sampling_process, "<br>",
#                                                 "sampling process:", df$sampling_point, "<br>",
#                                                 "observation count:", df$observation_count.y, "<br>",
#                                                 "sampling period:", df$period, "<br>"), group = u)
#       
#     }
#     
#     m <- m %>% addLegend("bottomleft", pal=pal_g, values=site_sums_sf$site_type, opacity=1, title = "site type",
#                          group = "site type")
#     
#     m <- m %>% addLayersControl(baseGroups = c(variable_u),
#                                 options = layersControlOptions(collapsed = FALSE))
#     
#     n <- sprintf('%02d', which(cats == c))
#     
#     ## use html widget saveWidget function to make a standalone html
#     withr::with_dir('./', saveWidget(m, file = paste0(n, "_", c, ".html")))  
#     
#     ## error catch in case one group gets an error
#     ##  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#   }
#   
# }


get_country_domain <- function(countries = c('Slovenia', 'Greece'), return_format = 'sf'){

dir.create("temp/countries", recursive = TRUE)
download.file("https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/download/?format=geojson&timezone=Europe/Berlin&lang=en",
              destfile = "temp/countries/world_map.geojson")

countries <- tolower(countries)

countries <- (st_read("temp/countries/world_map.geojson")) %>% 
  select(name, geometry) %>% 
  mutate(name = tolower(name)) %>% 
  filter(name %in% c(countries))
#st_geometry(countries) <- countries$geometry
bb <- st_bbox(st_union(rbind(countries)))
sf_dom <- bb %>% st_as_sfc() %>% st_as_sf() %>% mutate(lon_min = bb$xmin, lon_max = bb$xmax, lat_min = bb$ymin, lat_max = bb$ymax)

if(return_format == 'bb'){
  return(bb)
}
if(return_format == 'sf'){
  return(sf_dom)
}

}


find_noaa_domain <- function(domain, html_out = FALSE){
  #domain <- sub_domain
  ## import all meteo sites
  met_info <- getMeta(plot = FALSE)
  ## geo reference them
  met_sf <- st_as_sf(met_info, coords = c("longitude", "latitude"), crs = 4326)
  
  met_in <- met_sf[domain,]
  
  met_in$start_year <- year(met_in$begin)
  met_in$end_year <- year(met_in$end)
  
  if(html_out == TRUE){
    
    dir.create('plots/noaa', recursive = TRUE)
  ## set palette for site types
  pal_g <- colorFactor("Set3", reverse = FALSE, domain = met_in$end_year)
  
  lon <- mean(st_coordinates(sub_domain)[,1])
  lat <- mean(st_coordinates(sub_domain)[,2])
  
  ## create base base
  m <- leaflet() %>% 
    setView(lon, lat, zoom = 6) %>% 
    addProviderTiles('CartoDB.Positron')

    
    m <- m %>% addCircleMarkers(data = met_in, fillColor = ~pal_g(end_year), color = 'black', weight = 1,
                                opacity = 1.0, fillOpacity = 1.0,
                                popup = paste("station code:", met_in$code, "<br>",
                                              "station name:", met_in$station, "<br>",
                                              "country:", met_in$ctry, "<br>",
                                              "date start:", met_in$begin, "<br>",
                                              "date end:", met_in$end, "<br>",
                                              "elevation (m):", met_in$`elev(m)`, "<br>",
                                              "usaf:", met_in$usaf, "<br>"))
    
  
  m <- m %>% addLegend("bottomleft", pal=pal_g, values=met_in$end_year, opacity=1, title = "Latest year")

  print(paste0("saving html to: plots/noaa/",round(lon,1), "_", round(lat,1) ,".html"))
  ## use html widget saveWidget function to make a standalone html
  withr::with_dir('./', saveWidget(m, file = paste0("plots/noaa/obs_domain_",round(lon,1), "_", round(lat,1) ,".html")))
  
  }
  
  
  return(met_in)
  
}


find_noaa_sites <- function(sites, start_date, end_date){

  ## import all meteo sites
  met_info <- getMeta()
  ## geo reference them
  met_info <- st_as_sf(met_info, coords = c("longitude", "latitude"), crs = 4326)
  
  ## find nearest meteo station to site
  sites$nearest_NOAA <- met_info$code[st_nearest_feature(sites, met_info)]
  
  all_met_sites <- unique(sites$nearest_NOAA)
  
  ## find data range
  d8s <- seq(start_date, end_date)
  all_met <- list()
  
  for (m in all_met_sites){
    tryCatch({
      data_met <- importNOAA(m, d8s) %>% 
        select(code, station, date, latitude, longitude, elev, ws, wd, air_temp, atmos_pres,RH, ceil_hgt)
      
      all_met[[m]] <- data_met
      print(m)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  obs_met <- do.call(rbind, all_met)
  
  
}



ws_wd_plot <- function(ws_rast = NA, wd_rast = NA, start_hr = 1, end_hr = 4){

  
   # ws_rast <- crop(subset(ws_rast,start_hr:end_hr),domain)
    #wd_rast <- crop(subset(wd_rast,start_hr:end_hr), domain)
    
    ws_dat <- data.frame(rasterToPoints(ws_rast)) %>% 
      melt(c('x', 'y')) %>% 
      mutate(date = ymd_hm(gsub("X", "", variable)),
             ws = value) %>% 
      select(-variable, -value)
    
    wd_dat <- data.frame(rasterToPoints(wd_rast)) %>% 
      melt(c('x', 'y')) %>% 
      mutate(date = ymd_hm(gsub("X", "", variable)),
             wd = value) %>% 
      select(-variable, -value)
    
    
    ws_wd_dis <- ws_dat %>% 
      left_join(wd_dat, by = c('date', 'x', 'y')) %>% 
      select(lon = x, lat = y, ws, wd, date) %>% 
      mutate(wd = (wd*pi)/180) ## convert to radians
    

framez <- NROW(unique(ws_wd_dis$date))

g1 <- ggplot(ws_wd_dis, 
             aes(x = lon, 
                 y = lat, 
                 fill = ws, 
                 angle = wd, 
                 radius = scales::rescale(ws, c(.2, .8)/2))) +
  geom_raster() +
  geom_spoke(arrow = arrow(angle = 20, length = unit(1.0, 'mm'))) + 
  scale_fill_distiller(palette = "RdYlGn") +  
  coord_equal(expand = 0) + 
  theme(legend.position = 'bottom', 
        legend.direction = 'horizontal',
        panel.background = element_rect(fill='white', colour='black'))+
  transition_manual(date) +
  labs(title = "Period: {current_frame}")

# animate in a two step process:
animate(g1, height = 1600, width =1200, fps = 2, nframes = framez)

}


u_v_plot <- function(u_rast = NA, v_rast = NA, domain, start_hr = 1, end_hr = 4){
  
  
  if(is.na(ws_rast)){
    
    windDir <-function(u,v){
      (270-atan2(u,v)*180/pi)%%360 
    }
    
    ra_ws = sqrt(u_rast^2 + v_rast^2)
    ra_wd = windDir(u = u_rast, v = v_rast)
    
    names(ra_ws) <- u_rast@data@names
    names(ra_wd) <- u_rast@data@names
    
    ws_rast <- crop(subset(ra_ws,start_hr:end_hr),domain)
    wd_rast <- crop(subset(ra_wd,start_hr:end_hr), domain)
    
    
    ws_dat <- data.frame(rasterToPoints(ws_rast)) %>% 
      melt(c('x', 'y')) %>% 
      mutate(date = ymd_hm(gsub("X", "", variable)),
             ws = value) %>% 
      select(-variable, -value)
    
    wd_dat <- data.frame(rasterToPoints(wd_rast)) %>% 
      melt(c('x', 'y')) %>% 
      mutate(date = ymd_hm(gsub("X", "", variable)),
             wd = value) %>% 
      select(-variable, -value)
    
    
    ws_wd_dis <- ws_dat %>% 
      left_join(wd_dat, by = c('date', 'x', 'y')) %>% 
      select(lon = x, lat = y, ws, wd, date) %>% 
      mutate(wd = (wd*pi)/180) ## convert to radians
    
  } else {
    
    ws_rast <- crop(subset(ws_rast,start_hr:end_hr),domain)
    wd_rast <- crop(subset(wd_rast,start_hr:end_hr), domain)
    
    ws_dat <- data.frame(rasterToPoints(ws_rast)) %>% 
      melt(c('x', 'y')) %>% 
      mutate(date = ymd_hm(gsub("X", "", variable)),
             ws = value) %>% 
      select(-variable, -value)
    
    wd_dat <- data.frame(rasterToPoints(wd_rast)) %>% 
      melt(c('x', 'y')) %>% 
      mutate(date = ymd_hm(gsub("X", "", variable)),
             wd = value) %>% 
      select(-variable, -value)
    
    ws_wd_dis <- ws_dat %>% 
      left_join(wd_dat, by = c('date', 'x', 'y')) %>% 
      select(lon = x, lat = y, ws, wd, date) %>% 
      mutate(wd = (wd*pi)/180) ## convert to radians
    
  }
  
  framez <- NROW(unique(ws_wd_dis$date))
  
  g1 <- ggplot(ws_wd_dis, 
               aes(x = lon, 
                   y = lat, 
                   fill = ws, 
                   angle = wd, 
                   radius = scales::rescale(ws, c(.2, .8)/2))) +
    geom_raster() +
    geom_spoke(arrow = arrow(angle = 20, length = unit(1.0, 'mm'))) + 
    scale_fill_distiller(palette = "RdYlGn") +  
    coord_equal(expand = 0) + 
    theme(legend.position = 'bottom', 
          legend.direction = 'horizontal',
          panel.background = element_rect(fill='white', colour='black'))+
    transition_manual(date) +
    labs(title = "Period: {current_frame}")
  
  # animate in a two step process:
  animate(g1, height = 1600, width =1200, fps = 2, nframes = framez)
  
}


regrid <- function(raster2change, desired_grid, statistic = 'mean'){
  # raster2change <- conc_dom
  # desired_grid <- new_grid
  # statistic <- 'mean'
  nlayerz <- raster2change@data@nlayers
  layer_stack <- list()
  for (n in 1:nlayerz){
    
    rl <- subset(raster2change, n)
  
  resamp <- exactextractr::exact_resample(x = rl, y = desired_grid, fun = statistic)
  nam <- as.character(n)
  layer_stack[[nam]] <- resamp
  print(paste0('processing layer ', n))
  
  }
  
  new_brick <- brick(layer_stack)
  names(new_brick) <- names(raster2change)
  
  return(new_brick)
  
}


meteo_plot <- function(raster_in, all_layers = TRUE, statistic = 'mean', variable = 'temper', average = TRUE, start_hr = 1, end_hr = 64){
  
  if(all_layers == TRUE){
  raster_in <- subset(raster_in, start_hr, end_hr)
  
  bg <- basemaps::basemap_raster(ext=subset(raster_in,1), map_service = "carto", map_type = "light")
  
  var_df <- data.frame(var = c('temper', 'wspd_surf', 'wdir_surf'),
                       units = c('K', 'm/s', 'degrees'))
  
  legend_title <- filter(var_df, var == variable)
  
  if(NROW(legend_title)<1){
    legend_title <- variable
  }
  
  d8s <- as.character(ymd_hm(gsub("\\.", "_", gsub("X", "", names(raster_in)))))
  
  bks <- seq(min(raster_in@data@min), max(raster_in@data@max), by = (max(raster_in@data@max)-min(raster_in@data@min))/20)
  
  pal_A <- pals::jet(NROW(bks))
  
  tm_1 <- tm_shape(bg)+
    tm_rgb()+
    tm_shape(raster_in) +
    tm_raster(palette = pal_A,alpha = 0.6, style = 'cont', breaks = bks, title = parse(text = legend_title))+
    tm_layout(legend.outside = FALSE, frame = FALSE, legend.outside.position = 'left', title.size = 1, panel.labels = d8s,
              panel.label.color = 'white', panel.label.bg.color = 'black',
              bg.color = "white")+
    tm_legend(position = c("left", "top"))+
    tm_facets(nrow = 1, ncol = 1)
  

  dir.create("plots/meteo", recursive = TRUE)
  tmap_animation(tm_1, filename = paste0("plots/meteo/", variable, "_", d8s[1], "_", d8s[NROW(d8s)], "_.gif"), delay = 60)
  #tmap_animation(tm_1, filename = paste0("C:/Users/kellyb/OneDrive - TNO/topas_plots/", start_date, "_", end_date, "_", lbl, ".gif"),width = 2100, height = 2200, dpi = 300, delay = 60)
  
  } else {
    
    d8s <- as.character(ymd_hm(gsub("\\.", "_", gsub("X", "", names(raster_in)))))
    
    raster_in <- calc(raster_in, statistic)
    
    bg <- basemaps::basemap_raster(ext=subset(raster_in,1), map_service = "carto", map_type = "light")
    
    var_df <- data.frame(var = c('temper', 'wspd_surf', 'wdir_surf'),
                         units = c('K', 'm/s', 'degrees'))
    
    legend_title <- filter(var_df, var == variable)
    
    d8s <- as.character(ymd_hm(gsub("\\.", "_", gsub("X", "", names(raster_in)))))
    
    bks <- seq(min(raster_in@data@min), max(raster_in@data@max), by = (max(raster_in@data@max)-min(raster_in@data@min))/20)
    
    pal_A <- pals::jet(NROW(bks))
    
    tm_1 <- tm_shape(bg)+
      tm_rgb()+
      tm_shape(raster_in) +
      tm_raster(palette = pal_A,alpha = 0.6, style = 'cont', breaks = bks, title = parse(text = legend_title))+
      tm_layout(legend.outside = FALSE, frame = FALSE, legend.outside.position = 'left', title.size = 1, panel.labels = paste(variable,d8s),
                panel.label.color = 'white', panel.label.bg.color = 'black',
                bg.color = "white")+
      tm_legend(position = c("left", "top"))
    
    tmap_save(tm_1, paste0("plots/meteo/", variable, "_", statistic, "_", d8s[1], "_", d8s[NROW(d8s)], "_.gif"))
    
  }
  
}


mod_plot <- function(raster_list, statistic = 'mean', variable = 'o3'){

  for (s in species){
  
  rstr_in <- brick(raster_list[grepl(s, names(raster_list))])

    bks <- seq(min(rstr_in@data@min), max(rstr_in@data@max), by = (max(rstr_in@data@max)-min(rstr_in@data@min))/20)

    #crs_in <- crs(raster_in)

    bg <- basemaps::basemap_raster(ext=subset(rstr_in,1), map_service = "carto", map_type = "light")
    #extent(bg) <- extent(raster_in)
 #    bg <- projectRaster(bg, crs = crs_in)
 # bg <- crop(bg, raster_in)
    legend_title <- filter(varient_df, le_specs == s)
    #mapview(subset(raster_in,1))+bg

    if(NROW(legend_title)<1){
      legend_title <- variable
    }

    d8s <- as.character(ymd_hm(gsub("\\.", "_", gsub("X", "", names(rstr_in)))))

    pal_A <- pals::jet(NROW(bks))

    tm_1 <- tm_shape(bg)+
      tm_rgb()+
      tm_shape(rstr_in) +
      tm_raster(palette = pal_A,alpha = 0.6, style = 'cont', breaks = bks, title = parse(text = legend_title$tmap_nam))+
      tm_layout(legend.outside = FALSE, frame = FALSE, legend.outside.position = 'left', title.size = 1, panel.labels = d8s,
                panel.label.color = 'white', panel.label.bg.color = 'black',
                bg.color = "white")+
      tm_legend(position = c("left", "top"))+
      tm_facets(nrow = 1, ncol = 1)

    dir.create("plots/concs", recursive = TRUE)
    tmap_animation(tm_1, filename = paste0("plots/concs/", s, "_",
                                           gsub(" ", "_", str_sub(d8s[1], 1,-7)), "_", gsub(" ", "_", str_sub(d8s[NROW(d8s)], 1,-7)), ".gif"), delay = 60)
    
  }
  
}



find_aq_sites <- function(raster_list, type_def = 'background', area_def = c('urban', 'rural', 'suburban'), domain, species){
  # data_brick <- conc_dom
  # site_type = c('background')
  # site_area = c('urban', 'rural', 'suburban')
  # variable = 'tpm25'
  # d8_range <- ymd_hm(gsub("\\.", "_", gsub("X", "", names(data_brick))))
  # min_d8 <- min(d8_range)
  # max_d8 <- max(d8_range)
  ## get all sites from database
  all_sites_sf <- list()
  for (s in species){
  
  rstr_in <- brick(raster_list[grepl(s, names(raster_list))])
  
  sitez <- saqgetr::get_saq_sites()
  st <- unique(sitez$site_type)
  min_d8 <- ymd_hm(gsub("\\.", "_", gsub("X", "", names(rstr_in))))[1]
  max_d8 <- ymd_hm(gsub("\\.", "_", gsub("X", "", names(rstr_in))))[rstr_in@data@nlayers]
  
  
  ## get more detailed data
  processes <- get_saq_processes()
  
  ## combine and convert to sf
  site_sums_sf <- sitez %>% 
    left_join(processes, by = 'site') %>% 
    filter(!is.na(latitude)) %>%
    filter(date_start.y < min_d8 & date_end.y > max_d8) %>% 
    mutate(variable_long = tolower(variable_long)) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
    filter(site_type %in% type_def) %>% 
    filter(site_area %in% area_def) %>% 
    filter(variable == s) %>% 
    filter(period == 'hour')
  
  
  db <- crop(rstr_in, sub_domain)
  
  ## define extent of brick
  r1 <- st_as_sf(st_as_sfc(st_bbox(subset(db,1))))
  ## filter points in area
  site_sf <- site_sums_sf[r1,]
  
  all_sites_sf[[s]] <- site_sf
  
  print(paste0('all ', s, ' sites imported between ', min_d8, " & ", max_d8))
  
  }
  
  all_sf <- do.call(rbind, all_sites_sf)
  
  return(all_sf)
  
}


plot_obs <- function(sites_sf, variable, write_out = FALSE){
  
  sitez <- sites_sf
  ## set palette for site types
  
  pal_st <- colorFactor("Set1", reverse = FALSE, domain = sitez$site_type)
  pal_sa <- colorFactor("Set3", reverse = FALSE, domain = sitez$site_area)
  
  lat <- mean(st_coordinates(sitez)[,2])
  lon <- mean(st_coordinates(sitez)[,1])
  ##plot on a map
  
    
    ## create base base
    m <- leaflet() %>% 
      setView(lon, lat, zoom = 6) %>% 
      addProviderTiles('CartoDB.Positron')
    
      
      m <- m %>% addCircleMarkers(data = sitez, fillColor = ~pal_st(site_type), color = 'black', weight = 1,
                                  opacity = 1.0, fillOpacity = 1.0,
                                  popup = paste("site code:", sitez$site, "<br>",
                                                "site name:", sitez$site_name, "<br>",
                                                "site type:", sitez$site_type, "<br>",
                                                "date start:", sitez$date_start.y, "<br>",
                                                "date end:", sitez$date_end.y, "<br>",
                                                "data source:", sitez$data_source.y, "<br>",
                                                "sampling process:", sitez$sampling_process, "<br>",
                                                "sampling process:", sitez$sampling_point, "<br>",
                                                "observation count:", sitez$observation_count.y, "<br>",
                                                "sampling period:", sitez$period, "<br>"), group = 'site type')
      
    
    m <- m %>% addLegend("bottomleft", pal=pal_st, values=sitez$site_type, opacity=1, title = "site type",
                         group = "site type")
    
    m <- m %>% addCircleMarkers(data = sitez, fillColor = ~pal_sa(site_area), color = 'black', weight = 1,
                                opacity = 1.0, fillOpacity = 1.0,
                                popup = paste("site code:", sitez$site, "<br>",
                                              "site name:", sitez$site_name, "<br>",
                                              "site type:", sitez$site_type, "<br>",
                                              "date start:", sitez$date_start.y, "<br>",
                                              "date end:", sitez$date_end.y, "<br>",
                                              "data source:", sitez$data_source.y, "<br>",
                                              "sampling process:", sitez$sampling_process, "<br>",
                                              "sampling process:", sitez$sampling_point, "<br>",
                                              "observation count:", sitez$observation_count.y, "<br>",
                                              "sampling period:", sitez$period, "<br>"), group = 'site area')
    
    
    m <- m %>% addLegend("bottomleft", pal=pal_sa, values=sitez$site_area, opacity=1, title = "site area",
                         group = "site area")
    
    m <- m %>% addLayersControl(baseGroups = c('site type', 'site area'),
                                options = layersControlOptions(collapsed = FALSE))
    
    m
    
    if(write_out == TRUE){
    
    dir.create("plots/concs/", recursive = TRUE)
    ## use html widget saveWidget function to make a standalone html
    withr::with_dir('plots/concs/', saveWidget(m, file = paste0(variable, ".html")))  
    }
    
    return(m)
    
    ## error catch in case one group gets an error
    ##  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}


mod_obs_combine <- function(raster_list, sites_sf, species){

  mod_obs_spec <- list()
  for (s in species){
  
  rstr_in <- brick(raster_list[grepl(s, names(raster_list))])
  
  min_d8 <- ymd_hm(gsub("\\.", "_", gsub("X", "", names(rstr_in))))[1]
  max_d8 <- ymd_hm(gsub("\\.", "_", gsub("X", "", names(rstr_in))))[rstr_in@data@nlayers]
  
  s_sf <- filter(sites_sf, variable == s)

  site_dat <- get_saq_observations(site = unique(s_sf$site), start = year(min_d8), end = year(max_d8), variable = s) %>% 
    filter(date >= min_d8 & date <= max_d8) %>% 
    select(date, site, obs = value) %>% 
    timeAverage('hour', type = 'site')
  
  sites_in <- unique(s_sf$site)
  
  df <- data.frame(t(extract(rstr_in, s_sf)))
  
  names(df) <- s_sf$site
  
  rast_d8s <- ymd_hm(gsub("X", "", row.names(df)))
  
  dat <- data.frame(date = rast_d8s, df)
  
  dat_species <- dat %>% 
    melt(c('date')) %>% 
    select(date, site = 'variable', mod = value) %>% 
    left_join(site_dat, by = c('date', 'site')) %>% 
  mutate(species = s)
  
  mod_obs_spec[[s]] <- dat_species
  
  print(s)
  
  }
  
  all_mod_obs <- do.call(rbind, mod_obs_spec)
  
  return(all_mod_obs)
  
}

rm_obs <- function(trees = 300, samples = 300, species, variables = c("wd", "ws", "air_temp", "RH", "date_unix", "day_julian", "weekday", "hour"))


# Met normalisation
list_rm <- rmw_do_all(
  rmw_prepare_data(AQ_met, value = "value"),
  variables,
  n_trees = trees,
  n_samples = samples,
  verbose = TRUE
)

# Check model object's performance
rmw_model_statistics(list_rm$model)

# Check if model has suffered from overfitting
p1 <- rmw_predict_the_test_set(list_rm$model, list_rm$observations) %>% 
  rmw_plot_test_prediction()

# Plot variable importances
p2 <- list_rm$model %>% 
  rmw_model_importance() %>% 
  rmw_plot_importance()

# Plot normalised time series
p3 <- rmw_plot_normalised(list_rm$normalised) +
  labs(subtitle=paste0("Monitoring site: ", site_name," Site Type: ", site_type), 
       y=key_aq, 
       x="Year",
       title=paste0("Weather normalised ", species, " Concentrations at ", u), 
       caption = paste0("Data source: " , source_dat, " accessed using the saqgetr R package. Weather normalisation carried out using the rmweather R package"))

p4 <- ggdraw() +
  draw_plot(p1, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(p2, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(p3, x = 0, y = 0, width = 1, height = 0.5)

filename <- paste0("plots/", u, "_", v, "_metNormalised.png")
png(filename, width=20000, height=18000, units="px", res=1400)
print(p9)
dev.off()


# 
# import_surf_ALL <- function(path = 'mod_files/output', conc_pattern = "_conc-sfc_", meteo_pattern = "" write_out = TRUE, species,  output_crs = 4326, output_units = 'mass'){
#   
#   # path <- "mod_files/output"
#   # pattern = "_conc-sfc_"
#   # species <- 'o3'
#   # output_units <- 'mass'
#   # output_crs = 4326
#   
#   files_p <- list.files(path, pattern, full.names = TRUE)
#   files <- files_p[!grepl('metout', files_p)] ## metout files are generated as part of defualt model run
#   files <- files[grepl('.nc', files)]
#   ## open one file to get species
#   lefile <- nc_open(files[1])
#   
#   longitude <- ncvar_get(lefile, "longitude")
#   latitude <- ncvar_get(lefile, "latitude")
#   lon_res <- (longitude[2]-longitude[1])/2
#   lat_res <- (latitude[2]-latitude[1])/2
#   ##create min x and y
#   x_min <- min(longitude)-lon_res
#   x_max <- max(longitude)+lon_res
#   y_min <- min(latitude)-lat_res
#   y_max <- max(latitude)+lat_res
#   ##determine number of lat and lon points
#   n_y <- NROW(latitude)
#   n_x <- NROW(longitude)
#   
#   ##work out number of x and y cells
#   
#   ##create a data frame to setup polygon generation
#   df <- data.frame(X = c(x_min, x_max, x_max, x_min),
#                    Y = c(y_max, y_max, y_min, y_min))
#   
#   ##generate a polygon of the area
#   vgt_area <- df %>%
#     st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
#     dplyr::summarise(data = st_combine(geometry)) %>%
#     st_cast("POLYGON")
#   
#   nc_close(lefile)
#   rstz <- list()
#   for (f in files){
#     
#     lefile <- nc_open(f)
#     
#     TIME <- ncvar_get(lefile, "time", start = c(1), count = c(1))
#     
#     time_since <- str_sub(lefile$dim$time$units, 15, -14)
#     
#     d8_time <- paste0((lubridate::ymd(time_since) + lubridate::seconds(TIME)), " ", sprintf("%02d", seq(1:24)-1), ":00")
#     d8 <- lubridate::ymd(lubridate::ymd(time_since) + lubridate::seconds(TIME))
#     ## import variable, convert to brick and transpose
#     var_in <- t(brick(ncvar_get(lefile, species, start = c(1,1,1,1), count = c(n_x, n_y, 1,24))))
#     
#     ##define the crs
#     crs(var_in) <- 4326
#     ## define the extent
#     bb <- extent(vgt_area)
#     extent(var_in) <- bb
#     ## flip the domain
#     r1 <- flip(var_in, direction = 'y')
#     
#     if(output_units == 'mass'){
#       r1_out <- r1*10^9
#     }
#     if(output_units == 'volume'){
#       r1_out <- r1*var_info$units
#     }
#     
#     r1_out <- raster::projectRaster(r1_out, crs = output_crs)
#     
#     names(r1_out) <- d8_time
#     
#     nam_out <- as.character(d8)
#     
#     rstz[[nam_out]] <- r1_out
#     
#     print(paste('importing ', f, species))
#     
#     
#   }
#   
#   conc_rast <- brick(rstz)
#   
#   #path <- "output"
#   #pattern = "_meteo_"
#   
#   files_p <- list.files(path, pattern, full.names = TRUE)
#   files <- files_p[!grepl('metout', files_p)] ## metout files are generated as part of defualt model run
#   files <- files[grepl('.nc', files)]
#   ## open one file to get species
#   lefile <- nc_open(files[1])
#   
#   longitude <- ncvar_get(lefile, "longitude")
#   latitude <- ncvar_get(lefile, "latitude")
#   lon_res <- (longitude[2]-longitude[1])/2
#   lat_res <- (latitude[2]-latitude[1])/2
#   ##create min x and y
#   x_min <- min(longitude)-lon_res
#   x_max <- max(longitude)+lon_res
#   y_min <- min(latitude)-lat_res
#   y_max <- max(latitude)+lat_res
#   ##determine number of lat and lon points
#   n_y <- NROW(latitude)
#   n_x <- NROW(longitude)
#   
#   ##work out number of x and y cells
#   
#   ##create a data frame to setup polygon generation
#   df <- data.frame(X = c(x_min, x_max, x_max, x_min),
#                    Y = c(y_max, y_max, y_min, y_min))
#   
#   ##generate a polygon of the area
#   vgt_area <- df %>%
#     st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
#     dplyr::summarise(data = st_combine(geometry)) %>%
#     st_cast("POLYGON")
#   
#   nc_close(lefile)
#   rstz <- list()
#   for (f in files){
#     
#     lefile <- nc_open(f)
#     
#     TIME <- ncvar_get(lefile, "time", start = c(1), count = c(1))
#     
#     time_since <- str_sub(lefile$dim$time$units, 15, -14)
#     
#     d8_time <- paste0((lubridate::ymd(time_since) + lubridate::seconds(TIME)), " ", sprintf("%02d", seq(1:24)-1), ":00")
#     d8 <- lubridate::ymd(lubridate::ymd(time_since) + lubridate::seconds(TIME))
#     
#     var_dim <- NROW(dim(ncvar_get(lefile, variable)))
#     
#     lefile$var$wdir_surf$ndims
#     if(var_dim == 4){
#       var_in <- t(brick(ncvar_get(lefile, variable, start = c(1,1,1,1), count = c(n_x, n_y, 1, 24))))
#     }
#     
#     if(var_dim == 3){
#       var_in <- t(brick(ncvar_get(lefile, variable, start = c(1,1,1), count = c(n_x, n_y, 24))))
#     }
#     
#     
#     ##define the crs
#     crs(var_in) <- 4326
#     ## define the extent
#     bb <- extent(vgt_area)
#     extent(var_in) <- bb
#     ## flip the domain
#     r1 <- flip(var_in, direction = 'y')
#     
#     r1_out <- raster::projectRaster(r1, crs = output_crs)
#     
#     names(r1_out) <- d8_time
#     
#     nam_out <- as.character(d8)
#     
#     rstz[[nam_out]] <- r1_out
#     
#     print(paste('importing ', f, variable))
#     
#     
#   }
#   
#   
#   
# }
# 
# 
