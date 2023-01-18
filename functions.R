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
library(htmlwidgets)
library(ggplot2)
library(gganimate)
library(pals)
library(tmap)
library(exactextractr)

select <- dplyr::select

varient_df <- data.frame(le_specs = c("o3", "no2", "no", "nh3", "so2", 'hno3', "co", "tpm25", "tpm10"),
                         omrekenfactor = c(1e12*0.048/24.4,  1e12*0.046005/24.4, 1e12*0.03001/24.4, 1e12*0.017031/24.4, 1e12*0.0640628/24.4, 1e12*0.06301/24.4,1e12*0.02801/24.4, 1e9, 1e9),
                         title = c("'O'[3]*' ('* mu*'g/m'^3*')'", "'NO'[2]*' ('* mu*'g/m'^3*')'","'NO ('* mu*'g/m'^3*')'",
                                   "'NH'[3]*' ('* mu*'g/m'^3*')'","'SO'[2]*' ('* mu*'g/m'^3*')'","'HNO'[3]*' ('* mu*'g/m'^3*')'",
                                   "'CO'*' ('* mu*'g/m'^3*')'", "'PM'[2.5]*' ('* mu*'g/m'^3*')'",
                                   "'PM'[10]*' ('* mu*'g/m'^3*')'"))

## espg codes given here https://epsg.io/

varient_df <- varient_df[7:8,]
# 
# rc <- read.csv("mod_files/run/lotos-euros.rc", header = FALSE)
# 
# ## filter out the lines with !
# rc <- filter(rc, !grepl("!", V1))
# 
# rc <- colsplit(rc$V1, ":", c("C1", "C2"))

#sub_dirs <- filter(rc, grepl(".rc", C2) & grepl("/main.directory/", C2))



import_surf_concs <- function(path = 'mod_files/output', pattern = "_conc-sfc_", write_out = TRUE, species,  output_crs = 4326, output_units = 'mass'){
  
  # path <- "mod_files/output"
  # pattern = "_conc-sfc_"
  # species <- 'o3'
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
    ## import variable, convert to brick and transpose
    var_in <- t(brick(ncvar_get(lefile, species, start = c(1,1,1,1), count = c(n_x, n_y, 1,24))))
    
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
      r1_out <- r1*var_info$units
    }
    
    r1_out <- raster::projectRaster(r1_out, crs = output_crs)
    
    names(r1_out) <- d8_time
    
    nam_out <- as.character(d8)
    
    rstz[[nam_out]] <- r1_out
    
    print(paste('importing ', f, species))
   

  }
  
  if(write_out == TRUE){
  
    dir.create('bricks/concs', recursive = TRUE)
    ## convert to a terra geospatial raster and use terra writeRaster function to preserve layer names
    terra::writeRaster(terra::rast(brick(rstz)), filename=paste0("bricks/concs/", species, ".TIF"), overwrite = TRUE)
    
    #assign(s, s_rast)
    
    print(paste0("raster brick written to: bricks/concs/", species, ".TIF"))
  }

  return(brick(rstz))
}


conc_in <- import_surf_concs(species = varient_df$le_specs[1], write_out = TRUE)



meteo_spec <- data.frame(var = c('temper', 'wspd_surf', 'wdir_surf'),
                         


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


meteo_in <- import_surf_meteo(write_out = TRUE)

u_in <- import_surf_meteo(variable = 'usurf')
v_in <- import_surf_meteo(variable = 'vsurf')


plot_obs <- function{data_brick, variable}{
  
  ## set palette for site types
  pal_g <- colorFactor("Set3", reverse = FALSE, domain = site_sums_sf$site_type)
  
  ##plot on a map
  
  for (c in cats){
    
    ## tryCatch({
    cat <- get(c)
    
    ## create base base
    m <- leaflet() %>% 
      setView(lon, lat, zoom = 6) %>% 
      addProviderTiles('CartoDB.Positron')
    
    ## find unique variables
    variable_u <- unique(cat$variable_long)
    
    ## find how many variable colours are needed
    n <- NROW(variable_u)
    ## generate a palette of for all variables
    palette <- distinctColorPalette(n)
    
    ## loop through the variables to generate ability to toggle between species
    for (u in variable_u){
      
      p <- palette[which(variable_u == u)]
      
      df <- filter(cat, variable_long == u)
      
      m <- m %>% addCircleMarkers(data = df, fillColor = ~pal_g(site_type), color = 'black', weight = 1,
                                  opacity = 1.0, fillOpacity = 1.0,
                                  popup = paste("site code:", df$site, "<br>",
                                                "site name:", df$site_name, "<br>",
                                                "site type:", df$site_type, "<br>",
                                                "date start:", df$date_start.y, "<br>",
                                                "date end:", df$date_end.y, "<br>",
                                                "data source:", df$data_source.y, "<br>",
                                                "sampling process:", df$sampling_process, "<br>",
                                                "sampling process:", df$sampling_point, "<br>",
                                                "observation count:", df$observation_count.y, "<br>",
                                                "sampling period:", df$period, "<br>"), group = u)
      
    }
    
    m <- m %>% addLegend("bottomleft", pal=pal_g, values=site_sums_sf$site_type, opacity=1, title = "site type",
                         group = "site type")
    
    m <- m %>% addLayersControl(baseGroups = c(variable_u),
                                options = layersControlOptions(collapsed = FALSE))
    
    n <- sprintf('%02d', which(cats == c))
    
    ## use html widget saveWidget function to make a standalone html
    withr::with_dir('./', saveWidget(m, file = paste0(n, "_", c, ".html")))  
    
    ## error catch in case one group gets an error
    ##  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
}


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

sub_domain <- get_country_domain()

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

noaa_in <- find_noaa_domain(domain = sub_domain, html_out = TRUE)

find_noaa_sites <- function(sites){
  
  ## import all meteo sites
  met_info <- getMeta()
  ## geo reference them
  met_info <- st_as_sf(met_info, coords = c("longitude", "latitude"), crs = 4326)
  
  ## find nearest meteo station to site
  data_sites_sf$nearest_NOAA <- met_info$code[st_nearest_feature(data_sites_sf, met_info)]
  
  all_met_sites <- unique(data_sites_sf$nearest_NOAA)
  
  ## find data range
  start_d8 <- year(start_date)
  end_d8 <- year(end_date)
  d8s <- seq(start_d8, end_d8)
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
  
  saveRDS(obs_met, paste0("out/met_obs_", a, "_", s, ".RDS"))
  
}

}
  
  }


ws_wd_plot <- function(ws_rast = NA, wd_rast = NA, domain, start_hr = 1, end_hr = 4){

  
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

wswd_plot <- ws_wd_plot(ws_rast = ws_in, wd_rast = wd_in, domain = sub_domain)

wswd_plot

uv_plot <- u_v_plot(u_rast = u_in, v_rast = v_in, domain = sub_domain)

new_grid <- disaggregate(subset(conc_dom,1), fact = 2)


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



new_l <- regrid(raster2change = conc_dom, desired_grid = new_grid, statistic = 'sum')





raster_in <- ws_in
variable = 'wspd_surf'

meteo_plot <- function(raster_in, all_layers = TRUE, statistic = 'mean', variable = 'temper', average = TRUE, start_hr = 1, end_hr = 64){
  
  if(all_layers = TRUE){
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
    tm_layout(legend.outside = FALSE, frame = FALSE, legend.outside.position = 'left', title.size = 1, panel.labels = paste(variable,d8s),
              panel.label.color = 'white', panel.label.bg.color = 'black',
              bg.color = "white")+
    tm_legend(position = c("left", "top"))+
    tm_facets(nrow = 1, ncol = 1)
  
  lbl_out <- str_sub(lbl, 1,-2)
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

conc_plot <- function(raster_in, all_layers = TRUE, statistic = 'mean', domain, variable = 'tpm25', average = TRUE, start_hr = 1, end_hr = 4){
  
  # raster_in <- conc_in
  # statistic <- mean
  # variable = 'tpm25'
  # domain = sub_domain
  # start_hr = 1
  # end_hr = 10
  
  if(all_layers == TRUE){
    
    raster_in <- crop(raster_in,domain)
    raster_in <- subset(raster_in, start_hr:end_hr)
    bg <- basemaps::basemap_raster(ext=subset(raster_in,1), map_service = "carto", map_type = "light")
    
    legend_title <- filter(varient_df, le_specs == variable)
    
    if(NROW(legend_title)<1){
      legend_title <- variable
    }
    
    d8s <- as.character(ymd_hm(gsub("\\.", "_", gsub("X", "", names(raster_in)))))
    
    bks <- seq(min(raster_in@data@min), max(raster_in@data@max), by = (max(raster_in@data@max)-min(raster_in@data@min))/20)
    
    pal_A <- pals::jet(NROW(bks))
    
    tm_1 <- tm_shape(bg)+
      tm_rgb()+
      tm_shape(raster_in) +
      tm_raster(palette = pal_A,alpha = 0.6, style = 'cont', breaks = bks, title = parse(text = legend_title$title))+
      tm_layout(legend.outside = FALSE, frame = FALSE, legend.outside.position = 'left', title.size = 1, panel.labels = d8s,
                panel.label.color = 'white', panel.label.bg.color = 'black',
                bg.color = "white")+
      tm_legend(position = c("left", "top"))+
      tm_facets(nrow = 1, ncol = 1)
    
    dir.create("plots/concs", recursive = TRUE)
    tmap_animation(tm_1, filename = paste0("plots/concs/", variable, "_", 
                                           gsub(" ", "_", str_sub(d8s[1], 1,-7)), "_", gsub(" ", "_", str_sub(d8s[NROW(d8s)], 1,-7)), ".gif"), delay = 60)
    #tmap_animation(tm_1, filename = paste0("C:/Users/kellyb/OneDrive - TNO/topas_plots/", start_date, "_", end_date, "_", lbl, ".gif"),width = 2100, height = 2200, dpi = 300, delay = 60)
    
  } else {
   
   
    d8s <- as.character(ymd_hm(gsub("\\.", "_", gsub("X", "", names(raster_in)))))
    
    raster_in <- calc(crop(raster_in,domain), statistic)
    
    bg <- basemaps::basemap_raster(ext=subset(raster_in,1), map_service = "carto", map_type = "light")
    #bg <- crop(projectRaster(bg, crs = 4326), raster_in)
    
   legend_title <- filter(varient_df, le_specs == variable)
    
    bks <- seq(min(raster_in@data@min), max(raster_in@data@max), by = (max(raster_in@data@max)-min(raster_in@data@min))/20)
    
    pal_A <- pals::jet(NROW(bks))
    
    tm_1 <- tm_shape(bg)+
      tm_rgb()+
      tm_shape(raster_in) +
      tm_raster(palette = pal_A,alpha = 0.6, style = 'cont', breaks = bks, title = parse(text = legend_title$title))+
      tm_layout(legend.outside = FALSE, frame = FALSE, legend.outside.position = 'left', title.size = 1, panel.labels = parse(text = legend_title$title),
                panel.label.color = 'white', panel.label.bg.color = 'black',
                bg.color = "white")+
      tm_legend(position = c("left", "top"))

    dir.create("plots/concs", recursive = TRUE)
    tmap_save(tm_1, filename = paste0("plots/concs/", variable, ".png"))
    
  }
  
}



p1 <- conc_plot(raster_in = conc_in, all_layers = TRUE, start_hr = 1, end_hr = 30,  domain = sub_domain, statistic = mean, variable = 'tpm25')

sitez <- saqgetr::get_saq_sites()

site_types <- expand.grid(area = unique(sitez$site_area), type = unique(sitez$site_type)) %>% 
  filter(!is.na(area)) %>% 
  filter(!is.na(type)) %>% 
  transmute(paste0(area, "_", type))

conc_dom <- crop(conc_in, sub_domain)

find_aq_sites <- function(data_brick, site_type = c('background'), site_area = c('urban', 'rural', 'suburban'), variable){
  data_brick <- conc_dom
  site_type = c('background')
  site_area = c('urban', 'rural', 'suburban')
  variable = 'tpm25'
  d8_range <- ymd_hm(gsub("\\.", "_", gsub("X", "", names(data_brick))))
  min_d8 <- min(d8_range)
  max_d8 <- max(d8_range)
  ## get all sites from database
  sitez <- saqgetr::get_saq_sites()
  
  ## get more detailed data
  processes <- get_saq_processes()
  
  ## combine and convert to sf
  site_sums_sf <- sitez %>% 
    left_join(processes, by = 'site') %>% 
    filter(!is.na(latitude)) %>%
    filter(site_type %in% site_type, site_area %in% site_area, variable == 'o3', period == 'hour') %>%
    filter(date_start.y < min_d8 & date_end.y > max_d8) %>% 
    mutate(variable_long = tolower(variable_long)) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  ## define extent of brick
  r1 <- st_as_sf(st_as_sfc(st_bbox(subset(data_brick,1))))
  ## filter points in area
  site_sf <- site_sums_sf[r1,]
  
  
}



df <- find_aq_sites(data_brick = conc_dom)

plot_obs <- function(data_brick, variable){
  
  sitez <- find_aq_sites(data_brick, variable)
  
  ## set palette for site types
  
  pal_st <- colorFactor("Set3", reverse = FALSE, domain = sitez$site_type)
  pal_sa <- colorFactor("Set3", reverse = FALSE, domain = sitez$site_area)
  
  lat <- st_coordinates(st_centroid(st_as_sf(st_as_sfc(st_bbox(subset(data_brick,1))))))[2]
  lon <- st_coordinates(st_centroid(st_as_sf(st_as_sfc(st_bbox(subset(data_brick,1))))))[1]
  ##plot on a map
  
    
    ## create base base
    m <- leaflet() %>% 
      setView(lon, lat, zoom = 6) %>% 
      addProviderTiles('CartoDB.Positron')
    
      
      m <- m %>% addCircleMarkers(data = sitez, fillColor = ~pal_st(site_type), color = 'black', weight = 1,
                                  opacity = 1.0, fillOpacity = 1.0,
                                  popup = paste("site code:", df$site, "<br>",
                                                "site name:", df$site_name, "<br>",
                                                "site type:", df$site_type, "<br>",
                                                "date start:", df$date_start.y, "<br>",
                                                "date end:", df$date_end.y, "<br>",
                                                "data source:", df$data_source.y, "<br>",
                                                "sampling process:", df$sampling_process, "<br>",
                                                "sampling process:", df$sampling_point, "<br>",
                                                "observation count:", df$observation_count.y, "<br>",
                                                "sampling period:", df$period, "<br>"), group = 'site type')
      
    
    m <- m %>% addLegend("bottomleft", pal=pal_st, values=sitez$site_type, opacity=1, title = "site type",
                         group = "site type")
    
    m <- m %>% addCircleMarkers(data = sitez, fillColor = ~pal_sa(site_area), color = 'black', weight = 1,
                                opacity = 1.0, fillOpacity = 1.0,
                                popup = paste("site code:", df$site, "<br>",
                                              "site name:", df$site_name, "<br>",
                                              "site type:", df$site_type, "<br>",
                                              "date start:", df$date_start.y, "<br>",
                                              "date end:", df$date_end.y, "<br>",
                                              "data source:", df$data_source.y, "<br>",
                                              "sampling process:", df$sampling_process, "<br>",
                                              "sampling process:", df$sampling_point, "<br>",
                                              "observation count:", df$observation_count.y, "<br>",
                                              "sampling period:", df$period, "<br>"), group = 'site area')
    
    
    m <- m %>% addLegend("bottomleft", pal=pal_sa, values=sitez$site_area, opacity=1, title = "site area",
                         group = "site area")
    
    m <- m %>% addLayersControl(baseGroups = c('site type', 'site area'),
                                options = layersControlOptions(collapsed = FALSE))
    
    dir.create("plots/concs/", recursive = TRUE)
    ## use html widget saveWidget function to make a standalone html
    withr::with_dir('plots/concs/', saveWidget(m, file = paste0(variable, ".html")))  
    
    m
    ## error catch in case one group gets an error
    ##  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}


obs <- plot_obs(data_brick = conc_dom, variable = 'o3')

validate_concs <- 

LE_dat <- data.frame(t(extract(conc_in, df)))
names(LE_dat) <- df$site
LE_dat$date <- ymd_hm(gsub("\\.", "_", gsub("X", "", row.names(LE_dat))))
LE_dat <- melt(LE_dat, 'date')
names(LE_dat) <- c('date', 'site', 'mod')

d8_start <- str_sub(as.character(min_d8),-10,-1)
d8_end <- format(as.Date(max_d8), "%yyyy-%m-%d")

all_df <- saqgetr::get_saq_observations(site = df$site,verbose = TRUE, variable = 'o3', start = '2012-07-01', end = '2012-07-15')

all_df2 <- select(all_df, date, site, obs = value)

obs_mod <- left_join(all_df2, LE_dat, by = c('date', 'site'))

library(openair)

modS <- openair::modStats(obs_mod, type = 'site')

modS_all <- openair::modStats(obs_mod)

scatterPlot(filter(obs_mod, obs < 800), x = 'mod', y = 'obs', col = 'jet', method = 'density')

















species_brick <- function(species_name, brick_list, write_out = TRUE){
  
  for (s in species_name){
    
    r <- get()
    
    s_rast <- brick(brick_list[s])
    
    if(write_out == TRUE){
      
      dir.create('bricks/concs')
      ## convert to a terra geospatial raster and use terra writeRaster function to preserve layer names
      terra::writeRaster(terra::rast(s_rast), filename=paste0("bricks/concs/", s, ".TIF"), overwrite = TRUE)
      
      #assign(s, s_rast)
      
      print(paste0("writing raster brick to: bricks/concs/", s, ".TIF"))
    } else {
      
      #assign(s, s_rast)
      
    }
    
  }
  
}

spec_brick <- species_brick(species_name = 'o3', brick_list = conc_in)


import_surf_ALL <- function(path = 'mod_files/output', conc_pattern = "_conc-sfc_", meteo_pattern = "" write_out = TRUE, species,  output_crs = 4326, output_units = 'mass'){
  
  # path <- "mod_files/output"
  # pattern = "_conc-sfc_"
  # species <- 'o3'
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
    ## import variable, convert to brick and transpose
    var_in <- t(brick(ncvar_get(lefile, species, start = c(1,1,1,1), count = c(n_x, n_y, 1,24))))
    
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
      r1_out <- r1*var_info$units
    }
    
    r1_out <- raster::projectRaster(r1_out, crs = output_crs)
    
    names(r1_out) <- d8_time
    
    nam_out <- as.character(d8)
    
    rstz[[nam_out]] <- r1_out
    
    print(paste('importing ', f, species))
    
    
  }
  
  conc_rast <- brick(rstz)
  
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
  
  
  
}


