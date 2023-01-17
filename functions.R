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

select <- dplyr::select

varient_df <- data.frame(le_specs = c("o3", "no2", "no", "nh3", "so2", 'hno3', "co", "tpm25", "tpm10"),
                         omrekenfactor = c(1e12*0.048/24.4,  1e12*0.046005/24.4, 1e12*0.03001/24.4, 1e12*0.017031/24.4, 1e12*0.0640628/24.4, 1e12*0.06301/24.4,1e12*0.02801/24.4, 1e9, 1e9),
                         title = c("'O'[3]*' ('* mu*'g/m'^3*')'", "'NO'[2]*' ('* mu*'g/m'^3*')'","'NO ('* mu*'g/m'^3*')'",
                                   "'NH'[3]*' ('* mu*'g/m'^3*')'","'SO'[2]*' ('* mu*'g/m'^3*')'","'HNO'[3]*' ('* mu*'g/m'^3*')'",
                                   "'CO'*' ('* mu*'g/m'^3*')'", "'PM'[2.5]*' ('* mu*'g/m'^3*')'",
                                   "'PM'[10]*' ('* mu*'g/m'^3*')'"))

## espg codes given here https://epsg.io/

varient_df <- varient_df[7:8,]

import_surf_concs <- function(path = 'mod_files/output', pattern = "_conc-sfc_", species,  output_crs = 4326, output_units = 'mass'){
  
  path <- "output"
  pattern = "_conc-sfc_"
  species <- 'o3'
  output_units <- 'mass'
  
  files_p <- list.files(path, pattern, full.names = TRUE)
  files <- files_p[!grepl('metout', files_p)]
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
    
    r1_out <- raster::projectRaster(r1_out, output_crs)
    
    names(r1_out) <- d8_time
    
    nam_out <- as.character(d8)
    
    rstz[[nam_out]] <- r1_out
    
    print(paste('importing ', f, species))
   

  }

  return(brick(rstz))
}


conc_in <- import_surf_concs(species = varient_df$le_specs[1])



meteo_spec <- c('temper', 'wspd_surf', 'wdir_surf')


import_surf_meteo <- function(path = 'output', pattern = "_meteo_", variables = 'temper',  output_crs = 4326){
  
  path <- "output"
  pattern = "_meteo_"
  
  files_p <- list.files(path, pattern, full.names = TRUE)
  files <- files_p[!grepl('metout', files_p)]
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
      
      var_dim <- NROW(dim(ncvar_get(lefile, species)))
      
       lefile$var$wdir_surf$ndims
      if(var_dim == 4){
        var_in <- t(brick(ncvar_get(lefile, species, start = c(1,1,1,1), count = c(n_x, n_y, 1, 24))))
      }
      
      if(var_dim == 3){
        var_in <- t(brick(ncvar_get(lefile, species, start = c(1,1,1), count = c(n_x, n_y, 24))))
      }
      

      ##define the crs
      crs(var_in) <- 4326
      ## define the extent
      bb <- extent(vgt_area)
      extent(var_in) <- bb
      ## flip the domain
      r1 <- flip(var_in, direction = 'y')
      
      r1_out <- raster::projectRaster(r1_out, output_crs)
      
      names(r1_out) <- d8_time
      
      nam_out <- as.character(d8)
      
      rstz[[nam_out]] <- r1_out
      
      print(paste('importing ', f, s))
      
      
    }
  
  return(brick(rstz))
}




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

find_meteo_obs <- function(sites){
  
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


regrid <- function(){}

meteo_plot <- function(){
  
  bg <- basemaps::basemap_raster(ext=subset(days_brick,1), map_service = "carto", map_type = "light")
  
  tm_1 <- tm_shape(bg)+
    tm_rgb()+
    tm_shape(days_brick) +
    tm_raster(palette = topas_orig_pal,alpha = 0.6, style = 'cont', breaks = topas_bks, title = parse(text = var_info$title))+
    tm_layout(legend.outside = FALSE, frame = FALSE, legend.outside.position = 'left', title.size = 1, panel.labels = paste0(lbl, kr8_d8_hr),
              panel.label.color = 'white', panel.label.bg.color = 'black',
              bg.color = "white")+
    # tm_shape(e_shp)+
    # tm_lines(col = "black", lwd = 0.5)+
    # tm_shape(nl_shp)+
    # tm_lines(col = "black", lwd = 0.5)+
    tm_legend(position = c("left", "top"))+
    tm_logo("dat/426_1_TNO_ifl_zwart.png",
            height = 2,
            margin = 0.2,
            position = c("left", "bottom"),
            just = NA)+
    tm_facets(nrow = 1, ncol = 1)
  lbl_out <- str_sub(lbl, 1,-2)
  tmap_animation(tm_1, filename = paste0("C:/Users/kellyb/OneDrive - TNO/topas_plots/", start_date, "_", end_date, "_", lbl_out, "_small.gif"), delay = 60)
  #tmap_animation(tm_1, filename = paste0("C:/Users/kellyb/OneDrive - TNO/topas_plots/", start_date, "_", end_date, "_", lbl, ".gif"),width = 2100, height = 2200, dpi = 300, delay = 60)
  
  
}

conc_plot <- function(){}


meteo_list <- import_meteo(species = meteo_spec[1])


find_aq_sites <- function(data_brick, site_type, variable){
  
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
    filter(site_type %in% 'background', site_area %in% c('urban', 'rural'), variable == 'o3', period == 'hour') %>%
    filter(date_start.y < min_d8 & date_end.y > max_d8) %>% 
    mutate(variable_long = tolower(variable_long)) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  
}



df <- find_saq_sites(data_brick = conc_in)

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






select <- dplyr::select

all_d8s <- list.files("dat/bricks", pattern = "_EU_")

start_date <-  "2022-12-01"

end_date <- as.character(Sys.Date())

kr8_d8 <- as.character(seq(
  from=as.POSIXct(start_date, tz="UTC"),
  to=as.POSIXct(end_date, tz="UTC"),
  by="day"
) )
kr8_d8_2 <- kr8_d8[NROW(kr8_d8):1]
for (d8 in kr8_d8){
d8 <- ymd(d8)
y <- year(d8)
m <- sprintf("%02d", month(d8))
d <- sprintf("%02d", day(d8))

yesterday <- gsub("-", "", as.character(d8-1))

## EUROPE WIDE



lelab <- nc_open(le_files[1])

lab_names <- data.frame(ncvar_get(lelab, "labelnames"))
saveRDS(lab_names, "dat/EU_label_names.RDS")

lab_nums <- sprintf("%04d", seq(1:NROW(lab_names)))

longitude <- ncvar_get(lelab, "longitude")
latitude <- ncvar_get(lelab, "latitude")
##create min x and y
x_min <- min(longitude)
x_max <- max(longitude)
y_min <- min(latitude)
y_max <- max(latitude)
##determine number of lat and lon points
n_y <- NROW(latitude)
n_x <- NROW(longitude)

##work out number of x and y cells
res <- (x_max-x_min)/n_x
res <- (y_max-y_min)/n_y

##create a data frame to setup polygon generation
df <- data.frame(X = c(x_min, x_max, x_max, x_min),
                 Y = c(y_max, y_max, y_min, y_min))

units <- data.frame(unit = c("mole mole-1", "kg m-3"),
                   units = c(10^9*2,10^9))

varz <- data.frame(le_vars = c("no2", "no", "nh3", "so2", 'hno3', "co", "ch4", "tpm25", "tpm10", "tnmvoc", "o3"),
                   units = c(10^9*2,10^9*2, 10^9*2,10^9*2,10^9*2, 10^9*2,10^9*2,10^9, 10^9,10^9,10^9))
varz <- slice(varz,8)
v <- varz$le_vars[8]
##generate a polygon of the area
vgt_area <- df %>%
  st_as_sf(coords = c("X", "Y"), crs = latlong) %>%
  dplyr::summarise(data = st_combine(geometry)) %>%
  st_cast("POLYGON")

for (v in varz$le_vars){
  tryCatch({
rstz <- list()
d8s <- list()

  var_info <- filter(varz, le_vars == v)
l <- le_files[1]
for (l in le_files){
  tryCatch({
  le <- nc_open(l)

var_in <- ncvar_get(le, v, start = c(1,1,1,1,1), count = c(n_x, n_y, 1,24,NROW(lab_names)))

TIME <- ncvar_get(le, "time", start = c(1), count = c(1))

time_since <- str_sub(lelab$dim$time$units, 15, -14)

d8_time <- lubridate::ymd(time_since) + lubridate::seconds(TIME)

labels <- seq(1:NROW(lab_names))

for (lab in labels){

for (t in seq(1,24)){

  ##generate raster from it
  var <- var_in[1:n_x,1:n_y, t, lab]
  r1 <- t(var)
  r1 <- raster(r1)
  ##define the extent
  crs(r1) <- latlong
  bb <- extent(vgt_area)
  extent(r1) <- bb
  r1 <- flip(r1, direction = 'y')
  r1_ug <- r1*var_info$units
  lab_nam <- sprintf("%04d", lab)
  nam_out <- paste0(lab_nam, "_", d8_time, " ", t, ":00")

  rstz[[nam_out]] <- r1_ug
  d8s[[nam_out]] <- nam_out
print(nam_out)

}
}
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

for (la in lab_nums){

L <- grepl(la, names(rstz))

full_names <- lab_names[which(lab_nums == la),]

d8_nam <- paste(full_names, str_sub(do.call(rbind, d8s[L]), 6,-1))

rast_brick <- brick(rstz[L])
names(rast_brick) <- d8_nam

writeRaster(rast_brick, filename=paste0("dat/bricks/", yesterday, "_", v, "_", la, "_EU_brick.TIF"), format="GTiff", overwrite = TRUE)


}
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

}
