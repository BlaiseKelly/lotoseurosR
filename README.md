LOTOS-EUROS R TOOLS
================

## Introduction

lotoseurosR is a collection of functions to aid the processing of inputs
and outputs for the LOTOS-EUROS (LE) chemical transport model
https://lotos-euros.tno.nl/open-source-version/

Downloading the model and running with default settings will generate
the output in the mod_files/output folder. The examples given in this
document are based on these files but should also work with any surface
LE model output files.

The input_surf_concs and input_surf_meteo will import the concentration
or meteo outputs and output as a raster brick that can be plotted or
used with openair, worldmet and saqgetr to visualise the data and assist
with validation.

By converting to georeferenced raster layers and raster bricks, the
process of extracting time-series, regridding (e.g. to combine with data
on different grid systems/resolutions) and plotting is simplified and
sped up. Layered image files can also be an efficient file format in
terms of read/write speed and disk size, especially compared with
dataframes (e.g. csv). It should be noted that saving raster files as
other formats (e.g. native .RDS files) might appear to work, but they
are linked to the memory. On restarting R they appear to be loaded, but
when the RAM is cleared on restart they will be empty). For this reason
the option to write out the raster brick is included in the import
functions.

| le_specs | omrekenfactor | tmap_nam                           | dg_nam                              |
|:---------|--------------:|:-----------------------------------|:------------------------------------|
| o3       |    1967213115 | ‘O’\[3\]\*’ (‘\* mu*’g/m’^3*’)’    | O<sub>3</sub> (μg/m<sup>3</sup>)    |
| no2      |    1885450820 | ‘NO’\[2\]\*’ (‘\* mu*’g/m’^3*’)’   | NO<sub>2</sub> (μg/m<sup>3</sup>)   |
| no       |    1229918033 | ‘NO (’\* mu*‘g/m’^3*’)’            | NO (μg/m<sup>3</sup>)               |
| nh3      |     697991803 | ‘NH’\[3\]\*’ (‘\* mu*’g/m’^3*’)’   | NH<sub>3</sub> (μg/m<sup>3</sup>)   |
| so2      |    2625524590 | ‘SO’\[2\]\*’ (‘\* mu*’g/m’^3*’)’   | SO<sub>2</sub> (μg/m<sup>3</sup>)   |
| hno3     |    2582377049 | ‘HNO’\[3\]\*’ (‘\* mu*’g/m’^3*’)’  | HNO<sub>3</sub> (μg/m<sup>3</sup>)  |
| co       |    1147950820 | ‘CO’\*’ (‘\* mu*’g/m’^3*’)’        | CO (μg/m<sup>3</sup>)               |
| tpm25    |    1000000000 | ‘PM’\[2.5\]\*’ (‘\* mu*’g/m’^3*’)’ | PM<sub>10</sub> (μg/m<sup>3</sup>)  |
| tpm10    |    1000000000 | ‘PM’\[10\]\*’ (‘\* mu*’g/m’^3*’)’  | PM<sub>2.5</sub> (μg/m<sup>3</sup>) |

table of varient names

``` r
conc_rast <- import_surf_concs(species = 'o3', write_out = FALSE)
```

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120701.nc o3"

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120702.nc o3"

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120703.nc o3"

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120704.nc o3"

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120705.nc o3"

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120706.nc o3"

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120707.nc o3"

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120708.nc o3"

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120709.nc o3"

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120710.nc o3"

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120711.nc o3"

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120712.nc o3"

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120713.nc o3"

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120714.nc o3"

    Warning in raster::projectRaster(r1_out, crs = output_crs): input and ouput crs
    are the same

    [1] "importing  mod_files/output/LE_v2.2.001_conc-sfc_20120715.nc o3"

The plotting function will plot a summary of the period in the raster
file or the first 64 layers animated. The statistic option allows for
any statistical function to be applied. The base functions are ‘mean’,
‘sum’, ‘sd’.

``` r
#p1 <- conc_plot(raster_in = conc_rast, all_layers = TRUE, variable = 'o3', start_hr = 1, end_hr = 30)
```

``` r
sub_domain <- get_country_domain(countries = c('Greece'))
```

``` r
  lon <- mean(st_coordinates(sub_domain)[,1])
  lat <- mean(st_coordinates(sub_domain)[,2])

m <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
  setView(lon, lat, zoom = 4)

m <- m %>% addPolygons(data = sub_domain, color = "black", weight = 1,
                            opacity = 1.0, fillOpacity = 0.9,
                            fillColor = "yellow")

#m <- m %>% hideGroup(c("kartoblaaden"))

m
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)

``` r
#time_dygraph(concs_dat, variable = c(all_labs[c(3,7,8,9,10)], 'Total'), ylab = spec_df$dg_nam[1])

#p1 <- plot_obs(data_brick = )
```
