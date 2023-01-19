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
mod_rast <- import_surf_concs(species = 'o3', write_out = FALSE)
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
#p1 <- mod_plot(raster_in = conc_rast, all_layers = TRUE, variable = 'o3', start_hr = 1, end_hr = 30)
```

``` r
sub_domain <- get_country_domain(countries = c('Greece'))
```

    Loading basemap 'light' from map service 'carto'...

![](README_files/figure-gfm/unnamed-chunk-5-1.png)

![](README_files/figure-gfm/unnamed-chunk-6-1.png)

![](README_files/figure-gfm/unnamed-chunk-7-1.png)

![](README_files/figure-gfm/unnamed-chunk-7-2.png)

![](README_files/figure-gfm/unnamed-chunk-7-3.png)

Model statistics for all observation locations

``` r
kable(ms_all, caption = "model statistics for all ")
```

| default  |    n |      FAC2 |        MB |      MGE |        NMB |      NMGE |     RMSE |         r |        COE |       IOA |
|:---------|-----:|----------:|----------:|---------:|-----------:|----------:|---------:|----------:|-----------:|----------:|
| all data | 5885 | 0.6108751 | -46.41914 | 49.44879 | -0.4400171 | 0.4687358 | 55.47301 | 0.4187155 | -0.9328772 | 0.0335614 |

model statistics for all

``` r
#time_dygraph(concs_dat, variable = c(all_labs[c(3,7,8,9,10)], 'Total'), ylab = spec_df$dg_nam[1])

#p1 <- plot_obs(data_brick = )
```

Model statistics split by site

``` r
kable(ms_sites, caption = "model statistics split by site")
```

| site    |   n |      FAC2 |        MB |      MGE |        NMB |       NMGE |     RMSE |         r |        COE |        IOA |
|:--------|----:|----------:|----------:|---------:|-----------:|-----------:|---------:|----------:|-----------:|-----------:|
| gr0027a | 748 | 0.7513369 | -36.90206 | 39.14820 | -0.3821937 |  0.4054569 | 43.49471 | 0.5247234 | -0.9362756 |  0.0318622 |
| gr0028a | 750 | 0.7120000 | -40.42189 | 43.33507 | -0.4037772 |  0.4328772 | 48.40389 | 0.5089123 | -0.8287756 |  0.0856122 |
| gr0031a | 750 | 0.6946667 | -31.60491 | 40.36066 | -0.3425592 |  0.4374610 | 45.58681 | 0.5391540 | -0.2813734 |  0.3593133 |
| gr0035a | 555 | 0.5729730 | -46.49127 | 49.77308 | -0.4552349 |  0.4873697 | 53.63588 | 0.4365834 | -1.2248208 | -0.1010512 |
| gr0037a | 743 | 0.3028264 | -74.55897 | 74.55897 | -0.5555338 |  0.5555338 | 76.20295 | 0.5072082 | -7.2015476 | -0.7561436 |
| gr0039a | 135 | 0.6962963 | -50.24046 | 50.24046 | -0.4244138 |  0.4244138 | 53.54552 | 0.5783566 | -2.3769395 | -0.4077478 |
| gr0045a |   5 | 0.0000000 |  21.02946 | 21.02946 | 21.0294640 | 21.0294640 | 21.04613 |        NA |       -Inf | -1.0000000 |
| gr0047a | 701 | 0.2696148 | -80.64033 | 80.64033 | -0.5807733 |  0.5807733 | 82.50943 | 0.5046382 | -4.4925334 | -0.6358693 |
| gr0110r | 750 | 0.8800000 | -20.45601 | 27.42076 | -0.2669425 |  0.3578296 | 31.70830 | 0.5294739 | -0.1146254 |  0.4426873 |
| gr0120a | 748 | 0.6577540 | -42.52090 | 42.71126 | -0.4123081 |  0.4141540 | 45.55083 | 0.6372278 | -2.1708570 | -0.3692557 |

model statistics split by site
