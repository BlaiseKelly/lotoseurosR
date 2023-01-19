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

| date                | site    |      mod |      obs |
|:--------------------|:--------|---------:|---------:|
| 2012-07-01 00:00:00 | gr0027a | 46.30494 | 102.7708 |
| 2012-07-01 01:00:00 | gr0027a | 41.74566 |  98.6875 |
| 2012-07-01 02:00:00 | gr0027a | 39.83857 |  99.4375 |
| 2012-07-01 03:00:00 | gr0027a | 38.46919 |  99.0000 |
| 2012-07-01 04:00:00 | gr0027a | 34.99684 |  98.3125 |
| 2012-07-01 05:00:00 | gr0027a | 27.41164 |  98.0000 |
| 2012-07-01 06:00:00 | gr0027a | 47.53975 |  98.1875 |
| 2012-07-01 07:00:00 | gr0027a | 46.91219 |  99.0625 |
| 2012-07-01 08:00:00 | gr0027a | 50.03563 |  99.7500 |
| 2012-07-01 09:00:00 | gr0027a | 52.17095 | 101.0625 |
| 2012-07-01 10:00:00 | gr0027a | 55.19944 | 103.0000 |
| 2012-07-01 11:00:00 | gr0027a | 56.97648 | 104.6250 |
| 2012-07-01 12:00:00 | gr0027a | 58.68116 | 106.4375 |
| 2012-07-01 13:00:00 | gr0027a | 58.95839 | 107.2500 |
| 2012-07-01 14:00:00 | gr0027a | 58.08797 | 108.5625 |

snapshot of modelled and observed concentrations

![](README_files/figure-gfm/unnamed-chunk-7-1.png)

![](README_files/figure-gfm/unnamed-chunk-7-2.png)

![](README_files/figure-gfm/unnamed-chunk-7-3.png)

Model statistics for all observation locations

``` r
kable(ms_all, caption = "model statistics for all ")
```

| default  |    n |     FAC2 |        MB |     MGE |        NMB |      NMGE |     RMSE |        r |       COE |        IOA |
|:---------|-----:|---------:|----------:|--------:|-----------:|----------:|---------:|---------:|----------:|-----------:|
| all data | 2841 | 0.655755 | -45.76426 | 47.5945 | -0.4337823 | 0.4511304 | 53.30668 | 0.471697 | -1.001368 | -0.0006835 |

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
| gr0027a | 360 | 0.8166667 | -36.15528 | 37.40461 | -0.3750210 |  0.3879797 | 40.68981 | 0.6163953 | -1.1472836 | -0.0685907 |
| gr0028a | 360 | 0.7611111 | -39.89689 | 41.51119 | -0.3983702 |  0.4144890 | 45.18740 | 0.6085717 | -1.0461502 | -0.0225546 |
| gr0031a | 360 | 0.7972222 | -30.84237 | 36.09256 | -0.3342482 |  0.3911461 | 40.52581 | 0.6389239 | -0.2880993 |  0.3559503 |
| gr0035a | 270 | 0.6518519 | -46.00148 | 47.67794 | -0.4495104 |  0.4658921 | 51.53089 | 0.4935744 | -1.3609752 | -0.1528924 |
| gr0037a | 360 | 0.3111111 | -74.03338 | 74.03338 | -0.5513080 |  0.5513080 | 75.38889 | 0.5836262 | -7.7517888 | -0.7714753 |
| gr0039a |  66 | 0.7272727 | -49.60003 | 49.60003 | -0.4181144 |  0.4181144 | 52.03854 | 0.6676085 | -2.5909444 | -0.4430435 |
| gr0045a |   5 | 0.0000000 |  21.02946 | 21.02946 | 21.0294640 | 21.0294640 | 21.04613 |        NA |       -Inf | -1.0000000 |
| gr0047a | 340 | 0.2764706 | -79.72997 | 79.72997 | -0.5741859 |  0.5741859 | 81.07151 | 0.5948331 | -5.0715077 | -0.6705925 |
| gr0110r | 360 | 0.9305556 | -19.98104 | 24.46932 | -0.2604362 |  0.3189374 | 28.08607 | 0.6236108 | -0.1525534 |  0.4237233 |
| gr0120a | 360 | 0.6750000 | -41.64445 | 41.64445 | -0.4040171 |  0.4040171 | 43.71639 | 0.7524655 | -2.5507040 | -0.4367314 |

model statistics split by site
