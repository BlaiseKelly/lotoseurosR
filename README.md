Tools for the processing of LOTOS-EUROS outputs
================

## Introduction

lotoseurosR is a collection of functions to aid the processing of inputs
and outputs for the LOTOS-EUROS (LE) chemistry transport model
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
when the RAM is cleared on restart they will be empty).

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
mod_rast <- import_surf_concs(species = c('o3','no2'), path = 'files/output', output_units = 'volume')
```

    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120701.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120701.nc no2"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120702.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120702.nc no2"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120703.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120703.nc no2"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120704.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120704.nc no2"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120705.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120705.nc no2"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120706.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120706.nc no2"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120707.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120707.nc no2"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120708.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120708.nc no2"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120709.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120709.nc no2"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120710.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120710.nc no2"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120711.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120711.nc no2"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120712.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120712.nc no2"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120713.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120713.nc no2"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120714.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120714.nc no2"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120715.nc o3"
    [1] "importing  files/output/LE_v2.2.001_conc-sfc_20120715.nc no2"

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

    FALSE [1] "all o3 sites imported between 2012-07-01 & 2012-07-15 23:00:00"

![](README_files/figure-gfm/unnamed-chunk-6-1.png)

    FALSE [1] "o3"

|       | date                | site    |       mod |      obs | species |
|:------|:--------------------|:--------|----------:|---------:|:--------|
| o3.1  | 2012-07-01 00:00:00 | gr0027a |  91.09168 | 102.7708 | o3      |
| o3.2  | 2012-07-01 01:00:00 | gr0027a |  82.12260 |  98.6875 | o3      |
| o3.3  | 2012-07-01 02:00:00 | gr0027a |  78.37096 |  99.4375 | o3      |
| o3.4  | 2012-07-01 03:00:00 | gr0027a |  75.67710 |  99.0000 | o3      |
| o3.5  | 2012-07-01 04:00:00 | gr0027a |  68.84624 |  98.3125 | o3      |
| o3.6  | 2012-07-01 05:00:00 | gr0027a |  53.92454 |  98.0000 | o3      |
| o3.7  | 2012-07-01 06:00:00 | gr0027a |  93.52081 |  98.1875 | o3      |
| o3.8  | 2012-07-01 07:00:00 | gr0027a |  92.28627 |  99.0625 | o3      |
| o3.9  | 2012-07-01 08:00:00 | gr0027a |  98.43075 |  99.7500 | o3      |
| o3.10 | 2012-07-01 09:00:00 | gr0027a | 102.63137 | 101.0625 | o3      |
| o3.11 | 2012-07-01 10:00:00 | gr0027a | 108.58906 | 103.0000 | o3      |
| o3.12 | 2012-07-01 11:00:00 | gr0027a | 112.08488 | 104.6250 | o3      |
| o3.13 | 2012-07-01 12:00:00 | gr0027a | 115.43834 | 106.4375 | o3      |
| o3.14 | 2012-07-01 13:00:00 | gr0027a | 115.98372 | 107.2500 | o3      |
| o3.15 | 2012-07-01 14:00:00 | gr0027a | 114.27141 | 108.5625 | o3      |

snapshot of modelled and observed concentrations

Model statistics for all observation locations

``` r
kable(ms_all, caption = "model statistics for all ")
```

| default  |    n |      FAC2 |       MB |      MGE |      NMB |      NMGE |     RMSE |        r |        COE |       IOA |
|:---------|-----:|----------:|---------:|---------:|---------:|----------:|---------:|---------:|-----------:|----------:|
| all data | 2841 | 0.9296023 | 12.01345 | 28.48139 | 0.113871 | 0.2699644 | 35.90961 | 0.471697 | -0.1976541 | 0.4011729 |

model statistics for all

``` r
#time_dygraph(concs_dat, variable = c(all_labs[c(3,7,8,9,10)], 'Total'), ylab = spec_df$dg_nam[1])

#p1 <- plot_obs(data_brick = )
```

Model statistics split by site

``` r
kable(ms_sites, caption = "model statistics split by site")
```

| site    |   n |      FAC2 |         MB |      MGE |        NMB |       NMGE |     RMSE |         r |        COE |        IOA |
|:--------|----:|----------:|-----------:|---------:|-----------:|-----------:|---------:|----------:|-----------:|-----------:|
| gr0027a | 360 | 0.9555556 |  22.122604 | 27.87151 |  0.2294669 |  0.2890975 | 34.80123 | 0.6163953 | -0.6000175 |  0.1999912 |
| gr0028a | 360 | 0.9472222 |  18.380993 | 25.48185 |  0.1835341 |  0.2544361 | 33.17873 | 0.6085717 | -0.2560395 |  0.3719803 |
| gr0031a | 360 | 0.8500000 |  28.574978 | 32.43452 |  0.3096757 |  0.3515027 | 42.72669 | 0.6389239 | -0.1575482 |  0.4212259 |
| gr0035a | 270 | 0.9444444 |   8.486839 | 23.46411 |  0.0829304 |  0.2292831 | 28.73631 | 0.4935744 | -0.1619250 |  0.4190375 |
| gr0037a | 360 | 0.9722222 | -15.755505 | 26.25128 | -0.1173273 |  0.1954867 | 32.59708 | 0.5836262 | -2.1032714 | -0.3555188 |
| gr0039a |  66 | 1.0000000 |  17.164619 | 30.13688 |  0.1446930 |  0.2540455 | 35.68663 | 0.6676085 | -1.1818504 | -0.0833469 |
| gr0045a |   5 | 0.0000000 |  42.336650 | 42.33665 | 42.3366501 | 42.3366501 | 42.36869 |        NA |       -Inf | -1.0000000 |
| gr0047a | 340 | 0.9529412 | -22.541123 | 27.16373 | -0.1623329 |  0.1956231 | 34.75429 | 0.5948331 | -1.0685417 | -0.0331353 |
| gr0110r | 360 | 0.8222222 |  34.899002 | 36.25033 |  0.4548795 |  0.4724930 | 41.90507 | 0.6236108 | -0.7074619 |  0.1462691 |
| gr0120a | 360 | 0.9972222 |  17.772900 | 27.11035 |  0.1724253 |  0.2630134 | 34.66519 | 0.7524655 | -1.3114925 | -0.1347582 |

model statistics split by site
