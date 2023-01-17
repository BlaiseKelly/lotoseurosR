README
================

## lotoseurosR

Tools to aid inputs and outputs for the LOTOS-EUROS (LE) chemical
transport model https://lotos-euros.tno.nl/open-source-version/

The model has been downloaded and run on the default settings, with the
outputs in the mod_files/outputs folder. Cloning this folder and running
the functions should work with the model files. The functions should
also work with any LE model ooutput files provided the output files are
given distinguishable names that can be given to the pattern input to
the function.

The input_concs and input_meteo will import the concentration outputs
and output as a dataframe that can be used with openair and saqgetr to
validate.

By converting to georeferenced raster layers and raster bricks
simplifies processing and plotting

``` r
1 + 1
```

    [1] 2

You can add options to executable code like this

    [1] 4

The `echo: false` option disables the printing of code (only output is
displayed).
