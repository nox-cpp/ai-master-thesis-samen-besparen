# Data processing
Code in this folder is based on processing and manipulating the data using R. Some of the R scripts contain function definitions only and others can be run by themselves. Generally speaking, the environment variables are required to use the code in this folder. Some of the R scripts are wrapped using bash scripts.

## NOTE: Data
The source data for this project is not and will not be made available. Code here is made available for review only, although many of the functions may be suitable for other purposes.

## Using the bash scripts
The ```data_pipeline.sh``` script is the main script present in this folder. Run it with the ```--help``` option to see what options are available.

Generally speaking, running the script should be done from the project root folder and a valid ```.env``` file needs to be available.

## NOTE: Locale
Due to the limited date/time facilities in NetLogo, currently some of the scripts might be dependent on the system locale. US-int should always work. A workaround in the script is pending

## Using the data processing Rscripts
The top-level scripts in this directory that can be run directly are the following:
- data_collection.r: Collects and combines the data from the local sources and writes datasets to file
    - data augmentation can take a long time and a large amount of processing power. To skip this run the script with the additional ```no-augment``` flag.
        - e.g.: ```Rscript data_collection.r --args --no-augment```
        - or: ```R -f data_collection.r --args --no-augmentation```


- data_summary_helpers.r: Processes the combined data and displays and generates summary statistics

To use these top-level files, it is necessary to create and supply an environment file for the scripts (see template.env for an example) so that the data sources can be found and generated files are stored in the correct location.

## Data re-mapping
This package includes methods that can be used to (re-)map values and attributes in data frames.

### Column mappings
Column mappings are provided as key-value pairs (currently in .csv for convenient reading), where the key represents the replacement column name and the value the target which is to be replaced. The target is evaluated as a fixed string which must (partially) match a (sub)string of a column name. Prior to the mapping the names are converted to proper R-names by means of ```as.names(x)```. Therefore, when using these functions with any data.frame in R requires that the column names are already valid.

A column mapping file might look like this:
```
column_mappings.csv
datum_van_invullen, Start datum
tijd_van_invullen, Start tijd 
geslacht, Ben je man vrouw of anders
telefoon, Je telefoonnummer
telefoon, Telefoonnummer 
leeftijd, Wat is je leeftijd
```

The first target column that will be matched for after conversion is ```Start.datum```, which will be replaced with ```datum_van_invullen```.

### Value mappings
Value mappings are either key-value pairs, mapping old values to new values, or a triplet in which the key-values are augmented with a target column name. This is optional and dependent on whether the mapping is used on a dataframe or a single column of data.
Example:
```
value_mappings.csv
car motor-vehicle transportation
1 first_level
2 second_level proficiency
```
This maps the value of "car" to "motor-vehicle" in the transportation column, the value of "1" to "first_level" etc. Note that in order to do the latter transformation it may be necessary to change the column type from numerical to character or factor.

The matching is done via grep with fixed matching, meaning that a partial match will be replaced and no regular expressions are allowed. Replacements are processed one at a time in order of definition. Case-sensitive matching is used.

Caution: Attempting to replace with target "" will match all values and thus wipe the data frame.

### Column merging
It may be the case that multiple columns become merged together based on name. In order to be able to have a consistent dataset, we should be able to convert these merged rows which have multiple values combined into a single value based on some rules. For this some additional configuration is required. The main scenario in which multiple columns are merged into one with different value levels is for categorical data. In this situation, one way to ensure that these values are properly merged is to use the value mapping for those columns to replace different data values with one consistent factor. These mappings can be defined with the value mapping system and optionally a configuration to set all possible valid factors for a column.

Column factor mapping takes the form of a key-value/csv. Each row in the csv maps a single factor onto a data column. The order in which factors are introduced is implicitly set as the factor order (lower to higher). If no factors are explicitly defined for a given column, this is valid and then the existing values (unmerged) are used as factors. The column merge is applied as previously described.

After value replacement is performed and columns are merged, rows with multiple values in a column have that value replaced with the valid factor which is most common. In case of a tie the highest or first factor is selected. Configuration might be provided instead for a different tie-breaker value or selection.


## The following is a list of R packages used in this project
- BiocManager
- bnlearn
- corrplot
- data.table
- dplyr
- EnvStats
- ggcorrplot
- ggplot2
- ggpubr
- glmnet
- glue
- gRain
- gridExtra
- mipfp
- multiUS
- naniar
- purrr
- Rgraphviz
- rstanarm
- stringr
- tibble
- tidyr
- UpSetR
- VIM
- xtable
