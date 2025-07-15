# Discovery Farm Runoff Generation

This repo is for Discovery Farm Runoff Generation Project. The overall topic of this project is to explore the effects of practices/land cover use on P-Q relationships and nutrients.



## Overview

Write overview here

## Data sources

### USGS edge-of-field data
Raw [USGS data](https://www.sciencebase.gov/catalog/item/6696bef8d34ecb78f609f651) release containing DF site data is in `00\_Data/USGS\_raw`.
### Metadata
Metadata are in `00\_Data/Metadata`.
`DF EOF Site & Year Metadata (2004-2023)-Site` contains general information for each DF site
`DF EOF Site & Year Metadata (2004-2023)-Site' contains information for each DF site across the years
`DF_EOF_Crop` contains crop information for each DF site across the years.
Note: All DF data follow the USGS water year rather than a typical calendar year. For example, the field year 2017 includes all runoff events from 1 October 2016 to 30 September 2017. This becomes more relevant when looking at crop information because “previous crop” will have more influence on fall and spring runoff events than the crop grown that water year, which won’t be planted until ~75% of the way through the year.

## Exploratory analysis

Codes for Exploratory analysis are in `01\_Exploratory\_analysis`.

## Data processing
Data processing uses Ellen Albright's codes as reference: `Ellen USGS processing scripts/1-data cleaning.R`
