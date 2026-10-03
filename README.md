---
---
---

# Where Are the Stops?

Public transport stop availability across Greater Melbourne LGAs.

CSE5DEV Assessment 2 - Lesandu Hetti Arachchige (21533031)

Live app: <https://lesa-hettiarachchi.shinyapps.io/where-are-the-stops/>

## What it does

A public-facing Shiny dashboard comparing public transport stop availability across the 35 Greater Melbourne Local Government Areas, built around the three task abstractions planned in Task 1A:

|   | Task abstraction | Indicator type | Visual idiom |
|------------------|------------------|------------------|------------------|
| TA1 | Summarise stop count by transport mode per LGA | Raw (distinct stop counts) | Small-multiple horizontal bar charts |
| TA2 | Compare stop density per LGA | Processed (stops per km2) | Leaflet choropleth |
| TA3 | Rank LGAs on a 0-100 under-served index | Normalised (residents per stop, min-max scaled) | Scatter plot plus ranking table |

The app has six tabs as required by the assessment specification: public presentation, purpose and design, raw data, preprocessing issues, processed tables, and student credit.

## Folder structure

```         
Shiny App/
├── app.R                       the Shiny app (Task 2B)
├── raw/                        data as supplied, plus the ABS population file
├── clean/                      output of Task 1B, input to Task 2A
├── processed/                  output of Task 2A, read directly by app.R
├── Task1B_Preprocessing.Rmd    original preprocessing (issues 1-11)
├── Task1B_Corrections.Rmd      additional issues found on review (12-15)
└── Task2A_Processing.Rmd       builds the three analysis-ready tables
```

## Data flow

```         
raw/  --Task1B_Preprocessing.Rmd-->  clean/  --Task2A_Processing.Rmd-->  processed/  -->  app.R
            Task1B_Corrections.Rmd
```

Each stage reads only from the stage before it. `app.R` reads nothing but `processed/`, so the live app needs just those five files.

Rebuild everything from scratch by knitting the three R Markdown files in the order shown above, with `Shiny App/` as the working directory.

## Running locally

Requires R with: shiny, bslib, DT, ggplot2, dplyr, tidyr, readr, stringr, sf, leaflet, scales.

``` r
setwd("path/to/Shiny App")
shiny::runApp("app.R")
```

`sf` needs the system libraries GEOS, GDAL and PROJ. On macOS: `brew install gdal geos proj`.

## Deploying

``` r
setwd("path/to/Shiny App")
rsconnect::deployApp(appFiles = c("app.R", "processed"), appName = "where-are-the-stops")
```

The `appFiles` argument matters. Without it rsconnect bundles the whole folder, including `raw/`, `clean/` and the knitted HTML - about 41 MB instead of 256 KB. Only `app.R` and `processed/` are needed online; everything else is here as evidence for marking.

The first deployment takes 5 to 15 minutes because `sf` is compiled on the server. Later deployments are much faster and reuse the same URL.

If the app starts but shows a blank map, `processed/lga_boundaries.geojson` was not included in the bundle. Check the server log with:

``` r
rsconnect::showLogs(appName = "where-are-the-stops")
```

## Data sources

- Public transport stop, route and frequency data, LGA boundaries, sector and context files: provided by the CSE5DEV teaching team, derived from Public Transport Victoria / DataVic open data.
- Population: Australian Bureau of Statistics, Regional population by age and sex, 2024, Table 3 - Population estimates by age and sex, by LGA.
- LGA 2021 to LGA 2024 correspondence: Australian Bureau of Statistics, Australian Statistical Geography Standard.
- Base map tiles: OpenStreetMap contributors, via the leaflet R package.

## Limitations

Stop counts measure how many stops exist, not how often services run, how far people walk to reach them, or how crowded they are. An LGA is a large area and one value cannot describe every suburb inside it. The 0-100 index is relative to these 35 LGAs only, so a score of 0 means best of this group, not perfect. LGAs with no stops of a mode are reported separately as "No service" rather than scored.
