# SHARKS
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1977491.svg)](https://doi.org/10.5281/zenodo.1977491)

This repository holds files necessary to run the Stream Hydrology And Rainfall Knowledge System (SHARKS) web app. The SHARKS platform was developed to retrieve and visualize hydrologic and meteorologic data from various sources while addressing syntactic and semantic differences in data formats. SHARKS also provides a suite of exploratory data analysis features including the ability to calculate the total precipitation depth recorded for any period, interpolate the annual recurrence interval for precipitation events, perform hydrograph separations, and calculate the volume of runoff for any period.

SHARKS is also available at: https://bigbadcrad.shinyapps.io/SHARKS/

## Getting Started
These instructions will help you get a copy of SHARKS up and running on your local machine for testing and development.

### Required Software
To run SHARKS, you must have both R and RStudio installed on your local machine.
R:  https://www.r-project.org/

RStudio Software: https://www.rstudio.com/products/rstudio/download/

You must also install the Shiny Package in R.
```
install.packages('shiny')
```

### Download & Run SHARKS
#### Method 1:
The easiest way to run SHARKS is by retrieving the app directly in R from this GitHub repository:
```
library(shiny)
runGitHub('SHARKS','bigbadcrad')
```
#### Method 2:
Alternatively, you can clone the git repository by downloading the SHARKS files from GitHub as a ZIP folder and unzipping it to your desired directory. Then, you can run SHARKS by opening the app.R file in the unzipped folder using RStudio and clicking the "Run App" button.

## User Manual
Please read the [SHARKS Methods & User Manual](https://github.com/bigbadcrad/SHARKS/blob/master/www/SHARKS%20Methods%20%26%20User%20Manual.pdf) for details on methodology and usage of the SHARKS app.

## Author
* **Conrad Brendel** - cbrendel@vt.edu

## License
This project is licensed under the MIT License - see the [LICENSE.txt](LICENSE.txt) file for details.
