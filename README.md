# Walla Walla County Notifiable Conditions Analysis

### Description

Walla Walla County has experienced the highest enteric disease burden by county
in Washington State for several consecutive years. Enteric diseases (or 
gastrointestinal illnesses) are typically notifiable conditions due to their 
relativelyhigh virulence. Although surveillance is in place for these conditions, 
the County has historically lacked the resources to dive deeply into this data.

The COVID-19 pandemic motivated an expansion for the Walla Walla County Department 
of Community Health (WWCDCH) epidemiology staff, which has since allowed for 
ongoing monitoring and trend analysis for notifiable conditions. These notifiable 
conditions include but are not limited to enteric diseases. Since 2022, WWCDCH 
staff have produced annual reports on all notifiable conditions which are used
to guide health promotion campaigns in the County.

The current project was motivated by a desire to compile a historical database 
of existing data from 2013 to 2023, and retrospectively extend these analyses. In 
doing so, we hope to better understand the direction of trends pre- and 
post-pandemic, and inform future public health interventions.

This R project includes code necessary to create a historical database of 
notifiable conditions for Walla Walla County (2013 - 2023), perform trend analyses, 
and generate some basic data visualizations.

> [!IMPORTANT]  
>
> **Data Notice**: This repository does not include source data due to privacy 
restrictions. Scripts and visualizations are available to reproduce results only 
with appropriate data access. If you are interested in obtaining the data used 
to perform these analyses, please reach out to the Walla Walla County Department 
of Community Health.
>
> All visualizations have been edited to ensure no small cell sizes (n < 10) 
are displayed. This is to protect individuals from being re-identified in areas 
with a small population.

### How to run these analyses

1. Clone this repo

2. Open `wwcdch.Rproj`. This ensures `here` and `renv` packages work properly  
   
3. Run the following code in RStudio console once:  
   
   ```r
   renv::restore()
   source("scripts/00_run_all.R")
   ```  
   This ensures all package versions are the same as my own, and runs the 
   'run all' script which will follow the pipeline from data ingestion, cleaning,
   and transformation through exploratory and inferential analysis, then generate
   vizualizations and reports.
   
   Optionally, you may choose to run a specific script by substituting the script
   name in the `source()` function.
   
### Methodology

Data for this project was sourced from the following:  

- Annual Communicable Disease Reports (WDOH)  
- Washington Disease Reporting System
- STI Data from WDOH
- Washington Tracking Network

After cleaning and compiling all data, exploratory analyses were conducted to
begin identifying outliers in terms of overrepresented diseases, sociodemographic
disparities, or spatial effects.  

### Acknowledgements

Much thanks to the epidemiology staff at Walla Walla County Dept. of Community 
Health for supplying the data and oversight for this project. Thank you to the
UW Student Epidemiology Action Leaders program for making this work possible.  

<div align = "center">
  <a href = "https://dch.wwcowa.gov/">
    <img src = "images/walla_walla_co_logo.png"
    style = "height:100px; display:inline-block"
    alt = "Walla Walla County Logo">
  </a> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <a href = "https://epi.washington.edu/">
    <img src = "images/seal_logo.png"
    style = "height:100px; display:inline-block"
    alt = "SEAL Logo">
  </a>  
</div>






