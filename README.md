# Walla Walla County Enteric Diseases Historical Analysis
This R project includes code necessary to create a historical database of 
notifiable conditions for Walla Walla County (2013 - 2023), perform trend analyses, 
and generate some basic data visualizations. 

**Data Notice**  
This repository does not include source data due to privacy restrictions. Scripts 
and visualizations are available to reproduce results with appropriate data access.
If you are interested in obtaining the data used to perform these analyses, please
reach out to the Walla Walla County Department of Community Health.

Please also note that all visualizations have been edited to ensure no small cell
sizes (n < 10) are displayed. This is to protect individuals from being re-identified
in areas with a small population.

**How to run:**  

1. Clone repo (Git) / Download file and open in RStudio (non-Git)

2. Open `wwcdch.Rproj`  
   This ensures `here` and `renv` packages work properly  
   
3. Run the following code in RStudio console once:  
   
   ```r
   renv::restore()
   source("scripts/01_clean.R")
   ```  
   This ensures all package versions are the same as my own, and runs the 
   data cleaning script so that you are ready for analysis.