# WWCDCH Analysis
This R project includes code necessary to create a historical database of 
notifiable conditions for Walla Walla County (2013 - 2023), perform trend analyses, 
and generate some basic data visualizations. 

How to run:

**1. Clone repo (Git) / Download file and open in RStudio (non-Git)**  

**2. Open `wwcdch.Rproj`**  
   This ensures `here` and `renv` packages work properly  
   
**3. In the Console:**  
   Run once:  
   
   ```r
   renv::restore()
   source("scripts/01_clean.R")
   ```  
   This ensures all package versions are the same as my own, and runs the 
   data cleaning script so that you are ready for analysis.