
# Guide to use NRMP Historic Data Cleaning App

Below is a step by step guide on how to open and use the NRMP Data Cleaning App for MIS or historic DBF Uploader data. The App can be accessed by link, or by running the program through R. 

## Direct link to App

https://amyjdavis.shinyapps.io/nrmp_historic_data_checker/ 
 - The link above takes you directly to the app and works as a normal webpage. This is a reasonable option for access. However, there is a threshold on how many users can utilize the app through the link and may be slower. The faster and more stable way to access the app is by running it through R (described below). 

## Files included

The following files need to be saved to a folder on your computer that will serve as the working directory. From the main repository page for the "NRMP_Historic_Data_Checker" on the right hand side there should be a section called "Releases".  Click on 'Releases' and then select the most recent release. From here you can download a zip or tar.gz file of all of the files in this repository.  Specifically to run the app you will need: 

1. app.R - This is the R code you will need to open in R via RStudio to run the shiny app (used in Step 4) 
2. www - You will need to have a folder within your working direction named 'www'.  The following files need to be within the www folder.

   a)	DataCheckingErrorsCodesAll.xls
   
   b)	DataCheckingErrorsCodesAll.pdf
   
   c) MIS_DBF_colnames.csv
   
   d)	USDA_bw_transparent.png 
   
   e) scenarios.pdf
   
   f) NRMP Target Species and MIS Target.pdf


## Step by Step to Launch App From R

### Step 1: You need to have R and RStudio on your computer, if you down't yet then download R and RStudio (if both are already installed proceed to Step 2)

- Submit an IT ticket requesting to have R and R Studio (latest versions) installed. R can be installed without administrative privileges as long as it is saved under My Documents and not Programs.  

#### -OR-

- If you have administrative privileges on your computer, do either of the following:  

###### Windows Users - to install R
  
   1. Open an internet browser and go to www.r-project.org. 
   2. Click the "download R" link in the middle of the page under "Getting Started." 
   3. Select a CRAN location (a mirror site) and click the corresponding link. You should select at CRAN location that is closer to your location for speed of download.  
   4. Click on the "Download R for Windows" link at the top of the page. 
   5. Click on the "install R for the first time" link at the top of the page. 
   6. Click "Download R for Windows" and save the executable file somewhere on your computer.  Run the .exe file and follow the installation instructions.
   7. Now that R is installed, you need to download and install RStudio. 
   
##### Mac Users - to install R
    
   1. Open an internet browser and go to www.r-project.org.
   2. Click the "download R" link in the middle of the page under "Getting Started."
   3. Select a CRAN location (a mirror site) and click the corresponding link. You should select at CRAN location that is closer to your location for speed of download.  
   4. Click on the "Download R for (Mac) OS X" link at the top of the page.
   5. Click on the file containing the latest version of R under "Files."
   6. Save the .pkg file, double-click it to open, and follow the installation instructions.
   7. Now that R is installed, you need to download and install RStudio.
    
##### To install RStudio
   1. Use the link below to install RStudio Desktop  
   https://www.rstudio.com/products/rstudio/download/
   
### Step 2: Open RStudio and create a new project 
1. File --> New Project 
2. Existing Directory 

   - choose the working directory you saved the files to previously 

   - You do not need to open a new session (leave box at bottom left unchecked)

3. Create Project

### Step 3: Install neccessary packages

Copy and paste the following text into your R console, then click 'enter'.  You may get several warnings about the instillation process, you can ignore these.  If you are prompted to 'Unpack data from source?' select "YES". 
    
    install.packages("Rtools")   
    install.packages("shiny")   
    install.packages("shinythemes")    
    install.packages("DT")  
    install.packages("leaflet")  
    install.packages("leaflegend")  
    install.packages("readxl")    
    install.packages("raster")  
    install.packages("tidyverse")  
    install.packages("tigris")  
    install.packages("shinycssloaders")  
    install.packages("htmltools")  
    install.packages("rgdal")  
    install.packages("reshape2")  
    install.packages("viridis")  
    install.packages("data.table")  
    install.packages("shinyjs")  
    install.packages("shinycssloaders")  
    install.packages("viridis")  
    install.packages("viridisLite")  
    install.packages("shinydashboard")  
    install.packages("pwr")  
    install.packages("shinyWidgets")  
    install.packages("hablar")  

### Step 4: Run the app from R using the script: app.R

Open the file "app.R" in RStudio. You can do this be selecting File -> Open File -> then select "app.R" or within R studio on the bottom right of the screen you should see a tab named "Files", select that tab to see all of the files within your working directory, click on the name of the "app.R" file to open the script. Once app.R is open it should appear in the top left quadrant of the R studio screen. Press the "Run App" button on the top right of the script to run the app.  This will open the app within your web browser window.

#### NRMP Data Cleaning App should open in your web browser

![alt text](https://github.com/AmyJDavis/NRMP_Historic_Data_Checker/blob/main/MISDBF.PNG?raw=true)  
Image 1. Home screen of NRMP MIS Data Cleaning App.

### Step 5: Select and add dataset to shiny app

Select the type of data you will be uploading either MIS data or historic data from the DBF Uploader. Click the "Browse" button under the "Select Data File" on the left panel to select the Excel spreadsheet of MIS data you would like to run the data checker on. Click the "Run data checking" button to start the data checking, this may take a few moments depending on the file size you upload.  The current limit of the app is 30MB. 

The app provides several tabs to visually explore issues with the data.  It is not necessary to examine these.  You can proceed directly to Step 6 and download the data with error codes.  The codes for the errors are provided on the "Error Definitions" tab.  You can download the PDF of the errors codes on your computer to get descriptions on the errors.  If you are interested in visualizing your data and associated errors click through the following tabs. 

1. User Guide - This tab provides a user guide for this app and section on trouble shooting.
2. Summary of Errors – This is a tab that summarizes the errors in your dataset.  There is a bar chart that shows the error codes and the number of records with those errors.  Then there is a table that tells you the number of error and provides a description of the errors.  This is a good reference to see what common issues are showing up in your data.
3. Location check – This tab checks the latitude and longitude information against the state and county information. The top shows a dashboard indicating the number of records that have a state/location mismatch and the number that have a county/location mismatch.  There is also an interactive map that lets you visually see the records that have a county/location mismatch (shown as red points).  As you scroll your curser over the map it will tell you which county your curser is in.  If you click on a point, an info box will pop up that tells you the record ID number, the species of that record, and the county that record says it is in. Below the map is a table of just the records that have a state or county mismatch with the location. Warning: If you are uploading a very large file, you may not want to try and visualize the map as it will take a long time to load.
4. Method-Fate check – This tab shows a pivot-type table of all of the Method-Fate combinations in your data.  There are some Method-Fate combinations that are not allowed (see the Method/Fate Scenarios-PDF for details). By visualizing the data in this table you can see if you have any combinations that are not allowed. 
5. Sample Results Check – This tab shows the number of records that are missing sample result information.  If brain samples, blood samples, or teeth/jaw samples are collected then the results from those samples need to be filled in within a reasonable time period.  For RABIESBRAINRESULTS, a value needs to be entered within 30 days.  If it has been longer than that, an error will be indicated for that record.  For RABIESNVA_IUML, TTCC, and AGERECORDED results need to be provided within a year. 
6. Target Species Check – This tab shows a table of the species in your dataset and a count of the number that are recorded as target or non-target species.  See Target Species-PDF tab to see which species should be considered target species.
7. Error Definitions-PDF – This tab has a reference PDF that can be downloaded that explains the error codes and their definitions.  This is helpful when going through the downloaded data with error codes file.
8. Method/Fate Scenarios-PDF – This tab has a reference PDF that explains the Method-Fate combinations that are allowed in the data and why.  If you have an error with your Method-Fate combinations, this file will help you understand it.
9. Target Species-PDF – This tab has a reference PDF that explains which species should be considered target species. 


### Step 6: Download the datafile with errors 

Click the "Download data with errors" button to save the data file. This file will be downloaded as a .csv file and will have the same name as the file you uploaded but with "with_errors" at the end of the name.  The file will be saved in the working directory you are using for this app. The data will match the data you imported with a five additional columns.  The new columns are: State_on_record,	State_from_GPS,	County_on_record,	County_from_GPS, and	Errors.  The state and county information is for ease of comparison between the information from the GPS data (latitude/longitude) and the information in the MIS record.  The Error column will have a list of the errors associated with each line of code.  There may be no errors in which the value will be blank, or there may be one or many codes.  Codes are designated by the letter "F" then a number.  The codes can be looked up in the PDF that is found on the "Error Definitions" tab. Contact Kathy Nelson for any issues with error descriptions. 

