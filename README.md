
# Guide to use NRMP Historic Data Cleaning App

Below is a step by step guide on how to open and use the NRMP Data Cleaning App for MIS or historic DBF Uploader data. The App can be accessed by link, or by running the program through R. 

## Direct link to App

 https://ajdavis.shinyapps.io/mis_datacheck_publish/  
 - The link above takes you directly to the app and works as a normal webpage. This is a reasonable option for access. However, there is a threshold on how many users can utilize the app through the link and may be slower. The faster and more stable way to access the app is by running it through R (described below). 

## Files included

The following files need to be saved to a folder on your computer that will serve as the working directory. 

1. app.R - This is the R code you will need to open in R via RStudio to run the shiny app (used in Step 4) 
2. www - You will need to have a folder within your working direction named 'www'.  The following files need to be within the www folder.

   a)	DataCheckingErrors.xls
   
   b) MIS_DBF_colnames.csv
   
   c)	DataCheckingErrors.pdf
   
   d)	USDA_bw_transparent.png 


## Step by Step to Launch App From R

### Step 1: Download R and RStudio (if both are already installed proceed to Step 2)

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

![alt text](https://github.com/AmyJDavis/NRMP_Historic_Data_Checker/blob/main/NRMP%20MIS%20DBF%20Data%20Cleaning%20App.png?raw=true)  
Image 1. Home screen of NRMP MIS Data Cleaning App.

### Step 5: Select and add dataset to shiny app

Select the type of data you will be uploading either MIS data or historic data from the DBF Uploader. Click the "Browse" button under the "Select Data File" on the left panel to select the Excel spreadsheet of MIS data you would like to run the data checker on. Click the "Run data checking" button to start the data checking, this may take a few moments depending on the file size you upload.  The current limit of the app is 30MB. 

The app provides several tabs to visually explore issues with the data.  It is not necessary to examine these.  You can proceed directly to Step 6 and download the data with error codes.  The codes for the errors are provided on the "Error Definitions" tab.  You can download the PDF of the errors codes on your computer to get descriptions on the errors.  If you are interested in visualizing your data and associated errors click through the following tabs. 

1. Location Check tab: This tab provides a map of MIS samples, the points are color coded and are red if the county in the MIS record does not match the county where the latitude/longitude data show the location. The map in interactive so you can soom in and out as needed.  Your curser will show the county it is hovering over.  If you click on any point it will show the animal ID number, the species, and the county on the MIS record for that individual.  There is also a table showing the samples with county location errors. Lastly, there is a summary box showing the number of location issues.

2. Rabies Check tab: This tab shows the number of samples that are awaiting results (either rabies results, titer results, or results from other samples) shown in yellow compared to the number of samples with test results provided, shown in purple. The summary boxes along the bottom show the number of results that are needed.  The errors for needing these samples to be filled in will only show after either 30 days (for rabies or rabies variant) or 1 year (for age or RVNA) has elapsed and no results are provided. 

3. Fate Check tab: This tab shows the distribution of different fate types in the data in a pie chart.  Also provided is a table of fate-method combinations.  This information is helpful as certain fate-method combinations should not occur and this can help point those out.

4. Serology tab: This tab will only have information if there is serology information collected in the data set.  The box shows the number of samples with serology information. The figure shows the distribution of RVNA results (RVNA values) based on if they were classified as positive or negative. 

5. Summary of Errors tab: This tab shows the summary of errors in the data both in a plot and table form.  It gives the number of each type of errors that is present in the data set.   This can be helpful to identify where common problems are in data reporting for your data set. 

6. Error Definitions tab: This tab has a PDF table of error codes and their corresponding definitions.  This should be downloaded to your computer so help with data cleaning. 

### Step 6: Download the datafile with errors 

Click the "Download data with errors" button to save the data file. This file will be downloaded as a .csv file and will have the same name as the file you uploaded but with "with_errors" at the end of the name.  The file will be saved in the working directory you are using for this app. The data will match the data you imported with a five additional columns.  The new columns are: State_on_record,	State_from_GPS,	County_on_record,	County_from_GPS, and	Errors.  The state and county information is for ease of comparison between the information from the GPS data (latitude/longitude) and the information in the MIS record.  The Error column will have a list of the errors associated with each line of code.  There may be no errors in which the value will be blank, or there may be one or many codes.  Codes are designated by the letter "F" then a number.  The codes can be looked up in the PDF that is found on the "Error Definitions" tab. Contact Kathy Nelson for any issues with error descriptions. 

