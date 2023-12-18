########################################################################
########################################################################
###
### Shiny app for NRMP data checking, 
### This app is targeted for examination of MIS data and the DBF uploader, 
###
### Amy J Davis
### February 16, 2021, Updated April 25, 2023
###
########################################################################
########################################################################


###  Details of this app
###  This app will not look at time of last ORV baiting since historical shapefiles are poor
###  This app will look at some logical issues that are not an issue in the current MIS data entry options
###  

### Libraries
t_col <- function(color, percent = 30, name = NULL) {
  #	  color = color name; percent = % transparency; name = an optional name for the color
  rgb.val <- col2rgb(color) ## Get RGB values for named color
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],max = 255,alpha = (100-percent)*255/100,names = name)
  invisible(t.col) ## Save the color
}
library(shiny) 
library(shinythemes)
library(DT)
library(leaflet)
library(leaflegend)
library(readxl)
library(tidyverse)
library(tigris)
library(shinycssloaders)
library(htmltools)
library(reshape2)
library(viridis)
library(data.table)
library(shinyjs)
library(shinycssloaders)
library(viridis)
library(viridisLite)
library(shinydashboard)
library(pwr)
library(shinyWidgets)
library(hablar)
library(sf)


get_popup_content <- function(dfsp) {
  paste0(
    "<b>ID: ", dfsp$IDNUMBER, "</b>",
    "<br>",
    "<br>Species: ", dfsp$SPECIES,
    "<br>County in record: ", dfsp$COUNTY  )
}

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)
options(shiny.maxRequestSize = 30*1024^2)
### 

### Read in Error code file
Fix_Comments <- openxlsx::read.xlsx("www/DataCheckingErrorCodesAll.xlsx")
misdbf=read.csv("www/MIS_DBF_colnames.csv")



### County information
stfp <- 1:56
stfp <- stfp[-c(2,3,7,14,15,43,52)]
uscd=tigris::counties(state=stfp,cb = TRUE)


# Define UI f
ui <- dashboardPage(
  
  skin='blue',
  title="NRMP Data Checking App",
  header = dashboardHeader(titleWidth='100%',
                           # Set height of dashboardHeader
                           tags$li(class = "dropdown",
                                   tags$style(".main-header {max-height: 100px}"),
                                   tags$style(".main-header .logo {height: 100px;}"),
                                   tags$style(".sidebar-toggle {height: 100px; padding-top: 1px !important;}"),
                                   tags$style(".navbar {min-height:100px !important}")
                           ),
                           title=span(tags$img(src='USDA_bw_transparent.png', style='margin-top:8px;',height=90,width=120,align="left"),
                                      column(10, class="title-box", 
                                             tags$h1(class="primary-title", style='margin-top:25px;font-size=50px',
                                                     "NRMP MIS and DBF Uploader Data Checking App")))),
  
  # Sidebar  
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
    width=350,
    title=tags$h2(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"Data selection",align='left'),
    useShinyjs(),
    # This makes web page load the JS file in the HTML head.
    # The call to singleton ensures it's only included once
    # in a page. It's not strictly necessary in this case, but
    # it's good practice.
    singleton(tags$head(tags$script(src = "message-handler.js"))),
    tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"Use this file uploader to select the Excel file of the data you would like checked for errors. ",align='left'),
    tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"Select the data type then upload the file you want to check.  Then click the button to run the data checker",align='left'),
    ## User inputs or from our study
    radioButtons(inputId = "datatype",label="Is your data from MIS (most common) or the historical database (DBF uploader)?",
                 choices=c("MIS","Historical"),selected = "MIS",inline = TRUE),
    fileInput(inputId = "ersdata",label = "Select Data File",accept = c('xls','xlsx')),
    # Go! button
    fluidRow(column(2),column(1,actionButton("go","Run data checker",icon("running"),
                                             style="color: #fff; background-color: #c00000; border-color: #Adf.combF2F; font-size:140%"))),
    
    tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"For a .csv file: ",align='left'),
    downloadButton(outputId = "download",label =  "Download data with errors as .csv",style="color:black;font-size:18px"),
    tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"For a .xlsx file: ",align='left'),
    downloadButton(outputId = "downloadxlsx",label =  "Download data with errors as .xlsx",style="color:black;font-size:18px")
    
  ),
  
  # Show output
  dashboardBody(
    tabsetPanel(
      tabPanel("User Guide",icon=icon("info"),
               box(width=12,title=span("How to use this data checking app",style="color:green;font-size:28px"),status="success",
                   column(8,p("Welcome to the NRMP MIS data checking app. This app was developed to help check for errors in data entry from MIS or from historical DBF uploader. Historically, this data checking was done by hand by NRMP staff. By automating this task, now rabies field staff (as well as NRMP) can check for errors in their own data.",style="font-size:130%;"),
                          p("To start this app use the file uploader on the left panel to browse (and select) the file you would like to check for errors. Select if this file is in the MIS format or the DBF format. The file needs to be in an Excel format (.xls or .xlsx) and must include the 94 columns from an MIS output (data dump). The column names must also match the MIS data dump or the DBF uploader. Please note, this file uploader can only handle file sizes of 30MB or less. Larger files will take longer to check than smaller files. Once you have selected your file and chosen if the file is MIS or DBF, you can click on the “Run data checker” button to check the data. When the data has been checked, you can click one of the “Download data with errors” buttons to save a file to your computer either as a .csv file or a .xlsx file. The farthest right column will be the error codes. A pdf of the error codes and their descriptions can be found on the “Error Definitions-PDF” tab. You should download this pdf as a reference for understanding the errors. If you have any questions about the definition of an error code, please contact Kathy Nelson (Kathleen.M.Nelson@usda.gov).",style="font-size:130%;"), 
                          p("You can be done with this app by simply uploading your data and then downloading the data with errors. However, we have provided additional tabs in this app to help you visualize and understand some of the errors in your data. Below are descriptions of the tabs and how to use them.",style="font-size:130%;")),
                   column(4,withSpinner(plotOutput(outputId = "datadone"))),
                   column(11,        
                          p("     •	",strong("Summary of Errors")," – This is a tab that summarizes the errors in your dataset. There is a bar chart that shows the error codes and the number of records with those errors. Then there is a table that tells you the number of errors and provides a description of the errors. This is a good reference to see what common issues are showing up in your data.",style="font-size:130%;"),
                          p("     •	",strong("Location Check")," – This tab checks the LATITUDE and LONGITUDE on your MIS record against the STATE and COUNTY information. The top shows a dashboard indicating the number of records that have a state/location mismatch and the number that have a county/location mismatch. There is also an interactive map that lets you see the records with a county/location mismatch (shown as red points). As you scroll over the map it will tell you which county your cursor is in. If you click on a point, an info box will pop up that tells you the IDNUMBER, SPECIES, and COUNTY shown on that MIS record. Below the map is a table of just the records that have a state or county mismatch with the location. Warning: If you are uploading a very large file, you may not want to try and visualize the map as it will take a long time to load.",style="font-size:130%;"),
                          p("     •	",strong("Method-Fate Check")," – This tab shows a pivot-type table of all of the METHOD-FATE combination errors. There are some combinations that are not allowed (see the “Method/Fate Scenarios-PDF” tab for details). By visualizing the data in this table, you can see the bad combinations (errors) in your data. ",style="font-size:130%;"),
                          p("     •	",strong("Sample Results Check")," – This tab shows the number of records that are missing sample result information. If brain, blood, or teeth/jaw samples are collected then the results from those samples need to be filled in within a reasonable time period. For RABIESBRAINRESULTS, a value needs to be entered within 30 days. If it has been longer than that, an error will be indicated for that record. For RABIESNVA_IUML, TTCC, and AGERECORDED results need to be provided within a year.  We understand that sometimes CDC, Wadsworth and Matson’s take longer than 365 days to return our results, but this error acts as a reminder for you as well.",style="font-size:130%;"),
                          p("     •	",strong("Target Species Check")," – This tab shows a table of errors between the SPECIES and TARGETSPECIES (Yes or No).  The count shown in the table is the number of records with that error. See the “Target Species-PDF” tab to see which species should be considered target species.",style="font-size:130%;"),
                          p("     •	",strong("Error Definitions-PDF ")," – This tab has a pdf that explains the error codes and their definitions.  You should download this pdf as a reference. This is helpful when going through the downloaded csv file that contains your data with error codes (farthest right column).",style="font-size:130%;"),
                          p("     •	",strong("Method/Fate Scenarios-PDF"),"  – – This tab has a reference pdf that explains the Method-Fate combinations that are allowed in the data and why. If you have an error with your Method-Fate combinations, this file will help you understand it.",style="font-size:130%;"),
                          p("     •	",strong("Target Species-PDF")," – This tab has a reference pdf that explains which species should be considered target species and how they relate to the MISTARGET field.",style="font-size:130%;"),
                   )
               ),
               box(width=12,title=span("Trouble-shooting",style="color:green;font-size:28px"),status="success",
                   # 
                   column(11,p("",style="font-size:130%;"),
                          p("We have tried to make this app as user-friendly as possible. However, we know that issues may arise. Here are some tips to help solve problems you may encounter. ",style="font-size:130%;"), 
                          
                          p("     •	",strong("Is there something wrong with the file you uploaded?"),style="font-size:130%;"),
                          p("          	----	Ensure you have the correct file extension. As mentioned above, this app accepts .xls and .xlsx files only. There should be a warning if the file extension is incorrect. ",style="font-size:130%;"),
                          p("           ----	When you dump data from MIS it outputs the result with an .html extension that looks and feels like Excel (but it’s not Excel). If you see an error in this app when trying to upload that says 'Unable to open file', you should open the file in Excel and click “Save As” and select .xls or .xlsx. Then try uploading this new file to the data checking app. ",style="font-size:130%;"),
                          p("     •	",strong("Did the file upload but there are no data checking results? "),style="font-size:130%;"),
                          p("           ----	Ensure you have the correct column names in your uploaded file. This app works based on the column names that are dumped from MIS. If these names have been modified, some of the data checking will not work. Double check the column names if the file has uploaded fine, but no data checking is done.",style="font-size:130%;"),
                          p("           ----	You may need to check the date format for all of your records as some export from MIS with an incorrect date format. In Excel sort by DATE and if any are left adjusted (usually shown at the bottom after sorting), retype in those dates and resave the file and try loading in the app again. ",style="font-size:130%;"),
                          p("     •	",strong("Email Kathleen.M.Nelson@usda.gov for other issues. "),style="font-size:130%;")
                   )
               )
      ),
      tabPanel("Summary of Errors",icon = icon("bar-chart-o"),
               box(width=12,title=span("Summary of errors in the data",style="color:blue;font-size:28px"),status="success",
                   # varImp Plot
                   column(10,plotOutput('ErrorPlots'))
               ),
               box(width=12,title=span("Table of data errors by code",style="color:blue;font-size:28px"),status="success",
                   #confusion matrix, model accuracy metrics
                   column(10,withSpinner(dataTableOutput(outputId="tableerror")))
               )
      ),
      tabPanel("Location Check",icon = icon("map"),
               box(width=12,title=span("Location check disclaimer",style="color:green;font-size:28px"),status="success",
                   # 
                   column(11,p("This location check determines if the latitude and longitude information corresponds to the same state and county as are on the data record.  Based on the map projection, points that are along county boundaries might suggest there is a discrepancy when there is not one (particularly along river boundaries). Use these errors as a guide to double check the locations. If you believe the location and county to be correct, then keep the record as is. ",style="font-size:130%;"),
                   )
               ),
               fluidRow(
                 box(width=12,title=span("Summary of location issues",style="color:blue;font-size:28px"),status="success",
                     # 
                     # Dynamic valueBoxes
                     valueBoxOutput("stateerror"),
                     valueBoxOutput("countyerror")
                 )
               ),
               box(width=12,title=span("Map of MIS samples",style="color:blue;font-size:28px"),status="success",
                   withSpinner(leafletOutput(outputId = "mapx"))
                   
               ),
               box(width=12,title=span("Table of samples with county location errors",style="color:blue;font-size:28px"),status="success",
                   dataTableOutput('tablex')
               )
               
      ),  
      tabPanel("Method-Fate Check",icon = icon("bar-chart-o"),
               box(width=12,title=span("How to interpret this table",style="color:green;font-size:28px"),status="success",
                   # 
                   column(11,p("This table shows all Method-Fate combinations in your dataset that involved an error (N06-N13).  See the Method/Fate Scenarios-PDF for more details. If no table displays then you don't have any Method-Fate issues in your dataset.",style="font-size:130%;")
                   )
               ),
               box(width=12,title=span("METHOD-FATE combinations",style="color:blue;font-size:28px"),status="success",
                   #confusion matrix, model accuracy metrics
                   column(8,withSpinner(dataTableOutput(outputId="tablefate"))))
      ),  
      tabPanel("Sample Results Check",icon = icon("bar-chart-o"),
               box(width=12,title=span("Sample results summaries",style="color:green;font-size:28px"),status="success",
                   # 
                   column(11,p("This tab shows a summary of how many records that had brain, blood, or teeth samples collected are missing results after a reasonable period of time.",style="font-size:130%;"),
                   )
               ),
               fluidRow(
                 box(width=12,title=span("Summary of test result issues",style="color:blue;font-size:28px"),status="success",
                     valueBoxOutput("rabiesissue"),
                     valueBoxOutput("titerissue"),
                     valueBoxOutput("othersampissue"))
               )
      ),
      tabPanel("Target Species Check",icon = icon("bar-chart-o"),
               box(width=12,title=span("NRMP target species and MIS target",style="color:blue;font-size:28px"),status="success",
                   column(11,p("This table shows the species and if the animal was designated as a target species.  Only data with errors are shown (N20). See NRMP Target Species and MIS Target.pdf for more detail. If no table displays there are no Target Species issues in your dataset.",style="font-size:130%;")),
                   
                   column(8,withSpinner(dataTableOutput(outputId="targetsps"))))
      ),
      
      tabPanel("Error Definitions-PDF",icon = icon("fa-solid fa-file-pdf"),
               tags$iframe(style="height:1000px; width:100%; scrolling=yes; color:blue", 
                           src="DataCheckingErrorCodesAll.pdf")),
      tabPanel("Method/Fate Scenarios-PDF",icon = icon("fa-solid fa-file-pdf"),
               tags$iframe(style="height:1000px; width:100%; scrolling=yes", 
                           src="scenarios.pdf")),
      tabPanel("Target Species-PDF",icon = icon("fa-solid fa-file-pdf"),
               tags$iframe(style="height:1000px; width:100%; scrolling=yes", 
                           src="NRMP Target Species and MIS Target.pdf"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  
  data <- eventReactive(input$go,{
    req(input$ersdata)
    req(input$datatype)
    #### Read in Data ####
    # This is the file dumped from MIS
    # New option for importing the data
    #NRMP_Masters <- openxlsx::read.xlsx(input$ersdata$datapath,detectDates = TRUE)
    
    
    if(tools::file_ext(input$ersdata$datapath)=="csv"){
      if(input$datatype=="Historical"){
        NRMP_Masters=read.csv(input$ersdata$datapath)
        
        NRMP_Masters$Last=NA
        colind=match(names(NRMP_Masters),misdbf$DBF.Uploader)
        NRMP_Master=NRMP_Masters
        names(NRMP_Master)=ifelse(is.na(colind),names(NRMP_Masters),misdbf$MIS[colind])
      }else{
        NRMP_Masters=read.csv(input$ersdata$datapath)
        
        names(NRMP_Masters)[which(names(NRMP_Masters)=="LAT.LONRECORDED")]="LAT/LONRECORDED"
        names(NRMP_Masters)[which(names(NRMP_Masters)=="PROCESSED.30DAYSAGO")]="PROCESSED<30DAYSAGO"
        NRMP_Master=NRMP_Masters
      }
      
      
    }else{
      NRMP_Masters <- openxlsx::read.xlsx(input$ersdata$datapath,detectDates = TRUE)
      if(input$datatype=="Historical"){
        NRMP_Masters$Last=NA
        colind=match(names(NRMP_Masters),misdbf$DBF.Uploader)
        NRMP_Master=NRMP_Masters
        names(NRMP_Master)=ifelse(is.na(colind),names(NRMP_Masters),misdbf$MIS[colind])
      }else{
        NRMP_Master=NRMP_Masters
      }
    }
    
    
    ### Change the names of the columns if the data came from the historical data checker
    
    NRMP_Master$column=dim(NRMP_Masters)[2]-1
    NRMP_Master$AmyID=1:dim(NRMP_Master)[1]
    # NRMP_Master=NRMP_Master[!is.na(NRMP_Master$STATE),]
    
    # Fix Data Before Checks
    NRMP_Master$DATE <- as.character(NRMP_Master$DATE)
    NRMP_Master$DATELASTORV <- as.character(NRMP_Master$DATELASTORV)
    NRMP_Master$RECAPTURE <- as.character(NRMP_Master$RECAPTURE)
    # Make columns for checks 
    Fixdf=data.frame(matrix(0,length(NRMP_Master$DATE),dim(Fix_Comments)[1]))
    names(Fixdf)=Fix_Comments$ErrorCode
    NRMP_Master=cbind(NRMP_Master,Fixdf)
    
    
    ######################################
    ###
    ### Check County and State ####
    ###
    ######################################
    # setwd("~/R/Rabies_Datachecker_3")
    # Get US Counties for desired States from tigris
    
    fips <- fips_codes
    fips$FIPS=paste(fips$state_code,fips$county_code,sep="")
    NRMP_Master$MIS_State=toupper(fips[match(NRMP_Master$STATE,fips$state),"state_name"])
    NRMP_Master$lat=ifelse(is.na(NRMP_Master$LATITUDE),0,NRMP_Master$LATITUDE)
    NRMP_Master$lon=ifelse(is.na(NRMP_Master$LONGITUDE),0,NRMP_Master$LONGITUDE)
    
    ## Specific fix for DeKalb county Alabama and Dona Ana county New Mexico
    NRMP_Master$COUNTY[NRMP_Master$COUNTY=="DE KALB"]="DEKALB"
    NRMP_Master$COUNTY[NRMP_Master$COUNTY=="DONA ANA"]="DOÑA ANA"
    
    # To get county polygon data frame information
    pnts_sf <- st_as_sf(NRMP_Master, coords = c('lon', 'lat'), crs = st_crs(uscd))
    
    pnts <- pnts_sf %>% mutate(
      intersection = as.integer(st_intersects(geometry, uscd)),
      LATLON_State = toupper(if_else(is.na(intersection), '', uscd$STATE_NAME[intersection])),
      MIS_County=COUNTY,
      LATLON_County = gsub('[[:punct:] ]+',' ',toupper(if_else(is.na(intersection), '', uscd$NAME[intersection]))),
    ) 
    
    NRMP_Master=pnts %>% st_drop_geometry()
    
    NRMP_Master$N01=ifelse(NRMP_Master$MIS_State!=NRMP_Master$LATLON_State&NRMP_Master$`LAT/LONRECORDED`=="YES",1,0)
    NRMP_Master$N02=ifelse(str_remove(NRMP_Master$COUNTY," CITY")!=NRMP_Master$LATLON_County&NRMP_Master$`LAT/LONRECORDED`=="YES",1,0)
    
    table(NRMP_Master$N01)
    table(NRMP_Master$N02)
    
    
    
    ######################################
    ###
    ### AJD Check for Method/Fate Errors 
    ###
    ######################################
    NRMP_Master$N04=ifelse(grepl("CAGE TRAP|FIREARMS|HANDCAUGHT/GATHERED|LEG/FOOT HOLD TRAP|ROAD KILL|WS INCIDENTAL TAKE ",NRMP_Master$METHOD)& NRMP_Master$COLLECTOR!="WS",1,0)
    NRMP_Master$N05=ifelse(NRMP_Master$COLLECTOR!="WS"&(NRMP_Master$FATE=="DIED UNDER CARE"|NRMP_Master$FATE=="EUTHANIZED"|NRMP_Master$FATE=="FOUND DEAD"|NRMP_Master$FATE=="NO FATE"|NRMP_Master$FATE=="OTHER"|NRMP_Master$FATE=="RELEASED"|NRMP_Master$FATE=="SAMPLED (WS TAKE)"),1,0)
    NRMP_Master$N05a=ifelse(!is.na(NRMP_Master$OTHERCOLLECTOR)&(NRMP_Master$COLLECTOR!="OTHER"),1,0)
    NRMP_Master$F05b=ifelse(NRMP_Master$COLLECTOR=="OTHER"&!is.na(NRMP_Master$COLLECTOR)&is.na(NRMP_Master$OTHERCOLLECTOR),1,0)
    NRMP_Master$N06=ifelse(NRMP_Master$METHOD=="CAGE TRAP"& !grepl('DIED UNDER CARE|EUTHANIZED|FOUND DEAD|OTHER|RELEASED|NO FATE',NRMP_Master$FATE),1,0)
    NRMP_Master$N07=ifelse(NRMP_Master$METHOD=="HANDCAUGHT/GATHERED"&!grepl('DIED UNDER CARE|EUTHANIZED|FOUND DEAD|RELEASED|NO FATE',NRMP_Master$FATE),1,0)
    NRMP_Master$N08=ifelse(NRMP_Master$METHOD=="LEG/FOOT HOLD TRAP"& !grepl('DIED UNDER CARE|EUTHANIZED|FOUND DEAD|OTHER|RELEASED|NO FATE',NRMP_Master$FATE),1,0)
    NRMP_Master$N09=ifelse(NRMP_Master$METHOD=="FIREARMS (SHOT)"& NRMP_Master$FATE!="EUTHANIZED",1,0)
    NRMP_Master$N10=ifelse(NRMP_Master$METHOD=="NON-WS CARCASS COLLECTION"&NRMP_Master$FATE!="SAMPLED (NON-WS TAKE)",1,0)
    NRMP_Master$N11=ifelse(NRMP_Master$METHOD=="NON-WS EUTHANIZED"&NRMP_Master$FATE!="SAMPLED (NON-WS TAKE)",1,0)
    NRMP_Master$N12=ifelse(NRMP_Master$METHOD=="ROAD KILL"&NRMP_Master$FATE!="FOUND DEAD",1,0)
    NRMP_Master$N13=ifelse(NRMP_Master$METHOD=="WS INCIDENTAL TAKE"&NRMP_Master$FATE!="SAMPLED (WS TAKE)",1,0)
    NRMP_Master$N13a=ifelse(NRMP_Master$DENSITYSTUDY=="NO"&!is.na(NRMP_Master$DENSITYSTUDY)&!is.na(NRMP_Master$DENSITYID),1,0)
    
    ###
    ### ORV issues
    #
    NRMP_Master$N15=ifelse(NRMP_Master$ORVNAIVE=="YES"&!is.na(NRMP_Master$DATELASTORV),1,0)
    # Error if ACTIVITY and ORVNAIVE don't match
    NRMP_Master$N16=ifelse(NRMP_Master$ACTIVITY=="TRAPPING (ORV NAIVE)"&NRMP_Master$ORVNAIVE!="YES",1,0)
    NRMP_Master$N16=ifelse(NRMP_Master$ACTIVITY=="TRAPPING (ORV POST-BAIT)"&NRMP_Master$ORVNAIVE!="NO",1,NRMP_Master$N16)
    
    # Error for negative days since last orv
    NRMP_Master$N17=ifelse(NRMP_Master$DAYSPOSTBAIT<0,1,0)
    NRMP_Master$N17[is.na(NRMP_Master$N17)]=0
    
    # Error for ORVNAIVE and bait type match
    NRMP_Master$N18=ifelse(NRMP_Master$ORVNAIVE=="YES"&NRMP_Master$ORVBAITTYPE!="NONE (NAIVE)",1,0)
    NRMP_Master$N18=ifelse(NRMP_Master$ORVNAIVE=="NO"&(is.na(NRMP_Master$ORVBAITTYPE)|NRMP_Master$ORVBAITTYPE=="NONE (NAIVE)"),1,NRMP_Master$N18)
    NRMP_Master$N18=ifelse((!is.na(NRMP_Master$ORVBAITTYPE)|NRMP_Master$ORVBAITTYPE!="NONE (NAIVE)")&is.na(NRMP_Master$ORVNAIVE),1,NRMP_Master$N18)
    NRMP_Master$N18[is.na(NRMP_Master$N18)]=0
    
    # Error for ORVNAIVE is no DATELASTORV must have a value
    NRMP_Master$N19=ifelse(NRMP_Master$ORVNAIVE=="NO"&is.na(NRMP_Master$DATELASTORV),1,0)
    NRMP_Master$N19[is.na(NRMP_Master$N19)]=0
    
    
    ###
    ### Target species issues
    ###
    # Error make sure TARGETSPECIES is a target species for NRMP,
    # Only check it when ACTIVITY = COORDINATED TVR or TRAPPING (ORV NAIVE) or TRAPPING (ORV POST-BAIT).  
    # Then, if SPECIES = BOBCATS or COYOTES or FOXES, GRAY or FOXES, RED or RACCOONS or SKUNKS, HOG-NOSED or SKUNKS, HOODED or SKUNKS, SPOTTED or SKUNKS, STRIPED, then TARGETSPECIES should be YES.  
    # For all other SPECIES except NO SPECIES, TARGETSPECIES should be NO.  For SPECIES = NO SPECIES, TARGETSPECIES should be NO CAPTURE.
    targetsps=c("BOBCATS","COYOTES","FOXES, GRAY","FOXES, RED","RACCOONS","SKUNKS, STRIPED","SKUNKS, SPOTTED","SKUNKS, HOG-NOSED","SKUNKS, HOODED")
    NRMP_Master$N20=ifelse((NRMP_Master$ACTIVITY=="COORDINATED TVR"|NRMP_Master$ACTIVITY=="TRAPPING (ORV NAIVE)"|NRMP_Master$ACTIVITY=="TRAPPING (ORV POST-BAIT)")&NRMP_Master$SPECIES%in%targetsps&NRMP_Master$TARGETSPECIES!="YES",1,0)
    NRMP_Master$N20=ifelse(!NRMP_Master$SPECIES%in%targetsps&NRMP_Master$SPECIES!="NO SPECIES"&NRMP_Master$TARGETSPECIES!="NO",1,NRMP_Master$N20)
    
    # 
    NRMP_Master$N20a=ifelse(NRMP_Master$TARGETSPECIES=="YES"&NRMP_Master$MISTARGET!="INTENTIONAL",1,0)
    
    ###
    ### Recapture process issues
    ###
    # Error if MISTARGET is "INTENTIONAL and RECAPTURE is blank
    NRMP_Master$N21=ifelse((NRMP_Master$METHOD=="CAGE TRAP"|NRMP_Master$METHOD=="HANDCAUGHT/GATHERED"|NRMP_Master$METHOD=="LEG/FOOT HOLD TRAP")&NRMP_Master$MISTARGET=="INTENTIONAL"&!is.na(NRMP_Master$MISTARGET)&is.na(NRMP_Master$RECAPTURE),1,0)
    # Error if MISTARGET is "INTENTIONAL and there is no value if it was collected within 30 days
    NRMP_Master$N22=ifelse((NRMP_Master$METHOD=="CAGE TRAP"|NRMP_Master$METHOD=="HANDCAUGHT/GATHERED"|NRMP_Master$METHOD=="LEG/FOOT HOLD TRAP")&NRMP_Master$MISTARGET=="INTENTIONAL"&!is.na(NRMP_Master$MISTARGET)&is.na(NRMP_Master$`PROCESSED<30DAYSAGO`),1,0)
    
    
    ######################################
    ###
    ### Individual checks
    ###
    ######################################
    NRMP_Master$DATE2=as.POSIXct(NRMP_Master$DATE,"%Y-%m-%d")
    NRMP_Master$DaysSinceCapture=as.numeric(difftime(Sys.Date(),NRMP_Master$DATE2,units="days"))
    
    
    ### Need to manipulate the IDs to get left and right ear tags to talk to eachother
    # get all ids
    
    all.id=strsplit(NRMP_Master$IDNUMBER[which(!is.na(NRMP_Master$IDNUMBER))],split="/")
    
    # no. of ids together
    n.id=unlist(lapply(all.id,length))
    
    uniq.1=unlist(lapply(all.id,"[[",1))
    uniq.2=rep("",length(uniq.1))
    uniq.2[which(n.id>1)]=unlist(lapply(all.id[n.id>1],"[[",2))
    
    ###################################
    comb.id=sapply(1:length(all.id),function(x){
      if(n.id[x]>1){paste(all.id[[x]],collapse="/")  # if two ids, keep as is
      }else{
        if(uniq.1[x]%in%c(uniq.2,uniq.1[-x])){# if only one id make sure not in any others
          ind2=which(uniq.2==uniq.1[x]) # second position
          indL=which(uniq.1==uniq.1[x]) # first position
          indL=indL[-which(indL==x)]
          if(length(ind2)!=0){ids=unique(uniq.1[ind2])}
          if(length(indL)!=0){ids=unique(uniq.2[indL])} # if in others, use combined
          ifelse(length(unique(c(uniq.1[x],ids)))>1,paste(unique(c(uniq.1[x],ids)),collapse="/"),uniq.1[x])
        }else{
          uniq.1[x] # or single id
        }}}, # end function
      simplify=TRUE)
    
    NRMP_Master$IDNUMBER2=NA
    NRMP_Master$IDNUMBER2[which(!is.na(NRMP_Master$IDNUMBER))]= comb.id
    ####
    NRMP_Master=NRMP_Master[order(NRMP_Master$STATE,NRMP_Master$IDNUMBER2,NRMP_Master$DATE),]
    NRMP_Master$WasCaught=0
    NRMP_Master$diffdat=c(0,as.numeric(diff(NRMP_Master$DATE2,units="days"),units="days"))
    
    NRMP_Master$IDState=paste(NRMP_Master$IDNUMBER2,NRMP_Master$STATE,sep=".")
    
    # Indicator for first of ID series
    NRMP_Master$t=sapply(1:nrow(NRMP_Master)-1,function(x)identical(NRMP_Master$IDState[x],NRMP_Master$IDState[x+1]))
    NRMP_Master$diffdat=ifelse(NRMP_Master$t==FALSE,999,NRMP_Master$diffdat)
    
    # Error for the if the animal was caught within the last 30 days but PROCESSED<30DAYSAGE is "NO"
    NRMP_Master$N23=ifelse(NRMP_Master$diffdat<30&NRMP_Master$`PROCESSED<30DAYSAGO`=="NO"&!is.na(NRMP_Master$`PROCESSED<30DAYSAGO`),1,0)
    
    # Making sure animals don't change species among captures
    spcheck=tapply(NRMP_Master$SPECIES,NRMP_Master$IDState,function(x)length(unique(x))>1)
    NRMP_Master$N24=0
    NRMP_Master[which(NRMP_Master$IDState%in%names(spcheck[spcheck==TRUE])&!is.na(NRMP_Master$IDNUMBER2)),"N24"]=1
    NRMP_Master$N24=ifelse(is.na(NRMP_Master$IDNUMBER2),0,NRMP_Master$N24)
    
    # Error if an individual is called both a male and female at some point during its capture
    scheck=tapply(NRMP_Master$SEX,NRMP_Master$IDState,function(x)any(x%in%c("MALE"))&any(x%in%c("FEMALE")))
    NRMP_Master$N25=0
    NRMP_Master[which(NRMP_Master$IDState%in%names(scheck[scheck==TRUE])),"N25"]=1
    NRMP_Master$N25=ifelse(is.na(NRMP_Master$IDNUMBER2),0,NRMP_Master$N25)
    
    # Error if an individual as the same ID and captured on the same day
    NRMP_Master$N26=ifelse(NRMP_Master$diffdat==0&!is.na(NRMP_Master$IDNUMBER2),1,0)
    
    NRMP_Master=NRMP_Master[order(NRMP_Master$AmyID),]
    NRMP_Master$N26a=ifelse(NRMP_Master$LACTATION=="YES"&NRMP_Master$SEX!="FEMALE",1,0)
    NRMP_Master$N26a[is.na(NRMP_Master$N26a)]=0
    
    NRMP_Master=NRMP_Master[order(NRMP_Master$AmyID),]
    NRMP_Master$N26a=ifelse(NRMP_Master$LACTATION=="YES"&NRMP_Master$SEX!="FEMALE",1,0)
    NRMP_Master$N26a[is.na(NRMP_Master$N26a)]=0
    
    # Trying to check for animals that died and came back to life
    NRMP_Master=NRMP_Master[order(NRMP_Master$IDState,NRMP_Master$DATE2),]
    scheck=tapply(NRMP_Master$FATE,NRMP_Master$IDState,function(x)any(x[-length(x)]=="EUTHANIZED"|x[-length(x)]=="FOUND DEAD"|x[-length(x)]=="DIED UNDER CARE"))
    NRMP_Master$N25a=0
    NRMP_Master[which(NRMP_Master$IDState%in%names(scheck[scheck==TRUE])),"N25a"]=1
    NRMP_Master$N25a=ifelse(is.na(NRMP_Master$IDNUMBER2),0,NRMP_Master$N25a)
    
    ###
    ### More individual checks
    ###
    # Error if juveniles are lactating
    NRMP_Master$N27=ifelse(NRMP_Master$RELATIVEAGE=="JUVENILE (YOY)"&!is.na(NRMP_Master$RELATIVEAGE)&NRMP_Master$LACTATION=="YES"&!is.na(NRMP_Master$LACTATION),1,0)
    
    # Error if no weight is provided when WEIGHTRECORDED is "YES"
    NRMP_Master$N27a=ifelse(NRMP_Master$WEIGHTRECORDED=="YES"&!is.na(NRMP_Master$WEIGHTRECORDED)&is.na(NRMP_Master$WEIGHT),1,0)
    
    # Error in Raccoon WEIGHT
    NRMP_Master$N28=ifelse(NRMP_Master$SPECIES=="RACCOONS"&!is.na(NRMP_Master$WEIGHT)&(NRMP_Master$WEIGHT<0.5|NRMP_Master$WEIGHT>20),1,0)
    
    # Error if HANDVACCINATED is "YES" and not drugs are provided
    drugs=paste(NRMP_Master$DRUG1APPLIED,NRMP_Master$DRUG2APPLIED,NRMP_Master$DRUG3APPLIED,sep=";")
    NRMP_Master$N29=ifelse(NRMP_Master$HANDVACCINATED=="YES"&!is.na(NRMP_Master$HANDVACCINATED)&(!grepl("IMRAB3, RABIES VACCINE",drugs)),1,0)
    NRMP_Master$N29=ifelse(NRMP_Master$HANDVACCINATED=="NO"&!is.na(NRMP_Master$HANDVACCINATED)&grepl("IMRAB3, RABIES VACCINE",drugs),1,NRMP_Master$N29)
    
    ###
    ### Test result errors
    ###
    # Error if BLOODSAMPLE is "YES" and RABIESSERUM is blank
    NRMP_Master$N30=ifelse(NRMP_Master$BLOODSAMPLE=="YES"&!is.na(NRMP_Master$BLOODSAMPLE)&is.na(NRMP_Master$RABIESSERUM),1,0)
    
    # Error if RABIESVNA_IUML is "NOT RECORDED" after a year and a BLOODSAMPLE is "YES"
    NRMP_Master$N31=ifelse(NRMP_Master$BLOODSAMPLE=="YES"&!is.na(NRMP_Master$BLOODSAMPLE)&NRMP_Master$DaysSinceCapture>366,ifelse(is.na(NRMP_Master$RABIESVNA_IUML),1,0),0)
    
    # Error if age is not filled in after a year and a sample was collected
    NRMP_Master$N32=ifelse((NRMP_Master$PM1SAMPLE=="YES"|NRMP_Master$PM2SAMPLE=="YES"|NRMP_Master$K9SAMPLE=="YES"|NRMP_Master$JAWSAMPLE=="YES")&NRMP_Master$DaysSinceCapture>366&is.na(NRMP_Master$AGERECORDED),1,0)
    NRMP_Master$N32[is.na(NRMP_Master$N32)]=0
    
    # Error if RABIESBRAINTEST is "NOT RECORDED" after a year and a BRAINSTEMSAMPLE is "YES"
    NRMP_Master$N33=ifelse(NRMP_Master$BRAINSTEMSAMPLE=="YES"&!is.na(NRMP_Master$BRAINSTEMSAMPLE)&NRMP_Master$DaysSinceCapture>29,ifelse((NRMP_Master$RABIESBRAINTEST=="NOT RECORDED"),1,0),0)
    
    
    
    ###
    ### Other checks
    ###
    # Error if OTHERSAMPLE is "YES" and OTHERSAMPLEEXPLAIN is blank
    NRMP_Master$N33a=ifelse(NRMP_Master$OTHERSAMPLE=="YES"&is.na(NRMP_Master$OTHERSAMPLEEXPLAIN),1,
                            ifelse(!is.na(NRMP_Master$OTHERSAMPLEEXPLAIN)&is.na(NRMP_Master$OTHERSAMPLE),1,0))
    NRMP_Master$N33a[is.na(NRMP_Master$N33a)]=0
    NRMP_Master$N33b=ifelse(is.na(NRMP_Master$WHYEUTHANIZED)&NRMP_Master$FATE=="EUTHANIZED",1,0)
    NRMP_Master$N34=ifelse(NRMP_Master$FATE=="OTHER",1,0)
    NRMP_Master$N35=ifelse(NRMP_Master$FATE=="NO FATE"&NRMP_Master$SPECIES!="NO SPECIES",1,0)
    
    # Error if BRAINSTEMTEST is "YES" and FATE is "RELEASED"
    NRMP_Master$N35a=ifelse(NRMP_Master$BRAINSTEMSAMPLE=="YES"&!is.na(NRMP_Master$BRAINSTEMSAMPLE)&NRMP_Master$FATE=="RELEASED",1,0)
    
    # Error if RABIESBRAINTEST is "POSITIVE" and RABIESVARIANT is "AWAITING VARIANT TYPING"
    NRMP_Master$N36=ifelse(NRMP_Master$RABIESBRAINRESULTS=="POSITIVE"&NRMP_Master$DaysSinceCapture>29&NRMP_Master$RABIESVARIANT=="AWAITING VARIANT TYPING",1,0)
    NRMP_Master$N36[is.na(NRMP_Master$N36)]=0
    
    # Error if the VNA interpret does not match the IUML information
    NRMP_Master$VNAvals=as.numeric(gsub("<|>|=",replacement = "",NRMP_Master$RABIESVNA_IUML))
    NRMP_Master$N37=ifelse(grepl("<",NRMP_Master$RABIESVNA_IUML)&NRMP_Master$RABIESVNAINTERPRET!="NEGATIVE",1,0)
    
    
    
    ######################################
    ###
    ### Collecting results
    ###
    ######################################
    errordf=NRMP_Master[,which(names(NRMP_Master)%in%names(Fixdf))]
    errordf[is.na(errordf)]=1
    
    NRMP_Master[,which(names(NRMP_Master)%in%names(Fixdf))]=errordf
    
    #### Creating a columne for the errors
    hm=apply(errordf,1,function(x){which(x==1)})
    if(is.list(hm)){
      hm2=lapply(hm,names)
      NRMP_Master$Errors=do.call('rbind',lapply(hm2,paste,collapse=";"))
      
    } else{
      NRMP_Master$Errors=paste(rownames(hm),collapse=";")
      
    }
    
    ### Make lat/longs zeros NA values
    NRMP_Master$LATITUDE[NRMP_Master$LATITUDE==0]=NA
    NRMP_Master$LONGITUDE[NRMP_Master$LONGITUDE==0]=NA
    
    NRMP_Master
    
  })
  
  ####
  ### Data done spinner on info page
  ####
  output$datadone <- renderPlot({
    bob=dim(data())
    par(mar=c(1,1,1,1))
    my_image <- png::readPNG("www/Check.png")
    
    # Set up a plot area with no plot
    plot(0,0,xaxt="n",yaxt="n",xlab="",ylab="",col="white")
    
    # Get the plot information so the image will fill the plot box, and draw it
    lim <- par()
    rasterImage(my_image, 
                xleft=-1, xright=1, 
                ybottom=-1, ytop=1)
    
    text(0,0,"Data \n Checking \n Complete",cex=4)
  })
  
  ####
  ### Map issues tab components
  ####
  
  output$mapx<-renderLeaflet({
    if (is.null(data())) {
      return(NULL)
    }
    data <- data()
    
    df=data[data$N02==1,c("IDNUMBER","LONGITUDE","LATITUDE","N02","SPECIES","COUNTY","STATE")]
    df=df[which(!is.na(df$LONGITUDE)),]
    loccols=c("black","red")[df$N02+1]
    
    xy <- df[,c("LONGITUDE","LATITUDE")]
    dfsp=st_as_sf(df,coords = c('LONGITUDE', 'LATITUDE'),crs = ("+proj=longlat +datum=NAD83 +no_defs"))
    
    # Create leaflet
    lngmin=min(df$LONGITUDE[df$LONGITUDE<0],na.rm = TRUE)
    lngmax=max(df$LONGITUDE[df$LONGITUDE<0],na.rm = TRUE)
    latmin=min(df$LATITUDE[df$LATITUDE>0],na.rm = TRUE)
    latmax=max(df$LATITUDE[df$LATITUDE>0],na.rm = TRUE)
    
    iconSet <- awesomeIconList(
      Yes = makeAwesomeIcon(
        icon = 'ion-arrow-up-b',
        library = 'ion',
        iconColor = 'white',
        markerColor = 'black'
      ),
      No = makeAwesomeIcon(
        icon = 'ion-arrow-down-b',
        library = 'ion',
        iconColor = 'white',
        markerColor = 'red'
      ))
    
    l2<-leaflet(dfsp)%>%
      addTiles()%>%
      fitBounds(lng1 = lngmin, lng2 = lngmax, lat1 = latmin, lat2 = latmax) %>% 
      addAwesomeMarkers(data=dfsp,
                        popup = ~get_popup_content(dfsp),
                        icon=awesomeIcons(
                          library = "ion",  # the ion set of icons
                          icon = ifelse(  # conditional icon
                            test = dfsp$SPECIES == "RACCOONS",
                            yes = "ion-arrow-down-b",  # primary gets a down arrow
                            no = "ion-arrow-up-b"  # up arrows for secondary schools
                          ),
                          iconColor = "white",
                          markerColor = loccols  # you can specify a color for the marker 
                        ))%>%
      addLegendAwesomeIcon(iconSet = iconSet,
                           orientation = 'vertical',
                           marker = TRUE,
                           title = htmltools::tags$div(
                             style = 'font-size: 20px;',
                             'County-location match-up'),
                           labelStyle = 'font-size: 16px;',
                           position = 'topleft',
                           group = 'Vertical Legend')
    
    addPolygons(map=l2,data = uscd, 
                color = "blue",
                fillOpacity = 0,
                weight  = 1,
                layerId = ~COUNTYNS,
                label = paste(uscd$NAME, " County"))
    
    
  })
  
  
  output$tablex <- renderDataTable({
    data<-data()
    badlocs=data[which(data$N02==1),c("DATE","IDNUMBER","SPECIES","LATITUDE","LONGITUDE","MIS_State","MIS_County","LATLON_State","LATLON_County")]
    badlocs
  })
  
  output$stateerror <- renderValueBox({
    data<-data()
    state.err=sum(data$N01,na.rm = TRUE)
    valueBox(
      state.err, "Number of records with GPS-state mismatches", icon = icon("exclamation-triangle"),
      color = "orange"
    )
  })
  
  output$countyerror <- renderValueBox({
    data<-data()
    cty.err=sum(data$N02,na.rm = TRUE)
    valueBox(
      cty.err, "Number of records with GPS-county mismatches", icon = icon("exclamation-triangle"),
      color = "green"
    )
  })
  
  
  
  ####
  ### Sample Results tab info
  ####
  output$RabiesResult<-renderPlot({
    data<-data()
    sumres=data.frame(Errors=c("Rabies","Titer","Other"),Number=c(length(which(data$BRAINSTEMSAMPLE=="YES"))-sum(data$N33),
                                                                  length(which(data$BLOODSAMPLE=="YES"))-sum(data$N31),
                                                                  length(which(data$OTHERSAMPLE=="YES"))-sum(data$N33a)),
                      WithResults=c(sum(data$N33), sum(data$N31),sum(data$N33a)))
    barplot(t(as.matrix(sumres[,-1])),names=sumres$Errors,xlab="Samples",ylab="Count",col=viridis(2))
    legend("topright",c("Completed results","Results needed"),fill=viridis(2))
  })
  
  output$rabiesissue <- renderValueBox({
    data<-data()
    rab.err=sum(data$N33,na.rm = TRUE)
    valueBox(
      rab.err, "Number of missing RABIESBRAINRESULTS after 30 days", icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$titerissue <- renderValueBox({
    data<-data()
    tit.err=sum(data$N31,na.rm = TRUE)
    valueBox(
      tit.err, "Number of missing RABIESVNA_IUML results after a year", icon = icon("exclamation-triangle"),
      color = "green"
    )
  }) 
  
  output$othersampissue <- renderValueBox({
    data<-data()
    ct.err=sum(data$N33a,na.rm = TRUE)
    valueBox(
      ct.err, "Number of missing AGERECORDED or TTCC results after a year", icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })
  
  
  ####
  ### Method-fate tab info
  ####
  
  output$tablefate <- renderDataTable({
    data<-data()
    data1=data[which(data$N06==1|data$N07==1|data$N08==1|data$N09==1|data$N10==1|data$N11==1|data$N12==1|data$N13),]
    
    if(dim(data1)[1]==0){
      data.table(Error="No method-fate errors")
    }else{
      fatecoltab=data.frame(table(data1$METHOD,data1$FATE))
      names(fatecoltab)=c("Method","Fate","Freq")
      fcts=dcast(fatecoltab,Method~Fate)
      fcts
    }
    
  })
  
  
  ####
  ### Error tab info
  ####
  ## Rabies results tab info
  output$ErrorPlots<-renderPlot({
    data<-data()
    
    errsum=colSums(data[,which(names(data)%in%Fix_Comments$ErrorCode)],na.rm = TRUE)
    
    errsum=data.frame(Names=names(errsum),Count=errsum)
    errsum=errsum[which(errsum$Count>0),]
    errsum$Names=factor(errsum$Names,ordered=TRUE,levels=rev(Fix_Comments$ErrorCode))
    ggplot(errsum, aes(x = ((Names)), y = Count, main="Number of Errors")) +
      geom_bar(stat = "identity") +
      coord_flip() + scale_y_continuous(name="Number of errors") +
      scale_x_discrete(name="Error Code") +
      theme(axis.text.x = element_text(face="bold", color="#008000",
                                       size=8, angle=0),
            axis.text.y = element_text(face="bold", color="#008000",
                                       size=8, angle=0))
  })
  
  ### Error results tab
  output$tableerror <- renderDataTable({
    data<-data()
    errsum=data.frame(Error_ID=Fix_Comments$ErrorCode,Count=colSums(data[,which(names(data)%in%Fix_Comments$ErrorCode)],na.rm = TRUE),Error=Fix_Comments$Description)
    errsum=errsum[which(errsum$Count>0),]
    data.table(errsum)
  })
  
  ### Target species tab
  output$targetsps <- renderDataTable({
    data<-data()
    data1=data[which(data$N20==1,),]
    
    if(dim(data1)[1]==0){
      data.table(Error="No target species errors")
    }else{
      spstab=data.frame(table(data1$SPECIES,data1$TARGETSPECIES))
      names(spstab)=c("Species","Target_Species","Freq")
      scts=dcast(spstab,Species~Target_Species)
      scts
    }
  })
  
  ### Download data button information
  output$download <- downloadHandler(
    filename = function() {
      paste(gsub("\\..*","",input$ersdata), "_withErrors.csv", sep="")
    },
    content = function(file) {
      data=data()[,c(1:data()$column[1],which(names(data())%in%c("MIS_State","LATLON_State","MIS_County","LATLON_County","Errors")))]
      
      if(input$datatype=="MIS"){
        write.csv(data, file,row.names = FALSE,na="")
      }else{
        names(data)=c(misdbf[match(names(data)[1:data()$column[1]],misdbf$MIS),"DBF.Uploader"],"MIS_State","LATLON_State","MIS_County","LATLON_County","Errors")
        write.csv(data, file,row.names = FALSE,na="")
      }      
    }
  )
  
  ### Download data button information for .xlsx
  output$downloadxlsx <- downloadHandler(
    filename = function() {
      paste(gsub("\\..*","",input$ersdata), "_withErrors.xlsx", sep="")
    },
    content = function(file) {
      data=data()[,c(1:data()$column[1],which(names(data())%in%c("MIS_State","LATLON_State","MIS_County","LATLON_County","Errors")))]
      if(input$datatype=="MIS"){
        #openxlsx::write.xlsx(data, file,col.names = TRUE)
        writexl::write_xlsx(data,file,col_names = TRUE)
      }else{
        names(data)=c(misdbf[match(names(data)[1:data()$column[1]],misdbf$MIS),"DBF.Uploader"],"MIS_State","LATLON_State","MIS_County","LATLON_County","Errors")
        
        #openxlsx::write.xlsx(data, file,col.names = TRUE)
        writexl::write_xlsx(data,file,col_names = TRUE)
      }      
    }
    
  )
  
  
  session$onSessionEnded(stopApp)
  
}

# Run the application 
shinyApp(ui = ui, server = server)



