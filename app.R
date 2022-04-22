########################################################################
########################################################################
###
### Shiny app for NRMP data checking, 
### This app is targeted for examination of MIS data and the DBF uploader, 
###
### Amy J Davis
### February 16, 2021, Updated April 19, 2022
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
library(raster)
library(tidyverse)
library(tigris)
library(shinycssloaders)
library(htmltools)
library(rgdal)
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


get_popup_content <- function(dfsp) {
  paste0(
    "<b>ID: ", dfsp@data$IDNUMBER, "</b>",
    "<br>",
    "<br>Species: ", dfsp@data$SPECIES,
    "<br>County in record: ", dfsp@data$COUNTY  )
}

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)
options(shiny.maxRequestSize = 30*1024^2)
### 

### Read in Error code file
Fix_Comments <- read_excel("www/DataCheckingErrorCodesAll.xlsx")
misdbf=read.csv("www/MIS_DBF_colnames.csv")

### County information
stfp <- 1:56
stfp <- stfp[-c(2,3,7,14,15,43,52)]
countiesx <- tigris::counties(state = stfp, cb=TRUE)


# Define UI f
ui <- dashboardPage(
  
  skin='blue',
  title="NRMP Data Cleaning App",
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
                                                     "NRMP MIS and DBF Uploader Data Cleaning App")))),
  
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
    tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"Use this file uploader to select the Excel file of the data you would like checked for errors.  The file needs to be in an Excel format (.xls or .xlsx) and should include the standard columns from either MIS output or the historical DBF Uploader. The column names must not be modified. Note: This file uploader can only handle file sizes of 30MB or less. The larger the file the longer it will take to check the data. If you are uploading a very large file, I recommend downloading the data with errors first before viewing other tabs. The Location Check tab in particular will take a long time to plot for large datasets.",align='left'),
    tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"Select the data type then upload the file you want to check.  Then click the button to run the data checker",align='left'),
    ## User inputs or from our study
    radioButtons(inputId = "datatype",label="Is your data from MIS (most common) or the historical database (DBF uploader)?",
                 choices=c("MIS","Historical"),selected = "MIS",inline = TRUE),
    fileInput(inputId = "ersdata",label = "Select Data File",accept = c('xls','xlsx')),
    # Go! button
    fluidRow(column(2),column(1,actionButton("go","Run data checker",icon("running"),
                                             style="color: #fff; background-color: #c00000; border-color: #Adf.combF2F; font-size:140%"))),
    
    tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"To download the data with errors, click the button below",align='left'),
    downloadButton(outputId = "download",label =  "Download data with errors",style="color:black;font-size:18px")
  ),
  
  # Show output
  dashboardBody(
    tabsetPanel(
      tabPanel("Summary of Errors",
               box(width=12,title=span("Summary of errors in the data",style="color:blue;font-size:28px"),status="success",
                   # varImp Plot
                   column(10,plotOutput('ErrorPlots'))
               ),
               box(width=12,title=span("Table of data errors by code",style="color:blue;font-size:28px"),status="success",
                   #confusion matrix, model accuracy metrics
                   column(10,withSpinner(dataTableOutput(outputId="tableerror")))
               )
      ),
      tabPanel("Location Check",
               box(width=12,title=span("Map of MIS samples",style="color:blue;font-size:28px"),status="success",
                   withSpinner(leafletOutput(outputId = "mapx"))
                   
               ),
               box(width=12,title=span("Table of samples with county location errors",style="color:blue;font-size:28px"),status="success",
                   dataTableOutput('tablex')
               ),
               fluidRow(
                 box(width=12,title=span("Summary of location issues",style="color:blue;font-size:28px"),status="success",
                     # 
                     # Dynamic valueBoxes
                     valueBoxOutput("stateerror"),
                     valueBoxOutput("countyerror")
                 )
               )
               
      ),      
      tabPanel("Rabies Check",
               box(width=12,title=span("Proportion of samples with rabies results",style="color:blue;font-size:28px"),status="success",
                   column(10,plotOutput('RabiesResult')),
                   column(6,HTML("The error for results not provided will only show if it has been more than 30 days for rabies or variant typing or more than a year for age and RVNA results since the sample was collected, allowing time for the laboratory results to come back. "))   
               ),
               fluidRow(
                 box(width=12,title=span("Summary of test result issues",style="color:blue;font-size:28px"),status="success",
                     valueBoxOutput("rabiesissue"),
                     valueBoxOutput("titerissue"),
                     valueBoxOutput("othersampissue"))
               )
      ),
      
      tabPanel("Fate Check",
               box(width=12,title=span("Distribution of different FATE types",style="color:blue;font-size:28px"),status="success",
                   # varImp Plot
                   column(6,plotOutput('FateResult'))
               ),
               box(width=12,title=span("FATE-METHOD combinations",style="color:blue;font-size:28px"),status="success",
                   #confusion matrix, model accuracy metrics
                   column(8,withSpinner(dataTableOutput(outputId="tablefate"))))
      ),
      tabPanel("Serology Check",
               box(width=12,title=span("Serology Information",style="color:blue;font-size:28px"),status="success",
                   valueBoxOutput("serologyinfo")),
               box(width=12,title=span("Distribution of RVNA results by interpretation",style="color:blue;font-size:28px"),status="success",
                   # varImp Plot
                   column(6,plotOutput('VNAresult'))
               )
      ),
      tabPanel("Error Definitions",
               tags$iframe(style="height:1000px; width:100%; scrolling=yes", 
                           src="DataCheckingErrorCodesAll.pdf")),
      tabPanel("Method/Fate Scenarios",
               tags$iframe(style="height:1000px; width:100%; scrolling=yes", 
                           src="scenarios.pdf"))
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
    NRMP_Masters <- read_excel(input$ersdata$datapath)
    
    
    ### Change the names of the columns if the data came from the historical data checker
    if(input$datatype=="Historical"){
      NRMP_Masters$Last=NA
      colind=match(names(NRMP_Masters),misdbf$DBF.Uploader)
      NRMP_Master=NRMP_Masters
      names(NRMP_Master)=ifelse(is.na(colind),names(NRMP_Masters),misdbf$MIS[colind])
    }else{
      NRMP_Master=NRMP_Masters
    }
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
    NRMP_Master$State_on_record=toupper(fips[match(NRMP_Master$STATE,fips$state),"state_name"])
    
    # AJD edit to get county polygon data frame information
    uscd=tigris::counties(state=stfp,cb = TRUE)
    uscsf=uscd$geometry
    uscs=as(uscsf,"Spatial")
    # Extract polygon ID's
    pid <- sapply(slot(uscs, "polygons"), function(x) slot(x, "ID"))
    # Create dataframe with correct rownames
    p.df <- data.frame( GEOID=uscd$GEOID, row.names = pid)
    usc=SpatialPolygonsDataFrame(uscs,data = p.df)
    # Define coordinate system for NRMP data and pull coordinate system from tigris counties file
    usc_crs <- proj4string(usc)
    NRMP_crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
    # Get NRMP and usc ready for over()
    NRMP_Master$LONGITUDE[is.na(NRMP_Master$LONGITUDE)] <- 0
    NRMP_Master$LATITUDE[is.na(NRMP_Master$LATITUDE)] <- 0
    
    NRMP_locs=NRMP_Master[,c("IDNUMBER","LONGITUDE","LATITUDE")]
    coordinates(NRMP_locs) <- c('LONGITUDE', 'LATITUDE')
    proj4string(NRMP_locs) <- usc_crs
    NRMP_Co_ref <- spTransform(NRMP_locs, usc_crs)
    # Perform over()
    Co_per_NRMP_rec <- over(NRMP_Co_ref, usc)
    # AJD edit to get state and county info
    Co_per_NRMP_rec$STATE=fips[match(substr(Co_per_NRMP_rec$GEOID, start = 1, stop = 2),fips$state_code),"state_name"]
    Co_per_NRMP_rec$COUNTY=fips[match(Co_per_NRMP_rec$GEOID,fips$FIPS),"county"]
    Co_per_NRMP_rec$COUNTY=toupper(stringr::str_remove(Co_per_NRMP_rec$COUNTY," County"))
    
    
    # Create Check County and State columns
    NRMP_Master$State_from_GPS <- toupper(Co_per_NRMP_rec$STATE)
    NRMP_Master$State_from_GPS <- gsub('[[:punct:] ]+',' ',NRMP_Master$State_from_GPS)
    
    NRMP_Master$County_on_record <- NRMP_Master$COUNTY
    NRMP_Master$County_from_GPS <- Co_per_NRMP_rec$COUNTY
    NRMP_Master$County_from_GPS <- gsub('[[:punct:] ]+',' ',NRMP_Master$County_from_GPS)
    
    
    # AJD Getting counts of correct state and county locations
    table(NRMP_Master$State_on_record==NRMP_Master$State_from_GPS)
    table(NRMP_Master$COUNTY==NRMP_Master$County_from_GPS)
    
    # AJD Check N01 error: Location in wrong state
    NRMP_Master$N01=ifelse(NRMP_Master$State_on_record!=NRMP_Master$State_from_GPS&NRMP_Master$`LAT/LONRECORDED`=="YES",1,0)
    # AJD Check N02 error: Location in wrong county
    NRMP_Master$N02=ifelse(NRMP_Master$COUNTY!=NRMP_Master$County_from_GPS&NRMP_Master$`LAT/LONRECORDED`=="YES",1,0)
    
    table(NRMP_Master$N01)
    table(NRMP_Master$N02)
    
    ######################################
    ###
    ### AJD Check Cities ####
    ###
    ######################################
    # You need to place a 1 in the City_Check file for your state if you would like the cities checked
    # Reverse Geocode coordinates to cities
    NRMP_Master$lat <- NRMP_Master$LATITUDE
    NRMP_Master$lon <- NRMP_Master$LONGITUDE
    
    # if(input$checkcity==TRUE){
    #   NRMP_Master$ChCitySelect = (City_Check[match(NRMP_Master$STATE,City_Check$State),1])$Select
    #   NRMP_Master$ChCity <- ""
    #   
    #   ####Pick Section 1 or Section 2 to turn on
    #   ###Section 1
    #   citycheckind=intersect(intersect(which(!is.na(NRMP_Master$TOWN)),which(!is.na(NRMP_Master$LONGITUDE))),which(NRMP_Master$ChCitySelect == 1))
    #   city_per_NRMP_rec=apply(NRMP_Master[citycheckind,],1,function(x)revgeo(x[which(names(NRMP_Master)=="lon")],x[which(names(NRMP_Master)=="lat")], provider =  'photon', API = NULL, output = 'frame', item = 'city'))
    #   NRMP_Master[citycheckind,"ChCity"] <- toupper(as.character(sapply(city_per_NRMP_rec,function(x)x$city)))
    #   
    #   NRMP_Master$ChCity=ifelse(NRMP_Master$ChCity=="CITY NOT FOUND",NA,NRMP_Master$ChCity)
    #   
    #   ###End Section 2
    #   
    #   ##N03
    #   ##Check if the town is correct
    #   NRMP_Master$N03=ifelse(NRMP_Master$TOWN==NRMP_Master$ChCity,0,1)
    #   NRMP_Master[which(is.na(NRMP_Master$N03)),"N03"]=1
    #   
    #   # In this version the NA values for N03 indicate that the Town information was not provided originally
    #   ### End Check City ####
    #   
    # }
    
    
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
    NRMP_Master$N07=ifelse(NRMP_Master$METHOD=="HANDCAUGHT/GATHERED"&!grepl('DIED UNDER CARE|EUTHANIZED|FOUND DEAD|OTHER|RELEASED|NO FATE',NRMP_Master$FATE),1,0)
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
    NRMP_Master=NRMP_Master[order(NRMP_Master$STATE,NRMP_Master$IDNUMBER,NRMP_Master$DATE),]
    NRMP_Master$WasCaught=0
    NRMP_Master$diffdat=c(0,as.numeric(diff(NRMP_Master$DATE2,units="days"),units="days"))
    
    NRMP_Master$IDState=paste(NRMP_Master$IDNUMBER,NRMP_Master$STATE,sep=".")
    
    # Indicator for first of ID series
    NRMP_Master$t=sapply(1:nrow(NRMP_Master)-1,function(x)identical(NRMP_Master$IDState[x],NRMP_Master$IDState[x+1]))
    NRMP_Master$diffdat=ifelse(NRMP_Master$t==FALSE,999,NRMP_Master$diffdat)
    
    # Error for the if the animal was caught within the last 30 days but PROCESSED<30DAYSAGE is "NO"
    NRMP_Master$N23=ifelse(NRMP_Master$diffdat<30&NRMP_Master$`PROCESSED<30DAYSAGO`=="NO"&!is.na(NRMP_Master$`PROCESSED<30DAYSAGO`),1,0)
    
    # Making sure animals don't change species among captures
    spcheck=tapply(NRMP_Master$SPECIES,NRMP_Master$IDState,function(x)length(unique(x))>1)
    NRMP_Master$N24=0
    NRMP_Master[which(NRMP_Master$IDState%in%names(spcheck[spcheck==TRUE])&!is.na(NRMP_Master$IDNUMBER)),"N24"]=1
    
    # Error if an individual is called both a male and female at some point during its capture
    scheck=tapply(NRMP_Master$SEX,NRMP_Master$IDState,function(x)any(x%in%c("MALE"))&any(x%in%c("FEMALE")))
    NRMP_Master$N25=0
    NRMP_Master[which(NRMP_Master$IDState%in%names(scheck[scheck==TRUE])),"N25"]=1
    
    # Error if an individual as the same ID and captured on the same day
    NRMP_Master$N26=ifelse(NRMP_Master$diffdat==0&!is.na(NRMP_Master$IDNUMBER),1,0)
    
    NRMP_Master=NRMP_Master[order(NRMP_Master$AmyID),]
    NRMP_Master$N26a=ifelse(NRMP_Master$LACTATION=="YES"&NRMP_Master$SEX!="FEMALE",1,0)
    NRMP_Master$N26a[is.na(NRMP_Master$N26a)]=0
    
    ###
    ### More individual checkes
    ###
    # Error if juveniles are lactating
    NRMP_Master$N27=ifelse(NRMP_Master$RELATIVEAGE=="JUVENILE (YOY)"&!is.na(NRMP_Master$RELATIVEAGE)&NRMP_Master$LACTATION=="YES"&!is.na(NRMP_Master$LACTATION),1,0)
    
    # Error if no weight is provided when WEIGHTRECORDED is "YES"
    NRMP_Master$N27a=ifelse(NRMP_Master$WEIGHTRECORDED=="YES"&!is.na(NRMP_Master$WEIGHTRECORDED)&is.na(NRMP_Master$WEIGHT),1,0)
    
    # Error in Raccoon WEIGHT
    NRMP_Master$N28=ifelse(NRMP_Master$SPECIES=="RACCOONS"&!is.na(NRMP_Master$WEIGHT)&(NRMP_Master$WEIGHT<0.5|NRMP_Master$WEIGHT>20),1,0)
    
    # Error if HANDVACCINATED is "YES" and not drugs are provided
    NRMP_Master$N29=ifelse(NRMP_Master$HANDVACCINATED=="YES"&!is.na(NRMP_Master$HANDVACCINATED)&(NRMP_Master$DRUG1APPLIED!="IMRAB3, RABIES VACCINE"|NRMP_Master$DRUG2APPLIED!="IMRAB3, RABIES VACCINE"|NRMP_Master$DRUG3APPLIED!="IMRAB3, RABIES VACCINE"),1,0)
    NRMP_Master$N29=ifelse(NRMP_Master$HANDVACCINATED=="NO"&!is.na(NRMP_Master$HANDVACCINATED)&grepl("IMRAB3, RABIES VACCINE",paste(NRMP_Master$DRUG1APPLIED,NRMP_Master$DRUG2APPLIED,NRMP_Master$DRUG3APPLIED)),1,NRMP_Master$N29)
    
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
    hm2=lapply(hm,names)
    NRMP_Master$Errors=do.call('rbind',lapply(hm2,paste,collapse=";"))
    
    NRMP_Master
    
  })
  
  ####
  ### Map issues tab components
  ####
  
  output$mapx<-renderLeaflet({
    if (is.null(data())) {
      return(NULL)
    }
    data <- data()
    
    df=data[,c("IDNUMBER","LONGITUDE","LATITUDE","N02","SPECIES","COUNTY","STATE")]
    loccols=c("black","red")[df$N02+1]
    
    xy <- df[,c("LONGITUDE","LATITUDE")]
    dfsp=SpatialPointsDataFrame(coords = xy, data = df,
                                proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))
    
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
                            test = dfsp@data$SPECIES == "RACCOONS",
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
    
    addPolygons(map=l2,data = countiesx, 
                color = "blue",
                fillOpacity = 0,
                weight  = 1,
                layerId = ~COUNTYNS,
                label = paste(countiesx$NAME, " County"))
    
    
    
    
  })
  
  
  output$tablex <- renderDataTable({
    data<-data()
    badlocs=data[which(data$N02==1),c("IDNUMBER","SPECIES","County_on_record","State_on_record","County_from_GPS","State_from_GPS","LONGITUDE","LATITUDE")]
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
  ### Rabies results tab info
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
      rab.err, "Number of records missing rabies results after a year", icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$titerissue <- renderValueBox({
    data<-data()
    tit.err=sum(data$N31,na.rm = TRUE)
    valueBox(
      tit.err, "Number of records missing titer results after a year", icon = icon("exclamation-triangle"),
      color = "green"
    )
  }) 
  
  output$othersampissue <- renderValueBox({
    data<-data()
    ct.err=sum(data$N33a,na.rm = TRUE)
    valueBox(
      ct.err, "Number of records missing results from other samples", icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })
  
  
  
  ####
  ### Fate tab info
  ####
  
  output$FateResult<-renderPlot({
    data<-data()
    pie(table(data$FATE),col=viridis(5),cex=1.3)
  })
  
  output$tablefate <- renderDataTable({
    data<-data()
    fatecoltab=data.frame(table(data$FATE,data$COLLECTOR))
    names(fatecoltab)=c("Fate","Collector","Freq")
    fcts=dcast(fatecoltab,Fate~Collector)
    fcts
  })
  
  ####
  #### VNA results info tab
  ####
  output$serologyinfo <- renderValueBox({
    data<-data()
    rab.err=length(which(!is.na(data$VNAvals)))
    valueBox(
      rab.err, "Number of records with serology results", icon = icon("chart-bar"),
      color = "red"
    )
  })
  
  output$VNAresult<-renderPlot({
    data<-data()
    nmax=max(data$VNAvals,na.rm=TRUE)
    h1=hist(data$VNAvals[data$RABIESVNAINTERPRET=="NEGATIVE"],breaks=seq(0,nmax,0.05), col=rgb(1,0,0,0.5),xlab="RVNA IUML",main="Histogram of RVNA results")
    h2=hist(data$VNAvals[data$RABIESVNAINTERPRET=="POSITIVE"],breaks=seq(0,nmax,0.05),col=rgb(0,0,1,0.5), add=TRUE)
    legend("topright",legend = c("NEGATIVE","POSITIVE"),fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))
    
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
  
  ### Download data button information
  output$download <- downloadHandler(
    filename = function() {
      paste(gsub("\\..*","",input$ersdata), "_withErrors.csv", sep="")
    },
    content = function(file) {
      data=data()[,c(1:data()$column[1],which(names(data())%in%c("State_on_record","State_from_GPS","County_on_record","County_from_GPS","Errors")))]
      
      if(input$datatype=="MIS"){
        write.csv(data, file,row.names = FALSE,na="")
      }else{
        names(data)=c(misdbf[match(names(data)[1:data()$column[1]],misdbf$MIS),"DBF.Uploader"],"State_on_record","State_from_GPS","County_on_record","County_from_GPS","Errors")
        write.csv(data, file,row.names = FALSE,na="")
      }      
    }
  )
  
  session$onSessionEnded(stopApp)
  
}

# Run the application 
shinyApp(ui = ui, server = server)



