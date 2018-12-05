### Stream Hydrology And Rainfall Knowledge System (SHARKS)
### Author: Conrad Brendel, cbrendel@vt.edu

### Install Missing Packages ________________________________________________________________________________________________________________
list.of.packages <- c("shiny", "shinyjs","shinydashboard","jsonlite","RJSONIO","RCurl","lubridate","stringr","DT","data.table","ggplot2","zoo","dataRetrieval","EcoHydRology","grid","gridExtra","dplyr","devtools","googlesheets","googleAuthR","lubridate","shinyWidgets","rwunderground","pracma","shinyalert","leaflet","scales","rgdal","shadowtext")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Install packages from github if not installed
if(!"dashboardthemes" %in% installed.packages()[,"Package"]){
  devtools::install_github("nik01010/dashboardthemes")
}

if(!"googleID" %in% installed.packages()[,"Package"]){
  devtools::install_github("MarkEdmondson1234/googleID")
}

### Call Libraries __________________________________________________________________________________________________________________________
library(shiny)
library(shinydashboard) # Used for UI
library(shinyWidgets)
library(RCurl)
library(lubridate) # Used for date manipulation
library(stringr)
library(DT)
library(data.table)
library(ggplot2) # Used to plot
require(zoo) # Used for precipitation data sliding window analysis
library(dataRetrieval) #See USGS GRAN Site, pulls data from NWIS
library(EcoHydRology) #need this for baseflow separation
library(grid)
library(gridExtra)
library(dplyr)
library(dashboardthemes) #Applys theme to dashboard
library(googlesheets)
library(shinyjs)
library(googleAuthR) # Used for Google Authentication
library(googleID) # Used to get Google ID when logging in
library(lubridate) # Used to manage dates
library(rwunderground) # Used for Forecast
library(pracma) # Used to calculate volumes of brushed data in interactive hydrograph
library(shinyalert) # Used for help buttons
library(leaflet) # Used for mapping
library(rgdal) # Used to display shapefiles
library(shadowtext) # Adds halo to data labels for flood stage

### Create Theme for UI _______________________________________________________________________________________________________________________

# Create Dashboard Theme Object
theme_blue_gradient <- shinyDashboardThemeDIY(
  
  # general
  appFontFamily = "Tahoma"
  ,appFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  # header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(0,0,0)"
  ,headerButtonIconColor = "rgb(255,255,255)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(0,0,0)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  # sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(20,97,117)"
    ,colorMiddle = "rgb(56,161,187)"
    ,colorEnd = "rgb(3,22,56)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 16
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"

  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  # boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"

  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  # inputs
  ,buttonBackColor = "rgba(58,140,182,1)"
  ,buttonTextColor = "rgb(255,255,255)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(0,0,0)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(211,211,211)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  # tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

### Google Authentification Options _________________________________________________________________________________________________________
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile",
                                        "https://www.googleapis.com/auth/spreadsheets",
                                        "https://www.googleapis.com/auth/spreadsheets.readonly"
))


options(googleAuthR.webapp.client_id = "removed_client_id") # Create a Google App and add your client and secret here (instead of "client_id" and "client_secret") to access Google Sheets that store data
options(googleAuthR.webapp.client_secret = "removed_client_secret")

### Define API Functions ____________________________________________________________________________________________________________________

# Helper functions that test whether an object is either NULL or a list of NULLs and then process
is.NullOb=function(x) is.null(x) | all(sapply(x, is.null))

rmNullObs=function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) 
    rmNullObs(x) else x)
}

# Get values from GoogleSheet
getvalues=function(spreadsheetId, ranges = NULL, majorDimension=NULL,valueRenderOption=NULL,dateTimeRenderOption=NULL) {
  url <- sprintf("https://sheets.googleapis.com/v4/spreadsheets/%s/%s", spreadsheetId,"values:batchGet")
  # sheets.spreadsheets.get
  pars = list(ranges = ranges, majorDimension=majorDimension,valueRenderOption=valueRenderOption,dateTimeRenderOption=dateTimeRenderOption)
  f <- googleAuthR::gar_api_generator(url, "GET", pars_args = rmNullObs(pars), 
                                      data_parse_function = function(x) x)
  f()
  
}

### Define UI _______________________________________________________________________________________________________________________________
sidebar=dashboardSidebar(
  useShinyjs(),
  useShinyalert(), # Set up Shiny Alerts for Help boxes
  
  # Apply Dashboard Theme
  theme_blue_gradient,

  # Override/Format CSS
  tags$head(
    tags$style(HTML("hr {border-top: 5px solid #000000;}", # Format thickness for horizontal rule
                    "hr {margin-bottom: -15px !important;}", # Format bottom spacing for horizontal rule
                    "h3 {margin-bottom: 5px !important;}", # Format bottom spacing for h3 headers
                    ".form-group, .selectize-control {margin-bottom: -5px;}", # Format bottom spacing for inputs
                    ".help-button {color:black;}", # Text Color for help buttons on tabs
                    ".help-button:hover {color:gray;}", #Hover Text Color for help buttons on tabs
                    ".help-button:focus {color:black;" #Changes help buttons back to black after clicking on them
                    ))
  ),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",         # Hide Error Messages
             ".shiny-output-error:before { visibility: hidden; }",  # Hide Error Messages
             
             ".selectize-input {@include form-element(); top:1px; height: auto; display: inline-block:}", # Fixes dashboard theme override for selectize inputs <-caused input boxes to not resize and hid selected inputs
             
             ".input-sm { font-size:110%; line-height: 110%;}",
             ".selectize-input { font-size: 110%; line-height: 110%;}",     # adjust text size
             ".selectize-dropdown { font-size: 110%; line-height: 110%; }"  # adjust text size
             ),

  width=300,
  sidebarMenu(
      id="sidebarmenu",
      menuItem("Summary", tabName = "Summary", icon = icon("sliders"), badgeLabel = "Home",badgeColor="aqua"),
      menuItem("Map",tabName="Map",icon=icon("map")),
      menuItem("Forecast",tabName="Forecast",icon=icon("bolt")),
      menuItem("Interactive Plots",tabName="InteractivePlots",icon=icon("signal")),
      menuItem("Tables", icon = icon("th"), tabName = "Tables"),
      menuItem("Storm Sewer", icon=icon("bullseye"),tabName="HOBO"),
      menuItem("Real-Time Flood Stages",icon=icon("bar-chart"),tabName="Flood"),
      menuItem("Help",icon=icon("question"),tabName="Help")
    ),
    
    hr(), # Add Horizontal Rule
  
    HTML("<p> \n &nbsp; <p>"), #Add some blank space,
  
    HTML("<b><font size='5'>&nbsp; Inputs: </font></b>"),
  
    # Input Date Range
    dateRangeInput("PrecipDate", label=div(div(style="font-size:115%;display:inline-block",tags$b("Date Range")),div(style="display:inline-block",actionButton("help_dates",label="",icon=icon("question"),style="height:0px;width:0px;padding:0px;border:none;background:none"))),
                   start = NULL, end = NULL,
                   min = NULL, max = NULL,
                   format = "yyyy-mm-dd",
                   startview = "month", weekstart = 0,
                   language = "en", separator = " to "),
  
    # Input State
    selectInput("user.state", label=div(div(style="font-size:115%;display:inline-block",tags$b("ASOS Station State")),div(style="display:inline-block",actionButton("help_state",label="",icon=icon("question"),style="height:0px;width:0px;padding:0px;border:none;background:none"))), 
                                                                    choices=list("AK"="AK","AL"="AL","AR"="AR","AZ"="AZ","CA"="CA","CO"="CO", "CT"="CT","DE"="DE","FL"="FL","GA"="GA",
                                                                                 "HI"="HI","IA"="IA","ID"="ID","IL"="IL","IN"="IN","KS"="KS","KY"="KY","LA"="LA","MA"="MA","MD"="MD",
                                                                                 "ME"="ME","MI"="MI","MN"="MN","MO"="MO","MS"="MS","MT"="MT","NC"="NC","ND"="ND","NE"="NE","NH"="NH",
                                                                                 "NJ"="NJ","NM"="NM","NV"="NV","NY"="NY","OH"="OH","OK"="OK","OR"="OR","PA"="PA","RI"="RI","SC"="SC",
                                                                                 "SD"="SD","TN"="TN","TX"="TX","UT"="UT","VA"="VA","VT"="VT","WA"="WA","WI"="WI","WV"="WV","WY"="WY"),
                selected = "VA", multiple = FALSE, selectize = TRUE),
    
    # Input Site FAA ID
    textInput("user.faaid", label = div(div(style="font-size:115%;display:inline-block",tags$b("ASOS Station FAA ID")),div(style="display:inline-block",actionButton("help_faaid",label="",icon=icon("question"),style="height:0px;width:0px;padding:0px;border:none;background:none"))), value="ROA", placeholder="XXX"),
    
    # Input USGS Precip Gauges
    selectizeInput("USGS_Precip", label=div(div(style="font-size:115%;display:inline-block",tags$b("USGS Precip. Station IDs")),div(style="display:inline-block",actionButton("help_usgs_precip",label="",icon=icon("question"),style="height:0px;width:0px;padding:0px;border:none;background:none"))), choices=list("Lick Run"="0205551460",
                                                                                        "Roanoke Centre for Industry & Technology (RCIT)"="371840079534900",
                                                                                        "Peters Creek Road"="371824080002600",
                                                                                        "Roanoke Fire-EMS Station #5"="371709079580800",
                                                                                        "Strauss Park"="371657080002800",
                                                                                        "Shrine Hill Park"="371518079591700",
                                                                                        "Garden City Elementary School"="371339079554400",
                                                                                        "Hidden Valley Middle School"="371520080015100",
                                                                                        "Mill Mountain"="371459079560300"),
                   selected=c("Lick Run"="0205551460"), multiple=TRUE,options=list(create=TRUE,placeholder="Select From List Below or Enter Station #")
    ),
    
    # Input USGS Site ID
    selectizeInput("USGS_Site", label=div(div(style="font-size:115%;display:inline-block",tags$b("USGS Stream Station IDs")),div(style="display:inline-block",actionButton("help_usgs_flow",label="",icon=icon("question"),style="height:0px;width:0px;padding:0px;border:none;background:none"))), 
                   choices=list("Lick Run"="0205551460",
                                "Roanoke River at Roanoke"="02055000",
                                "Roanoke River at Glenvar"="02054530",
                                "Roanoke River at Lafayette"="02054500",
                                "Tinker Creek"="02055100"),
                   selected=c("Lick Run"="0205551460"), multiple=TRUE,options=list(create=TRUE,placeholder="Select From List Below or Enter Station #")
    ),
    
    # Input Watershed Area
    uiOutput("Box3"),
  
    # Select Parameters to Download
    selectizeInput("parameterCd", label=div(div(style="font-size:115%;display:inline-block",tags$b("Download Parameters")),div(style="display:inline-block",actionButton("help_parameters",label="",icon=icon("question"),style="height:0px;width:0px;padding:0px;border:none;background:none"))),choices=list("Discharge (cfs)"="00060",
                                                                                  "Gauge Height (ft)"="00065",
                                                                                  "Water Temperature (C)"="00010",
                                                                                  "Specific Conductance (microS/cm @ 25 C)"="00095",
                                                                                  "Dissolved Oxygen (mg/L)"="00300",
                                                                                  "pH"="00400",
                                                                                  "Turbidity (FNU)"="63680"),
                   selected=c("Discharge (cfs)"="00060","Gauge Height (ft)"="00065"), multiple=TRUE, options=list(placeholder="Select Parameters From List Below or Enter USGS Parameter Code")),
    
    hr(), # Add Horizontal Rule
  
    HTML("<p> \n &nbsp; <p>"), #Add some blank space
  
    HTML("<b><font size='5'>&nbsp; Options: </font></b>"),
    
    # Choose whether or not to run sliding window ARI analysis
    div(div(style="display:inline-block",switchInput("user.sw",label=tags$b("Calculate ARI"),value=FALSE,onLabel="Yes",offLabel="No",inline=T)),div(style="display:inline-block",actionButton("help_ari",label="",icon=icon("question"),style="height:0px;width:0px;padding:0px;border:none;background:none"))),
    

    # Select which Precipitation Gauge to Plot on Hyetograph
    uiOutput("Box1"),
    
    # Choose to plot a water quality parameter on secondary axis
    uiOutput("Box2"),
    
    # Button to show/hide extra inputs
    switchInput("show_hide",label=tags$b("Hidden Settings"),value=FALSE,onLabel="Show",offLabel="Hide"),
  
    # Extra inputs
    div(id="extra",
        hr(), # Add Horizontal Rule
        # Input Timezone
        selectInput("user.tz",label=div(div(style="font-size:115%;display:inline-block",tags$b("Time Zone")),div(style="display:inline-block",actionButton("help_time",label="",icon=icon("question"),style="height:0px;width:0px;padding:0px;border:none;background:none"))),
                    choices=list("America/New York (EST/EDT)"="America/New_York",
                                 "America/Chicago (CST/CDT)"="America/Chicago",
                                 "America/Denver (MST/MDT)"="America/Denver",
                                 "America/Los Angeles (WST/WDT)"="America/Los_Angeles",
                                 "America/Anchorage (AKST/AKDT)"="America/Anchorage",
                                 "Coordinated Universal Time (UTC)"="Etc/UTC"
                    ),
                    selected="America/New York (EST/EDT)", multiple=FALSE, selectize=TRUE
        ),
        
        # # Input Network Type
        selectInput("user.network", label=div(div(style="font-size:115%;display:inline-block",tags$b("NOAA Network")),div(style="display:inline-block",actionButton("help_network",label="",icon=icon("question"),style="height:0px;width:0px;padding:0px;border:none;background:none"))), choices=list("ASOS"=1),
                    selected = NULL, multiple = FALSE, selectize = TRUE),
        
        selectInput("plot_color",label=div(div(style="font-size:115%;display:inline-block",tags$b("Plot Colors By:")),div(style="display:inline-block",actionButton("help_color",label="",icon=icon("question"),style="height:0px;width:0px;padding:0px;border:none;background:none"))),choices=list("Station"=0,"Dataset"=1),selected=c("Station"=0),multiple=FALSE,selectize=TRUE)
    )
)

body=dashboardBody(
  tabItems(
    tabItem(tabName = "Summary",
            div(id="FAA",
                div(div(style="display:inline-block",h3("FAA IDs for selected State:")),div(style="display:inline-block",actionButton("help_faaid_table",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),
                DT::dataTableOutput("faaidtable")
            ),
            div(id="Summary_Tab",
              div(div(style="display:inline-block",h3("Data Summary:")),div(style="display:inline-block",actionButton("help_data_summary",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),
              DT::dataTableOutput("summary_table"),
              div(div(style="display:inline-block",h3("Runoff Volume Coefficient Summary:")),div(style="display:inline-block",actionButton("help_rv",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),
              DT::dataTableOutput("RV"),
              div(id="ARI",
                  div(div(style="display:inline-block", h3("ARI Summary:")),div(style="display:inline-block",actionButton("help_ari_table",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),
                  div(style="display:inline-block",downloadButton("ari_download","Download")),
                  div(style="display:inline-block",switchInput("ARI_summary",label=tags$b("Display"),value=TRUE,onLabel="Summary",offLabel="All Data")),
                  DT::dataTableOutput("sw")
              ),
              div(div(style="display:inline-block",h3("Hyetograph & Hydrograph:")),div(style="display:inline-block",actionButton("help_hh",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),
              uiOutput("sized_plot"),
              # div(img(src="Gauges.png",width=1000),style="text-align: center;") # Add static map of Roanoke stations
              div(id="flood",
                div(div(style="display:inline-block",h3("Maximum Flood Stages:")),div(style="display:inline-block",actionButton("help_flood",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),
                div(style="text-align:center",
                  div(id="lick_run",style="display:inline-block",plotOutput("flood_lick_run",width=300)),
                  div(id="tinker",style="display:inline-block",plotOutput("flood_tinker",width=300)),
                  div(id="rr1",style="display:inline-block",plotOutput("flood_rr1",width=300)),
                  div(id="rr2",style="display:inline-block",plotOutput("flood_rr2",width=300)),
                  div(id="rr3",style="display:inline-block",plotOutput("flood_rr3",width=300))
                )
              )
            )
    ),
    
    tabItem(tabName="Map",
            leafletOutput("map",height=850)
    ),
    
    tabItem(tabName="Forecast",
            div(style="display:inline-block",textInput("Location",label=div(div(style="display:inline-block","Forecast Location:"),div(style="display:inline-block",actionButton("help_forecast",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),value="Roanoke, VA")),
            HTML("<p> \n &nbsp; <p>"), #Add some blank space,
            div(style="text-align: center",htmlOutput("sticker")),
            HTML("<p> \n &nbsp; <p>"), #Add some blank space,
            DT::dataTableOutput("forecast")
    ),
    
    tabItem(tabName = "InteractivePlots",
            
            div(div(style="display:inline-block",switchInput("interact",label=tags$b("Select Plot:"),value=TRUE,onLabel="Hydrograph",offLabel="Hyetograph",inline=T)),div(style="display:inline-block",actionButton("help_interact_switch",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),
            div(id="hydro_int",
                plotOutput("hydro_interact",click="hydro_click",brush=brushOpts("hydro_brush",direction="x")),
                div(div(style="display:inline-block",h3("Clicked Data:")),div(style="display:inline-block",actionButton("help_hydro_click",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),
                DT::dataTableOutput("hydro_click"),
                div(div(style="display:inline-block",h3("Volume Summary:")),div(style="display:inline-block",actionButton("help_hydro_vol",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),
                DT::dataTableOutput("hydro_brush_vol"),
                div(div(style="display:inline-block", h3("Brushed Data:")),div(style="display:inline-block",actionButton("help_hydro_brush",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;padding-right:20px;border:none;background:none")),div(style="display:inline-block",downloadButton("hydro_brush_download","Download"))),
                DT::dataTableOutput("hydro_brush")
            ),
            div(id="hyet_int",
                plotOutput("hyet_interact",brush=brushOpts("hyet_brush",direction="x")),
                HTML("<p> \n &nbsp; <p>"), #Add some blank space,
                textOutput("hyet_depth"),
                textOutput("hyet_dur"),
                textOutput("hyet_int"),
                textOutput("hyet_max_int"),
                div(div(style="display:inline-block", h3("Brushed Data:")),div(style="display:inline-block",actionButton("help_hyet_brush",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;padding-right:20px;border:none;background:none")),div(style="display:inline-block",downloadButton("hyet_brush_download","Download"))),
                DT::dataTableOutput("hyet_brush")
            )
    ),
    
    tabItem(tabName = "Tables",
            div(div(style="display:inline-block", h3("Combined Data:")),div(style="display:inline-block",actionButton("help_combined",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;padding-right:20px;border:none;background:none")),div(style="display:inline-block",downloadButton("combine_download","Download"))),
            DT::dataTableOutput("combine_table"),
            div(div(style="display:inline-block", h3("NOAA PFDS Mean Values:")),div(style="display:inline-block",actionButton("help_noaa_pfds",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;padding-right:20px;border:none;background:none"),div(style="display:inline-block",downloadButton("NOAA_PFDS_download","Download")))),
            htmlOutput("NOAA_site"),
            HTML("<p> \n &nbsp; <p>"), #Add some blank space,
            DT::dataTableOutput("NOAA_PFDS"),
            div(div(style="display:inline-block", h3("USGS PFDS Mean Values:")),div(style="display:inline-block",actionButton("help_usgs_pfds",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;padding-right:20px;border:none;background:none"),div(style="display:inline-block",downloadButton("USGS_PFDS_download","Download")))),
            uiOutput("Box5"),
            HTML("<p> \n &nbsp; <p>"), #Add some blank space,
            DT::dataTableOutput("USGS_PFDS")
     ),
    tabItem(tabName="HOBO",
            googleAuthUI("gauth_login"),
            textOutput("display_username"),
            div(id="auth",
                h3("Please Login to access this tab")
            ),
            HTML("<p> \n &nbsp; <p>"), #Add some blank space
            div(id="HOBO",
                leafletOutput("hobo_map",height=500),
                # div(img(src="HOBO.png",width=1000),style="text-align: center;"), # Add static map of storm sewer sensors
                HTML("<p> \n &nbsp; <p>"), #Add some blank space,
                div(style="display:inline-block",selectizeInput("HOBO_sensor",label=div(div(style="display:inline-block",tags$b("Select Sensors to Map/Plot")),div(style="display:inline-block",actionButton("help_hobo_sensor",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),choices=list("ROA1","ROA2","ROA3","ROA4","ROA5","ROA6","ROA7","ROA8"),selected=c("ROA1","ROA2","ROA3","ROA4","ROA5","ROA6","ROA7","ROA8"),multiple=TRUE,options=list(create=FALSE,placeholder="Select From List Below"))),
                div(style="display:inline-block",uiOutput("Box4")),
                div(style="display:inline-block",selectInput("HOBO_data",label=div(div(style="display:inline-block",tags$b("Storm Sewer Plot Parameter")),div(style="display:inline-block",actionButton("help_hobo_parameter",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),choices=list("Relative Depth"="d_rel","Relative Flow"="Q_rel"),selected=c("Relative Depth"="d_rel"),multiple = FALSE, selectize = TRUE)),
                hr(),
                HTML("<p> \n &nbsp; <p>"), #Add some blank space,
                uiOutput("HOBOsized_plot"),
                div(div(style="display:inline-block", h3("Storm Sewer Sensor Data:")),div(style="display:inline-block",actionButton("help_hobo_data",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;padding-right:20px;border:none;background:none"),div(style="display:inline-block",downloadButton("hobo_download","Download")))),
                DT::dataTableOutput("HOBO_Table")
            )

    ),
    tabItem(tabName="Flood",
            div(div(style="display:inline-block", h3("Real-Time Flood Stages:")),div(style="display:inline-block",actionButton("help_real_flood",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;padding-right:20px;border:none;background:none"))),
            div(style="text-align:center",
                div(style="display:inline-block",plotOutput("gh_lick_run",width=300)),
                div(style="display:inline-block",plotOutput("gh_tinker",width=300)),
                div(style="display:inline-block",plotOutput("gh_rr1",width=300)),
                div(style="display:inline-block",plotOutput("gh_rr2",width=300)),
                div(style="display:inline-block",plotOutput("gh_rr3",width=300))
            )
    ),
    
    tabItem(tabName="Help",
            div(style="display:inline-block", h3("Download User Manual:")),
            div(style="display:inline-block",downloadButton("manual_download","Download")),
            HTML('<b> <p> \n Help Buttons, indicated by "?" symbols, are placed throughout the SHARKS app to provide information regarding the various inputs, outputs, and options. <p> <b>'),
            HTML("<b> <p> \n For questions, please contact the app developers at cbrendel@vt.edu <p> <b>")
    )
  )
)

# Combine sidebar and body into a dashboardPage
ui=dashboardPage(title="SHARKS",
  dashboardHeader(title=div(tags$a(tags$img(src='Shark.png', height='55', width='60'),HTML('</span> <font color="white" size="5"> <b> SHARKS </b>  </font> ')))),
  sidebar,
  body
)


### Define Server Logic __________________________________________________________________________________________________________________________
server=function(input, output,session) {
  ### Workaround to avoid shinyaps.io URL problems
  observe({
    if (rv$login) {
      shinyjs::onclick("gauth_login-googleAuthUi",
                       shinyjs::runjs("window.location.href = 'removed_redirect_URL';")) # Add redirect URL here if you add client id and secret to retrieve storm sewer data stored in Google Sheets
    }
  })
  
  ### Reactive User Interface
  # Update DateRange so you can't select Start Date prior to End Date or End date prior to Start Date
  dates=reactiveValues(start_cur=0,end_cur=0) # Create list to store reactive values in and assign values an initial value
  
  observeEvent(input$PrecipDate,{
    # Set "old" start and end dates to date before clicking
    dates$start_old=dates$start_cur
    dates$end_old=dates$end_cur
    
    # Set "current" start and end dates to values in input
    dates$start_cur=input$PrecipDate[1]
    dates$end_cur=input$PrecipDate[2]
    
    # If start date was changed, then update end date if end date is prior to start date
    if(dates$start_old!=dates$start_cur&input$PrecipDate[1]>input$PrecipDate[2]){
        updateDateRangeInput(session,"PrecipDate", end=input$PrecipDate[1])
    }
    
    # If end date was changed, then update start date if end date is prior to start date
    if(dates$end_old!=dates$end_cur&input$PrecipDate[2]<input$PrecipDate[1]){
        updateDateRangeInput(session,"PrecipDate", start=input$PrecipDate[2])
    }
  })
  
  # Select which rain gauge to plot on hyetograph
  #  -if user enters a precip site that isn't in the key, then return option based on ID name
  plot_site=reactive({
    if(is.null(unlist(precip_site_key[input$user.faaid],use.names = F))==TRUE){
      choices=unlist(input$user.faaid)
    }
    else{
      choices=unlist(precip_site_key[input$user.faaid],use.names = F)
    }
    for(site in 1:length(input$USGS_Precip)){
      if(is.null(unlist(precip_site_key[input$USGS_Precip[site]],use.names = F))==TRUE){
        choices=c(choices,input$USGS_Precip[site])
      }
      else{
        choices=c(choices,unlist(precip_site_key[input$USGS_Precip[site]],use.names = F))
      }
    }
    choices
  })
  
  # - create variable for hyetograph to determine which data to retrieve
  plot_site2=reactive({
      if(is.null(unlist(precip_site_key2[input$user.gauge],use.names = F))==TRUE){
        choice=input$user.gauge
      }
      else{
        choice=c(unlist(precip_site_key2[input$user.gauge],use.names = F))
      }
  })
  
  # - create variable for HOBO hyetograph to determine which data to retrieve
  plot_site3=reactive({
    if(is.null(unlist(precip_site_key2[input$HOBO.gauge],use.names = F))==TRUE){
      choice=input$user.gauge
    }
    else{
      choice=c(unlist(precip_site_key2[input$HOBO.gauge],use.names = F))
    }
  })
  
  # Select which precipitation gauge to plot on hyetograph
  output$Box1=renderUI(selectInput("user.gauge",label=div(div(style="font-size:115%;display:inline-block",tags$b("Plot Precipitation Gauge")),div(style="display:inline-block",actionButton("help_plot_precip",label="",icon=icon("question"),style="height:0px;width:0px;padding:0px;border:none;background:none"))), choices=plot_site(), multiple=FALSE,selected=plot_site()[2])) # Sets default plot gauge to the first USGS precip site. To set default to NOAA gauge then set select=input$user.faaid
  
  # Select which water quality parameter to plot on secondary axis
  output$Box2=renderUI(selectInput("user.wq",label=div(div(style="font-size:115%;display:inline-block",tags$b("Plot Parameter")),div(style="display:inline-block",actionButton("help_plot_parameter",label="",icon=icon("question"),style="height:0px;width:0px;padding:0px;border:none;background:none"))),choices=c("None",unlist(wq_key[input$parameterCd],use.names=F)),multiple=FALSE,selected="None"))
  
  # Input Watersed Area and choose all watersheds
  output$Box3=renderUI(selectizeInput("WS_Area_ac", label=div(div(style="font-size:115%;display:inline-block",tags$b("Watershed Area (Acres)")),div(style="display:inline-block",actionButton("help_area",label="",icon=icon("question"),style="height:0px;width:0px;padding:0px;border:none;background:none"))), choices=unlist(watershed_area_key[input$USGS_Site],use.names=F),multiple=TRUE,options=list(create=TRUE,placeholder="Enter Area")))

  observe({
    selected_choices=unlist(watershed_area_key[input$USGS_Site],use.names=F)
    updateSelectizeInput(session,inputId="WS_Area_ac",selected=selected_choices)
  })
  
  # Choose which Precip Station to graph with HOBO data
  output$Box4=renderUI(selectInput("HOBO.gauge",label=div(div(style="display:inline-block",tags$b("Storm Sewer Plot Precipitation Gauge")),div(style="display:inline-block",actionButton("help_hobo_precip",label="",icon=icon("question-circle"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))), choices=plot_site(), multiple=FALSE,selected=plot_site()[2])) # Sets default plot gauge to the first USGS precip site. To set default to NOAA gauge then set select=input$user.faaid
  
  # Choose which Precip Station to get PFDS
  options=reactive({
    sites=as.list(input$USGS_Precip)
    unlist(rapply(sites,function(x)ifelse(x%in%names(precip_site_key),unlist(precip_site_key[x],use.names=F),x),how="replace")) #rename list elements with location description if the sites are in Roanoke
  }) 

  
  output$Box5=renderUI(selectInput("counter",label=tags$b("Select USGS Precipitation Gauge"),choices=options()))
  
  # Show/Hide ARI Summary
  observe({
    if(input$user.sw==0){
      shinyjs::hide(id="ARI")
    }
    else{
      shinyjs::show(id="ARI")
    }
  })
  
  # Show/Hide Extra Settings
  observe({
    if(input$show_hide==1){
      shinyjs::show(id="extra")
      }
    else{
      shinyjs::hide(id="extra")
      }
  })
  
  # Show/Hide FAA ID Table
  observe({
    if(is.null(input$user.faaid)||input$user.faaid==""){
      shinyjs::show(id="FAA")
      shinyjs::hide(id="Summary_Tab")
    }
    else{
      shinyjs::hide(id="FAA")
      shinyjs::show(id="Summary_Tab")
    }
  })
  
  # Switch between Interactive Plots
  observe({
    if(input$interact==1){
      shinyjs::show(id="hydro_int")
      shinyjs::hide(id="hyet_int")
    }
    else{
      shinyjs::hide(id="hydro_int")
      shinyjs::show(id="hyet_int")
    }
  })
  
  # Show/Hide Authentification Message and HOBO Stuff
  observe({
    if(rv$login==F){
      shinyjs::show(id="auth")
      shinyjs::hide(id="HOBO")
    }
    else{
      shinyjs::hide(id="auth")
      shinyjs::show(id="HOBO")
    }
  })
  
  # Show/Hide Flood Stage Plots
  observe({
    if("00065"%in%input$parameterCd){
      if(any(c("0205551460","02055000","02055100","02054530","02054500")%in%input$USGS_Site)==F){
        shinyjs::hide(id="flood")
      } else{
        shinyjs::show(id="flood")
      }
      
      if("0205551460"%in%input$USGS_Site){
        shinyjs::show(id="lick_run")
      } else{shinyjs::hide(id="lick_run")}
      
      if("02055000"%in%input$USGS_Site){
        shinyjs::show(id="rr1")
      } else{shinyjs::hide(id="rr1")}
      
      if("02055100"%in%input$USGS_Site){
        shinyjs::show(id="tinker")
      } else{shinyjs::hide(id="tinker")}
      
      if("02054530"%in%input$USGS_Site){
        shinyjs::show(id="rr2")
      } else{shinyjs::hide(id="rr2")}
      
      if("02054500"%in%input$USGS_Site){
        shinyjs::show(id="rr3")
      } else{shinyjs::hide(id="rr3")}
    } 
    else{
      shinyjs::hide(id="flood")
    }
  })
  
  ### Define Functions
  
  # Define Setnames function to update column names even if certain column names are absent
    Setnames= function(x, old, new, allow.absent.cols=F) {
      if (!allow.absent.cols) {
        setnames(x, old, new)
      } 
      else {
        old.intersect=intersect(old, names(x))
        common.indices=old %in% old.intersect
        new.intersect=new[common.indices]
        setnames(x, old.intersect, new.intersect)
      }
    }
  
  # Define SubsetCols to take a Subset of data table even if certain column names are absent
    SubsetCols= function(x, col_names, allow.absent.cols=F) {
      if (!allow.absent.cols) {
        subset(x, select=col_names)
      } 
      else {
        col_names.intersect=intersect(col_names, names(x))
        subset(x, select=col_names.intersect)
      }
    }
    
  ### Create Keys
    
  # USGS Flow Sites
  flow_site_key=vector(mode="list",length=10)
  names(flow_site_key)=c("0205551460","02055000","02054530","02054500","02055100")
  flow_site_key[[1]]="Lick Run";flow_site_key[[2]]="Roanoke River at Roanoke";flow_site_key[[3]]="Roanoke River at Glenvar";flow_site_key[[4]]="Roanoke River at Lafayette";flow_site_key[[5]]="Tinker Creek"
  
  # USGS Flow Site Watershed Area
  watershed_area_key=vector(mode="list",length=2)
  names(watershed_area_key)=c("0205551460","02055000")
  watershed_area_key[[1]]="Lick Run (3,544)";watershed_area_key[[2]]="Roanoke River at Roanoke (245,683)"
  
  watershed_area_key2=vector(mode="list",length=2)
  names(watershed_area_key2)=c("Lick Run (3,544)","Roanoke River at Roanoke (245,683)")
  watershed_area_key2[[1]]=(154376640/43560);watershed_area_key2[[2]]=(10701969855/43560)

  # USGS Precip Sites
  precip_site_key=vector(mode="list",length=10)
  names(precip_site_key)=c("0205551460","371840079534900","371824080002600","371709079580800","371657080002800","371518079591700","371339079554400","371520080015100","371459079560300","ROA")
  precip_site_key[[1]]="Lick Run";precip_site_key[[2]]="Roanoke Centre for Industry & Technology (RCIT)";precip_site_key[[3]]="Peters Creek Road";precip_site_key[[4]]="Roanoke Fire-EMS Station #5";precip_site_key[[5]]="Strauss Park";precip_site_key[[6]]="Shrine Hill Park";precip_site_key[[7]]="Garden City Elementary School";precip_site_key[[8]]="Hidden Valley Middle School";precip_site_key[[9]]="Mill Mountain";precip_site_key[[10]]="Roanoke Regional Airport"
  
  precip_site_key2=vector(mode="list",length=10)
  names(precip_site_key2)=c("Lick Run","Roanoke Centre for Industry & Technology (RCIT)","Peters Creek Road","Roanoke Fire-EMS Station #5","Strauss Park","Shrine Hill Park","Garden City Elementary School","Hidden Valley Middle School","Mill Mountain","Roanoke Regional Airport")
  precip_site_key2[[1]]="0205551460";precip_site_key2[[2]]="371840079534900";precip_site_key2[[3]]="371824080002600";precip_site_key2[[4]]="371709079580800";precip_site_key2[[5]]="371657080002800";precip_site_key2[[6]]="371518079591700";precip_site_key2[[7]]="371339079554400";precip_site_key2[[8]]="371520080015100";precip_site_key2[[9]]="371459079560300";precip_site_key2[[10]]="ROA"

  # USGS Parameter Codes
  combine_key=vector(mode="list", length=7)
  names(combine_key)=c("00065","00010","00095","00300","00400","63680","00060")
  combine_key[[1]]="GH_Inst";combine_key[[2]]="Wtemp_Inst";combine_key[[3]]="SpecCond_Inst";combine_key[[4]]="DO_Inst";combine_key[[5]]="pH_Inst";combine_key[[6]]="Turb_Inst";combine_key[[7]]="Flow_Inst"

  # Names of USGS Parameter Codes
  wq_key=vector(mode="list",length=6)
  names(wq_key)=c("00065","00010","00095","00300","00400","63680")
  wq_key[[1]]="Gauge Height (ft)";wq_key[[2]]="Water Temperature (C)";wq_key[[3]]="Specific Conductance (microS/cm @ 25 C)";wq_key[[4]]="Dissolved Oxygen (mg/L)";wq_key[[5]]="pH";wq_key[[6]]="Turbidity (FNU)"
  
  # Names of renamed USGS Columns
  wqkey=vector(mode="list",length=6)
  names(wqkey)=c("Wtemp_Inst","SpecCond_Inst","DO_Inst","pH_Inst","Turb_Inst","GH_Inst")
  wqkey[[1]]="Water Temperature (C)";wqkey[[2]]="Specific Conductance (microS/cm @ 25 C)";wqkey[[3]]="Dissolved Oxygen (mg/L)";wqkey[[4]]="pH";wqkey[[5]]="Turbidity (FNU)";wqkey[[6]]="Gauge Height (ft)"
  
  # Key to go from input$user.wq to plot water quality parameter
  plot_key=vector(mode="list",length=6)
  names(plot_key)=c("Water Temperature (C)","Specific Conductance (microS/cm @ 25 C)","Dissolved Oxygen (mg/L)","pH","Turbidity (FNU)","Gauge Height (ft)")
  plot_key[[1]]="Wtemp_Inst";plot_key[[2]]="SpecCond_Inst";plot_key[[3]]="DO_Inst";plot_key[[4]]="pH_Inst";plot_key[[5]]="Turb_Inst";plot_key[[6]]="GH_Inst"
  
  # Key to go from input$user.wq to hydrograph legend
  hydro_legend_key=vector(mode="list",length=6)
  names(hydro_legend_key)=c("Water Temperature (C)","Specific Conductance (microS/cm @ 25 C)","Dissolved Oxygen (mg/L)","pH","Turbidity (FNU)","Gauge Height (ft)")
  hydro_legend_key[[1]]="Water Temperature";hydro_legend_key[[2]]="Specific Conductance";hydro_legend_key[[3]]="Dissolved Oxygen";hydro_legend_key[[4]]="pH";hydro_legend_key[[5]]="Turbidity";hydro_legend_key[[6]]="Gauge Height"
  
  # Key for HOBO plot y-axis
  HOBO_key=vector(mode="list",length=2)
  names(HOBO_key)=c("d_rel","Q_rel")
  HOBO_key[[1]]="Relative Depth";HOBO_key[[2]]="Relative Flow"
  
  ### Create Help Messages
  
  # Date Range Input
  observeEvent(input$help_dates,{shinyalert("Date Range Input",
                                          type="info",
                                          text="This input controls the start and end dates of the date range for which data is downloaded and displayed.

                                            The app is dynamically linked to the date range and will immediately retrieve data upon changes to the date range. Thus, if users wish to select a date range prior to the current date range, then users should first adjust the end date and then adjust the start date. Otherwise, the app will attempt to retrieve data from the new start date to the original end date. Likewise, if users wish to select a date range after the current date range, then users should first adjust the start date and then adjust the end date."
                                          )})
  # ASOS Station
  observeEvent(input$help_state,{shinyalert("ASOS Station State",
                                          type="info",
                                          text='This input is used to select the state in which the desired NOAA ASOS station is located. Select a state from the dropdown menu and then select an ASOS meteorological station using the "ASOS Station FAA ID" input.'
                                          )})
  # ASOS FAA ID
  observeEvent(input$help_faaid,{shinyalert("ASOS Station FAA ID",
                                          type="info",
                                          text='This input allows users to specify the airport FAA ID for the ASOS station they wish to retreive meteorological data from by typing the ID into the input box. Characters must be either captial letters or numbers
                                            
                                            Leaving this input empty will display a table on the SHARKS Summary Tab with the location and FAA ID of every ASOS station within the state selected using the "ASOS Station State" input.'
                                          )})
  # USGS Precip
  observeEvent(input$help_usgs_precip,{shinyalert("USGS Precip. Station IDs",
                                          type="info",
                                          text="This input allows users to select the USGS meteorological station they wish to retrieve precipitation data from. The nine USGS stations located in Roanoke are pre-loaded into SHARKS and can be selected by clicking the station description in the drop-down list. A map depicting the spatial location of these stations is included on the SHARKS Summary Tab.
                                            
                                            Precipitation data can also be retrieved from USGS stations located outside of Roanoke by entering the USGS ID number of the desired station in the input box."
                                          )})
  # USGS Flow
  observeEvent(input$help_usgs_flow,{shinyalert("USGS Stream Station IDs",
                                          type="info",
                                          text="This input allows users to select the USGS stream station they wish to retrieve discharge and/or water quality data from. The Lick Run and Roanoke River USGS stations located in Roanoke are pre-loaded into SHARKS and can be selected by clicking the station description in the drop-down list.
                                                  
                                            Discharge/Water Quality data can also be retrieved from USGS stations located outside of Roanoke by entering the USGS ID number of the desired station in the input box."
                                          )})
  # Watershed Area
  observeEvent(input$help_area,{shinyalert("Watershed Area",
                                          type="info",
                                          text='The watershed area is used to calculate the depth of direct runoff for each USGS stream flow station. If the user selects one of the Roanoke streamflow stations, then the watershed area input automatically populates. However, if users have manually entered a station in the "USGS Stream Station IDs" input, then they must also manually enter the watershed area in acres for the station. The order of the stations in the "USGS Stream Station IDs" input must match the order of the areas in the "Watershed Area" input.'
                                          )})
  # Download Parameters
  observeEvent(input$help_parameters,{shinyalert("Download Parameters",
                                          type="info",
                                          text="This input allows users to select which datasets to retrieve for the specified USGS stream stations. Available parameters are Discharge (cfs), Gauge Height (ft), Water Temperature (Degrees C), Specific Conductance (micro S/cm @ 25 Degrees C), Dissolved Oxygen (mg/L), pH, and Turbidity (FNU). Parameters are selected by clicking the parameter description in the drop-down list and are removed by clicking the name of a selected parameter and then pressing the backspace or delete keys."
                                          )})
  # Calculate ARI
  observeEvent(input$help_ari,{shinyalert("Calculate ARI",
                                          type="info",
                                          text='This switch allows users to determine whether SHARKS will calculae the average annual recurrence interval (ARI) for precipitation events occuring during the specified date range. If the switch is set to "Yes", then the ARI Summary table will appear above the hyetograph/hydrograph on the SHARKS Summary Tab.This table table summarizes the location, station ID, ARI, event duration, event start time, precipitation depth, and precipitation intensity of all events with an ARI >1 year occurring during the specified date range'
                                          )})
  # Plot Precipitation Gauge
  observeEvent(input$help_plot_precip,{shinyalert("Plot Precipitation Gauge",
                                          type="info",
                                          text="This option allows users to specify the source of data to be plotted in the hyetograph in the combined hyetograph/hydrograph on the Summary Tab and the interactive hyetograph on the Interactive Plots Tab. Clicking the drop-down menu displays a list of all of the selected NOAA and USGS precipitation stations and clicking the desired station name updates all of the aforementioned hyetographs."
                                          )})
  # Plot Parameter
  observeEvent(input$help_plot_parameter,{shinyalert("Plot Parameter",
                                          type="info",
                                          text="This option option allows users to specify which water quality parameter, if any, is displayed on a secondary axis on the hydrograph in the combined hyetograph/hydrograph on the Summary Tab and in the interactive hydrograph on the Interactive Plots Tab. Clicking the drop-down menu displays a list of all of the downloaded water quality parameters. "
                                          )})
  # Time Zone
  observeEvent(input$help_time,{shinyalert("Time Zone",
                                          type="info",
                                          text="This setting controls how the NOAA and USGS precipitation, stream flow, and water quality data are downloaded and displayed.
                                           
                                            The NOAA and USGS data sources used in SHARKS handle time zones differently. While NOAA returns data for stations from 0:00-24:00 in the specified time zone, USGS returns data for stations from 0:00-24:00 in the local time zone of the station and the data must then be transposed to match the specified time zone. Thus, if the user specifies a time zone that is different than the local time of the USGS stations, the downloaded time periods for the NOAA and USGS data will be offset by the time difference between the specified time zone and the local time zone of the USGS station.
                                           
                                            The NOAA and USGS data sources used in SHARKS also handle Daylight Saving Time differently. During Daylight Savings Time, NOAA returns data from 0:00 on the specified start date to 24:00 on the specified end date. However, when Daylight Savings Time ends, NOAA returns data from 23:00 the day before the specified start date to 23:00 on the specified end date. In contrast, USGS returns data from 0:00 on the specified start date to 24:00 on the specified end date both during Daylight Savings Time and when Daylight Savings Time has ended. Thus, the hyetograph and hydrographs may also appear offset when Daylight Savings Time ends, despite being plotted correctly, because NOAA and USGS return data for slightly different time periods."
                                          )})
  # NOAA Network
  observeEvent(input$help_network,{shinyalert("NOAA Network",
                                          type="info",
                                          text="Currently, the service SHARKS uses to retrieve NOAA data only supports ASOS stations. However, this option is included in SHARKS to allow for integration of NOAA AWOS data if it becomes available."
                                          )})
  # Plot Colors
  observeEvent(input$help_color,{shinyalert("Plot Colors By",
                                          type="info",
                                          text="This input allows users to set how the colors are displayed for the datasets in the combined hyetograph/hydrograph on the Summary Tab and the interactive hydrograph on the Interactive Plots Tab.
                                           
                                           If the input is set to plot colors by station, then the same color will be used for all datasets from the same station. However, if the input is set to plot colors by dataset, then a different color will be used for each dataset on the plot."
                                          )})
  # Data Summary
  observeEvent(input$help_data_summary,{shinyalert("Data Summary",
                                          type="info",
                                          text="This table displays an overview of the selected NOAA meteorological station, USGS meteorological stations, and USGS stream flow stations. Parameters displayed in the table include the station location and the station ID. For meteorological stations, the total precipitation depth (inches) and the maximum incremental precipitation intensity (inches/hour) are displayed. For stream flow stations, the maximum flow rate (cubic feet/second), and total direct runoff volume (cubic feet) and total direct runoff depth (inches) are displayed. Data in the table can be sorted by clicking the column titles"
                                          )})
  # Runoff Volume Coefficient Summary
  observeEvent(input$help_rv,{shinyalert("Runoff Volume Coefficient Summary",
                                          type="info",
                                          text="This table displays the runoff volume coefficients calculated for each stream flow and meteorological station combination. Runoff volume coefficients represent the proportion of precipitation leaving the watershed and are calculated as the direct runoff depth calculated for the stream flow station divided by the total precipitation depth measured at the meteorological station. Data in the table can be sorted by clicking on the column titles."
                                          )})
  # Hyetograph/Hydrograph
  observeEvent(input$help_hh,{shinyalert("Hyetograph & Hydrograph",
                                          type="info",
                                          text='The combined hyetograph/hydrograph is controlled by the inputs and options located in the SHARKS sidebar menu. While the hyetograph only displays data from the station specified in the Plot Precipitation Gauge option for the date range specified in the "Date Range" input, the hydrograph displays all available data for the streamflow stations specified in the USGS Stream Station IDs input. The Plot Parameter option can be used to graph Gauge Height (ft), Water Temperature (degrees C), Specific Conductance (micro S/cm @ 25 degrees C), Dissolved Oxygen (mg/L), pH, and Turbidity (FNU) on a secondary y-axis on the hydrograph. If data for the parameter selected using the Plot Parameter option is unavailable, a message is displayed in the secondary y-axis indicating that the data is unavailable. The colors displayed for the datasets in the combined hyetograph/hydrograph can be set using the hidden setting, Plot Colors By.'
                                          )})
  # ARI Summary
  observeEvent(input$help_ari_table,{shinyalert("ARI Summary",
                                          type="info",
                                          text='This table displays the average annual recurrence intervals (ARI) calculated for precipitation events occurring during the specified date range. The data table summarizes the location, station ID, ARI, event duration, event start time, precipitation depth, and precipitation intensity of all events with an ARI >1 year occurring during the specified date range. Data can be sorted in the table by clicking the column titles and the "Display" toggle switch allows the user to either display only the maximum ARI recorded at each station or every calculated ARI for the specified date range. The table can be exported as a CSV file by clicking the "Download" button.'
                                          )})
  
  # Forecast Location
  observeEvent(input$help_forecast,{shinyalert("Forecast Location",
                                          type="info",
                                          text="This input specifies the location for which the current weather conditions and the 10-day weather forecast are retrieved."
                                          )})
  # Interactive Plots Switch
  observeEvent(input$help_interact_switch,{shinyalert("Interactive Plots Hydrograph/Hyetograph Switch",
                                          type="info",
                                          text="This switch allows you to toggle between an interactive hydrograph and an interactive hyetograph. The hydrograph and hyetograph are controlled by the inputs and options located in the SHARKS sidebar menu and displays the same information as the combined hyetograph/hydrograph on the Summary Tab."
                                          )})
  
  # Interactive Hydrograph Clicked Data
  observeEvent(input$help_hydro_click,{shinyalert("Clicked Data",
                                          type="info",
                                          text='Users can click on any point on the Discharge dataset on the above hydrograph and data from the nearest point will be displayed in this section. If Discharge data is unavailable, users should utilize the "Brushed Data" feature as the "Clicked Data" feature is not supported.'
                                          )})
  # Interactive Hydrograph Volume Summary
  observeEvent(input$help_hydro_vol,{shinyalert("Volume Summary",
                                          type="info",
                                          text='Users can "Brush" data in the hydrograph by clicking and dragging a box around the desired data. The total discharge volume, baseflow volume, and stormflow runoff volume for each stream flow station for the "Brushed" time period, calculated via trapezoidal integration, is displayed in this table. Data within this table can be sorted by clicking on the column titles. 
                                          
                                          The "Brushed" time period and the data in the table can be refined by either drawing a new box or by resizing the box by hovering the cursor on the edges of the box until a double-sided arrow appears and then clicking and dragging the edges. The box can also be moved by clicking and dragging the middle of the box.'
                                          )})
  # Interactive Hydrograph Brushed Data
  observeEvent(input$help_hydro_brush,{shinyalert("Brushed Data",
                                          type="info",
                                          text='Users can "Brush" data in the hydrograph by clicking and dragging a box around the desired data. The discharge and water quality data for each stream flow station for the "Brushed" time period is displayed in this table. Data within this table can be sorted by clicking on the column titles and the table can be exported as a CSV file by clicking the "Download" button. 
                                          
                                          The "Brushed" time period and the data in the table can be refined by either drawing a new box or by resizing the box by hovering the cursor on the edges of the box until a double-sided arrow appears and then clicking and dragging the edges. The box can also be moved by clicking and dragging the middle of the box.'
                                          )})
  # Interactive Hydrograph Brushed Data
  observeEvent(input$help_hyet_brush,{shinyalert("Brushed Data",
                                          type="info",
                                          text='The interactive hyetograph displays data from the station selected using "Plot Precipitation Gauge" option in the SHARKS sidebar menu. Users can "Brush" data in the hyetograph by clicking and dragging a box around the desired data. The incremental precipitation data for the selected station for the "Brushed" time period is displayed in this table. Data within this table can be sorted by clicking on the column titles and the table can be exported as a CSV file by clicking the "Download" button.
                                          
                                          The duration of the "Brushed" time period is displayed below the interactive hyetograph as well as the total precipitation depth, average precipitation intensity, and maximum precipitation intensity occurring during the period. Furthermore, the date/time(s) in which the maximum precipitation intensity occurred are displayed after the maximum precipitation intensity value. The average precipitation intensity is calculated as the total precipitation depth occurring during the period divided by the duration of the period.
                                          
                                          The "Brushed" time period and the data in the table can be refined by either drawing a new box or by resizing the box by hovering the cursor on the edges of the box until a double-sided arrow appears and then clicking and dragging the edges. The box can also be moved by clicking and dragging the middle of the box.'
                                          )})
  # Combined Data Table
  observeEvent(input$help_combined,{shinyalert("Combined Data",
                                          type="info",
                                          text='The combined data table presents all of the time series data for all NOAA and USGS stations in one table. The time series data are joined by date/time and each time series is displayed in its own column. Column names represent the station ID, dataset, and data units. Data in the table can be sorted by clicking on the column titles. The data table can also be downloaded as a CSV file by clicking the "Download" button.'
                                          )})
  # NOAA PFDS Table
  observeEvent(input$help_noaa_pfds,{shinyalert("NOAA PFDS Mean Values",
                                          type="info",
                                          text='The NOAA PFDS Mean Values table displays the NOAA Atlas 14 point precipitation frequency estimates for the ASOS precipitation station specified using the "ASOS Station State" and "ASOS Station FAA ID" inputs in the SHARKS sidebar menu. The PFDS table is retrieved from NOAA and displays the precipitation depth (inches) corresponding to different storm durations and average annual recurrence intervals (ARI). The table can be exported as a CSV file by clicking the "Download" button.'
                                          )})
  # USGS PFDS Table
  observeEvent(input$help_usgs_pfds,{shinyalert("USGS PFDS Mean Values",
                                          type="info",
                                          text='The USGS PFDS Mean Values table displays the NOAA Atlas 14 point precipitation frequency estimates for all USGS precipitation stations specified using the "USGS Precip. Station IDs" input in the SHARKS sidebar menu. The PFDS table is retrieved from NOAA and displays the precipitation depth (inches) corresponding to different storm durations and average annual recurrence intervals (ARI). Users can specify which USGS PFDS table to display by selecting the station name from the dropdown list above the table and the table can be exported as a CSV file by clicking the "Download" button.'
                                          )})
  # HOBO Sensor
  observeEvent(input$help_hobo_sensor,{shinyalert("Select Sensors to Map/Plot",
                                          type="info",
                                          text="This input allows the user to specify which of the eight downtown Roanoke level loggers to display data for. Users can choose to display data for any combination of the eight sensors. "
                                          )})
  # HOBO Parameter
  observeEvent(input$help_hobo_parameter,{shinyalert("Storm Sewer Plot Parameter",
                                          type="info",
                                          text="This option allows users to either display the flow rate or flow depth relative to those of full-flow conditions."
                                          )})
  # HOBO Precipitation Gauge
  observeEvent(input$help_hobo_precip,{shinyalert("Storm Sewer Plot Precipitation Gauge",
                                          type="info",
                                          text="Users can display the hyetograph for any of the meteorological stations specified in the SHARKS sidebar menu by selecting the station name from the dropdown list."
                                          )})
  # HOBO Data
  observeEvent(input$help_hobo_data,{shinyalert("Storm Sewer Sensor Data",
                                          type="info",
                                          text='This table displays the sensor stage, relative depth (d_rel), and relative flow rate (Q_rel) for each of the sensors specified using the "Select Sensors to Plot" input for the date range specified using the "Date Range" input in the SHARKS sidebar menu. Data in the table can be sorted by clicking the column titles and the data table can be exported as a CSV file by clicking the "Download" button.'
                                          )})
  # FAA ID Table
  observeEvent(input$help_faaid_table,{shinyalert("FAA IDs for selected State",
                                          type="info",
                                          text='This table displays the location and FAA ID of every NOAA ASOS meteorological station in the state specified using the "ASOS Station State" input in the SHARKS sidebar menu. The FAA ID for an ASOS station can be entered in the "ASOS Station FAA ID" input in the sidebar menu to retrieve precipitation data from that station.'
  )})
  
  # Flood Stages
  observeEvent(input$help_flood,{shinyalert("Maximum Flood Stages",
                                          type="info",
                                          text='These plots display the maximum gauge height for Roanoke stream stations during the specified date range relative to the National Weather Service flood thresholds. The bar chart data labels indicate the maximum gauge height (ft) recorded during the specified date range as well as the first date/time in which that height was recorded.'
  )})
  
  # Real Time Flood Stages
  observeEvent(input$help_real_flood,{shinyalert("Real-Time Flood Stages",
                                          type="info",
                                          text='These plots display the most recent gauge height measurements for Roanoke stream stations relative to the National Weather Service flood thresholds. The bar chart data labels indicate the most recent gauge heights (ft) recorded as well as the date/time in which they were recorded.'
  )})
  
  
  ### 1) Download Data from ISU Mesonet ----------------------------------------------------------------------------------------------------------
  ## Web GUI here: https://mesonet.agron.iastate.edu/request/download.phtml?network=VA_ASOS
  ## Adapted from https://github.com/realmiketalbot/R-scripts/blob/master/iem_scraper_example.r
  
  # Convert Input Start Date
  date1=reactive({
    ISOdate(as.integer(strsplit(toString(input$PrecipDate[1]),"-")[[1]])[1], as.integer(strsplit(toString(input$PrecipDate[1]),"-")[[1]])[2], as.integer(strsplit(toString(input$PrecipDate[1]),"-")[[1]])[3])
  })
  
  # Convert Input End Date
  date2=reactive({
    ISOdate(as.integer(strsplit(toString(input$PrecipDate[2]),"-")[[1]])[1], as.integer(strsplit(toString(input$PrecipDate[2]),"-")[[1]])[2], as.integer(strsplit(toString(input$PrecipDate[2]),"-")[[1]])[3])
  })
  
  # Create URL to access data
  service=reactive({
    str_c("https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?","data=all&tz=",
          input$user.tz,
          "&format=comma&latlon=yes&",
          "year1=", as.integer(strsplit(toString(input$PrecipDate[1]),"-")[[1]])[1], 
          "&month1=",as.integer(strsplit(toString(input$PrecipDate[1]),"-")[[1]])[2],
          "&day1=",as.integer(strsplit(toString(input$PrecipDate[1]),"-")[[1]])[3],
          "&",
          "year2=",as.integer(strsplit(toString(input$PrecipDate[2]),"-")[[1]])[1],
          "&month2=",as.integer(strsplit(toString(input$PrecipDate[2]),"-")[[1]])[2],
          "&day2=",as.integer(strsplit(toString(input$PrecipDate[2]),"-")[[1]])[3],
          "&",
          sep="")
  })
  
  # Create string of states
  states= c("AK AL AR AZ CA CO CT DE FL GA ")
  states= str_c(states,"HI IA ID IL IN KS KY LA MA MD ")
  states= str_c(states,"ME MI MN MO MS MT NC ND NE NH ")
  states= str_c(states,"NJ NM NV NY OH OK OR PA RI SC ") 
  states= str_c(states,"SD TN TX UT VA VT WA WI WV WY")
  
  # Split string of states into character list
  states=unlist(strsplit(states, " "))
  
  # Creates list of state abbreviations with ASOS ending; ex. "VA_ASOS"
  networks="AWOS"
  for (i in 1:length(states)) {
    networks[i+1] = str_c(states[i], "_ASOS", sep="")
  }
  
  # Grab the network for the state entered by the user; ex. selectes "VA_ASOS" from list
  network_type=reactive({
    if (as.double(input$user.network) == 1){
      network_type="ASOS"
    }
  })

  selected_network=reactive(networks)
  selected_network=reactive({
    if (as.double(input$user.network) == 1){
      selected_network=networks[which(networks %in% str_c(input$user.state, "_", network_type()))]
    } else {
      selected_network=subset(networks %in% str_c(network_type()))
    }
  })
  
  ### Retrieve data for the specified airport site
  
  #Get Metadata
  jdict=reactive(jsonlite::fromJSON(url(str_c("https://mesonet.agron.iastate.edu/geojson/network/", selected_network(), ".geojson", sep=""))))
  
  # Get List of Stations if FAA ID field is Blank
  Site=reactive({
    if(input$user.faaid == ""){
      jdict()$features$properties[c("sname","sid")]
    }
    else{Site=input$user.faaid}
  })
  
  # Find site matching input FAA ID
  rownum=reactive({which(jdict()$features$properties$sid==input$user.faaid)}) ## this works to get row number of matching site id
  
  # Get Site Name for entered FAA ID
  sitename=reactive({jdict()$features$properties$sname[rownum()]})
  
  # Get URL for data
  uri=reactive({str_c(toString(service()),"station=",toString(input$user.faaid))})
  
  # Retreive data from URL
  iadata=reactive({fread(str_c(uri()))})
  
  ### 2) Parse ISU data and format ---------------------------------------------------------------------------------------------------------------
  DateTime=reactive({as.POSIXct(iadata()$valid, origin = "1970-01-01 00:00.00 UTC")}) # Converts text to date
  DateNumeric=reactive({as.numeric(DateTime())}) # Converts date to numeric

  P_Raw_in=reactive(as.numeric(unlist(strsplit(gsub("NA","0",toString(as.numeric(iadata()$p01i))),",")))) # Gets Precip Data & replaces NA precipitation values with 0
  
  ### The P_Raw_in is cumulative precipitation totals that resets on the hour and sometimes other increments. 
  ### This code extracts incremental depths and calculates total rainfall for the period

  NOAAminute=reactive(minute(DateTime())) # Get Minute for each Date/Time Value

  # Loop to get Incremental Precipitation
  P_Inc_in=reactive({
    P_Inc_value=numeric(length(P_Raw_in()))
    P_Inc_value[1]=P_Raw_in()[1]
    
    for (j in 2:length(P_Raw_in())){
      if (NOAAminute()[j] > NOAAminute()[j-1]){
      #sometimes P_Raw_in resets NOT on the hour...this catches that
        if(P_Raw_in()[j] >= P_Raw_in()[j-1]){
          P_Inc_value[j]=P_Raw_in()[j] - P_Raw_in()[j-1] # If the cumulative precip depth at time=t2 is greater than or equal to the cumulative precip depth at time=t1, incremental precip = difference between the two readings
        } else {
          P_Inc_value[j]=P_Raw_in()[j] # If the cumulative precip depth at time=t2 is less than the cumulative precip depth at time=t1, then the depth reset and incremental precip = raw precip
        }
      }
      else{
        P_Inc_value[j]=P_Raw_in()[j] # If there is no change in time, then incremental precip = raw precip
      }
    }
    P_Inc_value
  })

  
  #  Create data table for NOAA Precipitation Data & Calculate Precipitation Intensity
  
  #  - use as.character for Date/Time to get it to display properly since xtable doesn't play nice with dates/Shiny
  NOAA=reactive({
    NOAA_value=data.table(Precip_Inst=P_Inc_in(),Char_Date=as.character(DateTime()),dateTime=DateTime())
    
    NOAA_value$dt_min=NA
    NOAA_value$dt_min[-1]=diff(NOAA_value$dateTime)
    NOAA_value$i_inhr=NOAA_value$Precip_Inst/(NOAA_value$dt_min / 60)
    
    NOAA_value=subset(NOAA_value,select=c("Char_Date","Precip_Inst","i_inhr"))
    setnames(NOAA_value, old = c("Precip_Inst","i_inhr"), new = c(paste(toString(input$user.faaid),"Precip_Inst",sep="_"),paste(toString(input$user.faaid),"i_inhr",sep="_")))
    NOAA_value
  })

  ### 3) Get USGS Precipitation Data; downloads data from 0:00 local time for the site. time zone parameter (tz) just adjusts the time of the same data
  
  USGS_precip=reactive({
    counter=1
    # Loop through each USGS station
    for(site in input$USGS_Precip){
      # If the start date and end date are't the same, then NOAA does 0:00 Date 1 - 0:00 Date 2 whereas USGS does 0:00 Date 1 - 24:00 Date 2. This sets both to retrieve to 0:00 Date 2   
      if(input$PrecipDate[2]==input$PrecipDate[1]){
        
        # USGS UTC time zone code is different from NOAA UTC time zone code. This addresses that
        if(input$user.tz=="Etc/UTC"){
          USGS_value=renameNWISColumns(readNWISuv(siteNumbers = site, parameterCd = "00045", 
                                                  startDate = input$PrecipDate[1], endDate = input$PrecipDate[2],tz="UTC"))}
        else{
          USGS_value=renameNWISColumns(readNWISuv(siteNumbers = site, parameterCd = "00045", 
                                                  startDate = input$PrecipDate[1], endDate = input$PrecipDate[2],tz=input$user.tz))
        }
      }
      else{
        if(input$user.tz=="Etc/UTC"){
          USGS_value=renameNWISColumns(readNWISuv(siteNumbers = site, parameterCd = "00045",
                                                  startDate = input$PrecipDate[1], endDate = input$PrecipDate[2]-1,tz="UTC"))}
        else{
          USGS_value=renameNWISColumns(readNWISuv(siteNumbers = site, parameterCd = "00045",
                                                  startDate = input$PrecipDate[1], endDate = input$PrecipDate[2]-1,tz=input$user.tz))
        }
      } 
      
      # If data doesn't exist for the USGS gauge, then create an empty dataframe with column names
      if (nrow(USGS_value)==0){USGS_value=data.table(agency_cd="USGS",site_no=paste(toString(site)),dateTime=as.POSIXct(NA, origin = "1970-01-01 00:00.00 UTC"),Precip_Inst=NA)}

      # Add USGS Station Number to Column Names in Data - Use Setnames function defined above in case certain column names are absent
      Setnames(USGS_value, names(select(USGS_value,ends_with("Precip_Inst"))), paste(toString(site),"Precip_Inst",sep="_"),allow.absent.cols=T) # Have to use ends_with to select columns because sometimes renameNWISColumns names stuff as "..2.._Precip_Inst" instead of just "Precip_Inst"
      
      if (counter==1){store_value=USGS_value}
      else{store_value=full_join(x=store_value,y=USGS_value, by="dateTime")} # combine data from each USGS station into one datatable
      
      counter=counter+1
    }
    
    store_value$DateNumeric=as.numeric(store_value$dateTime)
    store_value$Char_Date=as.character(store_value$dateTime)# change dateTime to character so it displays correctly in Shiny table since xtable doesn't play nice
    store_value
    
  })
  
  ### 4) Get USGS Flow & Water Quality Data; downloads data from 0:00 local time for the site. time zone parameter (tz) just adjusts the time of the same data
  
  USGS=reactive({
    counter=1
    # store_value=data.table(site_no=character()) # Want to create different columns for the data from each station; this variable stores the data downloaded from each site
    
    # Loop through each USGS station
    for(site in input$USGS_Site){
      # If the start date and end date are't the same, then NOAA does 0:00 Date 1 - 0:00 Date 2 whereas USGS does 0:00 Date 1 - 24:00 Date 2. This sets both to retrieve to 0:00 Date 2   
      if(input$PrecipDate[2]==input$PrecipDate[1]){
        
        # USGS UTC time zone code is different from NOAA UTC time zone code. This addresses that
        if(input$user.tz=="Etc/UTC"){
          USGS_value=renameNWISColumns(readNWISuv(siteNumbers = site, parameterCd = input$parameterCd, 
                                                  startDate = input$PrecipDate[1], endDate = input$PrecipDate[2],tz="UTC"))}
        else{
          USGS_value=renameNWISColumns(readNWISuv(siteNumbers = site, parameterCd = input$parameterCd, 
                                                  startDate = input$PrecipDate[1], endDate = input$PrecipDate[2],tz=input$user.tz))
        }
      }
      else{
        if(input$user.tz=="Etc/UTC"){
          USGS_value=renameNWISColumns(readNWISuv(siteNumbers = site, parameterCd = input$parameterCd,
                                                  startDate = input$PrecipDate[1], endDate = input$PrecipDate[2]-1,tz="UTC"))}
        else{
          USGS_value=renameNWISColumns(readNWISuv(siteNumbers = site, parameterCd = input$parameterCd,
                                                  startDate = input$PrecipDate[1], endDate = input$PrecipDate[2]-1,tz=input$user.tz))
        }
      } 
      
      # If data doesn't exist for the USGS gauge, then create an empty dataframe with column names
      if (nrow(USGS_value)==0){USGS_value=data.table(agency_cd="USGS",site_no=paste(toString(site)),dateTime=as.POSIXct(NA, origin = "1970-01-01 00:00.00 UTC"))}

      # Add USGS Station Number to Column Names in Data - Use Setnames function defined above in case certain column names are absent
      Setnames(USGS_value, unlist(combine_key[input$parameterCd],use.names=F), paste(toString(site),unlist(combine_key[input$parameterCd],use.names=F),sep="_"),allow.absent.cols=T)

      if (counter==1){store_value=USGS_value}
      else{store_value=full_join(x=store_value,y=USGS_value, by="dateTime")} # combine data from each USGS station into one datatable
      
      counter=counter+1

    }
    
    store_value$DateNumeric=as.numeric(store_value$dateTime)
    store_value$Char_Date=as.character(store_value$dateTime)# change dateTime to character so it displays correctly in Shiny table since xtable doesn't play nice
    store_value
    
  })
  
  ### 5) Combine USGS Data into one Table & Perform Calculations
  combine_table=reactive({
    
    # Combine Data into One Table
    combine_table_value=full_join(x=USGS(),y=USGS_precip(), by="Char_Date")
    
    # Choose Which Columns are Included
    precip_names=names(select(combine_table_value,ends_with("Precip_Inst")))
    col_list=c("Char_Date",precip_names)
    
    for(cd in input$parameterCd){
      col_list=c(col_list,names(select(combine_table_value,ends_with(toString(combine_key[cd],use.names=F)))))
    }

    combine_table_value=subset(combine_table_value,select=col_list)
    
    # Sort Table by Char_Date
    setorder(combine_table_value,Char_Date)

    # Calculate Precipitation Intensity (in/hr)
    for(site in input$USGS_Precip){
      new_col_name=paste(site,"i_inhr",sep="_")
      data_col=paste(site,"Precip_Inst",sep="_")

      dt=subset(combine_table_value,select=c("Char_Date",data_col))
      dt$dt_min=NA
      
      # Calculate time differential if there is gauge data
      if(nrow(dt)>=2){
        for (i in 2:length(dt$Char_Date)){
          dt$dt_min[i]=as.POSIXct(combine_table_value$Char_Date[i], origin = "1970-01-01 00:00.00 UTC")-as.POSIXct(combine_table_value$Char_Date[i-1], origin = "1970-01-01 00:00.00 UTC")
        }
      }

      dt[[new_col_name]]=dt[,data_col]/(dt[,"dt_min"]/ 60)

      combine_table_value[[new_col_name]]=dt[,new_col_name]
    }
    
    # Calculate baseflow using recursive digital filter from Nathan and McMahon (1990)
    counter=1
    for(site in input$USGS_Site){
      col_name=paste(toString(site),"Flow_Inst",sep="_")
      
      # If flow data doesn't exist for site
      if(!col_name%in%colnames(USGS())){
        USGS_Qvalue=data.table(Char_Date=c(as.character(NA)),Flow_Inst=c(as.numeric(NA)),stormflow_cfs=c(as.numeric(NA)),baseflow_cfs=c(as.numeric(NA)),DRO_Vol_cf=c(as.numeric(NA)),DRO_Depth_in=c(as.numeric(NA)))
        Setnames(USGS_Qvalue, c("Flow_Inst","stormflow_cfs","baseflow_cfs","DRO_Vol_cf","DRO_Depth_in"), c(paste(site,"Flow_Inst",sep="_"),paste(site,"stormflow_cfs",sep="_"),paste(site,"baseflow_cfs",sep="_"),paste(site,"DRO_Vol_cf",sep="_"),paste(site,"DRO_Depth_in",sep="_")),allow.absent.cols=T)
      }
      
      # If flow data exists for site
      else{
        # If column exists but all data is NA
        if(all(is.na(USGS()[col_name]))==T){
          USGS_Qvalue=data.table(Char_Date=c(as.character(NA)),stormflow_cfs=c(as.numeric(NA)),baseflow_cfs=c(as.numeric(NA)),DRO_Vol_cf=c(as.numeric(NA)),DRO_Depth_in=c(as.numeric(NA)))
          Setnames(USGS_Qvalue, c("stormflow_cfs","baseflow_cfs","DRO_Vol_cf","DRO_Depth_in"), c(paste(site,"stormflow_cfs",sep="_"),paste(site,"baseflow_cfs",sep="_"),paste(site,"DRO_Vol_cf",sep="_"),paste(site,"DRO_Depth_in",sep="_")),allow.absent.cols=T)
        }
        else{
        USGS_Qvalue=USGS()[complete.cases(USGS()[,paste(toString(site),"Flow_Inst",sep="_")]),]
        USGS_Qvalue=SubsetCols(USGS_Qvalue,c("dateTime",col_name),allow.absent.cols=TRUE)
  
          USGS_Qvalue$DateNumeric=as.numeric(USGS_Qvalue$dateTime)
          USGS_Qvalue$baseflow_cfs=NA; USGS_Qvalue$stormflow_cfs=NA
          passes=3
          
          # Specify alpha values - Nathan & McMahon 1990 range is between 0.90 and 0.95, with 0.95 giving the lowest baseflow
          if(site=="0205551460"){ # Lick Run
            alpha=0.9875
          } else if(site=="02055000"){ # Roanoke River
            alpha=0.9250
          } else{ # All other sites
            alpha=0.925 # value recommended by Nathan and McMahon 1990
          }
    
          USGS_Qvalue$baseflow_cfs=BaseflowSeparation(USGS_Qvalue[,col_name], filter_parameter = alpha, passes = passes)[,1]
          # USGS_Qvalue$stormflow_cfs=BaseflowSeparation(USGS_Qvalue[,col_name], filter_parameter = alpha, passes = passes)[,2] # If you do this, then baseflow+stormflow doesn't equal total flow
          USGS_Qvalue$stormflow_cfs=USGS_Qvalue[,col_name]-USGS_Qvalue$baseflow_cfs
    
          # Calculate Direct Runoff Volume via Trapezoidal Integration & Calculate Runoff Depth
          USGS_Qvalue$dt_sec=c(NA,as.numeric(diff(USGS_Qvalue$DateNumeric))) #calculate differential time in seconds
          USGS_Qvalue$DRO_Vol_cf=NA
          USGS_Qvalue$DRO_Depth_in=NA
          for(i in 2:dim(USGS_Qvalue)[1]){
            USGS_Qvalue$DRO_Vol_cf[i]=((USGS_Qvalue$stormflow_cfs[i-1] + USGS_Qvalue$stormflow_cfs[i]) / 2) * USGS_Qvalue$dt_sec[i] # Calculate Direct Runoff Volume (ft^3)
            if(site %in% names(watershed_area_key)){
              USGS_Qvalue$DRO_Depth_in[i]=((USGS_Qvalue$DRO_Vol_cf[i]/(as.numeric(unlist(watershed_area_key2[input$WS_Area_ac[counter]],use.names = FALSE))*43560)) * 12) # Calculate Direct Runoff Depth (in) <-- if USGS site is in keys
            }
            else{
              USGS_Qvalue$DRO_Depth_in[i]=((USGS_Qvalue$DRO_Vol_cf[i]/(as.numeric(input$WS_Area_ac[counter])*43560)) * 12) # Calculate Direct Runoff Depth (in) <-- if user enters a USGS site that isn't in the keys
            }
          }

        USGS_Qvalue$Char_Date=as.character(USGS_Qvalue$dateTime)
  
        USGS_Qvalue=subset(USGS_Qvalue,select=c("Char_Date","stormflow_cfs","baseflow_cfs","DRO_Vol_cf","DRO_Depth_in"))
        Setnames(USGS_Qvalue, c("stormflow_cfs","baseflow_cfs","DRO_Vol_cf","DRO_Depth_in"), c(paste(site,"stormflow_cfs",sep="_"),paste(site,"baseflow_cfs",sep="_"),paste(site,"DRO_Vol_cf",sep="_"),paste(site,"DRO_Depth_in",sep="_")),allow.absent.cols=T)
      }
    }
        if (counter==1){store_value=USGS_Qvalue}
        else{store_value=full_join(x=store_value,y=USGS_Qvalue, by="Char_Date")} # combine data from each USGS station into one datatable
        counter=counter+1
    }

    # Add baseflow/stormflow to combine data table
    combine_table_value=full_join(x=combine_table_value,y=store_value, by="Char_Date")
    
    # Add NOAA precip data to combine data table
    combine_table_value=full_join(x=combine_table_value,y=NOAA(), by="Char_Date")
    
    # Sort Table by Char_Date again
    setorder(combine_table_value,Char_Date)
    
    combine_table_value
  })
  

  
  ### 6) Get NOAA and USGS PFDS from NOAA Servers for specified site <-- using eventReactive so it only updates if site is changed --------------------------------
  
  ## Settings
  cgiLoc <- "http://hdsc.nws.noaa.gov/cgi-bin/hdsc/new/" #from the documentation
  selTbl <- "" #this can also be "_mean", "_uppr", or "_lwr". If left blank it grabs all 3
  selType <- "depth" #I believe this can also be "intensity" but haven't tried
  selUnits <- "english"
  selSeries <- "pds" #partial duration series. Can also be "ams", but annual maxima misses many big events, so PDS is more representative
  
  # Add duration in minutes to a PFDS table
  m=setnames(data.table(matrix(c(5,10,15,30,60,120,180,360,720,1440,2880,4320,5760,10080,14400,28800,43200,64800,86400),nrow=19)),old="V1",new="Duration_min")

  ## NOAA PFDS
  NOAA_PFDS=eventReactive(input$user.faaid,{
    
    # Get Latitude & Longitude
    lat=iadata()$lat[1]
    lon=iadata()$lon[1]

    # Create URL for PFDS from NOAA servers
    txtPgURL=paste(cgiLoc, 'fe_text', selTbl, '.csv?', 'lat=', lat, '&lon=', lon, '&type=pf', '&data=', selType, '&units=',selUnits, '&series=', selSeries, sep="")

    # Retreive data from URL and set column names
    PFDS_Mean=setnames(x=data.table(fread(str_c(txtPgURL), skip=14)),
               old=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11"),
               new=c("Duration","ARI_1","ARI_2","ARI_5","ARI_10","ARI_25","ARI_50","ARI_100","ARI_200","ARI_500","ARI_1000"))
  
    # # Gets PFDS limits for 90% confidence interval
    # # PFDS_L90=eventReactive(input$user.faaid,{
    # #   setnames(x=data.table(fread(str_c(txtPgURL()), skip=58)),
    # #                             old=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11"),
    # #                             new=c("Duration","ARI_1","ARI_2","ARI_5","ARI_10","ARI_25","ARI_50","ARI_100","ARI_200","ARI_500","ARI_1000"))})
    # # 
    # # PFDS_U90=eventReactive(input$user.faaid,{setnames(x=data.table(fread(str_c(txtPgURL()), skip=36)),
    # #                              old=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11"),
    # #                              new=c("Duration","ARI_1","ARI_2","ARI_5","ARI_10","ARI_25","ARI_50","ARI_100","ARI_200","ARI_500","ARI_1000"))})
    # 
    # 
  
  
    cbind(PFDS_Mean,m)
    
  })

  ## USGS PFDS
  
  USGS_PFDS=eventReactive(input$USGS_Precip,{
    counter=1
    # store_value=vector(mode="list",length=length(input$USGS_Precip%in%lat_key)) # From when lat/lon was hard coded in and wasn't automatically retrieved
    store_value=vector(mode="list",length=length(input$USGS_Precip))
    
    # Loop through each USGS station
    for(site in input$USGS_Precip){
      
        lat=readNWISsite(site)$dec_lat_va # Retrieve decimal latitude
        lon=readNWISsite(site)$dec_long_va # Retrieve decimal longitude
        
        # Create URL for PFDS from NOAA servers
        txtPgURL=paste(cgiLoc, 'fe_text', selTbl, '.csv?', 'lat=', lat, '&lon=', lon, '&type=pf', '&data=', selType, '&units=',selUnits, '&series=', selSeries, sep="")
        
        # Retreive data from URL and set column names
        PFDS_Mean=setnames(x=data.table(fread(str_c(txtPgURL), skip=14)),
                           old=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11"),
                           new=c("Duration","ARI_1","ARI_2","ARI_5","ARI_10","ARI_25","ARI_50","ARI_100","ARI_200","ARI_500","ARI_1000"))
        
        # Store USGS PFDS into list of PFDS
        store_value[[counter]]=cbind(PFDS_Mean,m)
      counter=counter+1
    }
    store_value
  })
  
  ### 8) Do sliding window analysis on NOAA gauge data ---------------------------------------------------------------------------------------------------
  # - gets every possible interval in time between start date and end date
  # - window width goes from length between each consecutive data point to entire length of specified date range
  # - calculates total precipitation depth (P_Inc_in) over windows
  # - divides precipitation depth by duration to get intensity
  sw_noaa= reactive({
    if(as.double(input$user.sw) == 1){
      
      withProgress(message="Calculating NOAA ARI",value=0,{ # Create Progress Bar
      
        ## Get NOAA input data
        inputdata=NOAA()
        inputdata$dateTime=as.POSIXct(inputdata$Char_Date, origin = "1970-01-01 00:00.00 UTC")
        inputdata$dt_min=NA
        inputdata$dt_min[-1]=diff(inputdata$dateTime)
        
        # create empty vectors to add data to <-- this speeds up the program instead of appending data to end of list b/c appending means the program has to keep copying the data over and over
        sw_length=((dim(inputdata)[1]*(dim(inputdata)[1]+1))/2)-1 ### this length is correct
        index_min=vector(mode="numeric",length=sw_length); index_max=vector(mode="numeric",length=sw_length) ;P_Inc_in=vector(mode="numeric",length=sw_length); dt_min=vector(mode="numeric",length=sw_length); window_width=vector(mode="numeric",length=sw_length)
        
        len=as.integer(dim(inputdata)[1])
        lwidth=len-1
        index_1=as.integer(1)
        
        for(width in 1:lwidth){
          index_2=as.integer(index_1+len-as.integer(width))
          
          index_min[index_1:index_2]=rollapply(zoo(as.numeric(rownames(inputdata))), width = width, by = 1, FUN = min, align = "left")
          index_max[index_1:index_2]=rollapply(zoo(as.numeric(rownames(inputdata))), width = width, by = 1, FUN = max, align = "left")
          P=rollapply(zoo(P_Inc_in()), width = width, by = 1, FUN = sum, align = "left")
          P_Inc_in[index_1:index_2]=P
          dt_min[index_1:index_2]=rollapply(zoo(inputdata$dt_min), width = width, by = 1, FUN = sum, align = "left")
          window_width[index_1:index_2]=rep(width, length(P))
          
          index_1=as.integer(index_1+len-(as.integer(width)-1))
        }
        
        swvalue=data.frame(index_min, index_max,window_width, P_Inc_in, dt_min)
        swvalue=swvalue[complete.cases(swvalue[,4:5]),]
        swvalue=swvalue[which(swvalue$P_Inc_in > 0),] # Get only windows where there is precipitation
        swvalue$intensity_inhr=swvalue$P_Inc_in/(swvalue$dt_min / 60)
        
        # Subset sliding window to only include durations corresponding to PFDS table
        swvalue=swvalue[swvalue$dt_min %in% c(5,10,15,30,60,120,180,360,720,1440,2880,4320,5760,10080,14400,28800,43200,64800,86400),]
        
        # If there is no precipitation; no precipitation = no rows = length ==0 <-- have to do this because R doesn't like empty columns
        if(length(unlist(swvalue))==0){
          store_value=swvalue
          
        }
        else{
        # Interpolate ARI values <-- this part takes a long time
        ARI=as.numeric(NA)
        for(i in 2:dim(swvalue[1])){ # goes through every sliding window <-- starts with row 2 b/c dt_min for first row is NA
          for(j in 1:19){ # goes through all 19 rows in PFDS
            if(swvalue$dt_min[i] == NOAA_PFDS()$Duration_min[j]){ #if duration is exact match in PFDS table:
              #interpolate along the vector, return the linearly interpolated ARI
              # - x gets value for each ARI for duration in row of PFDS
              # - y is each ARI
              # - xout is values where interpolation takes place
              ARIvalue=approx(x = NOAA_PFDS()[j,2:11], y = c(1,2,5,10,25,50,100,200,500,1000), xout = swvalue$P_Inc_in[i], method = "linear")$y #not linear - 4th order poly?
            }
            ## Code to Interpolate ARI values if duration is between durations in PFDS table <-- this takes a long time; need to comment out code to subset sliding window values above
            # else if(swvalue$dt_min[i] > PFDS()$Duration_min[j] & swvalue$dt_min[i] < PFDS()$Duration_min[j+1]){ # if duration is between durations in PFDS table:
            #   #loop through all the columns and interpolate values b/w the two rows, generate new vector
            #   row_interp=as.numeric()
            #   for(k in 1:10){ # 10 ARIs
            #     colnum=as.numeric(k+1) # create column index number variable because R doesn't understand k+1 as a column index
            #     # have to put .. in front of colnum so R knows it's a column index number and not a variable
            #     row_interp=c(row_interp, approx(x = c(PFDS()[j,"Duration_min"], PFDS()[j+1,"Duration_min"]), y = c(PFDS()[j,..colnum],PFDS()[j+1,..colnum]), xout = swvalue$dt_min[i])$y)
            #     # row_interp=c(i,j,k,PFDS()[j,"Duration_min"], PFDS()[j+1,"Duration_min"],PFDS()[j,..colnum],PFDS()[j+1,..colnum])
            #   }
            #   # now find ARI using interpolated row:
            #   ARIvalue2=approx(x = row_interp, y = c(1,2,5,10,25,50,100,200,500,1000), xout = swvalue$P_Inc_in[i], method = "linear")$y #not linear - 4th order poly?
            # }
            j=j+1
          }
          # store ARI value into ARI variable
          ARI[i]=ARIvalue
          i=i+1
        }
  
        store_value=cbind(swvalue,ARI)
  
        # Subset Columns
        store_value=subset(store_value,select=c("P_Inc_in","dt_min","intensity_inhr","ARI","index_min"))
  
        # Add station and location
        store_value$site_no=as.character(input$user.faaid)
        
        if(input$user.faaid %in% names(precip_site_key)){
          store_value$Location=as.character(unlist(precip_site_key[input$user.faaid],use.names=F))
        } else{
          store_value$Location=as.character(input$user.faaid)
        }
  
        # Remove rows with ARI=NA
        store_value=store_value[complete.cases(store_value[,"ARI"]),]
        }
        # Add Start date/time
        store_value$Start=with(NOAA(),Char_Date[store_value$index_min])
        
        incProgress(1/(length(unlist(input$USGS_Precip)))) #Update Progress Bar
        store_value
      })
    }
  })
  
  ### 9) Do sliding window analysis on USGS gauge data ---------------------------------------------------------------------------------------------------
  # - gets every possible interval in time between start date and end date
  # - window width goes from length between each consecutive data point to entire length of specified date range
  # - calculates total precipitation depth (P_Inc_in) over windows
  # - divides precipitation depth by duration to get intensity
  sw_usgs=reactive({
    if(as.double(input$user.sw) == 1){
      
      withProgress(message="Calculating USGS ARI",value=0,{ # Create Progress Bar
        counter=1
        
        # Loop through each USGS station
        for(site in input$USGS_Precip){
            
            # If there is no data for each site; if ==3 then there is only one row of data that is all NA's; if a site had data then there would be more than 1 row, but can't use nrow because R doesn't like it when columns are all NA values
            if((length(unlist(USGS_precip()))-3)/length(input$USGS_Precip)==3){
              store_value=data.table(P_Inc_in=as.numeric(NA),dt_min=as.numeric(NA),intensity_inhr=as.numeric(NA),ARI=as.numeric(NA),Start=as.character(NA))
            }
            
            # If there is data for at least one site
            else{
              ## Get USGS input data
              inputdata=SubsetCols(combine_table(),c("Char_Date",paste(site,"Precip_Inst",sep="_")),allow.absent.cols=TRUE)
              inputdata=inputdata[complete.cases(inputdata[,paste(site,"Precip_Inst",sep="_")]),] # get rid of rows with NA Precip b/c of NOAA data
              inputdata$dateTime=as.POSIXct(inputdata$Char_Date, origin = "1970-01-01 00:00.00 UTC")
              inputdata$dt_min=NA
              inputdata$dt_min[-1]=diff(inputdata$dateTime)
              
              # create empty vectors to add data to
              sw_length=((dim(inputdata)[1]*(dim(inputdata)[1]+1))/2)-1 ### this length is correct
              index_min=vector(mode="numeric",length=sw_length); index_max=vector(mode="numeric",length=sw_length) ;P_Inc_in=vector(mode="numeric",length=sw_length); dt_min=vector(mode="numeric",length=sw_length); window_width=vector(mode="numeric",length=sw_length)
              
              len=as.integer(dim(inputdata)[1])
              lwidth=len-1
              index_1=as.integer(1)
              
              for(width in 1:lwidth){
                index_2=as.integer(index_1+len-as.integer(width))
  
                index_min[index_1:index_2]=rollapply(zoo(as.numeric(rownames(inputdata))), width = width, by = 1, FUN = min, align = "left")
                index_max[index_1:index_2]=rollapply(zoo(as.numeric(rownames(inputdata))), width = width, by = 1, FUN = max, align = "left")
                P=rollapply(zoo(inputdata[,paste(site,"Precip_Inst",sep="_")]), width = width, by = 1, FUN = sum, align = "left")
                P_Inc_in[index_1:index_2]=P
                dt_min[index_1:index_2]=rollapply(zoo(inputdata$dt_min), width = width, by = 1, FUN = sum, align = "left")
                window_width[index_1:index_2]=rep(width, length(P))
  
                index_1=as.integer(index_1+len-(as.integer(width)-1))
              }
              
              swvalue=data.frame(index_min, index_max,window_width, P_Inc_in, dt_min)
              swvalue=swvalue[complete.cases(swvalue[,4:5]),]
              swvalue=swvalue[which(swvalue$P_Inc_in > 0),] # Get only windows where there is precipitation
              swvalue$intensity_inhr=swvalue$P_Inc_in/(swvalue$dt_min / 60)
              
              # Subset sliding window to only include durations corresponding to PFDS table
              swvalue=swvalue[swvalue$dt_min %in% c(5,10,15,30,60,120,180,360,720,1440,2880,4320,5760,10080,14400,28800,43200,64800,86400),]
              
              # If precipitation only occurs during 1 or 2 sliding windows (nrow<=2), then ARI loop (for i in 2:dim(swvalue[1])) doesn't work, so just set store_value to swvalue
              # If there is no precipitation occuring during any sliding window: no precipitation = no rows = length ==0 <-- have to do this because R doesn't like empty columns
              if(nrow(swvalue)<=2|length(unlist(swvalue))==0){
                store_value=swvalue
  
              }
              else{
                # Get PFDS
                PFDS=USGS_PFDS()[[counter]]
  
                # Interpolate ARI values <-- this part takes a long time
                ARI=as.numeric(NA)
                for(i in 2:dim(swvalue[1])){ # goes through every sliding window <-- starts with row 2 b/c dt_min for first row is NA
                  for(j in 1:19){ # goes through all 19 rows in PFDS
                    if(swvalue$dt_min[i] == PFDS$Duration_min[j]){ #if duration is exact match in PFDS table:
                      #interpolate along the vector, return the linearly interpolated ARI
                      # - x gets value for each ARI for duration in row of PFDS
                      # - y is each ARI
                      # - xout is values where interpolation takes place
                      ARIvalue=approx(x = PFDS[j,2:11], y = c(1,2,5,10,25,50,100,200,500,1000), xout = swvalue$P_Inc_in[i], method = "linear")$y #not linear - 4th order poly?
                    }
                    ## Code to Interpolate ARI values if duration is between durations in PFDS table <-- this takes a long time; need to comment out code to subset sliding window values above
                    # else if(swvalue$dt_min[i] > PFDS()$Duration_min[j] & swvalue$dt_min[i] < PFDS()$Duration_min[j+1]){ # if duration is between durations in PFDS table:
                    #   #loop through all the columns and interpolate values b/w the two rows, generate new vector
                    #   row_interp=as.numeric()
                    #   for(k in 1:10){ # 10 ARIs
                    #     colnum=as.numeric(k+1) # create column index number variable because R doesn't understand k+1 as a column index
                    #     # have to put .. in front of colnum so R knows it's a column index number and not a variable
                    #     row_interp=c(row_interp, approx(x = c(PFDS()[j,"Duration_min"], PFDS()[j+1,"Duration_min"]), y = c(PFDS()[j,..colnum],PFDS()[j+1,..colnum]), xout = swvalue$dt_min[i])$y)
                    #     # row_interp=c(i,j,k,PFDS()[j,"Duration_min"], PFDS()[j+1,"Duration_min"],PFDS()[j,..colnum],PFDS()[j+1,..colnum])
                    #   }
                    #   # now find ARI using interpolated row:
                    #   ARIvalue2=approx(x = row_interp, y = c(1,2,5,10,25,50,100,200,500,1000), xout = swvalue$P_Inc_in[i], method = "linear")$y #not linear - 4th order poly?
                    # }
                    j=j+1
                  }
                  # store ARI value into ARI variable
                  ARI[i]=ARIvalue
                  i=i+1
                }
  
                store_value=cbind(swvalue,ARI)
  
                # Subset Columns
                store_value=subset(store_value,select=c("P_Inc_in","dt_min","intensity_inhr","ARI","index_min"))
  
                # Add station and location
                store_value$site_no=as.character(site)
                
                if(site %in% names(precip_site_key)){
                  store_value$Location=as.character(unlist(precip_site_key[site],use.names=F))
                } else{
                  store_value$Location=as.character(site)
                }
  
                # Remove rows with ARI=NA
                store_value=store_value[complete.cases(store_value[,"ARI"]),]
                
                # Add Start date/time
                store_value$Start=with(inputdata,Char_Date[store_value$index_min])
              }
            }
          if (counter==1){
            out_value=store_value
          }
          else{out_value=full_join(x=out_value,y=store_value)} # combine data from each USGS station into one datatable
          
          incProgress(1/(length(unlist(input$USGS_Precip)))) #Update Progress Bar
          counter=counter+1
        }
        out_value
      })
    }
  })
  
  ### 10) Combine Sliding Window Tables
  
  sw_out=reactive({
    if(as.double(input$user.sw) == 1){
      
      # Join NOAA and USGS Data Tables
      sw_out_value=full_join(x=sw_noaa(),y=sw_usgs())
      
      # Reorder Columns
      sw_out_value=SubsetCols(sw_out_value,c("Location","site_no","ARI","dt_min","Start","P_Inc_in","intensity_inhr"),allow.absent.cols=TRUE)
      
      # If ARIs exist
      if("ARI"%in%colnames(sw_out_value)){
        sw_out_value=sw_out_value[order(-sw_out_value$ARI),] # Sort Table by ARI
        
        # Summarize ARI if selected
        ari_summary=sw_out_value%>%
          group_by(site_no)%>% # Sort by station
          filter(ARI==max(ARI))%>% # Grab station max
          arrange(desc(ARI)) # Sort by ARI
        
        if(input$ARI_summary==T){
          ari_summary=Setnames(ari_summary, c("site_no","ARI","dt_min","Start","P_Inc_in","intensity_inhr"), c("Station","ARI (years)","Duration (min)","Start Time","Precipitation Depth (in)","Precipitation Intensity (in/hr)"),allow.absent.cols=T)
          ari_summary
        }
        else{
          sw_out_value=Setnames(sw_out_value, c("site_no","ARI","dt_min","Start","P_Inc_in","intensity_inhr"), c("Station","ARI (years)","Duration (min)","Start Time","Precipitation Depth (in)","Precipitation Intensity (in/hr)"),allow.absent.cols=T)
          sw_out_value
        }
      }
      # If ARIs do not exist
      else{
        sw_out_value=Setnames(sw_out_value, c("site_no","ARI","dt_min","Start","P_Inc_in","intensity_inhr"), c("Station","ARI (years)","Duration (min)","Start Time","Precipitation Depth (in)","Precipitation Intensity (in/hr)"),allow.absent.cols=T)
        sw_out_value
      }
    }
  })
  

  ### 11) Calculate Summary Information
  
  # Calculate Flow Summary
  Q_summary=reactive({
    location=vector(mode="character",length=length(input$USGS_Site))
    site_name=vector(mode="character",length=length(input$USGS_Site))
    max_Q=vector(mode="numeric",length=length(input$USGS_Site))
    total_DRO=vector(mode="numeric",length=length(input$USGS_Site))
    DRODepth=vector(mode="numeric",length=length(input$USGS_Site))
    counter=1
    for(site in input$USGS_Site){
      col_name=paste(site,"Flow_Inst",sep="_")
      
      # If flow data exists for site
      if(all(is.na(combine_table()[col_name]))==F){
        max_Q[counter]=max(combine_table()[,paste(site,"Flow_Inst",sep="_")],na.rm=TRUE) # Calculate Maximum Flow Rate
        total_DRO[counter]=sum(combine_table()[,paste(site,"DRO_Vol_cf",sep="_")],na.rm=TRUE) # Calculate Direct Runoff Volume in cubic feet -- my volume is a little different than Marcus b/c I'm doing from 0:00 Date 1 - 0:00 Date 2 to match the NOAA date/time range and he's doing to 24:00 Date 2

        if(site %in% names(watershed_area_key)){
          DRODepth[counter]=((total_DRO[counter]/(as.numeric(unlist(watershed_area_key2[input$WS_Area_ac[counter]],use.names = FALSE))*43560)) * 12) # Calculate Direct Runoff Depth in inches <-- if USGS site is in keys
        }
        else{
          DRODepth[counter]=((total_DRO[counter]/(as.numeric(input$WS_Area_ac[counter])*43560)) * 12) # Calculate Direct Runoff Depth in inches <-- if user inputs a USGS site not in the keys
        }
      }
      # If flow data doesn't exist for site
      else{
        max_Q[counter]=NA
        total_DRO[counter]=NA
        DRODepth[counter]=NA
      }
      
      site_name[counter]=site
      
      if(site %in% names(flow_site_key)){
        location[counter]=unlist(flow_site_key[site],use.names=F)
      } else{
        location[counter]=site
      }
      
      counter=counter+1
    }
    
    data.table(Location=as.character(location),Station=as.character(site_name),Max_Q_cfs=as.numeric(max_Q),Total_DROVol_cf=as.numeric(total_DRO),Total_DRODepth_in=as.numeric(DRODepth))
  })
  
  # Calculate USGS Precipitation Summary
  P_summary=reactive({
    location=vector(mode="character",length=length(input$USGS_Precip))
    site_name=vector(mode="character",length=length(input$USGS_Precip))
    depth=vector(mode="numeric",length=length(input$USGS_Precip))
    i=vector(mode="numeric",length=length(input$USGS_Precip))

    counter=1
    for(site in input$USGS_Precip){
      depth[counter]=sum(combine_table()[,paste(site,"Precip_Inst",sep="_")],na.rm=TRUE) # Calculate Total Precipitation Depth
      i[counter]=max(combine_table()[,paste(site,"i_inhr",sep="_")],na.rm=TRUE)   # Calculate Maximum Intensity
      site_name[counter]=site
      
      if(site %in% names(precip_site_key)){
        location[counter]=unlist(precip_site_key[site],use.names=F)
      } else{
        location[counter]=site
      }
      
      counter=counter+1
    }
    data.table(Location=as.character(location),Station=as.character(site_name),Total_P_in=as.numeric(depth),Max_i_inhr=as.numeric(i))
    
  })
  
  # Calculate NOAA Precipitation Summary
  NOAA_P_summary=reactive({
    if (input$user.faaid %in% names(precip_site_key)){
      location=unlist(precip_site_key[input$user.faaid],use.names=F)
    }
    else{location=input$user.faaid}
    site_name=input$user.faaid
    depth=sum(as.numeric(combine_table()[,paste(input$user.faaid,"Precip_Inst",sep="_")]),na.rm=TRUE) # Calculate Total Precipitation Depth
    i=max(NOAA()[,paste(input$user.faaid,"i_inhr",sep="_"),with=FALSE],na.rm=TRUE)
    data.table(Location=as.character(location),Station=as.character(site_name),Total_P_in=as.numeric(depth),Max_i_inhr=as.numeric(i))
  })
  
  # Combine Data into Summary Table  
  summary_table=reactive({
    summary_table_value=full_join(x=Q_summary(),y=P_summary())
    summary_table_value=full_join(x=summary_table_value,y=NOAA_P_summary())
    Setnames(summary_table_value,old=c("Max_Q_cfs","Total_DROVol_cf","Total_DRODepth_in","Total_P_in","Max_i_inhr"),new=c("Maximum Discharge (cfs)","Total DRO Volume (cf)","Total DRO Depth (in)","Total Precipitation (in)","Maximum Intensity (in/hr)"))
    summary_table_value
  })  

  # Calculate Runoff Volume Coefficient
  rv_table=reactive({
    
    # Get data for just streamflow gauges & subset to get DRO Depth (in)
    streamdata=summary_table()[summary_table()$Station %in% input$USGS_Site,]
    streamdata=SubsetCols(streamdata,c("Location","Station","Total DRO Depth (in)"),allow.absent.cols=TRUE)
    
    # Get lists of each precipitation station and total precipitation depth
    precip_data=summary_table()[summary_table()$Station %in% c(input$user.faaid,input$USGS_Precip),]
    precip_data=SubsetCols(precip_data,c("Station","Total Precipitation (in)"), allow.absent.cols = FALSE)

    for(station in precip_data$Station){
      P_depth=precip_data[precip_data$Station %in% station,"Total Precipitation (in)"]
      streamdata[,station]=streamdata[,"Total DRO Depth (in)"]/P_depth
    }

    # Subset Columns to Just display Rv and rename columns based on station locations
    outdata=SubsetCols(streamdata,c("Location",input$USGS_Precip,input$user.faaid),allow.absent.cols=TRUE)
    if(input$user.faaid %in% names(precip_site_key)){
      Setnames(outdata, c("Location",input$USGS_Precip[which(input$USGS_Precip%in%names(precip_site_key))],input$user.faaid), c("Stream Gauge",unlist(precip_site_key[input$USGS_Precip],use.names=F),unlist(precip_site_key[input$user.faaid],use.names=F)),allow.absent.cols=T)  #<-- rename if user uses NOAA site that is in key
    }
    else{
      Setnames(outdata, c("Location",input$USGS_Precip[which(input$USGS_Precip%in%names(precip_site_key))]), c("Stream Gauge",unlist(precip_site_key[input$USGS_Precip],use.names=F)),allow.absent.cols=T) # <-- rename if user uses NOAA site that isn't in key
    }
    
    outdata
  })

  # To regraph hydrograph/hyetograph need to make a new table b/c ggplot 2 can't handle variables that start with numbers
  plot_table=reactive({
    counter=1
    
    # Reformat Flow and Water Quality Data
    for(site in input$USGS_Site){
      station=gsub("_.*$","",site) #Extract station name from column names
      data_col=c(paste(site,"baseflow_cfs",sep="_"),paste(site,unlist(combine_key[input$parameterCd],use.names=F),sep="_"))
      data=SubsetCols(combine_table(),c("Char_Date",data_col),allow.absent.cols=TRUE)
      data$site_no=as.character(station)

      if(station %in% names(flow_site_key)){
        data$Location=unlist(flow_site_key[station],use.names=F)
      } else{
        data$Location=station
      }
      
      # Remove Site number from columns so data from multiple sites can be joined into one column
      newnames=c("baseflow_cfs",unlist(combine_key[input$parameterCd],use.names=F))
      data=Setnames(data, data_col,newnames,allow.absent.cols=T)
      
      if (counter==1){store_value=data}
      else{store_value=full_join(x=store_value,y=data)} # combine data from each USGS station into one datatable
      counter=counter+1
    }
    
    # Reformat Precipitation Data
    precip_cols=names(select(combine_table(),ends_with("Precip_Inst")))
    
    for(col in precip_cols){
      station=gsub("_.*$","",col) #Extract station name from column names
      data=SubsetCols(combine_table(),c("Char_Date",col,paste(station,"i_inhr",sep="_")),allow.absent.cols=TRUE)
      data$site_no=as.character(station)

      if(station %in% names(precip_site_key)){
        data$Location=unlist(precip_site_key[station],use.names=F)
      } else{
        data$Location=station
      }
      
      # Remove Site number from columns so data from multiple sites can be joined into one column
      data=Setnames(data, c(col,paste(station,"i_inhr",sep="_")),c("Precip_Inst","i_inhr"),allow.absent.cols=T)
      store_value=full_join(x=store_value,y=data) # combine data from each precipitation station into one datatable
    }
    
    # Sort Table by Char_Date
    setorder(store_value,Char_Date)
    
    # Add dateTime variable for plotting
    store_value$dateTime=as.POSIXct(store_value$Char_Date, origin = "1970-01-01 00:00.00 UTC")
    
    store_value
  })
  
  ### Google Authentification to access Storm Sewer Data ______________________________________________________________________________________
  
  # Create reactive variable to store whether or not authentification has occured
  rv <- reactiveValues(
    login = FALSE
  )
  
  # Authentication
  accessToken <- callModule(googleAuth, "gauth_login",
                            login_class = "btn btn-primary",
                            logout_class = "btn btn-primary")
  userDetails <- reactive({
    validate(
      need(accessToken(), "User: Not Logged In") # This won't display if errors are hidden in Shiny
    )
    rv$login <- TRUE
    with_shiny(get_user_info, shiny_access_token = accessToken())
  })
  
  # Download Data
  HOBO_data=reactive({
    validate(
      need(!is.null(accessToken()),
           message =
             paste("Click 'Login via Google' to redirect to a Google page where", # This won't display if errors are hidden in Shiny
                   "you will authenticate yourself and authorize",
                   "this app to access your Google Sheets and Google Drive."))
    )
    
    # Spreadsheet ID's for each HOBO sensor
    HOBO_sheets=c("removed_Google_Sheet_ID",  # Add Google Sheet IDs here for Google Sheets containing storm sewer data
                  "removed_Google_Sheet_ID",
                  "removed_Google_Sheet_ID",
                  "removed_Google_Sheet_ID",
                  "removed_Google_Sheet_ID",
                  "removed_Google_Sheet_ID",
                  "removed_Google_Sheet_ID",
                  "removed_Google_Sheet_ID"
    )
    
    
    # Loop through and retrieve HOBO data for each sensor
    counter=1
    for(sheet in HOBO_sheets){

      # Download Data from Sheet
      drivesheet_value=with_shiny(f=getvalues,shiny_access_token = accessToken(),
                                  spreadsheetId=sheet,
                                  ranges="B2:G", # Get all values from google sheet without headers
                                  majorDimension="ROWS" #ROWS or COLUMNS
      )

      # Convert from JSON list to dataframe
      drivesheet_value=unlist(drivesheet_value[2])
      drivesheet_value=drivesheet_value[-c(1,2)]
      drivesheet_value=data.frame(matrix(drivesheet_value,ncol=6),stringsAsFactors = FALSE)

      # Rename Columns
      setnames(drivesheet_value,old=c("X1","X2","X3","X4","X5","X6"),new=c("Sensor_Stage_ft","Sensor_Stage_in","DateTime","DateNumeric","d_rel","Q_rel"))

      sensor=paste("ROA",counter,sep="")
      drivesheet_value$Sensor=sensor

      # Convert string "NA" values to NA and remove
      drivesheet_value[drivesheet_value=="NA"]=NA
      drivesheet_value=drivesheet_value[complete.cases(drivesheet_value[,c("Sensor_Stage_ft","Sensor_Stage_in","d_rel","Q_rel")]),]

      # Specify Value Types
      drivesheet_value$Sensor_Stage_ft=as.numeric(drivesheet_value$Sensor_Stage_ft)
      drivesheet_value$Sensor_Stage_in=as.numeric(drivesheet_value$Sensor_Stage_in)
      drivesheet_value$DateNumeric=as.numeric(drivesheet_value$DateNumeric)
      drivesheet_value$d_rel=as.numeric(drivesheet_value$d_rel)
      drivesheet_value$Q_rel=as.numeric(drivesheet_value$Q_rel)
      drivesheet_value$DateTime=ymd_hms(drivesheet_value$DateTime,tz="America/New_York")
      drivesheet_value$Char_Date=as.character(drivesheet_value$DateTime)

      #Subset/Reorder Columns
      drivesheet_value=drivesheet_value[,c("Char_Date","DateTime","Sensor","Sensor_Stage_ft","Sensor_Stage_in","d_rel","Q_rel")]

      if(counter==1){store_value=drivesheet_value}
      else{store_value=full_join(x=store_value,y=drivesheet_value)}
      counter=counter+1
    }
    store_value
  })
  
  # Create Table of HOBO Data to view/export
  HOBO_data_out=reactive({
    data=subset(HOBO_data(),select=c("Char_Date","Sensor","Sensor_Stage_ft","Sensor_Stage_in","d_rel","Q_rel"),(Sensor %in% input$HOBO_sensor)&(DateTime>=ymd(toString(input$PrecipDate[1]),tz="America/New_York"))&(DateTime<ymd(toString(input$PrecipDate[2]+1),tz="America/New_York")))
    Setnames(data,old=c("Char_Date","Sensor","Sensor_Stage_ft","Sensor_Stage_in","d_rel","Q_rel"),new=c("Date/Time","Sensor","Sensor Stage (ft)","Sensor Stage (in)","Relative Depth","Relative Discharge"))
    data
  })

  ### Download Current Conditions & 10-day forecast from Wunderground

  # Function to GeoCode using Google API
  getGeoData <- function(location, api_key){
    location <- gsub(' ','+',location)
    geo_data <- getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",location,sprintf("&key=%s",api_key), sep=""))
    geo_data <- RJSONIO::fromJSON(geo_data)
    return(geo_data$results[[1]])
  }
  
  # Geocode Text Input to get Location
  location=reactive(getGeoData(input$Location,"AIzaSyDB9cW2leUiuNk2rn3PVArvy4bK_jJYLQ8"))
  LatLong=reactive(location()$geometry$location) # Get Lat/Long
  Address=reactive(unlist(strsplit(location()$formatted_address,","))) # Get Address
  
  # Retrieve 10-Day Forecast
  forecast=reactive({
    set_api_key("removed_API_Key") # Add API key created for Wunderground Developer Account
    forecast=forecast10day(location=set_location(lat_long=paste(LatLong()[1],LatLong()[2],sep=",")),key=get_api_key())
    
    forecast$day=weekdays(forecast$date)
    forecast$date=format(as.Date(forecast$date),"%m/%d")
    
    forecast=subset(forecast,select=c("day","date","temp_high","temp_low","cond","p_precip","rain_allday"))
    setnames(forecast,old=c("day","date","temp_high","temp_low","cond","p_precip","rain_allday"),new=c("Day","Date","High Temp.","Low Temp.","Condition","Precip. Probability (%)","Precip. Depth (in)"))
    forecast
  })
  
  # Create HTML for Weather Sticker
  Sticker=reactive(paste("<width: 488px; font-family: sans-serif; font-size: 12px;'><a href='http://www.wunderground.com/cgi-bin/findweather/getForecast?query=",gsub(pattern=" ",replacement="",x=Address()[1]),", ",gsub(pattern=" ",replacement="",x=gsub('[[:digit:]]+', '', Address()[2])),"' target='_blank' title='",gsub(pattern=" ",replacement="",x=Address()[1]),", ",gsub(pattern=" ",replacement="",x=gsub('[[:digit:]]+', '', Address()[2]))," Weather Forecast'><img src='http://weathersticker.wunderground.com/weathersticker/big2_cond/language/english/",gsub(pattern=" ",replacement="",x=Address()[3]),"/",gsub(pattern=" ",replacement="",x=gsub('[[:digit:]]+', '', Address()[2])),"/",gsub(pattern=" ",replacement="",x=Address()[1]),".gif' alt='Find more about Weather in ",gsub(pattern=" ",replacement="",x=Address()[1]),", ",gsub(pattern=" ",replacement="",x=gsub('[[:digit:]]+', '', Address()[2])),"' /></a>",sep=""))
  
  
  ### Interactive Map
  
  # Create dataframe to use for interactive map
  map_data=reactive({
    # Subset data columns
    data=subset(summary_table(),select=c("Location","Station","Maximum Discharge (cfs)","Total Precipitation (in)","Maximum Intensity (in/hr)"))
    
    # Add Latitude/Longitude for NOAA Precip Station
    data[which(data$Station==input$user.faaid),"Latitude"]=iadata()$lat[1]
    data[which(data$Station==input$user.faaid),"Longitude"]=iadata()$lon[1]
    
    # Create Label for NOAA Precip Station
    data[which(data$Station==input$user.faaid),"Label"]=paste(
      sep="<br/>",
      paste("<b>",data[which(data$Station==input$user.faaid),"Location"],"</b>",sep=""),
      paste("FAA ID: ",data[which(data$Station==input$user.faaid),"Station"],sep=""),
      paste("Total Precipitation (in): ",data[which(data$Station==input$user.faaid),"Total Precipitation (in)"],sep=""),
      paste("Maximum Intensity (in/hr): ",data[which(data$Station==input$user.faaid),"Maximum Intensity (in/hr)"],sep="")
    )
    
    # Set Marker Attributes for NOAA Precip Station
    data[which(data$Station==input$user.faaid),"Marker_Color"]="darkblue"
    data[which(data$Station==input$user.faaid),"Icon"]="tint"
    data[which(data$Station==input$user.faaid),"Icon_Color"]="#ffffff"
    
    # Add Latitude/Longitude for USGS Precip Stations & Set Icon Attributes
    for(site in input$USGS_Precip){
      data[which(data$Station==site),"Latitude"]=readNWISsite(site)$dec_lat_va
      data[which(data$Station==site),"Longitude"]=readNWISsite(site)$dec_long_va
      
      # Get USGS Station Name for Stations Outside of Roanoke
      if(data[which(data$Station==site),"Location"]==data[which(data$Station==site),"Station"]){
        data[which(data$Station==site),"Location"]=readNWISsite(site)$station_nm
      }
      
      # Create Label for USGS Precip Station
      data[which(data$Station==site),"Label"]=paste(
        sep="<br/>",
        paste("<b><a href='https://waterdata.usgs.gov/va/nwis/uv?site_no=",data[which(data$Station==site),"Station"],"'",'target="_blank"',">",data[which(data$Station==site),"Location"],"</a></b>",sep=""),
        paste("USGS Station ID: ",data[which(data$Station==site),"Station"],sep=""),
        paste("Total Precipitation (in): ",data[which(data$Station==site),"Total Precipitation (in)"],sep=""),
        paste("Maximum Intensity (in/hr): ",data[which(data$Station==site),"Maximum Intensity (in/hr)"],sep="")
      )
      
      # Set Marker Attributes for USGS Precip Station
      data[which(data$Station==site),"Marker_Color"]="blue"
      data[which(data$Station==site),"Icon"]="tint"
      data[which(data$Station==site),"Icon_Color"]="#ffffff"
    }
    
    # Add Latitude/Longitude for USGS Stream Stations
    for(site in input$USGS_Site){
      data[which(data$Station==site),"Latitude"]=readNWISsite(site)$dec_lat_va
      data[which(data$Station==site),"Longitude"]=readNWISsite(site)$dec_long_va
      
      # Get USGS Station Name for Stations Outside of Roanoke
      if(data[which(data$Station==site),"Location"]==data[which(data$Station==site),"Station"]){
        data[which(data$Station==site),"Location"]=readNWISsite(site)$station_nm
      }
      
      # Create Label & set Marker Attributes for USGS Stream Stations
      if(is.na(data[which(data$Station==site),"Marker_Color"])==T){ # station does not have preciptiation data; only has discharge/water quality data
        
        # Create Label for USGS Stream Stations
        data[which(data$Station==site),"Label"]=paste(
          sep="<br/>",
          paste("<b><a href='https://waterdata.usgs.gov/va/nwis/uv?site_no=",data[which(data$Station==site),"Station"],"'",'target="_blank"',">",data[which(data$Station==site),"Location"],"</a></b>",sep=""),
          paste("USGS Station ID: ",data[which(data$Station==site),"Station"],sep=""),
          paste("Maximum Discharge (cfs): ",data[which(data$Station==site),"Maximum Discharge (cfs)"],sep="")
        )
        
        # Set Marker Attributes for USGS Stream Stations
        data[which(data$Station==site),"Marker_Color"]="red"
        data[which(data$Station==site),"Icon"]="anchor"
        data[which(data$Station==site),"Icon_Color"]="black"
      }
      else{ # If station was already marked as precipitation station, then it has both precip and discharge/water quality
        
        # Create Label for USGS Precip/Stream Stations
        data[which(data$Station==site),"Label"]=paste(
          sep="<br/>",
          paste("<b><a href='https://waterdata.usgs.gov/va/nwis/uv?site_no=",data[which(data$Station==site),"Station"],"'",'target="_blank"',">",data[which(data$Station==site),"Location"],"</a></b>",sep=""),
          paste("USGS Station ID: ",data[which(data$Station==site),"Station"],sep=""),
          paste("Maximum Discharge (cfs): ",data[which(data$Station==site),"Maximum Discharge (cfs)"],sep=""),
          paste("Total Precipitation (in): ",data[which(data$Station==site),"Total Precipitation (in)"],sep=""),
          paste("Maximum Intensity (in/hr): ",data[which(data$Station==site),"Maximum Intensity (in/hr)"],sep="")
        )
        
        # Set Marker Attributes for USGS Precip/Stream Stations
        data[which(data$Station==site),"Marker_Color"]="purple"
        data[which(data$Station==site),"Icon"]="support"
        data[which(data$Station==site),"Icon_Color"]="black"
      }
    }
    data
  })
  
  # Create Marker/Icons
  map_icons=reactive({
    awesomeIcons(icon=map_data()$Icon,
                 iconColor=map_data()$Icon_Color,
                 library="fa",
                 markerColor=map_data()$Marker_Color)
    
  })
  
  # Create Legend Marker/Icons
  legend_icons=reactive({
    awesomeIconList(
      "NOAA ASOS Precipitation"   = makeAwesomeIcon(icon= 'tint', markerColor = 'darkblue', iconColor = '#ffffff', library = "fa"),
      "USGS Precipitation"   = makeAwesomeIcon(icon= 'tint', markerColor = 'blue', iconColor = '#ffffff', library = "fa"),
      "USGS Stream/Water Quality"   = makeAwesomeIcon(icon= 'anchor', markerColor = 'red', iconColor = 'black', library = "fa"),
      "USGS Precip. & Stream/WQ" = makeAwesomeIcon(icon= 'support', markerColor = 'purple', iconColor = 'black', library = "fa")
    )
  }) 
  
  # Create Map Legend HTML
  markerLegendHTML = function(IconSet) {
    # Container div:
    legendHtml = "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:10px; margin: 0;'> Legend </h4>"
    
    n=1
    # Add each icon for font-awesome icons icons:
    for (Icon in IconSet) {
      legendHtml= paste0(legendHtml, "<div style='width: auto; height: 45px'>",
                          "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                         "<i style='margin-left: 5px; margin-top: 11px;color:",Icon[["iconColor"]],"; ","'class= 'fa fa-",Icon[["icon"]]," fa-inverse'></i>",
                          "</div>",
                          "<p style='position: relative; top: 11px; display: inline-block; ' >", names(IconSet)[n] ,"</p>",
                          "</div>")
      n=n+1
    }
    paste0(legendHtml, "</div>")
  }
  
  ### Interactive Storm Sewer Map
  
  # Import storm sewer shapefile
  sewer=reactive({
    shapefile=readOGR("www/Sewer","sewer") 
    shapefile=spTransform(shapefile,CRS("+proj=longlat +datum=WGS84 +no_defs")) # Apply projection
    shapefile
  })
  
  # Create Dataframe for sensor markers
  hobo_map_data=reactive({
    data=setNames(data.frame(matrix(ncol=5,nrow=0)),c("Sensor","Latitude","Longitude","max_d_rel","Label"))
    
    # Get Sensor Names & Maximum Relative Flow Depths for Selected Sensors
    counter=1
    for(sensor in input$HOBO_sensor){
      # Get Name
      data[counter,"Sensor"]=sensor
      
      # Get Maximum Relative Flow Depth
      if(dim(HOBO_data_out())[1]==0){ # No relative flow data in table (columns but now rows of data)
        data[counter,"max_d_rel"]=NA
      }
      else{ # relative flow data exists
        data[counter,"max_d_rel"]=round(max(HOBO_data_out()[which(HOBO_data_out()$Sensor==sensor),"Relative Depth"]),2)
      }
      counter=counter+1
    }
    
    # Set Lat/Long for Sensors
    data[which(data$Sensor=="ROA1"),"Latitude"]=37.27370493350
    data[which(data$Sensor=="ROA1"),"Longitude"]=-79.94378641460

    data[which(data$Sensor=="ROA2"),"Latitude"]=37.27210111200
    data[which(data$Sensor=="ROA2"),"Longitude"]=-79.93535528780

    data[which(data$Sensor=="ROA3"),"Latitude"]=37.27301487650
    data[which(data$Sensor=="ROA3"),"Longitude"]=-79.93800644610

    data[which(data$Sensor=="ROA4"),"Latitude"]=37.27283549150
    data[which(data$Sensor=="ROA4"),"Longitude"]=-79.94331075400

    data[which(data$Sensor=="ROA5"),"Latitude"]=37.27250149760
    data[which(data$Sensor=="ROA5"),"Longitude"]=-79.94362797880

    data[which(data$Sensor=="ROA6"),"Latitude"]=37.27246745150
    data[which(data$Sensor=="ROA6"),"Longitude"]=-79.94272589030

    data[which(data$Sensor=="ROA7"),"Latitude"]=37.27217105160
    data[which(data$Sensor=="ROA7"),"Longitude"]=-79.94268625180

    data[which(data$Sensor=="ROA8"),"Latitude"]=37.27212806650
    data[which(data$Sensor=="ROA8"),"Longitude"]=-79.93536153150
    
    # Create Labels
    for(sensor in input$HOBO_sensor){
      data[which(data$Sensor==sensor),"Label"]=paste(
        sep="<br/>",
        paste("<b>",sensor,"</b>",sep=""),
        paste0("Max. Relative Depth: ",data[which(data$Sensor==sensor),"max_d_rel"])
      )
    }
    data
  })
  
  # Create Legend Marker/Icons for Storm Sewer Map
  hobo_legend=reactive({
    legendHtml=paste0(
      # Container div:
      "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:10px; margin: 0;'> Legend </h4>",
          
          # Storm Sewer Sensor:
          "<div style='width: auto; height: 45px'>",
          "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-","black"," awesome-marker'>",
          "<i style='margin-left: 0px; margin-top: 8px;color:#ffffff",";","'class= 'fa fa-","adjust"," fa-inverse fa-rotate-90'></i>",
          "</div>",
          "<p style='position: relative; top: 11px; display: inline-block; ' >", "Storm Sewer Sensor" ,"</p>",
          "</div>",
      
          # Conveyances:
          "<div style='width: auto; height: 45px'>",
          "<div style='position: relative; display: inline-block; width: 36px; height: 45px'>",
          "<i style='margin-left: 11px; margin-top: 8px;color:blue",";","'class= 'fa fa-","window-minimize"," fa-inverse'></i>",
          "</div>",
          "<p style='position: relative; top: 11px; display: inline-block; ' >", "Conveyances" ,"</p>",
          "</div>",
      
      "</div>"
    )
  })

  ### Flood Stage Graphics
  
  ## Lick Run
  flood_lick_run=reactive({
    # Station Parameters
    station="Lick Run" 
    station_id="0205551460" 
    stages=c(8,9,10) # Minor Alert, Moderate Alert, and Major Stages
    
    # Create data frame to store Gauge Height Data
    data=data.frame(matrix(ncol=3))
    colnames(data)=c("Station","Date/Time","Gauge Height")
    data[1,"Station"]=station
    data[1,"Date/Time"]=combine_table_out()[which(combine_table_out()[,paste0(station_id,"_Gauge_Height_ft")]==max(combine_table_out()[,c("Date/Time",paste0(station_id,"_Gauge_Height_ft"))][,paste0(station_id,"_Gauge_Height_ft")],na.rm=T)),"Date/Time"][1]
    data[1,"Gauge Height"]=max(combine_table_out()[,c("Date/Time",paste0(station_id,"_Gauge_Height_ft"))][,paste0(station_id,"_Gauge_Height_ft")],na.rm=T)
    
    # Create Plot
    plot=ggplot(data,aes(x=Station,y=`Gauge Height`))

    # Change Color of Bar depending on gauge height
    if(data[which(data[,"Station"]==station),"Gauge Height"]<stages[1]){
      plot=plot+geom_bar(stat="identity",fill="#60e519")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[1]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[2]){
      plot=plot+geom_bar(stat="identity",fill="#fcec0a")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[2]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[3]){
      plot=plot+geom_bar(stat="identity",fill="#A80000")
    } else{
      plot=plot+geom_bar(stat="identity",fill="#303030")
    }

    plot=plot+
      scale_y_continuous(limits=c(0,max(20,data[1,"Gauge Height"])+5))+ #Delete this line whenever I specify stages
      # scale_y_continuous(limits=c(0,max(c(stages[3],data[1,"Gauge Height"])+5)),sec.axis=sec_axis(~.,breaks=stages,name="Flood Stage",labels=c(paste0("Minor (",stages[1],".0 ft.)"),paste0("Moderate (",stages[2],".0 ft.)"),paste0("Major (",stages[3],".0 ft.)"))))+ # Add Flood Stage Axis
      # geom_hline(yintercept=stages[1],linetype="dashed",color="yellow",size=2.5)+
      # geom_hline(yintercept=stages[2],linetype="dashed",color="red",size=2.5)+
      # geom_hline(yintercept=stages[3],linetype="dashed",color="black",size=2.5)+
      geom_shadowtext(aes(label=paste(data[1,"Gauge Height"],"ft."),fontface='bold'),vjust=-1.5,size=6,color="black",bg.colour="white")+ # Add Data Label Gauge Height
      geom_shadowtext(aes(label=paste(data[1,"Date/Time"]),fontface='bold'),vjust=-0.5,size=4,color="black",bg.colour="white")+ # Add Data Label Date/Time
      xlab(station)+
      ylab("Maximum Gauge Height (ft)")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face='bold'),
            axis.text.x=element_blank(),
            legend.position="bottom",
            legend.title=element_text(size=14, face='bold'),
            legend.text=element_text(size=12),
            plot.title=element_text(size=16,face='bold',hjust=0.5))
    plot
  })
  
  ## Tinker Creek
  flood_tinker=reactive({
    # Station Parameters
    station="Tinker Creek"
    station_id="02055100"
    stages=c(11,13,15) # Minor Alert, Moderate Alert, and Major Stages
    
    # Create data frame to store Gauge Height Data
    data=data.frame(matrix(ncol=3))
    colnames(data)=c("Station","Date/Time","Gauge Height")
    data[1,"Station"]=station
    data[1,"Date/Time"]=combine_table_out()[which(combine_table_out()[,paste0(station_id,"_Gauge_Height_ft")]==max(combine_table_out()[,c("Date/Time",paste0(station_id,"_Gauge_Height_ft"))][,paste0(station_id,"_Gauge_Height_ft")],na.rm=T)),"Date/Time"][1]
    data[1,"Gauge Height"]=max(combine_table_out()[,c("Date/Time",paste0(station_id,"_Gauge_Height_ft"))][,paste0(station_id,"_Gauge_Height_ft")],na.rm=T)
    
    # Create Plot
    plot=ggplot(data,aes(x=Station,y=`Gauge Height`))
    
    # Change Color of Bar depending on gauge height
    if(data[which(data[,"Station"]==station),"Gauge Height"]<stages[1]){
      plot=plot+geom_bar(stat="identity",fill="#60e519")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[1]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[2]){
      plot=plot+geom_bar(stat="identity",fill="#fcec0a")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[2]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[3]){
      plot=plot+geom_bar(stat="identity",fill="#A80000")
    } else{
      plot=plot+geom_bar(stat="identity",fill="#303030")
    }
    
    plot=plot+
      scale_y_continuous(limits=c(0,max(c(20,data[1,"Gauge Height"])+5)),sec.axis=sec_axis(~.,breaks=stages,name="Flood Stage",labels=c(paste0("Minor (",stages[1],".0 ft.)"),paste0("Moderate (",stages[2],".0 ft.)"),paste0("Major (",stages[3],".0 ft.)"))))+ # Add Flood Stage Axis
      geom_hline(yintercept=stages[1],linetype="dashed",color="yellow",size=2.5)+
      geom_hline(yintercept=stages[2],linetype="dashed",color="red",size=2.5)+
      geom_hline(yintercept=stages[3],linetype="dashed",color="black",size=2.5)+
      geom_shadowtext(aes(label=paste(data[1,"Gauge Height"],"ft."),fontface='bold'),vjust=-1.5,size=6,color="black",bg.colour="white")+ # Add Data Label Gauge Height
      geom_shadowtext(aes(label=paste(data[1,"Date/Time"]),fontface='bold'),vjust=-0.5,size=4,color="black",bg.colour="white")+ # Add Data Label Date/Time
      xlab(station)+
      ylab("Maximum Gauge Height (ft)")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face='bold'),
            axis.text.x=element_blank(),
            legend.position="bottom",
            legend.title=element_text(size=14, face='bold'),
            legend.text=element_text(size=12),
            plot.title=element_text(size=16,face='bold',hjust=0.5))
    plot
  })
  
  ## Roanoke River at Roanoke
  flood_rr1=reactive({
    # Station Parameters
    station="Roanoke River at Roanoke"
    station_id="02055000"
    stages=c(10,12,16) # Minor Alert, Moderate Alert, and Major Stages
    
    # Create data frame to store Gauge Height Data
    data=data.frame(matrix(ncol=3))
    colnames(data)=c("Station","Date/Time","Gauge Height")
    data[1,"Station"]=station
    data[1,"Date/Time"]=combine_table_out()[which(combine_table_out()[,paste0(station_id,"_Gauge_Height_ft")]==max(combine_table_out()[,c("Date/Time",paste0(station_id,"_Gauge_Height_ft"))][,paste0(station_id,"_Gauge_Height_ft")],na.rm=T)),"Date/Time"][1]
    data[1,"Gauge Height"]=max(combine_table_out()[,c("Date/Time",paste0(station_id,"_Gauge_Height_ft"))][,paste0(station_id,"_Gauge_Height_ft")],na.rm=T)
    
    # Create Plot
    plot=ggplot(data,aes(x=Station,y=`Gauge Height`))
    
    # Change Color of Bar depending on gauge height
    if(data[which(data[,"Station"]==station),"Gauge Height"]<stages[1]){
      plot=plot+geom_bar(stat="identity",fill="#60e519")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[1]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[2]){
      plot=plot+geom_bar(stat="identity",fill="#fcec0a")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[2]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[3]){
      plot=plot+geom_bar(stat="identity",fill="#A80000")
    } else{
      plot=plot+geom_bar(stat="identity",fill="#303030")
    }
    
    plot=plot+
      scale_y_continuous(limits=c(0,max(c(20,data[1,"Gauge Height"])+5)),sec.axis=sec_axis(~.,breaks=stages,name="Flood Stage",labels=c(paste0("Minor (",stages[1],".0 ft.)"),paste0("Moderate (",stages[2],".0 ft.)"),paste0("Major (",stages[3],".0 ft.)"))))+ # Add Flood Stage Axis
      geom_hline(yintercept=stages[1],linetype="dashed",color="yellow",size=2.5)+
      geom_hline(yintercept=stages[2],linetype="dashed",color="red",size=2.5)+
      geom_hline(yintercept=stages[3],linetype="dashed",color="black",size=2.5)+
      geom_shadowtext(aes(label=paste(data[1,"Gauge Height"],"ft."),fontface='bold'),vjust=-1.5,size=6,color="black",bg.colour="white")+ # Add Data Label Gauge Height
      geom_shadowtext(aes(label=paste(data[1,"Date/Time"]),fontface='bold'),vjust=-0.5,size=4,color="black",bg.colour="white")+ # Add Data Label Date/Time
      xlab(station)+
      ylab("Maximum Gauge Height (ft)")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face='bold'),
            axis.text.x=element_blank(),
            legend.position="bottom",
            legend.title=element_text(size=14, face='bold'),
            legend.text=element_text(size=12),
            plot.title=element_text(size=16,face='bold',hjust=0.5))
    plot
  })
  
  ## Roanoke River at Glenvar
  flood_rr2=reactive({
    # Station Parameters
    station="Roanoke River at Glenvar"
    station_id="02054530"
    stages=c(9,14,16) # Minor Alert, Moderate Alert, and Major Stages
    
    # Create data frame to store Gauge Height Data
    data=data.frame(matrix(ncol=3))
    colnames(data)=c("Station","Date/Time","Gauge Height")
    data[1,"Station"]=station
    data[1,"Date/Time"]=combine_table_out()[which(combine_table_out()[,paste0(station_id,"_Gauge_Height_ft")]==max(combine_table_out()[,c("Date/Time",paste0(station_id,"_Gauge_Height_ft"))][,paste0(station_id,"_Gauge_Height_ft")],na.rm=T)),"Date/Time"][1]
    data[1,"Gauge Height"]=max(combine_table_out()[,c("Date/Time",paste0(station_id,"_Gauge_Height_ft"))][,paste0(station_id,"_Gauge_Height_ft")],na.rm=T)
    
    # Create Plot
    plot=ggplot(data,aes(x=Station,y=`Gauge Height`))
    
    # Change Color of Bar depending on gauge height
    if(data[which(data[,"Station"]==station),"Gauge Height"]<stages[1]){
      plot=plot+geom_bar(stat="identity",fill="#60e519")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[1]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[2]){
      plot=plot+geom_bar(stat="identity",fill="#fcec0a")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[2]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[3]){
      plot=plot+geom_bar(stat="identity",fill="#A80000")
    } else{
      plot=plot+geom_bar(stat="identity",fill="#303030")
    }
    
    plot=plot+
      scale_y_continuous(limits=c(0,max(c(20,data[1,"Gauge Height"])+5)),sec.axis=sec_axis(~.,breaks=stages,name="Flood Stage",labels=c(paste0("Minor (",stages[1],".0 ft.)"),paste0("Moderate (",stages[2],".0 ft.)"),paste0("Major (",stages[3],".0 ft.)"))))+ # Add Flood Stage Axis
      geom_hline(yintercept=stages[1],linetype="dashed",color="yellow",size=2.5)+
      geom_hline(yintercept=stages[2],linetype="dashed",color="red",size=2.5)+
      geom_hline(yintercept=stages[3],linetype="dashed",color="black",size=2.5)+
      geom_shadowtext(aes(label=paste(data[1,"Gauge Height"],"ft."),fontface='bold'),vjust=-1.5,size=6,color="black",bg.colour="white")+ # Add Data Label Gauge Height
      geom_shadowtext(aes(label=paste(data[1,"Date/Time"]),fontface='bold'),vjust=-0.5,size=4,color="black",bg.colour="white")+ # Add Data Label Date/Time
      xlab(station)+
      ylab("Maximum Gauge Height (ft)")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face='bold'),
            axis.text.x=element_blank(),
            legend.position="bottom",
            legend.title=element_text(size=14, face='bold'),
            legend.text=element_text(size=12),
            plot.title=element_text(size=16,face='bold',hjust=0.5))
    plot
  })
  
  ## Roanoke River at Lafayette
  flood_rr3=reactive({
    # Station Parameters
    station="Roanoke River at Lafayette"
    station_id="02054500"
    stages=c(8,11,13) # Minor Alert, Moderate Alert, and Major Stages
    
    # Create data frame to store Gauge Height Data
    data=data.frame(matrix(ncol=3))
    colnames(data)=c("Station","Date/Time","Gauge Height")
    data[1,"Station"]=station
    data[1,"Date/Time"]=combine_table_out()[which(combine_table_out()[,paste0(station_id,"_Gauge_Height_ft")]==max(combine_table_out()[,c("Date/Time",paste0(station_id,"_Gauge_Height_ft"))][,paste0(station_id,"_Gauge_Height_ft")],na.rm=T)),"Date/Time"][1]
    data[1,"Gauge Height"]=max(combine_table_out()[,c("Date/Time",paste0(station_id,"_Gauge_Height_ft"))][,paste0(station_id,"_Gauge_Height_ft")],na.rm=T)
    
    # Create Plot
    plot=ggplot(data,aes(x=Station,y=`Gauge Height`))
    
    # Change Color of Bar depending on gauge height
    if(data[which(data[,"Station"]==station),"Gauge Height"]<stages[1]){
      plot=plot+geom_bar(stat="identity",fill="#60e519")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[1]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[2]){
      plot=plot+geom_bar(stat="identity",fill="#fcec0a")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[2]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[3]){
      plot=plot+geom_bar(stat="identity",fill="#A80000")
    } else{
      plot=plot+geom_bar(stat="identity",fill="#303030")
    }
    
    plot=plot+
      scale_y_continuous(limits=c(0,max(c(20,data[1,"Gauge Height"])+5)),sec.axis=sec_axis(~.,breaks=stages,name="Flood Stage",labels=c(paste0("Minor (",stages[1],".0 ft.)"),paste0("Moderate (",stages[2],".0 ft.)"),paste0("Major (",stages[3],".0 ft.)"))))+ # Add Flood Stage Axis
      geom_hline(yintercept=stages[1],linetype="dashed",color="yellow",size=2.5)+
      geom_hline(yintercept=stages[2],linetype="dashed",color="red",size=2.5)+
      geom_hline(yintercept=stages[3],linetype="dashed",color="black",size=2.5)+
      geom_shadowtext(aes(label=paste(data[1,"Gauge Height"],"ft."),fontface='bold'),vjust=-1.5,size=6,color="black",bg.colour="white")+ # Add Data Label Gauge Height
      geom_shadowtext(aes(label=paste(data[1,"Date/Time"]),fontface='bold'),vjust=-0.5,size=4,color="black",bg.colour="white")+ # Add Data Label Date/Time
      xlab(station)+
      ylab("Maximum Gauge Height (ft)")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face='bold'),
            axis.text.x=element_blank(),
            legend.position="bottom",
            legend.title=element_text(size=14, face='bold'),
            legend.text=element_text(size=12),
            plot.title=element_text(size=16,face='bold',hjust=0.5))
    plot
  })

  ### Real-Time Flood Stage Graphics

  ## Lick Run
  gh_lick_run=reactive({
    # Station Parameters
    station="Lick Run" 
    station_id="0205551460" 
    stages=c(8,9,10) # Minor Alert, Moderate Alert, and Major Stages
    
    # Retrieve Current Gauge Height Data for Today
    gh_data=renameNWISColumns(readNWISuv(siteNumbers=station_id,parameterCd="00065",startDate=Sys.Date()-1,endDate=Sys.Date(),tz="America/New_York"))
    
    # Create data frame to store Gauge Height Data
    data=data.frame(matrix(ncol=3))
    colnames(data)=c("Station","Date/Time","Gauge Height")
    data[1,"Station"]=station
    data[1,"Date/Time"]=as.character(gh_data[nrow(gh_data),"dateTime"])
    data[1,"Gauge Height"]=gh_data[nrow(gh_data),names(select(gh_data,ends_with("GH_Inst")))] 
    
    # Create Plot
    plot=ggplot(data,aes(x=Station,y=`Gauge Height`))

    # Change Color of Bar depending on gauge height
    if(data[which(data[,"Station"]==station),"Gauge Height"]<stages[1]){
      plot=plot+geom_bar(stat="identity",fill="#60e519")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[1]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[2]){
      plot=plot+geom_bar(stat="identity",fill="#fcec0a")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[2]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[3]){
      plot=plot+geom_bar(stat="identity",fill="#A80000")
    } else{
      plot=plot+geom_bar(stat="identity",fill="#303030")
    }

    plot=plot+
      scale_y_continuous(limits=c(0,max(c(20,data[1,"Gauge Height"])+5)))+ #Delete this line whenever I specify stages
      # scale_y_continuous(limits=c(0,max(c(20,data[1,"Gauge Height"])+5)),sec.axis=sec_axis(~.,breaks=stages,name="Flood Stage",labels=c(paste0("Minor (",stages[1],".0 ft.)"),paste0("Moderate (",stages[2],".0 ft.)"),paste0("Major (",stages[3],".0 ft.)"))))+ # Add Flood Stage Axis
      # geom_hline(yintercept=stages[1],linetype="dashed",color="yellow",size=2.5)+
      # geom_hline(yintercept=stages[2],linetype="dashed",color="red",size=2.5)+
      # geom_hline(yintercept=stages[3],linetype="dashed",color="black",size=2.5)+
      geom_shadowtext(aes(label=paste(data[1,"Gauge Height"],"ft."),fontface='bold'),vjust=-1.5,size=6,color="black",bg.colour="white")+ # Add Data Label Gauge Height
      geom_shadowtext(aes(label=paste(data[1,"Date/Time"]),fontface='bold'),vjust=-0.5,size=4,color="black",bg.colour="white")+ # Add Data Label Date/Time
      xlab(station)+
      ylab("Current Gauge Height (ft)")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face='bold'),
            axis.text.x=element_blank(),
            legend.position="bottom",
            legend.title=element_text(size=14, face='bold'),
            legend.text=element_text(size=12),
            plot.title=element_text(size=16,face='bold',hjust=0.5))
    plot
  })
  
  ## Tinker Creek
  gh_tinker=reactive({
    # Station Parameters
    station="Tinker Creek"
    station_id="02055100"
    stages=c(11,13,15) # Minor Alert, Moderate Alert, and Major Stages

    # Retrieve Current Gauge Height Data for Today
    gh_data=renameNWISColumns(readNWISuv(siteNumbers=station_id,parameterCd="00065",startDate=Sys.Date()-1,endDate=Sys.Date(),tz="America/New_York"))
    
    # Create data frame to store Gauge Height Data
    data=data.frame(matrix(ncol=3))
    colnames(data)=c("Station","Date/Time","Gauge Height")
    data[1,"Station"]=station
    data[1,"Date/Time"]=as.character(gh_data[nrow(gh_data),"dateTime"])
    data[1,"Gauge Height"]=gh_data[nrow(gh_data),names(select(gh_data,ends_with("GH_Inst")))] 

    # Create Plot
    plot=ggplot(data,aes(x=Station,y=`Gauge Height`))

    # Change Color of Bar depending on gauge height
    if(data[which(data[,"Station"]==station),"Gauge Height"]<stages[1]){
      plot=plot+geom_bar(stat="identity",fill="#60e519")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[1]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[2]){
      plot=plot+geom_bar(stat="identity",fill="#fcec0a")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[2]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[3]){
      plot=plot+geom_bar(stat="identity",fill="#A80000")
    } else{
      plot=plot+geom_bar(stat="identity",fill="#303030")
    }

    plot=plot+
      scale_y_continuous(limits=c(0,max(c(20,data[1,"Gauge Height"])+5)),sec.axis=sec_axis(~.,breaks=stages,name="Flood Stage",labels=c(paste0("Minor (",stages[1],".0 ft.)"),paste0("Moderate (",stages[2],".0 ft.)"),paste0("Major (",stages[3],".0 ft.)"))))+ # Add Flood Stage Axis
      geom_hline(yintercept=stages[1],linetype="dashed",color="yellow",size=2.5)+
      geom_hline(yintercept=stages[2],linetype="dashed",color="red",size=2.5)+
      geom_hline(yintercept=stages[3],linetype="dashed",color="black",size=2.5)+
      geom_shadowtext(aes(label=paste(data[1,"Gauge Height"],"ft."),fontface='bold'),vjust=-1.5,size=6,color="black",bg.colour="white")+ # Add Data Label Gauge Height
      geom_shadowtext(aes(label=paste(data[1,"Date/Time"]),fontface='bold'),vjust=-0.5,size=4,color="black",bg.colour="white")+ # Add Data Label Date/Time
      xlab(station)+
      ylab("Current Gauge Height (ft)")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face='bold'),
            axis.text.x=element_blank(),
            legend.position="bottom",
            legend.title=element_text(size=14, face='bold'),
            legend.text=element_text(size=12),
            plot.title=element_text(size=16,face='bold',hjust=0.5))
    plot
  })
  
  ## Roanoke River at Roanoke
  gh_rr1=reactive({
    # Station Parameters
    station="Roanoke River at Roanoke"
    station_id="02055000"
    stages=c(10,12,16) # Minor Alert, Moderate Alert, and Major Stages

    # Retrieve Current Gauge Height Data for Today
    gh_data=renameNWISColumns(readNWISuv(siteNumbers=station_id,parameterCd="00065",startDate=Sys.Date()-1,endDate=Sys.Date(),tz="America/New_York"))
    
    # Create data frame to store Gauge Height Data
    data=data.frame(matrix(ncol=3))
    colnames(data)=c("Station","Date/Time","Gauge Height")
    data[1,"Station"]=station
    data[1,"Date/Time"]=as.character(gh_data[nrow(gh_data),"dateTime"])
    data[1,"Gauge Height"]=gh_data[nrow(gh_data),names(select(gh_data,ends_with("GH_Inst")))] 

    # Create Plot
    plot=ggplot(data,aes(x=Station,y=`Gauge Height`))

    # Change Color of Bar depending on gauge height
    if(data[which(data[,"Station"]==station),"Gauge Height"]<stages[1]){
      plot=plot+geom_bar(stat="identity",fill="#60e519")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[1]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[2]){
      plot=plot+geom_bar(stat="identity",fill="#fcec0a")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[2]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[3]){
      plot=plot+geom_bar(stat="identity",fill="#A80000")
    } else{
      plot=plot+geom_bar(stat="identity",fill="#303030")
    }

    plot=plot+
      scale_y_continuous(limits=c(0,max(c(20,data[1,"Gauge Height"])+5)),sec.axis=sec_axis(~.,breaks=stages,name="Flood Stage",labels=c(paste0("Minor (",stages[1],".0 ft.)"),paste0("Moderate (",stages[2],".0 ft.)"),paste0("Major (",stages[3],".0 ft.)"))))+ # Add Flood Stage Axis
      geom_hline(yintercept=stages[1],linetype="dashed",color="yellow",size=2.5)+
      geom_hline(yintercept=stages[2],linetype="dashed",color="red",size=2.5)+
      geom_hline(yintercept=stages[3],linetype="dashed",color="black",size=2.5)+
      geom_shadowtext(aes(label=paste(data[1,"Gauge Height"],"ft."),fontface='bold'),vjust=-1.5,size=6,color="black",bg.colour="white")+ # Add Data Label Gauge Height
      geom_shadowtext(aes(label=paste(data[1,"Date/Time"]),fontface='bold'),vjust=-0.5,size=4,color="black",bg.colour="white")+ # Add Data Label Date/Time
      xlab(station)+
      ylab("Current Gauge Height (ft)")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face='bold'),
            axis.text.x=element_blank(),
            legend.position="bottom",
            legend.title=element_text(size=14, face='bold'),
            legend.text=element_text(size=12),
            plot.title=element_text(size=16,face='bold',hjust=0.5))
    plot
  })
  
  ## Roanoke River at Glenvar
  gh_rr2=reactive({
    # Station Parameters
    station="Roanoke River at Glenvar"
    station_id="02054530"
    stages=c(9,14,16) # Minor Alert, Moderate Alert, and Major Stages

    # Retrieve Current Gauge Height Data for Today
    gh_data=renameNWISColumns(readNWISuv(siteNumbers=station_id,parameterCd="00065",startDate=Sys.Date()-1,endDate=Sys.Date(),tz="America/New_York"))
    
    # Create data frame to store Gauge Height Data
    data=data.frame(matrix(ncol=3))
    colnames(data)=c("Station","Date/Time","Gauge Height")
    data[1,"Station"]=station
    data[1,"Date/Time"]=as.character(gh_data[nrow(gh_data),"dateTime"])
    data[1,"Gauge Height"]=gh_data[nrow(gh_data),names(select(gh_data,ends_with("GH_Inst")))] 

    # Create Plot
    plot=ggplot(data,aes(x=Station,y=`Gauge Height`))

    # Change Color of Bar depending on gauge height
    if(data[which(data[,"Station"]==station),"Gauge Height"]<stages[1]){
      plot=plot+geom_bar(stat="identity",fill="#60e519")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[1]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[2]){
      plot=plot+geom_bar(stat="identity",fill="#fcec0a")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[2]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[3]){
      plot=plot+geom_bar(stat="identity",fill="#A80000")
    } else{
      plot=plot+geom_bar(stat="identity",fill="#303030")
    }

    plot=plot+
      scale_y_continuous(limits=c(0,max(c(20,data[1,"Gauge Height"])+5)),sec.axis=sec_axis(~.,breaks=stages,name="Flood Stage",labels=c(paste0("Minor (",stages[1],".0 ft.)"),paste0("Moderate (",stages[2],".0 ft.)"),paste0("Major (",stages[3],".0 ft.)"))))+ # Add Flood Stage Axis
      geom_hline(yintercept=stages[1],linetype="dashed",color="yellow",size=2.5)+
      geom_hline(yintercept=stages[2],linetype="dashed",color="red",size=2.5)+
      geom_hline(yintercept=stages[3],linetype="dashed",color="black",size=2.5)+
      geom_shadowtext(aes(label=paste(data[1,"Gauge Height"],"ft."),fontface='bold'),vjust=-1.5,size=6,color="black",bg.colour="white")+ # Add Data Label Gauge Height
      geom_shadowtext(aes(label=paste(data[1,"Date/Time"]),fontface='bold'),vjust=-0.5,size=4,color="black",bg.colour="white")+ # Add Data Label Date/Time
      xlab(station)+
      ylab("Current Gauge Height (ft)")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face='bold'),
            axis.text.x=element_blank(),
            legend.position="bottom",
            legend.title=element_text(size=14, face='bold'),
            legend.text=element_text(size=12),
            plot.title=element_text(size=16,face='bold',hjust=0.5))
    plot
  })

  ## Roanoke River at Lafayette
  gh_rr3=reactive({
    # Station Parameters
    station="Roanoke River at Lafayette"
    station_id="02054500"
    stages=c(8,11,13) # Minor Alert, Moderate Alert, and Major Stages

    # Retrieve Current Gauge Height Data for Today
    gh_data=renameNWISColumns(readNWISuv(siteNumbers=station_id,parameterCd="00065",startDate=Sys.Date()-1,endDate=Sys.Date(),tz="America/New_York"))
    
    # Create data frame to store Gauge Height Data
    data=data.frame(matrix(ncol=3))
    colnames(data)=c("Station","Date/Time","Gauge Height")
    data[1,"Station"]=station
    data[1,"Date/Time"]=as.character(gh_data[nrow(gh_data),"dateTime"])
    data[1,"Gauge Height"]=gh_data[nrow(gh_data),names(select(gh_data,ends_with("GH_Inst")))] 

    # Create Plot
    plot=ggplot(data,aes(x=Station,y=`Gauge Height`))

    # Change Color of Bar depending on gauge height
    if(data[which(data[,"Station"]==station),"Gauge Height"]<stages[1]){
      plot=plot+geom_bar(stat="identity",fill="#60e519")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[1]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[2]){
      plot=plot+geom_bar(stat="identity",fill="#fcec0a")
    } else if(data[which(data[,"Station"]==station),"Gauge Height"]>=stages[2]&data[which(data[,"Station"]==station),"Gauge Height"]<stages[3]){
      plot=plot+geom_bar(stat="identity",fill="#A80000")
    } else{
      plot=plot+geom_bar(stat="identity",fill="#303030")
    }

    plot=plot+
      scale_y_continuous(limits=c(0,max(c(20,data[1,"Gauge Height"])+5)),sec.axis=sec_axis(~.,breaks=stages,name="Flood Stage",labels=c(paste0("Minor (",stages[1],".0 ft.)"),paste0("Moderate (",stages[2],".0 ft.)"),paste0("Major (",stages[3],".0 ft.)"))))+ # Add Flood Stage Axis
      geom_hline(yintercept=stages[1],linetype="dashed",color="yellow",size=2.5)+
      geom_hline(yintercept=stages[2],linetype="dashed",color="red",size=2.5)+
      geom_hline(yintercept=stages[3],linetype="dashed",color="black",size=2.5)+
      geom_shadowtext(aes(label=paste(data[1,"Gauge Height"],"ft."),fontface='bold'),vjust=-1.5,size=6,color="black",bg.colour="white")+ # Add Data Label Gauge Height
      geom_shadowtext(aes(label=paste(data[1,"Date/Time"]),fontface='bold'),vjust=-0.5,size=4,color="black",bg.colour="white")+ # Add Data Label Date/Time
      xlab(station)+
      ylab("Current Gauge Height (ft)")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face='bold'),
            axis.text.x=element_blank(),
            legend.position="bottom",
            legend.title=element_text(size=14, face='bold'),
            legend.text=element_text(size=12),
            plot.title=element_text(size=16,face='bold',hjust=0.5))
    plot
  })

  ### Create Output Variables ###################################################################################################

  # Download Handler for User Manual
  output$manual_download=downloadHandler(
    filename=function(){
      paste("Methods & User Manual",".pdf",sep="")
    },
    content=function(file){
      file.copy("www/SHARKS Methods & User Manual.pdf",file)
    }
  )

  ## Output Table of Stations if FAAID is blank
  faaidtable_out=reactive({
    data=Site()
    data=setnames(data,old=c("sname","sid"),new=c("Location","FAA ID"))
    data
  })
  
  output$faaidtable=DT::renderDataTable(datatable(faaidtable_out(),options=list(scrollX=T)))
  
  
  
  ## Output NOAA & USGS PFDS Mean
  output$NOAA_site=renderText({paste("<b> FAA ID Code: ", input$user.faaid,"</b>")})
  output$NOAA_PFDS=DT::renderDataTable(NOAA_PFDS(),rownames=F,options=list(scrollX=T))
  
  output$USGS_PFDS=DT::renderDataTable(USGS_PFDS()[[match(input$counter,options())]],rownames=F,options=list(scrollX=T))
  
  ## Download NOAA PFDS
  output$NOAA_PFDS_download=downloadHandler(
    filename=function(){
      paste(input$user.faaid,"_PFDS",".csv",sep="")
    },
    content=function(file){
      write.csv(NOAA_PFDS(),file,row.names = FALSE)
    }
  )
  
  ## Download USGS PFDS
  output$USGS_PFDS_download=downloadHandler(
    filename=function(){
      paste(input$counter,"_PFDS",".csv",sep="")
    },
    content=function(file){
      write.csv(USGS_PFDS()[[match(input$counter,options())]],file,row.names = FALSE)
    }
  )

  ## Output Combined Data Table & Create Download Button; copy combine_table so column names can be renamed with units without messing up code using original column names
  combine_table_out=reactive({
    data=combine_table()
    
    data=data[rowSums(is.na(data))!=ncol(data),] # remove rows that are all NA
    
    # Add units to column names
    colnames(data)=gsub(x=colnames(data),pattern="Precip_Inst",replacement="Precip_in")
    colnames(data)=gsub(x=colnames(data),pattern="Flow_Inst",replacement="Flow_cfs")
    colnames(data)=gsub(x=colnames(data),pattern="GH_Inst",replacement="Gauge_Height_ft")
    colnames(data)=gsub(x=colnames(data),pattern="Wtemp_Inst",replacement="Water_Temp_C")
    colnames(data)=gsub(x=colnames(data),pattern="SpecCond_Inst",replacement="Spec_Cond_microS/cm")
    colnames(data)=gsub(x=colnames(data),pattern="DO_Inst",replacement="DO_mg/L")
    colnames(data)=gsub(x=colnames(data),pattern="pH_Inst",replacement="pH")
    colnames(data)=gsub(x=colnames(data),pattern="Turb_Inst",replacement="Turbidity_FNU")
    colnames(data)=gsub(x=colnames(data),pattern="baseflow",replacement="Baseflow")
    colnames(data)=gsub(x=colnames(data),pattern="stormflow",replacement="Stormflow")
    colnames(data)=gsub(x=colnames(data),pattern="i_inhr",replacement="Precip_Intensity_in/hr")
    
    Setnames(data, old = c("Char_Date"), new = c("Date/Time"),allow.absent.cols=T)
    
    data=data[,order(names(data))] # Order columns alphabetically
    data=data[,c(which(colnames(data)=="Date/Time"),which(colnames(data)!="Date/Time"))] # Set Date/Time First
  })
  
  # Output Combine Data Table
  output$combine_table=DT::renderDataTable(datatable(combine_table_out(),options=list(scrollX=T))%>%formatRound(c(grep(x=colnames(combine_table_out()),pattern="Baseflow_cfs"),grep(x=colnames(combine_table_out()),pattern="DRO_Depth_in"),grep(x=colnames(combine_table_out()),pattern="DRO_Vol_cf"),grep(x=colnames(combine_table_out()),pattern="Stormflow_cfs")),2))
  
  # Download Handler for Combine Data Table
  output$combine_download=downloadHandler(
    filename=function(){
      paste("combined_data_",input$PrecipDate[1],"_",input$PrecipDate[2],".csv",sep="")
    },
    content=function(file){
      write.csv(combine_table_out(),file,row.names = FALSE)
    }
  )
  
  ## Output Sliding Window Data
  sw_out_formatted=reactive({
    if("ARI (years)"%in%colnames(sw_out())){
      datatable(sw_out(),options=list(scrollX=T))%>%formatRound(c("ARI (years)", "Precipitation Depth (in)","Precipitation Intensity (in/hr)"),2)
    }
    else{
      datatable(sw_out(),options=list(scrollX=T))
    }
  })
  
  output$sw=DT::renderDataTable(sw_out_formatted())
  
  output$ari_download=downloadHandler(
    filename=function(){
      paste("ARI_",input$PrecipDate[1],"_",input$PrecipDate[2],".csv",sep="")
    },
    content=function(file){
      write.csv(sw_out(),file,row.names = FALSE)
    }
  )
  
  ## Output Summary Table
  output$summary_table=DT::renderDataTable(datatable(summary_table(),rownames=F,options=list(scrollX=T))%>%formatRound(c("Total DRO Volume (cf)"),0)%>%formatRound(c("Total DRO Depth (in)"),3)%>%formatRound(c("Total Precipitation (in)","Maximum Intensity (in/hr)"),2))
  
  
  ## Output Runoff Volume Coefficient Summary
  output$RV=DT::renderDataTable(datatable(rv_table(),rownames=F,options=list(scrollX=T))%>%formatRound(c(unlist(precip_site_key[input$USGS_Precip],use.names=F),unlist(precip_site_key[input$user.faaid],use.names=F),input$USGS_Precip[which(!input$USGS_Precip%in%names(precip_site_key))],input$user.faaid[which(!input$user.faaid%in%names(precip_site_key))]),3))
  
  ## Output Hyetograph & Hydrograph
  
  #' To make the precipitation depth legend match the blue used in the hyetograph, the colors must be manually specified. However, to still get dynamic colors, a function is created to
  #' replicate the default ggplot color scale.
  #' 
  #' The legend item for Precipitation is 0bserved Precipitation with a 'zero' to force it to be first alphabetically  

  # Generate Color Function 
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length=n+1)
    # hues = seq(15, 375, length=6) #if you want to plot colors by dataset and want the colors to be the same for plotting one site and for plotting one site+wq, then set length= to some number However, the colors won't plot the same if you do two sites+wq thought b/c it just applies color based on the order of the datasets and adding the wq messes up the order
    hcl(h=hues, l=65, c=100)[1:n]
  }

  hydro=reactive({
    # Plot if flow data doesn't exist; flow data not downloaded or flow data all NA
    if(length(grep(x=colnames(plot_table()),pattern="Flow_Inst"))==0|all(is.na(plot_table()[grep(x=colnames(plot_table()),pattern="Flow_Inst")]))==T){
      if(input$user.wq=="None"){
        
        # Get data
        data=subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))
        
        # Generate Color Values
        adj_names=(paste("0bserved ",input$user.gauge," Precipitation",sep=""))
        
        # Plot Colors by Station/Dataset
        values = gg_color_hue(length(adj_names))
        names(values) = adj_names
        values[[paste("0bserved ",input$user.gauge," Precipitation",sep="")]]="blue" # Specify that precip depth is blue to match hyetograph
        
        ggplot(data,aes(x=dateTime))+
          geom_line(aes(x=as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC")[1]-1,y=0,color=paste("0bserved ",input$user.gauge," Precipitation",sep="")))+ # Add line for Precipitation
          scale_x_datetime(limits=c(sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"))[1],sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"),decreasing=TRUE)[1]))+
          xlab("Date/Time")+
          ylab("Discharge (cfs) Data Unavailable \n \n")+
          scale_color_manual(values=values)+
          labs(color="")+ # changes legend title
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=14,face='bold'),
                legend.position="bottom",
                legend.title=element_text(size=14, face='bold'),
                legend.text=element_text(size=12))+
          guides(color=guide_legend(override.aes = list(linetype='solid')))
      }
      else{
        # If user selects to plot water quality data and water quality data exists for at least one site
        if(unlist(plot_key[input$user.wq],use.names=F) %in% colnames(plot_table())){
          wq=plot_table()[,c("dateTime","site_no","Location",toString(unlist(plot_key[input$user.wq],use.names=F)))]
          
          # Get data
          data=subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(toString(input$user.wq))))
          
          # Generate Color Values
          adj_names=sort(setdiff(c(paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))$Location),unlist(hydro_legend_key[input$user.wq],use.names=F))),paste("0bserved ",input$user.gauge," Precipitation",sep="")))
          
          # Plot Colors by Station/Dataset
            values = gg_color_hue(length(adj_names))
            names(values) = adj_names
            values[[paste("0bserved ",input$user.gauge," Precipitation",sep="")]]="blue" # Specify that precip depth is blue to match hyetograph

          # Create list of linetypes to format legend
            style=rep('1F',each=(length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))$Location))))
            style=c(style,'solid') # add line type for precipitation

          plot=ggplot(data,aes(x=dateTime))+
            geom_line(aes(x=as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC")[1]-1,y=0,color=paste("0bserved ",input$user.gauge," Precipitation",sep="")))+ # Add line for Precipitation
            geom_point(data=subset(wq,site_no %in% input$USGS_Site),aes_q(x=~dateTime,y=as.name(unlist(plot_key[input$user.wq],use.names=F)),color=~paste(Location,unlist(hydro_legend_key[input$user.wq],use.names=F))),na.rm=TRUE)+
            scale_x_datetime(limits=c(sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"))[1],sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"),decreasing=TRUE)[1]))+
            xlab("Date/Time")+
            ylab(paste(toString(wqkey[unlist(plot_key[input$user.wq],use.names=F)]),"\n \n",sep=" "))+
            scale_color_manual(values=values)+
            labs(color="")+ # changes legend title
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face='bold'),
                  legend.position="bottom",
                  legend.title=element_text(size=14, face='bold'),
                  legend.text=element_text(size=12))+
            guides(color=guide_legend(override.aes = list(linetype=style)))
          plot
        }
        
        # Else if user selects to plot water quality data and water quality data doesn't exist for any site
        else{
          # Get data
          data=subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))

          # Generate Color Values
          adj_names=paste("0bserved ",input$user.gauge," Precipitation",sep="")
          
          # Plot Colors by Station/Dataset
          values = gg_color_hue(length(adj_names))
          names(values) = adj_names
          values[[paste("0bserved ",input$user.gauge," Precipitation",sep="")]]="blue" # Specify that precip depth is blue to match hyetograph
          
          plot=ggplot(data,aes(x=dateTime))+
            geom_line(aes(x=as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC")[1]-1,y=0,color=paste("0bserved ",input$user.gauge," Precipitation",sep="")))+ # Add line for Precipitation
            scale_x_datetime(limits=c(sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"))[1],sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"),decreasing=TRUE)[1]))+
            xlab("Date/Time")+
            ylab(paste(toString(wqkey[unlist(plot_key[input$user.wq],use.names=F)]),"Data Unavailable"," \n \n",sep=" "))+
            scale_color_manual(values=values)+
            labs(color="")+ # changes legend title
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face='bold'),
                  legend.position="bottom",
                  legend.title=element_text(size=14, face='bold'),
                  legend.text=element_text(size=12))+
            guides(color=guide_legend(override.aes = list(linetype='solid')))
          plot
        }
      }
    }
    # Plot if flow data does exist
    else{
      if(input$user.wq=="None"){
        
        # Get data
        data=subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))
        
        # Generate Color Values
        adj_names=sort(setdiff(c(paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location),'Discharge'),paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location),'Baseflow')),paste("0bserved ",input$user.gauge," Precipitation",sep="")))
        
        # Plot Colors by Station
        if(input$plot_color==0){
          values = rep(gg_color_hue(length(adj_names)/2),each=2) # Repeat Each Value 2 times
          names(values) = adj_names
          values[[paste("0bserved ",input$user.gauge," Precipitation",sep="")]]="blue" # Specify that precip depth is blue to match hyetograph
        }
        
        # Plot Colors by Dataset
        if(input$plot_color==1){
          values = gg_color_hue(length(adj_names))
          names(values) = adj_names
          values[[paste("0bserved ",input$user.gauge," Precipitation",sep="")]]="blue" # Specify that precip depth is blue to match hyetograph
        }
        
        
        # Create list of linetypes to format legend
        style=c('solid')
        for(i in 1:length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location))){
          style=c(style,'3313','solid')
        }
        
        ggplot(data,aes(x=dateTime))+
          geom_line(aes(y=Flow_Inst, color=paste(Location,"Discharge")),size=1)+
          geom_line(aes(y=baseflow_cfs, color=paste(Location, 'Baseflow')),size=1,linetype="3313")+
          geom_line(aes(x=as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC")[1]-1,y=0,color=paste("0bserved ",input$user.gauge," Precipitation",sep="")))+ # Add line for Precipitation
          scale_x_datetime(limits=c(sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"))[1],sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"),decreasing=TRUE)[1]))+
          xlab("Date/Time")+
          ylab("Discharge (cfs) \n \n")+
          scale_color_manual(values=values)+
          labs(color="")+ # changes legend title
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=14,face='bold'),
                legend.position="bottom",
                legend.title=element_text(size=14, face='bold'),
                legend.text=element_text(size=12))+
          guides(color=guide_legend(override.aes = list(linetype=style)))
      }
      else{
        # If user selects to plot water quality data and water quality data exists for at least one site
        if(unlist(plot_key[input$user.wq],use.names=F) %in% colnames(plot_table())){
          wq=plot_table()[,c("dateTime","site_no","Location",toString(unlist(plot_key[input$user.wq],use.names=F)))]
          wq[,toString(unlist(plot_key[input$user.wq],use.names=F))]=wq[,toString(unlist(plot_key[input$user.wq],use.names=F))]*(max(plot_table()$Flow_Inst,na.rm=TRUE)/(max(plot_table()[,c(toString(unlist(plot_key[input$user.wq],use.names=F)))],na.rm=TRUE)))
          
          # Get data
          data=subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst))&(!is.na(toString(input$user.wq))))
          
          # Generate Color Values
          adj_names=sort(setdiff(c(paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))$Location),'Discharge'),paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))$Location),'Baseflow'),paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))$Location),unlist(hydro_legend_key[input$user.wq],use.names=F))),paste("0bserved ",input$user.gauge," Precipitation",sep="")))
          
          # Plot Colors by Station
          if(input$plot_color==0){
            values = rep(gg_color_hue(length(adj_names)/3),each=3) # Repeat Each Value 3 times
            names(values) = adj_names
            values[[paste("0bserved ",input$user.gauge," Precipitation",sep="")]]="blue" # Specify that precip depth is blue to match hyetograph
          }
          
          # Plot Colors by Dataset
          if(input$plot_color==1){
            values = gg_color_hue(length(adj_names))
            names(values) = adj_names
            values[[paste("0bserved ",input$user.gauge," Precipitation",sep="")]]="blue" # Specify that precip depth is blue to match hyetograph
          }
          
          # Create list of linetypes to format legend
          # Add linetypes for water quality for station that has no flow data; if statement checks for number of sites that don't have flow data
          if(length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))$Location))-length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location))!=0){
            style=rep('1F',each=(length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))$Location))-length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location))))
            style=c(style,'solid') # add line type for precipitation
          }
          else{
            style=c('solid')
          }
          
          # Add linetypes for baseflow and water quality for station that has flow data
          for(i in 1:length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location))){
            style=c(style,'3313','solid','1F')
          }
          
          plot=ggplot(data,aes(x=dateTime))+
            geom_line(aes(y=Flow_Inst,color=paste(Location,"Discharge")),size=1)+
            geom_line(aes(y=baseflow_cfs,color=paste(Location, 'Baseflow')),size=1,linetype="3313")+
            geom_line(aes(x=as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC")[1]-1,y=0,color=paste("0bserved ",input$user.gauge," Precipitation",sep="")))+ # Add line for Precipitation
            geom_point(data=subset(wq,site_no %in% input$USGS_Site),aes_q(x=~dateTime,y=as.name(unlist(plot_key[input$user.wq],use.names=F)),color=~paste(Location,unlist(hydro_legend_key[input$user.wq],use.names=F))),na.rm=TRUE)+
            scale_x_datetime(limits=c(sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"))[1],sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"),decreasing=TRUE)[1]))+
            scale_y_continuous(sec.axis=sec_axis(~.*(max(plot_table()[,c(toString(unlist(plot_key[input$user.wq],use.names=F)))],na.rm=TRUE)/max(plot_table()$Flow_Inst,na.rm=TRUE)),name=toString(wqkey[unlist(plot_key[input$user.wq],use.names=F)])))+
            xlab("Date/Time")+
            ylab("Discharge (cfs) \n \n")+
            scale_color_manual(values=values)+
            labs(color="")+ # changes legend title
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face='bold'),
                  legend.position="bottom",
                  legend.title=element_text(size=14, face='bold'),
                  legend.text=element_text(size=12))+
            guides(color=guide_legend(override.aes = list(linetype=style)))
          plot
        }
        
        # Else if user selects to plot water quality data and water quality data doesn't exist for any site
        else{
          # Get data
          data=subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst))&(!is.na(toString(input$user.wq))))
          
          # Generate Color Values
          adj_names=sort(setdiff(c(paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location),'Discharge'),paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location),'Baseflow')),paste("0bserved ",input$user.gauge," Precipitation",sep="")))
          
          # Plot Colors by Station
          if(input$plot_color==0){
            values = rep(gg_color_hue(length(adj_names)/2),each=2) # Repeat Each Value 2 times
            names(values) = adj_names
            values[[paste("0bserved ",input$user.gauge," Precipitation",sep="")]]="blue" # Specify that precip depth is blue to match hyetograph
          }
          
          # Plot Colors by Dataset
          if(input$plot_color==1){
            values = gg_color_hue(length(adj_names))
            names(values) = adj_names
            values[[paste("0bserved ",input$user.gauge," Precipitation",sep="")]]="blue" # Specify that precip depth is blue to match hyetograph
          }
          
          # Create list of linetypes to format legend
          style=c('solid')
          for(i in 1:length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location))){
            style=c(style,'3313','solid')
          }
          
          plot=ggplot(data,aes(x=dateTime))+
            geom_line(aes(y=Flow_Inst,color=paste(Location,"Discharge")),size=1)+
            geom_line(aes(y=baseflow_cfs,color=paste(Location, 'Baseflow')),size=1,linetype="3313")+
            geom_line(aes(x=as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC")[1]-1,y=0,color=paste("0bserved ",input$user.gauge," Precipitation",sep="")))+ # Add line for Precipitation
            scale_x_datetime(limits=c(sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"))[1],sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"),decreasing=TRUE)[1]))+
            scale_y_continuous(sec.axis=sec_axis(~./max(plot_table()$Flow_Inst,na.rm=TRUE),name=paste(toString(wqkey[unlist(plot_key[input$user.wq],use.names=F)]),"Data Unavailable",sep=" ")))+
            xlab("Date/Time")+
            ylab("Discharge (cfs) \n \n")+
            scale_color_manual(values=values)+
            labs(color="")+ # changes legend title
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face='bold'),
                  legend.position="bottom",
                  legend.title=element_text(size=14, face='bold'),
                  legend.text=element_text(size=12))+
            guides(color=guide_legend(override.aes = list(linetype=style)))
          plot
        }
      }
    }
  })
  
  ## Create Hyetograph to bind to Hydrograph
  USGS_hyet=reactive({
    ggplot(subset(plot_table(),(site_no %in% plot_site2())&(!is.na(Char_Date))),aes(x=dateTime,y=Precip_Inst))+
      geom_col(color='blue', size=1)+
      ylab("Precipitation Depth (in) \n \n")+
      scale_x_datetime(limits=c(sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"))[1],sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"),decreasing=TRUE)[1]))+
      scale_y_reverse()+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face='bold'),
            axis.title.x=element_blank()
            # axis.text.x=element_blank(), # Hide x-axis text 
            # axis.ticks.x=element_blank() # Hide x-axis tick marks
      )
  })
  
  output$Flow=renderPlot(grid.draw(rbind(ggplotGrob(USGS_hyet()), ggplotGrob(hydro()), size = "last")))

  output$sized_plot=renderUI(plotOutput("Flow",height=600)) #so plot has a fixed size
  
  
  ## Interactive Hydrograph; Duplicate Hydrograph but without labels for Precipitation Depth
  hydro_int=reactive({
    # Plot if flow data doesn't exist; flow data not downloaded or flow data all NA
    if(length(grep(x=colnames(plot_table()),pattern="Flow_Inst"))==0|all(is.na(plot_table()[grep(x=colnames(plot_table()),pattern="Flow_Inst")]))==T){
      if(input$user.wq=="None"){
        
        # Get data
        data=subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))
        
        # Generate Color Values
        adj_names=(paste("0bserved ",input$user.gauge," Precipitation",sep=""))
        
        # Plot Colors by Station/Dataset
        values = gg_color_hue(length(adj_names))
        names(values) = adj_names
        values[[paste("0bserved ",input$user.gauge," Precipitation",sep="")]]="blue" # Specify that precip depth is blue to match hyetograph
        
        ggplot(data,aes(x=dateTime))+
          geom_line(aes(x=as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC")[1]-1,y=0,color=paste("0bserved ",input$user.gauge," Precipitation",sep="")))+ # Add line for Precipitation
          scale_x_datetime(limits=c(sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"))[1],sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"),decreasing=TRUE)[1]))+
          xlab("Date/Time")+
          ylab("Discharge (cfs) Data Unavailable \n \n")+
          scale_color_manual(values=values)+
          labs(color="")+ # changes legend title
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=14,face='bold'),
                legend.position="none", #hide legend
                legend.title=element_text(size=14, face='bold'),
                legend.text=element_text(size=12))+
          guides(color=guide_legend(override.aes = list(linetype='solid')))
      }
      else{
        # If user selects to plot water quality data and water quality data exists for at least one site
        if(unlist(plot_key[input$user.wq],use.names=F) %in% colnames(plot_table())){
          wq=plot_table()[,c("dateTime","site_no","Location",toString(unlist(plot_key[input$user.wq],use.names=F)))]
          
          # Get data
          data=subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(toString(input$user.wq))))
          
          # Generate Color Values
          adj_names=sort(paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))$Location),unlist(hydro_legend_key[input$user.wq],use.names=F)))
          
          # Plot Colors by Station/Dataset
          values = gg_color_hue(length(adj_names))
          names(values) = adj_names

          plot=ggplot(data,aes(x=dateTime))+
            geom_line(aes(x=as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC")[1]-1,y=0))+ # Add line for Precipitation
            geom_point(data=subset(wq,site_no %in% input$USGS_Site),aes_q(x=~dateTime,y=as.name(unlist(plot_key[input$user.wq],use.names=F)),color=~paste(Location,unlist(hydro_legend_key[input$user.wq],use.names=F))),na.rm=TRUE)+
            scale_x_datetime(limits=c(sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"))[1],sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"),decreasing=TRUE)[1]))+
            xlab("Date/Time")+
            ylab(paste(toString(wqkey[unlist(plot_key[input$user.wq],use.names=F)]),"\n \n",sep=" "))+
            scale_color_manual(values=values)+
            labs(color="")+ # changes legend title
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face='bold'),
                  legend.position="bottom",
                  legend.title=element_text(size=14, face='bold'),
                  legend.text=element_text(size=12))+
            guides(color=guide_legend(override.aes = list(linetype='1F')))
          plot
        }
        
        # Else if user selects to plot water quality data and water quality data doesn't exist for any site
        else{
          # Get data
          data=subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))
          
          # Generate Color Values
          adj_names=paste("0bserved ",input$user.gauge," Precipitation",sep="")
          
          # Plot Colors by Station/Dataset
          values = gg_color_hue(length(adj_names))
          names(values) = adj_names
          values[[paste("0bserved ",input$user.gauge," Precipitation",sep="")]]="blue" # Specify that precip depth is blue to match hyetograph
          
          plot=ggplot(data,aes(x=dateTime))+
            geom_line(aes(x=as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC")[1]-1,y=0,color=paste("0bserved ",input$user.gauge," Precipitation",sep="")))+ # Add line for Precipitation
            scale_x_datetime(limits=c(sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"))[1],sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"),decreasing=TRUE)[1]))+
            xlab("Date/Time")+
            ylab(paste(toString(wqkey[unlist(plot_key[input$user.wq],use.names=F)]),"Data Unavailable"," \n \n",sep=" "))+
            scale_color_manual(values=values)+
            labs(color="")+ # changes legend title
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face='bold'),
                  legend.position="none", #hide legend
                  legend.title=element_text(size=14, face='bold'),
                  legend.text=element_text(size=12))+
            guides(color=guide_legend(override.aes = list(linetype='solid')))
          plot
        }
      }
    }
    # Plot if flow data exists
    else{
      if(input$user.wq=="None"){
        
        # Get data
        data=subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))
        
        # Generate Color Values
        adj_names=sort(setdiff(c(paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location),'Discharge'),paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location),'Baseflow'))," Precipitation Depth"))
        
        # Plot Colors by Station
        if(input$plot_color==0){
          values = rep(gg_color_hue(length(adj_names)/2),each=2) # Repeat Each Value 2 times
          names(values) = adj_names
        }
        
        # Plot Colors by Dataset
        if(input$plot_color==1){
          values = gg_color_hue(length(adj_names))
          names(values) = adj_names
        }
        
        
        # Create list of linetypes to format legend
        style=c('solid')
        for(i in 1:length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location))){
          style=c(style,'3313','solid')
        }
        
        ggplot(data,aes(x=dateTime))+
          geom_line(aes(y=Flow_Inst, color=paste(Location,"Discharge")),size=1)+
          geom_line(aes(y=baseflow_cfs, color=paste(Location, 'Baseflow')),size=1,linetype="3313")+
          scale_x_datetime(limits=c(sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"))[1],sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"),decreasing=TRUE)[1]))+
          xlab("Date/Time")+
          ylab("Discharge (cfs) \n \n")+
          scale_color_manual(values=values)+
          labs(color="")+ # changes legend title
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=14,face='bold'),
                legend.position="bottom",
                legend.title=element_text(size=14, face='bold'),
                legend.text=element_text(size=12))+
          guides(color=guide_legend(override.aes = list(linetype=c('3313','solid'))))
      }
      else{
        # If user selects to plot water quality data and water quality data exists for at least one site
        if(unlist(plot_key[input$user.wq],use.names=F) %in% colnames(plot_table())){
          wq=plot_table()[,c("dateTime","site_no","Location",toString(unlist(plot_key[input$user.wq],use.names=F)))]
          wq[,toString(unlist(plot_key[input$user.wq],use.names=F))]=wq[,toString(unlist(plot_key[input$user.wq],use.names=F))]*(max(plot_table()$Flow_Inst,na.rm=TRUE)/(max(plot_table()[,c(toString(unlist(plot_key[input$user.wq],use.names=F)))],na.rm=TRUE)))
          
          # Get data
          data=subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst))&(!is.na(toString(input$user.wq))))
          
          # Generate Color Values
          adj_names=sort(setdiff(c(paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))$Location),'Discharge'),paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))$Location),'Baseflow'),paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))$Location),unlist(hydro_legend_key[input$user.wq],use.names=F))),paste("0bserved ",input$user.gauge," Precipitation",sep="")))
          
          # Plot Colors by Station
          if(input$plot_color==0){
            values = rep(gg_color_hue(length(adj_names)/3),each=3) # Repeat Each Value 3 times
            names(values) = adj_names
          }
          
          # Plot Colors by Dataset
          if(input$plot_color==1){
            values = gg_color_hue(length(adj_names))
            names(values) = adj_names
          }
          
          # Create list of linetypes to format legend
          # Add linetypes for water quality for station that has no flow data; if statement checks for number of sites that don't have flow data
          if(length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))$Location))-length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location))!=0){
            style=rep('1F',each=(length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no)))$Location))-length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location))))
            # Add linetypes for baseflow and water quality for station that has flow data
            for(i in 1:length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location))){
              style=c(style,'3313','solid','1F')
            }
          }
          else{ #If all sites have flow data
            style=c('3313','solid','1F')
          }
  
          plot=ggplot(data,aes(x=dateTime))+
            geom_line(aes(y=Flow_Inst,color=paste(Location,"Discharge")),size=1)+
            geom_line(aes(y=baseflow_cfs,color=paste(Location, 'Baseflow')),size=1,linetype="3313")+
            geom_point(data=subset(wq,site_no %in% input$USGS_Site),aes_q(x=~dateTime,y=as.name(unlist(plot_key[input$user.wq],use.names=F)),color=~paste(Location,unlist(hydro_legend_key[input$user.wq],use.names=F))),na.rm=TRUE)+
            scale_x_datetime(limits=c(sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"))[1],sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"),decreasing=TRUE)[1]))+
            scale_y_continuous(sec.axis=sec_axis(~.*(max(plot_table()[,c(toString(unlist(plot_key[input$user.wq],use.names=F)))],na.rm=TRUE)/max(plot_table()$Flow_Inst,na.rm=TRUE)),name=toString(wqkey[unlist(plot_key[input$user.wq],use.names=F)])))+
            xlab("Date/Time")+
            ylab("Discharge (cfs) \n \n")+
            scale_color_manual(values=values)+
            labs(color="")+ # changes legend title
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face='bold'),
                  legend.position="bottom",
                  legend.title=element_text(size=14, face='bold'),
                  legend.text=element_text(size=12))+
            # guides(color=guide_legend(override.aes = list(linetype=c('3313','solid','1F'))))
            guides(color=guide_legend(override.aes = list(linetype=style)))
          plot
        }
        
        # Else if user selects to plot water quality data and water quality data doesn't exist for any site
        else{
          # Get data
          data=subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst))&(!is.na(toString(input$user.wq))))
          
          # Generate Color Values
          adj_names=sort(setdiff(c(paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location),'Discharge'),paste(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location),'Baseflow'))," Precipitation Depth"))
          
          # Plot Colors by Station
          if(input$plot_color==0){
            values = rep(gg_color_hue(length(adj_names)/2),each=2) # Repeat Each Value 2 times
            names(values) = adj_names
          }
          
          # Plot Colors by Dataset
          if(input$plot_color==1){
            values = gg_color_hue(length(adj_names))
            names(values) = adj_names
          }
          
          # Create list of linetypes to format legend
          style=c('solid')
          for(i in 1:length(unique(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst)))$Location))){
            style=c(style,'3313','solid')
          }
          
          plot=ggplot(data,aes(x=dateTime))+
            geom_line(aes(y=Flow_Inst,color=paste(Location,"Discharge")),size=1)+
            geom_line(aes(y=baseflow_cfs,color=paste(Location, 'Baseflow')),size=1,linetype="3313")+
            scale_x_datetime(limits=c(sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"))[1],sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"),decreasing=TRUE)[1]))+
            scale_y_continuous(sec.axis=sec_axis(~./max(plot_table()$Flow_Inst,na.rm=TRUE),name=paste(toString(wqkey[unlist(plot_key[input$user.wq],use.names=F)]),"Data Unavailable",sep=" ")))+
            xlab("Date/Time")+
            ylab("Instantaneous Flow (cfs) \n \n")+
            scale_color_manual(values=values)+
            labs(color="")+ # changes legend title
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face='bold'),
                  legend.position="bottom",
                  legend.title=element_text(size=14, face='bold'),
                  legend.text=element_text(size=12))+
            guides(color=guide_legend(override.aes = list(linetype=c('3313','solid'))))
          plot
        }
      }
    }
  })
  
  output$hydro_interact=renderPlot({hydro_int()})
  
  output$hydro_click=DT::renderDataTable({
    if("Flow_Inst"%in%colnames(plot_table())){
      data=nearPoints(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Flow_Inst))),input$hydro_click,threshold=10,maxpoints=1)
      data=SubsetCols(data,c("Char_Date","Location","site_no","baseflow_cfs",unlist(combine_key[input$parameterCd],use.names=F)),allow.absent.cols=TRUE)
    }
    else{
      data=nearPoints(plot_table(),input$hydro_click,threshold=10,maxpoints=1)
      data=nearPoints(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))),input$hydro_click,threshold=10,maxpoints=1)
      data=SubsetCols(data,c("Char_Date","Location","site_no",unlist(combine_key[input$parameterCd],use.names=F)),allow.absent.cols=TRUE)
    }
    names=unlist(combine_key[input$parameterCd],use.names=F) # Get USGS Parameters that aren't Flow_Inst b/c wq_key doesn't include Discharge so you can't plot Discharge on 2nd y-axis
    data=Setnames(data, c("Char_Date","site_no","baseflow_cfs","Flow_Inst",names[names!="Flow_Inst"]), c("Date","Station","Baseflow (cfs)","Discharge (cfs)",unlist(wq_key[input$parameterCd],use.names=F)),allow.absent.cols=T)
    
    if("Flow_Inst"%in%colnames(plot_table())){ # Round values if flow is downloaded
      datatable(data,rownames=F,options=list(scrollX=T))%>%formatRound(c("Baseflow (cfs)","Discharge (cfs)"),2)
    }
    else{
      datatable(data,rownames=F,options=list(scrollX=T))
    }
  })
  
  # Create Hydro Brush Table
  hydro_brush_table=reactive({
    data=brushedPoints(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Char_Date))),input$hydro_brush,xvar="dateTime")
    
    data=SubsetCols(data,c("Char_Date","Location","site_no","baseflow_cfs",unlist(combine_key[input$parameterCd],use.names=F)),allow.absent.cols=TRUE)
    
    # Get only rows where there is at least one data point
    data=data[rowSums(is.na(data[c("baseflow_cfs",colnames(data)[which(names(data)%in%unlist(combine_key[input$parameterCd],use.names=F))])]))!=(length(colnames(data)[which(names(data)%in%unlist(combine_key[input$parameterCd],use.names=F))])+1),]

    # Remove baseflow from table if discharge data not downloaded
    if(!"Flow_Inst"%in%colnames(plot_table())){
      data=data[,-match("baseflow_cfs",names(data))]
    }
    
    names=unlist(combine_key[input$parameterCd],use.names=F) # Get USGS Parameters that aren't Flow_Inst b/c wq_key doesn't include Discharge so you can't plot Discharge on 2nd y-axis
    data=Setnames(data, c("Char_Date","site_no","baseflow_cfs","Flow_Inst",names[names!="Flow_Inst"]), c("Date","Station","Baseflow (cfs)","Discharge (cfs)",unlist(wq_key[input$parameterCd],use.names=F)),allow.absent.cols=T)
  
    data
  })
  
  # Calculate Volume of Runoff and Baseflow
  hydro_vols=reactive({
    data=brushedPoints(subset(plot_table(),(site_no %in% input$USGS_Site)&(!is.na(site_no))&(!is.na(Char_Date))),input$hydro_brush,xvar="dateTime")
    
    data=SubsetCols(data,c("Char_Date","Location","site_no","baseflow_cfs",unlist(combine_key[input$parameterCd],use.names=F)),allow.absent.cols=TRUE)
    
    # Get only rows where there is at least one data point
    data=data[rowSums(is.na(data[c("baseflow_cfs",colnames(data)[which(names(data)%in%unlist(combine_key[input$parameterCd],use.names=F))])]))!=(length(colnames(data)[which(names(data)%in%unlist(combine_key[input$parameterCd],use.names=F))])+1),]
    
    data$dateTime=as.numeric(as.POSIXct(data$Char_Date, origin = "1970-01-01 00:00.00 UTC"))
    
    if("Flow_Inst"%in%colnames(plot_table())){
      data%>%group_by(Location)%>%
        summarize(`Discharge Volume (cf)`=trapz(dateTime,Flow_Inst),
                  `Baseflow Volume (cf)`=trapz(dateTime,baseflow_cfs),
                  `Runoff Volume (cf)`=trapz(dateTime,Flow_Inst)-trapz(dateTime,baseflow_cfs)
        )
    }
    else{
      data%>%group_by(Location)%>%
        summarize(`Discharge Volume (cf)`=NA,
                  `Baseflow Volume (cf)`=NA,
                  `Runoff Volume (cf)`=NA
        )
    }
  })
  
  output$hydro_brush_vol=DT::renderDataTable({datatable(hydro_vols(),rownames=F,options=list(scrollX=T))%>%formatRound(c("Discharge Volume (cf)","Baseflow Volume (cf)","Runoff Volume (cf)"),0)})
  
  # Output Hydro Brush Table
  hydro_brush_table_format=reactive({
    if("Flow_Inst"%in%colnames(plot_table())){ # Round values if flow is downloaded
      datatable(hydro_brush_table(),options=list(scrollX=T))%>%formatRound(c("Baseflow (cfs)","Discharge (cfs)"),2)
    }
    else{
      datatable(hydro_brush_table(),options=list(scrollX=T))
    }
  })
  
  output$hydro_brush=DT::renderDataTable({hydro_brush_table_format()})
  
  # Download handler for Hydro Brush table
  output$hydro_brush_download <- downloadHandler(
    filename=function(){
      paste("hydro_brush_",input$PrecipDate[1],"-",input$PrecipDate[2],".csv",sep="")
    },
    content = function(fname) {
      write.csv(hydro_brush_table(),fname,row.names=FALSE)
    }
  )
  
  ## Interactive Hyetograph
  USGS_hyet_int=reactive({
    values=gg_color_hue(1)
    values[[paste("0bserved ",input$user.gauge," Precipitation",sep="")]]="blue"
    
    plot=ggplot(subset(plot_table(),(site_no %in% plot_site2())&(!is.na(Char_Date))),aes(x=dateTime,y=Precip_Inst))+
      geom_col(aes(color=paste("0bserved ",input$user.gauge," Precipitation",sep="")), size=1)+
      xlab("Date/Time")+
      ylab("Precipitation Depth (in) \n \n")+
      scale_x_datetime(limits=c(sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"))[1],sort(as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC"),decreasing=TRUE)[1]))+
      scale_y_reverse()+
      scale_color_manual(values=values)+
      labs(color="")+ # changes legend title
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face='bold'),
            legend.position="bottom",
            legend.title=element_text(size=14, face='bold'),
            legend.text=element_text(size=12))
    plot
  })
  
  output$hyet_interact=renderPlot({USGS_hyet_int()})
  
  hyet_data=reactive({
    data=brushedPoints(subset(plot_table(),(site_no %in% plot_site2())&(!is.na(Char_Date))),input$hyet_brush,xvar="dateTime")
    data=SubsetCols(data,c("Char_Date","Location","site_no","Precip_Inst","i_inhr"),allow.absent.cols=TRUE)
  })
  
  # Create Hyet Brush Table
  hyet_brush_table=reactive({
    data=subset(hyet_data(),Precip_Inst!=0)
    data=setnames(data,old=c("Char_Date","site_no","Precip_Inst","i_inhr"),new=c("Date","Station","Precip. Depth (in)","Precip. Intensity (in/hr)"))
  })
  
  # Output Hyet Brush Table
  output$hyet_brush=DT::renderDataTable({datatable(hyet_brush_table(),options=list(scrollX=T))%>%formatRound(c("Precip. Depth (in)","Precip. Intensity (in/hr)"),2)})
  
  # Download handler for Hyet Brush table
  output$hyet_brush_download <- downloadHandler(
    filename=function(){
      paste("hyet_brush_",input$PrecipDate[1],"-",input$PrecipDate[2],".csv",sep="")
    },
    content = function(fname) {
      write.csv(hyet_brush_table(),fname,row.names=FALSE)
    }
  )
  
  # Calculate Total Precipitation Depth for brushed data
  output$hyet_depth=renderText({paste("Total Precipitation Depth of Brushed Data: ",sum(hyet_data()$"Precip_Inst",na.rm=TRUE),'"',sep="")}) 
  
  dur=reactive({ # Calculate duration between brushed datapoints
    if (length(input$hyet_brush)!=0){ # If the user brushes the data
      time.interval=as.POSIXct(hyet_data()[1,"Char_Date"],origin = "1970-01-01 00:00.00 UTC") %--% as.POSIXct(hyet_data()[nrow(hyet_data()),"Char_Date"],origin = "1970-01-01 00:00.00 UTC")
    } else { # If the user does not brush any data, set duration = 0
      time.interval=0
    }
    time.period=as.period(time.interval)
    time.period
  })
  
  output$hyet_dur=renderText({paste("Duration of Brushed Data:",dur(),sep=" ")})
  
  # Calculate Precipitation Intensity for brushed data
  output$hyet_int=renderText({
    if (length(input$hyet_brush)!=0){ # If the user brushes the data
      paste("Average Intensity of Brushed Data: ",round(as.numeric(sum(hyet_data()$"Precip_Inst",na.rm=TRUE))/as.numeric(difftime(as.POSIXct(hyet_data()[nrow(hyet_data()),"Char_Date"],origin = "1970-01-01 00:00.00 UTC"),as.POSIXct(hyet_data()[1,"Char_Date"],origin = "1970-01-01 00:00.00 UTC"),units="hours")),digits=2)," (in/hr)",sep="") 
    } else { # If the user does not brush any data, set duration = 0
      paste("Average Intensity of Brushed Data: ",0," (in/hr)",sep="")} 
  })
  
  # Calculate Max Intensity for brushed data
  output$hyet_max_int=renderText({
    if (length(input$hyet_brush)!=0 & length(hyet_brush_table()$Date>=1)){ # If the user brushes the data
      paste("Maximum Intensity of Brushed Data: ",round(max(hyet_data()$i_inhr,na.rm=T),digits=2)," in/hr at ",toString(hyet_data()[which(hyet_data()$i_inhr==max(hyet_data()$i_inhr,na.rm = T)),]$Char_Date),sep="")
    } else { # If the user does not brush any data, set duration = 0
      paste("Maximum Intensity of Brushed Data: ",0," in/hr",sep="")}
  })
  
  ## Display user's Google display name after successful login
  output$display_username <- renderText({
    validate(
      need(userDetails(), "Getting User Details")
    )
    paste("User:",userDetails()$displayName,sep=" ")
  })
  
  ## Create Plot of HOBO data
  HOBO_graph=reactive({
    # Subset Data
    data=subset(HOBO_data(),(Sensor %in% input$HOBO_sensor)&(DateTime>=ymd(toString(input$PrecipDate[1]),tz="America/New_York"))&(DateTime<ymd(toString(input$PrecipDate[2]+1),tz="America/New_York")))
    
    # Generate Color Values
    adj_names=sort(unique(data$Sensor))
    
    # Plot Colors
    values = gg_color_hue(length(adj_names))
    names(values) = adj_names
    values[[paste("0bserved ",input$HOBO.gauge," Precipitation",sep="")]]="blue" # Specify that precip depth is blue to match hyetograph
    
    # Create variable to store plot
    plot=ggplot(data,aes(x=DateTime))+
      geom_line(aes_string(y=input$HOBO_data,color="Sensor"),size=1)+
      geom_line(aes(x=as.POSIXct(plot_table()$Char_Date, origin = "1970-01-01 00:00.00 UTC")[1]-1,y=0,color=paste("0bserved ",input$HOBO.gauge," Precipitation",sep="")))+ # Add line for Precipitation
      xlab("Date/Time")+
      ylab(paste(unlist(HOBO_key[input$HOBO_data],use.names = F),"\n","\n","\n",sep=" "))+
      scale_color_manual(values=values)+
      labs(color="")+ # changes legend title
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face='bold'),
            legend.position="bottom",
            legend.title=element_text(size=14, face='bold'),
            legend.text=element_text(size=12))
    
    # If there is data then set the x-axis limits this way
    if(nrow(data)!=0){
      # If Start Date = End Date, hyetograph downloads until 24:00 start date, but if Start Date != End Date, hyetograph downloads data until 0:00 End Date; this if statement makes sure hyetograph will download for same date range as hydrograph
      if(input$PrecipDate[1]==input$PrecipDate[2]){
        plot=plot+scale_x_datetime(limits=c(ymd(toString(input$PrecipDate[1]),tz="America/New_York"),ymd(toString(input$PrecipDate[2]+1),tz="America/New_York"))) #This aligns correctly if there is data
      }
      else{
        plot=plot+scale_x_datetime(limits=c(ymd(toString(input$PrecipDate[1]),tz="America/New_York"),ymd(toString(input$PrecipDate[2]),tz="America/New_York"))) #This aligns correctly if there is data
      }
    }
    # If there is no data, the x-axis limits don't align with the ones for the hyetograph, so specify same limits as hyetograph
    else{
      # If Start Date = End Date, hyetograph downloads until 24:00 start date, but if Start Date != End Date, hyetograph downloads data until 0:00 End Date; this if statement makes sure hyetograph will download for same date range as hydrograph
      if(input$PrecipDate[1]==input$PrecipDate[2]){
        plot=plot+scale_x_datetime(limits=c(as.POSIXct(toString(input$PrecipDate[1]), origin = "1970-01-01 00:00.00 UTC"),as.POSIXct(toString(input$PrecipDate[2]+1), origin = "1970-01-01 00:00.00 UTC")))
      }
      else{
        plot=plot+scale_x_datetime(limits=c(as.POSIXct(toString(input$PrecipDate[1]), origin = "1970-01-01 00:00.00 UTC"),as.POSIXct(toString(input$PrecipDate[2]), origin = "1970-01-01 00:00.00 UTC")))
      }
    }
    plot
  })
  
  ## Create Hyetograph to bind to HOBO plot
  HOBO_hyet=reactive({
    plot=ggplot(subset(plot_table(),site_no %in% plot_site3()),aes(x=dateTime,y=Precip_Inst))+
      geom_col(color='blue', size=1)+
      ylab("Precipitation Depth (in) \n \n \n")+
      scale_y_reverse()+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face='bold'),
            axis.title.x=element_blank()
            # axis.text.x=element_blank(), # Hide x-axis text 
            # axis.ticks.x=element_blank() # Hide x-axis tick marks
      )
    # If Start Date = End Date, hyetograph downloads until 24:00 start date, but if Start Date != End Date, hyetograph downloads data until 0:00 End Date; this if statement makes sure hyetograph will download for same date range as hydrograph
    if(input$PrecipDate[1]==input$PrecipDate[2]){
      plot=plot+scale_x_datetime(limits=c(as.POSIXct(toString(input$PrecipDate[1]), origin = "1970-01-01 00:00.00 UTC"),as.POSIXct(toString(input$PrecipDate[2]+1), origin = "1970-01-01 00:00.00 UTC")))
    }
    else{
      plot=plot+scale_x_datetime(limits=c(as.POSIXct(toString(input$PrecipDate[1]), origin = "1970-01-01 00:00.00 UTC"),as.POSIXct(toString(input$PrecipDate[2]), origin = "1970-01-01 00:00.00 UTC")))
    }
    plot
  })
  
  output$HOBO_plot=renderPlot(grid.draw(rbind(ggplotGrob(HOBO_hyet()), ggplotGrob(HOBO_graph()), size = "last")))
  
  output$HOBOsized_plot=renderUI(plotOutput("HOBO_plot",height=600)) #so plot has a fixed size
  
  ## Display HOBO Data in Table and Download File
  output$HOBO_Table=DT::renderDataTable({datatable(HOBO_data_out(),options=list(scrollx=T))%>%formatRound(c("Sensor Stage (ft)","Sensor Stage (in)","Relative Depth","Relative Discharge"),3)})
  
  output$hobo_download=downloadHandler(
    filename=function(){
      paste("stormsewer_data_",input$PrecipDate[1],"_",input$PrecipDate[2],".csv",sep="")
    },
    content=function(file){
      write.csv(HOBO_data_out(),file,row.names = FALSE)
    }
  )
  
  ## Forecast Table
  output$forecast=DT::renderDataTable({datatable(forecast(),options=list(scrollX=T))%>%formatRound(c("Precip. Depth (in)"),2)})
  
  ## Current Weather Conditions Sticker
  output$sticker=renderText(Sticker())
  
  ## Create Map Output for Map Tab
  output$map=renderLeaflet({
    map=leaflet()%>%
      addTiles()%>%
      addAwesomeMarkers(lng=map_data()$Longitude, # Marker Longitude
                        lat=map_data()$Latitude,  # Marker Latitude
                        icon=map_icons(),         # Marker Icons
                        clusterOptions=markerClusterOptions(showCoverageOnHover=F), # Cluster Markers when zooming out
                        popup=map_data()$Label
      )%>%
      addControl(html=markerLegendHTML(IconSet=legend_icons()),position="bottomright")%>%
      fitBounds(lng1=min(map_data()$Longitude),lng2=max(map_data()$Longitude),lat1=min(map_data()$Latitude),lat2=max(map_data()$Latitude))
    map
  })
  
  ## Create HOBO Map Output
  output$hobo_map=renderLeaflet({
    if(length(input$HOBO_sensor)==1){ # Only one sensor <- use different view options for just one point vs. multiple points
      map=leaflet(options=leafletOptions(minZoom=17,maxZoom = 19))%>%
        addTiles()%>%
        addPolylines(data=sewer())%>% # Add storm sewer shapefile
        addAwesomeMarkers(lng=hobo_map_data()$Longitude, # Marker Longitude
                          lat=hobo_map_data()$Latitude,  # Marker Latitude
                          icon=awesomeIcons(icon="adjust",
                                            iconColor="#ffffff",
                                            iconRotate=90,
                                            library="fa",
                                            markerColor="black"), # Marker Icons
                          popup=hobo_map_data()$Label
        )%>%
        addControl(html=hobo_legend(),position="bottomright")%>%
        setView(lng=mean(hobo_map_data()$Longitude),lat=mean(hobo_map_data()$Latitude),zoom=18)
    }
    else{ # Multiple Sensors
      map=leaflet(options=leafletOptions(minZoom=17,maxZoom = 19))%>%
        addTiles()%>%
        addPolylines(data=sewer())%>% # Add storm sewer shapefile
        addAwesomeMarkers(lng=hobo_map_data()$Longitude, # Marker Longitude
                          lat=hobo_map_data()$Latitude,  # Marker Latitude
                          icon=awesomeIcons(icon="adjust",
                                            iconColor="#ffffff",
                                            iconRotate=90,
                                            library="fa",
                                            markerColor="black"), # Marker Icons
                          clusterOptions=markerClusterOptions(showCoverageOnHover=F,spiderfyOnMaxZoom = T), # Cluster Markers when zooming out
                          popup=hobo_map_data()$Label
        )%>%
        addControl(html=hobo_legend(),position="bottomright")%>%
        fitBounds(lng1=min(hobo_map_data()$Longitude)-0.0001,lng2=max(hobo_map_data()$Longitude)+0.0001,lat1=min(hobo_map_data()$Latitude)-0.0001,lat2=max(hobo_map_data()$Latitude)+0.0001)
    }
    map
  })
  
  # Flood Stage Plots
  output$flood_lick_run=renderPlot(flood_lick_run())
  output$flood_tinker=renderPlot(flood_tinker())
  output$flood_rr1=renderPlot(flood_rr1())
  output$flood_rr2=renderPlot(flood_rr2())
  output$flood_rr3=renderPlot(flood_rr3())
  
  # Real Time Flood Stage Plots
  output$gh_lick_run=renderPlot(gh_lick_run())
  output$gh_tinker=renderPlot(gh_tinker())
  output$gh_rr1=renderPlot(gh_rr1())
  output$gh_rr2=renderPlot(gh_rr2())
  output$gh_rr3=renderPlot(gh_rr3())

}

### Run the app __________________________________________________________________________________________________________________________
shinyApp(ui=ui,server=server)

