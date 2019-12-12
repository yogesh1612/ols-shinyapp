####################################################
#      Summary & OLS App                           #
####################################################

library("shiny")
#library("foreign")
library(DT)

shinyUI(pageWithSidebar(
  # Header:
  headerPanel("OLS App"),
  # Input in sidepanel:
  sidebarPanel(

    h5(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header)"),
    fileInput("filep", "Upload prediction data (csv file with header)"),
    h5(p("Data Selection")),
    htmlOutput("yvarselect"),
    htmlOutput("xvarselect"),
    htmlOutput("fxvarselect"),
    br()
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                
                tabPanel("Overview",
                         h4(p("How to use this shiny application")),
                         p("This shiny application require one data input from the user. To do so, click on the Browse (in left side-bar panel) and upload the csv data input file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names.",align="justify"),
                         p("Once csv file is uploaded successfully, variables in the data file will reflect in left-side Data Selection Panel. Now you can select 
                            dependent variable (Y Variable) from drop-down menu. By default all other remaining variables will be selected as explanatory variables (X variables). 
                              If you want to drop any variable from explanatory variables, just uncheck that variable and it will be dropped from the model. 
                            If any of the variables selected in explanatory variables is a factor variable, you can define that variable as factor variable just
                            by selecting that variable in the last list of variables
                           ",align="justify"),
                         br(),
                         h4(p("Download Sample Input Files")),
                         # br(),
                         downloadButton('downloadData', 'Download model training input file (works only in browsers)'),
                         br(),
                         br(),
                         downloadButton('downloadData2', 'Download prediction input file (works only in browsers)'),
                         h5("Description of variables in sample file is as follows-"),
                         h5('week_num        =    week serial number'),
                         h5('sku_num         =    SKU serial number'),
                         h5('beer_brand      =    brand names of beers'),
                         h5('sku_size_oz     =    SKU size in fluid ounces'),
                         h5('bottle          =    whether bottle or can'),
                         h5('light_color     =    Whether Light colored beer'),
                         h5('amber_color     =    Whether Amber colored beer'),
                         h5('golden_color    =    Whether Golden colored beer'),
                         h5('lite_beer       =    Whether low calorie beer'),
                         h5('regular_beer    =    Whether regular beer'),
                         h5('ad_spend    =    ad spend in $000'),
                         h5('price_per_oz    =    Price in $ per oz'),
                         h5('beer_distbn    =    Weighted distribution'),
                         h5('promo    =    Weighted Promotional activities'),
                         h5('beer_sales_vol    =    volume sold in that week'),
                         h5('month    =    month number 1 to 12'),
                         
                         br(),
                         p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                         img(src = "example1.png") #, height = 280, width = 400
                ),
                
                tabPanel("Summary Stats", verbatimTextOutput("summary")),
                tabPanel("Correlation", verbatimTextOutput("correlation"),plotOutput("heatmap")),
                tabPanel("Summary OLS", h4("Summary OLS Model"),verbatimTextOutput("olssummary"),
                         h4("Residual Summary: "),
                         verbatimTextOutput("residual"),
                         h4("Coefficients: "),
                         dataTableOutput("coeff"),
                         h4("Model Fit"),
                         verbatimTextOutput("text"),
                         
                         
                         h3("Summary OLS standardized model"),
                         verbatimTextOutput("olssummarystd"),
                
                         h4("Residual Summary: "),
                         verbatimTextOutput("residual1"),
                         h4("Coefficients: "),
                         dataTableOutput("coeff1"),
                         h4("Model Fit"),
                         verbatimTextOutput("text1")
                        ),
                tabPanel("Residuals Plot",h4("Fitted Values vs Residuals"),
                         plotOutput("resplot2"),h4("Fitted Values vs Y"),
                         plotOutput("resplot3"),h4("Residuals plot"),
                         plotOutput("resplot1")),
                tabPanel("Data with predicted Y",tableOutput("datatable")),
                tabPanel("Prediction",br(),
                         h4("First 10 rows of predicted data"),
                         p('"Yhat" column is the predicted value.'),
                         verbatimTextOutput('prediction'),
                         h4("Download Predicted data"),
                         downloadButton('downloadData1', 'Download Predicted data (Works only in browser)')
                ),
                tabPanel("Sample Summary Tab",h4("Summary OLS Model"),verbatimTextOutput("sampleols"))

                )
      ) 
    ) 
  )
