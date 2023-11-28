rm(list = ls())

# Libraries galore...
library(shinydashboard);library(shiny);library(DT)
library(tidyverse); library(kableExtra); library(reshape2); 
library(janitor); library(zoo); library(rlang); library(srvyr)
library(glue); library(stringr); library(zoo); library(plotly)
library(fresh); library(sass); library(sf); library(shinyWidgets); library(purrr)

my_theme = create_theme(
  adminlte_color(light_blue = "#006278"),
  adminlte_global(content_bg = "#FCCEBA"),
  adminlte_sidebar(dark_bg = "#006278", dark_color = "#FCCEBA")
)

# read in objects
MART.Dash <- readRDS("tom_finished.rds")
shape_data <- readRDS("shape_data.rds")
# shapes_la <- readRDS("shapes_la.rds")
# shapes_bmra <- readRDS("Shape.BMRA.rds")
# shapes_icb <- readRDS("shapes_icb.rds")

# Create a copy of the original dataset
MART.Dash_Original <- MART.Dash

# Ok, let's begin....
shinyApp(ui = shinyUI(
  dashboardPage(
    #skin= "#004b88",
    dashboardHeader(title = "NRI V1.1"),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Front Page", tabName = "frontPage", icon = icon("home"),badgeLabel = "NEW", badgeColor = "maroon"), 
      menuItem("Frequencies", tabName = "Frequencies"),
      menuItem("Proportions: 1 Group",tabName = "Proportions"),
      menuItem("Proportions: 2 Groups",tabName = "Proportions2"),
      menuItem("Averages: 1 Group",tabName = "Averages"),
      menuItem("Averages: 2 Groups",tabName ="Averages2" ),
      menuItem("Social Tariff Machine",tabName = "Modeling"),
      menuItem("Modeling: lower 2 expenditures",tabName = "Modeling2"),
      menuItem("Modeling: increase 1 income",tabName = "Modeling3"),
      menuItem("Line: 1 group",tabName = "Lines"),
      menuItem("Line: 2 groups",tabName ="Lines2"),
      menuItem("Line: Negbud",tabName ="Lines3",badgeLabel = "NEW", badgeColor = "maroon"),
      menuItem("Bar: Simple",tabName = "Bars"),
      menuItem("Bar: Stacked",tabName = "Bars2"),
      menuItem("Bar: Grouped",tabName = "Bars3" ),
      menuItem("Area: Stacked",tabName ="Areas" ),
      menuItem("Area: %",tabName = "Areas2"),
      menuItem(text = "Maps", tabName = "Maps", badgeLabel = "NEW", badgeColor = "maroon"),
      menuItem(text = "Density", tabName = "Density", badgeLabel = "NEW", badgeColor = "maroon")
    
    )),
    dashboardBody(
      use_theme(my_theme),
      fluidRow(
        column(
          width = 12,
          p("'Information is the oil of the 21st Century' - Peter Sondergaard", style = "color: #004b88; font-weight: bold;")
        )
      ),
      tabItems(
        #Front Page
        tabItem(tabName = "frontPage",
                h2("Welcome!"),
                fluidRow(
                  column(6,
                         uiOutput("variablePicker"), # Categorical variable selection
                         uiOutput("levelPicker") # Level selection for chosen variable
                  ),
                  column(6,
                         uiOutput("numericVariablePicker"), # Numeric variable selection
                         radioButtons('numeric_filter_type', 'Filter type:', choices = c("No filter needed", "Greater than", "Less than", "Equal to", "Between")),
                         uiOutput("numericValuePicker") # Numeric value or range selection
                  )
                ),
                actionButton('confirm', 'Confirm Filter'),
                actionButton('reset', 'Reset Filter'),
                verbatimTextOutput("filterStatus")
        )
        ,
        # Frequencies page
        tabItem(tabName = "Frequencies", 
                h3("Frequencies"),
                fluidRow(column(selectInput(inputId = "Variable1",
                                            label = "Choose a time variable:",
                                            choices = c("Financial.Year")), width = 6),
                         column(selectInput(inputId = "Variable2",
                                            label = "Choose a grouping variable:",
                                            choices = c("Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                                        "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                                        "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                                        "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                                        "Demographic.Fishy", "Demographic.FishyV2", "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                                        "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                                        "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based", "Demographic.Means.Tested","Demographic.Negbud",
                                                        "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", 
                                                        "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                                        "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                                        "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", "Demographic.ethnicity.HRP.Partner", 
                                                        "Demographic.Household.Type.LCFS", "LoE_Equivalised_V1",
                                                        "LoE_Equivalised_V2")), width = 6)),
                fluidRow(box(DT::dataTableOutput("table1"), width = 12)),
                downloadButton("download", "Download Data")
        )
        ,
        
      
      # Proportions page
      tabItem(tabName = "Proportions",
              h3("Proportions"),
              fluidRow(
                column(width = 6, 
                       selectInput(inputId = "Tab2Variable1",
                                   label = "Choose a time variable:",
                                   choices = c("Financial.Year"))
                ),
                column(width = 6, 
                       selectInput(inputId = "Tab2Variable2",
                                   label = "Choose a grouping variable:",
                                   choices = c("Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                               "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                               "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                               "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                                "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                               "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                               "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based", "Demographic.Means.Tested","Demographic.Negbud", "Demographic.Negbud.LCFS",
                                               "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", 
                                               "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                               "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                               "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", "Demographic.ethnicity.HRP.Partner", 
                                               "Demographic.Household.Type.LCFS", "LoE_Equivalised_V1",
                                               "LoE_Equivalised_V2"))
                )
              ),
              fluidRow(
                box(DT::dataTableOutput("table2"), width = 12)
              ),
              downloadButton("download2", "Download Data")
      )
      ,
      
      # Proportions 2
      tabItem(tabName = "Proportions2",
              h3("Proportions"),
              fluidRow(
                column(width = 4, 
                       selectInput(inputId = "Tab3Variable1",
                                   label = "Choose a time variable:",
                                   choices = c("Financial.Year"))
                ),
                column(width = 4, 
                       selectInput(inputId = "Tab3Variable2",
                                   label = "Choose a grouping variable:",
                                   choices = c("Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                               "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                               "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                               "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                               "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                               "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                               "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based", "Demographic.Means.Tested","Demographic.Negbud", "Demographic.Negbud.LCFS",
                                               
                                               "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", 
                                               "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                               "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                               "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", "Demographic.ethnicity.HRP.Partner", 
                                               "Demographic.Household.Type.LCFS", "Demographic.FishyV2", "LoE_Equivalised_V1",
                                               "LoE_Equivalised_V2"))
                ),
                column(width = 4, 
                       selectInput(inputId = "Tab3Variable3",
                                   label = "Choose a grouping variable:",
                                   choices = c("Demographic.Negbud","Demographic.Negbud.LCFS", "Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                               "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                               "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                               "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                               "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                               "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                               "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based", "Demographic.Means.Tested",
                                               
                                               "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", 
                                               "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                               "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                               "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", "Demographic.ethnicity.HRP.Partner", 
                                               "Demographic.Household.Type.LCFS", "LoE_Equivalised_V1",
                                               "LoE_Equivalised_V2"))
                )
              ),
              fluidRow(
                box(DT::dataTableOutput("table3"), width = 12)
              ),
              downloadButton("download3", "Download Data") 
      )
      ,
      
      # Summary stats 1
      tabItem(tabName = "Averages",
              h3("Averages"),
              fluidRow(
                column(width = 6,
                       selectInput(inputId = "Tab4Variable1",
                                   label = "Choose a time variable:",
                                   choices = c("Financial.Year"))
                ),
                column(width = 6,
                       selectInput(inputId = "Tab4Variable2",
                                   label = "Choose a numeric variable:",
                                   choices = c("Surplus", "Surplus.LCFS" ,"Expenditure.Comms.Mobile.Phone", 
                                               "Expenditure.Council.Tax", "Expenditure.Council.Tax.Support", "Expenditure.Decile.Numeric", 
                                               "Expenditure.Flexible.Total.LCFS", "Expenditure.Flexible.Total.LoE", "Expenditure.Fixed.Total", 
                                               "Expenditure.Food.House.Keeping", "Expenditure.Food.House.Keeping.MART", "Expenditure.Health", "Expenditure.Housing", 
                                               "Expenditure.Housing.LCFS", "Expenditure.Leisure.Cost", "Expenditure.Leisure.MART", "Expenditure.Mortgage", "Expenditure.Others", 
                                               "Expenditure.Personal.Cost", "Expenditure.Personal.MART", "Expenditure.Rent", "Expenditure.Rent.Net", 
                                               "Expenditure.Total","Expenditure.Total.LCFS" , "Expenditure.Transport", 
                                               "Expenditure.Utilities", "Expenditure.Utilities.MART", "Expenditure.Vehicle.Insurance", "Expenditure.Water", 
                                               "Income.Carer.Allowance", "Income.Child.Benefit", "Income.Child.Tax.Credit", "Income.Decile.Numeric", 
                                               "Income.ESA", "Income.ExtPaym.Housing.Benefit", "Income.Gross.Wages.Partner", "Income.Gross.Wages.Respondent", 
                                               "Income.Housing.Benefit", "Income.Income.Support", "Income.JSA.Combined", "Income.JSA.Contribution.Based", 
                                               "Income.JSA.Income.Based", "Income.Pension.Credit", "Income.PIP.Car", "Income.PIP.Mob", 
                                               "Income.Takehome.Wages.Partner", "Income.Takehome.Wages.Respondent", "Income.Total", 
                                               "Income.Total.Benefits", "Income.UC", "Income.WTC","Income.Equivalised",
                                               "Expenditure.Equivalised",
                                               "Surplus.Equivalised"))
                )
              ),
              fluidRow(box(DT::dataTableOutput("table4"), width = 12)),
              downloadButton("download4", "Download Data") 
      )
      ,
      
      # Summary stats 2
      tabItem(tabName = "Averages2",
              h3("Averages"),
              fluidRow(
                column(width = 4,
                       selectInput(inputId = "Tab5Variable1",
                                   label = "Choose a time variable:",
                                   choices = c("Financial.Year"))),
                column(width = 4,
                       selectInput(inputId = "Tab5Variable2",
                                   label = "Choose a grouping variable:",
                                   choices = c("Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                               "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                               "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                               "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                               "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                               "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                               "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based", "Demographic.Means.Tested","Demographic.Negbud", "Demographic.Negbud.LCFS",
                                               
                                               "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", 
                                               "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                               "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                               "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", "Demographic.ethnicity.HRP.Partner", 
                                               "Demographic.Household.Type.LCFS", "LoE_Equivalised_V1",
                                               "LoE_Equivalised_V2"))),
                column(width = 4,
                       selectInput(inputId = "Tab5Variable3",
                                   label = "Choose a numeric variable:",
                                   choices = c("Surplus", "Surplus.LCFS" ,"Expenditure.Comms.Mobile.Phone", 
                                               "Expenditure.Council.Tax", "Expenditure.Council.Tax.Support", "Expenditure.Decile.Numeric", 
                                               "Expenditure.Flexible.Total.LCFS", "Expenditure.Flexible.Total.LoE", "Expenditure.Fixed.Total", 
                                               "Expenditure.Food.House.Keeping", "Expenditure.Food.House.Keeping.MART", "Expenditure.Health", "Expenditure.Housing", 
                                               "Expenditure.Housing.LCFS", "Expenditure.Leisure.Cost", "Expenditure.Leisure.MART", "Expenditure.Mortgage", "Expenditure.Others", 
                                               "Expenditure.Personal.Cost", "Expenditure.Personal.MART", "Expenditure.Rent", "Expenditure.Rent.Net", 
                                               "Expenditure.Total","Expenditure.Total.LCFS" , "Expenditure.Transport", 
                                               "Expenditure.Utilities", "Expenditure.Utilities.MART", "Expenditure.Vehicle.Insurance", "Expenditure.Water", 
                                               "Income.Carer.Allowance", "Income.Child.Benefit", "Income.Child.Tax.Credit", "Income.Decile.Numeric", 
                                               "Income.ESA", "Income.ExtPaym.Housing.Benefit", "Income.Gross.Wages.Partner", "Income.Gross.Wages.Respondent", 
                                               "Income.Housing.Benefit", "Income.Income.Support", "Income.JSA.Combined", "Income.JSA.Contribution.Based", 
                                               "Income.JSA.Income.Based", "Income.Pension.Credit", "Income.PIP.Car", "Income.PIP.Mob", 
                                               "Income.Takehome.Wages.Partner", "Income.Takehome.Wages.Respondent", "Income.Total", 
                                               "Income.Total.Benefits", "Income.UC", "Income.WTC","Income.Equivalised",
                                               "Expenditure.Equivalised",
                                               "Surplus.Equivalised")))
              ),
              fluidRow(
                column(width = 12,
                       box(DT::dataTableOutput("table5"), width = 12))
              ),
              fluidRow(
                column(width = 12,
                       downloadButton("download5", "Download Data"))
              )
      )
      ,
      
      # Modeling 1
      tabItem(tabName = "Modeling",
              h3("Modeling"),
              fluidRow(
                column(width = 6, sliderInput("number7a", "Water % Reduction:", min = 0, max = 100, value = 10)),
                column(width = 6, sliderInput("number7b", "Communications % Reduction:", min = 0, max = 100, value = 10))
              ),
              fluidRow(
                column(width = 6, sliderInput("number7c", "Car Insurance % Reduction:", min = 0, max = 100, value = 10)),
                column(width = 6, sliderInput("number7d", "Mortgage % Reduction:", min = 0, max = 100, value = 10))
              ),
              fluidRow(
                column(width = 6, sliderInput("number7e", "Transport % Reduction:", min = 0, max = 100, value = 10)),
                column(width = 6, sliderInput("number7f", "Food % Reduction:", min = 0, max = 100, value = 10))
              ),
              fluidRow(
                column(width = 6, sliderInput("number7g", "Energy % Reduction:", min = 0, max = 100, value = 10)),
                column(width = 6, sliderInput("number7h", "Leisure % Reduction:", min = 0, max = 100, value = 10))
              ),
              fluidRow(
                box(DT::dataTableOutput("table7"), width = 12)
              ),
              downloadButton("download7", "Download Data")
      )
      
      ,
      
      # Modeling 2
      tabItem(tabName = "Modeling2",
              h3("Modeling"),
              fluidRow(
                column(width = 6, sliderInput("number8a", "Percentage Reduction:",
                                              min = 0, max = 100, value = 10)),
                column(width = 6, sliderInput("number8b", "Percentage Reduction:",
                                              min = 0, max = 100, value = 10))
              ),
              fluidRow(
                column(width = 6, selectInput(inputId = "Tab8Variable1",
                                              label = "Choose an expenditure for the slider above:",
                                              choices = c("Expenditure.Comms.Mobile.Phone", "Expenditure.Council.Tax", "Expenditure.Council.Tax.Support", 
                                                          "Expenditure.Decile.Numeric", "Expenditure.Flexible.Total.LCFS", "Expenditure.Flexible.Total.LoE", 
                                                          "Expenditure.Fixed.Total", "Expenditure.Food.House.Keeping", "Expenditure.Food.House.Keeping.MART", 
                                                          "Expenditure.Health", "Expenditure.Housing", "Expenditure.Housing.LCFS", "Expenditure.Leisure.Cost", 
                                                          "Expenditure.Leisure.MART", "Expenditure.Mortgage", "Expenditure.Others", "Expenditure.Personal.Cost", 
                                                          "Expenditure.Personal.MART", "Expenditure.Rent", "Expenditure.Rent.Net", "Expenditure.Total", "Expenditure.Total.LCFS",
                                                          "Expenditure.Transport", "Expenditure.Utilities", "Expenditure.Utilities.MART", 
                                                          "Expenditure.Vehicle.Insurance", "Expenditure.Water","Expenditure.Equivalised"))),
                column(width = 6, selectInput(inputId = "Tab8Variable2",
                                              label = "Choose an expenditure for the slider above:",
                                              choices = c("Expenditure.Water","Expenditure.Comms.Mobile.Phone", "Expenditure.Council.Tax", "Expenditure.Council.Tax.Support", 
                                                          "Expenditure.Decile.Numeric", "Expenditure.Flexible.Total.LCFS", "Expenditure.Flexible.Total.LoE", 
                                                          "Expenditure.Fixed.Total", "Expenditure.Food.House.Keeping", "Expenditure.Food.House.Keeping.MART", 
                                                          "Expenditure.Health", "Expenditure.Housing", "Expenditure.Housing.LCFS", "Expenditure.Leisure.Cost", 
                                                          "Expenditure.Leisure.MART", "Expenditure.Mortgage", "Expenditure.Others", "Expenditure.Personal.Cost", 
                                                          "Expenditure.Personal.MART", "Expenditure.Rent", "Expenditure.Rent.Net", "Expenditure.Total", "Expenditure.Total.LCFS",
                                                          "Expenditure.Transport", "Expenditure.Utilities", "Expenditure.Utilities.MART", 
                                                          "Expenditure.Vehicle.Insurance","Expenditure.Equivalised")))
              ),
              fluidRow(box(DT::dataTableOutput("table8"), width = 12)),
              downloadButton("download8", "Download Data")   
      ) 
      ,
      
      # Modeling 3
      tabItem(tabName = "Modeling3",
              h3("Modeling"),
              fluidRow(
                column(width = 6,
                       sliderInput("number9", "Percentage Increase:",
                                   min = 0, max = 100, value = 10)
                ),
                column(width = 6,
                       selectInput(inputId = "Tab9Variable1",
                                   label = "Choose an income to increase:",
                                   choices = c("Income.Carer.Allowance", "Income.Child.Benefit", "Income.Child.Tax.Credit", 
                                               "Income.Decile.Numeric", "Income.ESA", "Income.ExtPaym.Housing.Benefit", 
                                               "Income.Gross.Wages.Partner", "Income.Gross.Wages.Respondent", "Income.Housing.Benefit", 
                                               "Income.Income.Support", "Income.JSA.Combined", "Income.JSA.Contribution.Based", 
                                               "Income.JSA.Income.Based", "Income.Pension.Credit", "Income.PIP.Car", "Income.PIP.Mob", 
                                               "Income.Takehome.Wages.Partner", "Income.Takehome.Wages.Respondent", "Income.Total", 
                                               "Income.Total.Benefits", "Income.UC", "Income.WTC","Income.Equivalised"))
                )
              ),
              fluidRow(box(DT::dataTableOutput("table9"), width = 12)),
              downloadButton("download9", "Download Data")       
      )
      
      ,
      

      tabItem(tabName = "Lines",
              h3("Lines"),
              fluidRow(
                column(width = 6,
                       selectInput(inputId = "Tab10Variable1",
                                   label = "Choose a numeric variable:",
                                   choices = c("Surplus", "Surplus.LCFS" ,"Expenditure.Comms.Mobile.Phone", 
                                               "Expenditure.Council.Tax", "Expenditure.Council.Tax.Support", "Expenditure.Decile.Numeric", 
                                               "Expenditure.Flexible.Total.LCFS", "Expenditure.Flexible.Total.LoE", "Expenditure.Fixed.Total", 
                                               "Expenditure.Food.House.Keeping", "Expenditure.Food.House.Keeping.MART", "Expenditure.Health", "Expenditure.Housing", 
                                               "Expenditure.Housing.LCFS", "Expenditure.Leisure.Cost", "Expenditure.Leisure.MART", "Expenditure.Mortgage", "Expenditure.Others", 
                                               "Expenditure.Personal.Cost", "Expenditure.Personal.MART", "Expenditure.Rent", "Expenditure.Rent.Net", 
                                               "Expenditure.Total","Expenditure.Total.LCFS" , "Expenditure.Transport", 
                                               "Expenditure.Utilities", "Expenditure.Utilities.MART", "Expenditure.Vehicle.Insurance", "Expenditure.Water", 
                                               "Income.Carer.Allowance", "Income.Child.Benefit", "Income.Child.Tax.Credit", "Income.Decile.Numeric", 
                                               "Income.ESA", "Income.ExtPaym.Housing.Benefit", "Income.Gross.Wages.Partner", "Income.Gross.Wages.Respondent", 
                                               "Income.Housing.Benefit", "Income.Income.Support", "Income.JSA.Combined", "Income.JSA.Contribution.Based", 
                                               "Income.JSA.Income.Based", "Income.Pension.Credit", "Income.PIP.Car", "Income.PIP.Mob", 
                                               "Income.Takehome.Wages.Partner", "Income.Takehome.Wages.Respondent", "Income.Total", 
                                               "Income.Total.Benefits", "Income.UC", "Income.WTC","Income.Equivalised",
                                               "Expenditure.Equivalised",
                                               "Surplus.Equivalised"))
                ),
                column(width = 6,
                       selectInput(inputId = "TimeVariable",
                                   label = "Choose a time variable:",
                                   choices = c("Financial.Year"))
                )
              ),
              tabsetPanel(
                tabPanel("Plot",
                         fluidRow(
                           column(width = 12, plotlyOutput("plot10"))
                         )
                ),
                tabPanel("Table",
                         fluidRow(
                           column(width = 12, DT::dataTableOutput("table10"))
                         )
                )
              ),
              downloadButton("download10", "Download Data"),
              downloadButton("download10b", "Download Graph")
      )
      ,
      
      tabItem(tabName = "Lines2",
              h3("Lines"),
              fluidRow(
                column(width = 4,
                       selectInput(inputId = "Tab11Variable1",
                                   label = "Choose a grouping variable:",
                                   choices = c("Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                               "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                               "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                               "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                               "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                               "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                               "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based", "Demographic.Means.Tested","Demographic.Negbud", "Demographic.Negbud.LCFS",
                                               
                                               "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", 
                                               "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                               "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                               "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", "Demographic.ethnicity.HRP.Partner", 
                                               "Demographic.Household.Type.LCFS", "LoE_Equivalised_V1",
                                               "LoE_Equivalised_V2"))
                ),
                column(width = 4,
                       selectInput(inputId = "Tab11Variable2",
                                   label = "Choose a numeric variable:",
                                   choices = c("Surplus", "Surplus.LCFS" ,"Expenditure.Comms.Mobile.Phone", 
                                               "Expenditure.Council.Tax", "Expenditure.Council.Tax.Support", "Expenditure.Decile.Numeric", 
                                               "Expenditure.Flexible.Total.LCFS", "Expenditure.Flexible.Total.LoE", "Expenditure.Fixed.Total", 
                                               "Expenditure.Food.House.Keeping", "Expenditure.Food.House.Keeping.MART", "Expenditure.Health", "Expenditure.Housing", 
                                               "Expenditure.Housing.LCFS", "Expenditure.Leisure.Cost", "Expenditure.Leisure.MART", "Expenditure.Mortgage", "Expenditure.Others", 
                                               "Expenditure.Personal.Cost", "Expenditure.Personal.MART", "Expenditure.Rent", "Expenditure.Rent.Net", 
                                               "Expenditure.Total","Expenditure.Total.LCFS" , "Expenditure.Transport", 
                                               "Expenditure.Utilities", "Expenditure.Utilities.MART", "Expenditure.Vehicle.Insurance", "Expenditure.Water", 
                                               "Income.Carer.Allowance", "Income.Child.Benefit", "Income.Child.Tax.Credit", "Income.Decile.Numeric", 
                                               "Income.ESA", "Income.ExtPaym.Housing.Benefit", "Income.Gross.Wages.Partner", "Income.Gross.Wages.Respondent", 
                                               "Income.Housing.Benefit", "Income.Income.Support", "Income.JSA.Combined", "Income.JSA.Contribution.Based", 
                                               "Income.JSA.Income.Based", "Income.Pension.Credit", "Income.PIP.Car", "Income.PIP.Mob", 
                                               "Income.Takehome.Wages.Partner", "Income.Takehome.Wages.Respondent", "Income.Total", 
                                               "Income.Total.Benefits", "Income.UC", "Income.WTC","Income.Equivalised",
                                               "Expenditure.Equivalised",
                                               "Surplus.Equivalised"))
                ),
                column(width = 4,
                       selectInput(inputId = "TimeVariable2",
                                   label = "Choose a time variable:",
                                   choices = c("Financial.Year"))
                )
              ),
              tabsetPanel(
                tabPanel("Plot",
                         fluidRow(
                           column(width = 12, plotlyOutput("plot11"))
                         )
                ),
                tabPanel("Table",
                         fluidRow(
                           column(width = 12, DT::dataTableOutput("table11"))
                         )
                )
              ),
              downloadButton("download11", "Download Data"),
              downloadButton("download11b", "Download Graph")
      )
      ,
      tabItem(tabName = "Lines3",
              h3("Lines"),
              fluidRow(
                column(width = 4,
                       selectInput(inputId = "Tab20Variable1",
                                   label = "Choose a grouping variable:",
                                   choices = c("Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                               "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                               "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                               "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                               "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                               "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                               "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based","Demographic.Means.Tested", "Demographic.Negbud", "Demographic.Negbud.LCFS",
                                              
                                               "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", 
                                               "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                               "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                               "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", "Demographic.ethnicity.HRP.Partner", 
                                               "Demographic.Household.Type.LCFS", "LoE_Equivalised_V1",
                                               "LoE_Equivalised_V2"))
                ),
                column(width = 4,
                       selectInput(inputId = "TimeVariableNeg",
                                   label = "Choose a time variable:",
                                   choices = c("Financial.Year"))
                )
              ),
              tabsetPanel(
                tabPanel("Plot",
                         fluidRow(
                           column(width = 12, plotlyOutput("plot20"))
                         )
                ),
                tabPanel("Table",
                         fluidRow(
                           column(width = 12, DT::dataTableOutput("table20"))
                         )
                )
              ),
              downloadButton("download20", "Download Data"),
              downloadButton("download20b", "Download Graph")
      )
      
      ,
      
    
      
      tabItem(tabName = "Bars",
              h3("Bars"),
              fluidRow(
                column(width = 4,
                       selectInput(inputId = "Tab12Variable1",
                                   label = "Choose a grouping variable:",
                                   choices = c("Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                               "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                               "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                               "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                               "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                               "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                               "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based", "Demographic.Means.Tested","Demographic.Negbud", "Demographic.Negbud.LCFS",
                                               
                                               "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", 
                                               "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                               "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                               "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", "Demographic.ethnicity.HRP.Partner", 
                                               "Demographic.Household.Type.LCFS", "LoE_Equivalised_V1",
                                               "LoE_Equivalised_V2"))
                ),
                column(width = 4,
                       selectInput(inputId = "Tab12Variable2",
                                   label = "Choose a numeric variable:",
                                   choices = c("Surplus", "Surplus.LCFS" ,"Expenditure.Comms.Mobile.Phone", 
                                               "Expenditure.Council.Tax", "Expenditure.Council.Tax.Support", "Expenditure.Decile.Numeric", 
                                               "Expenditure.Flexible.Total.LCFS", "Expenditure.Flexible.Total.LoE", "Expenditure.Fixed.Total", 
                                               "Expenditure.Food.House.Keeping", "Expenditure.Food.House.Keeping.MART", "Expenditure.Health", "Expenditure.Housing", 
                                               "Expenditure.Housing.LCFS", "Expenditure.Leisure.Cost", "Expenditure.Leisure.MART", "Expenditure.Mortgage", "Expenditure.Others", 
                                               "Expenditure.Personal.Cost", "Expenditure.Personal.MART", "Expenditure.Rent", "Expenditure.Rent.Net", 
                                               "Expenditure.Total","Expenditure.Total.LCFS" , "Expenditure.Transport", 
                                               "Expenditure.Utilities", "Expenditure.Utilities.MART", "Expenditure.Vehicle.Insurance", "Expenditure.Water", 
                                               "Income.Carer.Allowance", "Income.Child.Benefit", "Income.Child.Tax.Credit", "Income.Decile.Numeric", 
                                               "Income.ESA", "Income.ExtPaym.Housing.Benefit", "Income.Gross.Wages.Partner", "Income.Gross.Wages.Respondent", 
                                               "Income.Housing.Benefit", "Income.Income.Support", "Income.JSA.Combined", "Income.JSA.Contribution.Based", 
                                               "Income.JSA.Income.Based", "Income.Pension.Credit", "Income.PIP.Car", "Income.PIP.Mob", 
                                               "Income.Takehome.Wages.Partner", "Income.Takehome.Wages.Respondent", "Income.Total", 
                                               "Income.Total.Benefits", "Income.UC", "Income.WTC","Income.Equivalised",
                                               "Expenditure.Equivalised",
                                               "Surplus.Equivalised"))
                ),
                column(width = 4,
                       selectInput(inputId = "TimeVariable3",
                                   label = "Choose a time variable:",
                                   choices = c("Financial.Year"))
                )
              ),
              tabsetPanel(
                tabPanel("Plot",
                         fluidRow(
                           column(width = 12, plotlyOutput("plot12"))
                         )
                ),
                tabPanel("Table",
                         fluidRow(
                           column(width = 12, DT::dataTableOutput("table12"))
                         )
                )
              ),
              downloadButton("download12", "Download Data"),
              downloadButton("download12b", "Download Graph")
      )
      ,
      
      
      tabItem(tabName = "Bars2",
              h3("Bars"),
              fluidRow(
                column(width = 3,
                       selectInput(inputId = "Tab13Variable1",
                                   label = "Grouping variable 1:",
                                   choices = c("Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                               "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                               "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                               "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                              "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                               "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                               "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based", "Demographic.Means.Tested","Demographic.Negbud", "Demographic.Negbud.LCFS",
                                              
                                               "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", 
                                               "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                               "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                               "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", "Demographic.ethnicity.HRP.Partner", 
                                               "Demographic.Household.Type.LCFS", "LoE_Equivalised_V1",
                                               "LoE_Equivalised_V2"))),
                column(width = 3,
                       selectInput(inputId = "Tab13Variable2",
                                   label = "Grouping variable 2:",
                                   choices = c("Demographic.Negbud","Demographic.Negbud.LCFS","Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                               "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                               "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                               "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                               "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                               "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                               "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based", "Demographic.Means.Tested",
                                              
                                               "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", 
                                               "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                               "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                               "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", "Demographic.ethnicity.HRP.Partner", 
                                               "Demographic.Household.Type.LCFS", "LoE_Equivalised_V1",
                                               "LoE_Equivalised_V2"))),
                column(width = 3,
                       selectInput(inputId = "Tab13Variable3",
                                   label = "Numeric variable:",
                                   choices = c("Expenditure.Comms.Mobile.Phone", "Expenditure.Council.Tax", "Expenditure.Council.Tax.Support", 
                                               "Expenditure.Decile.Numeric", "Expenditure.Flexible.Total.LCFS", "Expenditure.Flexible.Total.LoE", 
                                               "Expenditure.Fixed.Total", "Expenditure.Food.House.Keeping", "Expenditure.Food.House.Keeping.MART", 
                                               "Expenditure.Health", "Expenditure.Housing", "Expenditure.Housing.LCFS", "Expenditure.Leisure.Cost", 
                                               "Expenditure.Leisure.MART", "Expenditure.Mortgage", "Expenditure.Others", "Expenditure.Personal.Cost", 
                                               "Expenditure.Personal.MART", "Expenditure.Rent", "Expenditure.Rent.Net", "Expenditure.Total", 
                                               "Expenditure.Transport", "Expenditure.Utilities", "Expenditure.Utilities.MART", "Expenditure.Vehicle.Insurance", 
                                               "Expenditure.Water", "Income.Carer.Allowance", "Income.Child.Benefit", "Income.Child.Tax.Credit", 
                                               "Income.Decile.Numeric", "Income.ESA", "Income.ExtPaym.Housing.Benefit", "Income.Gross.Wages.Partner", 
                                               "Income.Gross.Wages.Respondent", "Income.Housing.Benefit", "Income.Income.Support", 
                                               "Income.JSA.Combined", "Income.JSA.Contribution.Based", "Income.JSA.Income.Based", 
                                               "Income.Pension.Credit", "Income.PIP.Car", "Income.PIP.Mob", "Income.Takehome.Wages.Partner", 
                                               "Income.Takehome.Wages.Respondent", "Income.Total", "Income.Total.Benefits", "Income.UC", "Income.WTC", "Surplus",
                                               "Income.Equivalised",
                                               "Expenditure.Equivalised",
                                               "Surplus.Equivalised"))),
                column(width = 3,
                       selectInput(inputId = "TimeVariable4",
                                   label = "Time variable:",
                                   choices = c("Financial.Year"))
                )
              ),
              tabsetPanel(
                tabPanel("Plot",
                         fluidRow(
                           column(width = 12, plotlyOutput("plot13"))
                         )
                ),
                tabPanel("Table",
                         fluidRow(
                           column(width = 12, DT::dataTableOutput("table13"))
                         )
                )
              ),
              downloadButton("download13", "Download Data"),
              downloadButton("download13b", "Download Graph")
      )
      ,
      

      tabItem(tabName = "Bars3",
              h3("Bars"),
              fluidRow(
                column(width = 3,
                       selectInput(inputId = "Tab14Variable1",
                                   label = "Grouping variable 1:",
                                   choices = c("Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                               "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                               "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                               "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                               "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                               "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                               "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based", "Demographic.Means.Tested","Demographic.Negbud", "Demographic.Negbud.LCFS",
                                              
                                               "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", 
                                               "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                               "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                               "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", "Demographic.ethnicity.HRP.Partner", 
                                               "Demographic.Household.Type.LCFS","Demographic.FishyV2", "Demographic.FishyV3", "LoE_Equivalised_V1",
                                               "LoE_Equivalised_V2"))),
                column(width = 3,
                       selectInput(inputId = "Tab14Variable2",
                                   label = "Grouping variable 2:",
                                   choices = c("Demographic.Negbud","Demographic.Negbud.LCFS","Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                               "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                               "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                               "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                               "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                               "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                               "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based", 
                                               
                                               "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", "Demographic.Means.Tested",
                                               "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                               "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                               "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", 
                                               "Demographic.ethnicity.HRP.Partner", "Demographic.Household.Type.LCFS","Demographic.FishyV2","Demographic.FishyV3", "LoE_Equivalised_V1",
                                               "LoE_Equivalised_V2"))),
                column(width = 3,
                       selectInput(inputId = "Tab14Variable3",
                                   label = "Numeric variable:",
                                   choices = c("Surplus", "Surplus.LCFS" ,"Expenditure.Comms.Mobile.Phone", 
                                               "Expenditure.Council.Tax", "Expenditure.Council.Tax.Support", "Expenditure.Decile.Numeric", 
                                               "Expenditure.Flexible.Total.LCFS", "Expenditure.Flexible.Total.LoE", "Expenditure.Fixed.Total", 
                                               "Expenditure.Food.House.Keeping", "Expenditure.Food.House.Keeping.MART", "Expenditure.Health", "Expenditure.Housing", 
                                               "Expenditure.Housing.LCFS", "Expenditure.Leisure.Cost", "Expenditure.Leisure.MART", "Expenditure.Mortgage", "Expenditure.Others", 
                                               "Expenditure.Personal.Cost", "Expenditure.Personal.MART", "Expenditure.Rent", "Expenditure.Rent.Net", 
                                               "Expenditure.Total","Expenditure.Total.LCFS" , "Expenditure.Transport", 
                                               "Expenditure.Utilities", "Expenditure.Utilities.MART", "Expenditure.Vehicle.Insurance", "Expenditure.Water", 
                                               "Income.Carer.Allowance", "Income.Child.Benefit", "Income.Child.Tax.Credit", "Income.Decile.Numeric", 
                                               "Income.ESA", "Income.ExtPaym.Housing.Benefit", "Income.Gross.Wages.Partner", "Income.Gross.Wages.Respondent", 
                                               "Income.Housing.Benefit", "Income.Income.Support", "Income.JSA.Combined", "Income.JSA.Contribution.Based", 
                                               "Income.JSA.Income.Based", "Income.Pension.Credit", "Income.PIP.Car", "Income.PIP.Mob", 
                                               "Income.Takehome.Wages.Partner", "Income.Takehome.Wages.Respondent", "Income.Total", 
                                               "Income.Total.Benefits", "Income.UC", "Income.WTC","Income.Equivalised",
                                               "Expenditure.Equivalised",
                                               "Surplus.Equivalised"))),
                column(width = 3,
                       selectInput(inputId = "TimeVariable5",
                                   label = "Time variable:",
                                   choices = c("Financial.Year"))
                )
              ),
              tabsetPanel(
                tabPanel("Plot",
                         fluidRow(
                           column(width = 12, plotlyOutput("plot14"))
                         )
                ),
                tabPanel("Table",
                         fluidRow(
                           column(width = 12, DT::dataTableOutput("table14"))
                         )
                )
              ),
              downloadButton("download14", "Download Data"),
              downloadButton("download14b", "Download Graph")
      )
      ,
      
   
      tabItem(tabName = "Areas",
              h3("Areas"),
              fluidRow(
                column(width = 4,
                       selectInput(inputId = "Tab15Variable1",
                                   label = "Choose a grouping variable:",
                                   choices = c("Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                               "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                               "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                               "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                               "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                               "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                               "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based", "Demographic.Means.Tested","Demographic.Negbud", "Demographic.Negbud.LCFS",
                                               
                                               "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", 
                                               "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                               "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                               "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", "Demographic.ethnicity.HRP.Partner", 
                                               "Demographic.Household.Type.LCFS", "LoE_Equivalised_V1",
                                               "LoE_Equivalised_V2"))),
                column(width = 4,
                       selectInput(inputId = "Tab15Variable2",
                                   label = "Choose a numeric variable:",
                                   choices = c("Surplus", "Surplus.LCFS" ,"Expenditure.Comms.Mobile.Phone", 
                                               "Expenditure.Council.Tax", "Expenditure.Council.Tax.Support", "Expenditure.Decile.Numeric", 
                                               "Expenditure.Flexible.Total.LCFS", "Expenditure.Flexible.Total.LoE", "Expenditure.Fixed.Total", 
                                               "Expenditure.Food.House.Keeping", "Expenditure.Food.House.Keeping.MART", "Expenditure.Health", "Expenditure.Housing", 
                                               "Expenditure.Housing.LCFS", "Expenditure.Leisure.Cost", "Expenditure.Leisure.MART", "Expenditure.Mortgage", "Expenditure.Others", 
                                               "Expenditure.Personal.Cost", "Expenditure.Personal.MART", "Expenditure.Rent", "Expenditure.Rent.Net", 
                                               "Expenditure.Total","Expenditure.Total.LCFS" , "Expenditure.Transport", 
                                               "Expenditure.Utilities", "Expenditure.Utilities.MART", "Expenditure.Vehicle.Insurance", "Expenditure.Water", 
                                               "Income.Carer.Allowance", "Income.Child.Benefit", "Income.Child.Tax.Credit", "Income.Decile.Numeric", 
                                               "Income.ESA", "Income.ExtPaym.Housing.Benefit", "Income.Gross.Wages.Partner", "Income.Gross.Wages.Respondent", 
                                               "Income.Housing.Benefit", "Income.Income.Support", "Income.JSA.Combined", "Income.JSA.Contribution.Based", 
                                               "Income.JSA.Income.Based", "Income.Pension.Credit", "Income.PIP.Car", "Income.PIP.Mob", 
                                               "Income.Takehome.Wages.Partner", "Income.Takehome.Wages.Respondent", "Income.Total", 
                                               "Income.Total.Benefits", "Income.UC", "Income.WTC","Income.Equivalised",
                                               "Expenditure.Equivalised",
                                               "Surplus.Equivalised"))),
                column(width = 4,
                       selectInput(inputId = "TimeVariable6",
                                   label = "Choose a time variable:",
                                   choices = c("Financial.Year"))
                )
              ),
              tabsetPanel(
                tabPanel("Plot",
                         fluidRow(
                           column(width = 12, plotlyOutput("plot15"))
                         )
                ),
                tabPanel("Table",
                         fluidRow(
                           column(width = 12, DT::dataTableOutput("table15"))
                         )
                )
              ),
              downloadButton("download15", "Download Data"),
              downloadButton("download15b", "Download Graph")
      )
      ,
      
      
      tabItem(tabName = "Areas2",
              h3("Areas"),
              fluidRow(
                column(width = 4,
                       selectInput(inputId = "Tab16Variable1",
                                   label = "Choose a grouping variable:",
                                   choices = c("Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                               "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                               "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                               "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                               "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                               "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                               "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based", "Demographic.Means.Tested","Demographic.Negbud", "Demographic.Negbud.LCFS",
                                               
                                               "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", 
                                               "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                               "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                               "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", "Demographic.ethnicity.HRP.Partner", 
                                               "Demographic.Household.Type.LCFS", "LoE_Equivalised_V1",
                                               "LoE_Equivalised_V2")))
              ),
              tabsetPanel(
                tabPanel("Plot",
                         fluidRow(
                           column(width = 12, plotlyOutput("plot16"))
                         )
                ),
                tabPanel("Table",
                         fluidRow(
                           column(width = 12, DT::dataTableOutput("table16"))
                         )
                )
              ),
              downloadButton("download16", "Download Data"),
              downloadButton("download16b", "Download Graph"))
      ,
      
      tabItem(tabName = "Maps",
              h3("Maps"),
              fluidRow(
                column(4, 
                       selectInput(inputId = "Boundary",
                                   label = "Select Boundary:",
                                   choices = c("Region", "Local Authority", "BMRA",
                                               "Integrated.Care.Board")
                       )
                ),
                column(4, 
                       selectInput(inputId = "Tab18Variable1",
                                   label = "Choose a variable:",
                                   choices = c("Surplus", "Surplus.LCFS" ,"Expenditure.Comms.Mobile.Phone", 
                                               "Expenditure.Council.Tax", "Expenditure.Council.Tax.Support", "Expenditure.Decile.Numeric", 
                                               "Expenditure.Flexible.Total.LCFS", "Expenditure.Flexible.Total.LoE", "Expenditure.Fixed.Total", 
                                               "Expenditure.Food.House.Keeping", "Expenditure.Food.House.Keeping.MART", "Expenditure.Health", "Expenditure.Housing", 
                                               "Expenditure.Housing.LCFS", "Expenditure.Leisure.Cost", "Expenditure.Leisure.MART", "Expenditure.Mortgage", "Expenditure.Others", 
                                               "Expenditure.Personal.Cost", "Expenditure.Personal.MART", "Expenditure.Rent", "Expenditure.Rent.Net", 
                                               "Expenditure.Total","Expenditure.Total.LCFS" , "Expenditure.Transport", 
                                               "Expenditure.Utilities", "Expenditure.Utilities.MART", "Expenditure.Vehicle.Insurance", "Expenditure.Water", 
                                               "Income.Carer.Allowance", "Income.Child.Benefit", "Income.Child.Tax.Credit", "Income.Decile.Numeric", 
                                               "Income.ESA", "Income.ExtPaym.Housing.Benefit", "Income.Gross.Wages.Partner", "Income.Gross.Wages.Respondent", 
                                               "Income.Housing.Benefit", "Income.Income.Support", "Income.JSA.Combined", "Income.JSA.Contribution.Based", 
                                               "Income.JSA.Income.Based", "Income.Pension.Credit", "Income.PIP.Car", "Income.PIP.Mob", 
                                               "Income.Takehome.Wages.Partner", "Income.Takehome.Wages.Respondent", "Income.Total", 
                                               "Income.Total.Benefits", "Income.UC", "Income.WTC","Income.Equivalised",
                                               "Expenditure.Equivalised",
                                               "Surplus.Equivalised")  
                       )
                ),
                column(4, 
                       selectInput(inputId = "Tab18Variable2",
                                   label = "Select a time period:",
                                   choices = c("Financial.Year")
                       )
                ),
                column(4, 
                       selectInput(inputId = "Tab18Variable3",
                                   label = "Select a value:",
                                   choices = c("2020-21", "2021-22", "2022-23", "2023-24")
                       )
                ),
                column(4, 
                       selectInput(inputId = "Tab18Variable4",
                                   label = "Select a colour:",
                                   choices = c("Red", "Green")
                       )
                )
              ),
              tabsetPanel(
                tabPanel("Plot",
                         fluidRow(
                           column(width = 12, plotlyOutput("plot18"))
                         )
                ),
                tabPanel("Table",
                         fluidRow(
                           column(width = 12, DT::dataTableOutput("table18"))
                         )
                )
              ),
              downloadButton("download18", "Download Data"),
              downloadButton("download18b", "Download Plot")
      ),
      tabItem(tabName = "Density",
              h3("Density"),
              fluidRow(
                column(12,
                       fluidRow(
                         column(4, 
                                selectInput(inputId = "Tab17Variable1",
                                            label = "Choose a grouping variable:",
                                            choices = c("Demographic.Age.Group.HRP", "Demographic.Age.Partner", "Demographic.Age.Respondent", 
                                                        "Demographic.Child.Gender", "Demographic.Child.Tax.Credit", "Demographic.Child.Benefit", 
                                                        "Demographic.Cohort", "Demographic.Disabled", "Demographic.Derived.Household.Type.LCFS", 
                                                        "Demographic.Derived.Household.Type.LCFS.child", "Demographic.ESA", "Demographic.Expenditure.Decile", 
                                                        "Demographic.FishyV3", "Demographic.Housing", "Demographic.Housing.Benefit", "Demographic.Housing.Tenure.LCFS", 
                                                        "Demographic.HRP.LCFS", "Demographic.Income.Decile", "Demographic.Income.Support", "Demographic.JSA.Combined", 
                                                        "Demographic.JSA.Contribution.Based", "Demographic.JSA.Income.Based", "Demographic.Means.Tested",  "Demographic.Negbud", "Demographic.Negbud.LCFS",
                                                        
                                                        "Demographic.PIP.Car", "Demographic.PIP.Mob", "Demographic.Pension.Credit", 
                                                        "Demographic.Relationship.LCFS", "Demographic.Role.Type", "Demographic.Role.Type.Partner", 
                                                        "Demographic.Region.LCFS", "Demographic.Surplus.Bands.V1", "Demographic.Surplus.Bands.V2", 
                                                        "Demographic.UC", "Demographic.WTC", "Demographic.ethnicity.HRP", "Demographic.ethnicity.HRP.Partner", 
                                                        "Demographic.Household.Type.LCFS", "LoE_Equivalised_V1",
                                                        "LoE_Equivalised_V2")
                                )
                         ),
                         column(4, 
                                selectInput(inputId = "Tab17Variable2",
                                            label = "Choose a numeric variable:",
                                            choices = c("Surplus", "Surplus.LCFS" ,"Expenditure.Comms.Mobile.Phone", 
                                                        "Expenditure.Council.Tax", "Expenditure.Council.Tax.Support", "Expenditure.Decile.Numeric", 
                                                        "Expenditure.Flexible.Total.LCFS", "Expenditure.Flexible.Total.LoE", "Expenditure.Fixed.Total", 
                                                        "Expenditure.Food.House.Keeping", "Expenditure.Food.House.Keeping.MART", "Expenditure.Health", "Expenditure.Housing", 
                                                        "Expenditure.Housing.LCFS", "Expenditure.Leisure.Cost", "Expenditure.Leisure.MART", "Expenditure.Mortgage", "Expenditure.Others", 
                                                        "Expenditure.Personal.Cost", "Expenditure.Personal.MART", "Expenditure.Rent", "Expenditure.Rent.Net", 
                                                        "Expenditure.Total","Expenditure.Total.LCFS" , "Expenditure.Transport", 
                                                        "Expenditure.Utilities", "Expenditure.Utilities.MART", "Expenditure.Vehicle.Insurance", "Expenditure.Water", 
                                                        "Income.Carer.Allowance", "Income.Child.Benefit", "Income.Child.Tax.Credit", "Income.Decile.Numeric", 
                                                        "Income.ESA", "Income.ExtPaym.Housing.Benefit", "Income.Gross.Wages.Partner", "Income.Gross.Wages.Respondent", 
                                                        "Income.Housing.Benefit", "Income.Income.Support", "Income.JSA.Combined", "Income.JSA.Contribution.Based", 
                                                        "Income.JSA.Income.Based", "Income.Pension.Credit", "Income.PIP.Car", "Income.PIP.Mob", 
                                                        "Income.Takehome.Wages.Partner", "Income.Takehome.Wages.Respondent", "Income.Total", 
                                                        "Income.Total.Benefits", "Income.UC", "Income.WTC","Income.Equivalised",
                                                        "Expenditure.Equivalised",
                                                        "Surplus.Equivalised")
                                )
                         ),
                         column(4, 
                                selectInput(inputId = "Tab17Variable3",
                                            label = "Select a Year:",
                                            choices = c("2020-21", "2021-22", "2022-23", "2023-24")
                                )
                         )
                       ),
                       plotlyOutput(outputId = "plot17")
                ),
                downloadButton("download17b", "Download Graph")
              ),
 
    )
  )
)
)
)
,

# server time...
server = shinyServer(function(input,output, session){
  
  MART.Dash_original <<- MART.Dash # backup original data
  
  # output$variablePicker <- renderUI({
  #   selectInput("group_var", "Choose a grouping variable:", choices = names(MART.Dash)[sapply(MART.Dash, is.factor)])
  # })
  
  output$variablePicker <- renderUI({
    # Get all factor variables, excluding "Index", "Month", and "Quarter"
    factor_vars <- names(MART.Dash)[sapply(MART.Dash, is.factor)]
    excluded_vars <- c("Index", "Month", "Quarter")
    valid_vars <- setdiff(factor_vars, excluded_vars)
    
    # Create the selectInput with the valid variables
    selectInput(
      "group_var", 
      "Choose a grouping variable:", 
      choices = valid_vars
    )
  })
  
  output$levelPicker <- renderUI({
    req(input$group_var)
    pickerInput('group_levels', 'Select levels:', choices = unique(MART.Dash[, input$group_var]), multiple = TRUE, options = list(`actions-box` = TRUE))
  })
  

  
  output$numericVariablePicker <- renderUI({
    selectInput("numeric_var", "Choose a numeric variable:", choices = c("No filter needed", names(MART.Dash[sapply(MART.Dash, is.numeric)])))
  })
  
  output$numericFilterTypePicker <- renderUI({
    selectInput("numeric_filter_type", "Choose a filter type:", choices = c("Greater than", "Less than", "Equal to", "Between"))
  })
  
  # output$numericValuePicker <- renderUI({
  #   if (input$numeric_var != "No filter needed") {
  #     req(input$numeric_var)
  #     req(input$numeric_filter_type)
  #     if (input$numeric_filter_type == "Between") {
  #       sliderInput('numeric_range', 'Set range:', min = min(MART.Dash[, input$numeric_var], na.rm = TRUE),
  #                   max = max(MART.Dash[, input$numeric_var], na.rm = TRUE), value = c(min(MART.Dash[, input$numeric_var], na.rm = TRUE),
  #                                                                                      max(MART.Dash[, input$numeric_var], na.rm = TRUE)))
  #     } else {
  #       numericInput('numeric_value', 'Set value:', value = mean(MART.Dash[, input$numeric_var], na.rm = TRUE))
  #     }
  #   }
  # })
  
  output$numericValuePicker <- renderUI({
    if (input$numeric_var != "No filter needed") {
      req(input$numeric_var)
      req(input$numeric_filter_type)
      
      if (input$numeric_filter_type == "Between") {
        fluidRow(
          column(6, numericInput('numeric_lower', 'Set lower bound:', 
                                 value = min(MART.Dash[, input$numeric_var], na.rm = TRUE))),
          column(6, numericInput('numeric_upper', 'Set upper bound:', 
                                 value = max(MART.Dash[, input$numeric_var], na.rm = TRUE)))
        )
      } else {
        numericInput('numeric_value', 'Set value:', value = mean(MART.Dash[, input$numeric_var], na.rm = TRUE))
      }
    }
  })
  
  
  observeEvent(input$confirm, {
    if (!is.null(input$group_var) && !is.null(input$group_levels)) {
      MART.Dash <<- MART.Dash %>% filter(get(input$group_var) %in% input$group_levels)
      output$filterStatus <- renderText("By jove you've only went and set a filter!")
    }
    
    if (input$numeric_var != "No filter needed") {
      if (input$numeric_filter_type == "Greater than") {
        MART.Dash <<- MART.Dash %>% filter(get(input$numeric_var) > input$numeric_value)
        output$filterStatus <- renderText("By jove you've only went and set a filter!")
      } else if (input$numeric_filter_type == "Less than") {
        MART.Dash <<- MART.Dash %>% filter(get(input$numeric_var) < input$numeric_value)
        output$filterStatus <- renderText("By jove you've only went and set a filter!")
      } else if (input$numeric_filter_type == "Equal to") {
        MART.Dash <<- MART.Dash %>% filter(get(input$numeric_var) == input$numeric_value)
        output$filterStatus <- renderText("By jove you've only went and set a filter!")
      } else if (input$numeric_filter_type == "Between") {
        MART.Dash <<- MART.Dash %>% filter(get(input$numeric_var) >= input$numeric_range[1] & get(input$numeric_var) <= input$numeric_range[2])
        output$filterStatus <- renderText("By jove you've only went and set a filter!")
      }
    }
  })
  
  observeEvent(input$reset, {
    MART.Dash <<- MART.Dash_original # restore original data
    output$filterStatus <- renderText("OK, phew, back to normal!")
  })
  
  # Make tab 1 table
  # table_data <- reactive({
  #   
  #   # MART.Counts(input$Variable1, input$Variable2)
  #   #MART.Dash <- readRDS("MART.Dash.rds")
  #   Time = sym(input$Variable1)
  #   Variable = sym(input$Variable2)
  #   
  #   data <- table(MART.Dash[[Time]], MART.Dash[[Variable]])
  #   data2 <- as.data.frame(data)
  #   
  #   data2
  #   
  # })
  
  # Make tab 1 table
  table_data <- reactive({
    
    # Convert your data to a survey design object
    survey_data <- MART.Dash %>%
      as_survey_design(weights = Weight.LCFS)
    
    # Dynamic variable reference using `rlang`
    Time = sym(input$Variable1)
    Variable = sym(input$Variable2)
    
    # Compute the table counts
    count_data <- survey_data %>%
      as_survey_design(weights = Weight.LCFS) %>% 
      group_by_at(vars(Time, Variable)) %>%
        summarise(n=survey_total()) %>% 
   as.data.frame() 
    
    # Compute the table counts
    count_data_unweighted <- MART.Dash %>%
      group_by_at(vars(Time, Variable)) %>%
      summarise(n=n()) %>% 
      as.data.frame() 
    
    count_data <- count_data %>% 
      mutate(Households = (n*1000)/2) %>% 
      mutate(Households = case_when(Financial.Year == "2020-21" ~ Households*2,
                                    TRUE ~ Households)) %>% 
      mutate_if(is.numeric, round, 0) 
    
    count_data$n <- count_data_unweighted$n

    
    
    return(count_data)
  })
  
  
  
  
  # output$table1 <- DT::renderDataTable({
  #   
  #   dt <- DT::datatable(table_data(),options = list(scrollY = "200px"), class = "cell-border")
  #   
  #   # Apply conditional formatting
  #   dt <- dt %>%
  #     formatStyle(
  #       'n',  # name of the column
  #       backgroundColor = styleInterval(
  #         c(100, 300), 
  #         c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
  #       )
  #     )
  #   
  #   dt
  #   
  # })
  
  output$table1 <- DT::renderDataTable({
    
    # Get the current data from the reactive function
    data_current <- table_data()
    
    # Retrieve the chosen variable and Time dynamically
    chosen_variable <- input$Variable2
    chosen_time <- input$Variable1
    
    # Check if both columns exist in the dataset
    if(!(chosen_variable %in% colnames(data_current) & chosen_time %in% colnames(data_current))) {
      stop("One of the chosen columns is not found in the dataset!")
    }
    
    # Define the columns to display
    columns_to_display <- c(chosen_time, chosen_variable, "n", "Households")
    
    # Subset the data to show only the desired columns
    data_to_show <- data_current[, columns_to_display, drop = FALSE]
    
    # Render the datatable
    dt <- DT::datatable(data_to_show, options = list(scrollY = "200px"), class = "cell-border")
    
    # Apply conditional formatting to 'n'
    dt <- dt %>%
      formatStyle(
        'n',
        backgroundColor = styleInterval(
          c(100, 300),
          c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
        )
      )
    
    dt
  })
  
  
  
  # Download tab 1 data
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Variable2, ".csv")
    },
    content = function(file) {
      write.csv(table_data(),file)
    }
  )
  
  # tab2_data <- reactive({
  #   
  #   # Convert your data to a survey design object
  #   survey_data <- MART.Dash %>%
  #     as_survey_design(weights = Weight.LCFS)
  #   
  #   # Dynamic variable reference using `rlang`
  #   Time <- sym(input$Tab2Variable1)
  #   Variable <- sym(input$Tab2Variable2)
  #   
  #   # Identify combined data based on selected time variable
  #   combined_levels <- switch(as.character(Time),
  #                             "Year" = c("2021", "2022"),
  #                             "Financial.Year" = c("2021-22", "2022-23"),
  #                             "Year.Month" = c(paste0(month.abb, " 2021"), paste0(month.abb, " 2022")),
  #                             "Quarter" = c("2021-2", "2021-3", "2021-4", "2022-1"),
  #                             "Financial.Quarter" = c("2021-22.Q1", "2021-22.Q2", "2021-22.Q3", "2022-23.Q4"),
  #                             NULL
  #   )
  #   
  #   # Adjust weights for combined data
  #   survey_data <- survey_data %>%
  #     mutate(IsCombined = ifelse(!!Time %in% combined_levels, 0.5, 1))
  #   
  #   # Sample n and Weighted proportions
  #   weighted_data <- survey_data %>%
  #     group_by(!!Time, !!Variable) %>%
  #     summarise(
  #       n = n(),
  #       WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
  #     ) %>%
  #     spread(!!Variable, WeightedPercent) %>%
  #     mutate(across(where(is.numeric), ~round(. ,2)))
  #   
  #   # Calculate Number of households
  #   household_data <- survey_data %>%
  #     group_by(!!Time) %>%
  #     summarise(Households = survey_total(vartype = "ci") * mean(IsCombined) * 1000) %>%
  #     mutate(across(where(is.numeric), ~round(. ,2)))
  #   
  #   # Combine all data
  #   Data <- left_join(weighted_data, household_data, by = rlang::as_string(Time))
  #   
  #   # step 1: find column range
  #   end_col <- which(names(Data) == "Households") - 1
  #   cols_for_calculation <- names(Data)[5:end_col]
  #   
  #   # Step 2: Calculate only_value using these columns
  #   Data <- Data %>%
  #     rowwise() %>%
  #     mutate(only_value = sum(c_across(all_of(cols_for_calculation)), na.rm = TRUE))
  #   
  #   # Step 3: Using this value to create the Households column
  #   Data <- Data %>%
  #     mutate(Households = round(only_value * Households / 100, 0))
  #   
  #   # Optional: If you want to remove the temporary 'only_value' column
  #   Data <- Data %>% select(-only_value) 
  #   
  #     return(Data)
  # })
  # 
  
  tab2_data <- reactive({
    
    # Convert your data to a survey design object
    survey_data <- MART.Dash %>%
      as_survey_design(weights = Weight.LCFS)
    
    # Dynamic variable reference using `rlang`
    Time <- sym(input$Tab2Variable1)
    Variable <- sym(input$Tab2Variable2)
    
    # If the Variable is not Demographic.Cohort
    if(as.character(Variable) != "Demographic.Cohort") {
      
      combined_levels <- switch(as.character(Time),
                                "Year" = c("2021", "2022"),
                                "Financial.Year" = c("2021-22", "2022-23"),
                                "Year.Month" = c(paste0(month.abb, " 2021"), paste0(month.abb, " 2022")),
                                "Quarter" = c("2021-2", "2021-3", "2021-4", "2022-1"),
                                "Financial.Quarter" = c("2021-22.Q1", "2021-22.Q2", "2021-22.Q3", "2022-23.Q4"),
                                NULL
      )
      
      # Adjust weights for combined data
      survey_data <- survey_data %>%
        mutate(IsCombined = ifelse(!!Time %in% combined_levels, 0.5, 1))
      
    } else {
      survey_data <- survey_data %>%
        mutate(IsCombined = 1)
    }
    
    # Sample n and Weighted proportions
    weighted_data <- survey_data %>%
      group_by(!!Time, !!Variable) %>%
      summarise(
        n = n(),
        WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
      ) %>%
      spread(!!Variable, WeightedPercent) %>%
      mutate(across(where(is.numeric), ~round(. ,2)))
    
    # Calculate Number of households
    household_data <- survey_data %>%
      group_by(!!Time) %>%
      summarise(Households = survey_total(vartype = "ci") * mean(IsCombined) * 1000) %>%
      mutate(across(where(is.numeric), ~round(. ,2)))
    
    # Combine all data
    Data <- left_join(weighted_data, household_data, by = rlang::as_string(Time))
    
    # step 1: find column range
    end_col <- which(names(Data) == "Households") - 1
    cols_for_calculation <- names(Data)[5:end_col]
    
    # Step 2: Calculate only_value using these columns
    Data <- Data %>%
      rowwise() %>%
      mutate(only_value = sum(c_across(all_of(cols_for_calculation)), na.rm = TRUE))
    
    # Step 3: Using this value to create the Households column
    Data <- Data %>%
      mutate(Households = round(only_value * Households / 100, 0)) %>% 
      mutate(Households = case_when(Financial.Year == "2023-24" ~ Households/2,
                                    TRUE ~ Households)) %>% 
      mutate_if(is.numeric, round, 0) 
    
    # Optional: If you want to remove the temporary 'only_value' column
    Data <- Data %>% select(-only_value) 
    
    return(Data)
  })
  
  
  output$table2 <- DT::renderDataTable({
    
    # Get the current data from the reactive function
    data_current <- tab2_data()
    
    # Obtain the levels of the chosen factor variable from the original data
    factor_levels <- unique(MART.Dash[[input$Tab2Variable2]])
    existing_levels <- colnames(data_current)[colnames(data_current) %in% factor_levels]
    
    # Define the columns to display
    columns_to_display <- c(colnames(data_current)[1],"n", existing_levels, "Households")
    
    # Subset the data to show only the desired columns
    data_to_show <- data_current[, columns_to_display, drop = FALSE]
    
    # Render the datatable
    dt <- DT::datatable(data_to_show, options = list(scrollY = "200px"), class = "cell-border")
    
    # Check if 'n' exists in the subsetted data and apply style if it does
    if ('n' %in% colnames(data_to_show)) {
      dt <- dt %>%
        formatStyle(
          'n',
          backgroundColor = styleInterval(
            c(100, 300),
            c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
          )
        )
    }
    
    dt
  })

  
  
    # Download tab 2 data 
  output$download2 <- downloadHandler(
    filename = function() {
      paste0(input$Tab2Variable2, ".csv")
    },
    content = function(file) {
      write.csv(tab2_data(),file)
    }
  )
  
  # # Make tab 3 data
  # tab3_data <- reactive({
  #   
  #   # Convert your data to a survey design object
  #   survey_data <- MART.Dash %>%
  #     as_survey_design(weights = Weight.LCFS)
  #   
  #   # Dynamic variable reference using `rlang`
  #   Time <- sym(input$Tab3Variable1)
  #   Var1 <- sym(input$Tab3Variable2)
  #   Var2 <- sym(input$Tab3Variable3)
  #   
  #   # Sample n and Weighted proportions for both variables
  #   weighted_data <- survey_data %>%
  #     group_by(!!Time, !!Var1, !!Var2) %>%
  #     summarise(
  #       n = n(),
  #       WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
  #     ) %>%
  #     spread(!!Var2, WeightedPercent) %>%
  #     mutate(across(where(is.numeric), ~round(. ,2)))
  #   
  #   # Calculate Number of households
  #   household_data <- survey_data %>%
  #     group_by(!!Time, !!Var1) %>%
  #     summarise(Households = survey_total(vartype = "ci")*1000) %>%
  #     mutate(across(where(is.numeric), ~round(. ,2)))
  #   
  #   # Combine all data
  #   Data <- left_join(weighted_data, household_data, by = c(rlang::as_string(Time), rlang::as_string(Var1)))
  #   
  #   return(Data)
  # })
  
  # Make tab 3 data
  tab3_data <- reactive({
    
    # Convert your data to a survey design object
    survey_data <- MART.Dash %>%
      as_survey_design(weights = Weight.LCFS)
    
    # Dynamic variable reference using `rlang`
    Time <- sym(input$Tab3Variable1)
    Var1 <- sym(input$Tab3Variable2)
    Var2 <- sym(input$Tab3Variable3)
    
    # If the Variable1 or Variable2 is not Demographic.Cohort
    if(as.character(Var1) != "Demographic.Cohort" && as.character(Var2) != "Demographic.Cohort") {
      
      combined_levels <- switch(as.character(Time),
                                "Year" = c("2021", "2022"),
                                "Financial.Year" = c("2021-22", "2022-23"),
                                "Year.Month" = c(paste0(month.abb, " 2021"), paste0(month.abb, " 2022")),
                                "Quarter" = c("2021-2", "2021-3", "2021-4", "2022-1"),
                                "Financial.Quarter" = c("2021-22.Q1", "2021-22.Q2", "2021-22.Q3", "2022-23.Q4"),
                                NULL
      )
      
      # Adjust weights for combined data
      survey_data <- survey_data %>%
        mutate(IsCombined = ifelse(!!Time %in% combined_levels, 0.5, 1))
      
    } else {
      survey_data <- survey_data %>%
        mutate(IsCombined = 1)
    }
    
    # Sample n and Weighted proportions for both variables
    weighted_data <- survey_data %>%
      group_by(!!Time, !!Var1, !!Var2) %>%
      summarise(
        n = n(),
        WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
      ) %>%
      spread(!!Var2, WeightedPercent) %>%
      mutate(across(where(is.numeric), ~round(. ,2)))
    
    # Calculate Number of households
    household_data <- survey_data %>%
      group_by(!!Time, !!Var1) %>%
      summarise(Households = survey_total(vartype = "ci") * mean(IsCombined) * 1000) %>%
      mutate(across(where(is.numeric), ~round(. ,2)))
    
    # Combine all data
    Data <- left_join(weighted_data, household_data, by = c(rlang::as_string(Time), rlang::as_string(Var1)))
    
    # Step 1: find column range
    end_col <- which(names(Data) == "Households") - 1
    cols_for_calculation <- names(Data)[5:end_col]
    
    # Step 2: Calculate only_value using these columns
    Data <- Data %>%
      rowwise() %>%
      mutate(only_value = sum(c_across(all_of(cols_for_calculation)), na.rm = TRUE))
    
    # Step 3: Using this value to create the Households column
    Data <- Data %>%
      mutate(Households = round(only_value * Households / 100, 0)) %>% 
      mutate(Households = case_when(Financial.Year == "2023-24" ~ Households/4,
                                    TRUE ~ Households/2)) %>% 
      mutate(Households = round(Households, 0))
    
    # Optional: If you want to remove the temporary 'only_value' column
    Data <- Data %>% select(-only_value) 
    
    return(Data)
  })
  
  
  output$table3 <- DT::renderDataTable({
    
    # Get the current data from the reactive function
    data_current <- tab3_data()
    
    # Identify the indices of the "WeightedPercent_upp" and "Households" columns
    start_idx <- which(colnames(data_current) == "WeightedPercent_upp") + 1
    end_idx <- which(colnames(data_current) == "Households") - 1
    
    # Columns to display
    columns_to_display <- c(colnames(data_current)[1:3], colnames(data_current)[start_idx:end_idx], "Households")
    
    # Subset the data to display only the desired columns
    data_to_show <- data_current[, columns_to_display, drop = FALSE]
    
    # Render the datatable with conditional formatting applied to 'n'
    DT::datatable(data_to_show, options = list(scrollY = "200px"), class = "cell-border") %>%
      formatStyle(
        'n',
        backgroundColor = styleInterval(
          c(100, 300),
          c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
        )
      )
  })
  

   # Download tab 3 data
   output$download3 <- downloadHandler(
     filename = function() {
       paste0(input$Tab3Variable2, ".csv")
     },
     content = function(file) {
       write.csv(tab3_data(),file)
     }
   )
   
   # Table 4 data
   tab4_data <- reactive({
     
     Time <- sym(input$Tab4Variable1)
     Variable = sym(input$Tab4Variable2)
     
     # Filter conditionally for Surplus
     if (as_string(Variable) == "Surplus") {
       data_filtered <- MART.Dash
     } else {
       data_filtered <- MART.Dash %>% filter({{Variable}} != 0)
     }
     
     combined_levels <- switch(as.character(Time),
                               "Year" = c("2021", "2022"),
                               "Financial.Year" = c("2021-22", "2022-23"),
                               "Year.Month" = c(paste0(month.abb, " 2021"), paste0(month.abb, " 2022")),
                               "Quarter" = c("2021-2", "2021-3", "2021-4", "2022-1"),
                               "Financial.Quarter" = c("2021-22.Q1", "2021-22.Q2", "2021-22.Q3", "2022-23.Q4"),
                               NULL
     )
     
     # Convert your data to a survey design object
     survey_data <- as_survey_design(data_filtered, weights = Weight.LCFS) 
     
     # Adjust weights for combined data
     survey_data <- survey_data %>%
       mutate(IsCombined = ifelse(!!Time %in% combined_levels, 0.5, 1))
     
     # Unweighted stats
     unweighted_data <- data_filtered %>% 
       group_by({{Time}}) %>% 
       summarise(
         n = n(),
         UnweightedMean = mean({{Variable}}, na.rm=TRUE),
         UnweightedMedian = median({{Variable}}, na.rm=TRUE)
       )
     
     # Weighted stats
     weighted_data <- survey_data %>% 
       group_by(!!Time) %>%
       summarise(
         WeightedMean = survey_mean(!!Variable, na.rm = TRUE,vartype = "ci"),
         WeightedMedian = survey_median(!!Variable, na.rm = TRUE,vartype = "ci")
       )
     
     # Calculate Number of households
     household_data <- survey_data %>%
       group_by(!!Time) %>%
       summarise(Households = survey_total(vartype = "ci") * mean(IsCombined) * 1000) %>%
       mutate(across(where(is.numeric), ~round(. ,2)))
     
     # Combine all data
     Data <- left_join(unweighted_data, weighted_data, by = rlang::as_string(Time))
     Data <- left_join(Data, household_data, by = rlang::as_string(Time))
     
     Data <- Data %>% 
       mutate(across(where(is.numeric) & !Households, ~round(., 2)), 
              Households = round(Households, 0)) %>% 
       mutate(Households = case_when(Financial.Year == "2023-24" ~ Households/2,
                                     TRUE ~ Households)) %>% 
       mutate(Households = round(Households, 0))
     
     return(Data)
   })
   
   # Return tab 4 data 
   output$table4 <- DT::renderDataTable({
     
     # Subset data to retain desired columns
     data_to_show <- tab4_data() %>%
       select(1:5, 8, 11)
     
     # Render the datatable
     dt <- DT::datatable(data_to_show, options = list(scrollY = "200px"),
                         class = "cell-border")
     
     # Apply conditional formatting to the 'n' column
     dt <- dt %>% formatStyle(
       'n',
       backgroundColor = styleInterval(
         c(100, 300),
         c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
       )
     )
     
     dt
     
   })

   # Download tab 4 data
   output$download4 <- downloadHandler(
     filename = function() {
       paste0(input$Tab4Variable2, ".csv")
     },
     content = function(file) {
       write.csv(tab4_data(),file)
     }
   )
  
   
   tab5_data <- reactive({
     
     Time <- sym(input$Tab5Variable1)
     Var.Group <- sym(input$Tab5Variable2)
     Var.Number <- sym(input$Tab5Variable3)
     
     # Filter conditionally for Surplus
     if (as_string(Var.Number) == "Surplus") {
       data_filtered <- MART.Dash
     } else {
       data_filtered <- MART.Dash %>% filter({{Var.Number}} != 0)
     }
     
     # Convert your data to a survey design object
     survey_data <- as_survey_design(data_filtered, weights = Weight.LCFS) 
     
     # If the Variable1 or Variable2 is not Demographic.Cohort
     if(as.character(Var.Group) != "Demographic.Cohort" ) {
       
       combined_levels <- switch(as.character(Time),
                                 "Year" = c("2021", "2022"),
                                 "Financial.Year" = c("2021-22", "2022-23"),
                                 "Year.Month" = c(paste0(month.abb, " 2021"), paste0(month.abb, " 2022")),
                                 "Quarter" = c("2021-2", "2021-3", "2021-4", "2022-1"),
                                 "Financial.Quarter" = c("2021-22.Q1", "2021-22.Q2", "2021-22.Q3", "2022-23.Q4"),
                                 NULL
       )
       
       # Adjust weights for combined data
       survey_data <- survey_data %>%
         mutate(IsCombined = ifelse(!!Time %in% combined_levels, 0.5, 1))
       
     } else {
       survey_data <- survey_data %>%
         mutate(IsCombined = 1)
     }
     
     # Unweighted stats
     unweighted_data <- data_filtered %>% 
       group_by({{Time}}, {{Var.Group}}) %>% 
       summarise(
         n = n(),
         UnweightedMean = mean({{Var.Number}}, na.rm=TRUE),
         UnweightedMedian = median({{Var.Number}}, na.rm=TRUE)
       )
     
     # Weighted stats
     weighted_data <- survey_data %>% 
       group_by(!!Time, !!Var.Group) %>%
       summarise(
         WeightedMean = survey_mean(!!Var.Number, na.rm = TRUE,vartype = "ci"),
         WeightedMedian = survey_median(!!Var.Number, na.rm = TRUE,vartype = "ci")
       )
     
     # Calculate Number of households
     household_data <- survey_data %>%
       group_by(!!Time, !!Var.Group) %>%
       summarise(Households = survey_total(vartype = "ci") * mean(IsCombined) * 1000) %>%
       mutate(across(where(is.numeric), ~round(. ,2)))
     
     # Combine all data
     Data <- left_join(unweighted_data, weighted_data, by = c(rlang::as_string(Time), rlang::as_string(Var.Group)))
     Data <- left_join(Data, household_data, by = c(rlang::as_string(Time), rlang::as_string(Var.Group)))
     
     # Data <- Data %>% 
     #   mutate(across(where(is.numeric) & !Households, ~round(., 2)), 
     #          Households = round(Households, 0))
     
     Data <- Data %>%
       mutate(across(where(is.numeric) & !Households, ~round(., 2)),
              Households = round(Households, 0)) %>%
       mutate(Households = case_when(Financial.Year == "2023-24" ~ Households/2,
                                     TRUE ~ Households)) %>%
       mutate(Households = round(Households, 0))
     
     return(Data)
   })
   
   # # Return tab 5 data 
   output$table5 <- DT::renderDataTable({

     # Subset data to retain desired columns
     data_to_show <- tab5_data() %>%
       select(1:6, 9, 12)
     
     # Render the datatable
     dt <- DT::datatable(data_to_show, options = list(scrollY = "200px"),
                         class = "cell-border")

     # Apply conditional formatting to the columns
     # Replace 'ColumnName' with the name of the column you want to format
     dt <- dt %>% formatStyle(
       'n',
       backgroundColor = styleInterval(
         c(100, 300),
         c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
       )
     )

     dt

   })

   # # Download tab 5 data 
   output$download5 <- downloadHandler(
     filename = function() {
       paste0(input$Tab5Variable3, ".csv")
     },
     content = function(file) {
       write.csv(tab5_data(),file)
     }
   )
   
  # Make tab 7 data
  tab7_data <- reactive({
    
  # Projection.Expenditure.MEGA(input$number7a, input$number7b, input$number7c, input$number7d,
  #                               input$number7e, input$number7f, input$number7g, input$number7h)
    
    #MART.Dash <- readRDS("MART.Dash.rds")
    
    Water <- input$number7a
    Communications <- input$number7b
    Car_Insurance <- input$number7c
    Mortgage <- input$number7d
    Transport <- input$number7e
    Food <- input$number7f
    Energy <- input$number7g
    Leisure <- input$number7h
    
    if (Mortgage > 0) {
      dat <- MART.Dash %>% 
        dplyr::filter(Expenditure.Mortgage > 0)
    } else {
      dat <- MART.Dash
    }
    
    Prop1 <- (100-Water)/100
    Prop2 <- (100-Communications)/100
    Prop3 <- (100-Car_Insurance)/100
    Prop4 <- (100-Mortgage)/100
    Prop5 <- (100-Transport)/100
    Prop6 <- (100-Food)/100
    Prop7 <- (100-Energy)/100
    Prop8 <- (100-Leisure)/100
    dat <- dat %>% 
      dplyr::mutate(New.Water = Expenditure.Water*(Prop1),
                    New.Communications = Expenditure.Comms.Mobile.Phone*(Prop2),
                    New.Car.Insurance = Expenditure.Vehicle.Insurance*(Prop3),
                    New.Mortgage = Expenditure.Mortgage*(Prop4),
                    New.Transport = Expenditure.Transport*(Prop5),
                    New.Food = Expenditure.Food.House.Keeping.MART*(Prop6),
                    New.Energy = Expenditure.Utilities.MART*(Prop7),
                    New.Leisure = Expenditure.Leisure.MART*(Prop8),
                    New.Expenditure.Total = Expenditure.Total.Essential.LCFS.MART - ((Expenditure.Water - New.Water)+(Expenditure.Comms.Mobile.Phone - New.Communications)+
                                                                   (Expenditure.Vehicle.Insurance - New.Car.Insurance)+(Expenditure.Mortgage - New.Mortgage)+
                                                                   (Expenditure.Transport - New.Transport)+(Expenditure.Food.House.Keeping.MART - New.Food)+
                                                                   (Expenditure.Utilities.MART - New.Energy)+(Expenditure.Leisure.MART - New.Leisure)),
                    
                    New.Surplus = Income.Total.Disposable - New.Expenditure.Total,
                    New.Negative.Budget = case_when(New.Surplus <= 0 ~ 'Negative.Budget',
                                                    TRUE ~ 'Positive.Budget'),
                    
                    
                    Budget.Change = case_when(Demographic.Negbud == "Negative.Budget" & 
                                                New.Negative.Budget == "Positive.Budget" ~ 'Changed',
                                              TRUE ~ 'Unchanged'))
    
    dat.subset <- subset(dat, Financial.Year == levels(Financial.Year)[length(levels(Financial.Year))])
    
    # Convert data to a survey design object using srvyr for weighted calculations
    survey_data <- dat.subset %>% as_survey_design(weights = Weight.LCFS)
    
    Prop.Table <- survey_data %>%
      group_by(New.Negative.Budget) %>%
      summarise(
        WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
      ) %>%
      filter(New.Negative.Budget == "Negative.Budget") %>% 
      rename("Most.Recent.FY" = "New.Negative.Budget") 
    
    # Weighted calculation for old.dat
    old.dat <- survey_data %>%
      group_by(Demographic.Negbud) %>%
      summarise(
        WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
      ) %>%
      filter(Demographic.Negbud == "Negative.Budget") %>% 
      rename("Most.Recent.FY" = "Demographic.Negbud") 
    
    New.Surplus <- survey_data %>%
      # dplyr::filter(Expenditure.Water > 0 & Expenditure.Comms.Mobile.Phone > 0 &
      #                 Expenditure.Broadband > 0 & Expenditure.Insurance.Total > 0) %>%
      group_by(Financial.Year) %>% 
      summarise(Weighted_Median = survey_median(New.Surplus, na.rm = TRUE, vartype = "ci")) %>% 
      filter(Financial.Year == levels(dat$Financial.Year)[length(levels(dat$Financial.Year))])
    
    Old.Surplus <- survey_data %>%
      # dplyr::filter(Expenditure.Water > 0 & Expenditure.Mobile > 0 &
      #                 Expenditure.Broadband > 0 & Expenditure.Insurance.Total > 0) %>%
      group_by(Financial.Year) %>% 
      summarise(Weighted_Median = survey_median(Surplus, na.rm = TRUE, vartype = "ci")) %>% 
      filter(Financial.Year == levels(dat$Financial.Year)[length(levels(dat$Financial.Year))])
    
    # Houshehold data
    # Calculate number of households in negative budget for Prop.Table
    Prop.Table_Households <- survey_data %>%
      filter(New.Negative.Budget == "Negative.Budget") %>%
      summarise(Households = survey_total(na.rm = TRUE,vartype = "ci")*1000) 
    
    # Calculate number of households in negative budget for old.dat
    old.dat_Households <- survey_data %>%
      filter(Demographic.Negbud == "Negative.Budget") %>%
      summarise(Households = survey_total(na.rm = TRUE,vartype = "ci")*1000) 
    
    
    # Subtract the difference
    Household_Difference <- Prop.Table_Households$Households[1] - old.dat_Households$Households[1]
    Household_Difference <- Household_Difference/2
    
    Final.Output <- rbind(Prop.Table,old.dat) %>% 
      mutate(Most.Recent.FY = c("New.Negative.Budget", "Old.Negative.Budget")) %>% 
      mutate(Surplus = c(New.Surplus$Weighted_Median[1], Old.Surplus$Weighted_Median[1])) %>% 
      mutate(Parameters = paste("W",Water, "C",Communications, "CI",Car_Insurance, "M",Mortgage, 
                                "T",Transport,"F",Food,
                                "E",Energy, "L", Leisure,sep = ".")) %>% 
      mutate(Difference_In_Households = Household_Difference) %>% 
      mutate_if(is.numeric,round,2) %>% 
      select(Parameters, everything())
    
    Final.Output
    
    
  })
  
  # return tab 7 data 
  output$table7 <- DT::renderDataTable({
    
    # Subset data to retain desired columns
    data_to_show <- tab7_data() %>%
      select(1:3, 6:7)
    
    # Render the datatable
    dt <- DT::datatable(data_to_show, options = list(scrollY = "200px"),
                        class = "cell-border")
  })
  
  # Download tab 7 data 
  output$download7 <- downloadHandler(
    filename = function() {
      paste0("Tariff", ".csv")
    },
    content = function(file) {
      write.csv(tab7_data(),file)
    }
  )
  
  # Make tab 8 data
  # tab8_data <- reactive({
  #   
  #   Per1 <- input$number8a
  #   Per2 <- input$number8b
  #   
  #   Exp1 <- sym(input$Tab8Variable1)
  #   Exp2 <- sym(input$Tab8Variable2)
  #   dat <- MART.Dash
  #   Prop1 <- (100-Per1)/100
  #   Prop2 <- (100-Per2)/100
  #   dat <- dat %>% dplyr::filter({{Exp1}} > 0 & {{Exp2}} > 0 ) %>% 
  #     dplyr::mutate(New.Exp1 = {{Exp1}}*(Prop1),
  #                   New.Exp2 = {{Exp2}}*(Prop2),
  #                   New.Expenditure.Total = Expenditure.Total.Essential.LCFS.MART - (({{Exp1}} - New.Exp1)+({{Exp2}} - New.Exp2)),
  #                   New.Surplus = Income.Total.Disposable - New.Expenditure.Total,
  #                   New.Negative.Budget = case_when(New.Surplus <= 0 ~ 'Negative.Budget', TRUE ~ 'Positive.Budget'))
  #   
  #   dat.subset <- subset(dat, Financial.Year == levels(Financial.Year)[length(levels(Financial.Year))])
  #   
  #   # Convert data to a survey design object using srvyr for weighted calculations
  #   survey_data <- dat.subset %>% as_survey_design(weights = Weight.LCFS)
  #   
  #   # Weighted calculation for Prop.Table
  #   # Sample n and Weighted proportions
  #   Prop.Table <- survey_data %>%
  #     group_by(New.Negative.Budget) %>%
  #     summarise(
  #       WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
  #     ) %>%
  #     filter(New.Negative.Budget == "Negative.Budget") %>% 
  #     rename("Most.Recent.FY" = "New.Negative.Budget") 
  #   
  #   # Weighted calculation for old.dat
  #   old.dat <- survey_data %>%
  #     group_by(Demographic.Negbud) %>%
  #     summarise(
  #       WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
  #     ) %>%
  #     filter(Demographic.Negbud == "Negative.Budget") %>% 
  #     rename("Most.Recent.FY" = "Demographic.Negbud") 
  #   
  #   Final.Output <- rbind(Prop.Table, old.dat) %>%
  #     mutate(Most.Recent.FY = c("New.Negative.Budget", "Old.Negative.Budget")) %>%
  #     mutate_if(is.numeric, round, 2)
  #   
  #   Final.Output
  # }) 
  
  tab8_data <- reactive({
    
    Per1 <- input$number8a
    Per2 <- input$number8b
    
    Exp1 <- sym(input$Tab8Variable1)
    Exp2 <- sym(input$Tab8Variable2)
    dat <- MART.Dash
    Prop1 <- (100-Per1)/100
    Prop2 <- (100-Per2)/100
    dat <- dat %>% dplyr::filter({{Exp1}} > 0 & {{Exp2}} > 0 ) %>% 
      dplyr::mutate(New.Exp1 = {{Exp1}}*(Prop1),
                    New.Exp2 = {{Exp2}}*(Prop2),
                    New.Expenditure.Total = Expenditure.Total.Essential.LCFS.MART - (({{Exp1}} - New.Exp1)+({{Exp2}} - New.Exp2)),
                    New.Surplus = Income.Total.Disposable - New.Expenditure.Total,
                    New.Negative.Budget = case_when(New.Surplus <= 0 ~ 'Negative.Budget', TRUE ~ 'Positive.Budget'))
    
    dat.subset <- subset(dat, Financial.Year == levels(Financial.Year)[length(levels(Financial.Year))])
    
    # Convert data to a survey design object using srvyr for weighted calculations
    survey_data <- dat.subset %>% as_survey_design(weights = Weight.LCFS)
    
    # Weighted calculation for Prop.Table
    Prop.Table <- survey_data %>%
      group_by(New.Negative.Budget) %>%
      summarise(
        WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
      ) %>%
      filter(New.Negative.Budget == "Negative.Budget") %>% 
      rename("Most.Recent.FY" = "New.Negative.Budget") 
    
    # Weighted calculation for old.dat
    old.dat <- survey_data %>%
      group_by(Demographic.Negbud) %>%
      summarise(
        WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
      ) %>%
      filter(Demographic.Negbud == "Negative.Budget") %>% 
      rename("Most.Recent.FY" = "Demographic.Negbud") 
    
    # Calculate number of households in negative budget for Prop.Table
    Prop.Table_Households <- survey_data %>%
      filter(New.Negative.Budget == "Negative.Budget") %>%
      summarise(Households = survey_total(na.rm = TRUE, vartype = "ci")*1000) 
    
    # Calculate number of households in negative budget for old.dat
    old.dat_Households <- survey_data %>%
      filter(Demographic.Negbud == "Negative.Budget") %>%
      summarise(Households = survey_total(na.rm = TRUE, vartype = "ci")*1000) 
    
    # Subtract the difference
    Household_Difference <- Prop.Table_Households$Households[1] - old.dat_Households$Households[1]
    Household_Difference <- Household_Difference/2
    
    Final.Output <- rbind(Prop.Table, old.dat) %>%
      mutate(Most.Recent.FY = c("New.Negative.Budget", "Old.Negative.Budget")) %>%
      mutate(Difference_In_Households = Household_Difference) %>% 
      mutate_if(is.numeric, round, 2)
    
    Final.Output
  })
  
  
  
  # return tab 8 data 
  output$table8 <- DT::renderDataTable({
    
    DT::datatable(tab8_data(),options = list(scrollY = "200px"),
                  class = "cell-border")
  })
  
  # Download tab 8 data 
  output$download8 <- downloadHandler(
    filename = function() {
      paste0(input$Tab8Variable1, ".csv")
    },
    content = function(file) {
      write.csv(tab8_data(),file)
    }
  )
  
  # Make tab 9 data
  # tab9_data <- reactive({
  #   
  # # Projection.Income(input$number9, input$Tab9Variable1)
  # # MART.Dash <- readRDS("MART_dash.rds")
  #   
  #   Percentage <- input$number9
  #   Income <- sym(input$Tab9Variable1)
  #   dat <- MART.Dash
  #   Proportion <- (100+Percentage)/100
  #   dat <- dat %>% dplyr::filter({{Income}} > 0) %>%
  #     dplyr::mutate(New.Income = {{Income}}*(Proportion),
  #                   New.Income.Total = Income.Total.Disposable + (New.Income - {{Income}}),
  #                   New.Surplus = New.Income.Total - Expenditure.Total.Essential.LCFS.MART,
  #                   New.Negative.Budget = case_when(New.Surplus <= 0 ~ 'Negative.Budget',
  #                                                   TRUE ~ 'Positive.Budget'))
  #   
  #   dat.subset <- subset(dat, Financial.Year == levels(Financial.Year)[length(levels(Financial.Year))])
  #   
  #   # Convert data to a survey design object using srvyr for weighted calculations
  #   survey_data <- dat.subset %>% as_survey_design(weights = Weight.LCFS)
  #   
  #   # Weighted calculation for Prop.Table
  #   # Sample n and Weighted proportions
  #   Prop.Table <- survey_data %>%
  #     group_by(New.Negative.Budget) %>%
  #     summarise(
  #       WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
  #     ) %>%
  #     filter(New.Negative.Budget == "Negative.Budget") %>% 
  #     rename("Most.Recent.FY" = "New.Negative.Budget") 
  #   
  #   # Weighted calculation for old.dat
  #   old.dat <- survey_data %>%
  #     group_by(Demographic.Negbud) %>%
  #     summarise(
  #       WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
  #     ) %>%
  #     filter(Demographic.Negbud == "Negative.Budget") %>% 
  #     rename("Most.Recent.FY" = "Demographic.Negbud") 
  #   
  #   Final.Output <- rbind(Prop.Table, old.dat) %>%
  #     mutate(Most.Recent.FY = c("New.Negative.Budget", "Old.Negative.Budget")) %>%
  #     mutate_if(is.numeric, round, 2)
  #   
  #   Final.Output
  #   
  #   
  # })
  
  tab9_data <- reactive({
    
    Percentage <- input$number9
    Income <- sym(input$Tab9Variable1)
    dat <- MART.Dash
    Proportion <- (100+Percentage)/100
    dat <- dat %>% dplyr::filter({{Income}} > 0) %>%
      dplyr::mutate(New.Income = {{Income}}*(Proportion),
                    New.Income.Total = Income.Total.Disposable + (New.Income - {{Income}}),
                    New.Surplus = New.Income.Total - Expenditure.Total.Essential.LCFS.MART,
                    New.Negative.Budget = case_when(New.Surplus <= 0 ~ 'Negative.Budget',
                                                    TRUE ~ 'Positive.Budget'))
    
    dat.subset <- subset(dat, Financial.Year == levels(Financial.Year)[length(levels(Financial.Year))])
    
    # Convert data to a survey design object using srvyr for weighted calculations
    survey_data <- dat.subset %>% as_survey_design(weights = Weight.LCFS)
    
    # Weighted calculation for Prop.Table
    Prop.Table <- survey_data %>%
      group_by(New.Negative.Budget) %>%
      summarise(
        WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
      ) %>%
      filter(New.Negative.Budget == "Negative.Budget") %>% 
      rename("Most.Recent.FY" = "New.Negative.Budget") 
    
    # Weighted calculation for old.dat
    old.dat <- survey_data %>%
      group_by(Demographic.Negbud) %>%
      summarise(
        WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
      ) %>%
      filter(Demographic.Negbud == "Negative.Budget") %>% 
      rename("Most.Recent.FY" = "Demographic.Negbud") 
    
    # Calculate number of households in negative budget for Prop.Table
    Prop.Table_Households <- survey_data %>%
      filter(New.Negative.Budget == "Negative.Budget") %>%
      summarise(Households = survey_total(na.rm = TRUE, vartype = "ci")*1000) 
    
    # Calculate number of households in negative budget for old.dat
    old.dat_Households <- survey_data %>%
      filter(Demographic.Negbud == "Negative.Budget") %>%
      summarise(Households = survey_total(na.rm = TRUE, vartype = "ci")*1000) 
    
    # Subtract the difference
    Household_Difference <- Prop.Table_Households$Households[1] - old.dat_Households$Households[1]
    Household_Difference <- Household_Difference/2
    
    Final.Output <- rbind(Prop.Table, old.dat) %>%
      mutate(Most.Recent.FY = c("New.Negative.Budget", "Old.Negative.Budget")) %>%
      mutate(Difference_In_Households = Household_Difference) %>% 
      mutate_if(is.numeric, round, 2)
    
    Final.Output
  })
  
  
  # return tab 9 data 
  output$table9 <- DT::renderDataTable({
    
    DT::datatable(tab9_data(),options = list(scrollY = "200px"),
                  class = "cell-border")
  })
  
  # Download tab 9 data 
  output$download9 <- downloadHandler(
    filename = function() {
      paste0(input$Tab9Variable1, ".csv")
    },
    content = function(file) {
      write.csv(tab9_data(),file)
    }
  )
  
  # # data for plot 10
  # table10_data <- reactive({
  #   
  #   # x-axis label
  #   Var.Number <- sym(input$Tab10Variable1)
  #   
  #   # Time Variable
  #   TimeVar <- sym(input$TimeVariable)
  #   
  #   # data for plot
  #   dat <- MART.Dash %>% 
  #     dplyr::filter({{Var.Number}} > 0) %>% 
  #     dplyr::group_by({{TimeVar}}) %>% 
  #     dplyr::summarise(Mean = mean({{Var.Number}}, na.rm=TRUE, trim = 0.2),
  #                      Median = median({{Var.Number}}),
  #                      n()) %>% 
  #     mutate_if(is.numeric,round,2)
  #   
  #   # create similar table using MART.Dash_original dataframe
  #   dat2 <- MART.Dash_original %>% 
  #     dplyr::filter({{Var.Number}} > 0) %>% 
  #     dplyr::group_by({{TimeVar}}) %>% 
  #     dplyr::summarise(Mean = mean({{Var.Number}}, na.rm=TRUE, trim = 0.2),
  #                      Median = median({{Var.Number}}),
  #                      n()) %>% 
  #     mutate_if(is.numeric,round,2) 
  #   
  #   # Check if the number of rows for MART.Dash is the same as MART.Dash_original
  #   if(nrow(MART.Dash) != nrow(MART.Dash_original)){
  #     dat <- dat %>% mutate(Category = paste0(rlang::as_name(Var.Number), ": Filtered"))
  #     dat2 <- dat2 %>% mutate(Category = paste0(rlang::as_name(Var.Number), ": All"))
  #     dat <- bind_rows(dat, dat2) 
  #     dat <- dat %>% 
  #       select(Category, everything())
  #   }
  #   
  #   dat
  # })
  # 
  
  table10_data <- reactive({
    
    # x-axis label
    Var.Number <- sym(input$Tab10Variable1)
    
    # Time Variable
    TimeVar <- sym(input$TimeVariable)
    
    # Convert MART.Dash to a survey design object
    survey_data <- as_survey_design(MART.Dash, weights = Weight.LCFS)
    
    # Weighted stats for MART.Dash
    dat <- survey_data %>% 
      filter({{Var.Number}} > 0) %>% 
      group_by({{TimeVar}}) %>% 
      summarise(
        Mean = survey_mean({{Var.Number}}, na.rm=TRUE, trim = 0.2),
        Median = survey_median({{Var.Number}}),
        n()
      ) %>% 
      mutate_if(is.numeric,round,2)
    
    # Convert MART.Dash_original to a survey design object
    survey_data_original <- as_survey_design(MART.Dash_original, weights = Weight.LCFS)
    
    # Weighted stats for MART.Dash_original
    dat2 <- survey_data_original %>% 
      filter({{Var.Number}} > 0) %>% 
      group_by({{TimeVar}}) %>% 
      summarise(
        Mean = survey_mean({{Var.Number}}, na.rm=TRUE, trim = 0.2),
        Median = survey_median({{Var.Number}}),
        n()
      ) %>% 
      mutate_if(is.numeric,round,2)
    
    # Check if the number of rows for MART.Dash is the same as MART.Dash_original
    if(nrow(MART.Dash) != nrow(MART.Dash_original)){
      dat <- dat %>% mutate(Category = paste0(rlang::as_name(Var.Number), ": Filtered"))
      dat2 <- dat2 %>% mutate(Category = paste0(rlang::as_name(Var.Number), ": All"))
      dat <- bind_rows(dat, dat2) 
      dat <- dat %>% 
        select(Category, everything())
    }
    
    dat
  })
  
  # plot 10
  table10_plot <- reactive({
    
    # Extended colour palette
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", 
                     "#005743", "#c2bdde","#9a1d4e","#d4e5ef","#d9bf77","#66b2b2",
                     "#8c4356","#f7ea39","#394240","#f26c4f","#3b3e42","#d27d67","#6b9362","#f9cb9c")
    
    # Time variable
    TimeVar <- sym(input$TimeVariable)
    
    # Determine the x-axis breaks based on TimeVar
    x_breaks <- if(grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table10_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    # If row counts of the dataframes differ, make a grouped line chart
    if(nrow(MART.Dash) != nrow(MART.Dash_original)) {
      plot <- ggplot(table10_data(), aes(x= {{TimeVar}}, y=Median, group = Category, color = Category)) +
        geom_line(size = 0.75) + geom_point() +
        scale_colour_manual(values= Cab.Colours[1:2])
    } 
    # If row counts of the dataframes are the same, make the original line chart
    else {
      plot <- ggplot(table10_data(), aes(x= {{TimeVar}}, y=Median)) +
        geom_line(group = 1, size = 0.75, colour = "#004b88") + geom_point() +
        scale_colour_manual(values= Cab.Colours)
    }
    
    # Common plot aesthetics
    plot <- plot + theme_classic() +
      theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"), 
            strip.background = element_rect(color="#004b88", 
                                            fill="#fcbb69", size=1, linetype="solid"), 
            strip.text = element_text(colour = "#004b88"), 
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"), 
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"), 
            legend.background = element_rect(colour = "#004b88", fill = "white"), 
            legend.text = element_text(colour = "#004b88"), 
            legend.title = element_text(colour = "#004b88")) +
      ylab(input$Tab10Variable1) 

    # Apply x-axis breaks if TimeVar contains "Quarter"
    if(!is.null(x_breaks)) {
      plot <- plot + scale_x_discrete(breaks = x_breaks)
    }
    
    
    plot
  })
  
  
  
  # Return tab 10 data 
  output$table10 <- DT::renderDataTable({
    
    dt <- DT::datatable(table10_data(),options = list(scrollY = "200px"),
                        class = "cell-border")
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      'n()',
      backgroundColor = styleInterval(
        c(100, 300),
        c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
      )
    )
    
    dt
  })
  

  # Return plot10 as Plotly
  output$plot10 <- renderPlotly({
    
    # Extended colour palette
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", 
                     "#005743", "#c2bdde","#9a1d4e","#d4e5ef","#d9bf77","#66b2b2",
                     "#8c4356","#f7ea39","#394240","#f26c4f","#3b3e42","#d27d67","#6b9362","#f9cb9c")
    
    # Time variable
    TimeVar <- sym(input$TimeVariable)
    
    # Determine the x-axis breaks based on TimeVar
    x_breaks <- if(grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table10_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    # If row counts of the dataframes differ, make a grouped line chart
    if(nrow(MART.Dash) != nrow(MART.Dash_original)) {
      p <- ggplot(table10_data(), aes(x= {{TimeVar}}, y=Median, group = Category, color = Category)) +
        geom_line(size = 1) + geom_point(size = 3) +
        scale_colour_manual(values= Cab.Colours[1:2])
    } 
    # If row counts of the dataframes are the same, make the original line chart
    else {
      p <- ggplot(table10_data(), aes(x= {{TimeVar}}, y=Median, group= 1)) +
        geom_line(size = 1, colour = "#004b88") + geom_point(size = 3) +
        scale_colour_manual(values= Cab.Colours)
    }
    
    # Common plot aesthetics
    p <- p + theme_bw() +
      theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"),
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"),
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"),
            legend.background = element_blank(),  # This line removes the legend background
            legend.key = element_blank(),         # This line removes the box around the legend
            legend.text = element_text(colour = "#004b88", size = 8)) +
      ylab(input$Tab10Variable1) +
      # scale_x_discrete(breaks=c("2019-1","2020-1","2021-1", "2022-1", "2023-1"),
      #                  labels=c("2019.Q1","2020.Q1","2021.Q1", "2022.Q1", "2023.Q1")) +
      labs(colour = NULL)
      #scale_y_continuous(limits = c(min(table10_data()$Mean) * 0.9, max(table10_data()$Mean) * 1.1))
    
    if(!is.null(x_breaks)) {
      p <- p + scale_x_discrete(breaks = x_breaks)
    }
    
    ggplotly(p, tooltip = c("x", "y"))  
    
  })
  
  # Download tab 10 data 
  output$download10 <- downloadHandler(
    filename = function() {
      paste0(input$Tab10Variable1, ".csv")
    },
    content = function(file) {
      write.csv(table10_data(),file)
    }
  )
  
  # Download tab 10 plot 
  output$download10b <- downloadHandler(
    filename = function() {
      paste0(input$Tab10Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table10_plot(), device = device, height = 5.5)
    }
  )

  
  # table11_data <- reactive({
  #   
  #   # x-axis label
  #   Var.Group <- sym(input$Tab11Variable1)
  #   Var.Number <- sym(input$Tab11Variable2)
  #   
  #   # Time variable
  #   TimeVar <- sym(input$TimeVariable2)
  #   
  #   # data for plot
  #   dat <- MART.Dash %>% 
  #     dplyr::filter({{Var.Number}} > 0) %>% 
  #     dplyr::group_by({{TimeVar}}, {{Var.Group}}) %>% 
  #     dplyr::summarise(Mean = mean({{Var.Number}}, na.rm=TRUE, trim = 0.2),
  #                      Median = median({{Var.Number}}),
  #                      n()) %>% 
  #     mutate_if(is.numeric,round,2) %>%
  #     filter(!is.na({{Var.Group}}), !grepl("NA|Other", as.character({{Var.Group}}), ignore.case = TRUE)) 
  #   
  #   # overall statistics (group not applied)
  #   overall <- MART.Dash %>% 
  #     dplyr::filter({{Var.Number}} > 0) %>% 
  #     dplyr::group_by({{TimeVar}}) %>% 
  #     dplyr::summarise(Mean = mean({{Var.Number}}, na.rm=TRUE, trim = 0.2),
  #                      Median = median({{Var.Number}}),
  #                      n()) %>% 
  #     mutate_if(is.numeric,round,2)
  #   
  #   # Convert Var.Group to a string and add a new column to overall
  #   Var.Group.String <- rlang::as_string(Var.Group)
  #   overall <- overall %>%
  #     dplyr::mutate(!!Var.Group.String := "All")
  #   
  #   # combine the grouped data and the overall data
  #   combined_dat <- rbind(dat, overall)
  #   
  #   combined_dat
  # })
  # 
  
  table11_data <- reactive({
    
    # x-axis label
    Var.Group <- sym(input$Tab11Variable1)
    Var.Number <- sym(input$Tab11Variable2)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable2)
    
    # Convert MART.Dash to a survey design object
    survey_data <- as_survey_design(MART.Dash, weights = Weight.LCFS)
    
    # Weighted stats for group
    dat <- survey_data %>% 
      filter({{Var.Number}} > 0) %>% 
      group_by({{TimeVar}}, {{Var.Group}}) %>% 
      summarise(
        Mean = survey_mean({{Var.Number}}, na.rm=TRUE, trim = 0.2),
        Median = survey_median({{Var.Number}}),
        n()
      ) %>% 
      mutate_if(is.numeric,round,2) %>%
      filter(!is.na({{Var.Group}}), !grepl("NA|Other", as.character({{Var.Group}}), ignore.case = TRUE)) 
    
    # Weighted stats for overall
    overall <- survey_data %>% 
      filter({{Var.Number}} > 0) %>% 
      group_by({{TimeVar}}) %>% 
      summarise(
        Mean = survey_mean({{Var.Number}}, na.rm=TRUE, trim = 0.2),
        Median = survey_median({{Var.Number}}),
        n()
      ) %>% 
      mutate_if(is.numeric,round,2)
    
    # Convert Var.Group to a string and add a new column to overall
    Var.Group.String <- rlang::as_string(Var.Group)
    overall <- overall %>%
      dplyr::mutate(!!Var.Group.String := "All")
    
    # combine the grouped data and the overall data
    combined_dat <- rbind(dat, overall)
    
    combined_dat
  })
  
  
  # plot 11 input
  table11_plot <- reactive({
    # Extended colour palette
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", 
                     "#005743", "#c2bdde","#9a1d4e","#d4e5ef","#d9bf77","#66b2b2",
                     "#8c4356","#f7ea39","#394240","#f26c4f","#3b3e42","#d27d67","#6b9362",
                     "#f9cb9c","#1e5090","#e58d7d","#2f615e","#c49bcd","#e2cf56",
                     "#6f83a5","#cf6a88","#55805b","#e9a1cf","#768d64")
    Cab.Colours_short <- Cab.Colours[0:2]
    
    Var.Group <- sym(input$Tab11Variable1)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable2)
    
    # Determine the x-axis breaks based on TimeVar
    x_breaks <- if(grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table11_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    plot <- ggplot(table11_data(), aes(x= {{TimeVar}}, y=Median, group={{Var.Group}}, colour = {{Var.Group}})) +
      geom_line(size = 0.75) + geom_point() +theme_classic() +
      theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"), 
            strip.background = element_rect(color="#004b88", 
                                            fill="#fcbb69", size=1, linetype="solid"), 
            strip.text = element_text(colour = "#004b88"), 
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"), 
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"), 
            legend.background = element_rect(colour = "#004b88", fill = "white"), 
            legend.text = element_text(colour = "#004b88"), 
            legend.title = element_text(colour = "#004b88")) +
      scale_colour_manual(values= Cab.Colours) + ylab(input$Tab11Variable1) 

    # Apply x-axis breaks if TimeVar contains "Quarter"
    if(!is.null(x_breaks)) {
      plot <- plot + scale_x_discrete(breaks = x_breaks)
    }
    
    plot
  
  })
  
  # Return tab 11 data 
  output$table11 <- DT::renderDataTable({
    
    dt <- DT::datatable(table11_data(),options = list(scrollY = "200px"),
                        class = "cell-border")
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      'n()',
      backgroundColor = styleInterval(
        c(100, 300),
        c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
      )
    )
    
    dt
  })
  

  # Return plot11
  output$plot11 <- renderPlotly({
    
    # Extended colour palette
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", 
                     "#005743", "#c2bdde","#9a1d4e","#d4e5ef","#d9bf77","#66b2b2",
                     "#8c4356","#f7ea39","#394240","#f26c4f","#3b3e42","#d27d67","#6b9362",
                     "#f9cb9c","#1e5090","#e58d7d","#2f615e","#c49bcd","#e2cf56",
                     "#6f83a5","#cf6a88","#55805b","#e9a1cf","#768d64")
    Cab.Colours_short <- Cab.Colours[0:2]
    
    Var.Group <- sym(input$Tab11Variable1)
    
    TimeVar <- sym(input$TimeVariable2)
    
    # Determine the x-axis breaks based on TimeVar
    x_breaks <- if(grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table11_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    p <- ggplot(table11_data(), aes(x= {{TimeVar}}, y=Median, group={{Var.Group}}, colour = {{Var.Group}})) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      theme_bw() +
      theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"),
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"),
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"),
            legend.background = element_blank(),  # This line removes the legend background
            legend.key = element_blank(),         # This line removes the box around the legend
            legend.text = element_text(colour = "#004b88", size = 8)) +
      scale_colour_manual(values= Cab.Colours) + ylab(input$Tab11Variable1) +
      labs(colour = NULL)  # This line removes the legend title
    
    # Apply x-axis breaks if TimeVar contains "Quarter"
    if(!is.null(x_breaks)) {
      p <- p + scale_x_discrete(breaks = x_breaks)
    }
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
    
  })
  
  
 
  # Download tab 11 data 
  output$download11 <- downloadHandler(
    filename = function() {
      paste0(input$Tab11Variable1, ".csv")
    },
    content = function(file) {
      write.csv(table11_data(),file)
    }
  )
  
  # Download tab 11 plot 
  output$download11b <- downloadHandler(
    filename = function() {
      paste0(input$Tab11Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table11_plot(), device = device, height = 5.5)
    }
  )
  
  ##########################################################
  # tab 20 data
  table20_data <- reactive({
    
    # x-axis label
    Var.Group <- sym(input$Tab20Variable1)
    
    # Time variable
    TimeVar <- sym(input$TimeVariableNeg)
    
    # Convert MART.Dash to a survey design object
    survey_data <- as_survey_design(MART.Dash, weights = Weight.LCFS) 
    
    # Sample n and Weighted proportions for both variables
    weighted_data <- survey_data %>%
      group_by(!!TimeVar, !!Var.Group, Demographic.Negbud) %>%
      summarise(
        n = n(),
        WeightedPercent = survey_mean(na.rm = TRUE, vartype = "ci")*100
      ) %>%
      spread(Demographic.Negbud, WeightedPercent) %>%
      mutate(across(where(is.numeric), ~round(. ,2))) %>% 
      select(1:3,6)%>%
      filter(row_number() %% 2 == 1) 
    
    
  
    weighted_data

  })
  
  
  # tab 20 plot input
  table20_plot <- reactive({
    # Extended colour palette
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", 
                     "#005743", "#c2bdde","#9a1d4e","#d4e5ef","#d9bf77","#66b2b2",
                     "#8c4356","#f7ea39","#394240","#f26c4f","#3b3e42","#d27d67","#6b9362",
                     "#f9cb9c","#1e5090","#e58d7d","#2f615e","#c49bcd","#e2cf56",
                     "#6f83a5","#cf6a88","#55805b","#e9a1cf","#768d64")
    Cab.Colours_short <- Cab.Colours[0:2]
    
    Var.Group <- sym(input$Tab20Variable1)
    
    # Time variable
    TimeVar <- sym(input$TimeVariableNeg)
    
    # Determine the x-axis breaks based on TimeVar
    x_breaks <- if(grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table20_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    plot <- ggplot(table20_data(), aes(x= {{TimeVar}}, y=Negative.Budget, group={{Var.Group}}, colour = {{Var.Group}})) +
      geom_line(size = 0.75) + geom_point() +theme_classic() +
      theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"), 
            strip.background = element_rect(color="#004b88", 
                                            fill="#fcbb69", size=1, linetype="solid"), 
            strip.text = element_text(colour = "#004b88"), 
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"), 
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"), 
            legend.background = element_rect(colour = "#004b88", fill = "white"), 
            legend.text = element_text(colour = "#004b88"), 
            legend.title = element_text(colour = "#004b88")) +
      scale_colour_manual(values= Cab.Colours) + ylab(input$Tab11Variable1) 
    
    # Apply x-axis breaks if TimeVar contains "Quarter"
    if(!is.null(x_breaks)) {
      plot <- plot + scale_x_discrete(breaks = x_breaks)
    }
    
    plot
    
  })
    
  # tab 20 plot output
  output$plot20 <- renderPlotly({
    
    # Extended colour palette
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", 
                     "#005743", "#c2bdde","#9a1d4e","#d4e5ef","#d9bf77","#66b2b2",
                     "#8c4356","#f7ea39","#394240","#f26c4f","#3b3e42","#d27d67","#6b9362",
                     "#f9cb9c","#1e5090","#e58d7d","#2f615e","#c49bcd","#e2cf56",
                     "#6f83a5","#cf6a88","#55805b","#e9a1cf","#768d64")
    Cab.Colours_short <- Cab.Colours[0:2]
    
    Var.Group <- sym(input$Tab20Variable1)
    
    TimeVar <- sym(input$TimeVariableNeg)
    
    # Determine the x-axis breaks based on TimeVar
    x_breaks <- if(grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table20_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    p <- ggplot(table20_data(), aes(x= {{TimeVar}}, y=Negative.Budget, group={{Var.Group}}, colour = {{Var.Group}})) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      theme_bw() +
      theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"),
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"),
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"),
            legend.background = element_blank(),  # This line removes the legend background
            legend.key = element_blank(),         # This line removes the box around the legend
            legend.text = element_text(colour = "#004b88", size = 8)) +
      scale_colour_manual(values= Cab.Colours) + ylab(input$Tab11Variable1) +
      labs(colour = NULL)  # This line removes the legend title
    
    # Apply x-axis breaks if TimeVar contains "Quarter"
    if(!is.null(x_breaks)) {
      p <- p + scale_x_discrete(breaks = x_breaks)
    }
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
    
  })
  # tab 20 data output
  output$table20 <- DT::renderDataTable({
    
    dt <- DT::datatable(table20_data(),options = list(scrollY = "200px"),
                        class = "cell-border")
    
    # Apply conditional formatting to the column "n"
    dt <- dt %>% formatStyle(
      'n',
      backgroundColor = styleInterval(
        c(100, 300),
        c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
      )
    )
    
    dt
  })
  # download tab 20 data
  output$download20 <- downloadHandler(
    filename = function() {
      paste0(input$Tab20Variable1, ".csv")
    },
    content = function(file) {
      write.csv(table20_data(),file)
    }
  )
  # download tab 20 plot
  output$download20b <- downloadHandler(
    filename = function() {
      paste0(input$Tab20Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table20_plot(), device = device, height = 5.5)
    }
  )
  #########################################################
  
  table12_data <- reactive({
    
    Var.Group <- sym(input$Tab12Variable1)
    Var.Number <- sym(input$Tab12Variable2)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable3)
    
    # Convert MART.Dash to a survey design object
    survey_data <- as_survey_design(MART.Dash, weights = Weight.LCFS)
    
    # Weighted stats
    dat.plot <- survey_data %>% 
      filter({{Var.Number}} > 0) %>% 
      group_by({{TimeVar}}, {{Var.Group}}) %>% 
      summarise(
        Mean = survey_mean({{Var.Number}}, na.rm=TRUE, trim = 0.2),
        Median = survey_median({{Var.Number}}),
        n()
      ) %>% 
      filter({{TimeVar}} == levels({{TimeVar}})[length(levels({{TimeVar}}))]) %>% 
      filter(!is.na({{Var.Group}}), !grepl("NA|Other", as.character({{Var.Group}}), ignore.case = TRUE)) %>%  # This line filters NA and Other values
      mutate_if(is.numeric,round,2) 
    dat.plot
    
  })
  
  
  # plot 12 input
  table12_plot <- reactive({
    Var.Group <- sym(input$Tab12Variable1)
    Var.Number <- sym(input$Tab12Variable2)
    
    # Plot
    ggplot(table12_data(), aes(x={{Var.Group}}, y=Median, fill = {{Var.Group}})) +
      geom_bar(stat = "identity", color = "#004b88", size = 0.75) + theme_classic() +
      theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"), 
            strip.background = element_rect(color="#004b88", 
                                            fill="#fcbb69", size=1, linetype="solid"), 
            strip.text = element_text(colour = "#004b88"), 
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"), 
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"), 
            legend.background = element_rect(colour = "#004b88", fill = "white"), 
            legend.text = element_text(colour = "#004b88"), 
            legend.title = element_text(colour = "#004b88")) +
      #scale_fill_manual(values= Cab.Colours.Modified) + 
      ylab(input$Tab12Variable2) 
    

    
  })
  
  
  # Return tab 12 data 
  output$table12 <- DT::renderDataTable({
    
    dt <- DT::datatable(table12_data(),options = list(scrollY = "200px"),
                        class = "cell-border")
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      'n()',
      backgroundColor = styleInterval(
        c(100, 300),
        c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
      )
    )
    
    dt
  })
  
  
  
  # Return plot12
  output$plot12 <- renderPlotly({

    Var.Group <- sym(input$Tab12Variable1)
    Var.Number <- sym(input$Tab12Variable2)

    # Plot
    p <- ggplot(table12_data()) +
      geom_bar(aes(x={{Var.Group}}, y=Median, fill = {{Var.Group}}), stat = "identity", color = "#004b88", size = 0.75, show.legend = FALSE) +
      theme_bw() +
      theme(text = element_text(size = 15), axis.text.y= element_text(color = "#004b88"),
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"),
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"),
            axis.text.x = element_text(colour = "#004b88", size = 6),
            legend.text = element_text(colour = "#004b88", size = 8),
            legend.title = element_blank()) +
      ylab(input$Tab12Variable2)


    
    # Check condition and remove x axis labels if true
    if (input$Tab12Variable1 == "Demographic.Housing.Tenure.LCFS"|
        input$Tab12Variable1 == "Demographic.Region.LCFS"|
        input$Tab12Variable1 == "Demographic.Household.Type.LCFS"|
        input$Tab12Variable1 == "Demographic.Derived.Household.Type.LCFS.child"|
        input$Tab12Variable1 == "Demographic.Derived.Household.Type.LCFS") {
      p <- p + theme(axis.text.x = element_blank())
    } else {
      p <- p + theme(axis.text.x = element_text(color = "#004b88"))
    }

    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  
  # Download tab 12 data 
  output$download12 <- downloadHandler(
    filename = function() {
      paste0(input$Tab12Variable1, ".csv")
    },
    content = function(file) {
      write.csv(table12_data(),file)
    }
  )
  
  # Download tab 12 plot 
  output$download12b <- downloadHandler(
    filename = function() {
      paste0(input$Tab12Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table12_plot(), device = device, height = 5.5)
    }
  )
  
  # # data for plot 13
  # table13_data <- reactive({
  #   
  #   Var.Group1 <- sym(input$Tab13Variable1)
  #   Var.Group2 <- sym(input$Tab13Variable2)
  #   Var.Number <- sym(input$Tab13Variable3)
  #   
  #   # Time variable
  #   TimeVar <- sym(input$TimeVariable4)
  #   
  #   dat.plot <- MART.Dash %>% 
  #     dplyr::filter({{Var.Number}} > 0) %>% 
  #     dplyr::group_by({{TimeVar}}, {{Var.Group1}},{{Var.Group2}}) %>% 
  #     dplyr::summarise(Mean = mean({{Var.Number}}, na.rm=TRUE, trim = 0.2),
  #                      Median = median({{Var.Number}}),
  #                      n()) %>% 
  #     filter({{TimeVar}} == levels({{TimeVar}})[length(levels({{TimeVar}}))]) %>%
  #     filter(!grepl("Other|NA", as.character({{Var.Group1}}), ignore.case = TRUE), !is.na({{Var.Group1}})) %>% 
  #     filter(!grepl("Other|NA", as.character({{Var.Group2}}), ignore.case = TRUE), !is.na({{Var.Group2}})) %>% 
  #     mutate_if(is.numeric,round,2)
  #   dat.plot
  # })
  # 
  
  # data for plot 13
  table13_data <- reactive({
    
    Var.Group1 <- sym(input$Tab13Variable1)
    Var.Group2 <- sym(input$Tab13Variable2)
    Var.Number <- sym(input$Tab13Variable3)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable4)
    
    # Convert MART.Dash to a survey design object
    survey_data <- as_survey_design(MART.Dash, weights = Weight.LCFS)
    
    # Weighted stats
    dat.plot <- survey_data %>% 
      filter({{Var.Number}} > 0) %>% 
      group_by({{TimeVar}}, {{Var.Group1}}, {{Var.Group2}}) %>% 
      summarise(
        Mean = survey_mean({{Var.Number}}, na.rm=TRUE, trim = 0.2),
        Median = survey_median({{Var.Number}}),
        n()
      ) %>% 
      filter({{TimeVar}} == levels({{TimeVar}})[length(levels({{TimeVar}}))]) %>% 
      filter(!grepl("Other|NA", as.character({{Var.Group1}}), ignore.case = TRUE), !is.na({{Var.Group1}})) %>% 
      filter(!grepl("Other|NA", as.character({{Var.Group2}}), ignore.case = TRUE), !is.na({{Var.Group2}})) %>% 
      mutate_if(is.numeric, round, 2)
    
    dat.plot
  })
  
  
  
  
  # plot 13 input
  table13_plot <- reactive({
    
    Var.Group1 <- sym(input$Tab13Variable1)
    Var.Group2 <- sym(input$Tab13Variable2)
    Var.Number <- sym(input$Tab13Variable3)
    
    # Plot
    ggplot(table13_data(), aes(x={{Var.Group1}}, y=Median, fill = {{Var.Group2}})) +
      geom_bar(stat = "identity", color = "#004b88", size = 0.75) + theme_classic() +
      theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"), 
            strip.background = element_rect(color="#004b88", 
                                            fill="#fcbb69", size=1, linetype="solid"), 
            strip.text = element_text(colour = "#004b88"), 
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"), 
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"), 
            legend.background = element_rect(colour = "#004b88", fill = "white"), 
            legend.text = element_text(colour = "#004b88"), 
            legend.title = element_text(colour = "#004b88")) +
      #scale_fill_manual(values= Cab.Colours.Modified) + 
      ylab(input$Tab13Variable3) 
    
  })
  
  
  # Return tab 13 data 
  output$table13 <- DT::renderDataTable({
    
    dt <- DT::datatable(table13_data(),options = list(scrollY = "200px"),
                        class = "cell-border")
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      'n()',
      backgroundColor = styleInterval(
        c(100, 300),
        c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
      )
    )
    
    dt
  })
  
  
  # plot 13 output
  output$plot13 <- renderPlotly({
    
    Var.Group1 <- sym(input$Tab13Variable1)
    Var.Group2 <- sym(input$Tab13Variable2)
    Var.Number <- sym(input$Tab13Variable3)
    
    # Plot
    p <- ggplot(table13_data(), aes(x={{Var.Group1}}, y=Median, fill = {{Var.Group2}})) +
      geom_bar(stat = "identity", color = "#004b88", size = 0.75, show.legend = FALSE) + 
      theme_bw() +
      theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88", size = 6),
            axis.text.y= element_text(color = "#004b88"), 
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"), 
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"),
            legend.text = element_text(colour = "#004b88", size = 8),
            legend.title = element_blank()) +
      ylab(input$Tab13Variable3)
    
    # Check condition and remove x axis labels if true
    if (input$Tab13Variable1 == "Demographic.Housing.Tenure.LCFS"|
        input$Tab13Variable1 == "Demographic.Region.LCFS"|
        input$Tab13Variable1 == "Demographic.Household.Type.LCFS"|
        input$Tab13Variable1 == "Demographic.Derived.Household.Type.LCFS.child"|
        input$Tab13Variable1 == "Demographic.Derived.Household.Type.LCFS") {
      p <- p + theme(axis.text.x = element_blank())
    } else {
      p <- p + theme(axis.text.x = element_text(color = "#004b88"))
    }
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
    
    
  })
  
  
  # Download tab 13 data 
  output$download13 <- downloadHandler(
    filename = function() {
      paste0(input$Tab13Variable3, ".csv")
    },
    content = function(file) {
      write.csv(table13_data(),file)
    }
  )
  
  # Download tab 13 plot 
  output$download13b <- downloadHandler(
    filename = function() {
      paste0(input$Tab13Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table13_plot(), device = device, height = 5.5)
    }
  )
  
  # # data for plot 14
  # table14_data <- reactive({
  #   
  #   Var.Group1 <- sym(input$Tab14Variable1)
  #   Var.Group2 <- sym(input$Tab14Variable2)
  #   Var.Number <- sym(input$Tab14Variable3)
  #   
  #   # Time variable
  #   TimeVar <- sym(input$TimeVariable5)
  #   
  #   dat.plot <- MART.Dash %>%
  #     dplyr::filter({{Var.Number}} > 0) %>%
  #     dplyr::group_by({{TimeVar}}, {{Var.Group1}},{{Var.Group2}}) %>%
  #     dplyr::summarise(Mean = mean({{Var.Number}}, na.rm=TRUE, trim = 0.2),
  #                      Median = median({{Var.Number}}),
  #                      n()) %>%
  #     mutate_if(is.numeric,round,2) %>%
  #     filter({{TimeVar}} == levels({{TimeVar}})[length(levels({{TimeVar}}))]) %>%
  #     filter(!grepl("Other|NA", as.character({{Var.Group1}}), ignore.case = TRUE), !is.na({{Var.Group1}})) %>%
  #     filter(!grepl("Other|NA", as.character({{Var.Group2}}), ignore.case = TRUE), !is.na({{Var.Group2}}))
  #   
  #   dat.plot
  # })
  # 
  
  # data for plot 14
  table14_data <- reactive({
    
    Var.Group1 <- sym(input$Tab14Variable1)
    Var.Group2 <- sym(input$Tab14Variable2)
    Var.Number <- sym(input$Tab14Variable3)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable5)
    
    # Convert MART.Dash to a survey design object
    survey_data <- as_survey_design(MART.Dash, weights = Weight.LCFS)
    
    # Compute the weighted statistics
    dat.plot <- survey_data %>% 
      filter({{Var.Number}} > 0) %>% 
      group_by({{TimeVar}}, {{Var.Group1}}, {{Var.Group2}}) %>% 
      summarise(
        Mean = survey_mean({{Var.Number}}, na.rm=TRUE, trim = 0.2),
        Median = survey_median({{Var.Number}}),
        n()
      ) %>% 
      mutate_if(is.numeric, round, 2) %>% 
      filter({{TimeVar}} == levels({{TimeVar}})[length(levels({{TimeVar}}))]) %>% 
      filter(!grepl("Other|NA", as.character({{Var.Group1}}), ignore.case = TRUE), !is.na({{Var.Group1}})) %>% 
      filter(!grepl("Other|NA", as.character({{Var.Group2}}), ignore.case = TRUE), !is.na({{Var.Group2}}))
    
    dat.plot
  })
  
  
  
  table14_plot <- reactive({
    
    Var.Group1 <- sym(input$Tab14Variable1)
    Var.Group2 <- sym(input$Tab14Variable2)
    Var.Number <- sym(input$Tab14Variable3)
  
    
    # Plot
    ggplot(table14_data(), aes(x={{Var.Group1}}, y=Median, fill = {{Var.Group2}})) +
      geom_bar(stat = "identity", color = "#004b88", size = 0.75, position = "dodge") + theme_classic() +
      theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"), 
            strip.background = element_rect(color="#004b88", 
                                            fill="#fcbb69", size=1, linetype="solid"), 
            strip.text = element_text(colour = "#004b88"), 
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"), 
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"), 
            legend.background = element_rect(colour = "#004b88", fill = "white"), 
            legend.text = element_text(colour = "#004b88"), 
            legend.title = element_text(colour = "#004b88")) +
      #scale_fill_manual(values= Cab.Colours.Modified) + 
      ylab(input$Tab14Variable3) 
    
  })
  
  
  # return tab 14 data 
  output$table14 <- DT::renderDataTable({
    
    dt <- DT::datatable(table14_data(), options = list(scrollY = "200px"),
                        class = "cell-border")
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      'n()',
      backgroundColor = styleInterval(
        c(100, 300),
        c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
      )
    )
    
    dt
  })
  
  
  # Return plot14
  output$plot14 <- renderPlotly({
    
    Var.Group1 <- sym(input$Tab14Variable1)
    Var.Group2 <- sym(input$Tab14Variable2)
    Var.Number <- sym(input$Tab14Variable3)
    
    # Plot
    p <- ggplot(table14_data(), aes(x={{Var.Group1}}, y=Median, fill = {{Var.Group2}})) +
      geom_bar(stat = "identity", color = "#004b88", size = 0.75, position = "dodge", show.legend = FALSE) +
      theme_bw() +
      theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88", size =6),
            axis.text.y= element_text(color = "#004b88"), 
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"), 
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"),
            legend.text = element_text(colour = "#004b88", size = 8),
            legend.title = element_blank()) +
      ylab(input$Tab14Variable3)
    
    # Check condition and remove x axis labels if true
    if (input$Tab14Variable1 == "Demographic.Housing.Tenure.LCFS"|
        input$Tab14Variable1 == "Demographic.Region.LCFS"|
        input$Tab14Variable1 == "Demographic.Household.Type.LCFS"|
        input$Tab14Variable1 == "Demographic.Derived.Household.Type.LCFS.child"|
        input$Tab14Variable1 == "Demographic.Derived.Household.Type.LCFS") {
      p <- p + theme(axis.text.x = element_blank())
    } else {
      p <- p + theme(axis.text.x = element_text(color = "#004b88"))
    }
    
    ggplotly(p, tooltip = c("x", "y", "fill"), showlegend = FALSE)
  })
  
  
  # Download tab 14 data 
  output$download14 <- downloadHandler(
    filename = function() {
      paste0(input$Tab14Variable3, ".csv")
    },
    content = function(file) {
      write.csv(table14_data(),file)
    }
  )
  
  # Download tab 14 plot 
  output$download14b <- downloadHandler(
    filename = function() {
      paste0(input$Tab14Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table14_plot(), device = device, height = 5.5)
    }
  )
  
  # # data for plot 15
  # table15_data <- reactive({
  #   
  #   Var.Group <- input$Tab15Variable1
  #   Var.Number <- input$Tab15Variable2
  #   TimeVar <- sym(input$TimeVariable6)
  #   
  #   dat.plot <- MART.Dash %>%
  #     dplyr::filter((!!sym(Var.Number)) > 0)
  #   
  #   # If TimeVar is "Year", transform it to a specific format (e.g. append quarter information)
  #   if (as.character(TimeVar) == "Year") {
  #     dat.plot <- dat.plot %>%
  #       mutate(!!TimeVar := as.integer(Year) + 2018)
  #   }
  #   
  #   dat.plot <- dat.plot %>%
  #     dplyr::group_by(!!TimeVar, (!!sym(Var.Group))) %>%
  #     dplyr::summarise(Mean = mean((!!sym(Var.Number)), na.rm=TRUE, trim = 0.2),
  #                      Median = median((!!sym(Var.Number))),
  #                      n()) %>%
  #     mutate_if(is.numeric, round, 2) %>%
  #     filter(!grepl("Other|NA", as.character((!!sym(Var.Group))), ignore.case = TRUE), !is.na((!!sym(Var.Group))))
  #   
  #   dat.plot
  # })
  
  # data for plot 15
  table15_data <- reactive({
    
    Var.Group <- sym(input$Tab15Variable1)
    Var.Number <- sym(input$Tab15Variable2)
    TimeVar <- sym(input$TimeVariable6)
    
    # Convert MART.Dash to a survey design object
    survey_data <- as_survey_design(MART.Dash, weights = Weight.LCFS)
    
    dat.plot <- survey_data %>%
      filter((!!Var.Number) > 0)
    
    # # If TimeVar is "Year", transform it to a specific format (e.g. append quarter information)
    # if (as.character(TimeVar) == "Year") {
    #   dat.plot <- dat.plot %>%
    #     mutate(!!TimeVar := as.integer(Year) + 2018)
    # }
    
    if (as.character(TimeVar) == "Financial.Year") {
      dat.plot <- dat.plot %>%
        ungroup() %>%
        mutate(Financial.Year = case_when(
          Financial.Year == "2020-21" ~ 2020,
          Financial.Year == "2021-22" ~ 2021,
          Financial.Year == "2022-23" ~ 2022,
          TRUE ~ NA_real_
        ))
    }
    
    dat.plot <- dat.plot %>%
      group_by(!!TimeVar, !!Var.Group) %>%
      summarise(
        Mean = survey_mean(!!Var.Number, na.rm=TRUE, trim = 0.2),
        Median = survey_median(!!Var.Number),
        n()
      ) %>% 
      mutate_if(is.numeric, round, 2) %>%
      filter(!grepl("Other|NA", as.character(!!Var.Group), ignore.case = TRUE), !is.na(!!Var.Group))
    
    dat.plot
  })
  
  
  # plot 15 input
  table15_plot <- reactive({
    
    Var.Group <- sym(input$Tab15Variable1)
    Var.Number <- sym(input$Tab15Variable2)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable6) 
    
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", 
                     "#005743", "#c2bdde","#9a1d4e","#d4e5ef","#d9bf77","#66b2b2",
                     "#8c4356","#f7ea39","#394240","#f26c4f","#3b3e42","#d27d67","#6b9362",
                     "#f9cb9c","#1e5090","#e58d7d","#2f615e","#c49bcd","#e2cf56",
                     "#6f83a5","#cf6a88","#55805b","#e9a1cf","#768d64")
    
    # modify colour palette to contain as many items as levels of inputted factor variable
    Cab.Colours.Modified <- Cab.Colours[1:nlevels(table15_data()[[Var.Group]])]
    if("Other/prefer not to say/unknown" %in% levels(table15_data()[[Var.Group]])){Cab.Colours.Modified <- head(Cab.Colours.Modified,-1)}
    print(Cab.Colours.Modified)
    
    
    # Plot
    ggplot(table15_data(), aes(x={{TimeVar}}, y=Median, fill = {{Var.Group}})) +
      geom_area(colour = "#004b88", size = 1, show.legend = FALSE) + theme_classic() +
      theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"), 
            strip.background = element_rect(color="#004b88", 
                                            fill="#fcbb69", size=1, linetype="solid"), 
            strip.text = element_text(colour = "#004b88"), 
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"), 
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"), 
            legend.background = element_rect(colour = "#004b88", fill = "white"), 
            legend.text = element_text(colour = "#004b88"), 
            legend.title = element_text(colour = "#004b88")) +
      scale_fill_manual(values= Cab.Colours) + ylab(input$Tab15Variable2) 
    
    
  })
  
  
  # return tab 15 data 
  output$table15 <- DT::renderDataTable({
    
    dt <- DT::datatable(table15_data(), options = list(scrollY = "200px"),
                        class = "cell-border")
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      'n()',
      backgroundColor = styleInterval(
        c(100, 300),
        c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
      )
    )
    
    dt
  })
  
  
  # Return plot15
  output$plot15 <- renderPlotly({
    
    Var.Group <- sym(input$Tab15Variable1)
    Var.Number <- sym(input$Tab15Variable2)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable6) 
    
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", 
                     "#005743", "#c2bdde","#9a1d4e","#d4e5ef","#d9bf77","#66b2b2",
                     "#8c4356","#f7ea39","#394240","#f26c4f","#3b3e42","#d27d67","#6b9362",
                     "#f9cb9c","#1e5090","#e58d7d","#2f615e","#c49bcd","#e2cf56",
                     "#6f83a5","#cf6a88","#55805b","#e9a1cf","#768d64")
    
    # Plot
    p <- ggplot(table15_data(), aes(x={{TimeVar}}, y=Median, fill = {{Var.Group}})) +
      geom_area(colour = "#004b88", size = 1, show.legend = FALSE) + theme_classic() +
      theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"), 
            strip.background = element_rect(color="#004b88", 
                                            fill="#fcbb69", size=1, linetype="solid"), 
            strip.text = element_text(colour = "#004b88"), 
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"), 
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"), 
            legend.background = element_blank(), 
            legend.text = element_text(colour = "#004b88", size = 8), 
            legend.title = element_blank()) +
      scale_fill_manual(values= Cab.Colours) + ylab(input$Tab15Variable2) +
      labs(fill = NULL)  # This line removes the legend title
    
    if (as.character(TimeVar) == "Financial.Year") {
      p <- p + scale_x_continuous(breaks=c(2020.0, 2021.0, 2022.0), labels=c("2020-21", "2021-22", "2022-23"))
    }
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
    
  })
  
  
  # Download tab 15 data 
  output$download15 <- downloadHandler(
    filename = function() {
      paste0(input$Tab15Variable2, ".csv")
    },
    content = function(file) {
      write.csv(table15_data(),file)
    }
  )
  
  # Download tab 15 plot 
  output$download15b <- downloadHandler(
    filename = function() {
      paste0(input$Tab15Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table15_plot(), device = device, height = 5.5)
    }
  )

  
  # # Table 16 data
  # table16_data <- reactive({
  #   
  #   Var.Group <- sym(input$Tab16Variable1)
  #   TimeVar <- as.character("Year")
  #   
  #   # Convert Year to integer if necessary
  #   dat.plot <- MART.Dash
  #   
  #   # Weighted percentages calculation
  #   survey_design <- as_survey_design(dat.plot, weights = Weight.LCFS)
  #   weighted_percentages <- survey_design %>%
  #     group_by(!!Var.Group) %>%
  #     summarize(WeightedPct = survey_mean(na.rm = TRUE) * 100) %>%
  #     deframe()  # Convert to named vector
  #   
  #   # Data for plot
  #   data <- dat.plot %>%
  #     filter(!grepl("Other|NA", as.character(!!Var.Group), ignore.case = TRUE), !is.na(!!Var.Group)) %>%
  #     group_by(!!sym(TimeVar), !!Var.Group) %>%
  #     summarize(n = n()) %>%
  #     spread(!!Var.Group, n) %>%
  #     ungroup()
  #   
  #   # Format data.plot
  #   data.plot <- data %>%
  #     select(-1) %>%
  #     data.matrix() %>%
  #     prop.table(margin=1) %>%
  #     data.frame() %>%
  #     mutate_if(is.numeric, ~ . * 100) %>%
  #     mutate(Time = data[[TimeVar]], n = rowSums(data[,-1])) %>%
  #     melt(id.vars = c("Time", "n")) %>%
  #     rename(Condition = "variable") %>%
  #     select(Time, Condition, n)
  #   
  #   # Replace the percentages with the weighted percentages using the named vector
  #   data.plot$Percentage <- weighted_percentages[data.plot$Condition]
  #   
  #   data.plot
  # })
  
  # Table 16 data
  table16_data <- reactive({
    
    Var.Group <- sym(input$Tab16Variable1)
    TimeVar <- as.character("Financial.Year")
    
    # Convert Year to integer if necessary
    dat.plot <- MART.Dash
    if (TimeVar == "Financial.Year") {
      dat.plot <- dat.plot %>%
        ungroup() %>%
        mutate(Financial.Year = case_when(
          Financial.Year == "2020-21" ~ 2020,
          Financial.Year == "2021-22" ~ 2021,
          Financial.Year == "2022-23" ~ 2022,
          Financial.Year == "2023-24" ~ 2023,
          TRUE ~ NA_real_
        ))
    }
    
    # Weighted percentages calculation
    survey_design <- as_survey_design(dat.plot, weights = Weight.LCFS)
    weighted_percentages <- survey_design %>%
      group_by(!!Var.Group) %>%
      summarize(WeightedPct = survey_mean(na.rm = TRUE) * 100) %>%
      deframe()  # Convert to named vector
    
    # Data for plot
    data <- dat.plot %>%
      filter(!grepl("Other|NA", as.character(!!Var.Group), ignore.case = TRUE), !is.na(!!Var.Group)) %>%
      group_by(!!sym(TimeVar), !!Var.Group) %>%
      summarize(n = n()) %>%
      spread(!!Var.Group, n) %>%
      ungroup()
    
    # Format data.plot
    data.plot <- data %>%
      select(-1) %>%
      data.matrix() %>%
      prop.table(margin=1) %>%
      data.frame() %>%
      mutate_if(is.numeric, ~ . * 100) %>%
      mutate(Time = data[[TimeVar]], n = rowSums(data[,-1])) %>%
      melt(id.vars = c("Time", "n")) %>%
      rename(Condition = "variable") %>%
      select(Time, Condition, n)
    
    # Replace the percentages with the weighted percentages using the named vector
    data.plot$Percentage <- weighted_percentages[data.plot$Condition]
    
    
    return(data.plot)
  })
  

   # plot 16 input
  table16_plot <- reactive({
    
    Var.Group <- sym(input$Tab16Variable1)
    
    # Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", 
    #                  "#005743", "#c2bdde","#9a1d4e","#d4e5ef","#d9bf77","#66b2b2",
    #                  "#8c4356","#f7ea39","#394240","#f26c4f","#3b3e42","#d27d67","#6b9362",
    #                  "#f9cb9c","#1e5090","#e58d7d","#2f615e","#c49bcd","#e2cf56",
    #                  "#6f83a5","#cf6a88","#55805b","#e9a1cf","#768d64")   
    # 
    # # modify colour palette to contain as many items as levels of inputted factor variable
    # Cab.Colours.Modified <- Cab.Colours[1:nlevels(table16_data()[[Var.Group]])]
    # if("Other/prefer not to say/unknown" %in% levels(table16_data()[[Var.Group]])){Cab.Colours.Modified <- head(Cab.Colours.Modified,-1)}
    # print(Cab.Colours.Modified)
    # 
    # 
    # # Plot
    # p <- ggplot(table16_data(), aes(x=Time, y=Percentage, fill=Condition)) + 
    #   geom_area(alpha=0.6 , size=1, colour="#004b88") + theme_classic() +
    #   theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88"),
    #         axis.text.y= element_text(color = "#004b88"), 
    #         strip.background = element_rect(color="#004b88", 
    #                                         fill="#fcbb69", size=1, linetype="solid"), 
    #         strip.text = element_text(colour = "#004b88"), 
    #         axis.line.x.bottom = element_line(colour = "#004b88"),
    #         axis.line.y.left = element_line(colour = "#004b88"), 
    #         axis.title.x = element_text(colour = "#004b88"),
    #         axis.title.y = element_text(colour = "#004b88"), 
    #         legend.background = element_rect(colour = "#004b88", fill = "white"), 
    #         legend.text = element_text(colour = "#004b88"), 
    #         legend.title = element_text(colour = "#004b88")) +
    #   scale_fill_manual(values= Cab.Colours)
    # 
    # p
    
    p <- ggplot(table16_data(), aes(x=Time, y=Percentage, fill=Condition, group=Condition)) + 
      geom_area() +
      theme_classic() +
      theme(text = element_text(size = 15), 
            axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"), 
            strip.background = element_rect(color="#004b88", fill="#fcbb69", size=1, linetype="solid"), 
            strip.text = element_text(colour = "#004b88"), 
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"), 
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"), 
            legend.background = element_rect(colour = "#004b88", fill = "white"), 
            legend.text = element_text(colour = "#004b88"), 
            legend.title = element_text(colour = "#004b88"))
   
   p
    
    
  })
  
  # return tab 16 data 
  output$table16 <- DT::renderDataTable({
    
    dt <- DT::datatable(table16_data(), options = list(scrollY = "200px"),
                        class = "cell-border")
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      'n',
      backgroundColor = styleInterval(
        c(100, 300),
        c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
      )
    )
    
    dt
  })
  

  # Return plot16
  output$plot16 <- renderPlotly({
    
    Var.Group <- sym(input$Tab16Variable1)
    
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba",
                     "#005743", "#c2bdde","#9a1d4e","#d4e5ef","#d9bf77","#66b2b2",
                     "#8c4356","#f7ea39","#394240","#f26c4f","#3b3e42","#d27d67","#6b9362",
                     "#f9cb9c","#1e5090","#e58d7d","#2f615e","#c49bcd","#e2cf56",
                     "#6f83a5","#cf6a88","#55805b","#e9a1cf","#768d64")
    # 
    # # Plot
    # p <- ggplot(table16_data(), aes(x=Time, y=Percentage, fill=Condition)) + 
    #   geom_area(alpha=0.6 , size=1, colour="#004b88", show.legend = FALSE) + theme_classic() +
    #   theme(text = element_text(size = 15), axis.text.x= element_text(color = "#004b88"),
    #         axis.text.y= element_text(color = "#004b88"), 
    #         strip.background = element_rect(color="#004b88", 
    #                                         fill="#fcbb69", size=1, linetype="solid"), 
    #         strip.text = element_text(colour = "#004b88"), 
    #         axis.line.x.bottom = element_line(colour = "#004b88"),
    #         axis.line.y.left = element_line(colour = "#004b88"), 
    #         axis.title.x = element_text(colour = "#004b88"),
    #         axis.title.y = element_text(colour = "#004b88"), 
    #         legend.background = element_blank(), 
    #         legend.text = element_text(colour = "#004b88", size = 8), 
    #         legend.title = element_blank()) +
    #   scale_fill_manual(values= Cab.Colours) + 
    #   labs(fill = NULL)  # This line removes the legend title
    # 
    # # Convert to Plotly and remove legend
    # p_plotly <- ggplotly(p, tooltip = c("x", "y", "fill")) 
    # 
    # # p_plotly <- p_plotly %>% 
    # #   layout(
    # #     xaxis = list(title = "", showticklabels = FALSE)
    # #   )
    # 
    # p_plotly
    
    p <- ggplot(table16_data(), aes(x=Time, y=Percentage, fill=Condition, group=Condition)) + 
      geom_area(alpha=0.6 , size=1, colour="#004b88") +
      theme_classic() +
      theme(text = element_text(size = 15), 
            axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"), 
            strip.background = element_rect(color="#004b88", fill="#fcbb69", size=1, linetype="solid"), 
            strip.text = element_text(colour = "#004b88"), 
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"), 
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"), 
            legend.background = element_rect(colour = "#004b88", fill = "white"), 
            legend.text = element_text(colour = "#004b88", size = 8),
            legend.title = element_blank()) 
    
    p_plotly <- ggplotly(p, tooltip = c("x", "y", "fill")) 
    
    p_plotly <- p_plotly %>% 
      layout(
      xaxis = list(title = "", showticklabels = FALSE)
        )
    
  })
  
  
  # Download tab 16 data 
  output$download16 <- downloadHandler(
    filename = function() {
      paste0(input$Tab16Variable1, ".csv")
    },
    content = function(file) {
      write.csv(table16_data(),file)
    }
  )
  
  # Download tab 16 plot 
  output$download16b <- downloadHandler(
    filename = function() {
      paste0(input$Tab16Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table16_plot(), device = device, height = 5.5)
    }
  )
  
  table18_data <- reactive({
    
    # Convert Variable and Time_Period strings to symbols
    Variable_sym <- rlang::sym(input$Tab18Variable1)
    Time_Period_sym <- rlang::sym(input$Tab18Variable2)
    
    # MART.Dash <- readRDS("MART_dash.rds")
    
    # Load shape data
    shape_data <- readRDS("shape_data.rds")
    shapes_la <- readRDS("shapes_la.rds")
    shapes_bmra <- readRDS("Shape.BMRA.rds")
    shapes_icb <- readRDS("shapes_icb.rds")
    
    # Determine which type of shape data to use and the grouping variable
    if (input$Boundary == "Region") {
      shape_data_use <- shape_data
      group_var <- "Plot.Region"
    } else if (input$Boundary == "Local Authority") {
      shape_data_use <- shapes_la
      group_var <- "Demographic.Local.Authority"
    } else if (input$Boundary == "BMRA") {
      shape_data_use <- shapes_bmra
      group_var <- "Demographic.BMRA"
    } else if (input$Boundary == "Integrated.Care.Board") {
      shape_data_use <- shapes_icb
      group_var <- "Demographic.Care.Board"
    }
    
    group_var_sym <- rlang::sym(group_var)
    
    # # Create data map
    # if (input$Tab18Variable1 == "Surplus") {
    #   data.map <- MART.Dash %>% 
    #     group_by(!!Time_Period_sym, !!group_var_sym) %>% 
    #     summarise(T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2), 
    #               n = n(), .groups = 'drop') %>% 
    #     filter(!!Time_Period_sym == input$Tab18Variable3)
    # } else {
    #   data.map <- MART.Dash %>% 
    #     filter(!!Variable_sym > 0) %>% 
    #     group_by(!!Time_Period_sym, !!group_var_sym) %>% 
    #     summarise(T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2),
    #               n = n(), .groups = 'drop') %>% 
    #     filter(!!Time_Period_sym == input$Tab18Variable3)
    # }
    
    
    # Convert to survey design
    survey_data <- as_survey_design(MART.Dash, weights = Weight.LCFS)
    
    if (input$Tab18Variable1 == "Surplus") {
      data.map <- survey_data %>% 
        group_by(!!Time_Period_sym, !!group_var_sym) %>% 
        summarise(T.Median = survey_median(!!Variable_sym, na.rm = TRUE),
                  n = n(), .groups = 'drop') %>% 
        filter(!!Time_Period_sym == input$Tab18Variable3)
    } else {
      data.map <- survey_data %>% 
        filter(!!Variable_sym > 0) %>% 
        group_by(!!Time_Period_sym, !!group_var_sym) %>% 
        summarise(T.Median = survey_median(!!Variable_sym, na.rm = TRUE),
                  n = n(), .groups = 'drop') %>% 
        filter(!!Time_Period_sym == input$Tab18Variable3)
    }
    
    # Perform the join
    joined_data <- left_join(data.map, shape_data_use, by = group_var)
    
    if ("Demographic.BMRA" %in% names(joined_data)) {
      joined_data <- joined_data %>%
        filter(Demographic.BMRA != "Unknown")
    } else if ("Demographic.Care.Board" %in% names(joined_data)) {
      joined_data <- joined_data %>%
        filter(Demographic.Care.Board != "Unknown")
    }
    
    
    joined_data
    
  })
  
  table18_plot <- reactive({
    
    # Convert Variable and Time_Period strings to symbols
    Variable_sym <- rlang::sym(input$Tab18Variable1)
    Time_Period_sym <- rlang::sym(input$Tab18Variable2)
    Colour <- as.character(input$Tab18Variable4)
    
    # Load shape data
    # shape_data <- readRDS("shape_data.rds")
    # shapes_la <- readRDS("shapes_la.rds")
    
    # Determine which type of shape data to use and the grouping variable
    if (input$Boundary == "Region") {
      shape_data_use <- shape_data
      group_var <- "Plot.Region"
    } else if (input$Boundary == "Local Authority") {
      shape_data_use <- shapes_la
      group_var <- "Demographic.Local.Authority"
    } else if (input$Boundary == "BMRA") {
      shape_data_use <- shapes_bmra
      group_var <- "Demographic.BMRA"
    } else if (input$Boundary == "Integrated.Care.Board") {
      shape_data_use <- shapes_icb
      group_var <- "Demographic.Care.Board"
    }
    
    group_var_sym <- rlang::sym(group_var)
    
    # Convert to survey design
    survey_data <- as_survey_design(MART.Dash, weights = Weight.LCFS)
    
    if (input$Tab18Variable1 == "Surplus") {
      data.map <- survey_data %>% 
        group_by(!!Time_Period_sym, !!group_var_sym) %>% 
        summarise(T.Median = survey_median(!!Variable_sym, na.rm = TRUE),
                  n = n(), .groups = 'drop') %>% 
        filter(!!Time_Period_sym == input$Tab18Variable3)
    } else {
      data.map <- survey_data %>% 
        filter(!!Variable_sym > 0) %>% 
        group_by(!!Time_Period_sym, !!group_var_sym) %>% 
        summarise(T.Median = survey_median(!!Variable_sym, na.rm = TRUE),
                  n = n(), .groups = 'drop') %>% 
        filter(!!Time_Period_sym == input$Tab18Variable3)
    }
    
    # Perform the join
    joined_data <- left_join(data.map, shape_data_use, by = group_var)
    
    if ("Demographic.BMRA" %in% names(joined_data)) {
      joined_data <- joined_data %>%
        filter(Demographic.BMRA != "Unknown")
    } else if ("Demographic.Care.Board" %in% names(joined_data)) {
      joined_data <- joined_data %>%
        filter(Demographic.Care.Board != "Unknown")
    }
    
    
    # # Calculate global min and max
    # if (input$Tab18Variable1 == "Surplus") {
    #   global_min <- MART.Dash %>% 
    #     group_by(!!Time_Period_sym, !!group_var_sym) %>% 
    #     summarise(T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2), .groups = 'drop') %>%
    #     summarise(Global.Min = min(T.Mean, na.rm = TRUE))
    #   
    #   global_max <- MART.Dash %>% 
    #     group_by(!!Time_Period_sym, !!group_var_sym) %>% 
    #     summarise(T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2), .groups = 'drop') %>%
    #     summarise(Global.Max = max(T.Mean, na.rm = TRUE))
    # } else {
    #   global_min <- MART.Dash %>% 
    #     filter(!!Variable_sym > 0) %>%
    #     group_by(!!Time_Period_sym, !!group_var_sym) %>% 
    #     summarise(T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2), .groups = 'drop') %>%
    #     summarise(Global.Min = min(T.Mean, na.rm = TRUE))
    #   
    #   global_max <- MART.Dash %>% 
    #     filter(!!Variable_sym > 0) %>%
    #     group_by(!!Time_Period_sym, !!group_var_sym) %>% 
    #     summarise(T.Mean = mean(!!Variable_sym, na.rm = TRUE, trim = 0.2), .groups = 'drop') %>%
    #     summarise(Global.Max = max(T.Mean, na.rm = TRUE))
    # }
    
    if (input$Tab18Variable1 == "Surplus") {
      global_min <- survey_data %>% 
        group_by(!!Time_Period_sym, !!group_var_sym) %>% 
        summarise(T.Median = survey_median(!!Variable_sym, na.rm = TRUE), .groups = 'drop') %>%
        summarise(Global.Min = min(T.Median, na.rm = TRUE))
      
      global_max <- survey_data %>% 
        group_by(!!Time_Period_sym, !!group_var_sym) %>% 
        summarise(T.Median = survey_median(!!Variable_sym, na.rm = TRUE), .groups = 'drop') %>%
        summarise(Global.Max = max(T.Median, na.rm = TRUE))
    } else {
      global_min <- survey_data %>% 
        filter(!!Variable_sym > 0) %>%
        group_by(!!Time_Period_sym, !!group_var_sym) %>% 
        summarise(T.Median = survey_median(!!Variable_sym, na.rm = TRUE), .groups = 'drop') %>%
        summarise(Global.Min = min(T.Median, na.rm = TRUE))
      
      global_max <- survey_data %>% 
        filter(!!Variable_sym > 0) %>%
        group_by(!!Time_Period_sym, !!group_var_sym) %>% 
        summarise(T.Median = survey_median(!!Variable_sym, na.rm = TRUE), .groups = 'drop') %>%
        summarise(Global.Max = max(T.Median, na.rm = TRUE))
    }
    
    # Define colour scheme
    if (Colour == "Red") {
      if (input$Tab18Variable1 == "Surplus") {
        low_colour <- "#800000" # dark red
        high_colour <- "white" # white
      } else {
        low_colour <- "white" # white
        high_colour <- "#800000" # dark red
      }
    } else if (Colour == "Green") {
      if (input$Tab18Variable1 == "Surplus") {
        low_colour <- "#006400" # dark green
        high_colour <- "white" # white
      } else {
        low_colour <- "white" # white
        high_colour <- "#006400" # dark green
      }
    }
    
    # Plot the choropleth map
    plot.map <- ggplot() +
      geom_sf(data = joined_data, aes(geometry = geometry, fill = T.Median, 
                                      text = paste0(group_var, ": ", !!group_var_sym, "<br>", input$Tab18Variable1, ": ", round(T.Median, 2)))) +
      scale_fill_gradient(low = low_colour, high = high_colour, 
                          limits = c(global_min$Global.Min, global_max$Global.Max),
                          name = paste0(input$Tab18Variable1)) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank())
    
    plot.map
    
  })
  
  # return tab 18 data
  output$table18 <- DT::renderDataTable({
    
    dt <- DT::datatable(table18_data(), options = list(scrollY = "200px"), class = "cell-border")
    
    # Apply conditional formatting to the column "n"
    dt <- dt %>% formatStyle(
      'n',
      backgroundColor = styleInterval(
        c(100, 300),
        c('lightcoral', 'lightgoldenrodyellow', 'lightgreen')
      )
    )
    
    dt
  })
  
  ## Return plot18
  output$plot18 <- renderPlotly({
    
    # Convert Variable and Time_Period strings to symbols
    Variable_sym <- rlang::sym(input$Tab18Variable1)
    Time_Period_sym <- rlang::sym(input$Tab18Variable2)
    Colour <- as.character(input$Tab18Variable4)
    
    # Determine which type of shape data to use and the grouping variable
    if (input$Boundary == "Region") {
      shape_data_use <- shape_data
      group_var <- "Plot.Region"
    } else if (input$Boundary == "Local Authority") {
      shape_data_use <- shapes_la
      group_var <- "Demographic.Local.Authority"
    } else if (input$Boundary == "BMRA") {
      shape_data_use <- shapes_bmra
      group_var <- "Demographic.BMRA"
    } else if (input$Boundary == "Integrated.Care.Board") {
      shape_data_use <- shapes_icb
      group_var <- "Demographic.Care.Board"
    }
    
    group_var_sym <- rlang::sym(group_var)
    
    # Convert to survey design
    survey_data <- as_survey_design(MART.Dash, weights = Weight.LCFS)
    
    if (input$Tab18Variable1 == "Surplus") {
      data.map <- survey_data %>% 
        group_by(!!Time_Period_sym, !!group_var_sym) %>% 
        summarise(T.Median = survey_median(!!Variable_sym, na.rm = TRUE),
                  n = n(), .groups = 'drop') %>% 
        filter(!!Time_Period_sym == input$Tab18Variable3)
    } else {
      data.map <- survey_data %>% 
        filter(!!Variable_sym > 0) %>% 
        group_by(!!Time_Period_sym, !!group_var_sym) %>% 
        summarise(T.Median = survey_median(!!Variable_sym, na.rm = TRUE),
                  n = n(), .groups = 'drop') %>% 
        filter(!!Time_Period_sym == input$Tab18Variable3)
    }
    
    # Perform the join
    joined_data <- left_join(data.map, shape_data_use, by = group_var)
    
    if ("Demographic.BMRA" %in% names(joined_data)) {
      joined_data <- joined_data %>%
        filter(Demographic.BMRA != "Unknown")
    } else if ("Demographic.Care.Board" %in% names(joined_data)) {
      joined_data <- joined_data %>%
        filter(Demographic.Care.Board != "Unknown")
    }
    
    if (input$Tab18Variable1 == "Surplus") {
      global_min <- survey_data %>% 
        group_by(!!Time_Period_sym, !!group_var_sym) %>% 
        summarise(T.Median = survey_median(!!Variable_sym, na.rm = TRUE), .groups = 'drop') %>%
        summarise(Global.Min = min(T.Median, na.rm = TRUE))
      
      global_max <- survey_data %>% 
        group_by(!!Time_Period_sym, !!group_var_sym) %>% 
        summarise(T.Median = survey_median(!!Variable_sym, na.rm = TRUE), .groups = 'drop') %>%
        summarise(Global.Max = max(T.Median, na.rm = TRUE))
    } else {
      global_min <- survey_data %>% 
        filter(!!Variable_sym > 0) %>%
        group_by(!!Time_Period_sym, !!group_var_sym) %>% 
        summarise(T.Median = survey_median(!!Variable_sym, na.rm = TRUE), .groups = 'drop') %>%
        summarise(Global.Min = min(T.Median, na.rm = TRUE))
      
      global_max <- survey_data %>% 
        filter(!!Variable_sym > 0) %>%
        group_by(!!Time_Period_sym, !!group_var_sym) %>% 
        summarise(T.Median = survey_median(!!Variable_sym, na.rm = TRUE), .groups = 'drop') %>%
        summarise(Global.Max = max(T.Median, na.rm = TRUE))
    }
    
    
    # Define colour scheme
    if (Colour == "Red") {
      if (input$Tab18Variable1 == "Surplus") {
        low_colour <- "#800000" # dark red
        high_colour <- "white" # white
      } else {
        low_colour <- "white" # white
        high_colour <- "#800000" # dark red
      }
    } else if (Colour == "Green") {
      if (input$Tab18Variable1 == "Surplus") {
        low_colour <- "#006400" # dark green
        high_colour <- "white" # white
      } else {
        low_colour <- "white" # white
        high_colour <- "#006400" # dark green
      }
    }
    
    
    # Plot the choropleth map
    plot.map <- ggplot() +
      geom_sf(data = joined_data, aes(geometry = geometry, fill = T.Median, 
                                      text = paste0(group_var, ": ", !!group_var_sym, "<br>", input$Tab18Variable1, ": ", round(T.Median, 2)))) +
      scale_fill_gradient(low = low_colour, high = high_colour, 
                          name = paste0(input$Tab18Variable1)) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank())
    
    # Convert to plotly
    plotly_map <- ggplotly(plot.map, tooltip = "text")
    
    return(plotly_map)
  })
  
  
  # Download tab 18 data 
  output$download18 <- downloadHandler(
    filename = function() {
      paste0(input$Tab18Variable1, ".csv")
    },
    content = function(file) {
      # Get the data
      data_to_write <- table18_data()
      
      # Remove the geometry column
      data_to_write <- data_to_write %>% 
        select(0:3)
      
      # Write the modified data to a CSV file
      write.csv(data_to_write, file)
    }
  )

  # Download tab 18 plot 
  output$download18b <- downloadHandler(
    filename = function() {
      paste0(input$Tab18Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table18_plot(), device = device, height = 5.5)
    }
  )
  
  
  #Table 17 data
  # table17_data <- reactive({
  #   
  #   # # Copy colour palette over from Flourish
  #   Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", "#005743", "#c2bdde","#9a1d4e","#d4e5ef")
  #   
  #   # MART.Dash <- readRDS("MART_Dash.rds")
  #   
  #   variable <- rlang::sym(input$Tab17Variable1)
  #   Quarterdate1 <- as.character(input$Tab17Variable2)
  #   Quarterdate2 <- as.character(input$Tab17Variable3)
  #   
  #   # Trim top and bottom 1%
  #   keep <- MART.Dash %>% 
  #     mutate(keep = .data[[rlang::as_string(variable)]] >= quantile(.data[[rlang::as_string(variable)]], .01) & 
  #              .data[[rlang::as_string(variable)]] <= quantile(.data[[rlang::as_string(variable)]], .99)) %>%   
  #     pull(keep)
  #   
  #   Expenditure.Plot <- MART.Dash[keep,]
  #   Expenditure.Plot <- Expenditure.Plot %>%
  #     select(!!variable, Year, Quarter) %>% 
  #     filter(!!variable > 0)
  #   
  #   # Filter data based on input Quarterdate
  #   df1.1 <- Expenditure.Plot %>% filter(Quarter == Quarterdate1)
  #   df2.1 <- Expenditure.Plot %>% filter(Quarter == Quarterdate2)
  #   df3.2 <- rbind(df1.1, df2.1)
  #   df3.2$Quarter <- as.factor(df3.2$Quarter)
  #   
  #   df3.2
  #   
  # })
  
  #Table 17 data
  # table17_data <- reactive({
  #   
  #   # Copy colour palette over from Flourish
  #   Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", "#005743", "#c2bdde","#9a1d4e","#d4e5ef")
  #   
  #   # Convert the inputs to symbols for dplyr operations
  #   variable1 <- rlang::sym(input$Tab17Variable1)
  #   variable2 <- rlang::sym(input$Tab17Variable2)
  #   Financial.Year <- as.character(input$Tab17Variable3)
  #   
  #   # Filter data based on input Financial.Year
  #   filtered_data <- MART.Dash %>% filter(Financial.Year == Financial.Year)
  #   
  #   # Extract the necessary columns for the two selected variables
  #   comparison_data <- filtered_data %>%
  #     select(!!variable1, !!variable2, Financial.Year) %>% 
  #     filter(!!variable1 > 0 & !!variable2 > 0)
  #   
  #   # If you want to trim data based on percentiles like in your original code:
  #   keep1 <- comparison_data %>%
  #     mutate(keep = .data[[rlang::as_string(variable1)]] >= quantile(.data[[rlang::as_string(variable1)]], .01) &
  #              .data[[rlang::as_string(variable1)]] <= quantile(.data[[rlang::as_string(variable1)]], .99)) %>%   
  #     pull(keep)
  #   
  #   keep2 <- comparison_data %>%
  #     mutate(keep = .data[[rlang::as_string(variable2)]] >= quantile(.data[[rlang::as_string(variable2)]], .01) &
  #              .data[[rlang::as_string(variable2)]] <= quantile(.data[[rlang::as_string(variable2)]], .99)) %>%   
  #     pull(keep)
  #   
  #   # Apply the trimming
  #   comparison_data <- comparison_data[keep1 & keep2,]
  #   
  #   return(comparison_data)
  #   
  # })
  # 
  
  # table17_data <- reactive({
  #   
  #   # Convert the inputs to symbols for dplyr operations
  #   variable1 <- rlang::sym(input$Tab17Variable1)
  #   variable2 <- rlang::sym(input$Tab17Variable2)
  #   Financial.Year.Input <- as.character(input$Tab17Variable3)
  #   
  #   # Filter data based on input Financial.Year
  #   filtered_data <- MART.Dash %>% filter(Financial.Year == Financial.Year.Input)
  #   
  #   # Extract the necessary columns for the two selected variables
  #   comparison_data <- filtered_data %>%
  #     select(!!variable1, !!variable2, Financial.Year) %>% 
  #     filter(!!variable1 > 0 & !!variable2 > 0)
  #   
  #   # Trim data based on percentiles for both variables
  #   lower_quantile1 <- quantile(comparison_data[[rlang::as_string(variable1)]], .01)
  #   upper_quantile1 <- quantile(comparison_data[[rlang::as_string(variable1)]], .99)
  #   
  #   lower_quantile2 <- quantile(comparison_data[[rlang::as_string(variable2)]], .01)
  #   upper_quantile2 <- quantile(comparison_data[[rlang::as_string(variable2)]], .99)
  #   
  #   trimmed_data <- comparison_data %>% 
  #     filter(!!variable1 >= lower_quantile1 & !!variable1 <= upper_quantile1 &
  #              !!variable2 >= lower_quantile2 & !!variable2 <= upper_quantile2)
  #   
  #   return(trimmed_data)
  #   
  # })
  
  table17_data <- reactive({
    
    # Convert the inputs to symbols for dplyr operations
    variable1 <- rlang::sym(input$Tab17Variable1)
    variable2 <- rlang::sym(input$Tab17Variable2)
    Financial.Year.Input <- as.character(input$Tab17Variable3)
    
    # Filter data based on input Financial.Year
    filtered_data <- MART.Dash %>% filter(Financial.Year == Financial.Year.Input)
    
    # Extract the necessary columns for the two selected variables
    comparison_data <- filtered_data %>%
      select(!!variable1, !!variable2, Financial.Year, Weight.LCFS) %>% 
      filter(!is.na(!!variable1) & !is.na(!!variable2) &
               !!variable2 != 0)  # Removed filtering based on variable1
    
    # Trim data based on percentiles for variable2 only, because variable1 is categorical
    lower_quantile2 <- quantile(comparison_data[[rlang::as_string(variable2)]], .01, na.rm = TRUE)
    upper_quantile2 <- quantile(comparison_data[[rlang::as_string(variable2)]], .99, na.rm = TRUE)
    
    trimmed_data <- comparison_data %>% 
      filter(!!variable2 >= lower_quantile2 & !!variable2 <= upper_quantile2)
    
    return(trimmed_data)
  })
  
  # # plot 17 input
  # table17_plot <- reactive({
  #   
  #   # Copy colour palette over from Flourish
  #   Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", "#005743", "#c2bdde","#9a1d4e","#d4e5ef")
  #   
  #   # Convert the inputs to symbols for dplyr operations
  #   variable1 <- rlang::sym(input$Tab17Variable1)
  #   variable2 <- rlang::sym(input$Tab17Variable2)
  #   
  #   # Calculate median values for both variables
  #   median1 <- median(table17_data()[, rlang::as_string(variable1)])
  #   median2 <- median(table17_data()[, rlang::as_string(variable2)])
  #   
  #   # Compute densities for both variables
  #   dens1 <- density(table17_data()[, rlang::as_string(variable1)])
  #   dens2 <- density(table17_data()[, rlang::as_string(variable2)])
  #   
  #   # Combine data for plotting
  #   df1 <- data.frame(x=dens1$x, y=dens1$y, Variable=input$Tab17Variable1)
  #   df2 <- data.frame(x=dens2$x, y=dens2$y, Variable=input$Tab17Variable2)
  #   combined_data <- rbind(df1, df2)
  #   
  #   # Plotting
  #   plot <- ggplot(combined_data, aes(x = x, y = y, fill = Variable)) +
  #     geom_area(alpha=0.5, position="identity") +
  #     ylab("Probability") + theme_classic() +
  #     theme(text = element_text(size= 20)) +
  #     scale_fill_manual(values= Cab.Colours) +
  #     theme(text = element_text(size = 15),
  #           axis.text.x= element_text(color = "#004b88"),
  #           axis.text.y= element_text(color = "#004b88"),
  #           axis.line.x.bottom = element_line(colour = "#004b88"),
  #           axis.line.y.left = element_line(colour = "#004b88"),
  #           axis.title.x = element_text(colour = "#004b88"),
  #           axis.title.y = element_text(colour = "#004b88"),
  #           legend.background = element_rect(colour = "#004b88", fill = "white"),
  #           legend.text = element_text(colour = "#004b88"),
  #           legend.title = element_text(colour = "#004b88")) +
  #     # Add annotations for medians
  #     annotate("segment", x = median1, xend = median1, y = 0,
  #              yend = max(dens1$y), colour = "#fcbb69", linetype = "dashed", size = 0.75) +
  #     annotate("segment", x = median2, xend = median2, y = 0,
  #              yend = max(dens2$y), colour = "#004b88", linetype = "dashed", size = 0.75) 
  #   
  #   plot
  # })
  
  table17_plot <- reactive({
    
    # Extended colour palette
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", 
                     "#005743", "#c2bdde","#9a1d4e","#d4e5ef","#d9bf77","#66b2b2",
                     "#8c4356","#f7ea39","#394240","#f26c4f","#3b3e42","#d27d67","#6b9362","#f9cb9c")
    
    # Convert the inputs to symbols for dplyr operations
    variable1 <- rlang::sym(input$Tab17Variable1)
    variable2 <- rlang::sym(input$Tab17Variable2)
    
    densities <- table17_data() %>%
      group_by(Variable = !!rlang::sym(variable1)) %>%
      nest() %>%
      mutate(density_data = map(data, ~{
        d = density(.x[[variable2]])
        data.frame(x = d$x, y = d$y)
      })) %>%
      select(-data) %>%
      unnest(cols = density_data)
    
    # Plotting
    # plot <- ggplot(densities, aes(x = x, y = y, fill = Variable)) +
    #   geom_area(alpha=0.5, position="identity") +
    #   ylab("Probability") + theme_classic() +
    #   theme(text = element_text(size= 20)) +
    #   scale_fill_manual(values= Cab.Colours) +
    #   theme(text = element_text(size = 15),
    #         axis.text.x= element_text(color = "#004b88"),
    #         axis.text.y= element_text(color = "#004b88"),
    #         axis.line.x.bottom = element_line(colour = "#004b88"),
    #         axis.line.y.left = element_line(colour = "#004b88"),
    #         axis.title.x = element_text(colour = "#004b88"),
    #         axis.title.y = element_text(colour = "#004b88"),
    #         legend.background = element_rect(colour = "#004b88", fill = "white"),
    #         legend.text = element_text(colour = "#004b88"),
    #         legend.title = element_text(colour = "#004b88"))
    
    plot <- ggplot(table17_data(), aes(x = !!variable2, weight = Weight.LCFS, fill = !!variable1)) +
      geom_density(alpha=0.5) +
      ylab("Unweighted Density") +
      xlab(input$Tab17Variable1) + # Replace "Variable Name" with the actual variable name
      theme_classic() +
      scale_fill_manual(values= Cab.Colours) +
      theme(text = element_text(size = 15),
            axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"),
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"),
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"),
            legend.background = element_rect(colour = "#004b88", fill = "white"),
            legend.text = element_text(colour = "#004b88", size = 8),
            legend.title = element_blank()) +
      labs(fill = NULL)
    
    plot
  })
  
  
  
  
  # Return tab 17 plot
  output$plot17 <- renderPlotly({
    
    # Extended colour palette
    Cab.Colours <- c("#004b88", "#fcbb69", "#57486b", "#a6d6ae", "#006278", "#fcceba", 
                     "#005743", "#c2bdde","#9a1d4e","#d4e5ef","#d9bf77","#66b2b2",
                     "#8c4356","#f7ea39","#394240","#f26c4f","#3b3e42","#d27d67","#6b9362","#f9cb9c")
    
    # Convert the inputs to symbols for dplyr operations
    variable1 <- rlang::sym(input$Tab17Variable1)
    variable2 <- rlang::sym(input$Tab17Variable2)
    

    densities <- table17_data() %>%
      group_by(Variable = !!rlang::sym(variable1)) %>%
      nest() %>%
      mutate(density_data = map(data, ~{
        d = density(.x[[variable2]])
        data.frame(x = d$x, y = d$y)
      })) %>%
      select(-data) %>%
      unnest(cols = density_data)
    
    
    # Plotting
# plot <- ggplot(original_data, aes(x = value, weight = weights, fill = group)) +
#   geom_density(alpha=0.5) +
#   ylab("Weighted Density") +
#   xlab("Variable Name") + # Replace "Variable Name" with the actual variable name
#   theme_classic() +
#   scale_fill_manual(values= Cab.Colours) +
#   theme(text = element_text(size = 15),
#         axis.text.x= element_text(color = "#004b88"),
#         axis.text.y= element_text(color = "#004b88"),
#         axis.line.x.bottom = element_line(colour = "#004b88"),
#         axis.line.y.left = element_line(colour = "#004b88"),
#         axis.title.x = element_text(colour = "#004b88"),
#         axis.title.y = element_text(colour = "#004b88"),
#         legend.background = element_rect(colour = "#004b88", fill = "white"),
#         legend.text = element_text(colour = "#004b88", size = 8),
#         legend.title = element_blank()) +
#         labs(fill = NULL)
    
    plot <- ggplot(table17_data(), aes(x = !!variable2, weight = Weight.LCFS, fill = !!variable1)) +
      geom_density(alpha=0.5) +
      ylab("Unweighted Density") +
      xlab(input$Tab17Variable1) + # Replace "Variable Name" with the actual variable name
      theme_classic() +
      scale_fill_manual(values= Cab.Colours) +
      theme(text = element_text(size = 15),
            axis.text.x= element_text(color = "#004b88"),
            axis.text.y= element_text(color = "#004b88"),
            axis.line.x.bottom = element_line(colour = "#004b88"),
            axis.line.y.left = element_line(colour = "#004b88"),
            axis.title.x = element_text(colour = "#004b88"),
            axis.title.y = element_text(colour = "#004b88"),
            legend.background = element_rect(colour = "#004b88", fill = "white"),
            legend.text = element_text(colour = "#004b88", size = 8),
            legend.title = element_blank()) +
      labs(fill = NULL)
      
    
    # Convert to Plotly
    plotly_plot <- ggplotly(plot,tooltip = c("x", "y", "fill"))
    
    return(plotly_plot)
  })
  

  
  # Download tab 17 plot 
  output$download17b <- downloadHandler(
    filename = function() {
      paste0(input$Tab17Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table17_plot(), device = device, height = 5.5)
    }
  )
  
   # # Server
  #  lapply(1:18, function(i) {
  #    output[[paste0("filterStatus", i)]] <- renderText({
  #      input$confirm
  #      if (is.null(input$group_var)) {
  #        return("No filter applied")
  #      }
  #      else {
  #        if(input$numeric_filter_type == "Between") {
  #          paste("Current Filter =", input$group_var, levels(MART.Dash[,input$group_var])[input$group_levels],
  #                input$numeric_var, input$numeric_filter_type, paste(input$numeric_range[1], "to", input$numeric_range[2]))
  #        } else {
  #          paste("Current Filter =", input$group_var, levels(MART.Dash[,input$group_var])[input$group_levels],
  #                input$numeric_var, input$numeric_filter_type, input$numeric_value)
  #       }
  #     }
  #   })
  # })

}))









