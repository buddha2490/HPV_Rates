### Shiny app
library(shiny)
library(dplyr)
library(shinythemes)
library(DT)
library(shinydashboard)
library(rsconnect)
library(AzureStor)
library(stringr)
library(ggplot2)
library(gridExtra)
library(rdrop2)
library(shinyjs)
library(shinyalert)
library(RSQLite)
library(rhandsontable)
library(dbplyr)
library(parallel)
library(grid)
library(plotly)
library(WriteXLS)
rm(list=ls())



# December 1st 2020
# App authors:  Brian Carter, Becky Hodge
# Note to reviewers:  The authors are not responsible for any positive or negative
#  side effects that stem from reading or reviewing this app.  This includes
#  any physical or emotional trauma, acute and post-acute, that you may experience.
#  Read this app at your own risk, the authors relinquish all responsibility.
#  The app works, nothing else matters.



# Ui ----------------------------------------------------------------------

ui = dashboardPage(
  title = "HPV Vaccination Systems and Strategies Inventory 2021",
  header = dashboardHeader(title = textOutput("username")),
  
  
  # Sidebar layout ----------------------------------------------------------
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      type = "hidden",
      menuItem("Log Out",
               href = "",
               icon=icon("user-cog"))
    ),
    tags$head(tags$style(".mybutton{background-color:black;} .skin-black .sidebar .mybutton{color: white;}"))
  ),
  
  body = dashboardBody(
    shinyjs::useShinyjs(),
    # need this so we can do the session$reload() thing
    shinyalert::useShinyalert(),
    # need this for those lovely pop-ups
    
    tags$style(
      HTML(
        ".box.box-solid.box-success>.box-header {
                                      background:#fca500
                                      }
                                      .box.box-solid.box-success{
                                      border-bottom-color:#fca500;
                                      border-left-color:#fca500;
                                      border-right-color:#fca500;
                                      border-top-color:#fca500
                                      }"
      )
    ),
    # this is to change the 'status=success' box color to yellow instead of green
    # Make the error messages red when we use the validate function
    tags$style(HTML(
      ".shiny-output-error-red_warnings {
                          color: red;
                          }
                          "
    )),
    tags$style(
      HTML(
        ".shiny-input-container:not(.shiny-input-container-inline) {
              width: 100%;
              }"
      )
    ),
      # Baseline information page -----------------------------------------------

          # Vaccination rates tab ---------------------------------------------------
          
          # TEXT STUFF
          #tabPanel(
            valueBoxOutput("date_brates", width=12),
            h3("Vaccination Rates", align = "center"),
            HTML("<br>"),
            h4("INSTRUCTIONS:"),
            helpText("This section collects baseline vaccination rates for your health system. Vaccination rates 
                     should be calculated for active medical patients ages 9-13 at participating clinic sites who were up-to-date 
                     with HPV, Tdap, and Meningococcal vaccines in 2020. The definitions and tips listed below will help you
                      calculate your vaccination rates. Data entry tables will appear after you answer the prompts to the questions
                      below."),
            HTML("<br>"),
            h4("DEFINITIONS:"),
            tags$ul(
              tags$li(
                em("HPV vaccine initiation"), "number should include patients who have ",
                strong("ever"), " received 1 dose of the HPV vaccine. (This number will include patients who received both their 1st and 2nd dose.)"
              ),
              tags$li(
                em("HPV vaccination series completion "), "includes patients who have received 2 doses of HPV vaccine separated by at least 5 months"
              ),
              tags$li(
                em("Active medical patients"),
                " are defined as those who were seen at least once during ",
                strong("the time period you specify below"),
                " (12 months, 18 months, 24 months). Medical visits do not include dental or other non-medical visits. Medical visits",
                em(" do "), "include well-child visits and sick visits. Please note that a longer time period allows more patients to be 
                included in the rate calculation to account for the infrequenty medical viits for patients ages 9-13."
              ),
              tags$li(
                "The ", em("reporting period "), "is used to define patient age to determine which active patients were, for example,
                 age 13 during the baseline year. The reporting period for baseline is January 1, 2020 to December 31, 2020. Patients included in the age 13 group turned 13 during the reporting period"
              ),
              tags$li(
                em("Up-to-date "), "is defined as active medical patients in the relevant age and sex categories who have ",
                strong("ever "), "received the specified vaccine (or dose). They may have received the vaccine prior to the reporting period
                and should still be counted. This means that the date of service is not relevant to the calculation of vaccination rates for this project.
                Use the following table to identify ", 
                strong("up-to-date "), "patients in 2020."
              )
            ),
            
            HTML("<br>"),
            tableOutput("static_table"),
            
            h4("TIPS ON DATA QUALITY:"),
            helpText(p("You will see ", strong("red notes ", style="color:red"), "appear if entered data includes the following data quality flags:")),
            tags$ul(
              tags$li(
                "The number of patients receiving a vaccination exceeds the number of active patients"
              ),
              tags$li(
                "Completion rates exceed initiation rates (every child that completed the series must have been initiated)"
              )
            ),
            helpText("Other quality tips you will not be notified for but you should pay attention to include:"),
            tags$ul(
              tags$li(
                "Be sure to enter data for the correct age into each table"
              ),
              tags$li(
                "The active patient population should not be the same for different age groups or sex (this would be highly unlikely)"
              ),
              tags$li(
                "Do any of the numbers seem really high or really low? Is the number of patients who received one or two doses for HPV a lot higher than Tdap and Meningococcal?
                Are more patients age 9-10 vaccinated than those age 13? These questions could highlight potential issues in data quality."
              )
            ),
            HTML("<br>"),
            h4("CUSTOMIZING YOUR VACCINATION RATE TABLES:"),
            helpText("We strongly encourage you to report vaccination rates separated by sex and specific age groupings (9-10, 11-12, and 13).
                      We recognize this may not always be possible. Use the following question prompts to customize the data entry tables you need. ",
                     strong("Once you respond to all the following prompts, your data table(s) will appear below.")
            ),
            HTML("<br>"),
            
            uiOutput("healthSystem"),
    
            # These go AFTER the instructions but before the rates
            splitLayout(
              uiOutput("Q27"), uiOutput("Q27_other")
            ),
            uiOutput("Q28"), 
            fluidRow(
              column(8,
                     uiOutput("Q28_other")),
              column(4,
                     uiOutput("Q28_other_message"))
            ),
            #uiOutput("Q28_other"),
            #uiOutput("Q28_other_message"),
            uiOutput("Q29"),
            HTML("<br>"),
            
            # Rates tables
            uiOutput("brates_9_10"),
            uiOutput("brates_11_12"),
            uiOutput("brates_13"),
            
            verticalLayout(
              HTML("<br><br>"),
              
              box(
                width = '100%',
                span(
                  uiOutput("Q30"),
                ),
                textOutput("error_rates"),
                uiOutput("Q31"),
                uiOutput("Q31_other"),
                uiOutput("Q32"),
                splitLayout(uiOutput("Q32_details"),
                            uiOutput("Q32_other"))
              ),
              
              fluidRow(
                column(6,  uiOutput("Q33")),
                column(6, uiOutput("Q33_other"))),
              uiOutput("Q34")
            ), # End vertical arrangement
            
            HTML("<br>"),
            p("Please select the Save button to save and download your data.", style = "color:green"),
            downloadButton("downloadData", "Save"))
            )
            

          
          


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  addClass(selector = "body", class = "sidebar-collapse")

# Initialize data ---------------------------------------------------------
  # Pull my inputData
  inputData <- dbConnect(SQLite(), "inputData.DB")
  
  # User directory
  shortUser <- stringr::str_replace(session$user, pattern = "[[@]].*", "")
  #shortUser <- "bcarter6"
  ## Define the endpoint and container
  
  # One primary containers: DataSrc
  endpoint <- storage_endpoint(
    dbReadTable(inputData, "azureDat")$myFileshare,
    dbReadTable(inputData, "azureDat")$mykey
  )
  DataSrc <- storage_container(endpoint, "hpv/RateSrc")
  
  # Within each container, each user gets a folder for their temporary data
  baseFiles <- dplyr::filter(list_storage_files(DataSrc), isdir == F)$name  # list of names or NULL
  
  # Create the filename for each user and initialize the SQLite
  userFilename <- paste0(shortUser,".DB")
  
  #  This will initialize the database from an initial null database
  if (!userFilename %in% baseFiles)  {
    file.copy("rateDB.DB", userFilename)
    storage_upload(DataSrc, userFilename)
  }
    
  if (userFilename %in% baseFiles) {
    storage_download(DataSrc, userFilename, overwrite = T)
  }
  
  dbDisconnect(inputData)
  myDB <- dbConnect(SQLite(), userFilename)
  
  
  
  
  
  # Rates page  -------------------------------------------------------------
  

  savedRates <- dbReadTable(myDB, "rates")
  
  
  # Checkbox groups
  foo28 <- with(savedRates, c(Q28_1, Q28_2, Q28_3, Q28_4))
  foo28 <- foo28[!is.na(foo28)]
  
  foo32 <- with(savedRates, c(Q32_details1, Q32_details2, Q32_details3, Q32_details4))
  foo32 <- foo32[!is.na(foo32)]
  
  output$healthSystem <- renderUI({
    textInput(
      inputId = "healthSystem",
      label = "Please enter the name of your health system",
      value = savedRates$healthSystem
    )
  })
  output$Q27 <- renderUI({
    selectInput(
      inputId = "Q27",
      label = "1  How are you defining your active patient populations?",
      choices = c("",
                  "12 months",
                  "18 months",
                  "24 months",
                  "Other (please specify)"),
      selected = savedRates$Q27,
      selectize = F
    )
  })
  output$Q27_other <- renderUI({
    validate(need(input$Q27, ""))
    if (input$Q27 != "Other (please specify)") {
      return(NULL)
    } else {
      textInput(
        inputId = "Q27_other",
        label = "Please specify",
        value = savedRates$Q27_other,
        placeholder = "",
        width = "150%")
    }
  })
  
  
  # Q28 - CheckBoxGroup
  output$Q28 <- renderUI({
    checkboxGroupInput(
      inputId = "Q28",
      label = "2. For what ages are you reporting?",
      choices = c("9-10",
                  "11-12",
                  "13",
                  "We can't report on these ages (other, please specify)"),
      selected = foo28)
  })
  output$Q28_other <- renderUI({
    validate(need(input$Q28, ""))
    if (("We can't report on these ages (other, please specify)" %in% input$Q28) &
        (!c("9-10", "11-12", "13") %in% input$Q28)) {
      textAreaInput(
        inputId = "Q28_other",
        label = "Please specify",
        value = savedRates$Q28_other,
        placeholder = "",
        height = "100px")
    } else {
      return(NULL)
    }
  })
  
  # Include message about how they should contact Jennifer to resolve this issue
  output$Q28_other_message <- renderUI({
    validate(need(input$Q28, ""))
    if (("We can't report on these ages (other, please specify)" %in% input$Q28) &
        (!c("9-10", "11-12", "13") %in% input$Q28)) {
      HTML("<br>")
      helpText("Please contact Jennifer Isher-Witt at Jennifer.Ish@cancer.org to resolve this issue",
               style = "color:red")
    }
  })
  
  output$Q29 <- renderUI({
    selectInput(
      inputId = "Q29",
      label = "3.  Can you pull rate data by sex?",
      choices = c("",
                  "Yes, we'll report separate data for male and female patients",
                  "No, we will have combined rate data"),
      selected = savedRates$Q29)
  })
  
  
  # This is how you get rid of the boxes
  observe({
    x <- input$Q28
    if (!"9-10" %in% x) {
      output$brates_9_10 <- renderUI(NULL)
    }
    if (!"11-12" %in% x) {
      output$brates_11_12 <- renderUI(NULL)
    }
    if (!"13" %in% x) {
      output$brates_13 <- renderUI(NULL)
    }
    
  })
  
  # Create the ages 9-10 boxes
  observeEvent(list(input$Q28, input$Q29), {
    validate(need(input$Q28, ""))
    validate(need(input$Q29, ""))
    
    
    # Boys and girls - ages 9-10
    if ("9-10" %in% input$Q28 &
        input$Q29 == "Yes, we'll report separate data for male and female patients") {
      
      # Girls ages 9-10 rates 
      output$brates_9_10 <- renderUI({
        verticalLayout(
          splitLayout(
            box(
              h3("Vaccination Rates for Female Patients, Ages 9-10"),
              title = "Vaccination Rates for Female Patients, Ages 9-10",
              collapsible = T,
              solidHeader = T,
              status = "primary",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("female ", style = "color:blue"),
                  "medical patients ",
                  em("ages 9-10 ", style = "color:red"),
                  "your clinic saw in 2020?"
                )
              ),
              numericInput(
                "FemAge1_total",
                label = NULL,
                value = savedRates$FemAge1_total,
                min = 0,
                step = 1,
                width = '50%'
              ),
              uiOutput("error_f9_total"),
              #textOutput("error_f9_total"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge1_dose1",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge1_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge1_dose1_rate")
              ),
              uiOutput("error_f9_gt1"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge1_dose2",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge1_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge1_dose2_rate")
              ),
              uiOutput("error_f9_2plus"),
              uiOutput("error_f9a_2plus")
            ),
            box( # Men aged 9-10
              h3("Males, ages 9-10"),
              title = "Males, ages 9-10",
              collapsible = T,
              solidHeader = T,
              status = "danger",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("male ", style = "color:blue"),
                  "medical patients ",
                  em("ages 9-10 ", style = "color:red"),
                  "your clinic saw in 2020?"
                )
              ),
              numericInput(
                "MenAge1_total",
                label = NULL,
                value = savedRates$MenAge1_total,
                min = 0,
                step = 1,
                width = '50%'
              ),
              uiOutput("error_m9_total"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge1_dose1",
                  label = NULL,
                  min = 0,
                  value =  savedRates$MenAge1_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge1_dose1_rate")
              ),
              uiOutput("error_m9_gt1"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge1_dose2",
                  label = NULL,
                  min = 0,
                  value =  savedRates$MenAge1_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge1_dose2_rate")
              ),
              uiOutput("error_m9_2plus"),
              uiOutput("error_m9a_2plus")
            )
          )) # end split and veritical layout
      })
      
      output$error_f9_total <- renderUI({
        if (is.na(input$FemAge1_total)) {
          helpText("This number is required", style = "color:red")
        } else {
          NULL
        }
      })
      output$error_f9_gt1 <- renderUI({
        if (is.na(input$FemAge1_dose1) | is.na(input$FemAge1_total)) {
          return(NULL)
        } else if (input$FemAge1_dose1 > input$FemAge1_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_f9_2plus <- renderUI({if (is.na(input$FemAge1_dose2) | is.na(input$FemAge1_total)) {
        return(NULL)
      } else if (input$FemAge1_dose2 > input$FemAge1_total) {
        helpText("This number can't be greater than the number of patients seen",
                 style = "color:red")
      }
      })
      output$error_f9a_2plus <- renderUI({
        if (is.na(input$FemAge1_dose2) | is.na(input$FemAge1_dose1)) {
          return(NULL)
        } else if (input$FemAge1_dose2 >= input$FemAge1_dose1){
          helpText("This number can't be greater than the total number of patients who received the first dose",
                   style = "color:red")
        }
      })
      output$FemAge1_dose1_rate <- renderText({
        validate(need(input$FemAge1_total, ""))
        if (is.na(input$FemAge1_dose1)) {
          return(NULL)
        }
        if (!is.na(input$FemAge1_dose1)) {
          rate <- input$FemAge1_dose1 / input$FemAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
      output$FemAge1_dose2_rate <- renderText({
        validate(need(input$FemAge1_total, ""))
        if (is.na(input$FemAge1_dose2)) {
          return(NULL)
        }
        if (!is.na(input$FemAge1_dose2)) {
          rate <- input$FemAge1_dose2 / input$FemAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
      
      output$error_m9_total <- renderUI({
        if (is.na(input$MenAge1_total)) {
          helpText("This number is required", style = "color:red")
        } else {
          NULL
        }
      })
      output$error_m9_gt1 <- renderUI({
        if (is.na(input$MenAge1_dose1) | is.na(input$MenAge1_total)) {
          return(NULL)
        } else if (input$MenAge1_dose1 > input$MenAge1_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_m9_2plus <- renderUI({if (is.na(input$MenAge1_dose2) | is.na(input$MenAge1_total)) {
        return(NULL)
      } else if (input$MenAge1_dose2 > input$MenAge1_total) {
        helpText("This number can't be greater than the number of patients seen",
                 style = "color:red")
      }
      })
      output$error_m9a_2plus <- renderUI({
        if (is.na(input$MenAge1_dose2) | is.na(input$MenAge1_dose1)) {
          return(NULL)
        } else if (input$MenAge1_dose2 >= input$MenAge1_dose1){
          helpText("This number can't be greater than the total number of patients who received the first dose",
                   style = "color:red")
        }
      })
      output$MenAge1_dose1_rate <- renderText({
        validate(need(input$MenAge1_total, ""))
        if (is.na(input$MenAge1_dose1)) {
          return(NULL)
        }
        if (!is.na(input$MenAge1_dose1)) {
          rate <- input$MenAge1_dose1 / input$MenAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
      output$MenAge1_dose2_rate <- renderText({
        validate(need(input$MenAge1_total, ""))
        if (is.na(input$MenAge1_dose2)) {
          return(NULL)
        }
        if (!is.na(input$MenAge1_dose2)) {
          rate <- input$MenAge1_dose2 / input$MenAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
    } 
    
    # Ages 9-10, combined
    if ("9-10" %in% input$Q28 &
        input$Q29 == "No, we will have combined rate data") {
      # Create the 'male and females combined' box
      output$brates_9_10 <- renderUI({
        box(
          h3("Vaccination Rates for Male and Female Patients Combined, Ages 9-10"),
          title = "Vaccination Rates for Male and Female Patients Combined, Ages 9-10",
          collapsible = T,
          solidHeader = T,
          status = "success",
          collapsed = T,
          width = '100%',
          HTML("<br>"),
          h5(
            strong(
              "Total number of active medical patients ",
              em("ages 9-10 ", style = "color:red"),
              "your clinic saw in 2020?"
            )
          ),
          numericInput(
            "BothAge1_total",
            label = NULL,
            value = savedRates$BothAge1_total,
            min = 0,
            step = 1,
            width = '50%'
          ),
          uiOutput("error_9_total"),
          h5(
            strong(
              "Number of active patients who received at least one (1) HPV dose"
            )
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge1_dose1",
              label = NULL,
              min = 0,
              value = savedRates$BothAge1_dose1,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge1_dose1_rate")
          ),
          uiOutput("error_9_gt1"),
          h5(
            strong("Number of active patients who received both (2) HPV doses")
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(inputId = "BothAge1_dose2",
                         label = NULL,
                         min = 0,
                         value = savedRates$BothAge1_dose2,
                         step = 1,
                         width = '50%'
            ),
            textOutput("BothAge1_dose2_rate")
          ),
          uiOutput("error_9_2plus"),
          uiOutput("error_9a_2plus")
        )
      })
      output$BothAge1_dose1_rate <- renderText({
        validate(need(input$BothAge1_total, ""))
        if (is.na(input$BothAge1_dose1)) {
          return(NULL)
        }
        if (!is.na(input$BothAge1_dose1)) {
          rate <- input$BothAge1_dose1 / input$BothAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
      output$BothAge1_dose2_rate <- renderText({
        validate(need(input$BothAge1_total, ""))
        if (is.na(input$BothAge1_dose2)) {
          return(NULL)
        }
        if (!is.na(input$BothAge1_dose2)) {
          rate <- input$BothAge1_dose2 / input$BothAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
      
      output$error_9_total <- renderUI({
        if (is.na(input$BothAge1_total)) {
          helpText("This number is required", style = "color:red")
        } else {
          NULL
        }
      })
      output$error_9_gt1 <- renderUI({
        if (is.na(input$BothAge1_dose1) | is.na(input$BothAge1_total)) {
          return(NULL)
        } else if (input$BothAge1_dose1 > input$BothAge1_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_9_2plus <- renderUI({if (is.na(input$BothAge1_dose2) | is.na(input$BothAge1_total)) {
        return(NULL)
      } else if (input$BothAge1_dose2 > input$BothAge1_total) {
        helpText("This number can't be greater than the number of patients seen",
                 style = "color:red")
      }
      })
      output$error_9a_2plus <- renderUI({
        if (is.na(input$BothAge1_dose2) | is.na(input$BothAge1_dose1)) {
          return(NULL)
        } else if (input$BothAge1_dose2 >= input$BothAge1_dose1){
          helpText("This number can't be greater than the total number of patients who received the first dose",
                   style = "color:red")
        }
      })
      
    }
    
  }) # end the 9-10 observation
  
  # Create the ages 11-12 boxes
  observeEvent(list(input$Q28, input$Q29), {
    validate(need(input$Q28, ""))
    validate(need(input$Q29, ""))
    
    if ("11-12" %in% input$Q28 &
        input$Q29 == "Yes, we'll report separate data for male and female patients") {
      
      # Girls ages 11-12
      output$brates_11_12 <- renderUI({
        verticalLayout(
          splitLayout(
            box(
              h3("Vaccination Rates for Females Patients, Ages 11-12"),
              title = "Vaccination Rates for Female Patients, Ages 11-12",
              collapsible = T,
              solidHeader = T,
              status = "primary",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("female ", style = "color:blue"),
                  "medical patients ",
                  em("ages 11-12 ", style = "color:red"),
                  "your clinic saw in 2020?"
                )
              ),
              numericInput(
                "FemAge2_total",
                label = NULL,
                value = savedRates$FemAge2_total,
                min = 0,
                step = 1,
                width = '100%'
              ),
              uiOutput("error_f11_total"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge2_dose1",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge2_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge2_dose1_rate")
              ),
              uiOutput("error_f11_gt1"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge2_dose2",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge2_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge2_dose2_rate")
              ),
              uiOutput("error_f11_2plus"),
              uiOutput("error_f11a_2plus"),
              h5(strong("Meningococcal")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge2_mening",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge2_mening,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge2_mening_rate")
              ),
              uiOutput("error_f11_mening"),
              h5(strong("Tdap")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge2_tdap",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge2_tdap,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge2_tdap_rate")
              ),
              uiOutput("error_f11_Tdap"),
            ),
            box( # males aged 11-12
              h3("Males, ages 11-12"),
              title = "Males, ages 11-12",
              collapsible = T,
              solidHeader = T,
              status = "danger",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("male ", style = "color:blue"),
                  "medical patients ",
                  em("ages 11-12 ", style = "color:red"),
                  "your clinic saw in 2020?"
                )
              ),
              numericInput(
                "MenAge2_total",
                label = NULL,
                value = savedRates$MenAge2_total,
                min = 0,
                step = 1,
                width = '100%'
              ),
              uiOutput("error_m11_total"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge2_dose1",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge2_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge2_dose1_rate")
              ),
              uiOutput("error_m11_gt1"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge2_dose2",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge2_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge2_dose2_rate")
              ),
              uiOutput("error_m11_2plus"),
              uiOutput("error_m11a_2plus"),
              h5(strong("Meningococcal")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge2_mening",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge2_mening,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge2_mening_rate")
              ),
              uiOutput("error_m11_mening"),
              h5(strong("Tdap")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge2_tdap",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge2_tdap,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge2_tdap_rate")
              ),
              uiOutput("error_m11_Tdap"),
            )
          )) # end split and vertical layout
      })
      output$FemAge2_dose1_rate <- renderText({
        validate(need(input$FemAge2_total, ""))
        if (is.na(input$FemAge2_dose1)) {
          return(NULL)
        }
        if (!is.na(input$FemAge2_dose1)) {
          rate <- input$FemAge2_dose1 / input$FemAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemAge2_dose2_rate <- renderText({
        validate(need(input$FemAge2_total, ""))
        if (is.na(input$FemAge2_dose2)) {
          return(NULL)
        }
        if (!is.na(input$FemAge2_dose2)) {
          rate <- input$FemAge2_dose2 / input$FemAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemAge2_mening_rate <- renderText({
        validate(need(input$FemAge2_total, ""))
        if (is.na(input$FemAge2_mening)) {
          return(NULL)
        }
        if (!is.na(input$FemAge2_mening)) {
          rate <- input$FemAge2_mening / input$FemAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemAge2_tdap_rate <- renderText({
        validate(need(input$FemAge2_total, ""))
        if (is.na(input$FemAge2_tdap)) {
          return(NULL)
        }
        if (!is.na(input$FemAge2_tdap)) {
          rate <- input$FemAge2_tdap / input$FemAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      
      output$error_f11_total <- renderUI({
        if (is.na(input$FemAge2_total)) {
          helpText("This number is required", style = "color:red")
        } else {
          NULL
        }
      })
      output$error_f11_gt1 <- renderUI({
        if (is.na(input$FemAge2_dose1) | is.na(input$FemAge2_total)) {
          return(NULL)
        } else if (input$FemAge2_dose1 > input$FemAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_f11_2plus <- renderUI({if (is.na(input$FemAge2_dose2) | is.na(input$FemAge2_total)) {
        return(NULL)
      } else if (input$FemAge2_dose2 > input$FemAge2_total) {
        helpText("This number can't be greater than the number of patients seen",
                 style = "color:red")
      }
      })
      output$error_f11a_2plus <- renderUI({
        if (is.na(input$FemAge2_dose2) | is.na(input$FemAge2_dose1)) {
          return(NULL)
        } else if (input$FemAge2_dose2 >= input$FemAge2_dose1){
          helpText("This number can't be greater than the total number of patients who received the first dose",
                   style = "color:red")
        }
      })
      output$error_f11_mening <- renderUI({
        if (is.na(input$FemAge2_mening) | is.na(input$FemAge2_total)) {
          return(NULL)
        } else if (input$FemAge2_mening > input$FemAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_f11_Tdap <- renderUI({
        if (is.na(input$FemAge2_tdap) | is.na(input$FemAge2_total)) {
          return(NULL)
        } else if (input$FemAge2_tdap > input$FemAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      
      output$MenAge2_dose1_rate <- renderText({
        validate(need(input$MenAge2_total, ""))
        if (is.na(input$MenAge2_dose1)) {
          return(NULL)
        }
        if (!is.na(input$MenAge2_dose1)) {
          rate <- input$MenAge2_dose1 / input$MenAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenAge2_dose2_rate <- renderText({
        validate(need(input$MenAge2_total, ""))
        if (is.na(input$MenAge2_dose2)) {
          return(NULL)
        }
        if (!is.na(input$MenAge2_dose2)) {
          rate <- input$MenAge2_dose2 / input$MenAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenAge2_mening_rate <- renderText({
        validate(need(input$MenAge2_total, ""))
        if (is.na(input$MenAge2_mening)) {
          return(NULL)
        }
        if (!is.na(input$MenAge2_mening)) {
          rate <- input$MenAge2_mening / input$MenAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenAge2_Tdap_rate <- renderText({
        validate(need(input$MenAge2_total, ""))
        if (is.na(input$MenAge2_tdap)) {
          return(NULL)
        }
        if (!is.na(input$MenAge2_tdap)) {
          rate <- input$MenAge2_tdap / input$MenAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      
      output$error_m11_total <- renderUI({
        if (is.na(input$MenAge2_total)) {
          helpText("This number is required", style = "color:red")
        } else {
          NULL
        }
      })
      output$error_m11_gt1 <- renderUI({
        if (is.na(input$MenAge2_dose1) | is.na(input$MenAge2_total)) {
          return(NULL)
        } else if (input$MenAge2_dose1 > input$MenAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_m11_2plus <- renderUI({if (is.na(input$MenAge2_dose2) | is.na(input$MenAge2_total)) {
        return(NULL)
      } else if (input$MenAge2_dose2 > input$MenAge2_total) {
        helpText("This number can't be greater than the number of patients seen",
                 style = "color:red")
      }
      })
      output$error_m11a_2plus <- renderUI({
        if (is.na(input$MenAge2_dose2) | is.na(input$MenAge2_dose1)) {
          return(NULL)
        } else if (input$MenAge2_dose2 >= input$MenAge2_dose1){
          helpText("This number can't be greater than the total number of patients who received the first dose",
                   style = "color:red")
        }
      })
      output$error_m11_mening <- renderUI({
        if (is.na(input$MenAge2_mening) | is.na(input$MenAge2_total)) {
          return(NULL)
        } else if (input$MenAge2_mening > input$MenAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_m11_Tdap <- renderUI({
        if (is.na(input$MenAge2_tdap) | is.na(input$MenAge2_total)) {
          return(NULL)
        } else if (input$MenAge2_tdap > input$MenAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      
    }
    
    
    # Ages 11-12 - boys and girls combined
    if ("11-12" %in% input$Q28 &
        input$Q29 == "No, we will have combined rate data") {
      output$brates_11_12 <- renderUI({
        box(
          h3("Vaccination Rates for Male and Female Patients Combined, Ages 11-12"),
          title = "Vaccination Rates for Male and Female Patients Combined, Ages 11-12",
          collapsible = T,
          solidHeader = T,
          status = "success",
          collapsed = T,
          width = '100%',
          HTML("<br>"),
          h5(
            strong(
              "Total number of active medical patients ",
              em("ages 11-12 ", style = "color:red"),
              "your clinic saw in 2020?"
            )
          ),
          numericInput(
            "BothAge2_total",
            label = NULL,
            value = savedRates$BothAge2_total,
            min = 0,
            step = 1,
            width = '100%'
          ),
          uiOutput("error_11_total"),
          h5(
            strong(
              "Number of active patients who received at least one (1) HPV dose"
            )
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge2_dose1",
              label = NULL,
              min = 0,
              value = savedRates$BothAge2_dose1,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge2_dose1_rate")
          ),
          uiOutput("error_11_gt1"),
          h5(
            strong("Number of active patients who received both (2) HPV doses")
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge2_dose2",
              label = NULL,
              min = 0,
              value = savedRates$BothAge2_dose2,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge2_dose2_rate")
          ),
          uiOutput("error_11_2plus"),
          uiOutput("error_11a_2plus"),
          h5(strong("Meningococcal")),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge2_mening",
              label = NULL,
              min = 0,
              value = savedRates$BothAge2_mening,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge2_mening_rate")
          ),
          uiOutput("error_11_mening"),
          h5(strong("Tdap")),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge2_tdap",
              label = NULL,
              min = 0,
              value = savedRates$BothAge2_tdap,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge2_tdap_rate")
          ),
          uiOutput("error_11_Tdap"),
        )
      })
      output$BothAge2_dose1_rate <- renderText({
        validate(need(input$BothAge2_total, ""))
        if (is.na(input$BothAge2_dose1)) {
          return(NULL)
        }
        if (!is.na(input$BothAge2_dose1)) {
          rate <- input$BothAge2_dose1 / input$BothAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothAge2_dose2_rate <- renderText({
        validate(need(input$BothAge2_total, ""))
        if (is.na(input$BothAge2_dose2)) {
          return(NULL)
        }
        if (!is.na(input$BothAge2_dose2)) {
          rate <- input$BothAge2_dose2 / input$BothAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothAge2_mening_rate <- renderText({
        validate(need(input$BothAge2_total, ""))
        if (is.na(input$BothAge2_mening)) {
          return(NULL)
        }
        if (!is.na(input$BothAge2_mening)) {
          rate <- input$BothAge2_mening / input$BothAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothAge2_tdap_rate <- renderText({
        validate(need(input$BothAge2_total, ""))
        if (is.na(input$BothAge2_tdap)) {
          return(NULL)
        }
        if (!is.na(input$BothAge2_tdap)) {
          rate <- input$BothAge2_tdap / input$BothAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      
      output$error_11_total <- renderUI({
        if (is.na(input$BothAge2_total)) {
          helpText("This number is required", style = "color:red")
        } else {
          NULL
        }
      })
      output$error_11_gt1 <- renderUI({
        if (is.na(input$BothAge2_dose1) | is.na(input$BothAge2_total)) {
          return(NULL)
        } else if (input$BothAge2_dose1 > input$BothAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_11_2plus <- renderUI({if (is.na(input$BothAge2_dose2) | is.na(input$BothAge2_total)) {
        return(NULL)
      } else if (input$BothAge2_dose2 > input$BothAge2_total) {
        helpText("This number can't be greater than the number of patients seen",
                 style = "color:red")
      }
      })
      output$error_11a_2plus <- renderUI({
        if (is.na(input$BothAge2_dose2) | is.na(input$BothAge2_dose1)) {
          return(NULL)
        } else if (input$BothAge2_dose2 >= input$BothAge2_dose1){
          helpText("This number can't be greater than the total number of patients who received the first dose",
                   style = "color:red")
        }
      })
      output$error_11_mening <- renderUI({
        if (is.na(input$BothAge2_mening) | is.na(input$BothAge2_total)) {
          return(NULL)
        } else if (input$BothAge2_mening > input$BothAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_11_Tdap <- renderUI({
        if (is.na(input$BothAge2_tdap) | is.na(input$BothAge2_total)) {
          return(NULL)
        } else if (input$BothAge2_tdap > input$BothAge2_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      
      
    }
    
  }) # end the 11-12 observation
  
  # Age 13 boxes
  observeEvent(list(input$Q28, input$Q29), {
    validate(need(input$Q28, ""))
    validate(need(input$Q29, ""))
    
    if ("13" %in% input$Q28 &
        input$Q29 == "Yes, we'll report separate data for male and female patients") {
      
      # Girls age 13
      output$brates_13 <- renderUI({
        verticalLayout(
          splitLayout(
            box(
              h3("Vaccination Rates for Female Patients, Age 13"),
              title = "Vaccination Rates for Female Patients, Age 13",
              collapsible = T,
              solidHeader = T,
              status = "primary",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("female ", style = "color:blue"),
                  "medical patients ",
                  em("age 13 ", style = "color:red"),
                  "your clinic saw in 2020?"
                )
              ),
              numericInput(
                "FemAge3_total",
                label = NULL,
                value = savedRates$FemAge3_total,
                min = 0,
                step = 1,
                width = '100%'
              ),
              uiOutput("error_f13_total"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge3_dose1",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge3_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge3_dose1_rate")
              ),
              uiOutput("error_f13_gt1"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge3_dose2",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge3_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge3_dose2_rate")
              ),
              uiOutput("error_f13_2plus"),
              uiOutput("error_f13a_2plus"),
              h5(strong("Meningococcal")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge3_mening",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge3_mening,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge3_mening_rate")
              ),
              uiOutput("error_f13_mening"),
              h5(strong("Tdap")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge3_tdap",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge3_tdap,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge3_tdap_rate")
              ),
              uiOutput("error_f13_Tdap"),
            ),
            box( # boys aged 13
              h3("Males, age 13"),
              title = "Males, age 13",
              collapsible = T,
              solidHeader = T,
              status = "danger",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("male ", style = "color:blue"),
                  "medical patients ",
                  em("age 13 ", style = "color:red"),
                  "your clinic saw in 2020?"
                )
              ),
              numericInput(
                "MenAge3_total",
                label = NULL,
                value = savedRates$MenAge3_total,
                min = 0,
                step = 1,
                width = '100%'
              ),
              uiOutput("error_m13_total"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge3_dose1",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge3_total,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge3_dose1_rate")
              ),
              uiOutput("error_m13_gt1"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge3_dose2",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge3_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge3_dose2_rate")
              ),
              uiOutput("error_m13_2plus"),
              uiOutput("error_m13a_2plus"),
              h5(strong("Meningococcal")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge3_mening",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge3_mening,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge3_mening_rate")
              ),
              uiOutput("error_m13_mening"),
              h5(strong("Tdap")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge3_tdap",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge3_tdap,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge3_tdap_rate")
              ),
              uiOutput("error_m13_Tdap"),
            )
          ))
      })
      output$FemAge3_dose1_rate <- renderText({
        validate(need(input$FemAge3_total, ""))
        if (is.na(input$FemAge3_dose1)) {
          return(NULL)
        }
        if (!is.na(input$FemAge3_dose1)) {
          rate <- input$FemAge3_dose1 / input$FemAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemAge3_dose2_rate <- renderText({
        validate(need(input$FemAge3_total, ""))
        if (is.na(input$FemAge3_dose2)) {
          return(NULL)
        }
        if (!is.na(input$FemAge3_dose2)) {
          rate <- input$FemAge3_dose2 / input$FemAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemAge3_mening_rate <- renderText({
        validate(need(input$FemAge3_total, ""))
        if (is.na(input$FemAge3_mening)) {
          return(NULL)
        }
        if (!is.na(input$FemAge3_mening)) {
          rate <- input$FemAge3_mening / input$FemAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemAge3_Tdap_rate <- renderText({
        validate(need(input$FemAge3_total, ""))
        if (is.na(input$FemAge3_tdap)) {
          return(NULL)
        }
        if (!is.na(input$FemAge3_tdap)) {
          rate <- input$FemAge3_tdap / input$FemAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      
      output$error_f13_total <- renderUI({
        if (is.na(input$FemAge3_total)) {
          helpText("This number is required", style = "color:red")
        } else {
          NULL
        }
      })
      output$error_f13_gt1 <- renderUI({
        if (is.na(input$FemAge3_dose1) | is.na(input$FemAge3_total)) {
          return(NULL)
        } else if (input$FemAge3_dose1 > input$FemAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_f13_2plus <- renderUI({if (is.na(input$FemAge3_dose2) | is.na(input$FemAge3_total)) {
        return(NULL)
      } else if (input$FemAge3_dose2 > input$FemAge3_total) {
        helpText("This number can't be greater than the number of patients seen",
                 style = "color:red")
      }
      })
      output$error_f13a_2plus <- renderUI({
        if (is.na(input$FemAge3_dose2) | is.na(input$FemAge3_dose1)) {
          return(NULL)
        } else if (input$FemAge3_dose2 >= input$FemAge3_dose1){
          helpText("This number can't be greater than the total number of patients who received the first dose",
                   style = "color:red")
        }
      })
      output$error_f13_mening <- renderUI({
        if (is.na(input$FemAge3_mening) | is.na(input$FemAge3_total)) {
          return(NULL)
        } else if (input$FemAge3_mening > input$FemAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_f13_Tdap <- renderUI({
        if (is.na(input$FemAge3_tdap) | is.na(input$FemAge3_total)) {
          return(NULL)
        } else if (input$FemAge3_tdap > input$FemAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      
      
      output$MenAge3_dose1_rate <- renderText({
        validate(need(input$MenAge3_total, ""))
        if (is.na(input$MenAge3_dose1)) {
          return(NULL)
        }
        if (!is.na(input$MenAge3_dose1)) {
          rate <- input$MenAge3_dose1 / input$MenAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenAge3_dose2_rate <- renderText({
        validate(need(input$MenAge3_total, ""))
        if (is.na(input$MenAge3_dose2)) {
          return(NULL)
        }
        if (!is.na(input$MenAge3_dose2)) {
          rate <- input$MenAge3_dose2 / input$MenAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenAge3_mening_rate <- renderText({
        validate(need(input$MenAge3_total, ""))
        if (is.na(input$MenAge3_mening)) {
          return(NULL)
        }
        if (!is.na(input$MenAge3_mening)) {
          rate <- input$MenAge3_mening / input$MenAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenAge3_tdap_rate <- renderText({
        validate(need(input$MenAge3_total, ""))
        if (is.na(input$MenAge3_tdap)) {
          return(NULL)
        }
        if (!is.na(input$MenAge3_tdap)) {
          rate <- input$MenAge3_tdap / input$MenAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      
      
      
      
      output$error_m13_total <- renderUI({
        if (is.na(input$MenAge3_total)) {
          helpText("This number is required", style = "color:red")
        } else {
          NULL
        }
      })
      output$error_m13_gt1 <- renderUI({
        if (is.na(input$MenAge3_dose1) | is.na(input$MenAge3_total)) {
          return(NULL)
        } else if (input$MenAge3_dose1 > input$MenAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_m13_2plus <- renderUI({if (is.na(input$MenAge3_dose2) | is.na(input$MenAge3_total)) {
        return(NULL)
      } else if (input$MenAge3_dose2 > input$MenAge3_total) {
        helpText("This number can't be greater than the number of patients seen",
                 style = "color:red")
      }
      })
      output$error_m13a_2plus <- renderUI({
        if (is.na(input$MenAge3_dose2) | is.na(input$MenAge3_dose1)) {
          return(NULL)
        } else if (input$MenAge3_dose2 >= input$MenAge3_dose1){
          helpText("This number can't be greater than the total number of patients who received the first dose",
                   style = "color:red")
        }
      })
      output$error_m13_mening <- renderUI({
        if (is.na(input$MenAge3_mening) | is.na(input$MenAge3_total)) {
          return(NULL)
        } else if (input$MenAge3_mening > input$MenAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_m13_Tdap <- renderUI({
        if (is.na(input$MenAge3_tdap) | is.na(input$MenAge3_total)) {
          return(NULL)
        } else if (input$MenAge3_tdap > input$MenAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      
    }
    
    # Boys and girls combined - age 13
    if ("13" %in% input$Q28 &
        input$Q29 == "No, we will have combined rate data") {
      output$brates_13 <- renderUI({
        box(
          h3("Vaccination Rates for Male and Female Patients Combined, Age 13"),
          title = "Vaccination Rates for Male and Female Patients Combined, Age 13",
          collapsible = T,
          solidHeader = T,
          status = "success",
          collapsed = T,
          width = '100%',
          HTML("<br>"),
          h5(
            strong(
              "Total number of active medical patients ",
              em("age 13 ", style = "color:red"),
              "your clinic saw in 2020?"
            )
          ),
          numericInput(
            "BothAge3_total",
            label = NULL,
            value = savedRates$BothAge3_total,
            min = 0,
            step = 1,
            width = '100%'
          ),
          uiOutput("error_13_total"),
          h5(
            strong(
              "Number of active patients who received at least one (1) HPV dose"
            )
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge3_dose1",
              label = NULL,
              min = 0,
              value = savedRates$BothAge3_dose1,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge3_dose1_rate")
          ),
          uiOutput("error_13_gt1"),
          h5(
            strong("Number of active patients who received both (2) HPV doses")
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge3_dose2",
              label = NULL,
              min = 0,
              value = savedRates$BothAge3_dose2,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge3_dose2_rate")
          ),
          uiOutput("error_13_2plus"),
          uiOutput("error_13a_2plus"),
          h5(strong("Meningococcal")),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge3_mening",
              label = NULL,
              min = 0,
              value = savedRates$BothAge3_mening,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge3_mening_rate")
          ),
          uiOutput("error_13_mening"),
          h5(strong("Tdap")),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge3_tdap",
              label = NULL,
              min = 0,
              value = savedRates$BothAge3_tdap,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge3_tdap_rate")
          ),
          uiOutput("error_13_Tdap"),
        )
      })
      output$BothAge3_dose1_rate <- renderText({
        validate(need(input$BothAge3_total, ""))
        if (is.na(input$BothAge3_dose1)) {
          return(NULL)
        }
        if (!is.na(input$BothAge3_dose1)) {
          rate <- input$BothAge3_dose1 / input$BothAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothAge3_dose2_rate <- renderText({
        validate(need(input$BothAge3_total, ""))
        if (is.na(input$BothAge3_dose2)) {
          return(NULL)
        }
        if (!is.na(input$BothAge3_dose2)) {
          rate <- input$BothAge3_dose2 / input$BothAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothAge3_mening_rate <- renderText({
        validate(need(input$BothAge3_total, ""))
        if (is.na(input$BothAge3_mening)) {
          return(NULL)
        }
        if (!is.na(input$BothAge3_mening)) {
          rate <- input$BothAge3_mening / input$BothAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothAge3_tdap_rate <- renderText({
        validate(need(input$BothAge3_total, ""))
        if (is.na(input$BothAge3_tdap)) {
          return(NULL)
        }
        if (!is.na(input$BothAge3_tdap)) {
          rate <- input$BothAge3_tdap / input$BothAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      
      output$error_13_total <- renderUI({
        if (is.na(input$BothAge3_total)) {
          helpText("This number is required", style = "color:red")
        } else {
          NULL
        }
      })
      output$error_13_gt1 <- renderUI({
        if (is.na(input$BothAge3_dose1) | is.na(input$BothAge3_total)) {
          return(NULL)
        } else if (input$BothAge3_dose1 > input$BothAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_13_2plus <- renderUI({if (is.na(input$BothAge3_dose2) | is.na(input$BothAge3_total)) {
        return(NULL)
      } else if (input$BothAge3_dose2 > input$BothAge3_total) {
        helpText("This number can't be greater than the number of patients seen",
                 style = "color:red")
      }
      })
      output$error_13a_2plus <- renderUI({
        if (is.na(input$BothAge3_dose2) | is.na(input$BothAge3_dose1)) {
          return(NULL)
        } else if (input$BothAge3_dose2 >= input$BothAge3_dose1){
          helpText("This number can't be greater than the total number of patients who received the first dose",
                   style = "color:red")
        }
      })
      output$error_13_mening <- renderUI({
        if (is.na(input$BothAge3_mening) | is.na(input$BothAge3_total)) {
          return(NULL)
        } else if (input$BothAge3_mening > input$BothAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      output$error_13_Tdap <- renderUI({
        if (is.na(input$BothAge3_tdap) | is.na(input$BothAge3_total)) {
          return(NULL)
        } else if (input$BothAge3_tdap > input$BothAge3_total) {
          helpText("This number can't be greater than the number of patients seen",
                   style = "color:red")
        }
      })
      
    }
  }) # end the 13 observation
  
  output$Q30 <- renderUI({
    textAreaInput(
      inputId = "Q30",
      label = "4.  Please share anything else about your baseline data you'd like us to know.",
      value =  savedRates$Q30,
      rows = 4)
  })
  
  output$Q31 <- renderUI({
    selectInput(inputId = "Q31",
                label = "5.  What was the primary data source used to calculate your vaccination rates?",
                choices =c("",
                           "EHR (preferred)",
                           "Chart Audit",
                           "State Immunization Registry",
                           "Other (specify)"),
                selectize = F,
                selected = savedRates$Q31,
                width = "100%")
  })
  output$Q31_other <- renderUI({
    validate(need(input$Q31, ""))
    if ("Other (specify)" %in% input$Q31) {
      textInput("Q31_other",
                label = "Please specify",
                value = savedRates$Q31_other,
                width = "100%")
    }
  })
  
  output$Q32 <- renderUI({
    selectInput(
      inputId = "Q32",
      label = "6.  Did you use a secondary data source?",
      choices = c("", "Yes", "No"),
      selected = savedRates$Q32,
      selectize = T)
  })
  
  # Q32 - group textbox
  output$Q32_details <- renderUI({
    validate(need(input$Q32, ""))
    if (input$Q32 == "Yes") {
      checkboxGroupInput(
        inputId = "Q32_details",
        label = "6a. What secondary sources were used (select all that apply)?",
        choices = c( "EHR",
                     "Chart Audit",
                     "State Immunization Registry",
                     "Other (specify)"),
        selected = foo32)
    }
  })
  output$Q32_other <- renderUI({
    validate(need(input$Q32_details, ""))
    if ("Other (specify)" %in% input$Q32_details) {
      textInput("Q32_other",
                label = "Please specify",
                value = savedRates$Q32_other,
                width = "100%")
    }
  })
  output$Q33 <- renderUI({
    selectInput(
      inputId = "Q33",
      label = "7.  Were you unable to report any of the requested data?",
      choices = c("", "Yes", "No"),
      selected = savedRates$Q33,
      selectize = T)
  })
  output$Q33_other <- renderUI({
    validate(need(input$Q33, ""))
    if (input$Q33 == "Yes") {
      textAreaInput("Q33_other",
                    label = "7a. Describe what you were unable to report and why.",
                    value = savedRates$Q33_other,
                    rows = 4)
    }
  })
  output$Q34 <- renderUI({
    selectInput(
      inputId = "Q34",
      label = "8.  Are you working on your reporting capacity and anticipate submitting updated data in the next few months?",
      choices = c("", "Yes", "No"),
      selected = savedRates$Q34,
      selectize = T,
      width = "100%")
  })
  
  
  
  
  # ---- RATES ----- #
  # Create the static table that people will use as reference
  output$static_table <- renderTable({
    q <-
      data.frame(rbind(
        c("AGES", "BORN", "EVER RECEIVED THE FOLLOWING VACCINES"),
        c("9-10", "2010-2011", "At least 1 dose of HPV; 2 doses of HPV"),
        c("11-12","2008-2009", "At least 1 dose of HPV; 2 doses of HPV; Meningococcal; Tdap"),
        c("13", "2007",  "At least 1 dose of HPV; 2 doses of HPV; Meningococcal; Tdap")
      ))
  }, striped = FALSE, align = "l", colnames = FALSE, rownames = FALSE, bordered =
    TRUE, spacing = "m", width = "70%")
  
    saveData <- reactive({
      
    input$downloadData
    
    healthSystem <- input$healthSystem
    
    # Vaccination rates
    Q27 <- input$Q27
    Q27_other <- ifelse(is.null(input$Q27_other), "", input$Q27_other)
    
    # Q28 Groupbox text input
    Q28 <-  ifelse(is.null(input$Q28), "", paste(input$Q28, collapse = ", "))
    answers <- c("9-10", "11-12",  "13",
                 "We can't report on these ages (other, please specify)?")
    Q28_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q28, fixed = T)) == 1, x, NA)
    })
    Q28_1 <- as.character(Q28_responses[1])
    Q28_2 <- as.character(Q28_responses[2])
    Q28_3 <- as.character(Q28_responses[3])
    Q28_4 <- as.character(Q28_responses[4])
    rm(Q28_responses, answers)
    
    Q28_other <- ifelse(is.null(input$Q28_other), "", input$Q28_other)
    Q29 <- input$Q29
    Q30 <- input$Q30
    
    # Rate variables
    
    # Girls - ages 9-10
    FemAge1_total <- ifelse(is.null(input$FemAge1_total), NA, input$FemAge1_total)
    FemAge1_dose1 <- ifelse(is.null(input$FemAge1_dose1), NA, input$FemAge1_dose1)
    FemAge1_dose2 <- ifelse(is.null(input$FemAge1_dose2), NA, input$FemAge1_dose2)
    # Boys - ages 9-10
    MenAge1_total <- ifelse(is.null(input$MenAge1_total), NA, input$MenAge1_total)
    MenAge1_dose1 <- ifelse(is.null(input$MenAge1_dose1), NA, input$MenAge1_dose1)
    MenAge1_dose2 <- ifelse(is.null(input$MenAge1_dose2), NA, input$MenAge1_dose2)
    # Both - ages 9-10
    BothAge1_total <- ifelse(is.null(input$BothAge1_total), NA, input$BothAge1_total)
    BothAge1_dose1 <- ifelse(is.null(input$BothAge1_dose1), NA, input$BothAge1_dose1)
    BothAge1_dose2 <- ifelse(is.null(input$BothAge1_dose2), NA, input$BothAge1_dose2)
    
    # Girls - ages 11-12
    FemAge2_total <- ifelse(is.null(input$FemAge2_total), NA, input$FemAge2_total)
    FemAge2_dose1 <- ifelse(is.null(input$FemAge2_dose1), NA, input$FemAge2_dose1)
    FemAge2_dose2 <- ifelse(is.null(input$FemAge2_dose2), NA, input$FemAge2_dose2)
    FemAge2_mening <- ifelse(is.null(input$FemAge2_mening), NA, input$FemAge2_mening)
    FemAge2_tdap <- ifelse(is.null(input$FemAge2_tdap), NA, input$FemAge2_tdap)
    
    # Boys - ages 11-12
    MenAge2_total <- ifelse(is.null(input$MenAge2_total), NA, input$MenAge2_total)
    MenAge2_dose1 <- ifelse(is.null(input$MenAge2_dose1), NA, input$MenAge2_dose1)
    MenAge2_dose2 <- ifelse(is.null(input$MenAge2_dose2), NA, input$MenAge2_dose2)
    MenAge2_mening <- ifelse(is.null(input$MenAge2_mening), NA, input$MenAge2_mening)
    MenAge2_tdap <- ifelse(is.null(input$MenAge2_tdap), NA, input$MenAge2_tdap)
    
    # Both - ages 11-12
    BothAge2_total <- ifelse(is.null(input$BothAge2_total), NA, input$BothAge2_total)
    BothAge2_dose1 <- ifelse(is.null(input$BothAge2_dose1), NA, input$BothAge2_dose1)
    BothAge2_dose2 <- ifelse(is.null(input$BothAge2_dose2), NA, input$BothAge2_dose2)
    BothAge2_mening <- ifelse(is.null(input$BothAge2_mening), NA, input$BothAge2_mening)
    BothAge2_tdap <- ifelse(is.null(input$BothAge2_tdap), NA, input$BothAge2_tdap)
    
    # Girls - ages 13
    FemAge3_total <-  ifelse(is.null(input$FemAge3_total), NA, input$FemAge3_total)
    FemAge3_dose1 <-  ifelse(is.null(input$FemAge3_dose1), NA, input$FemAge3_dose1)
    FemAge3_dose2 <- ifelse(is.null(input$FemAge3_dose2), NA, input$FemAge3_dose2)
    FemAge3_mening <- ifelse(is.null(input$FemAge3_mening), NA, input$FemAge3_mening)
    FemAge3_tdap <-  ifelse(is.null(input$FemAge3_tdap), NA, input$FemAge3_tdap)
    # Boys - ages 13
    MenAge3_total <- ifelse(is.null(input$MenAge3_total), NA, input$MenAge3_total)
    MenAge3_dose1 <- ifelse(is.null(input$MenAge3_dose1), NA, input$MenAge3_dose1)
    MenAge3_dose2 <- ifelse(is.null(input$MenAge3_dose2), NA, input$MenAge3_dose2)
    MenAge3_mening <- ifelse(is.null(input$MenAge3_mening), NA, input$MenAge3_mening)
    MenAge3_tdap <- ifelse(is.null(input$MenAge3_tdap), NA, input$MenAge3_tdap)
    
    # Both - ages 13
    BothAge3_total <- ifelse(is.null(input$BothAge3_total), NA, input$BothAge3_total)
    BothAge3_dose1 <- ifelse(is.null(input$BothAge3_dose1), NA, input$BothAge3_dose1)
    BothAge3_dose2 <- ifelse(is.null(input$BothAge3_dose2), NA, input$BothAge3_dose2)
    BothAge3_mening <- ifelse(is.null(input$BothAge3_mening), NA, input$BothAge3_mening)
    BothAge3_tdap <- ifelse(is.null(input$BothAge3_tdap), NA, input$BothAge3_tdap)
    
    # Create a data frame of rates, impute the BothAgexxx variables if needed
    baselineRates <- data.frame(
      FemAge1_total, FemAge1_dose1, FemAge1_dose2,
      MenAge1_total, MenAge1_dose1, MenAge1_dose2,
      BothAge1_total, BothAge1_dose1,  BothAge1_dose2,
      FemAge2_total, FemAge2_dose1, FemAge2_dose2, FemAge2_mening,  FemAge2_tdap,
      MenAge2_total, MenAge2_dose1, MenAge2_dose2, MenAge2_mening, MenAge2_tdap,
      BothAge2_total, BothAge2_dose1, BothAge2_dose2, BothAge2_mening, BothAge2_tdap,
      FemAge3_total, FemAge3_dose1, FemAge3_dose2, FemAge3_mening, FemAge3_tdap,
      MenAge3_total, MenAge3_dose1,  MenAge3_dose2, MenAge3_mening, MenAge3_tdap,
      BothAge3_total, BothAge3_dose1,  BothAge3_dose2, BothAge3_mening, BothAge3_tdap)
    
    # make sure all are numeric
    baselineRates[, names(baselineRates)] <-
      lapply(baselineRates[, names(baselineRates)], as.numeric)
    
    rm( FemAge1_total, FemAge1_dose1, FemAge1_dose2,
        MenAge1_total, MenAge1_dose1, MenAge1_dose2,
        BothAge1_total, BothAge1_dose1,  BothAge1_dose2,
        FemAge2_total, FemAge2_dose1, FemAge2_dose2, FemAge2_mening,  FemAge2_tdap,
        MenAge2_total, MenAge2_dose1, MenAge2_dose2, MenAge2_mening, MenAge2_tdap,
        BothAge2_total, BothAge2_dose1, BothAge2_dose2, BothAge2_mening, BothAge2_tdap,
        FemAge3_total, FemAge3_dose1, FemAge3_dose2, FemAge3_mening, FemAge3_tdap,
        MenAge3_total, MenAge3_dose1,  MenAge3_dose2, MenAge3_mening, MenAge3_tdap,
        BothAge3_total, BothAge3_dose1,  BothAge3_dose2, BothAge3_mening, BothAge3_tdap)
    
    # Reporting section
    Q31 <- input$Q31
    Q31_other <- ifelse(is.null(input$Q31_other), "", input$Q31_other)
    Q32 <- input$Q32
    
    # Q32 - group textb ox
    Q32_details <- ifelse(is.null(input$Q32_details), "", paste(input$Q32_details, collapse = ", "))
    answers <- c("EHR", "Chart Audit", "State Immunization Registry", "Other (specify)")
    Q32_details_responses <- sapply(answers, function(x) {
      ifelse(length(grep(x, input$Q32_details, fixed = T)) == 1, x, NA)
    })
    Q32_details1 <- as.character(Q32_details_responses[1])
    Q32_details2 <- as.character(Q32_details_responses[2])
    Q32_details3 <- as.character(Q32_details_responses[3])
    Q32_details4 <- as.character(Q32_details_responses[4])
    rm(Q32_details_responses, answers)
    
    Q32_other <- ifelse(is.null(input$Q32_other), "", input$Q32_other)
    Q33 <- input$Q33
    Q33_other <- ifelse(is.null(input$Q33_other), "", input$Q33_other)
    Q34 <- input$Q34
    
    
    savedRates <- data.frame(healthSystem,
      Q27, Q27_other, Q28, Q28_1, Q28_2, Q28_3, Q28_4, Q28_other,
                             Q29, 
                             # Rates - numbers
                             baselineRates, Q30,
                             Q31, Q31_other, Q32, Q32_details, Q32_details1, Q32_details2, Q32_details3, Q32_details4,
                             Q32_other, Q33, Q33_other, Q34)
    
    dbWriteTable(myDB, "rates", savedRates, overwrite = T)
    storage_upload(DataSrc, userFilename)
    
    names(savedRates) <- c("HealthSystem", "Q1", "Q1_Other", "Q2",
                           "q2_drop1", "Q2_drop2", "q2_drop3", "q2_drop4",
                           "Q2_other", "Q3",names(baselineRates), "Q4", "Q5",
                           "Q5_other", "Q6", 
                           "Q6_details",	"Q6_drop1",	"Q6_drop2",	"Q6_drop3",	"Q6_drop4",
                           "Q6_other",	"Q7",	"Q7_other",	"Q8")
    savedRates2 <- savedRates[,c("HealthSystem", "Q1", "Q1_Other", "Q2",
                  "Q3", names(baselineRates), "Q4", "Q5",
                  "Q5_other", "Q6", "Q6_details",
                  "Q7", "Q7_other", "Q8")]
    
    return(savedRates2)
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(paste0("HPV Vaccination rates - ", Sys.Date()), ".xlsx", sep = "")
    },
    content = function(file) {
      WriteXLS(saveData(), file, AllText = T)
      #write.csv(saveData(), file, row.names = FALSE, na = "")
    }
  )
  

  
  

  
  
  
 
  
  
} # end the server


shinyApp(ui=ui, server=server)


