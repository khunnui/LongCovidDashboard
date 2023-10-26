################################################################################
# body.R
#
# Create the body for the ui.
################################################################################

library(shinydashboard)
library(plotly)
library(DT)
library(gt)
library(shinyjs)

body <- dashboardBody(
  tags$head(tags$style(
    HTML(
      "div.box-header {text-align: center;}
            .skin-black .main-sidebar {background-color: #0B2C4B;}
            .myClass {font-size: 14px; text-align: right; margin-top: 17px; margin-right: 20px;}
            .small-box.bg-blue   { background-color: #a1caf1 !important; color: #000000 !important}
            .small-box.bg-green  { background-color: #ace1af !important; color: #000000 !important}
            .small-box.bg-orange { background-color: #B78F62 !important; color: #000000 !important}
            .small-box.bg-yellow { background-color: #f8de7e !important; color: #000000 !important}"
    )
  )),
  tags$script(
    HTML(
      '$(document).ready(function()
      {$("header").find("nav").append(\'<div id="dateHeader" class="myClass"></div>\');})'
    )
  ),
  tabItems(
    tabItem(tabName = "patient",
            fluidRow(
              box(title = "No. of Patients in Each Visit",
                  width = 12,
                  gt_output("enroll"))
            )),
    
    tabItem(tabName = "severe",
            fluidRow(
              box(title = "Severity of Acute Infection",
                  width = 12,
                  gt_output("severe"))
            )),
    tabItem(tabName = "complicate",
            fluidRow(
              box(title = "Complications during Acute COVID-19 Infection",
                  width = 12,
                  gt_output("complicate"))
            )),
    tabItem(tabName = "infect",
            fluidRow(
              box(title = "Diagnosed with Other Infection during Acute COVID-19 Infection",
                  width = 12,
                  gt_output("infect"))
            )),
    tabItem(tabName = "treat",
            fluidRow(
              box(title = "Clinical Management during Acute COVID-19 Infection",
                  width = 12,
                  gt_output("treat"))
          )),
    tabItem(tabName = "depress",
            fluidRow(
              box(title = "Depression Severity",
                  width = 12,
                  gt_output("depress"))
            ))
    
    
    
  ),
  useShinyjs()
)
