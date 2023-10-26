################################################################################
# sidebar.R
# 
# Create the sidebar menu options for the ui.
################################################################################

library(shinydashboard)

sidebar <- dashboardSidebar(
  width = 180,
  tags$style(
    HTML(
      "
        .shiny-input-container > label {margin-bottom: -15px;}
        .sidebar-menu li a {padding-top: 5px; padding-bottom: 4px;}
      "
    )
  ),
  sidebarMenu(
    # menuItem("Overview", tabName = "overview"),
    menuItem("No. of Patients", tabName = "patient"),
    menuItem("Severity", tabName = "severe"),
    menuItem("Complication", tabName = "complicate"),
    menuItem("Infections", tabName = "infect"),
    menuItem("Treatments", tabName = "treat"),
    menuItem("Depression", tabName = "depress")
    # menuItem("Vaccination", tabName = "vac"),
    # menuItem("ATK Used", tabName = "atk"),
    # menuItem("SARS-CoV-2 Detection", tabName = "detect"),
    # menuItem("Laboratory", tabName = "lab"),
    # menuItem("Serology Testing", tabName = "sero"),
    # menuItem("KAP", tabName = "KAP")
  )
  # tags$div(id = "div_test", tags$style("#div_test div {margin-bottom: -2px}"),
  #          selectInput(
  #            inputId = "province",
  #            label = "Province:",
  #            choices = c("All", as.character(unique(df_scrgender$province)))
  #          )),
  #         selectInput(
  #           inputId = "hospital",
  #           label = "Hospital:",
  #           choices = c("All", as.character(unique(df_scrgender$hospital)))
  #         ),
         

  # dateInput("dateto", "Date From:" ),
  # dateInput("datefrom", "Date To:" )

  
  )

