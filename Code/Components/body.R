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
    tabItem(
      tabName = "overview",
      fluidRow(
        # style = "margin-top: -5px; margin-bottom: -20px; margin-left: -20px; margin-right: -20px;",
        box(
          title = HTML("Prospective Study of Long COVID Symptoms Among Patients with COVID-19 Identified from COVID Fever Project"),
          width = 12,
          div(
            style = "text-align: center; font-size: 20px",
            h4("Objectives")
          ),
          HTML("<ul>
                  <li>To describe the proportion of patients with post-COVID conditions. </li>
                  <li>To better understand the most frequent symptoms and diagnoses experienced by patients with post-COVID conditions.</li>
                 </ul>"
          )
        ),
        box(
          title = "Project summary",
          width = 12,
          div(
            style = "text-align: left; font-size: 20px; font-weight: bold",
              ),
          HTML("<ul>
                  <li>Patients with SARS-CoV-2 RT-PCR positive identified from COVID Fever study in Nakhon Phanom and Tak during October 2022 – May 2023 were interviewed for health status, hospital visit after acute illness, ability to self-care and clinical characteristics related to COVID-19.  </li>
                  <li>Demographic, clinical presentation, laboratory, diagnoses, treatment information, vaccination history and reinfection with SARS-CoV-2 were collected from medical records and hospital databases. </li>
                  <li>Patient interview and medical record review were conducted every 3 months until the completion of the 1-year follow-up period.</li>
                </ul>"
          )
          
        )
        
      )
    ),
    
    tabItem(tabName = "patient",
            fluidRow(
              box(title = "COVID-19 Patients Under Follow-up by Enrollment Date (N=910)",
                  width = 12,
                  gt_output("enroll1"))
            ),
            fluidRow(
              box(title = "COVID-19 Patients Under Follow-up by Enrollment Date and Provinces",
                  width = 12,
                  gt_output("enroll2"))
            )
            
            ),
    
    tabItem(tabName = "severe",
            fluidRow(
              box(title = "Level of Clinical Severity during Acute COVID-19 Infection",
                  width = 12,
                  gt_output("severe")),
              box(
                title = "",
                width = 12,
                div(
                  style = "text-align: left; font-size: 20px; font-weight: bold",
                ),
                HTML("<table border=1>
    <thead>
        <tr style='vertical-align:top'>
            <th>Clinical Classification</th>
            <th>Based on available clinical records among hospitalized patients</th>
            <th>Based on self-report, if patient was quarantined at home or community isolation</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td bgcolor = 'CCFFCC'>Mild</td>
            <td bgcolor = 'CCFFCC'>Present with a variety of signs and symptoms e.g., fever, cough, sore throat, malaise, headache, muscle pain, nausea, vomiting, diarrhea, loss of taste and smell, but No shortness of breath, dyspnea on exertion, or abnormal imaging. No hypoxia or pneumonia</td>
            <td bgcolor = 'CCFFCC'>Did not receive oxygen</td>
        </tr>
        <tr>
            <td bgcolor = '99FFCC'>Moderate</td>
            <td bgcolor = '99FFCC'>Clinical signs of lower respiratory disease during clinical assessment or imaging, with SpO2 ≥94% on room air</td>
            <td bgcolor = '99FFCC'>Did not receive oxygen</td>
        </tr>
        <tr>
            <td bgcolor = '66FFB2'>Severe</td>
            <td bgcolor = '66FFB2'>Individuals who have SpO2 &lt;94% on room air at sea level, a ratio of arterial partial pressure of oxygen to fraction of inspired oxygen (PaO2/FiO2) &lt;300 mm Hg, a respiratory rate &gt;30 breaths/min, or lung infiltrates &gt;50%</td>
            <td bgcolor = '66FFB2'>Received oxygen<br>(or told you they needed it, but it was not available)</td>
        </tr>
        <tr>
            <td bgcolor = '33FF99'>Critical</td>
            <td bgcolor = '33FF99'>Individuals who have respiratory failure, septic shock, and/or multiple organ dysfunction OR Multi-Inflammatory Syndrome in Children (MISC) and adolescents temporally related to COVID-19</td>
            <td bgcolor = '33FF99'>Received invasive ventilation (or max available respiratory support)</td>
        </tr>
    </tbody>
</table>"
                )
                
              )
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
              box(title = "Depression Severity at Baseline and Follow-up (All patients in each period)",
                  width = 12,
                  plotlyOutput("depress"))
            )
            ,
            fluidRow(
              box(title = "Depression Severity at Baseline and Follow-up (Based on baseline patients)",
                  width = 12,
                  plotlyOutput("depress2"))
            )
            ),
    
    tabItem(tabName = "fu",
            fluidRow(
              box(title = "Number Hospital Visit during Follow-up Period",
                  width = 12,
                  align="center",
                  plotlyOutput("fu"),
              radioButtons(
                "provx",
                label = "",
                inline = TRUE,
                choices = list("All" = 1, "Nakorn Phanom" = 2, "Tak" = 3), 
                selected = 1
              ),
              radioButtons(
                "compx",
                label = "",
                inline = TRUE,
                choices = list("All" = 1, "Completed F/U" = 2), 
                selected = 1
              ))
            )),
    
    tabItem(tabName = "reinfect",
            fluidRow(
              box(title = "Patient with PCR or ATK Positive after Acute Infection",
                  width = 12,
                  align="center",
                  plotlyOutput("reinfect"),
                  radioButtons(
                    "provrf",
                    label = "",
                    inline = TRUE,
                    choices = list("All" = 1, "Nakorn Phanom" = 2, "Tak" = 3), 
                    selected = 1
                  ),
                  radioButtons(
                    "comprf",
                    label = "",
                    inline = TRUE,
                    choices = list("All" = 1, "Completed F/U" = 2), 
                    selected = 1
                  ))
            )),
    
    tabItem(tabName = "rediax",
            fluidRow(
              box(title = "Diagnosis during OPD/IPD Visits",
                  width = 12,
                  align="center",
                  plotlyOutput("rediax1"),
                  radioButtons(
                    "provdx",
                    label = "",
                    inline = TRUE,
                    choices = list("All" = 1, "Nakorn Phanom" = 2, "Tak" = 3), 
                    selected = 1
                  ),
                  radioButtons(
                    "compdx",
                    label = "",
                    inline = TRUE,
                    choices = list("All" = 1, "Completed F/U" = 2), 
                    selected = 1
                  ))
            ))
    
    
    
    
  ),
  useShinyjs()
)

