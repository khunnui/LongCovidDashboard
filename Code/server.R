################################################################################
# server.R
################################################################################

library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)
library(svglite)
library(gtsummary)
library(bstfun)



# library(cowplot)
theme_gtsummary_compact()

server <- function(input, output, session) {

  tt <- reactive({
    if (input$hospital != "All") {
      tt <- paste0(input$hospital, " Hospital")
    } else if (input$province != "All") {
      tt <- paste0(input$province, " Province")
    } else {
      tt <- ""
    }
    if (input$rps == "Yes") {
      tt <- paste0(tt, " (RPS Only)")
    } else if (input$rps == "No") {
      tt <- paste0(tt, " (Non-RPS Only)")
    } else {
      tt <- paste0(tt, " ")
    }
  })
  
  shinyjs::html("dateHeader", paste0(" Data as of ", lcddate))


  #----------enroll - number of patient------------------------
  output$enroll <- render_gt({
   
  
    t0 <- tbl_summary(data = df_lc1,
                      include = fu1:fu4,
                      by = period) %>%
      add_overall() %>%
      modify_header(update = list(label = "",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}"))
    t1 <-
      tbl_strata(
        data = df_lc1,
       strata = province,
        .tbl_fun =
          ~ .x %>%
          tbl_summary(include = fu1:fu4,
                      by = period) %>%
          add_overall() %>%
          modify_header(update = list(
            label = "",
            all_stat_cols() ~ "**{level}**<br>N = {n}"
          ))
      )
    
    tbl_merge(list(t0, t1)) %>%
      modify_spanning_header(list(
        c(var_type_1, stat_0_1, stat_1_1, stat_2_1) ~ "Overall",
        c(var_type_1_2, stat_0_1_2, stat_1_1_2, stat_2_1_2) ~ "Nakorn Phanom",
        c(var_type_2_2, stat_0_2_2, stat_1_2_2, stat_2_2_2) ~ "Tak"
      )) %>%
      
      as_gt() %>%
      
      tab_options(table.border.bottom.style = 'none') %>%
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_column_spanners())
  })
  #------------------------------------- 
  #severity
  #-------------------------------------
  
  output$severe <- render_gt({
   
   tbl_summary(data = df_lc2,
                      include = l1severegrade,
                      by = province     ) %>%
      add_overall() %>%
      modify_header(update = list(label = "",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>% 
      as_gt() %>%
      
      tab_options(table.border.bottom.style = 'none') %>%
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_column_spanners())
  })
  
  #------------------------------------- 
  # complication
  #-------------------------------------
  
  
  output$complicate <- render_gt({
   
   t0<- tbl_likert(data=df_lc3,include = l1cshock:l1csid) 
      
   t1<- tbl_likert(data=df_lc3 %>%  dplyr::filter(province =='Nakorn Phanom'),include = l1cshock:l1csid) 
    
   t2<- tbl_likert(data=df_lc3 %>%  dplyr::filter(province =='Tak'),include = l1cshock:l1csid)
   
  
   n0 = nrow(df_lc3)
   n1 = nrow(df_lc3 %>% filter(province == 'Nakorn Phanom'))
   n2 = nrow(df_lc3 %>% filter(province == 'Tak'))
   
   
   tbl_merge(list(t0, t1, t2),
             tab_spanner = c(
               paste0('All (N = ', n0, ')'),
               paste0('Nakorn Phanom (N = ', n1, ')'),
               paste0('Tak (N = ', n2, ')')
             )) %>%
     as_gt() %>%
     tab_style(style = cell_text(weight = "bold"),
               locations = cells_column_spanners())
  })
  
  
  #------------------------------------- 
  # Infections
  #-------------------------------------
  
  
  output$infect <- render_gt({
    
      # tbl_summary(data = df_lc4,
      #                 include = l1inuri:l1mucormycosis,
      #                 by = province, missing = 'no',type = l1inuri :l1mucormycosis  ~ "categorical") %>%
      # add_overall() %>%
      # modify_header(update = list(label = "",
      #                             all_stat_cols() ~ "**{level}**<br>N = {n}")) %>% 
      # 
        
        t0<- tbl_likert(data=df_lc4,include =  l1inuri:l1mucormycosis) 
        
        t1<- tbl_likert(data=df_lc4 %>%  dplyr::filter(province =='Nakorn Phanom'),include = l1inuri:l1mucormycosis) 
        
        t2<- tbl_likert(data=df_lc4 %>%  dplyr::filter(province =='Tak'),include =  l1inuri:l1mucormycosis)
        
        
        n0 = nrow(df_lc3)
        n1 = nrow(df_lc3 %>% filter(province == 'Nakorn Phanom'))
        n2 = nrow(df_lc3 %>% filter(province == 'Tak'))
        
        
        tbl_merge(list(t0, t1, t2),
                  tab_spanner = c(
                    paste0('All (N = ', n0, ')'),
                    paste0('Nakorn Phanom (N = ', n1, ')'),
                    paste0('Tak (N = ', n2, ')')
                  )) %>%
          as_gt() %>%
          tab_style(style = cell_text(weight = "bold"),
                    locations = cells_column_spanners())
      })
      
}
