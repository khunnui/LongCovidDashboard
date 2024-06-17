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
library(dplyr)


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
    tbl_summary(data = df_lc1,
                include = fu1:fu4,
                by = period ) %>%
      add_overall() %>%
      modify_header(update = list(label = "",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>% 
      as_gt() %>%
      
      tab_options(table.border.bottom.style = 'none') %>%
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_column_spanners())
    
  
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
  
  
  
    
    output$enroll1 <- render_gt({
      
      tbl_summary(data = df_lc1,
                  include = fu1:fu4,
                  by = period,
                  statistic = all_categorical() ~ "{n}<br>({p}%)") %>%
        add_overall() %>%
        modify_header(update = list(label = "",
                                    all_stat_cols() ~ "**{level}**<br>N = {n}")) %>% 
        
        as_gt() %>%
        gt::fmt_markdown(columns = everything()) %>% 
        tab_options(table.border.bottom.style = 'none') %>%
        tab_style(style = cell_text(weight = "bold"),
                  locations = cells_column_spanners())
    })
    
    
    output$enroll2 <- render_gt({
      tbl_strata(
        data = df_lc1,
        strata = province,
        .tbl_fun =
          ~ .x %>%
          tbl_summary(include = fu1:fu4,
                      by = period,
                      statistic = all_categorical() ~ " {n}<br>({p}%) ") %>%
          add_overall() %>%
          modify_header(update = list(
            label = "",
            all_stat_cols() ~ "**{level}**<br>N = {n}"
          ))
      ) %>% 
        as_gt() %>%
        gt::fmt_markdown(columns = everything()) %>% 
        tab_style(style = cell_text(weight = "bold"),
                  locations = cells_column_spanners())
    })
    
    
      
      
  #------------------------------------- 
  #severity
  #-------------------------------------
  
  output$severe <- render_gt({
   
   tbl_summary(data = df_lc2,
                      include = l1severegrade,
                      by = province ,
               statistic = all_categorical() ~ " {n}<br>({p}%) ") %>%
      add_overall() %>%
      modify_header(update = list(label = "**Severity**",
                                  all_stat_cols() ~ "**{level}**<br>N = {n}")) %>% 
      as_gt() %>%
      gt::fmt_markdown(columns = everything()) %>% 
      tab_options(table.border.bottom.style = 'none') %>%
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_column_spanners())
  })
  
  #------------------------------------- 
  # complication
  #-------------------------------------
  
  
  output$complicate <- render_gt({
   
   t0<- tbl_likert(data=df_lc3,include = l1cshock:l1csid,  statistic = "{n}<br>({p}%)") 
      
   t1<- tbl_likert(data=df_lc3 %>%  dplyr::filter(province =='Nakorn Phanom'),include = l1cshock:l1csid,  statistic = "{n}<br>({p}%)") 
    
   t2<- tbl_likert(data=df_lc3 %>%  dplyr::filter(province =='Tak'),include = l1cshock:l1csid,  statistic = "{n}<br>({p}%)")
   
  
   n0 = nrow(df_lc3)
   n1 = nrow(df_lc3 %>% filter(province == 'Nakorn Phanom'))
   n2 = nrow(df_lc3 %>% filter(province == 'Tak'))
   
   
   tbl_merge(list(t0, t1, t2),
             tab_spanner = c(
               paste0('Overall (N = ', n0, ')'),
               paste0('Nakorn Phanom (N = ', n1, ')'),
               paste0('Tak (N = ', n2, ')')
             )) %>%
     modify_header(update = list(label = "**Complications**",
                                 all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
    
     as_gt() %>%
     gt::fmt_markdown(columns = everything()) %>% 
     tab_style(style = cell_text(weight = "bold"),
               locations = cells_column_spanners())
  })
  
  
  #------------------------------------- 
  # Infections
  #-------------------------------------
  
  
  output$infect <- render_gt({
    
        t0<- tbl_likert(data=df_lc4,include =  l1inuri:l1mucormycosis,  statistic = "{n}<br>({p}%)") 
        
        t1<- tbl_likert(data=df_lc4 %>%  dplyr::filter(province =='Nakorn Phanom'),  statistic = "{n}<br>({p}%)",include = l1inuri:l1mucormycosis) 
        
        t2<- tbl_likert(data=df_lc4 %>%  dplyr::filter(province =='Tak'),  statistic = "{n}<br>({p}%)",include =  l1inuri:l1mucormycosis)
        
        
        n0 = nrow(df_lc3)
        n1 = nrow(df_lc3 %>% filter(province == 'Nakorn Phanom'))
        n2 = nrow(df_lc3 %>% filter(province == 'Tak'))
        
        
        tbl_merge(list(t0, t1, t2),
                  tab_spanner = c(
                    paste0('Overall (N = ', n0, ')'),
                    paste0('Nakorn Phanom (N = ', n1, ')'),
                    paste0('Tak (N = ', n2, ')')
                  )) %>%
          modify_header(update = list(label = "**Infection Diagnosis**",
                                      all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
          
          as_gt() %>%
          gt::fmt_markdown(columns = everything()) %>% 
          tab_style(style = cell_text(weight = "bold"),
                    locations = cells_column_spanners())
      })
      
    
    #------------------------------------- 
    # Treatments
    #-------------------------------------
  output$treat <- render_gt({
   
    t0<- tbl_likert(data=df_lc5,include =  l1antiviral:l1antithrom,  statistic = "{n}<br>({p}%)") 
    
    t1<- tbl_likert(data=df_lc5 %>%  dplyr::filter(province =='Nakorn Phanom'),  statistic = "{n}<br>({p}%)",include =  l1antiviral:l1antithrom) 
    
    t2<- tbl_likert(data=df_lc5 %>%  dplyr::filter(province =='Tak'),  statistic = "{n}<br>({p}%)",include =   l1antiviral:l1antithrom)
    
    
    n0 = nrow(df_lc3)
    n1 = nrow(df_lc3 %>% filter(province == 'Nakorn Phanom'))
    n2 = nrow(df_lc3 %>% filter(province == 'Tak'))
    
    
   tbl_merge(list(t0, t1, t2),
              tab_spanner = c(
                paste0('Overall (N = ', n0, ')'),
                paste0('Nakorn Phanom (N = ', n1, ')'),
                paste0('Tak (N = ', n2, ')')
              )) %>%
     modify_column_indent(columns = label,
                          rows = variable %in% c('l1avlop', 'l1avdar', 'l1avrem', 'l1avfav', 'l1avacy', 'l1avose', 'l1avpax', 'l1avmol')) %>%
     modify_header(update = list(label = "**Treatment Received**",
                                 all_stat_cols() ~ "**{level}**<br>N = {n}")) %>%
     
     as_gt() %>%
     gt::fmt_markdown(columns = everything()) %>% 
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_column_spanners())
  })
  
    
    #------------------------------------- 
    # Depression
    #-------------------------------------
  
  output$depress <- renderPlotly({
  plot_ly(
    data = df_lc6 %>%
      group_by(period, severe) %>%
      summarise(count = sum(n))%>%
      mutate(pct = count/sum(count)*100 ) %>% 
      mutate(pct = case_when(pct > 10  ~ round(pct,0),
                             pct < 10  ~ round(pct,1))),
    x = ~ period,
    y = ~ pct,
    type = "bar",
    color = ~ severe,
  #  colors = color_scale3,
    hoverinfo = 'y'
  ) %>% 
      layout(title = '', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Period'), 
             yaxis = list(title = 'Percentage'))
    
    # layout(barmode = 'stack',
    #        bargap = 0.5)
  })
    
    output$depress2 <- renderPlotly({
      plot_ly(
        data = df_lc62 %>%
          group_by(period, severe) %>%
          summarise(count = sum(n))%>%
          mutate(pct = count/sum(count)*100 ) %>% 
          mutate(pct = case_when(pct > 10  ~ round(pct,0),
                                 pct < 10  ~ round(pct,1))),
        x = ~ period,
        y = ~ pct,
        type = "bar",
        color = ~ severe,
        #  colors = color_scale3,
        hoverinfo = 'y'
      ) %>% 
        layout(title = '', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Period'), 
               yaxis = list(title = 'Percentage'))
      
      # layout(barmode = 'stack',
      #        bargap = 0.5)
    })
  
  
    
    #------------------------------------- 
    # Hospital visit
    #------------------------------------- 
  output$fu <- renderPlotly({
    if (input$provx == 2) {
      df <- df_lc21 %>% filter(province == "Nakorn Phanom")
    } else if (input$provx == 3) {
      df <- df_lc21 %>% filter(province == "Tak")
    } else {
      df <- df_lc21
    }
    if (input$compx == 2) {
      df <- df %>% filter(completefu == 1)
    } else {
      df <- df
    }
    plot_ly(
      data = df  %>%
        group_by(period, oipd) %>%
        summarise(count = sum(n))  ,
      x = ~ period,
      y = ~ count,
      type = "bar",
      color = ~ oipd,
     # colors = color_scale3,
      hoverinfo = 'y'
    )  %>% 
      layout(title = '', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Period'), 
             yaxis = list(title = 'No. of patients'))
   
  }) 
  
    
    #------------------------------------- 
    # Reinfections
    #-------------------------------------
  output$reinfect <- renderPlotly({
    if (input$provrf == 2) {
      df <- df_lc22 %>% filter(province == "Nakorn Phanom")
    } else if (input$provrf == 3) {
      df <- df_lc22 %>% filter(province == "Tak")
    } else {
      df <- df_lc22
    }
    if (input$comprf == 2) {
      df <- df %>% filter(completefu == 1)
    } else {
      df <- df
    }
    n1 <- sum(filter(df,period == 1)$n,na.rm=TRUE)
    n2 <- sum(filter(df,period == 2)$n,na.rm=TRUE)
    n3 <- sum(filter(df,period == 3)$n,na.rm=TRUE)
    n4 <- sum(filter(df,period == 4)$n,na.rm=TRUE)
    df <- df %>%
      mutate(period = factor(period, levels = 1:4 ,labels = c(
        paste0('Period 1 (n=', n1, ')'),
        paste0('Period 2 (n=', n2, ')'),
        paste0('Period 3 (n=', n3, ')'),
        paste0('Period 4 (n=', n4, ')')
      )))
    plot_ly(
      data = df  %>%
        group_by(period, reinfect) %>%
        summarise(count = sum(n))   %>% 
        mutate(pct = count/sum(count)*100)       %>% 
        mutate(pct = case_when(pct > 10  ~ round(pct,0),
                               pct < 10  ~ round(pct,1))),
      x = ~ period,
      y = ~ pct,
      type = "bar",
      color = ~ reinfect,
      # colors = color_scale3,
      hoverinfo = 'y'
    )  %>% 
      layout(title = '', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Period'), 
             yaxis = list(title = 'Percentage' ))
    
  
  }) 
  
    
    #------------------------------------- 
    # Diagnosis
    #-------------------------------------
  output$rediax1 <- renderPlotly({
    if (input$provdx == 2) {
      df <- df_lc3dx %>% filter(province == "Nakorn Phanom")
    } else if (input$provdx == 3) {
      df <- df_lc3dx %>% filter(province == "Tak")
    } else {
      df <- df_lc3dx  
    }
    
    if (input$compdx == 2) {
      df <- df %>% filter(completefu == 1)
    } else {
      df <- df
    }
    plot_ly(
      data = df %>%
        group_by(period, Diagnosis) %>%
        summarise(count = sum(y)),
      y = ~ count,
      x = ~ period,
      type = "bar",
      color = ~ Diagnosis,
      hoverinfo = 'y'
    )%>% 
      layout(title = '', plot_bgcolor = "", xaxis = list(title = 'Period'), 
             yaxis = list(title = 'Count'))
    
    
   
    
  }) 
  
}
