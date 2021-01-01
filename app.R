library(shiny)
library(shiny.semantic)
library(shiny.router)
library(dplyr)
library(tidyverse)
library(leaflet)
library(geosphere)
library(DT)
library(lubridate)
library(viridis) 
library(RColorBrewer)
library(pracma)
library(ggplot2)
library(plotly)
library(shiny.i18n)


options(semantic.themes = TRUE)

#importing data
dataset_ship <- read.csv(file = 'ships_cleaned.csv')
dataset_ship$DATETIME <- ymd_hms(dataset_ship$DATETIME)
dataset_ship$date <- ymd(dataset_ship$date)
dataset_ship <- dataset_ship[, -1]

# File with translations
i18n <- Translator$new(translation_json_path = "data/translation.json")
translator <- Translator$new(translation_json_path = "data/translation.json")
i18n$set_translation_language("EN") # here you select the default translation to display


# Gdansk port approximate borders
xp_gdansk <- c(54.420860, 54.344156, 54.339022, 54.402866)
yp_gdansk <- c(18.659307, 18.591881, 18.745706, 18.737222)
# St.Patersburg port approximate borders
xp_st_patersburg <- c(60.003781, 59.861573, 60.026894, 59.878687)
yp_st_patersburg <- c(30.135619, 30.137846, 30.474049, 30.456174)
# Gothenborg  port approximate borders
xp_gothenborg <- c(57.731156, 57.610065, 57.707230, 57.778578)
yp_gothenborg <- c(11.802402, 11.870641, 12.078366, 11.999240)
# Kalingrad port approximate borders
xp_kalingrad <- c(54.655970, 54.695489, 54.756110, 54.734615)
yp_kalingrad <- c(20.384638, 20.706282, 20.631373, 20.325918)
# Gdynia port approximate borders
xp_gdynia <- c(54.547912, 54.549266, 54.540949, 54.527006, 54.527104, 54.530489, 54.522120, 54.522712)
yp_gdynia <- c(18.561741, 18.504729, 18.496232, 18.518892, 18.531335, 18.541292, 18.549438, 18.562654)
# Kleipeda port approximate borders
xp_kleipeda <- c(55.785185, 55.800266, 55.636145, 55.594954)
yp_kleipeda <- c(21.022302, 21.157649, 21.264247, 21.072489)




page <- function() {
    #shiny.i18n::usei18n(i18n)
    div(
        div(class="menutext", menu_line
        )
        
    )
}
shiny.i18n::usei18n(i18n)
menu_line <- horizontal_menu(list(list(name = i18n$t("observation info"), link = route_link("/"), icon = "globe"),
                                  list(name = i18n$t("port map"), link = route_link("other"), icon ="anchor"),
                                  list(name = i18n$t("charts"), link = route_link("charts"), icon = "chart line"),
                                  list(name = i18n$t("table"), link = route_link("table"), icon = "th")
)
)

shiptypeInput <- selectInput("shiptype", NULL, choices = c(levels(dataset_ship$ship_type)))
shiptypeInput_table <- selectInput("shiptype_table", NULL, choices = c(levels(dataset_ship$ship_type)))

shipnameInput <- selectInput("shipname", NULL, choices = c("No values"))
shipnameInput_table <- selectInput("shipname_table", NULL, choices = c("No values"))

shipstatusInput <-  shiny::radioButtons("status", "",
                                        c("In move" = "move",
                                          "Parked" = "parked",
                                          "Both" = "both"),
                                        selected = "both")
shipstatusInput_port <-  shiny::radioButtons("status_port", "",
                                             c("In move" = "move",
                                               "Parked" = "parked",
                                               "Both" = "both"),
                                             selected = "both")
shipstatusInput_table <-  shiny::radioButtons("status_table", "",
                                              c("In move" = "move",
                                                "Parked" = "parked",
                                                "Both" = "both"),
                                              selected = "both")

specifydateToggle <- checkbox_input("specificDates", i18n$t("Specify date(s)"), type = "slider", is_marked = FALSE)
specifydateToggle_port <- checkbox_input("specificDates_port", i18n$t("Specify date(s)"), type = "slider", is_marked = FALSE)
specifydateToggle_table <- checkbox_input("specificDates_table", i18n$t("Specify date(s)"), type = "slider", is_marked = FALSE)


selectdateInput <- selectInput("selectDate", NULL, choices = c("No values"), multiple = TRUE, selected = NULL)
selectdateInput_table <- selectInput("selectDate_table", NULL, choices = c("No values"), multiple = TRUE, selected = NULL)

portnameInput <- selectInput("portname", NULL, choices = levels(dataset_ship$port))
portnameInput_charts <- selectInput("portname_charts", NULL, choices = levels(dataset_ship$port))

hoursliderInput <- range_input("spec_hour", value = 10, value2 = 14, min = 0, max = 23, step = 1, class = "labeled ticked")
hoursliderInput_chart <- range_input("spec_hour_chart", value = 10, value2 = 14, min = 0, max = 23, step = 1, class = "labeled ticked")

minutesliderInput <- range_input("spec_minute", value = 1, value2 = 59, min = 1, max = 59, step = 1, class = "labeled ticked")
minutesliderInput_chart <- range_input("spec_minute_chart", value = 1, value2 = 59, min = 1, max = 59, step = 1, class = "labeled ticked")

dateInputCalender <- calendar("portDateCalender", type = "date", value = NULL, 
                              "Select Date", min = format(min(unique(dataset_ship$date)), '%d.%m.%Y'), 
                              max = format(max(unique(dataset_ship$date)), '%d.%m.%Y'))

specifyPortToggle <- checkbox_input("specificPort", "Specify port name", type = "slider", is_marked = FALSE)


root_page <- div(page(),
                 div(class = "doubling stackable three column ui grid container",
                     
                     div(class = "column", 
                         div(class = "ui segment", "",
                             field(
                                 tags$h2(i18n$t("Ship Info")),
                                 
                                 tags$label(class = "label_styl", i18n$t("Ship type:")),
                                 div(class = "field_dist", 
                                     shiptypeInput
                                 ),
                                 tags$label(class = "label_styl", i18n$t("Ship name:")),
                                 div(class = "field_dist", 
                                     shipnameInput
                                 )
                             )
                         )
                     ),
                     div(class = "column", 
                         div(class = "ui segment col-height-align", "",
                             field(
                                 tags$h2(i18n$t("Ship Status")),
                                 tags$label(class = "label_styl", i18n$t("Ship current state:")),
                                 div(class = "field_dist", 
                                     shiny::radioButtons("status", "",
                                                         c("In move" = "move",
                                                           "Parked" = "parked",
                                                           "Both" = "both"),
                                                         selected = "both")
                                 )
                                 
                             )
                         )
                     ),
                     div(class = "column", 
                         div(class = "ui segment", "",
                             field(
                                 tags$h2(i18n$t("Date and Time")),
                                 div(class = "field_dist", 
                                     specifydateToggle
                                 ),
                                 conditionalPanel(
                                     condition = "input.specificDates == true",
                                     div(class = "field_dist", 
                                         selectdateInput
                                     )
                                 )
                                 
                             )
                         )
                     )
                     
                 ),
                 
                 div(class = "ui one column stackable grid container",
                     div(class = "column", 
                         div(class = "ui horizontal divider", icon("dice d20"), i18n$t("Analytics"))
                     )
                     
                     
                 ),
                 
                 div(class = "ui three column doubling stackable grid container",
                     
                     div(class = "column", 
                         
                         div(class = "ui raised segment segment-style",
                             icon("huge route"),
                             div(class = "position-align",
                                 tags$h3(i18n$t("Maximum Distance:")),
                                 htmlOutput("m_distance")
                             )
                         )
                         
                         
                         
                     ),
                     
                     div(class = "column", 
                         
                         div(class = "ui raised segment segment-style",
                             icon("huge dharmachakra"),
                             div(class = "position-align",
                                 tags$h3(i18n$t("Total Distance:")),
                                 htmlOutput("tot_distance")
                             )
                         )
                         
                         
                     ),
                     
                     div(class = "column", 
                         div(class = "ui raised segment segment-style",
                             icon("huge anchor"),
                             div(class = "position-align",
                                 tags$h3(i18n$t("Snoozing Time:")),
                                 htmlOutput("snoozing_time")
                             )
                         )
                         
                     ),
                     
                     
                     
                     div(class = "column", 
                         div(class = "ui raised segment segment-style",
                             icon("huge ship"),
                             div(class = "position-align",
                                 tags$h3(i18n$t("Sailing Time:")),
                                 htmlOutput("sailing_time")
                             )
                         )
                         
                     )
                     
                     
                 ),
                 
                 
                 
                 div(class = "ui one column stackable grid container",
                     div(class = "column", 
                         div(class = "ui horizontal divider", icon("globe"), i18n$t("Maps"))
                     ),
                     
                     div(class = "column", 
                         div(class = "ui raised segment",
                             
                             tabset(list(list(menu = div(i18n$t("Observations")), 
                                              content = div(
                                                  leafletOutput('observationMap')
                                              )), 
                                         list(menu = div(i18n$t("Distance Between Two Points")), 
                                              content = div(
                                                  leafletOutput('twoPointsDistance')
                                              ))
                             )
                             )
                             
                             
                         )
                     )
                 )
                 
)



other_page <- div(page(), 
                  div(class = "doubling stackable two column ui grid container",
                      
                      div(class = "column", 
                          div(class = "ui segment", "",
                              field(
                                  tags$h2(i18n$t("Port Info")),
                                  
                                  tags$label(class = "label_styl", i18n$t("Port Name:")),
                                  div(class = "field_dist", 
                                      portnameInput
                                  ),
                                  
                                  tags$label(class = "label_styl", i18n$t("Ship current state:")),
                                  div(class = "field_dist", 
                                      shipstatusInput_port
                                  ),
                                  
                                  div(class = "field_dist", 
                                      specifydateToggle_port
                                  ),
                                  conditionalPanel(
                                      condition = "input.specificDates_port == true",
                                      div(class = "field_dist", 
                                          dateInputCalender
                                      )
                                  )
                                  
                              )
                          )
                      ),
                      
                      div(class = "column", 
                          div(class = "ui segment", "",
                              field(
                                  tags$h2(i18n$t("Time")),
                                  
                                  tags$br(),
                                  tags$label(class = "label_styl", i18n$t("Hour(s):")),
                                  div(class = "field_dist", 
                                      hoursliderInput
                                  ),
                                  tags$label(class = "label_styl", i18n$t("Minute(s):")),
                                  div(class = "field_dist", 
                                      minutesliderInput
                                  )
                              )
                          )
                      )
                      
                  ),
                  
                  div(class = "ui one column stackable grid container",
                      div(class = "column", 
                          div(class = "ui horizontal divider", icon("dice d20"), i18n$t("Vessels Analytics"))
                      )
                      
                  ),
                  
                  
                  div(class = "ui three column doubling stackable grid container",
                      
                      div(class = "column",
                          div(class = "ui raised segment segment-style",
                              icon("huge ship"),
                              div(class = "position-align",
                                  tags$h3(i18n$t("In Port:")),
                                  htmlOutput("vessels_num")
                                  
                              )
                          )
                          
                      ),
                      div(class = "column",
                          div(class = "ui raised segment segment-style",
                              icon("huge clock outline"),
                              div(class = "position-align",
                                  tags$h3(i18n$t("Parked:")),
                                  htmlOutput("snoozing_num")
                                  
                              )
                          )
                          
                      ),
                      div(class = "column",
                          div(class = "ui raised segment segment-style",
                              icon("huge compass outline"),
                              div(class = "position-align",
                                  tags$h3(i18n$t("Moving:")),
                                  htmlOutput("sailing_num")
                                  
                              )
                          )
                      )
                  ),
                  
                  
                  
                  div(class = "ui one column stackable grid container",
                      div(class = "column", 
                          div(class = "ui horizontal divider", icon("anchor"), i18n$t("Map"))
                      ),
                      
                      div(class = "column", 
                          div(class = "ui raised segment",
                              leafletOutput('portmap')
                          )
                      )
                  )
                  
)

charts_page <- div(page(), 
                   div(class = "ui one column stackable grid container",
                       div(class = "column", 
                           div(class = "ui segment", "",
                               field(
                                   tags$h2(i18n$t("Port Info")),
                                   
                                   tags$label(class = "label_styl", i18n$t("Port Name:")),
                                   div(class = "field_dist", 
                                       portnameInput_charts
                                   )
                                   
                               )
                           )
                       )
                       
                   ),
                   
                   
                   
                   
                   
                   div(class = "ui one column stackable grid container",
                       div(class = "column", 
                           div(class = "ui horizontal divider", icon("anchor"), i18n$t("Maps"))
                       ),
                       
                       div(class = "column", 
                           div(class = "ui raised segment",
                               tabset(list(list(menu = div(i18n$t("Total number of vessels")),  
                                                content = div(
                                                    plotlyOutput("num_vessels_graph")
                                                )), 
                                           list(menu = div(i18n$t("Average stop duration")), 
                                                content = div(
                                                    plotlyOutput("stop_vessels_graph_dur")
                                                )), 
                                           list(menu = div(i18n$t("Total capacity")), 
                                                content = div(
                                                    plotlyOutput("dwt_total_graph")
                                                ))
                               ))
                               
                           )
                       )
                   )
                   
)



table_page <- div(page(), 
                  div(class = "doubling stackable three column ui grid container",
                      div(class = "column", 
                          div(class = "ui segment", "",
                              field(
                                  tags$h2(i18n$t("Ship Info")),
                                  
                                  tags$label(class = "label_styl", i18n$t("Ship type:")),
                                  div(class = "field_dist", 
                                      shiptypeInput_table
                                  ),
                                  tags$label(class = "label_styl", i18n$t("Ship name:")),
                                  div(class = "field_dist", 
                                      shipnameInput_table
                                  )
                              )
                          )
                      ),
                      div(class = "column", 
                          div(class = "ui segment col-height-align", "",
                              field(
                                  tags$h2(i18n$t("Ship Status")),
                                  tags$label(class = "label_styl", i18n$t("Ship current state:")),
                                  div(class = "field_dist", 
                                      shipstatusInput_table
                                  )
                                  
                              )
                          )
                      ),
                      div(class = "column", 
                          div(class = "ui segment col-height-align", "",
                              field(
                                  tags$h2(i18n$t("Port Info")),
                                  tags$label(class = "label_styl", i18n$t("Port Name:")),
                                  div(class = "field_dist",
                                      selectInput("portnameAll", NULL, choices = c("All", levels(dataset_ship$port)))
                                  )
                                  
                              )
                          )
                      )
                      
                  ),
                  
                  div(class = "ui two column stackable grid container",
                      div(class = "ten wide column", 
                          div(class = "ui segment", "",
                              field(
                                  tags$h2(i18n$t("Date and Time")),
                                  div(class = "field_dist", 
                                      specifydateToggle_table
                                  ),
                                  conditionalPanel(
                                      condition = "input.specificDates_table == true",
                                      div(class = "field_dist", 
                                          selectdateInput_table
                                      )
                                  ),
                                  tags$br(),
                                  tags$label(class = "label_styl", i18n$t("Hour(s):")),
                                  div(class = "field_dist", 
                                      hoursliderInput_chart
                                  ),
                                  tags$label(class = "label_styl", i18n$t("Minute(s):")),
                                  div(class = "field_dist", 
                                      minutesliderInput_chart
                                  )
                              )
                          )
                      ),
                      div(class = "six wide column", 
                          div(class = "ui segment", 
                              tags$h2(i18n$t("Download Filtered Data")),
                              downloadLink("downloadData",
                                           div(class = "ui orange basic big button dwld_btn", icon("download"), 
                                               i18n$t("Download"))
                              )
                          )
                      )
                  ),
                  
                  div(class = "ui one column stackable grid container",
                      div(class = "column", 
                          div(class = "ui horizontal divider", icon("th"), i18n$t("table"))
                      ),
                      
                      div(class = "column", 
                          div(class = "ui raised segment", 
                              DT::dataTableOutput("filteredTable")  
                          )
                      )
                  )
                  
)


observationinfo_server <- function(input, output, session) {
    shiptype <- reactive({
        req(input$shiptype)
        
        filter(dataset_ship, ship_type == input$shiptype)
    })
    
    observeEvent(shiptype(), {
        choices <- unique(shiptype()$SHIPNAME)
        updateSelectInput(session, "shipname", NULL, choices = choices)
    })
    
    # ... and here its reactive version that react to changes of the language.
    i18n <- reactive({
        selected <- input$selectedLanguage
        if (length(selected) > 0 && selected %in% translator$get_languages()) {
            translator$set_translation_language(selected)
        }
        translator
    })
    
    data_sample <- reactive({
        req(input$shiptype)
        req(input$shipname)
        req(input$status)
        
        
        if(input$status == "move"){
            dataset_ship %>%
                filter(ship_type == input$shiptype) %>%
                filter(SHIPNAME == input$shipname) %>%
                filter(is_parked == 0)
        } else if(input$status == "parked"){
            dataset_ship %>%
                filter(ship_type == input$shiptype) %>%
                filter(SHIPNAME == input$shipname) %>%
                filter(is_parked == 1)
        } else {
            dataset_ship %>%
                filter(ship_type == input$shiptype) %>%
                filter(SHIPNAME == input$shipname) 
        }
        
    })
    
    filtered_data <- reactive({
        dateSelected <- dmy(input$selectDate)
        
        if(length(input$selectDate) > 0){
            data_sample() %>%
                filter(date %in% dateSelected)
        } else {
            data_sample()
        }
    })
    
    
    
    observe({
        
        choices <- format(unique(data_sample()$date), i18n()$t('%d %B %Y'))
        
        if(!input$specificDates){
            updateSelectInput(session, "selectDate", NULL, choices = c("No values"), selected = NULL)
        } else {
            updateSelectInput(session, "selectDate", NULL, choices = choices)
        }
        
    })
    
    max_point <- reactive({
        max(filtered_data()$distance_between_observation, na.rm = TRUE)
    }) 
    
    total_distance_point <- reactive({
        sum(filtered_data()$distance_between_observation, na.rm = TRUE)
    }) 
    
    snoozing_time <- reactive({
        filtered_vector <- filtered_data() %>%
            filter(is_parked == 1) 
        
        filtered_vector <- as.numeric(difftime(strptime(paste(filtered_vector$DATETIME),"%Y-%m-%d %H:%M:%S"),
                                               strptime(paste(lag(filtered_vector$DATETIME)),"%Y-%m-%d %H:%M:%S")))
        
        sum(filtered_vector, na.rm = TRUE)
        
    })
    
    
    sailing_time <- reactive({
        filtered_vector <- filtered_data() %>%
            filter(is_parked == 0) 
        
        filtered_vector <- as.numeric(difftime(strptime(paste(filtered_vector$DATETIME),"%Y-%m-%d %H:%M:%S"),
                                               strptime(paste(lag(filtered_vector$DATETIME)),"%Y-%m-%d %H:%M:%S")))
        
        sum(filtered_vector, na.rm = TRUE)
        
    })
    
    
    output$observationMap <- renderLeaflet({
        pal <- colorFactor(palette = "Pastel1", 
                           domain = unique(format(filtered_data()$date, i18n()$t('%d %B')))
        )
        
        
        leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addCircles(lng = filtered_data()$LON, 
                       lat = filtered_data()$LAT,
                       popup = paste0(format(filtered_data()$DATETIME, i18n()$t('<b>%A, %d %b %Y</b> <br><span><b>Time: </b></span> %H:%M:%S'))),
                       color = pal(format(filtered_data()$date, i18n()$t('%d %B')))) %>%
            addLegend("bottomright", pal = pal, values = format(filtered_data()$date, i18n()$t('%d %B')),
                      title = i18n()$t("DATE:"),
                      opacity = 1
            ) %>%
            addProviderTiles(providers$CartoDB.DarkMatter)
    })
    
    
    output$twoPointsDistance <- renderLeaflet({
        dist_vector <- c((which(filtered_data()$distance_between_observation == max_point())), 
                         ((which(filtered_data()$distance_between_observation == max_point()))-1))
        
        #palette = c("#fbb39d", "#b6e2dd", "#c8ddbb", "#e9e5af", "#fbdf9d", "#fbc99d", "#fba09d", "#fbfefb"
        
        pal <- 
            colorFactor(palette = c("#fbb39d", "#bb9f5d"), 
                        levels = format(filtered_data()[dist_vector, ]$DATETIME, '%H:%M:%S'))
        
        
        leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addCircles(lng = filtered_data()[dist_vector, ]$LON, 
                       lat = filtered_data()[dist_vector, ]$LAT,
                       popup = paste0(format(filtered_data()[dist_vector, ]$DATETIME, i18n()$t('<b>%A, %d %b %Y</b> <br><span><b>Time: </b></span> %H:%M:%S'))),
                       color = pal(format(filtered_data()[dist_vector, ]$DATETIME, '%H:%M:%S'))) %>%
            addLegend("bottomright", pal = pal, values = format(filtered_data()[dist_vector, ]$DATETIME, '%H:%M:%S'),
                      title = i18n()$t("TIME:"),
                      opacity = 1
            ) %>%
            addProviderTiles(providers$CartoDB.DarkMatter)
    })
    
    
    output$m_distance <- renderText({  
        paste("<b><i>", round(max_point(), 3), "</i></b>", i18n()$t("meters."))
        #return(round(max_point(),4))
    })
    
    output$tot_distance <- renderText({
        paste("<b><i>", round(total_distance_point(), 3), "</i></b>", i18n()$t("meters."))
    })
    
    output$snoozing_time <- renderText({
        paste("<b><i>", round(snoozing_time()/60, 3), "</i></b>", i18n()$t("minutes."))
    })
    
    output$sailing_time <- renderText({
        paste("<b><i>", round(sailing_time()/60, 3), "</i></b>", i18n()$t("minutes."))
    })
    
    observeEvent(input$selectedLanguage, {
        # This print is just for demonstration
        #print(paste("Language change!", input$selectedLanguage))
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$selectedLanguage)
        
        if(input$selectedLanguage == 'PL'){
            shiny::updateRadioButtons(session, "status", label = "", 
                                      choices = c("W ruchu" = "move", "Zaparkowany" = "parked", "Obie" = "both"),
                                      selected = "both")
        }
        if(input$selectedLanguage == 'EN'){
            shiny::updateRadioButtons(session, "status", label = "", 
                                      choices = c("In move" = "move", "Parked" = "parked", "Both" = "both"),
                                      selected = "both")
        }
        if(input$selectedLanguage == 'ES'){
            shiny::updateRadioButtons(session, "status", label = "", 
                                      choices = c("Moviente" = "move", "Estacionado" = "parked", "Ambos" = "both"),
                                      selected = "both")
        }
        if(input$selectedLanguage == 'DE'){
            shiny::updateRadioButtons(session, "status", label = "", 
                                      choices = c("Ziehen um" = "move", "Geparkt" = "parked", "Beide" = "both"),
                                      selected = "both")
        }
        if(input$selectedLanguage == 'RU'){
            shiny::updateRadioButtons(session, "status", label = "", 
                                      choices = c("В Движении" = "move", "Припаркованные" = "parked", "Оба" = "both"),
                                      selected = "both")
        }
        
    })
    
}


port_server <- function(input, output, session) {
    
    i18n <- reactive({
        selected <- input$selectedLanguage
        if (length(selected) > 0 && selected %in% translator$get_languages()) {
            translator$set_translation_language(selected)
        }
        translator
    })
    
    data_sample_by_ports <- reactive({
        req(input$portname)
        
        if(input$portname == "St. Petersburg"){
            portData_sample <- dataset_ship[(inpolygon(dataset_ship$LAT, dataset_ship$LON, xp_st_patersburg, yp_st_patersburg, boundary = TRUE)), ] %>%
                filter(hour(DATETIME) >= input$spec_hour[1] & hour(DATETIME) <= input$spec_hour[2]) %>%
                filter(minute(DATETIME) >= input$spec_minute[1] & minute(DATETIME) <= input$spec_minute[2])
            
            if(length(input$portDateCalender) > 0){
                portData_sample <- portData_sample %>%
                    filter(date == ymd(input$portDateCalender))
            }
            
            return(portData_sample)
        }
        if(input$portname == "Gdańsk"){
            portData_sample <- dataset_ship[(inpolygon(dataset_ship$LAT, dataset_ship$LON, xp_gdansk, yp_gdansk, boundary = TRUE)), ] %>%
                filter(hour(DATETIME) >= input$spec_hour[1] & hour(DATETIME) <= input$spec_hour[2]) %>%
                filter(minute(DATETIME) >= input$spec_minute[1] & minute(DATETIME) <= input$spec_minute[2])
            
            if(length(input$portDateCalender) > 0){
                portData_sample <- portData_sample %>%
                    filter(date == ymd(input$portDateCalender))
            }
            
            return(portData_sample)
        }
        if(input$portname == "gothenborg"){
            portData_sample <- dataset_ship[(inpolygon(dataset_ship$LAT, dataset_ship$LON, xp_gothenborg, yp_gothenborg, boundary = TRUE)), ] %>%
                filter(hour(DATETIME) >= input$spec_hour[1] & hour(DATETIME) <= input$spec_hour[2]) %>%
                filter(minute(DATETIME) >= input$spec_minute[1] & minute(DATETIME) <= input$spec_minute[2])
            
            if(length(input$portDateCalender) > 0){
                portData_sample <- portData_sample %>%
                    filter(date == ymd(input$portDateCalender))
            }
            
            return(portData_sample)
        }
        if(input$portname == "Kalingrad"){
            portData_sample <- dataset_ship[(inpolygon(dataset_ship$LAT, dataset_ship$LON, xp_kalingrad, yp_kalingrad, boundary = TRUE)), ] %>%
                filter(hour(DATETIME) >= input$spec_hour[1] & hour(DATETIME) <= input$spec_hour[2]) %>%
                filter(minute(DATETIME) >= input$spec_minute[1] & minute(DATETIME) <= input$spec_minute[2])
            
            if(length(input$portDateCalender) > 0){
                portData_sample <- portData_sample %>%
                    filter(date == ymd(input$portDateCalender))
            }
            
            return(portData_sample)
        }
        if(input$portname == "Klaipeda"){
            portData_sample <- dataset_ship[(inpolygon(dataset_ship$LAT, dataset_ship$LON, xp_kleipeda, yp_kleipeda, boundary = TRUE)), ] %>%
                filter(hour(DATETIME) >= input$spec_hour[1] & hour(DATETIME) <= input$spec_hour[2]) %>%
                filter(minute(DATETIME) >= input$spec_minute[1] & minute(DATETIME) <= input$spec_minute[2])
            
            if(length(input$portDateCalender) > 0){
                portData_sample <- portData_sample %>%
                    filter(date == ymd(input$portDateCalender))
            }
            
            return(portData_sample)
        }
        if(input$portname == "Gdynia"){
            portData_sample <- dataset_ship[(inpolygon(dataset_ship$LAT, dataset_ship$LON, xp_gdynia, yp_gdynia, boundary = TRUE)), ] %>%
                filter(hour(DATETIME) >= input$spec_hour[1] & hour(DATETIME) <= input$spec_hour[2]) %>%
                filter(minute(DATETIME) >= input$spec_minute[1] & minute(DATETIME) <= input$spec_minute[2])
            
            if(length(input$portDateCalender) > 0){
                portData_sample <- portData_sample %>%
                    filter(date == ymd(input$portDateCalender))
            }
            
            return(portData_sample) 
        }
        
    })
    
    total_num_vessels <- reactive({
        req(data_sample_by_ports())
        data_sample_by_ports() %>%
            select(SHIPNAME) %>%
            count()
    })
    
    snoozing_num_vessels <- reactive({
        req(data_sample_by_ports())
        
        data_sample_by_ports() %>%
            filter(is_parked == 1) %>%
            select(SHIPNAME) %>%
            count()
    })
    
    sailing_num_vessels <- reactive({
        req(data_sample_by_ports())
        
        data_sample_by_ports() %>%
            filter(is_parked == 0) %>%
            select(SHIPNAME) %>%
            count()
    })
    
    
    data_sample <- reactive({
        req(data_sample_by_ports())
        req(input$status_port)
        
        
        if(input$status_port == "move"){
            data_sample_by_ports() %>%
                filter(is_parked == 0)
        } else if(input$status_port == "parked"){
            data_sample_by_ports() %>%
                filter(is_parked == 1)
        } else {
            data_sample_by_ports() 
        }
        
    })
    
    
    output$portmap <- renderLeaflet({
        
        pal <- 
            colorFactor(palette = 'Pastel1', 
                        levels = data_sample()$ship_type)
        
        leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addCircles(lng = data_sample()$LON, 
                       lat = data_sample()$LAT,
                       color = pal(data_sample()$ship_type),
                       popup = paste0(format(data_sample()$DATETIME, i18n()$t('<b>%A, %d %b %Y</b> <br><span><b>Time: </b></span> %H:%M:%S')), '<br>', '<b>', i18n()$t('Ship Type: '), '</b>', data_sample()$ship_type, '<br>', '<b>', i18n()$t('Ship Name: '), '</b>', data_sample()$SHIPNAME)) %>%
            addLegend("bottomright", pal = pal, values = data_sample()$ship_type,
                      title = i18n()$t("SHIP TYPE:"),
                      opacity = 1
            ) %>%
            addProviderTiles(providers$CartoDB.DarkMatter)
        
        
        
        
    })
    
    output$testdatecalender <- renderText({
        paste(input$portDateCalender)
    })
    
    output$vessels_num <- renderText({
        paste("<b><i>", total_num_vessels(), "</i></b>")
    })
    
    output$snoozing_num <- renderText({
        paste("<b><i>", snoozing_num_vessels(), "</i></b>")
    })
    
    output$sailing_num <- renderText({
        paste("<b><i>", sailing_num_vessels(), "</i></b>")
    })
    
    
    observeEvent(input$specificDates_port,{
        
        input$specificDates
        
        if(!input$specificDates_port){
            update_calendar(session, "portDateCalender", value = NULL)
        }
        
    })
    
    observeEvent(input$selectedLanguage, {
        # This print is just for demonstration
        #print(paste("Language change!", input$selectedLanguage))
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$selectedLanguage)
        
        
        if(input$selectedLanguage == 'PL'){
            shiny::updateRadioButtons(session, "status_port", label = "", 
                                      choices = c("W ruchu" = "move", "Zaparkowany" = "parked", "Obie" = "both"),
                                      selected = "both")
        }
        if(input$selectedLanguage == 'EN'){
            shiny::updateRadioButtons(session, "status_port", label = "", 
                                      choices = c("In move" = "move", "Parked" = "parked", "Both" = "both"),
                                      selected = "both")
        }
        if(input$selectedLanguage == 'ES'){
            shiny::updateRadioButtons(session, "status_port", label = "", 
                                      choices = c("Moviente" = "move", "Estacionado" = "parked", "Ambos" = "both"),
                                      selected = "both")
        }
        if(input$selectedLanguage == 'DE'){
            shiny::updateRadioButtons(session, "status_port", label = "", 
                                      choices = c("Ziehen um" = "move", "Geparkt" = "parked", "Beide" = "both"),
                                      selected = "both")
        }
        if(input$selectedLanguage == 'RU'){
            shiny::updateRadioButtons(session, "status_port", label = "", 
                                      choices = c("В Движении" = "move", "Припаркованные" = "parked", "Оба" = "both"),
                                      selected = "both")
        }
        
    })
    
    
}


chart_server <- function(input, output, session) {
    
    i18n <- reactive({
        selected <- input$selectedLanguage
        if (length(selected) > 0 && selected %in% translator$get_languages()) {
            translator$set_translation_language(selected)
        }
        translator
    })
    
    data_sample <- reactive({
        req(input$portname_charts)
        
        data_test <- dataset_ship %>%
            mutate(date=as.factor(date)) %>%
            filter(port == input$portname_charts) %>%
            group_by(date, ship_type) %>%
            summarise(ship_num_distinct=n_distinct(SHIPNAME))
        
        return(data_test)
    })
    
    num_vessels_port <- reactive({
        req(data_sample())
        
        data_sample() %>%
            ggplot(aes(x=date, y=ship_num_distinct, fill = ship_type)) +
            geom_bar(stat="identity", position=position_dodge()) +
            theme_bw() 
    })
    
    
    output$stop_vessels_graph_dur <- renderPlotly({
        
        req(input$portname_charts)
        
        
        dataset_ship %>%
            filter(is_parked == 1) %>%
            filter(port == input$portname_charts) %>% 
            group_by(ship_type, SHIPNAME) %>%
            arrange(date, DATETIME) %>%
            summarise(stop_dur=(mean(as.numeric(difftime(strptime(paste(DATETIME),"%Y-%m-%d %H:%M:%S"),
                                                         strptime(paste(lag(DATETIME)),"%Y-%m-%d %H:%M:%S"))), na.rm = TRUE)/60)) %>%
            ungroup() %>%
            group_by(ship_type) %>%
            summarise(stop_dur=mean(stop_dur, na.rm = TRUE)) %>%
            #Drop unused factor levels in a subsetted data frame
            mutate(ship_type = factor(ship_type)) %>%
            plot_ly(x = ~ship_type, y = ~stop_dur, 
                    type = 'bar', 
                    color = ~ship_type, 
                    colors = 'Pastel1' ,
                    text = ~paste('<br><b>', i18n()$t('Ship type:'), '</b>', ship_type,
                                  '<br><b>', i18n()$t('Stop duration:'), '</b>', round(stop_dur, 3)),
                    hoverinfo = "text"
            ) %>%
            layout(yaxis = list(title = i18n()$t('Average time of a stop in minutes')),
                   xaxis = list(title = '', tickangle = 315),
                   legend = list( orientation = "h", y = -0.4, xanchor = "center", x=0.5),
                   title = i18n()$t('Average stop duration by ship type')
            )
        
        
    })
    
    
    output$num_vessels_graph <- renderPlotly({
        
        req(input$portname_charts)
        
        dataset_ship %>%
            mutate(date=as.factor(format(date, i18n()$t('%d %b %Y')))) %>%
            filter(port == input$portname_charts) %>%
            group_by(date, ship_type) %>%
            summarise(ship_num_distinct=n_distinct(SHIPNAME)) %>%
            #Drop unused factor levels in a subsetted data frame
            mutate(ship_type = factor(ship_type)) %>%
            plot_ly(x = ~date, y = ~ship_num_distinct, 
                    type = 'bar', 
                    color = ~ship_type, 
                    colors = 'Pastel1',
                    text = ~paste('<b>', date, '</b>',
                                  '<br><b>', i18n()$t('Ship type:'), '</b>', ship_type,
                                  '<br><b>', i18n()$t('Number of vessels:'), '</b>', ship_num_distinct),
                    hoverinfo = "text"
            ) %>%
            layout(yaxis = list(title = i18n()$t('Number of vessels')),
                   xaxis = list(title = '', tickangle = 315),
                   barmode = 'stack',
                   legend = list( orientation = "h", y = -0.4, xanchor = "center", x=0.5),
                   title = i18n()$t('Total number of vessels in the port')
            )
        
        
    })
    
    
    output$dwt_total_graph <- renderPlotly({
        dataset_ship %>%
            filter(port == input$portname_charts) %>%
            select(ship_type, SHIPNAME, DWT, date) %>%
            mutate(date=as.factor(format(date, i18n()$t('%d %b %Y')))) %>%
            unique() %>%
            group_by(date, ship_type) %>%
            summarise(dwt_sum = sum(DWT, na.rm = TRUE)) %>%
            #Drop unused factor levels in a subsetted data frame
            mutate(ship_type = factor(ship_type)) %>%
            plot_ly(x = ~date, y = ~dwt_sum, 
                    type = 'bar', 
                    color = ~ship_type, 
                    colors = 'Pastel1',
                    text = ~paste('<b>', date, '</b>',
                                  '<br><b>', i18n()$t('Ship type:'), '</b>', ship_type,
                                  '<br><b>', i18n()$t('Sum capacity [DWT]:'), '</b>', dwt_sum),
                    hoverinfo = "text"
            ) %>%
            layout(yaxis = list(title = i18n()$t('Sum capacity [DWT]')),
                   xaxis = list(title = '', tickangle = 315),
                   barmode = 'stack',
                   legend = list( orientation = "h", y = -0.4, xanchor = "center", x=0.5),
                   title = i18n()$t('Total capacity of vessels in the port')
            )
        
    })
    
    observeEvent(input$selectedLanguage, {
        # This print is just for demonstration
        #print(paste("Language change!", input$selectedLanguage))
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$selectedLanguage)
    })
    
}








table_server <- function(input, output, session) {
    
    i18n <- reactive({
        selected <- input$selectedLanguage
        if (length(selected) > 0 && selected %in% translator$get_languages()) {
            translator$set_translation_language(selected)
        }
        translator
    })
    
    shiptype <- reactive({
        req(input$shiptype_table)
        
        filter(port_info_data(), ship_type == input$shiptype_table)
    })
    
    observeEvent(shiptype(), {
        choices <- unique(shiptype()$SHIPNAME)
        updateSelectInput(session, "shipname_table", NULL, choices = choices)
    })
    
    observeEvent(port_info_data(), {
        choices <- unique(port_info_data()$ship_type)
        updateSelectInput(session, "shiptype_table", NULL, choices = choices)
    })
    
    
    port_info_data <- reactive({
        req(input$portnameAll)
        
        if(input$portnameAll == "St. Petersburg"){
            portData_sample <- dataset_ship[(inpolygon(dataset_ship$LAT, dataset_ship$LON, xp_st_patersburg, yp_st_patersburg, boundary = TRUE)), ] %>%
                filter(hour(DATETIME) >= input$spec_hour_chart[1] & hour(DATETIME) <= input$spec_hour_chart[2]) %>%
                filter(minute(DATETIME) >= input$spec_minute_chart[1] & minute(DATETIME) <= input$spec_minute_chart[2])
            
            return(portData_sample)
        }
        if(input$portnameAll == "Gdańsk"){
            portData_sample <- dataset_ship[(inpolygon(dataset_ship$LAT, dataset_ship$LON, xp_gdansk, yp_gdansk, boundary = TRUE)), ] %>%
                filter(hour(DATETIME) >= input$spec_hour_chart[1] & hour(DATETIME) <= input$spec_hour_chart[2]) %>%
                filter(minute(DATETIME) >= input$spec_minute_chart[1] & minute(DATETIME) <= input$spec_minute_chart[2])
            
            return(portData_sample)
        }
        if(input$portnameAll == "gothenborg"){
            portData_sample <- dataset_ship[(inpolygon(dataset_ship$LAT, dataset_ship$LON, xp_gothenborg, yp_gothenborg, boundary = TRUE)), ] %>%
                filter(hour(DATETIME) >= input$spec_hour_chart[1] & hour(DATETIME) <= input$spec_hour_chart[2]) %>%
                filter(minute(DATETIME) >= input$spec_minute_chart[1] & minute(DATETIME) <= input$spec_minute_chart[2])
            
            return(portData_sample)
        }
        if(input$portnameAll == "Kalingrad"){
            portData_sample <- dataset_ship[(inpolygon(dataset_ship$LAT, dataset_ship$LON, xp_kalingrad, yp_kalingrad, boundary = TRUE)), ] %>%
                filter(hour(DATETIME) >= input$spec_hour_chart[1] & hour(DATETIME) <= input$spec_hour_chart[2]) %>%
                filter(minute(DATETIME) >= input$spec_minute_chart[1] & minute(DATETIME) <= input$spec_minute_chart[2])
            
            
            return(portData_sample)
        }
        if(input$portnameAll == "Klaipeda"){
            portData_sample <- dataset_ship[(inpolygon(dataset_ship$LAT, dataset_ship$LON, xp_kleipeda, yp_kleipeda, boundary = TRUE)), ] %>%
                filter(hour(DATETIME) >= input$spec_hour_chart[1] & hour(DATETIME) <= input$spec_hour_chart[2]) %>%
                filter(minute(DATETIME) >= input$spec_minute_chart[1] & minute(DATETIME) <= input$spec_minute_chart[2])
            
            
            return(portData_sample)
        }
        if(input$portnameAll == "Gdynia"){
            portData_sample <- dataset_ship[(inpolygon(dataset_ship$LAT, dataset_ship$LON, xp_gdynia, yp_gdynia, boundary = TRUE)), ] %>%
                filter(hour(DATETIME) >= input$spec_hour_chart[1] & hour(DATETIME) <= input$spec_hour_chart[2]) %>%
                filter(minute(DATETIME) >= input$spec_minute_chart[1] & minute(DATETIME) <= input$spec_minute_chart[2])
            
            return(portData_sample) 
        }
        if(input$portnameAll == "All"){
            portData_sample <- dataset_ship %>%
                filter(hour(DATETIME) >= input$spec_hour_chart[1] & hour(DATETIME) <= input$spec_hour_chart[2]) %>%
                filter(minute(DATETIME) >= input$spec_minute_chart[1] & minute(DATETIME) <= input$spec_minute_chart[2])
            return(portData_sample) 
        }
    })
    
    
    data_sample <- reactive({
        req(input$shiptype_table)
        req(input$shipname_table)
        req(input$status_table)
        
        
        if(input$status_table == "move"){
            port_info_data() %>%
                filter(ship_type == input$shiptype_table) %>%
                filter(SHIPNAME == input$shipname_table) %>%
                filter(is_parked == 0)
        } else if(input$status_table == "parked"){
            port_info_data() %>%
                filter(ship_type == input$shiptype_table) %>%
                filter(SHIPNAME == input$shipname_table) %>%
                filter(is_parked == 1)
        } else {
            port_info_data() %>%
                filter(ship_type == input$shiptype_table) %>%
                filter(SHIPNAME == input$shipname_table) 
        }
        
    })
    
    filtered_data <- reactive({
        dateSelected <- dmy(input$selectDate_table)
        
        if(length(input$selectDate_table) > 0){
            data_sample() %>%
                filter(date %in% dateSelected)
        } else {
            data_sample()
        }
    })
    
    output$filteredTable = DT::renderDataTable({
        filtered_data() %>%
            select(LON, LAT, SPEED, COURSE, HEADING, ELAPSED, DESTINATION, FLAG, LENGTH, ROT,
                   SHIPNAME, SHIP_ID, WIDTH, L_FORE, W_LEFT, DWT, GT_SHIPTYPE, date, DATETIME, 
                   week_nb, ship_type, is_parked, distance_between_observation)
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("ships-filtered-data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(filtered_data(), file)
        }
    )
    
    observe({
        
        choices <- format(unique(data_sample()$date), i18n()$t('%d %B %Y'))
        
        #month_list <- list(`January` = list(),
        #`February` = list(),
        #`March` = list(),
        #`April` = list(),
        #`May` = list(),
        #`June` = list(),
        #`July` = list(),
        #`August` = list(),
        #`September` = list(),
        #`October` = list(),
        #`November` = list(),
        #`December` = list())
        
        #if(length(choices) < 1){
        #month_list <- choices
        #} else {
        #for (x in 1:length(choices)) {
        #if(length(choices) == 1) {
        #if(month(choices[x]) == '1'){ month_list$January <- list(format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '2'){ month_list$February <- list(format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '3'){ month_list$March <- list(format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '4'){ month_list$April <- list(format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '5'){ month_list$May <- list(format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '6'){ month_list$June <- list(format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '7'){ month_list$July <- list(format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '8'){ month_list$August <- list(format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '9'){ month_list$September <- list(format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '10'){ month_list$October <- list(format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '11'){ month_list$November <- list(format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '12'){ month_list$December <- list(format(choices[x], '%d %B %Y')) }
        #} else {
        #if(month(choices[x]) == '1'){ month_list$January <- append(month_list$January, format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '2'){ month_list$February <- append(month_list$February, format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '3'){ month_list$March <- append(month_list$March, format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '4'){ month_list$April <- append(month_list$April, format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '5'){ month_list$May <- append(month_list$May, format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '6'){ month_list$June <- append(month_list$June, format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '7'){ month_list$July <- append(month_list$July, format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '8'){ month_list$August <- append(month_list$August, format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '9'){ month_list$September <- append(month_list$September, format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '10'){ month_list$October <- append(month_list$October, format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '11'){ month_list$November <- append(month_list$November, format(choices[x], '%d %B %Y')) }
        #if(month(choices[x]) == '12'){ month_list$December <- append(month_list$December, format(choices[x], '%d %B %Y')) }    
        #}
        
        #}
        #}
        
        if(!input$specificDates_table){
            updateSelectInput(session, "selectDate_table", NULL, choices = c("No values"), selected = NULL)
        } else {
            updateSelectInput(session, "selectDate_table", NULL, choices = choices)
        }
        
        
    })
    
    observeEvent(input$selectedLanguage, {
        # This print is just for demonstration
        #print(paste("Language change!", input$selectedLanguage))
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$selectedLanguage)
        
        
        if(input$selectedLanguage == 'PL'){
            shiny::updateRadioButtons(session, "status_table", label = "", 
                                      choices = c("W ruchu" = "move", "Zaparkowany" = "parked", "Obie" = "both"),
                                      selected = "both")
        }
        if(input$selectedLanguage == 'EN'){
            shiny::updateRadioButtons(session, "status_table", label = "", 
                                      choices = c("In move" = "move", "Parked" = "parked", "Both" = "both"),
                                      selected = "both")
        }
        if(input$selectedLanguage == 'ES'){
            shiny::updateRadioButtons(session, "status_table", label = "", 
                                      choices = c("Moviente" = "move", "Estacionado" = "parked", "Ambos" = "both"),
                                      selected = "both")
        }
        if(input$selectedLanguage == 'DE'){
            shiny::updateRadioButtons(session, "status_table", label = "", 
                                      choices = c("Ziehen um" = "move", "Geparkt" = "parked", "Beide" = "both"),
                                      selected = "both")
        }
        if(input$selectedLanguage == 'RU'){
            shiny::updateRadioButtons(session, "status_table", label = "", 
                                      choices = c("В Движении" = "move", "Припаркованные" = "parked", "Оба" = "both"),
                                      selected = "both")
        }
        
    })
    
}



router <- make_router(
    route("/", root_page, server = observationinfo_server),
    route("other", other_page, port_server),
    route("charts", charts_page, chart_server),
    route("table", table_page, table_server)
)

ui <- semanticPage(
    title = "Port analysis", 
    #theme = "flat",
    
    shiny.i18n::usei18n(i18n),
    
    tags$head(
        tags$link(rel="stylesheet", href="style.css", type="text/css" ),
        tags$link(rel="icon", href="https://cdn.iconscout.com/icon/free/png-256/ship-goods-vehicle-transport-delivery-vessel-sailing-1-26761.png")
    ),
    tags$header(i18n$t("Created by Anastasiia Kostiv for Appsilon"), align = "center", style = "
                        position:absolute;
                        top:0;
                        right:0;
                        left:0;
                        margin-bottom: 20px;
                        background: black;
                        color: white;
                        padding: 14px 10px 10px 10px;
                        box-sizing: border-box;
                        z-index: 20000;
                        width: 100%;
                        text-align: center",
                
                tags$div(
                    selectInput('selectedLanguage',
                                #i18n()$t("Change language"),
                                #choices = translator$get_languages(),
                                '',
                                choices = i18n$get_languages(),
                                selected = i18n$get_key_translation()
                    )
                )
                
    ),
    
    #router_ui() 
    router$ui
)

server <- function(input, output, session) {
    #router_server(input, output, session) 
    router$server(input, output, session)
    
    observeEvent(input$selectedLanguage, {
        # This print is just for demonstration
        #print(paste("Language change!", input$selectedLanguage))
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$selectedLanguage)
    })
    
}

shinyApp(ui, server)