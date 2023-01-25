library(shinyTime)
library(ggmap)
library(jsonlite)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(gifski)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(knitr)
library(ggplot2)
library(ggmap)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(scales)
library(formattable)
library(rgdal)
library(raster)
library(shinycssloaders)
library(gganimate)
library(png)
library(gapminder)
library(data.table)
library(rworldmap)
library(htmlwidgets)
library(XML)
library(jsonlite)
library(curl)
library(readxl)
library(mapview)
library(datefixR)
library(tm)
library(tidytext)
library(shiny)
library(wordcloud2)
library(shinyWidgets)
library(fmsb)
library(transformr)
library(processanimateR)
library(wordcloud)
library(rsconnect)
ggmap::register_google(key = "AIzaSyDdu1fz1bRRsjAtAqGWZqs_feER-yYSeN4")


# function to count symptom types
countSymptomType <- function(df, colsymptom)
{
  df[, colsymptom] <- gsub(" ", "", df[, colsymptom])
  symptoms_df <- data_frame(Text = df[, colsymptom])
  symptoms_words <- symptoms_df %>% unnest_tokens(output = word, 
                                                  input = Text) 
  symptoms_typecounts <- symptoms_words %>% 
    count(word, sort = TRUE)
  return(symptoms_typecounts)
}

###############################################################################

# function to generate leaflet based on filters
getleaf <- function(Gender, minAge, maxAge, 
                    startDate, endDate, 
                    Hospital, Nationality, Data){
  
  df <- Data
  
  # filter gender
  genderfix <- tolower(substr(Gender, 1, 1))
  df <- df[df[, "gender"] %in% genderfix, ]
  
  # filter age
  df <- df %>% filter(age >= minAge & age <= maxAge)
  
  # filter dates
  df <- df %>% filter(date >= startDate & date <= endDate)
  
  # filter nationality
  nationfix <- tolower(Nationality)
  df <- df[df[, "nationality"] %in% nationfix, ]
  
  # filter hospital
  hospitalfix <- tolower(Hospital)
  df <- df[df[, "hospital"] %in% hospitalfix, ]
  
  return(leaflet() %>% setView(lat = 1.290270, lng = 103.851959, zoom = 10) %>% 
           addTiles() %>% addProviderTiles(providers$CartoDB.Positron) %>%
           addCircleMarkers(data = df, lng = ~`Longitude`, lat = ~`Latitude`, 
                            radius = 5, clusterOptions =  markerClusterOptions(),
                            popup = df$address))
}

# function to retrieve data for animation map:
getdata <- function(Gender, minAge, maxAge, 
                              startDate, endDate, 
                              Hospital, Nationality, Data){
  
  df <- Data
  
  # filter gender
  genderfix <- tolower(substr(Gender, 1, 1))
  df <- df[df[, "gender"] %in% genderfix, ]
  
  # filter age
  df <- df %>% filter(age >= minAge & age <= maxAge)
  
  # filter dates
  df <- df %>% filter(date >= startDate & date <= endDate)
  
  # filter nationality
  nationfix <- tolower(Nationality)
  df <- df[df[, "nationality"] %in% nationfix, ]
  
  # filter hospital
  hospitalfix <- tolower(Hospital)
  df <- df[df[, "hospital"] %in% hospitalfix, ]
  
  return(df)
}

###############################################################################
# UI:
ui <- dashboardPage(
  dashboardHeader(title = "Disease Tracker"),
  
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("layer-group",
                                                              class="fa-thin fa-layer-group")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-simple",
                                                             class="fa-thin fa-chart-simple")),

    # slider for age
    sliderInput(inputId = "age", label = "Select Age Range:", 
                min = 0, max = 105, value = c(40, 60)),
    
    # checkbox for gender
    checkboxGroupInput(
      inputId = "gender",label = "Choose Gender:",choices = c("Male","Female"),
      selected = c("Male", "Female"),inline = TRUE),
    
    #rangebox for date
    dateRangeInput(inputId = "date", label = "Select Dates:",
                   start = "2020-01-23", end = "2020-04-14",
                   min = "2020-01-23", max = "2020-04-14",
                   format = "yyyy-mm-dd"),
    
    #checkbox for hospital
        checkboxGroupInput(
          inputId = "hospital", label = "Select Hospital to View:",
          choices = c("NTFGH","SKH","NUH","CIF","NCID","KTPH","SGH",
                      "TTSH","CGH","AH","KKH","MEN","FPH", "MENH","B","MAH","MEH","PEH"),
          selected = c("NTFGH","SKH","NUH","CIF","NCID","KTPH","SGH",
                       "TTSH","CGH","AH","KKH","MEN","FPH", "MENH","B","MAH","MEH","PEH"),
          inline = TRUE),
        
      #multiple selections for nationality
        multiInput(inputId = "nationality", label = "Select Nationality:",
                   choices = c("Myanmar","China","Bangladesh","India","Singapore PR","Singapore",
                               "Spain","Thailand","Indonesia","Ireland","UK","Malaysia",
                               "Philippines","Cameroon","Australia","Italy","France","USA",
                               "Brazil","Vietnam","Sweden","Switzerland","Netherlands",
                               "Slovakia","South Africa","Germany","UAE","Colombia",
                               "Sri Lanka","New Zealand","South Korea","Oman","Russia",
                               "Taiwan","Belgium","Japan","Canada","Denmark","Venezuela"),
                   
                   selected = c("Myanmar","China","Bangladesh","India","Singapore PR","Singapore",
                                "Spain","Thailand","Indonesia","Ireland","UK","Malaysia",
                                "Philippines","Cameroon","Australia","Italy","France","USA",
                                "Brazil","Vietnam","Sweden","Switzerland","Netherlands",
                                "Slovakia","South Africa","Germany","UAE","Colombia",
                                "Sri Lanka","New Zealand","South Korea","Oman","Russia",
                                "Taiwan","Belgium","Japan","Canada","Denmark","Venezuela"))
    
  )
  ),
  
  
  dashboardBody(
    fluidRow(
      tabItems(
        
        # FIRST PAGE
        tabItem(tabName = "overview",
                infoBoxOutput("totalcase"),
                infoBoxOutput("totaldeath"),
  
                tabBox(id = "tabset1",
                  # output panel for static map
                  tabPanel(title = "Overall Distribution in Singapore",
                             withSpinner(leafletOutput(outputId = "sgmap", 
                                                       height = 500)), 
                             status = "primary", solidHeader = TRUE),
                  # output panel for animated map
                  tabPanel(title = "Distribution Over Time (Animated)",
                    withSpinner(plotOutput(outputId = "sgmapanimate"))),
                  width = 600,
                  height = 500
                  ),
                
                # control panel for maptype
                #box(title = "Change Animated Maptype",
                absolutePanel(id = "maptypecontrol", class = "panel panel-default",
                              top = 110, left = 900, width = 100, fixed=TRUE,
                              draggable = TRUE, height = "auto",
                              
                              radioButtons(inputId = "maptype",label="Map Type (Animated)",
                             choices = c("roadmap","satellite","terrain","hybrid",
                                         "toner","toner-lite","terrain-background","watercolor"),
                             selected = "hybrid")),
                
                box(title = "Cumulative Cases",
                    withSpinner(plotOutput(outputId = "caseCum", height = 250)),
                    height = 300, status = "primary",solidHeader = TRUE, collapsible = TRUE),
                
                box(title = "Cumulative Deaths",
                    withSpinner(plotOutput(outputId = "deathCum", height = 250)),
                    height = 300, status = "primary", solidHeader = TRUE, collapsible = TRUE)
                ),
        
        # SECOND PAGE
        tabItem(tabName = "analysis",
                
                box(title = "Age vs Recovery",
                    withSpinner(plotOutput(outputId = "scatter",width = 500, height = 600)),
                    height = 600, status = "warning", solidHeader = TRUE),
                
                box(title = "Recovery Rate",
                    withSpinner(plotOutput(outputId = "recovery", width = 150, height = 600)),
                    height = 600, status = "primary", solidHeader = TRUE),
                
                box(title = "Symptoms WordCloud",
                    withSpinner(plotOutput(outputId = "symptomcloud",width = 200, height = 300)),
                    height = 300, status = "warning", solidHeader = TRUE),
                
                box(title = "Symptom Types",
                    withSpinner(plotOutput(outputId = "radar", width = 200, height = 300)),
                    height = 300, status = "primary", solidHeader = TRUE)
                
                
                
        )
      )
    )
  )
)



          
      
# SERVER:
server <- function(input, output) 
{
  getMap <- reactive({
    get_map('Singapore', zoom = 11, maptype = input$maptype)})
  
  df <- read.csv("cleansed_data.csv")
  output$totalcase <- renderInfoBox({infoBox("Total Cases", value = nrow(getdata(Gender = input$gender,
                                                                                 minAge = input$age[1],
                                                                                 maxAge = input$age[2],
                                                                                 startDate = input$date[1],
                                                                                 endDate = input$date[2],
                                                                                 Nationality = input$nationality,
                                                                                 Hospital = input$hospital,
                                                                                 Data = df)), 
                                             icon = icon("globe", class ="fa-thin fa-globe"))})
  
  output$totaldeath <- renderInfoBox({infoBox("Total Deaths", value = getdata(Gender = input$gender,
                                                                                  minAge = input$age[1],
                                                                                  maxAge = input$age[2],
                                                                                  startDate = input$date[1],
                                                                                  endDate = input$date[2],
                                                                                  Nationality = input$nationality,
                                                                                  Hospital = input$hospital,
                                                                                  Data = df) 
                                              %>% filter(!is.na(deceased)) %>% dplyr::select(9) %>% sum(),
                                              icon = icon("heart-pulse", class="fa-thin fa-heart-pulse"))})

  output$sgmap <- renderLeaflet({
    df <- read.csv("cleansed_data.csv")
    # handling age
    getleaf(Gender = input$gender,
            minAge = input$age[1],
            maxAge = input$age[2],
            startDate = input$date[1],
            endDate = input$date[2],
            Nationality = input$nationality,
            Hospital = input$hospital,
            Data = df)
  })
  
  output$sgmapanimate <- renderImage({
    df <- read.csv("cleansed_data.csv")
    outfile <- tempfile(fileext = ".gif")
    
    p  <- ggmap(getMap()) + geom_point(
      data = getdata(Gender = input$gender,
                     minAge = input$age[1],
                     maxAge = input$age[2],
                     startDate = input$date[1],
                     endDate = input$date[2],
                     Nationality = input$nationality,
                     Hospital = input$hospital,
                     Data = df),aes(x = Longitude, y = Latitude), shape = 23, 
      fill = "lightblue", color = "blue", size = 3) + transition_time(as.Date(date)) + 
      labs(title = "Duration:{frame_time}")
    
    anim_save("outfile.gif", animate(p))
    list(src = "outfile.gif",
         contentType = 'image/gif',
         deleteFile = TRUE)
  })
  
  output$caseCum <- renderPlot({
    # transform total cases data
    df <- read.csv("cleansed_data.csv")
    
    filtered <- getdata(Gender = input$gender, 
           minAge = input$age[1], 
           maxAge = input$age[2], 
           startDate = input$date[1], 
           endDate = input$date[2], 
           Nationality = input$nationality,
           Hospital = input$hospital, 
           Data = df)
    
    total.cases <- filtered %>% group_by(date) %>% summarise(total= n()) %>% mutate(cum = cumsum(total))
    filtered$date <- ymd(filtered$date)
    total.cases$date <- as.Date(total.cases$date)
    total.cases <- merge(filtered, total.cases, by="date")
    
    # plot line graph
    ggplot(data = total.cases, aes(x = date, y = cum)) + 
        geom_line(color="#69b3a2", group=1) + 
        geom_point(size = 1, alpha = 0.5) + 
        ylab("Cumulative cases") +  xlab("Date") + theme_bw() +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 12, 5, 5)) 
    })
  
  output$deathCum <- renderPlot({
    # transform total deaths data
    df <- read.csv("cleansed_data.csv")
    
    filtered <- getdata(Gender = input$gender, 
            minAge = input$age[1], 
            maxAge = input$age[2], 
            startDate = input$date[1], 
            endDate = input$date[2], 
            Nationality = input$nationality,
            Hospital = input$hospital, 
            Data = df)

    total.death <- filtered %>% group_by(date) %>% filter(deceased=="TRUE") %>% summarise(total= n()) %>% mutate(cum = cumsum(total))
    filtered$date <- ymd(filtered$date)
    total.death$date <- as.Date(total.death$date)
    total.death <- merge(filtered, total.death, by="date")
    
    # plot line graph
    ggplot(data = total.death, aes(x = date, y = cum)) + 
      geom_line( color="red", group=1) + geom_point(size = 1, alpha = 0.5) +
      ylab("Cumulative Death") +  xlab("Date") + theme_bw() +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
            plot.margin = margin(5, 12, 5, 5))
    
  })
  
  output$radar <- renderPlot({
    symptom_type <- countSymptomType(df = getdata(Gender = input$gender,
                                                  minAge = input$age[1], 
                                                  maxAge = input$age[2], 
                                                  startDate = input$date[1], 
                                                  endDate = input$date[2], 
                                                  Nationality = input$nationality,
                                                  Hospital = input$hospital, 
                                                  Data = df), 17)
    symptom_type <- spread(symptom_type, 1, 2)
    colnames(symptom_type)[6] <- c("nosymptom")
    symptom_type <- rbind(rep(3352,6) , rep(0,6) , symptom_type)
    radarchart(symptom_type, axistype = 1,
               #custom polygon
               pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
               
               #custom the grid
               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,3352,500), cglwd=0.8,
               
               #custom labels
               vlcex=0.8 )}, 
    width = 400, height = 250)
  
  output$symptomcloud <- renderPlot({
    df <- read.csv("cleansed_data.csv")
    df$symptoms <- ifelse(is.na(df$symptoms), "nosymptoms", df$symptoms)
    symp <- countSymptomType(df = getdata(Gender = input$gender,
                                          minAge = input$age[1], 
                                          maxAge = input$age[2], 
                                          startDate = input$date[1], 
                                          endDate = input$date[2], 
                                          Nationality = input$nationality,
                                          Hospital = input$hospital, 
                                          Data = df), 17)
    colnames(symp) <- c("type", "count")
    
    wordcloud(words = symp$type, freq = symp$count, min.freq = 1,
              max.words = 2000, scale= c(1,1),
              colors=brewer.pal(8, "Accent"))
    }, width = 400, height = 250)
  
  output$recovery <- renderPlot({
    # Recovery duration 
    
    df$date<-as.Date(df$date)
    df$date.discharged<-as.Date(df$date.discharged)
    
    df$recoveryduration<-difftime(df$date.discharged,df$date,units = "days" )
    
    tmp <-getdata(Gender = input$gender,
                   minAge = input$age[1], 
                   maxAge = input$age[2], 
                   startDate = input$date[1], 
                   endDate = input$date[2], 
                   Nationality = input$nationality,
                   Hospital = input$hospital, 
                   Data = df)
    tmp <- tmp %>% group_by(recoveryduration) %>% filter(is.recovered=="TRUE") %>% 
      summarise(numberofrecovered=n())
    
    ggplot(tmp, aes(x=recoveryduration, y=numberofrecovered)) + 
      geom_bar( fill="#c7ebfd", width = 0.8, stat="identity") +
      ylab("Number of Recovered Patients") +  xlab("Recovery Duration") + 
      theme_bw() + ggtitle("Distribution of Recovery over Duration")
    }, width = 500, height = 400)
  
  output$scatter <- renderImage(
    {outfile <- tempfile(fileext = ".gif")
    filtered <- getdata(Gender = input$gender,
                        minAge = input$age[1], 
                        maxAge = input$age[2], 
                        startDate = input$date[1], 
                        endDate = input$date[2], 
                        Nationality = input$nationality,
                        Hospital = input$hospital, 
                        Data = df)
    
    filtered$recoveryduration<-difftime(filtered$date.discharged,filtered$date,units = "days" )
    
    p <- ggplot(data = filtered, aes(x=age, y=recoveryduration,size=age)) + 
      geom_point(aes(color = "lightblue")) + theme(axis.text = element_text(size = 8)) + 
      ylab("Recovery Duration") +  xlab("Age") + theme_bw() + ggtitle("Number of cases across ages") + 
      transition_time(ymd(filtered$date)) + labs(title = "date: {frame_time}") 
    
    anim_save("outfile.gif", animate(p))
    list(src = "outfile.gif",
         contentType = 'image/gif',
         deleteFile = TRUE)
  })
}

shinyApp(ui = ui, server = server)


    


  
    
    
