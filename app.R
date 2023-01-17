#Assessed Project MAS61004 The Statistician's Toolkit
#Group 6: 200233880, 2002333743, 200233765, 200233857

# TODO: adjust file paths.

# ---- packages ----
library(shiny)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(imputeTS)
library(shiny)
library(leaflet)
library(sf)
library(rmapshaper)
library(zoo)
library(lubridate)
library(extrafont)
library(sjlabelled)
library(sjPlot)
library(ggeffects)
library(knitr)


# ---- preamble ----

load(here::here("data/dataSets.RData")) 


# ---- data preparation ----
accidents$Accident_Severity_Rec <- to_factor(accidents$Accident_Severity_Rec)
accidents$Special_Conditions_Rec <- to_factor(accidents$Special_Conditions_Rec)
accidents$Weekend_Rec <- to_factor(accidents$Weekend_Rec)
accidents$Young_Driver <- to_factor(accidents$Young_Driver)
accidents$Casualty_Class_Rec <- to_factor(accidents$Casualty_Class_Rec)
accidents$Speed_limit <- to_factor(accidents$Speed_limit)
accidents <- accidents %>% filter(!is.na(Speed_limit))



# ---- GLM ----
model <- glm(formula = Accident_Severity_Rec ~ Speed_limit +
                 Weekend_Rec +
                 Special_Conditions_Rec +
                 Young_Driver +
                 Casualty_Class_Rec,
             family =binomial(link="logit"),
             data = accidents)

# ---- shape files ----
#this are commands for the shapefile of the LA highway authority map
# downloaded from... https://geoportal.statistics.gov.uk/datasets/6638c31a8e9842f98a037748f72258ed_0


boundaries.17 <- st_read(dsn = here::here("data/Counties17"),
                         layer = "Counties_and_Unitary_Authorities_(December_2017)_Boundaries_UK") %>%
    st_transform(crs = 4326)

simplifiedBoundaries.17 <-rmapshaper::ms_simplify(boundaries.17)

country <- rep("red", 217)
country[153:163] <- "white"
country[164:195] <- "blue"
country[196:217] <- "green"

#The shape file has new LA Highway codes, where the accidents data contains some legacy codes.
#details of ONS internal migration here
#https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/methodologies/interalmigrationmethodology/internalmigrationqa2013tcm77368508.pdf
simplifiedBoundaries.17$ctyua17cd[56] <- "E06000048"
simplifiedBoundaries.17$ctyua17cd[92] <- "E08000020" 


# ---- User Interface ----
# Define UI for risk app
ui <- fluidPage(
    
    titlePanel("Road Traffic Accident Risk Tool"),
    
    sidebarLayout(
        
        sidebarPanel(
            #An input for each predictor variable other than speed in the model
            #Junction Control, Pedestrian Crossing facilities, Road Type
            #Initially give way, with no crossing facilities at a roundabout (in Sheffield) 
            selectInput(inputId = "weekday.weekend",
                        label=h3("Day type"), 
                        choices = list("Weekday (Monday to Friday)" = 0,
                                       "Weekend (Saturday or Sunday) (default)" = 1),
                        selected = 1),
            selectInput(inputId = "special.conditions",
                        label=h3("Special conditions"), 
                        choices = list("None, or obstruction other than roadworks or defective road surface (default)" = 0,
                                       "Roadworks or defective road surface" = 1),
                        selected = 0),
            selectInput(inputId = "young.driver",
                        label=h3("Age of drivers"), 
                        choices = list("No young drivers (default)" = 0,
                                       "At least one young driver" = 1),
                        selected = 0),
            selectInput(inputId = "casualty.class",
                        label=h3("Category of casualties"), 
                        choices = list("Driver, rider or vehicle passengers only (default)" = 1,
                                       "Pedestrians only" = 2,
                                       "At least one driver, rider or vehicle passenger, and at least one pedestrian" = 3),
                        selected = 1),
            leafletOutput("LAhighwaymap")
            
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("2019 Crash Map", leafletOutput("crashmap19")),
                tabPanel("Plot", plotOutput("plot"))
                
            )
        )
    )
)



# ---- Server ----
# Define server logic required to draw a histogram
server <- function(input, output){
    
    observeEvent(input$LAhighwaymap_shape_click,{
        p <- input$LAhighwaymap_shape_click
        print(p)
    })

    
    output$plot <- renderPlot({
            p <- ggpredict(model,
                           terms = c("Speed_limit"),
                           condition = c(Weekend_Rec = input$weekday.weekend,
                                         Special_Conditions_Rec = input$special.conditions,
                                         Young_Driver = input$young.driver,
                                         Casualty_Class_Rec = input$casualty.class))
            plot(p) +
                scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(x*100, "%")) +
                labs(
                    x ="Speed Limit (mph)",
                    y = "Probability (risk)", 
                    title = "Conditional probability of a serious or fatal accident given selected variables"
                )
            
    })

    
    output$LAhighwaymap <- renderLeaflet(
        leaflet() %>%
            setView(lng = -1.47, lat = 52.3, zoom = 6) %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
            addPolygons(data = simplifiedBoundaries.17,
                        fillOpacity = 0.05,
                        weight  = 1,
                        popup = ~ctyua17nm,
                        fillColor = country,
                        color = country,
                        layerId = ~ctyua17cd)
    )
    
    # output$summary <- renderPrint({
    #     "The plot tab shows the conditional probability of a serious or fatal accident occuring given the values of the variables selected at the various speed limits."
    # })
   
    
    output$crashmap19 <- renderLeaflet({
        if( is.null(input$LAhighwaymap_shape_click[1]) ){
            temp.code <- c( simplifiedBoundaries.17$ctyua17cd[75], "*", simplifiedBoundaries.17$lat[75], simplifiedBoundaries.17$long[75] )
        } else {
            temp.code <- input$LAhighwaymap_shape_click
        }
        
        LA_accidents <- accidents %>%
            filter(!is.na(Longitude)| !is.na(Latitude)) %>%
            subset( `Local_Authority_(Highway)` == as.character( temp.code[1]) ) %>%
            subset( Weekend_Rec == as.character(input$weekday.weekend)) %>%
            subset( Special_Conditions_Rec == as.character(input$special.conditions) ) %>%
            subset( Young_Driver == as.character(input$young.driver) ) %>%
            subset( Casualty_Class_Rec == as.character(input$casualty.class) )
        
        pal <- colorFactor( palette = c("darkblue", "red"), domain = LA_accidents$Accident_Severity_Rec )
        
        leaflet(LA_accidents) %>%
            setView(lng = as.numeric(temp.code[4]), lat = as.numeric(temp.code[3]), zoom = 8) %>%
            addProviderTiles(providers$CartoDB.VoyagerLabelsUnder) %>%
            addCircleMarkers(lng = LA_accidents$Longitude,
                            lat = LA_accidents$Latitude,
                            color = ~pal(LA_accidents$Accident_Severity_Rec),
                            popup = paste0("Speed limit: ", LA_accidents$Speed_limit, "mph" ,"<br>",
                                            "Date: ", LA_accidents$Date, "<br>",
                                            "Casualties: ", LA_accidents$Number_of_Casualties , "<br>")
            ) %>%
            addLegend(
                "bottomleft", 
                colors = c("red", "darkblue"), 
                labels = c("Fatal or Serious","Slight"),
                opacity = 1,
                title="Accident Severity"
            )
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
