#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(rgdal)
library(tidyverse)
library(DT)

options(cancensus.api_key='CensusMapper_f622f3df6a9035ecbfee37004202c139')
options(cancensus.cache_path = "C:/Users/Alfred/OneDrive/sciences_economiques/FAS1002/Projet spécial/cache")
dat_DA <- get_census(dataset='CA21', regions=list(CD=c("2466","2465")), vectors=c("v_CA21_1","v_CA21_4","v_CA21_6","v_CA21_386","v_CA21_434","v_CA21_452","v_CA21_497","v_CA21_498","v_CA21_593","v_CA21_596","v_CA21_671","v_CA21_719","v_CA21_767","v_CA21_905","v_CA21_1140","v_CA21_1144","v_CA21_906","v_CA21_907","v_CA21_923","v_CA21_944"), labels="detailed", geo_format="sf", level='DA')
# dat_CT <- get_census(dataset='CA21', regions=list(CD=c("2466","2465")), vectors=c("v_CA21_1","v_CA21_4","v_CA21_6","v_CA21_386","v_CA21_434","v_CA21_452","v_CA21_497","v_CA21_498","v_CA21_593","v_CA21_596","v_CA21_671","v_CA21_719","v_CA21_767","v_CA21_905","v_CA21_1140","v_CA21_1144","v_CA21_906","v_CA21_907","v_CA21_923","v_CA21_944"), labels="detailed", geo_format="sf", level='CT')


dat_DA = dat_DA %>% 
  rename_with(str_replace, pattern = "v_", replacement = "")

dat_DA = dat_DA %>% 
  rename_with(str_replace, pattern = ":.*", replacement = "")

breaks_qt <- classIntervals(dat_DA$CA21_906, n = 9, style = "quantile")
#Break the variable into quantiles

pal_fun <- colorQuantile("YlOrRd", NULL, n = 9)
#Create a function for the color palette
p_popup <- paste0("<strong>Revenu médian des ménages ($) : </strong>", dat_DA$CA21_906)


ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(title = "portrait socio-economique de Montreal"),
  dashboardSidebar(
    sliderInput("Salaire_median", label = "Salaire Median",
                min = min(dat_DA$CA21_906, na.rm = TRUE),
                max = max(dat_DA$CA21_906, na.rm = TRUE),
                value = c(min(dat_DA$CA21_906, na.rm = TRUE), max(dat_DA$CA21_906, na.rm = TRUE)),
                sep = "",
                step = 1)
  ),
  dashboardBody(
    fluidRow(box(width = 12, leafletOutput(outputId = "mymap"))),
    fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table")))
  )
)


server <- function(input, output) {
  
  data_input <- reactive({
    dat_DA %>% 
    filter(CA21_906 >= input$Salaire_median[1]) %>% 
    filter(CA21_906 <= input$Salaire_median[2]) %>% 
    group_by(`Area (sq km)`) %>% 
    summarize(CA21_906 = n())
  })  

  #data_input_ordered <- reactive({
    #data_input()[order(match(data_input()$CA21_906))]
  #})
    
  #lablels <- reactive ({
   # paste("<p>", data_input_ordered()$`Area (sq km)`,"<p>",
    #      "<p>","Solve Rate", round(data_input_ordered()))
 # })
  
  
  output$mymap <- renderLeaflet(
    leaflet(dat_DA) %>%
      addPolygons(
        stroke = TRUE, color = "black", opacity = 0.2, weight = 1.5,
        fillColor = ~pal_fun(CA21_906),
        fillOpacity = 0.75, smoothFactor = 0.5,
        highlightOptions = highlightOptions(color = "white", weight = 2,
                                            bringToFront = TRUE),
        popup = p_popup) %>%
      addTiles() #%>%
      #addLegend("bottomright", 
                #colors = brewer.pal(9, "YlOrRd"), 
                #labels = paste0("up to ", format(breaks_qt$brks[-1], digits = 2)),
                #title =  'Revenu moyen des ménages en $')
  )
  
  output$summary_table <- renderDataTable(data_input())

}

# Run the application 
shinyApp(ui = ui, server = server)
