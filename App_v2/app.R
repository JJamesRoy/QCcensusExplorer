
library(shiny)
library(shinydashboard)
library(rgdal)
library(tidyverse)
library(DT)
library(cancensus)
library(sf)
library(geojsonsf)
library(leaflet)
library(RColorBrewer)
library(classInt)
library(stringr)
library(janitor)
library(RCurl)
library(stringi)
library(ggplot2)
library(scales)

source("scaffold.R")

options(cancensus.api_key='CensusMapper_f622f3df6a9035ecbfee37004202c139')
options(cancensus.cache_path = "./data/cache")
dat_DA <- get_census(dataset='CA21', regions=list(CD=c("2466","2465")), vectors=c("v_CA21_1","v_CA21_4","v_CA21_6","v_CA21_386","v_CA21_434","v_CA21_452","v_CA21_497","v_CA21_498","v_CA21_593","v_CA21_596","v_CA21_671","v_CA21_719","v_CA21_767","v_CA21_905","v_CA21_1140","v_CA21_1144","v_CA21_906","v_CA21_907","v_CA21_923","v_CA21_944"), labels="detailed", geo_format="sf", level='DA')
# dat_CT <- get_census(dataset='CA21', regions=list(CD=c("2466","2465")), vectors=c("v_CA21_1","v_CA21_4","v_CA21_6","v_CA21_386","v_CA21_434","v_CA21_452","v_CA21_497","v_CA21_498","v_CA21_593","v_CA21_596","v_CA21_671","v_CA21_719","v_CA21_767","v_CA21_905","v_CA21_1140","v_CA21_1144","v_CA21_906","v_CA21_907","v_CA21_923","v_CA21_944"), labels="detailed", geo_format="sf", level='CT')


dat_DA = dat_DA %>% 
  rename_with(str_replace, pattern = "v_", replacement = "")

dat_DA = dat_DA %>% 
  rename_with(str_replace, pattern = ":.*", replacement = "")
### Codes postaux
#dat_pc <- read.table("~/R/QcCensusExplorer - Rstudio/pccf_fccp_V2209_2021.txt", header=FALSE, sep=";")
# Read PCCF file to link postal code to DA

#dat_pc$V2 = substr(dat_pc$V1, 1, 6)
#dat_pc$V3 = substr(dat_pc$V1, 126, 133)
#dat_pc$V3 = as.numeric(dat_pc$V3)

#dat_pc = dat_pc %>% filter(grepl("^(2465|2466)",V3)) %>% mutate(GeoUID = V3) %>%  mutate(PC = V2)

#dat_pc$GeoUID = as.character(dat_pc$GeoUID)

#dat_pc = subset(dat_pc, select = c(GeoUID, PC))

#testtest = dat_pc %>% nest(PC)

#testtest1 = dat_pc %>% nest(GeoUID)

#dat = left_join(dat_DA, dat_pc, by = 'GeoUID') #Add the postal code as a vector ?
#test = merge(dat_DA, dat_pc, by = "GeoUID")
###

breaks_qt <- classIntervals(dat_DA$CA21_906, n = 9, style = "quantile")
#Break the variable into quantiles

pal_fun <- colorQuantile("YlOrRd", NULL, n = 9)
#Create a function for the color palette
p_popup <- paste0("<strong>Revenu médian des ménages ($) : </strong>", dat_DA$CA21_906)


#Enlève toutes les lignes non complète
data_complete <- dat_DA[complete.cases(dat_DA$CA21_906), ]

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Portrait Socio-économque MTL"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Dashboard",tabName = "dashboard"),
      menuItem("Widgets",tabName = "widgets")
      
      #sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        #label = "Search...")
    )
  ),
  dashboardBody(
    tabItems(
      #premier item
      tabItem(tabName = 'dashboard',
              fluidRow(
                tabBox(
                  title = "Graphiques",
                  #status = 'primary',
                  id = "tabset1", 
                  height = "300px",
                  tabPanel("Tab1",plotOutput('plot1',height = 450)),
                  tabPanel("Tab2","Tab content 2")
                ),
                tabBox(
                  side = "left", height = "300px",
                  #background = "navy",
                  selected = "Tab1",
                  tabPanel("Tab1",
                           sliderInput('slider','Salaire median',
                                       min(data_complete$CA21_906),
                                       max(data_complete$CA21_906),
                                       value = c(min(data_complete$CA21_906), 
                                                 max(data_complete$CA21_906))),
                           tabPanel("tab2","Tab content 2"),
                           tabPanel("tab3", "Note that when side=right, the tab order is reversed.")
                  )
                )
              )
      ),
      #deuxième item
      tabItem(tabName = 'widgets',
              ('widjet tab content'))
    )
  )
  
)
    
   

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  histdata <- data_complete$CA21_906
  
 un <- reactive({
   df <- data_complete[data_complete$CA21_906 %in% seq(from=min(input$slider), to=max(input$slider), by=1),]
   print(df)
   df
 })
  
  output$plot <- renderPlot({
    #histdata[(input$slider)]
      ggplot(un(),aes(x=CA21_906)) + 
      geom_histogram(colour="black", fill="lightblue",bins=30)+
      xlab('Salaire median')+ 
      scale_x_continuous(labels = comma)+
      scale_y_continuous(labels = comma) + 
      theme(text = element_text(size = 20))
    input$tabset1
        #geom_vline(aes(xintercept=mean()),
                   #color="red", linetype="dashed", size=1)
      
  })
  

 
}


# Run the application 
shinyApp(ui, server)
