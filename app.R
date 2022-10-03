
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

options(cancensus.api_key='CensusMapper_f622f3df6a9035ecbfee37004202c139')
options(cancensus.cache_path = "C:/Users/Alfred/OneDrive/sciences_economiques/FAS1002/Projet_spécial/cache")
dat_DA <- get_census(dataset='CA21', regions=list(CD=c("2466","2465")), vectors=c("v_CA21_1","v_CA21_4","v_CA21_6","v_CA21_386","v_CA21_434","v_CA21_452","v_CA21_497","v_CA21_498","v_CA21_593","v_CA21_596","v_CA21_671","v_CA21_719","v_CA21_767","v_CA21_905","v_CA21_1140","v_CA21_1144","v_CA21_906","v_CA21_907","v_CA21_923","v_CA21_944"), labels="detailed", geo_format="sf", level='DA')
# dat_CT <- get_census(dataset='CA21', regions=list(CD=c("2466","2465")), vectors=c("v_CA21_1","v_CA21_4","v_CA21_6","v_CA21_386","v_CA21_434","v_CA21_452","v_CA21_497","v_CA21_498","v_CA21_593","v_CA21_596","v_CA21_671","v_CA21_719","v_CA21_767","v_CA21_905","v_CA21_1140","v_CA21_1144","v_CA21_906","v_CA21_907","v_CA21_923","v_CA21_944"), labels="detailed", geo_format="sf", level='CT')


dat_DA = dat_DA %>% 
  rename_with(str_replace, pattern = "v_", replacement = "")

dat_DA = dat_DA %>% 
  rename_with(str_replace, pattern = ":.*", replacement = "")


dat_pc <- read.table("pccf_fccp_V2209_2021.txt", header=FALSE,sep=";")
# Read PCCF file to link postal code to DA

dat_pc$V2 = substr(dat_pc$V1, 1, 6)
dat_pc$V3 = substr(dat_pc$V1, 126, 133)
dat_pc$V3 = as.numeric(dat_pc$V3)

dat_pc = dat_pc %>% filter(grepl("^(2465|2466)",V3)) %>% mutate(GeoUID = V3) %>%  mutate(PC = V2)

dat_pc$GeoUID = as.character(dat_pc$GeoUID)

dat_pc = subset(dat_pc, select = c(GeoUID, PC))

dat = left_join(dat_DA, dat_pc, by = 'GeoUID') #Add the postal code as a vector ?
test = merge(dat_DA, dat_pc, by = "GeoUID")

breaks_qt <- classIntervals(dat_DA$CA21_906, n = 9, style = "quantile")
#Break the variable into quantiles

pal_fun <- colorQuantile("YlOrRd", NULL, n = 9)
#Create a function for the color palette
p_popup <- paste0("<strong>Revenu médian des ménages ($) : </strong>", dat_DA$CA21_906)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = 'Portrait Socio-économque MTL'),
  dashboardSidebar(),
  dashboardBody(
    box(plotOutput('plot1'), height = 250),
    box(
      title = "Controle",
      sliderInput('slider','Salaire median',
                  min(test$CA21_906,na.rm = TRUE),
                  max(test$CA21_906,na.rm = TRUE),
                  value = c(min(test$CA21_906, na.rm = TRUE), max(test$CA21_906, na.rm = TRUE))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  histdata <- test$CA21_906
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
      ggplot(test, aes(x=CA21_906)) + 
      geom_histogram(colour="black", fill="white",bins=30)+
      geom_density(alpha=.2, fill="#FF6666") + 
      xlab('Salaire median') +
      scale_x_continuous(labels = comma)
    
    #p + scale_x_continuous(labels = comma)
      
  })
}

# Run the application 
shinyApp(ui, server)
