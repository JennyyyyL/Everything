#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load all needed packages
library(readr)
library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)
library(shinythemes)
library(geojsonio)
library(leaflet)

#reading necessary dataset into R
#data for model building
tidyWDI <- read.csv("tidyWDI.csv")
tidyWDI.1 <- select(tidyWDI, -1) 
tidyWDI.1<-select(tidyWDI,year, Response, E1, E2, E3, E4, E5,E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, E17, E18, E19, E20,E21, E22, E23, E24, E25)
tidyWDI.subset1<-select(tidyWDI,year,Response,E3,E4,E5,E19,E17,E14,E10,E22,E9,E8,E12,E7,E11,E21)
tidyWDI.subset1<-na.omit(tidyWDI.subset1)
tidyWDI.subset2<-select(tidyWDI,year,Response,E17,E14,E10,E22,E9,E8,E12,E7,E11,E21)
tidyWDI.subset2<-na.omit(tidyWDI.subset2)

#basic data for building map
tidyWDIwithname <-read.csv("tidyWDIwithname.csv")
tidyWDIwithname.noyear <-select(tidyWDIwithname,-1:-4)
map.data <-read.csv("datawithLatLong.csv")

# data set based on specific indicator
children.out.school <- select(map.data, 2,3,4,5,38,39)
contraceptive.prevalence <- select(map.data, 2,3,4,6,38,39)
female.bachelor <- select(map.data, 2,3,4,7,38,39) 
male.bachelor <- select(map.data, 2,3,4,8,38,39)
female.master <- select(map.data, 2,3,4,9,38,39)
male.master <- select(map.data, 2,3,4,10,38,39)
female.doctoral <- select(map.data, 2,3,4,11,38,39)
male.doctoral <- select(map.data, 2,3,4,12,38,39)
female.agriculture <- select(map.data, 2,3,4,13,38,39)
female.industry <- select(map.data, 2,3,4,14,38,39)
female.services <- select(map.data, 2,3,4,15,38,39)
female.management <- select(map.data, 2,3,4,16,38,39)
fertility.rate <- select(map.data, 2,3,4,17,38,39)
gdp <- select(map.data, 2,3,4,18,38,39)
gdp.per.capita <- select(map.data, 2,3,4,19,38,39)
inflation <- select(map.data, 2,3,4,20,38,39)
female.participation <- select(map.data, 2,3,4,21,38,39)
land.area <- select(map.data, 2,3,4,22,38,39)
life.expectancy <- select(map.data, 2,3,4,23,38,39)
maternal.death <- select(map.data, 2,3,4,24,38,39)
female.population <- select(map.data, 2,3,4,25,38,39)
anemia.non.pregnant <- select(map.data, 2,3,4,26,38,39)
anemia.pregnant <- select(map.data, 2,3,4,27,38,39)
participation.ratio <- select(map.data, 2,3,4,28,38,39)
research.expenditure <- select(map.data, 2,3,4,29,38,39)
self.employed.female<- select(map.data, 2,3,4,30,38,39)
suicide.rate<- select(map.data, 2,3,4,31,38,39)
urban.population <- select(map.data, 2,3,4,32,38,39)
business.index <- select(map.data, 2,3,4,33,38,39)
education.gap.bachelor<- select(map.data, 2,3,4,34,38,39)
education.gap.master <- select(map.data, 2,3,4,35,38,39)
education.gap.doctoral <- select(map.data, 2,3,4,36,38,39)

#Load data for countries' longitudes and latitudes
location<-read.csv("https://raw.githubusercontent.com/albertyw/avenews/master/old/data/average-latitude-longitude-countries.csv")
colnames(location)[2] <- "location"
location[185,2]<-"Russia"

#import the necessary code for the shape of polygons of different countries in the world
shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
#name a new data based on the imported code about shapes of polygons
WorldCountry <- geojson_read(shapeurl, what = "sp")

# Define UI for application
ui <- fluidPage(theme = shinytheme ("united"),
                #"united" is the theme for the app selected from the shinytheme package
                navbarPage("World Development Indicators",
                           #The first tab that visualize the total cases/deaths by making a reactive map
                           tabPanel("Map",
                                    titlePanel("World Development Indicator Map"),
                                    helpText("Click on the country to see label."),
                                    sidebarLayout(
                                        #sidebar panel for inputs ----
                                        sidebarPanel(
                                            selectInput("var", 
                                                        label = "Variable",
                                                        choices = c(colnames(tidyWDIwithname.noyear)),
                                                        selected = "Labor.force.participation.rate..female....of.female.population.ages.15....modeled.ILO.estimate."),
                                            numericInput("year",
                                                         label = "Year",2015, min = 1990, max = 2020)
                                        ),
                                        #main panel for displaying outputs
                                        #output: a reactive map
                                        mainPanel(leafletOutput("map")) 
                                    )
                           ), #end of tab 1-world map for different indicators
                           
                           tabPanel("Models",
                               titlePanel("Example Models for Femlale Labor Participation Rate"),
                               sidebarPanel(
                                   selectInput ("model",
                                                label = "Pairs of example model",
                                                choices = c("Example 1 (Best subset 1 with 16 variables)", 
                                                            "Example 2 (Adding E1 to three-variable model of Response~E23+E24+E25)", 
                                                            "Example 3 (Best subset 2 with 12 variables)", 
                                                            "Example 4 (Anemia model - adding E16 to one-variable model of Response~E15)"),
                                                selected = "Example 1 (Best subset 1 with 16 variables)"),
                                   radioButtons("residualnormality", 
                                                label = "Normality Plot of residuals",
                                                choices = list("Yes", "No"), 
                                                selected = "Yes"),
                                   radioButtons("residualfit", 
                                                label = "Residual Versus Fitted Value  Plot",
                                                choices = list("Yes", "No"), 
                                                selected ="Yes"),
                                   radioButtons("residualorder", 
                                                label = "Residual Versus Order Plot",
                                                choices = list("Yes", "No"), 
                                                selected = "Yes"),
                                   radioButtons("visualization", 
                                                label = "Visualization",
                                                choices = list("Yes", "No"), 
                                                selected = "Yes"),
                                   verbatimTextOutput("summarytable1"),
                                   verbatimTextOutput("summarytable2"),
                               ),
                               mainPanel(
                                   plotOutput("normalityplot1"),
                                   plotOutput("normalityplot2"),
                                   plotOutput("fitplot1"),
                                   plotOutput("fitplot2"),
                                   plotOutput("orderplot1"),
                                   plotOutput("orderplot2"),
                                   plotOutput("visualization1"),
                                   plotOutput("visualization2")
                               )
                           ),#end of tab 2
                           
                           
                           tabPanel (
                               "References",# Name of Panel 4
                               titlePanel("References"), 
                               #add title for the 4th panel
                               h3("Source of Original Data :"),
                               h4("https://datacatalog.worldbank.org/dataset/world-development-indicators"),
                               h3("Source of polygons used in the map visualization:"),
                               h4("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"),
                               h3("Source of longtitude and latitude of different countries:"),
                               h4("https://raw.githubusercontent.com/albertyw/avenews/master/old/data/average-latitude-longitude-countries.csv")
                           ) #end of tab 3, the References page and the corresponding url
                           
                ))#end of ui

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Tab 1
    #generate different data set for different input for "year"
    data1 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                      filter(children.out.school, year == input$year), 
                      by = c("id" ="Country.Code"))})
    data2 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(contraceptive.prevalence, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data3 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(female.bachelor, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data4 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(male.bachelor, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data5 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(female.master, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data6 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(male.master, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data7 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(female.doctoral, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data8 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(male.doctoral, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data9 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(female.agriculture, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data10 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(female.industry, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data11 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(female.services, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data12 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(female.management, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data13 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(fertility.rate, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data14 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(gdp, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data15 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(gdp.per.capita, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data16 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(inflation, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data17 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(female.participation, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data18 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(land.area, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data19 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(life.expectancy, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data20 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(maternal.death, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data21 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(female.population, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data22 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(anemia.non.pregnant, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data23 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(anemia.pregnant, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data24 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(participation.ratio, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data25 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(research.expenditure, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data26 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(self.employed.female, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data27 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(suicide.rate, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data28 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(urban.population, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data29 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(business.index, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data30 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(education.gap.bachelor, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data31 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(education.gap.master, year == input$year), 
                  by = c("id" ="Country.Code"))})
    data32 <-reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(education.gap.doctoral, year == input$year), 
                  by = c("id" ="Country.Code"))})
    
    output$map <- renderLeaflet({
        #Build different maps corresponding to  different input "var"
        if(input$var == "Children.out.of.school..primary..female"){
            pal <- colorBin("Reds", domain = data1()$Children.out.of.school..primary..female)
            myLabels <- paste("<strong>", data1()$location, "</strong>", "<br/>", 
                              "The number of primary-school-age children who are female and not enrolled in primary school:",
                              data1()$Children.out.of.school..primary..female)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data1()$Children.out.of.school..primary..female),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data1()$Children.out.of.school..primary..female,
                          title = "Children out of school, primary, female", 
                          position = "bottomright")
            
        }#end of option 1
        else if(input$var == "Contraceptive.prevalence..any.methods....of.women.ages.15.49.") {
            pal <- colorBin("magma", domain = data2()$Contraceptive.prevalence..any.methods....of.women.ages.15.49.)
            myLabels <- paste("<strong>", data2()$location, "</strong>", "<br/>", 
                              "The percent of women aged 15 to 49 that use any form of contraception :", 
                              data2()$Contraceptive.prevalence..any.methods....of.women.ages.15.49.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data2()$Contraceptive.prevalence..any.methods....of.women.ages.15.49.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data2()$Contraceptive.prevalence..any.methods....of.women.ages.15.49.,
                          title = "Contraceptive prevalence, any methods (% of women ages 15-49)", 
                          position = "bottomright")
            
        }#end of option 2
        else if(input$var == "Educational.attainment..at.least.Bachelor.s.or.equivalent..population.25...female......cumulative.") {
            pal <- colorBin("Reds", domain = data3()$Educational.attainment..at.least.Bachelor.s.or.equivalent..population.25...female......cumulative.)
            myLabels <- paste("<strong>", data3()$location, "</strong>", "<br/>", 
                              "The percentage of female population ages 25 and over that attained or completed Bachelor's or equivalent:", 
                              data3()$Educational.attainment..at.least.Bachelor.s.or.equivalent..population.25...female......cumulative.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data3()$Educational.attainment..at.least.Bachelor.s.or.equivalent..population.25...female......cumulative.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data3()$Educational.attainment..at.least.Bachelor.s.or.equivalent..population.25...female......cumulative.,
                          title = "Educational attainment, at least Bachelor's or equivalent, population 25+, female (%) (cumulative) ", 
                          position = "bottomright")
        }#end of option 3
        else if(input$var == "Educational.attainment..at.least.Bachelor.s.or.equivalent..population.25...male......cumulative.") {
            pal <- colorBin("Reds", domain = data4()$Educational.attainment..at.least.Bachelor.s.or.equivalent..population.25...male......cumulative.)
            myLabels <- paste("<strong>", data4()$location, "</strong>", "<br/>", 
                              "The percentage of male population ages 25 and over that attained or completed Bachelor's or equivalent:", 
                              data4()$Educational.attainment..at.least.Bachelor.s.or.equivalent..population.25...male......cumulative.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data4()$Educational.attainment..at.least.Bachelor.s.or.equivalent..population.25...male......cumulative.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data4()$Educational.attainment..at.least.Bachelor.s.or.equivalent..population.25...male......cumulative.,
                          title = "Educational attainment, at least Bachelor's or equivalent, population 25+, male (%) (cumulative) ", 
                          position = "bottomright")
        }#end of option 4
        else if(input$var == "Educational.attainment..at.least.Master.s.or.equivalent..population.25...female......cumulative.") {
            pal <- colorBin("Reds", domain = data5()$Educational.attainment..at.least.Master.s.or.equivalent..population.25...female......cumulative.)
            myLabels <- paste("<strong>", data5()$location, "</strong>", "<br/>", 
                              "The percentage of female population ages 25 and over that attained or completed Master's or equivalent:", 
                              data5()$Educational.attainment..at.least.Master.s.or.equivalent..population.25...female......cumulative.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data5()$Educational.attainment..at.least.Master.s.or.equivalent..population.25...female......cumulative.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data5()$Educational.attainment..at.least.Master.s.or.equivalent..population.25...female......cumulative.,
                          title = "Educational attainment, at least Master's or equivalent, population 25+, female (%) (cumulative)", 
                          position = "bottomright")
        }#end of option 5
        else if(input$var == "Educational.attainment..at.least.Master.s.or.equivalent..population.25...male......cumulative.") {
            pal <- colorBin("Reds", domain = data6()$Educational.attainment..at.least.Master.s.or.equivalent..population.25...male......cumulative.)
            myLabels <- paste("<strong>", data6()$location, "</strong>", "<br/>", 
                              "The percentage of male population ages 25 and over that attained or completed Master's or equivalent:", 
                              data6()$Educational.attainment..at.least.Master.s.or.equivalent..population.25...male......cumulative.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data6()$Educational.attainment..at.least.Master.s.or.equivalent..population.25...male......cumulative.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data6()$Educational.attainment..at.least.Master.s.or.equivalent..population.25...male......cumulative.,
                          title = "Educational attainment, at least Master's or equivalent, population 25+, male (%) (cumulative)", 
                          position = "bottomright")
        }#end of option 6
        else if(input$var == "Educational.attainment..Doctoral.or.equivalent..population.25...female......cumulative.") {
            pal <- colorBin("Reds", domain = data7()$Educational.attainment..Doctoral.or.equivalent..population.25...female......cumulative.)
            myLabels <- paste("<strong>", data7()$location, "</strong>", "<br/>", 
                              "The percentage of female population ages 25 and over that attained or completed Doctoral or equivalent:", 
                              data7()$Educational.attainment..Doctoral.or.equivalent..population.25...female......cumulative.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data7()$Educational.attainment..Doctoral.or.equivalent..population.25...female......cumulative.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data7()$Educational.attainment..Doctoral.or.equivalent..population.25...female......cumulative.,
                          title = "Educational attainment, Doctoral or equivalent, population 25+, female (%) (cumulative)", 
                          position = "bottomright")
        }#end of option 7
        else if(input$var == "Educational.attainment..Doctoral.or.equivalent..population.25...male......cumulative.") {
            pal <- colorBin("Reds", domain = data8()$Educational.attainment..Doctoral.or.equivalent..population.25...male......cumulative.)
            myLabels <- paste("<strong>", data8()$location, "</strong>", "<br/>", 
                              "The percentage of male population ages 25 and over that attained or completed Doctoral or equivalent:", 
                              data8()$Educational.attainment..Doctoral.or.equivalent..population.25...male......cumulative.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data8()$Educational.attainment..Doctoral.or.equivalent..population.25...male......cumulative.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data8()$Educational.attainment..Doctoral.or.equivalent..population.25...male......cumulative.,
                          title = "Educational attainment, Doctoral or equivalent, population 25+, female (%) (cumulative)", 
                          position = "bottomright")
        }#end of option 8
        else if(input$var == "Employment.in.agriculture..female....of.female.employment...modeled.ILO.estimate.") {
            pal <- colorBin("magma", domain = data9()$Employment.in.agriculture..female....of.female.employment...modeled.ILO.estimate.)
            myLabels <- paste("<strong>", data9()$location, "</strong>", "<br/>", 
                              "Percentage of women who are employed in the agriculture:", 
                              data9()$Employment.in.agriculture..female....of.female.employment...modeled.ILO.estimate.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data9()$Employment.in.agriculture..female....of.female.employment...modeled.ILO.estimate.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data9()$Employment.in.agriculture..female....of.female.employment...modeled.ILO.estimate.,
                          title = "Employment in agriculture, female (% of female employment) (modeled ILO estimate)", 
                          position = "bottomright")
        }#end of option 9
        else if(input$var == "Employment.in.industry..female....of.female.employment...modeled.ILO.estimate.") {
            pal <- colorBin("magma", domain = data10()$Employment.in.industry..female....of.female.employment...modeled.ILO.estimate.)
            myLabels <- paste("<strong>", data10()$location, "</strong>", "<br/>", 
                              "Percentage of female who are employed in the industry:", 
                              data10()$Employment.in.industry..female....of.female.employment...modeled.ILO.estimate.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data10()$Employment.in.industry..female....of.female.employment...modeled.ILO.estimate.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data10()$Employment.in.industry..female....of.female.employment...modeled.ILO.estimate.,
                          title = "Employment in industry, female (% of female employment) (modeled ILO estimate)", 
                          position = "bottomright")
        }#end of option 10
        else if(input$var == "Employment.in.services..female....of.female.employment...modeled.ILO.estimate.") {
            pal <- colorBin("magma", domain = data11()$Employment.in.services..female....of.female.employment...modeled.ILO.estimate.)
            myLabels <- paste("<strong>", data11()$location, "</strong>", "<br/>", 
                              "Percentage of female worked in service:", 
                              data11()$Employment.in.services..female....of.female.employment...modeled.ILO.estimate.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data11()$Employment.in.services..female....of.female.employment...modeled.ILO.estimate.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data11()$Employment.in.services..female....of.female.employment...modeled.ILO.estimate.,
                          title = "Employment in service, female (% of female employment) (modeled ILO estimate)", 
                          position = "bottomright")
        }#end of option 11
        else if(input$var == "Female.share.of.employment.in.senior.and.middle.management....") {
            pal <- colorBin("magma", domain = data12()$Female.share.of.employment.in.senior.and.middle.management....)
            myLabels <- paste("<strong>", data12()$location, "</strong>", "<br/>", 
                              "Female share of employment in senior and middle management (%) :", 
                              data12()$Female.share.of.employment.in.senior.and.middle.management....)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data12()$Female.share.of.employment.in.senior.and.middle.management....),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data12()$Female.share.of.employment.in.senior.and.middle.management....,
                          title = "Female share of employment in senior and middle management (%) ", 
                          position = "bottomright")
        }#end of option 12
        else if(input$var == "Fertility.rate..total..births.per.woman.") {
            pal <- colorBin("Greens", domain = data13()$Fertility.rate..total..births.per.woman.)
            myLabels <- paste("<strong>", data13()$location, "</strong>", "<br/>", 
                              "Fertility rate, total (births per woman) :", 
                              data13()$Fertility.rate..total..births.per.woman.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data13()$Fertility.rate..total..births.per.woman.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data13()$Fertility.rate..total..births.per.woman.,
                          title = "Fertility rate, total (births per woman) ", 
                          position = "bottomright")
        }#end of option 13    
        else if(input$var == "GDP..current.US..") {
            pal <- colorBin("Reds", domain = data14()$GDP..current.US..)
            myLabels <- paste("<strong>", data14()$location, "</strong>", "<br/>", 
                              "GDP (current US$)  :", 
                              data14()$GDP..current.US..)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data14()$GDP..current.US..),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data14()$GDP..current.US..,
                          title = "GDP (Gross Domestic Product) calculated in current USD", 
                          position = "bottomright")
        }#end of option 14
        else if(input$var == "GDP.per.capita..current.US..") {
            pal <- colorBin("Reds", domain = data15()$GDP.per.capita..current.US..)
            myLabels <- paste("<strong>", data15()$location, "</strong>", "<br/>", 
                              "GDP per capita (current US$) :", 
                              data15()$GDP.per.capita..current.US..)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data15()$GDP.per.capita..current.US..),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data15()$GDP.per.capita..current.US..,
                          title = "GDP per captia calcualted in current USD", 
                          position = "bottomright")
        }#end of option 15
        else if(input$var == "Inflation..GDP.deflator..annual...") {
            pal <- colorBin("magma", domain = data16()$Inflation..GDP.deflator..annual...)
            myLabels <- paste("<strong>", data16()$location, "</strong>", "<br/>", 
                              "Inflation, GDP deflator (annual %):", 
                              data16()$Inflation..GDP.deflator..annual...)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data16()$Inflation..GDP.deflator..annual...),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data16()$Inflation..GDP.deflator..annual...,
                          title = "Annual Inflation Rate calculated from GDP deflator", 
                          position = "bottomright")
        }#end of option 16
        else if(input$var == "Labor.force.participation.rate..female....of.female.population.ages.15....modeled.ILO.estimate.") {
            pal <- colorBin("magma", domain = data17()$Labor.force.participation.rate..female....of.female.population.ages.15....modeled.ILO.estimate.)
            myLabels <- paste("<strong>", data17()$location, "</strong>", "<br/>", 
                              "Labor force participation rate, female (% of female population ages 15+) (modeled ILO estimate):", 
                              data17()$Labor.force.participation.rate..female....of.female.population.ages.15....modeled.ILO.estimate.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data17()$Labor.force.participation.rate..female....of.female.population.ages.15....modeled.ILO.estimate.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data17()$Labor.force.participation.rate..female....of.female.population.ages.15....modeled.ILO.estimate.,
                          title = "Labor force participation rate, female (% of female population ages 15+) (modeled ILO estimate)", 
                          position = "bottomright")
        }#end of option 17
        else if(input$var == "Land.area..sq..km.") {
            pal <- colorBin("magma", domain = data18()$Land.area..sq..km.)
            myLabels <- paste("<strong>", data18()$location, "</strong>", "<br/>", 
                              "Land area (sq. km):", 
                              data18()$Land.area..sq..km.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data18()$Land.area..sq..km.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data18()$Land.area..sq..km.,
                          title = "Land area (sq.km)", 
                          position = "bottomright")
        }#end of option 18
        else if(input$var == "Life.expectancy.at.birth..female..years.") {
            pal <- colorBin("Reds", domain = data19()$Life.expectancy.at.birth..female..years.)
            myLabels <- paste("<strong>", data19()$location, "</strong>", "<br/>", 
                              "Life expectancy at birth, female (years) :", 
                              data19()$Life.expectancy.at.birth..female..years.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data19()$Life.expectancy.at.birth..female..years.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data19()$Life.expectancy.at.birth..female..years.,
                          title = "Life expectancy at birth, female (years)", 
                          position = "bottomright")
        }#end of option 19
        else if(input$var == "Number.of.maternal.deaths") {
            pal <- colorBin("Blues", domain = data20()$Number.of.maternal.deaths)
            myLabels <- paste("<strong>", data20()$location, "</strong>", "<br/>", 
                              "Number of maternal deaths:", 
                              data20()$Number.of.maternal.deaths)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data20()$Number.of.maternal.deaths),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data20()$Number.of.maternal.deaths,
                          title = "Number of maternal deaths", 
                          position = "bottomright")
        }#end of option 20
        else if(input$var == "Population..female") {
            pal <- colorBin("Reds", domain = data21()$Population..female)
            myLabels <- paste("<strong>", data21()$location, "</strong>", "<br/>", 
                              "Population, female :", 
                              data21()$Population..female)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data21()$Population..female),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data21()$Population..female,
                          title = "Female Population", 
                          position = "bottomright")
        }#end of option 21
        else if(input$var == "Prevalence.of.anemia.among.non.pregnant.women....of.women.ages.15.49.") {
            pal <- colorBin("magma", domain = data22()$Prevalence.of.anemia.among.non.pregnant.women....of.women.ages.15.49.)
            myLabels <- paste("<strong>", data22()$location, "</strong>", "<br/>", 
                              "Prevalence of anemia among non-pregnant women (% of women ages 15-49):", 
                              data22()$Prevalence.of.anemia.among.non.pregnant.women....of.women.ages.15.49.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data22()$Prevalence.of.anemia.among.non.pregnant.women....of.women.ages.15.49.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data22()$Prevalence.of.anemia.among.non.pregnant.women....of.women.ages.15.49.,
                          title = "Prevalence of anemia among non-pregnant women (% of women ages 15-49)", 
                          position = "bottomright")
        }#end of option 22
        else if(input$var == "Prevalence.of.anemia.among.pregnant.women....") {
            pal <- colorBin("magma", domain = data23()$Prevalence.of.anemia.among.pregnant.women....)
            myLabels <- paste("<strong>", data23()$location, "</strong>", "<br/>", 
                              "Prevalence of anemia among pregnant women (% of women ages 15-49):", 
                              data23()$Prevalence.of.anemia.among.pregnant.women....)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data23()$Prevalence.of.anemia.among.pregnant.women....),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data23()$Prevalence.of.anemia.among.pregnant.women....,
                          title = "Prevalence of anemia among pregnant women (% of women ages 15-49)", 
                          position = "bottomright")
        }#end of option 23
        else if(input$var == "Ratio.of.female.to.male.labor.force.participation.rate......modeled.ILO.estimate.") {
            pal <- colorBin("magma", domain = data24()$Ratio.of.female.to.male.labor.force.participation.rate......modeled.ILO.estimate.)
            myLabels <- paste("<strong>", data24()$location, "</strong>", "<br/>", 
                              "Ratio of female to male labor force participation rate (%) (modeled ILO estimate):", 
                              data24()$Ratio.of.female.to.male.labor.force.participation.rate......modeled.ILO.estimate.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data24()$Ratio.of.female.to.male.labor.force.participation.rate......modeled.ILO.estimate.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data24()$Ratio.of.female.to.male.labor.force.participation.rate......modeled.ILO.estimate.,
                          title = "Ratio of female to male labor force participation rate (%) (modeled ILO estimate)", 
                          position = "bottomright")
        }#end of option 24
        else if(input$var == "Research.and.development.expenditure....of.GDP.") {
            pal <- colorBin("magma", domain = data25()$Research.and.development.expenditure....of.GDP.)
            myLabels <- paste("<strong>", data25()$location, "</strong>", "<br/>", 
                              "Research and development expenditure (% of GDP):", 
                              data25()$Research.and.development.expenditure....of.GDP.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data25()$Research.and.development.expenditure....of.GDP.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data25()$Research.and.development.expenditure....of.GDP.,
                          title = "Research and development expenditure (% of GDP)", 
                          position = "bottomright")
        }#end of option 25
        else if(input$var == "Self.employed..female....of.female.employment...modeled.ILO.estimate.") {
            pal <- colorBin("Reds", domain = data26()$Self.employed..female....of.female.employment...modeled.ILO.estimate.)
            myLabels <- paste("<strong>", data26()$location, "</strong>", "<br/>", 
                              "Self-employed, female (% of female employment) (modeled ILO estimate):", 
                              data26()$Self.employed..female....of.female.employment...modeled.ILO.estimate.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data26()$Self.employed..female....of.female.employment...modeled.ILO.estimate.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data26()$Self.employed..female....of.female.employment...modeled.ILO.estimate.,
                          title = "Self-employed, female (% of female employment) (modeled ILO estimate)", 
                          position = "bottomright")
        }#end of option 26
        else if(input$var == "Suicide.mortality.rate..female..per.100.000.female.population.") {
            pal <- colorBin("Blues", domain = data27()$Suicide.mortality.rate..female..per.100.000.female.population.)
            myLabels <- paste("<strong>", data27()$location, "</strong>", "<br/>", 
                              "Suicide mortality rate, female (per 100,000 female population):", 
                              data27()$Suicide.mortality.rate..female..per.100.000.female.population.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data27()$Suicide.mortality.rate..female..per.100.000.female.population.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data27()$Suicide.mortality.rate..female..per.100.000.female.population.,
                          title = "Suicide mortality rate, female (per 100,000 female population)", 
                          position = "bottomright")
        }#end of option 27
        else if(input$var == "Urban.population....of.total.population.") {
            pal <- colorBin("Reds", domain = data28()$Urban.population....of.total.population.)
            myLabels <- paste("<strong>", data28()$location, "</strong>", "<br/>", 
                              "Urban population (% of total population):", 
                              data28()$Urban.population....of.total.population.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data28()$Urban.population....of.total.population.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data28()$Urban.population....of.total.population.,
                          title = "Urban population (% of total population)", 
                          position = "bottomright")
        }#end of option 28
        else if(input$var == "Women.Business.and.the.Law.Index.Score..scale.1.100.") {
            pal <- colorBin("magma", domain = data29()$Women.Business.and.the.Law.Index.Score..scale.1.100.)
            myLabels <- paste("<strong>", data29()$location, "</strong>", "<br/>", 
                              "Women Business and the Law Index Score (scale 1-100):", 
                              data29()$Women.Business.and.the.Law.Index.Score..scale.1.100.)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data29()$Women.Business.and.the.Law.Index.Score..scale.1.100.),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data29()$Women.Business.and.the.Law.Index.Score..scale.1.100.,
                          title = "Women Business and the Law Index Score (scale 1-100)", 
                          position = "bottomright")
        }#end of option 29
        else if(input$var == "Educational.attainment.gap.at.Bachelor.s") {
            pal <- colorBin("Reds", domain = data30()$Educational.attainment.gap.at.Bachelor.s)
            myLabels <- paste("<strong>", data30()$location, "</strong>", "<br/>", 
                              "Gap between Male and Female in Educational Attainment, at least Bachelor's or equivalent:", 
                              data30()$Educational.attainment.gap.at.Bachelor.s)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data30()$Educational.attainment.gap.at.Bachelor.s),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data30()$Educational.attainment.gap.at.Bachelor.s,
                          title = "Gap between Male and Female in Educational Attainment, at least Bachelor's or equivalent", 
                          position = "bottomright")
        }#end of option 30
        else if(input$var == "Educational.attainment.gap.at.Master.s") {
            pal <- colorBin("Reds", domain = data31()$Educational.attainment.gap.at.Master.s)
            myLabels <- paste("<strong>", data31()$location, "</strong>", "<br/>", 
                              "Gap between Male and Female in Educational Attainment, at least Master's or equivalent", 
                              data31()$Educational.attainment.gap.at.Master.s)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data31()$Educational.attainment.gap.at.Master.s),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data31()$Educational.attainment.gap.at.Master.s,
                          title = "Gap between Male and Female in Educational Attainment, at least Master's or equivalent", 
                          position = "bottomright")
        }#end of option 31
        else{
            pal <- colorBin("Reds", domain = data32()$Educational.attainment.gap.at.Doctoral)
            myLabels <- paste("<strong>", data32()$location, "</strong>", "<br/>", 
                              "Gap between Male and Female in Educational Attainment, at least Doctoral or equivalent:", 
                              data32()$Educational.attainment.gap.at.Doctoral)
            Map1 <- leaflet(WorldCountry) %>% addTiles() %>% 
                addPolygons(
                    fillColor = pal(data32()$Educational.attainment.gap.at.Doctoral),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 3,
                        color = "grey",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = lapply(myLabels, HTML))%>%
                addLegend(pal = pal, values = data32()$Educational.attainment.gap.at.Doctoral,
                          title = "Gap between Male and Female in Educational Attainment, at least Doctoral's or equivalent", 
                          position = "bottomright")
        }#end of option 32
        
})# end of map building
    
    
regmodel1<-reactive(
    if(input$model == "Example 1 (Best subset 1 with 16 variables)"){
        lm(Response~year+E3+E5+E19+E17+E14+E10+E22+E9+E8+E12+E7+E11+E21,data=tidyWDI.subset1)}
    else if (input$model == "Example 2 (Adding E1 to three-variable model of Response~E23+E24+E25)"){
        lm(Response~E23 + E24 + E25,data=tidyWDI.1)
    } #if the input of model is Example 2
    else if (input$model == "Example 3 (Best subset 2 with 12 variables)"){
        lm(Response~year+E10+E8,data=tidyWDI.subset2)
    } #if the input of model is Example 3
    else {
       lm (data = tidyWDI.1, Response ~ E15)
    })#end of reactive model1

regmodel2<-reactive(
    if(input$model == "Example 1 (Best subset 1 with 16 variables)"){
        lm(Response~year+E4+E5+E19+E17+E14+E10+E22+E9+E8+E12+E7+E11+E21,data=tidyWDI.subset1)
        }#if the input of model is Example 1
    else if (input$model == "Example 2 (Adding E1 to three-variable model of Response~E23+E24+E25)"){
        lm(Response~E1 +E23 + E24 + E25,data=tidyWDI.1)
    } #if the input of model is Example 2
    else if (input$model == "Example 3 (Best subset 2 with 12 variables)"){
        lm(Response~year+E17+E8,data=tidyWDI.subset2)
    } #if the input of model is Example 3
    else {
         lm (data = tidyWDI.1, Response ~ E16 + E15)
    })#end of reactive model1
    
    output$summarytable1 <-renderPrint({
            summary(regmodel1())})#end of summarytable1
    output$summarytable2 <-renderPrint({
        summary(regmodel2())})#end of summarytable1
    
    output$normalityplot1 <- renderPlot({
        if(input$residualnormality == "Yes"){
            qqnorm(resid(regmodel1()),main="Normal Probability Plot Model 1");qqline(resid(regmodel1())) }
        else{}
    })#end of normalityplot1
    output$normalityplot2 <- renderPlot({
        if(input$residualnormality == "Yes"){
        qqnorm(resid(regmodel2()),main="Normal Probability Plot Model 2");qqline(resid(regmodel2())) }
        else {}
    })#end of normalityplot2
    output$fitplot1 <- renderPlot({
        if(input$residualfit == "Yes"){
        plot(regmodel1()$fitted.values, resid(regmodel1()), main = "Residuals vs Fit Model 1",xlab="Fitted Values",ylab="Residuals");abline(h = 0) }
        else{}
    })#end of fitplot1
    output$fitplot2 <- renderPlot({
        if(input$residualfit == "Yes"){
        plot(regmodel2()$fitted.values, resid(regmodel2()), main = "Residuals vs Fit Model 2",xlab="Fitted Values",ylab="Residuals");abline(h = 0) }
        else{}
    })#end of fitplot2
    output$orderplot1 <-renderPlot({
        if(input$residualorder == "Yes"){
        plot(resid(regmodel1()), main = "Residuals vs Order Model 1",xlab="Order", ylab="Residuals");abline(h = 0) }
        else {}
    })#end of orderplot1
    output$orderplot2 <-renderPlot({
        if(input$residualorder == "Yes"){
        plot(resid(regmodel2()), main = "Residuals vs Order Model 2",xlab="Order", ylab="Residuals");abline(h = 0) }
        else {}
    })#end of orderplot2
    
    output$visualization1 <- renderPlot({
        if(input$model == "Example 1 (Best subset 1 with 16 variables)"){
            if(input$visualization == "Yes"){
                tidyWDI.subset1<-mutate(tidyWDI.subset1,"E3.sqrt"=sqrt(E3))
                tidyWDI.subset1<-mutate(tidyWDI.subset1,"E3.as.factor"=ifelse(E3.sqrt>=7.080078 & E3.sqrt<9.841240,"fourth.quartile", 
                                                                              ifelse(E3.sqrt>=4.295927 & E3.sqrt<7.080078,"third.quartile",
                                                                                     ifelse(E3.sqrt>=2.025463 & E3.sqrt<4.295927,"second.quartile",
                                                                                            ifelse(E3.sqrt>=0.09 & E3.sqrt<2.025463,"first.quartile","F")))))
                ggplot(data = tidyWDI.subset1, aes(x = E5, y = Response)) + 
                    geom_point()  + 
                    aes(colour = E3.as.factor)  + 
                    stat_smooth(method = lm,se=FALSE) +
                    theme(legend.position = "top") + 
                    labs(title = "Employment % of women in services (E5) versus in agriculture (E3)") +
                    xlab("Women in services (% of total employed women) (E5)") +
                    ylab("Women's labor force participation (%)") +
                    scale_fill_discrete(name="Levels of women in agriculture") }
            else{}
        }#if the input of model is Example 1
        else if (input$model == "Example 2 (Adding E1 to three-variable model of Response~E23+E24+E25)"){
            if(input$visualization == "Yes"){
                ggplot(data = tidyWDI.1, aes(x = E25, y = Response)) + 
                    geom_point()   + 
                    stat_smooth(method = lm,se=FALSE) + 
                    labs(title = "Education attainment gap, Doctorate level only (E25)") + 
                    xlab("Education attainment gap, Doctorate (%) (E25)") + 
                    ylab("Women's labor force participation (%)")
                
                }
            else{}
        } #if the input of model is Example 2
        else if (input$model == "Example 3 (Best subset 2 with 12 variables)"){
            if(input$visualization == "Yes"){
                tidyWDI.subset2<-mutate(tidyWDI.subset2,"E10.as.factor"=ifelse(E10>=23.1998691  & E10<26765.8582519,"best.10", 
                                                                               ifelse(E10>= 13.1615434& E10<23.1998691 ,"80.to.90",
                                                                                      ifelse(E10 >= 9.1359910 & E10<13.1615434,"70.to.80",
                                                                                             ifelse(E10>=6.7762667 & E10<9.1359910,"60.to.70",
                                                                                                    ifelse(E10>=4.8431358 & E10<6.7762667,"50.to.60",
                                                                                                           ifelse(E10>=3.4538321  & E10<4.8431358 ,"40.to.50",
                                                                                                                  ifelse(E10>=2.3586039  & E10<3.4538321,"30.to.40",
                                                                                                                         ifelse(E10>=1.407036 & E10<2.3586039 ,"20.to.30",
                                                                                                                                ifelse(E10>=0.1953952 & E10<1.407036,"10.to.20",
                                                                                                                                       ifelse(E10>= -31.5659150  & E10<0.1953952 ,"lowest.10","F")))))))))))
                ggplot(data = tidyWDI.subset2, aes(x = E8, y = Response)) + 
                    geom_point()  + 
                    aes(colour = E10.as.factor)  + 
                    stat_smooth(method = lm,se=FALSE) +
                    theme(legend.position = "top") + 
                    labs(title = "GDP (in $) (E8) versus Inflation as a GDP Deflator (E10)") + 
                    xlab("GDP (in $) (E8)") +
                    ylab("Women's labor force participation (%)")
                }
            else{}
        } #if the input of model is Example 3
        else {
            if(input$visualization == "Yes"){
                
                ggplot(data = tidyWDI.1, aes(x = E15, y = Response)) + 
                    geom_point()   + 
                    stat_smooth(method = lm,se=FALSE) + 
                    labs(title = "Anemia prevalence (%) among non-pregnant women only (E15)") +
                    xlab("Anemia prevalce (%) among non-pregnant women (E15)") +
                    ylab("Women's labor force participation (%)")
                }
            else{}
        }#the last option for input$model
    })#end of visualization1
    
    output$visualization2 <- renderPlot({
        if(input$model == "Example 1 (Best subset 1 with 16 variables)"){
            if(input$visualization == "Yes"){
                tidyWDI.subset1<-mutate(tidyWDI.subset1,"E4.sqrt"=sqrt(E4))
                tidyWDI.subset1<-mutate(tidyWDI.subset1,"E4.as.factor"=ifelse(E4.sqrt>=4.1385 & E4.sqrt<7.832624,"fourth.quartile", 
                                                                              ifelse(E4.sqrt>=3.4190641 & E4.sqrt<4.1385,"third.quartile",
                                                                                     ifelse(E4.sqrt>=2.7 & E4.sqrt<3.4190641,"second.quartile",
                                                                                            ifelse(E4.sqrt>=0.3162 & E4.sqrt<2.7,"first.quartile","F")))))
                ggplot(data = tidyWDI.subset1, aes(x = E5, y = Response)) + 
                    geom_point()  + 
                    aes(colour = E4.as.factor)  + 
                    stat_smooth(method = lm,se=FALSE) + 
                    theme(legend.position = "top") + 
                    labs(title = "Employment % of women in services (E5) versus in industry (E4)") + 
                    xlab("Women in services (% of total employed women) (E5)") + 
                    ylab("Women's labor force participation (%)") + 
                    scale_fill_discrete(name="Levels of women in industry")
                 }
            else{}
        }#if the input of model is Example 1
        else if (input$model == "Example 2 (Adding E1 to three-variable model of Response~E23+E24+E25)"){
            if(input$visualization == "Yes"){
                tidyWDI.1<-mutate(tidyWDI.1,"E1.log10"=log10(E1))
                tidyWDI.1<-mutate(tidyWDI.1,"E1.as.factor"=ifelse(E1.log10>=5.061893 & E1.log10<=7.244,"fourth.quartile", 
                                                                  ifelse(E1.log10>=4.353079 & E1.log10<5.061893,"third.quartile",
                                                                         ifelse(E1.log10>=3.541797 & E1.log10<4.353079,"second.quartile",
                                                                                ifelse(E1.log10>=1.146128 & E1.log10<3.541797,"first.quartile","F")))))
                ggplot(data = tidyWDI.1, aes(x = E25, y = Response)) + 
                    geom_point()  + 
                    aes(colour = E1.as.factor,name="Primary female students")  + 
                    stat_smooth(method = lm,se=FALSE) + 
                    theme(legend.position = "top") + 
                    labs(title = "Education attainment gap, Doctorate level versus Female primary students (E1)") + 
                    xlab("Education attainment gap, Doctorate (%) (E25)") + 
                    ylab("Women's labor force participation (%)") + 
                    scale_fill_discrete(name="Levels of Primary female students") }
            else{}
        } #if the input of model is Example 2
        else if (input$model == "Example 3 (Best subset 2 with 12 variables)"){
            if(input$visualization == "Yes"){
                tidyWDI.subset2<-mutate(tidyWDI.subset2,"E17.sqr"=E17^2)
                tidyWDI.subset2<-mutate(tidyWDI.subset2,"E17.as.factor"=ifelse(E17.sqr>=6859.90801& E17.sqr<11662.7,"fourth.quartile", 
                                                                               ifelse(E17.sqr>=5415.27919& E17.sqr<6859.90801,"third.quartile",
                                                                                      ifelse(E17.sqr>=3356.65386 & E17.sqr<5415.27919,"second.quartile",
                                                                                             ifelse(E17.sqr>=73.42 & E17.sqr<3356.65386,"first.quartile","F"
                                                                                             )))))
                ggplot(data = tidyWDI.subset2, aes(x = E8, y = Response)) + 
                    geom_point()  + 
                    aes(colour = E17.as.factor)  + 
                    stat_smooth(method = lm,se=FALSE) +
                    theme(legend.position = "top") + 
                    labs(title = "GDP (in $) (E8) versus Female to male labor force participation % (E17)") +
                    xlab("GDP (in $) (E8)") +
                    ylab("Women's labor force participation (%)")
                }
            else{}
        } #if the input of model is Example 3
        else {
            if(input$visualization == "Yes"){
                tidyWDI.1<-mutate(tidyWDI.1,"E16.sqrt"=sqrt(E16))
                tidyWDI.1<-mutate(tidyWDI.1,"E16.as.factor"=ifelse(E16.sqrt>=6.451743  & E16.sqrt<7.85 ,"fourth.quartile", 
                                                                   ifelse(E16.sqrt>=5.458938 & E16.sqrt<6.451743 ,"third.quartile",
                                                                          ifelse(E16.sqrt>=4.751313 & E16.sqrt<5.458938,"second.quartile",
                                                                                 ifelse(E16.sqrt>=3.015 & E16.sqrt< 4.751313,"first.quartile","F")))))
                ggplot(data = tidyWDI.1, aes(x = E15, y = Response)) + 
                    geom_point()  + 
                    aes(colour = E16.as.factor)  + 
                    stat_smooth(method = lm,se=FALSE) + 
                    theme(legend.position = "top") + 
                    labs(title = "Anemia prevalence (%) among non-pregnant (E15) versus pregnant women (E16)") +
                    xlab("Anemia prevalce (%) among non-pregnant women (E15)") + 
                    ylab("Women's labor force participation (%)")}
            else{}
        }#the last option for input$model
    })#end of visualization2

   }#end of server

# Run the application 
shinyApp(ui = ui, server = server)
