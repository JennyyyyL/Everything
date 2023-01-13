#load all needed packages
library(readr)
library(dplyr)
library(tidyr)
library(leaflet)
library(geojsonio)
library(shiny)
library(ggplot2)
library(factoextra)
library(shinythemes)
library(caret)

#Load Covid data
CovidData<-read.csv("owid-covid-data.csv")
#Change the format of date column
CovidData$date<-as.Date(CovidData$date, format = "%Y-%m-%d")
#Load data for countries' longitudes and latitudes
location<-read.csv("https://raw.githubusercontent.com/albertyw/avenews/master/old/data/average-latitude-longitude-countries.csv")
colnames(location)[2] <- "location"
location[185,2]<-"Russia"
#Add countries' longitudes and latitudes to Covid data
CovidData<-full_join(CovidData,location, by="location")
#Create four subsets for reactive maps
TotalCasesData<-select(CovidData, iso_code, location, date, total_cases, Latitude, Longitude)
TotalCasesMillion<-select(CovidData, iso_code, location, date, total_cases_per_million, Latitude, Longitude)
TotalDeaths<-select(CovidData, iso_code, location, date, total_deaths, Latitude, Longitude)
TotalDeathsMillion<-select(CovidData, iso_code, location, date, total_deaths_per_million, Latitude, Longitude)
#import the necessary code for the shape of polygons of different countries in the world
shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
#name a new data based on the imported code about shapes of polygons
WorldCountry <- geojson_read(shapeurl, what = "sp")
#Remove Antarctica, which is useless in the map
WorldCountry <- WorldCountry[-which(WorldCountry$id=="ATA"),]

#Select variables that we are interested in for clustering
ClusteringData<-select(CovidData,
                       date,
                       location,
                       total_cases_per_million,
                       new_cases_per_million,
                       total_deaths_per_million,
                       new_deaths_per_million,
                       stringency_index,
                       gdp_per_capita,
                       diabetes_prevalence,
                       life_expectancy)

data_modeling<- read.csv("tidy_data_modeling.csv")
#load the required data which has cleaned from previous data-cleaning file
#Note: you might need to change the file path if necessary


#UI for the app
ui <- fluidPage(theme = shinytheme ("united"),
                #"united" is the theme for the app selected from the shinytheme package
                navbarPage("COVID-19 Summary",
                 #The first tab that visualize the total cases/deaths by making a reactive map
                 tabPanel("Map",
                          titlePanel("COVID19 World Map"),
                          helpText("Click on the country to see label."),
                          sidebarLayout(
                              #sidebar panel for inputs ----
                              sidebarPanel(
                                  selectInput("var", 
                                              label = "Variable",
                                              choices = c("Total Cases",
                                                          "Total Cases/million",
                                                          "Total Deaths",
                                                          "Total Deaths/million"),
                                              selected = "Total Cases"),
                                  dateInput("time",
                                            label = "Date", 
                                            value = "2020-10-17"),
                              ),
                              #main panel for displaying outputs ----
                              #output: a reactive map
                              mainPanel(leafletOutput("map")) 
                          )
                 ),
                 
                 #The second tab that visualize COVID data on a certain day by clustering
                 tabPanel("Clustering",
                          titlePanel("COVID19 Clustering"),
                          sidebarLayout(
                              #sidebar panel for inputs ----
                              sidebarPanel(
                                  dateInput("date",
                                            label = "Date", 
                                            value = "2020-10-17"),
                                  selectInput("xvar", 
                                              label = "X Variable",
                                              choices = c("total_cases_per_million",
                                                          "new_cases_per_million",
                                                          "total_deaths_per_million",
                                                          "new_deaths_per_million",
                                                          "stringency_index",
                                                          "gdp_per_capita",
                                                          "diabetes_prevalence",
                                                          "life_expectancy"),
                                              selected = "total_cases_per_million"),
                                  selectInput("yvar", 
                                              label = "y Variable",
                                              choices = c("total_cases_per_million",
                                                          "new_cases_per_million",
                                                          "total_deaths_per_million",
                                                          "new_deaths_per_million",
                                                          "stringency_index",
                                                          "gdp_per_capita",
                                                          "diabetes_prevalence",
                                                          "life_expectancy"),
                                              selected = "new_cases_per_million"),
                                  numericInput("clusters",
                                               label = "Numer of clusters", 2, min = 1, max = 8),
                                  #output: a plot that shows optimal number of clusters
                                  plotOutput("plot2")
                              ),
                              #main panel for displaying outputs ----
                              #output: a reactive clustering plot
                              mainPanel(plotOutput("plot")) 
                          )),
                 #The second tab that visualize COVID data on a certain day by PCA
                 tabPanel("PCA",
                          titlePanel("Principle Components Analysis"),
                          sidebarPanel(
                              dateInput("date1",
                                        label = "Date", 
                                        value = "2020-10-10"),
                              plotOutput("screeplot")),
                          mainPanel(
                              plotOutput("biplot"),
                              verbatimTextOutput("PCA"))
                          ),
                 tabPanel(
                     "Multiple Linear Regression",# name of tab Panel 3
                     titlePanel("COVID-19 Modeling (Multiple Linear Regression)"),
                     sidebarPanel(
                         selectInput("selectedvariable",
                                     label = "Varible:", 
                                     choices = c("Total Cases", "Total Deaths", "New Cases")),
                         # choices of variables for the users to select
                         dateInput("date2",
                                   label = "Date", 
                                   value = "2020-10-17"),
                     ),#sidebar panel for Panel 3
                     
                     mainPanel(
                         plotOutput("modelplot"),
                         #code for plotting the text output
                         verbatimTextOutput("coef")
                     )#main panel for Panel 3
                     
                 ), # Panel 3, the LASSO model
                 
                 tabPanel (
                     "References",# Name of Panel 4
                     titlePanel("References"), 
                     #add title for the 4th panel
                     h3("Source of Original Data :"),
                     h4("https://covid.ourworldindata.org/data/owid-covid-data.csv"),
                     h3("Source of polygons used in the map visualization:"),
                     h4("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"),
                     h3("Source of longtitude and latitude of different countries:"),
                     h4("https://raw.githubusercontent.com/albertyw/avenews/master/old/data/average-latitude-longitude-countries.csv")
                 ) #Panel 4, the References page and the corresponding url

                 ))#end of ui

server <- function(input, output, session) {
    #Tab1
    #generate different data set for different input "var"
    data1 <- reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(TotalCasesData, date==input$time), 
                  by = c("id" ="iso_code"))})
    data2 <- reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(TotalCasesMillion, date==input$time), 
                  by = c("id" ="iso_code"))})
    data3 <- reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(TotalDeaths, date==input$time), 
                  by = c("id" ="iso_code"))})
    data4 <- reactive({
        left_join(data.frame(id = WorldCountry$id), 
                  filter(TotalDeathsMillion, date==input$time), 
                  by = c("id" ="iso_code"))})
    #Generate the output for the Map tab panel
    output$map <- renderLeaflet({
        #Four different maps corresponding to four different input "var"
        #Map for total cases
        if(input$var == "Total Cases"){
       pal <- colorNumeric("Reds", domain = data1()$total_cases)
        leaflet(WorldCountry) %>%
            addTiles()%>%
            addPolygons(
                fillColor = pal(data1()$total_cases),
                weight = 1,
                opacity = 1,
                color = "white",
                fillOpacity = 0.5,
                highlight = highlightOptions(
                    weight = 2,
                    color = "red",
                    fillOpacity = 0.7),
                popup = paste("<strong>", data1()$location, "</strong>", "<br/>",
                              "Total Cases:", data1()$total_cases))%>%
            addCircleMarkers(data = data1(), lat = ~ Latitude, lng = ~ Longitude, weight = 1, radius = ~(total_cases)^(1/6), 
                             fillOpacity = 0.2, color ="Red") %>%
            addLegend(pal = pal, values = data1()$total_cases,
                      title = "Total Cases", position = "bottomright")
        
        #Map for total cases oer million
        } else if (input$var == "Total Cases/million"){
            pal <- colorNumeric("Reds", domain = data2()$total_cases_per_million)
            leaflet(WorldCountry) %>%
                addTiles()%>%
                addPolygons(
                    fillColor = pal(data2()$total_cases_per_million),
                    weight = 1,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.5,
                    highlight = highlightOptions(
                        weight = 2,
                        color = "red",
                        fillOpacity = 0.7),
                    popup = paste("<strong>", data2()$location, "</strong>", "<br/>",
                                  "Total cases/million:", data2()$total_cases_per_million))%>%
                addCircleMarkers(data = data2(), lat = ~ Latitude, lng = ~ Longitude, weight = 1, radius = ~(total_cases_per_million)^(1/5), 
                                 fillOpacity = 0.2, color ="Red") %>%
                addLegend(pal = pal, values = data2()$total_cases_per_million,
                          title = "Total Cases/million", position = "bottomright")
          
          #Map for total deaths  
        } else if  (input$var == "Total Deaths"){
            pal <- colorNumeric("Blues", domain = data3()$total_deaths)
            leaflet(WorldCountry) %>%
                addTiles()%>%
                addPolygons(
                    fillColor = pal(data3()$total_deaths),
                    weight = 1,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.5,
                    highlight = highlightOptions(
                        weight = 2,
                        color = "blue",
                        fillOpacity = 0.7),
                    popup = paste("<strong>", data3()$location, "</strong>", "<br/>",
                                  "Total Deaths:", data3()$total_deaths))%>%
                addCircleMarkers(data = data3(), lat = ~ Latitude, lng = ~ Longitude, weight = 1, radius = ~(total_deaths)^(1/5), 
                                 fillOpacity = 0.2, color ="Blue") %>%
                addLegend(pal = pal, values = data3()$total_deaths,
                          title = "Total Deaths", position = "bottomright")
            
          #Map for total deaths per million     
        } else {
            pal <- colorNumeric("Blues", domain = data4()$total_deaths_per_million)
            leaflet(WorldCountry) %>%
                addTiles()%>%
                addPolygons(
                    fillColor = pal(data4()$total_deaths_per_million),
                    weight = 1,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.5,
                    highlight = highlightOptions(
                        weight = 2,
                        color = "blue",
                        fillOpacity = 0.7),
                    popup = paste("<strong>", data4()$location, "</strong>", "<br/>",
                                  "Total cases/million:", data4()$total_deaths_per_million))%>%
                addCircleMarkers(data = data4(), lat = ~ Latitude, lng = ~ Longitude, weight = 1, radius = ~(total_deaths_per_million)^(1/5), 
                                 fillOpacity = 0.2, color ="Blue") %>%
                addLegend(pal = pal, values = data4()$total_deaths_per_million,
                          title = "Total Deaths/million", position = "bottomright")
        }
    })
    
    #Tab2
    #generate a filtered data set that contains information needed on certain date
    Data_date <- reactive({
        select(filter(ClusteringData, date==input$date), -location, -date)
    })
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        na.omit(Data_date()[, c(input$xvar, input$yvar)])
    })
    #perform k-means clustering
    clusters <- reactive({
        kmeans(selectedData(), input$clusters)
    })
    #Generate the output for the Clustering panel
    output$plot <- renderPlot({
        palette(c("#4DAF4A", "#984EA3", "#E41A1C", "#377EB8", 
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF"))
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    #Generate the output plot that visualize optimal number of clusters
    output$plot2 <- renderPlot({
        fviz_nbclust(selectedData(), kmeans, method = "silhouette", k.max = 8)})
    
    #Tab3
    #generate a filtered data set that contains information needed on certain date for PCA
    Data_date1 <- reactive({
        na.omit(select(filter(ClusteringData, date==input$date1), -date))
    })
    Data_date2 <- reactive({
        testdata <- Data_date1()
        rownames(testdata) <- Data_date1()$location
        select(testdata, -location)
    })
    output$biplot <-renderPlot({
        fviz_pca_biplot(prcomp(Data_date2(),
                               scale = TRUE), 
                        repel = TRUE, 
                        col.var = "red", 
                        col.ind = "blue")
    })
    output$screeplot <- renderPlot({
        fviz_eig(prcomp(Data_date2(), scale = TRUE), addlabels = TRUE)
    })
    output$PCA <- renderPrint({
        prcomp(Data_date2(), scale = TRUE)
    })
    
    #tab 4
    data.3.1 <- reactive({
                  filter(data_modeling, date==input$date2)})
                         
    output$modelplot <-renderPlot({
        if(input$selectedvariable == "Total Cases"){
            fit.3.1<- lm(data = data.3.1(), total_cases ~  population + population_density 
                         + median_age + gdp_per_capita + diabetes_prevalence + 
                             life_expectancy)
            plot(fit.3.1)
            } # if the input is "Total Cases"
        
    else if (input$selectedvariable == "Total Deaths"){
        fit.3.2<- lm(data = data.3.1(), total_deaths ~  population + population_density 
                     + median_age + gdp_per_capita + diabetes_prevalence + 
                         life_expectancy)
        plot(fit.3.2)
    } #if the input is "Total Deaths"
        
    else {
        fit.3.3<- lm(data = data.3.1(), new_cases ~  population + population_density 
                     + median_age + gdp_per_capita + diabetes_prevalence + 
                         life_expectancy)
        plot(fit.3.3)
    }#if the input is "New Cases"
})#display the plot of the multiple linear regression model based on the user's choice
    
    output$coef <- renderPrint ({
        if(input$selectedvariable == "Total Cases"){
            fit.3.1<- lm(data = data.3.1(), total_cases ~  population + population_density 
                         + median_age + gdp_per_capita + diabetes_prevalence + 
                             life_expectancy)
            fit.3.1
        } # if the input is "Total Cases"
        else if (input$selectedvariable == "Total Deaths"){
            fit.3.2<- lm(data = data.3.1(), total_deaths ~  population + population_density 
                         + median_age + gdp_per_capita + diabetes_prevalence + 
                             life_expectancy)
            fit.3.2
        } #if the input is "Total Deaths"
        else {
            fit.3.3<- lm(data = data.3.1(), new_cases ~  population + population_density 
                         + median_age + gdp_per_capita + diabetes_prevalence + 
                             life_expectancy)
            fit.3.3
        }#if the input is "New Cases"
    })#display the coefficients of different variables with reaction to user's choice 
} #end of server
#run the application
shinyApp(ui = ui, server = server)
