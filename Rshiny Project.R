

#run necessary packages for this app


data1<- read.csv("/Users/jennyli/Desktop/tidy_net_saving.csv")
#load the required data which has cleaned from codes in the file: Project-Data Cleaning.rmd
#Note: you might need to change the file path if necessary 

# Define UI for the application
ui <- fluidPage (theme = shinytheme ("cerulean"),
                 #"cerulean" is the theme for the app selected from the shinytheme package
                 navbarPage (
                     "Adjusted Net National Income (current US$) Visualizer",
                     # add title for the app
                     tabPanel(
                         "Specific Country",
                         # name of tab Panel 1
                         sidebarPanel(
                             tags$h2("Input"),
                             #Add the name for sidebar panel
                             selectInput("countryselected", label = "Country:", 
                                         choices = c(data1$Country.Name)),
                                             # choices of country names for the users to select
                             ),#sidebar panel for Panel 1
                         
                         mainPanel(
                             h2("Scatter plot of the adjusted net national income of the selected country 
                                from 2000 to 2018 and its animation"),
                             #Add title for the main panel
                             plotlyOutput("plot1"),
                             #code for plotting a plotly output: plot1
                             plotOutput("plot2")
                             #code for poltting a plot output: plot2
                             )#main panel for Panel 2
                         
                         ), # Panel 1
                    
                     tabPanel(
                         "Specific Year",
                         # name for tab panel 2
                         sidebarPanel(
                           tags$h2("Input"),
                           #add title for sidebar panel
                           selectInput("yearselected", label = "Year:", 
                                       choices = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 
                                                   2007, 2008, 2009, 2010, 2011, 2012, 2013, 
                                                   2014, 2015, 2016, 2017, 2018)),
                           # choices for years for the users to select
                         ),#sidebar panel for Panel 2
                         
                         mainPanel(
                           h2 ("Bar plot the adjusted net national income of the world 
                               in the selected year"),
                           #Add title for the main panel
                           plotlyOutput("plot3")
                           #output the plotly output: plot3
                         )#Main panel for Panel 2
                         
                         ),# Panel 2
                     
                     tabPanel(
                       "References", # Name of Panel 3
                       h3("Source of Original Data :"),
                       h4("https://datacatalog.worldbank.org/dataset/adjusted-net-savings"),
                       h3("Previous Shiny Apps that inpsires the creation of this app:"),
                       h4("https://qishuyi.shinyapps.io/EarthquakeDataVisualizer/"),
                       h4 ("https://shiny.rstudio.com/gallery/telephones-by-region.html"),
                       h4 ("https://shiny.rstudio.com/gallery/movie-explorer.html"),
                       h3 ("Shiny Applications are learned from :"),
                       h4 ("https://www.youtube.com/watch?v=tfN10IUX9Lo"),
                       h4("https://shiny.rstudio.com/articles/plot-interaction.html"),
                       h3("Including plotly package into Shiny is learned from:"),
                       h4("https://community.plotly.com/t/incorporate-a-plotly-graph-into-a-shiny-app/5329/2")
                     )# References of the project and the corresponding url
))#end of ui

# Server for the desirable graph of this application
server <- function(input, output) {
  
  newdata <- reactive ({ 
    filter(data1, Country.Name == input$countryselected)
  })
  #creating a reactive dataset based on user's choice in Panel 1
  
  databyyear <- reactive ({ 
    filter(data1, Year == input$yearselected)
  })
  #creating a reactive dataset based on user's choice in Panel 2
  
  output$plot1 <- renderPlotly(
    plot_ly(data = newdata(), type = "scatter", mode = "markers", 
            x = ~Year, y = ~Value, frame = ~Year,
            showlegend = FALSE) %>%
      animation_opts(frame = 1300, easing = "elastic", redraw = FALSE)
  )
  #code for creating the corresponding animation of the scatter plot based on user's choice in panel 1(plot 1)
  
  output$plot2 <- renderPlot({
    ggplot(data = newdata()) +
      geom_point(mapping= aes(x=Year, y=Value, color = Year))
  })
  #code for creating the corresponding scatterplot based on user's choice in panel 1 (plot 2)
   
  output$plot3 <- renderPlotly (
    plot_ly(data = databyyear(), 
            type = "bar",  
            x = ~Country.Name, 
            y = ~Value)
  )#creating the corresponding interactive bar plot based on the user's choice in panel 2 (plot 3)
}
#end of server

# Run the application 
shinyApp(ui = ui, server = server)
