source("WorldWeatherOnlineAPIKey.R")

#install.packages("jsonlite")
#install.packages("httr")
#install.packages("shiny")
#install.packages("shinythemes")

library(jsonlite)
library(httr)
library(shiny)
library(shinythemes)

my.ui <- fluidPage(
  
  theme = shinytheme("darkly"),
  
  titlePanel("Ski Resort Comparison Tool"),
  "In this program, you will be able to choose a mountain in the Washington Region and get current information about the weather for today
  and for the forecast of tomorrow.",
  sidebarLayout(
    
    sidebarPanel(
      
      # User has the option to select which ski resort s/he would like to go to
      selectInput(
        "zipcode", "Select a ski resort", choices = list("Alpental/Snoqualmie" = "98068", "Stevens" = "98288", "Mt. Baker" = "98244", "Mission Ridge" = "98801", "Mt. Spokane" = "99021", "Crystal" = "98022", "White Pass" = "98937")
      ),
      "By choosing a mountain here, you will be able to see the information of your choosing about said mountain.",
      br(), br(),
      
      # To select what data is to be displayed
      radioButtons(
        "desired_data", "Select What You Want To Be Graphed:", choices = list("Chance of Snow", "Visibility", "Hourly Temperature", "Humidity", "Pressure", "Snowfall", "Total Snowfall")
      ),
      "Each choice will show you a different graph with the relevant information about your mountain selection."
    ),
    
    mainPanel(
      
      tabsetPanel(
        type = "tab",
        
        # This panel produces graphs to visualize the data provided by the API
        tabPanel("Graph of Data", plotOutput("plot", width = "90%")),
        
        # This panel shows information about the moon and sun rise and set times
        tabPanel("Moon and Sun Info", shiny::dataTableOutput("tables")),
        
        # This panel displays the URLs for the user to click on
        tabPanel("Website Homepage", "Here is a link to the homepage of your mountain where you will be able to find out more descriptive information.",
                 uiOutput("output")
        )
      )
    )
  )
)

# The server part of the app
my.server <- function(input, output) {
  
  get_data <- reactive ({
    
    # Reactively retreive the user input for whichever zipcode they select
    zip <- input$zipcode
    
    # Retreieve API data from WorldWeatherOnline
    get_response <- GET("http://api.worldweatheronline.com/premium/v1/ski.ashx", query = list(q = zip, key = API.Key, format = "JSON"))
    
    text <- content(get_response, "text")
    
    body <- fromJSON(text)
    
    # Convert the JSON data into a readable and manipulatable format
    dataframe <- as.data.frame(body)
    
    # Make an inverted version of dataframe, with selected rows, for a visually pleasing table to display
    inverted <- t(dataframe)
    selected <- as.data.frame.table(inverted[c(1, 2, 5, 6, 10),])
    truncated <- selected[, 1:7]
    
    # Data about the astronomy. This variable is named for the current dates that we have made the dataframe 
    # but will change for the days when the information is pulled at a later time.
    moon_sun <- (dataframe[["data.weather.astronomy"]])
    
    March_8_18 <- unlist(moon_sun[[1]])
    March_9_18 <- unlist(moon_sun[[2]])
    March_10_18 <- unlist(moon_sun[[3]])
    March_11_18 <- unlist(moon_sun[[4]])
    March_12_18 <- unlist(moon_sun[[5]])
    March_13_18 <- unlist(moon_sun[[6]])
    March_14_18 <- unlist(moon_sun[[7]])
    
    # Bind the data tables containing information about the next 7 days
    moonsun <- rbind(March_8_18, March_9_18, March_10_18, March_11_18, March_12_18, March_13_18, March_14_18)
    row_names = c("March 8", "March 9", "March 10", "March 11", "March 12", "March 13", "March 14")
    rownames(moonsun) <- row_names
    
    # Data frames for all of the information that will be graphed and compared.
    weather_hourly_td <- dataframe[[1,7]]
    weather_hourly_tm <- dataframe[[2,7]]
    
    # Chance of Snow (hourly)
    chance_of_snow_td <- weather_hourly_td[["chanceofsnow"]]
    
    # Total Snowfall (hourly)
    snowfall_td <- weather_hourly_td[["snowfall_cm"]]
    
    # Visibility (hourly)
    visibility_td <- weather_hourly_td[["visibility"]]
    
    # Pressure (hourly)
    pressure_td <- weather_hourly_td[["pressure"]]
    
    # Humidity (hourly)
    humidity_td <- weather_hourly_td[["humidity"]]
    
    # Mid-mountain Mean Temperatures
    mid_dataframe <- weather_hourly_td[["mid"]]
    
    # Function to compile data regarding temperatures from the mid mountain area, and sort it into a custom data table
    compile_temp <- function(dataframe) {
      final <- data.frame()
      for (i in 1:8) {
        temp <- flatten(dataframe[[i]])
        final <- rbind(final, temp)
      }
      hourly_temperature <- final$tempF
      return(hourly_temperature)
    }
    
    # Output for temperature during the day
    temps <- compile_temp(mid_dataframe)
    
    # Used to compare total snowfall for today and tomorrow
    total_snowfall_td <- dataframe[[1,10]]
    total_snowfall_tm <- dataframe[[2,10]]
    
    # if statement to select correct URL according to which resort the user selects
    if (identical(zip, "98068")) {
      url <- a("Snoqualmie Homepage", href= "http://www.summitatsnoqualmie.com/")
    } 
    
    else if (identical(zip, "98288")) {
      url <- a("Stevens Pass Homepage", href= "https://www.stevenspass.com/site")
    } 
    
    else if (identical(zip, "98244")) {
      url <- a("Mt. Baker Homepage", href= "http://www.mtbaker.us/")
    } 
    
    else if (identical(zip, "98801")) {
      url <- a("Mission Ridge Homepage", href= "https://www.missionridge.com/")
    } 
    
    else if (identical(zip, "99021")) {
      url <- a("Mt. Spokane Homepage", href= "http://www.mtspokane.com/")
    } 
    
    else if (identical(zip, "98022")) {
      url <- a("Crystal Mountain Resort Homepage", href= "https://www.crystalmountainresort.com/")
    } 
    
    else if (identical(zip, "98937")) {
      url <- a("White Pass Homepage", href= "https://skiwhitepass.com/")
    }
    
    # List of output from the reactive function, later used to plot data and make tables
    list(snow_chance = chance_of_snow_td, snowfall = total_snowfall_td, snowfall2 = total_snowfall_tm, visibility = visibility_td, temp = temps, humidity = humidity_td, link = url, pressure = pressure_td, hsnowfall = snowfall_td, infotable = truncated, moon_sun = moonsun)
  })
  
  # UI hyperlink to corresponding homepage for selected resort to see more information
  output$output <- renderUI({
    
    # This adds a hyperlink to the correct webpage
    tagList("URL link to", get_data()$link)
  })
  
  
  # Plot to visualize snowfall during the day (currently using example data, will need to replace with API data)
  output$plot <- renderPlot({
    
    # Vector (in military time) associated with the times of snapshots of data
    times <- c(0, 300, 600, 900, 1200, 1500, 1800, 2100)
    
    # Graphs chance of snow during the day (line graph)
    if (identical(input$desired_data, "Chance of Snow")) {
      plot(get_data()$snow_chance, type = "l", xlab = "Time", col = "orange", ylab = "Chance of Snow (%)", ylim = c(0,100), main = "Chances of Snow During the Day", lwd = 2, xaxt = 'n')
      axis(1, at=1:8, labels = times)
    } 
    
    # Graphs total snowfall for today and tomorrow (bar graph)
    else if (identical(input$desired_data, "Total Snowfall")) {
      barplot(c(as.numeric(get_data()$snowfall), as.numeric(get_data()$snowfall2)), col = c("red","blue"), width = .5, ylim = c(0,55), xlab = "Today", ylab = "Total Snowfall (cm)", 
              main = "Total Expected Snowfall for Today and Tomorrow", names.arg = list("Total Snowfall Today", "Total Snowfall Tomorrow"))
    } 
    
    # Graphs visibility during the day (line graph)
    else if (identical(input$desired_data, "Visibility")) {
      plot(get_data()$visibility, type = "l", xlab = "Time", col = "pink", ylab = "Visibility (Km)", ylim = c(0,10), main = "Visibility During the Day", lwd = 2, xaxt = 'n')
      axis(1, at=1:8, labels = times)
    } 
    
    # Graphs temperature during the day (line graph)
    else if (identical(input$desired_data, "Hourly Temperature")) {
      plot(get_data()$temp, type = "l", xlab = "Time", col = "green", ylab = "Temperature (F)", ylim = c(0,50), main = "Temperature During the Day", lwd = 2, xaxt = 'n')
      axis(1, at=1:8, labels = times)
    } 
    
    # Graphs humidity during the day (line graph)
    else if (identical(input$desired_data, "Humidity")) {
      plot(get_data()$humidity, type = "l", xlab = "Time", col = "purple", ylab = "Humidity (%)", ylim = c(0,100), main = "Humidity During the Day", lwd = 2, xaxt = 'n')
      axis(1, at=1:8, labels = times)
    } 
    
    # Graphs snowfall during the day (line graph)
    else if (identical(input$desired_data, "Snowfall")) {
      plot(get_data()$hsnowfall, type = "l", xlab = "Time", col = "cadetblue1", ylab = "Amount of Snowfall (cm)", ylim = c(0,100), main = "Snowfall During the Day", lwd = 2, xaxt = 'n')
      axis(1, at=1:8, labels = times)
    } 
    
    # Graphs pressure during the day (line graph)
    else if (identical(input$desired_data, "Pressure")) {
      plot(get_data()$pressure, type = "l", xlab = "Time", col = "deeppink", ylab = "Pressure (mb)", ylim = c(1000,1050), main = "Pressure During the Day", lwd = 2, xaxt = 'n')
      axis(1, at=1:8, labels = times)
    }
  })
  
  # Output the data for the moon and sun
  output$tables = shiny::renderDataTable({ 
    get_data()$moon_sun
  })
}

# Run app
shinyApp(ui = my.ui, server = my.server)
