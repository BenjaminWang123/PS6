library(shiny)
library(tidyverse)
library(plotly)
movies2 <- read_delim("movies2.csv")
library(rsconnect)



ui <- fluidPage(
                
                titlePanel("What Movie Should I watch?"),
                tabsetPanel(
                  # First page - introduction and purpose
                  tabPanel("General Information ", 
                           ## intro and general purpose page
                           h3("About this data"),
                           img(src = "https://github.com/ArthurWangN/Final-Project/blob/main/image.jpeg?raw=true"),
                           p("Watching movies is a great way to sit down and unwind by yourself or with friends and family. But, deciding to watch a movie is the easy part - picking what movie to watch is the challenge. Lucky for you, this webpage will help you narrow down what movie to watch based on data from the top 100 most popular movies from 2003 to 2022 according to IMDB. We are all busy people and sometimes our time is limited. Don’t waste your time scrolling through movies and getting frustrated trying to find the perfect movie. 
This webpage can help  anyone who wants to watch modern day movies and needs help narrowing down their options by popularity, profitability, and ratings and runtimes. If you have no specific preference, click on the popularity or profitability tab. These tabs will help you find a movie based on how popular it was by how well it was rated 1-10 and by how many people have watched it based on profitability. If you’re watching a movie with young children and don’t want them to see R rated movies, go over to the movies by rating tab and find the most popular movies based on what rating you want. Or, if you’re really picky about how long your movie times are, click on the movies by rating tab as well. 

The data used for this webpage was retrieved from Kaggle, its contributors are George Scutelnicu and he retrieved his information from IMDB.
"),
p("Source Link: www.kaggle.com/datasets/georgescutelnicu/top-100-popular-movies-from-2003-to-2022-imdb?resource=download"),
p("Source Name: Top 100 popular movies from 2003 to 2022 (iMDB)")                ),
                  
                  tabPanel("Popular Movie Year",
                           
                           ## 100 most popular movies in the years 2003 to 2022 interactive page
                           h2("Movie Years"),
                           fluidPage(
                             titlePanel("get ratings"),
                             p("There are", nrow(movies2), " movies collections hear! You can select years of movie and certain certification institutions to find your prefered golden movie years."),
                             sidebarLayout(
                               sidebarPanel(
                                 sliderInput("n", "Movie Years:",
                                             min = 2003,
                                             max = 2022,
                                             value = 2010),
                                 fluidRow(
                                   column(6,
                                          uiOutput("checkboxCut")
                                   )
                                 )
                               ),
                               mainPanel(
                                 plotOutput("plot")
                               )
                             )
                           )
                  ),
                  tabPanel("Profitable Movies",
                           ## Profits of the 100 most popular movies (2003-2022) interactive page
                           # title
                           h2("Profits of the 100 most popular movies in the years 2003-2022"),
                           fluidPage(
                             h4("This page finds out the profitable movies in input year. Users may want to have a more dimensional overview about how profitable those popular movies are. They may find out that even a movie is one of the 100 toppest popular ones, it still may create a budget deficit. Users can also find out in which months that most popular movies are produced."),
                             sidebarLayout
                             (
                               sidebarPanel
                               (
                                 sliderInput
                                 ("y", "Years: ",
                                   min = 2003,
                                   max = 2022,
                                   value = 2003
                                 )
                               ),
                               mainPanel
                               (
                                 plotOutput("Profitplot")
                               )
                             )
                           )
                  ),
                  
                  tabPanel("Movies by Rating",
                           ##Movies by certifications and also runtime
                           # tittle
                           
                            
                           fluidPage(
                             sidebarLayout(
                               sidebarPanel(
                                 h3("Choosing a Movie by Ratings and Run Times"),
                                 p("Depending on how old you are or who you are watching with, you may need to choose a movie based on the age appropriate certificate it is labled under. Use the following table to filter movies based on the certificates listed below. "),
                                 # Create a checkbox group input for the certificate filter
                                 checkboxGroupInput(
                                   inputId = "certificate_filter",
                                   label = "Certificate:",
                                   choices = c("Not Rated", "NA", "Unrated", "TV-Y7", "TV-MA", "TV-PG", "TV-14", "PG", "PG-13", "R"),
                                   selected = c("Not Rated", "NA", "Unrated", "TV-Y7", "TV-MA", "TV-PG", "TV-14", "PG", "PG-13", "R")
                                 )
                               ),
                               mainPanel(
                                 # Create a table output for the movie data
                                 tableOutput("movie_table")
                               )
                             )
                           )
                           
                           
                  ),
                  tabPanel("Conclusion",
                           ## Conclusion and takeaways 
                           h4("Takeaways"),
                           p("For this App, we want to provide more insights for someone who need to find a good movie. We visualize rating data, popular movie year, and profit of movies to better explain which movies are worth to watch. Our data contains some missing values, but they does not influence the final outcome, casue we disselect them during the data processing. In the future, we hope we can expand more functions of this App, such as use ML to take users preference of movies to provide more accurate and relavent advice.")
                           
                           )
                )
)

# Define server 
server <- function(input, output) {
  
  # intro page
  
  #interactive page 2
  output$Profitplot <- renderPlot({
    movies2 %>% 
      filter(Budget != "Unknown", Year != "Unknown", Month != "Unknown", Income != "Unknown") %>%
      mutate(Budget = substring(Budget, 2, length(Budget)),
             Income = substring(Income, 2, length(Income))) %>%
      mutate(Budget = str_replace_all(Budget ,pattern = ",",replacement = ""),
             Income = str_replace_all(Income,pattern = ",",replacement = "")) %>%
      mutate(profit = as.numeric(Income) - as.numeric(Budget)) %>%
      filter(Year == input$y, !is.na(profit)) %>% 
      ggplot(data = ., aes(x = Month, y = profit)) +
      geom_point(shape=19) + xlab("Months") + ylab("Profits")
  })


 # interactive page 3
  
  # Filter the movies dataset by the selected certificate
  filtered_movies <- reactive({
    movies2 %>%
      filter(Certificate %in% input$certificate_filter)
  })
  
  # Create a table of movie titles, runtimes, and certificates
  output$movie_table <- renderTable({
    filtered_movies() %>%
      select(Title, Runtime, Certificate)
  })
  
  output$checkboxCut <- renderUI({
    checkboxGroupInput("Certificate", "Choose Certificate",
                       choices = unique(movies2$Certificate)
    )
  })
  sample <- reactive({
    movies2 %>%
      filter(Certificate %in% input$Certificate,
             Year == input$n)
  })
  output$plot <- renderPlot({
    ggplot(data = sample(), aes(x = Rating, color = Certificate)) +
      geom_histogram()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

