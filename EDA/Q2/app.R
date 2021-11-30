#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinythemes::shinytheme("darkly"),
    
    # Application title
    titlePanel("Injury Counts in the NFL"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("age",
                        "Age:",
                        min = 21,
                        max = 75,
                        value = 37)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("agehist")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$agehist <- renderPlot({
        injury_age %>% 
            group_by(bodypart) %>%
            filter(age == input$age) %>%
            summarise(counts = sum(counts)) %>%
            ggplot(aes(x = reorder(bodypart, - counts), y = counts)) +
            geom_col() +
            theme(
                axis.title.x = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                title = element_text(size = 22),
                plot.title = element_text(hjust = 0.5)
            ) +
            xlab("Injuries") +
            ylab("Count") +
            ggtitle("Distriubtion of Injuries for NFL players")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


#Still need to fix axis & title label to include input$age
#Change theme
