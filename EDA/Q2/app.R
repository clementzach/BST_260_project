#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


offensive_position <- c("K", "OL", "P", "QB", "RB", "TE", "WR")

offense <- injury_gather %>% filter(position_id %in% offensive_position)
defense <- injury_gather %>% filter(position_id == "DEF") 


# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinythemes::shinytheme("darkly"),
    
    # Application title
    titlePanel("Injury Counts in the NFL"),

    # Sidebar 
    sidebarLayout(
        
        #Widget for selection
        sidebarPanel(
            #Radio buttons that allow the user to select one
            radioButtons("position", label = "Select Category of Players:",
                         choices = c("All Players", "Offense", "Defense"))
        ),

        # Main panel
        mainPanel(
           plotOutput("poshist")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$poshist <- renderPlot({
        if (input$position == "All Players") {
            injury_gather %>%
                group_by(bodypart) %>%
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
            
        } else if(input$position == "Offense" | "Defense") {
            str_to_lower(input$position) %>%
                group_by(bodypart) %>%
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
            
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


#Still need to fix axis & title label to include input$age
#Change theme
#Include text explaining what the app does
