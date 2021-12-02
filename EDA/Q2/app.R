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
    
    br(), # add some space

    
    p("This Shiny app uses a dataset related 
        to NFL injuries to examine the distribution 
        of different injury types among NFL football players."),
    
    br(), 
    
    # 1st row
    fluidRow(
        
        #Sidebar
        sidebarLayout(
            
            #Widget for selection
            sidebarPanel(
                
                # Explanatory text
                column(6, p("Let's look at the distribution based on broad position")
                ),
                
                #Radio buttons that allow the user to select one
                column(6, radioButtons("position", label = "Select Category of Players:",
                             choices = c("All Players", "Offense", "Defense"))
                )
            ),
            
            mainPanel(
                # Plot
                column(12, plotOutput("poshist")
                )
            )
        )
    ),
    
    br(),

    # 2nd row
    fluidRow(
        
        #Sidebar
        sidebarLayout(
            
            #Widget for selection
            sidebarPanel(
                
                # Explanatory text
                column(6, p("Now let's look just at the specific Offensive Positions")
                ),
                
                
                # Drop down menu for only offensive positions
                column(6, selectInput(inputId = "off", label = "Choose an Offensive Position",
                            choices = c(Lineman = "OL", QuarterBack = "QB", RunningBack = "RB", TightEnd = "TE", 
                                        WideReciever = "WR", Kicker = "K", Punter = "P"))
                )
                
            ),
            
            mainPanel(
                # Plot
                column(12, plotOutput("offhist") 
                )
            )
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
                scale_y_continuous(limits = c(0, 10000)) +
                theme(
                    axis.title.x = element_text(size = 15),
                    axis.title.y = element_text(size = 15),
                    title = element_text(size = 22),
                    plot.title = element_text(hjust = 0.5)
                ) +
                xlab("Injuries") +
                ylab("Count") +
                ggtitle("Distriubtion of Injuries for NFL players")
            
        } else if(input$position == "Offense" | input$position == "Defense") {
            eval(parse(text = str_to_lower(input$position))) %>%
                group_by(bodypart) %>%
                summarise(counts = sum(counts)) %>%
                ggplot(aes(x = reorder(bodypart, - counts), y = counts)) +
                geom_col() +
                scale_y_continuous(limits = c(0, 10000)) +
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
    
    
    output$offhist <- renderPlot({
        
        offense %>% 
            filter(position_id == input$off) %>%
            group_by(bodypart) %>%
            summarise(counts = sum(counts)) %>%
            ggplot(aes(x = reorder(bodypart, -counts), y = counts)) +
            geom_col() +
            scale_y_continuous(limits = c(0, 1500)) +
            theme(
                axis.title.x = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                title = element_text(size = 22),
                plot.title = element_text(hjust = 0.5)
            ) +
            xlab("Body Part") +
            ylab("Count") +
            ggtitle("Distriubtion of Offensive Injuries")
        
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


#Still need to fix axis & title label to include input$age
#Change theme
#Include text explaining what the app does
