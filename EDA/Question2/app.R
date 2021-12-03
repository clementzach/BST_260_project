#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(gsubfn)
library(tidyr)
library(ggthemes)


library(readr)
injuries <- read_csv("all_injuries_clean.csv")

injury_gather <- gather(injuries, key = "bodypart", value = "counts", 28:ncol(injuries))
injury_gather

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
                column(6, p("First, we can look at the distribution of injuries based on broad position")
                ),
                
                #Radio buttons that allow the user to select one
                column(6, radioButtons("position", label = "Select Category of Players:",
                             choices = c("All Players", "Offensive", "Defensive"))
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
                column(6, p("Now, let's look just at the distribution of injuries for the specific offensive positions")
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
                geom_text(aes(label = counts), position= position_dodge(width = 0.9), vjust = -0.25, fontface = 'bold') +
                scale_y_continuous(limits = c(0, 10000)) +
                xlab("Body Part") +
                ylab("Count") +
                ggtitle("Distribution of Injuries for All Injured NFL players") +
                theme_economist() +
                theme(
                    axis.title.x = element_text(size = 16, vjust = -3),
                    axis.title.y = element_text(size = 16, vjust = 3),
                    title = element_text(size = 20),
                    plot.title = element_text(hjust = 0.5)
                )
            
        } else if(input$position == "Offensive" | input$position == "Defensive") {
            lower <- str_to_lower(input$position)
            temp <- str_sub(lower, 1, nchar(lower) - 3)
            e <- "e"
            new <- paste(temp, e, sep = "")
            eval(parse(text = new)) %>%
                group_by(bodypart) %>%
                summarise(counts = sum(counts)) %>%
                ggplot(aes(x = reorder(bodypart, - counts), y = counts)) +
                geom_col() +
                geom_text(aes(label = counts), position = position_dodge(width = 0.9), vjust = -0.25, fontface = 'bold') +
                scale_y_continuous(limits = c(0, 10000)) +
                xlab("Body Part") +
                ylab("Count") +
                ggtitle(paste("Distribution of Injuries for Injured ", input$position, " NFL players")) + 
                theme_economist() +
                theme(
                    axis.title.x = element_text(size = 14, vjust = -3),
                    axis.title.y = element_text(size = 14, vjust = 3),
                    title = element_text(size = 18),
                    plot.title = element_text(hjust = 0.5)
                )
            
        }
    })
    
    
    output$offhist <- renderPlot({
        
        offense %>% 
            filter(position_id == input$off) %>%
            group_by(bodypart) %>%
            summarise(counts = sum(counts)) %>%
            ggplot(aes(x = reorder(bodypart, -counts), y = counts)) +
            geom_col() +
            geom_text(aes(label = counts), position=position_dodge(width = 0.9), vjust = -0.25, fontface='bold') +
            scale_y_continuous(limits = c(0, 1500)) +
            xlab("Body Part") +
            ylab("Count") +
            ggtitle(paste("Distribution of  Offensive Injuries for NFL", input$off)) +
            theme_economist() +
            theme(
                axis.title.x = element_text(size = 14, vjust = -3),
                axis.title.y = element_text(size = 14, vjust = 3),
                title = element_text(size = 18),
                plot.title = element_text(hjust = 0.5)
            )
        
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


#Still need to fix axis & title label to include input$age
#Change theme
#Include text explaining what the app does
