#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

reg_plot <- function(x){
    reg_2017_to_2020 %>% 
        filter(county == x) %>% 
        ggplot(aes(x = year, y = registration, color = party)) +
        geom_line(show.legend = FALSE, size = 0.54) +
        scale_color_manual(values = c("blue", "red")) +
        labs(x = "Year",
             y = "Registration",
             title = "Party Registration Figures",
             caption = "Source: Arizona Secretary of State") +
        theme_fivethirtyeight()
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Arizona Voter Registration Figures"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("county",
                        "Select a County:",
                        choices = c("Apache",
                                    "Cochise",
                                    "Coconino",
                                    "Gila",
                                    "Graham",
                                    "Greenlee",
                                    "Maricopa",
                                    "Mohave",
                                    "Navajo",
                                    "Pima",
                                    "Pinal",
                                    "Santa Cruz",
                                    "Yavapai",
                                    "Yuma"),
                        selected = "Apache")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("reg_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        data <- reg_plot(input$county)
        
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
