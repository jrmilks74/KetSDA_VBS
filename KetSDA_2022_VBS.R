#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Kettering Adventist Church
# Vacation Bible School 2022
# Attendance Tracker

library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(timetk)
library(plotly)
library(knitr)

# Load the data

VBS <- read_csv("VBS2022.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                                `Group_1` = col_number(), `Group_2` = col_number(),
                                                `Group_3` = col_number(), `Group_4` = col_number(),
                                                Total = col_number()))
VBS <- VBS %>%
        select(Date, Group_1, Group_2, Group_3, Group_4) %>%
        rename(Pre_K = Group_1, Kindergarten = Group_2, Grades_1_to_3 = Group_3, Grades_4_to_6 = Group_4) %>%
        pivot_longer(!Date, names_to = "Age", values_to = "Count")
Pre_K <- VBS %>%
        filter(Age == "Pre_K")
Kindergarten <- VBS %>%
        filter(Age == "Kindergarten")
Grades_1_to_3 <- VBS %>%
        filter(Age == "Grades_1_to_3")
Grades_4_to_6 <- VBS %>%
        filter(Age == "Grades_4_to_6")
Total <- VBS %>%
        group_by(Date) %>%
        summarize(Count = sum(Count))


ui <- fluidPage(

    titlePanel("KetSDA 2022 VBS Attendance"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
            sidebarPanel(
                    selectInput("group", label = h3("Age group"),
                                choices = list("Pre-K" = "Pre_K", "Kindergarten" = "Kindergarten", "Grades 1 - 3" = "Grades_1_to_3", "Grades 4 - 6" = "Grades_4_to_6", "Total" = "Total"),
                                selected = "Pre_K"),
                    p("Choose the group you wish to see. Default is total attendance per night.")    
            ),
            
            mainPanel(
                    h3("VBS attendance August 2022"),
                    plotlyOutput("stackedbar"),
                    h3("Closeup of the selected age group"),
                    plotlyOutput("barplot")
            )
    ),
    hr(),
    h4("Created by: Jim Milks"),
    "Version 1",
    br(),
    "Code and data available at:", 
    a(href = "https://github.com/jrmilks74/KetSDA_VBS", "https://github.com/jrmilks74/KetSDA_VBS"),
    br(),
    "Data updated: 03 August 2022"
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        
        reactive_choice <- reactive({
                if (input$group == "Pre_K")
                        Pre_K
                else if (input$group == "Kindergarten")
                        Kindergarten
                else if (input$group == "Grades_1_to_3")
                        Grades_1_to_3
                else if (input$group == "Grades_4_to_6")
                        Grades_4_to_6
                else
                        Total
                })
        
        output$stackedbar <- renderPlotly({
                 ggplot(VBS, aes(x = Date, y = Count, fill = Age)) +
                        geom_bar(position = "stack", stat = "identity") +
                        labs(title = "VBS attendance",
                             x = "Day",
                             y = "Count")
                })
        output$barplot <- renderPlotly({
                group_data <- reactive_choice()
                ggplot(group_data, aes(x = Date, y = Count)) +
                        geom_bar(stat = "identity", fill = "red") +
                        labs(title = "Attendance by age group",
                             x = "Day",
                             y = "Count")
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
