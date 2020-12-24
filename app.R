#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(shinythemes)
library(ggallin)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),

    # Application title
    titlePanel("A Visualization of the US Foreign Aid"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fluidRow(
               selectInput("selected_transaction_type", 
                           label = "Choose a transaction type", 
                           c("Obligations", "Disbursements"),
                           selected = "Disbursements") 
            ),
            conditionalPanel(condition = "input.selected_transaction_type == 'Obligations'",
                             sliderInput(
                                 "selected_obligation_year", "Choose a year",
                                min = 1946, max = 2019, value = 2019, step = 1,
                                sep = "",
                                round = TRUE)
        ), conditionalPanel(condition = "input.selected_transaction_type == 'Disbursements'",
                            sliderInput(
                                "selected_disbursement_year", "Choose a year",
                                min = 2001, max = 2019, value = 2019, step = 1,
                                sep = "",
                                round = TRUE)
        ), br(), br(),br(),br(),
        "Data Sources ", br(),
        "US Foreign Aid : ", tags$a(href="https://explorer.usaid.gov/data", "Foreign Aid Explorer [U.S. Agency for International Development]"),
        br(), 
        "Population : ", tags$a(href="https://ourworldindata.org/grapher/population", "Our World in Data"),
        br(),br(),
        "To report bugs, please open an issue at ", tags$a(href="https://github.com/stevin-wilson/US_Foreign_Aid/issues", "US Foreign Aid Visualization@Github")
        ),
        

        # Show a plot of the generated distribution
        mainPanel( 
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotlyOutput("distPlot", width = "1200px", height = "800px")),
                        tabPanel("Table", dataTableOutput("distTable"))
            )
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    us_foreign_aid_df <- readRDS("Data/us_foreign_aid_df.rds")
    population_df <- readRDS("Data/population_df.rds")
    
    combined_df <- us_foreign_aid_df %>%
        inner_join(population_df, by = c("country_code", "fiscal_year")) %>%
        drop_na()
    
    myColors <- brewer.pal(length(levels(combined_df$region_name)),"Dark2")
    names(myColors) <- levels(combined_df$region_name)
    
    filtered_transaction_df <- reactive({
        combined_df %>% 
            filter(transaction_type_name == input$selected_transaction_type)
    })
    
    filtered_transaction_year_df <- reactive({
        if(input$selected_transaction_type == 'Obligations'){
            filtered_transaction_df() %>%
                filter(as.character(fiscal_year) == as.character(input$selected_obligation_year))
            } else if (input$selected_transaction_type == 'Disbursements'){
                filtered_transaction_df() %>%
                    filter(as.character(fiscal_year) == as.character(input$selected_disbursement_year))
            }
    })
    
    year_reactive_variable <- reactive({
        if(input$selected_transaction_type == 'Obligations'){
                as.character(input$selected_obligation_year)
        } else if (input$selected_transaction_type == 'Disbursements'){
                as.character(input$selected_disbursement_year)
        }
    })
    
    final_df <- reactive({
        filtered_transaction_year_df() %>%
            mutate(income_group_name = factor(income_group_name, 
                                              levels = c("Low Income Country",
                                                         "Lower Middle Income Country",
                                                         "Upper Middle Income Country",
                                                         "High Income Country"),
                                              ordered = TRUE),
                   region_name = factor(region_name,
                                        levels = c("Sub-Saharan Africa",
                                                   "South and Central Asia",
                                                   "Western Hemisphere",
                                                   "Europe and Eurasia",
                                                   "Middle East and North Africa",
                                                   "East Asia and Oceania"),
                                        ordered = FALSE)) %>%
            rename(`country code` = country_code,
                   `country name` = country_name,
                   `region name` = region_name,
                   `income group` = income_group_name,
                   `transaction type` = transaction_type_name,
                   `fiscal year` = fiscal_year,
                   `amount in millions of 2016 US$` = US_dollars_2016_in_millions,
                   `total population in millions` = population)
        
    })
    
    output$distTable <- renderDataTable({
        final_df() %>%
            ungroup() %>%
            select(-c(`fiscal year`, `transaction type`)) %>%
            relocate(`country code`, 
                     `country name`, 
                     `region name`,
                     `income group`,
                     `total population in millions`,
                     `amount in millions of 2016 US$`)
        })

    output$distPlot <- renderPlotly({
        print(
        ggplotly( final_df() %>%
                     ggplot(aes(text = paste("country name :", `country name`,
                                             "</br>country code :", `country code`), 
                                x = `total population in millions`, 
                                y = `amount in millions of 2016 US$`, 
                                shape = `income group`, 
                                color = `region name`,
                     )) +
                     geom_point() +
                     scale_x_continuous(labels = scales::comma, trans = pseudolog10_trans, limits = c(min(filtered_transaction_df()$population - 10 ),max(filtered_transaction_df()$population + 10 ))) +
                     scale_y_continuous(labels = scales::comma, trans = pseudolog10_trans, limits = c(min(filtered_transaction_df()$US_dollars_2016_in_millions - 10 ),max(filtered_transaction_df()$US_dollars_2016_in_millions + 10))) +
                     xlab("Total Population (in millions)") +
                     ylab("Amount in millions of 2016 US$") + theme_minimal() +
                     ggtitle(paste("US Foreign Aid vs. Population - ", input$selected_transaction_type, " - ", year_reactive_variable())) + 
                     theme(axis.text.x = element_text(angle = 45)) +
                     scale_colour_manual(name = "region name",values = myColors,
                                         labels = levels(combined_df$region_name), drop = FALSE))
    )})
}

# Run the application 
shinyApp(ui = ui, server = server)
