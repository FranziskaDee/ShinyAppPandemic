#######################################################
### Datascience Workshop Uni Köln
### August 2021
### Fifth Shiny Version - Land, Y-Axe und Date Range
#######################################################

library(shiny)
library(shinydashboard)
library(mlxR)
library(markdown)
library(scales)
library(readr) 
library(ggplot2)
library(plyr)
library(dplyr)
library(magrittr)
library(rsconnect)

###Datensatz einlesen
#rm(list = ls())
library(readxl)
#covid_worldwide <- read_excel("covid_worldwide.xlsx")
vaccines <- read_excel("vaccines.xlsx")
covid_worldwide <- readRDS("covid_worldwide.rds")

vacc <- vaccines %>%
  group_by(vaccines$ReportingCountry)%>% 
  summarise(sum(FirstDose), sum(SecondDose))
vacc <- rename(vacc, "Erste" = "sum(FirstDose)")
vacc <- rename(vacc, "Zweite" = "sum(SecondDose)")
vacc <- rename(vacc, "EU" = "vaccines$ReportingCountry")

countries <- sort(unique(covid_worldwide$country))
covid_worldwide <- rename(covid_worldwide, "Fallzahlen" = "cases")
covid_worldwide <- rename(covid_worldwide, "Todeszahlen" = "deaths")
#View(covid_worldwide)

# Definiere UI für Shiny App
ui <- fluidPage(
  titlePanel("Covid-19"),
  # Sidebar layout mit Input- und Output-Definition
  sidebarLayout(
    # Variablen für X- und Y-Axe und Land
    sidebarPanel(
      selectInput("z", "Land auswählen:", choices = countries, "Afghanistan"),
      selectInput(inputId = "y", label = "Y-Axe:", 
                  choices=c("Fallzahlen", "Todeszahlen"), 
                  selected = "Fallzahlen"),
      dateRangeInput("datum", "Zeitraum auswählen", 
                     start = min(covid_worldwide$date), 
                     end = max(covid_worldwide$date), 
                     min = min(covid_worldwide$date), 
                     max = max(covid_worldwide$date), 
                     format = "dd.mm.yyyy", language = "de"),
    selectInput(inputId = "u", label = "Impfung", 
                choices=c("Erste", "Zweite"), 
                selected = "Erste")
    ),
    # Output: Bilde "Plot" ab
    mainPanel(
      plotOutput("covidPlot"),
      plotOutput("vaccinePlot"),
      h6("Alle verwendeten Daten wurden vom European Centre for Disease Prevention and Control heruntergeladen."),
      a("https://www.ecdc.europa.eu/en/covid-19/data")
    )
  )
)

# Definiere Server für Shiny App
server <- function(input, output) {

  # Plot bauen -> beginne mit render function
  output$covidPlot <- renderPlot({
    
    # Baue ggplot, das abgebildet werden soll
    
      s <- filter(covid_worldwide,
        covid_worldwide$country == input$z,
        as.Date(date) >= as.Date(input$datum[1]),
        as.Date(date) <= as.Date(input$datum[2])
      )
    
    ggplot(data=s, aes_string(x = "date", y = input$y)) +
      geom_bar(stat="identity", fill="#2fc4b2") + theme_classic() + xlab("Zeitraum") + ylab("Anzahl")# +
    #expand_limits(x = c(0, NA), y = c(0, NA)) +
    #scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))
  })
  output$vaccinePlot <- renderPlot({
    ggplot(data=vacc, aes_string(x = "EU", y = input$u)) +
      geom_bar(stat="identity", fill="#0F52BA") + theme_classic() + xlab("EU Länder") + ylab("Anzahl") +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))
    
  })
}

shinyApp(ui = ui, server = server)