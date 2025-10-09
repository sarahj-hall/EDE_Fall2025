#### Load packages ----
library(shiny)
library(tidyverse)
library(here)

#### Load & wrangle the data that will be used in the app ----
nutrient_data <- 
  read_csv("./Data/NTL-LTER_Lake_Nutrients_PeterPaul_Processed.csv") %>% 
  mutate(sampledate = ymd(sampledate)) %>%
  filter(depth_id > 0) %>%
  select(lakename, sampledate:po4)

#### Define the user interface (UI) ----
#Create a "fluid" page layout, the most common layout
ui <- fluidPage(
  #Set the theme for the app
  # See https://shiny.posit.co/r/gallery/application-layout/shiny-theme-selector/
  theme = shinythemes::shinytheme("flatly"),
  #Set the title of the fluid page
  titlePanel("Nutrients in Peter Lake and Paul Lake"),
  #Create a layout with a sidebar and main area
  sidebarLayout(
    # Add input controls in the sidebar
    sidebarPanel(
      
      # Add a `selectInput` control to select nutrient to plot
      selectInput(
        #Set the ID name of the control
        inputId = "dropdown_input",
        #Set how it should be labeled in the interface
        label = "Nutrient",
        #Set the choices and default selection
        choices = c("tn_ug", "tp_ug", "nh34", "no23", "po4"), 
        selected = "tp_ug"),
      ),

    # Now access the main panel in our sidebar layout
    mainPanel(
      #Add a canvas for a plot output, 
      #giving the plot the ID name of "the_scatterplot"
      plotOutput("the_scatterplot")
    )))

#### Define server: this defines the action behind the scenes  ----
# Define a server function for the app
# The input comes from the UI, and the output is sent to the UI
# This code runs when something in the UI changes; it is "responsive"
server <- function(input, output) {
  # We have only one function that is triggered: this code creates a
  # scatterplot using the value in the input control labeled `dropdown_input`
  output$the_scatterplot <- renderPlot({
    #Code to generate the plot; note the y variable is input$dropdown_input
    ggplot(
      nutrient_data,
      aes_string(
        x = "sampledate", 
        y = input$dropdown_input,
        fill = "depth_id", 
        shape = "lakename")
      ) +
      geom_point(alpha = 0.8, size = 2) +
      theme_classic(base_size = 14) +
      scale_shape_manual(values = c(21, 24)) +
      labs(
        x = "Date", 
        y = expression(Concentration ~ (mu*g / L)), 
        shape = "Lake", 
        fill = "Depth ID"
        ) +
      scale_fill_distiller(
        palette = "YlOrBr", 
        guide = "colorbar", 
        direction = 1)
    })
 
} #End of the server function

#### Create the Shiny app object ----
shinyApp(ui = ui, server = server)


