
# Load packages and data --------------------------------------------------

library(tidyverse)
library(scales)
library(glue)
library(ggraph)
library(igraph)
library(extrafont)

slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')


# Prepare data before Shiny use -------------------------------------------

aggregated_df <- slave_routes %>% 
  distinct(voyage_id, n_slaves_arrived, .keep_all = TRUE) %>% 
  # Aggregate by decade
  mutate(decade = year_arrival %/% 10 *10,
    # change the 'port unknown' message
    place_of_purchase = str_replace(place_of_purchase, ".?, port unspecified" , " (Port unknown)"))

# Choose to display only top 30 locations
choices <- aggregated_df %>% 
  group_by(place_of_purchase) %>% 
  summarise(total_slaves = sum(n_slaves_arrived, na.rm =T), .group = "drop") %>% 
  arrange(-total_slaves) %>% 
  slice(1:30) %>% 
  pull(place_of_purchase)


# The app interface -------------------------------------------------------
ui <- fluidPage(
  
  tags$head(
    tags$style("label{font-family: Roboto Condensed;}")
  ),
  # App title ----
  titlePanel("Slavery history"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      selectInput("var", label = "Choose a location",
        choices = choices, selected = "Ambriz")),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Plot 1
      plotOutput("bar"),
      # Plot 2
      plotOutput("igraph")
    )
  )
)



# The backend of the app --------------------------------------------------

server <- function(input, output) {
  #Plot one bar plot
  output$bar <- renderPlot({ 
    bar_plot_df <- aggregated_df %>%  
      group_by(decade, place_of_purchase) %>% 
      summarise(total_slaves = sum(n_slaves_arrived, na.rm = T), .groups = "drop") %>% 
      filter(!is.na(total_slaves), decade >= 1600) %>% 
      # Highlight the input$var
      mutate(highlight = case_when(
        place_of_purchase == input$var ~ "y",
        TRUE ~ "n")) 
    
    ggplot(bar_plot_df, aes(x = decade, y = total_slaves, fill = highlight))+
      geom_bar(stat = "identity", show.legend = FALSE)+
      labs(x = "Decade", y= NULL, title = glue("Number of slaves taken from {input$var}"))+
      scale_y_continuous(labels = scales::label_comma())+
      scale_fill_manual(values = c("y" = "#003366", n = "grey90"))+
      theme_classic()+
      theme(
        text = element_text(family = "Roboto Condensed"),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 10, color = "grey70"),
        axis.title = element_text(size = 10, color = "grey50"),
        axis.line = element_blank(),
        plot.title = element_markdown(size = 14, face = "bold"),
        # Align plot nicely to the top-left
        plot.title.position = "plot"
      )
    
  })
  # Plot two network arc graph
  output$igraph <- renderPlot({
    graph <- aggregated_df %>% 
      filter(decade >= 1800) %>%
      count(decade, place_of_purchase, port_arrival, sort = T) %>% 
      # Highlight the input$var
      mutate(highlight = ifelse(place_of_purchase == input$var, "y", "n")) %>% 
      select(from = place_of_purchase, to = port_arrival, n, highlight) %>% 
      drop_na() %>% 
      graph_from_data_frame()
    
    ggraph(graph, layout = 'linear') + 
      geom_edge_arc(aes(color = highlight, alpha = highlight), show.legend = FALSE)+
      scale_edge_color_manual(values = c("y" = "#003366", "n"= "grey90"))+
      # Override arcs that hide the blue lines
      scale_edge_alpha_manual(values = c("y" = 1, "n" = 0.1))+
      labs(title = glue("Ports slaves were taken to from {input$var}"), caption = "From 1800'")+
      theme_void()+
      theme(plot.title = element_markdown(size = 14, family = "Roboto Condensed", face = "bold"))
  })
}

shinyApp(ui = ui, server = server)
