# Install and load required packages
# webr::install("shiny")
# webr::install("plotly")
# webr::install("ggtext")
# webr::install("readr")
# webr::install("bslib")

library(shiny)
library(plotly)
library(ggtext)
library(readr)
library(bslib)

df_plot_long <- read.csv('https://raw.githubusercontent.com/datadrivenenvirolab/smogstrippes_web/main/data/data_shiny.csv')

who_palette_2021 <- c("Within Recommended Value of 5µg/m^3"= "#00E400", 
                      "Within WHO Interim Target 4 of 10µg/m^3" = "#FB6A4A", 
                      "Within WHO Interim Target 3 of 15µg/m^3"="#EF3B2C", 
                      "Within WHO Interim Target 2 of 25µg/m^3" = "#CB181D", 
                      "Within WHO Interim Target 1 of 35µg/m^3" = "#A50F15", 
                      "Exceeding All Recommended Guidelines & Targets" = "#67000D")
who_palette_2005 <- c("Within Recommended Value of 10µg/m^3" = "#00E400",            
                      "Within WHO Interim Target 3 of 15µg/m^3" = "#EF3B2C", 
                      "Within WHO Interim Target 2 of 25µg/m^3" = "#CB181D", 
                      "Within WHO Interim Target 1 of 35µg/m^3" = "#A50F15", 
                      "Exceeding All Recommended Guidelines & Targets" ="#67000D")


# UI
ui <- page_navbar(
  selected = "Smogstripes",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  sidebar = sidebar(
    title = "City Selection",
    radioButtons(
      choices = unique(df_plot_long$who_year),
      selected = unique(df_plot_long$who_year)[1],
      width = "100%",
      inputId = "who_select",
      label = "Select WHO Standard:"
    ),
    selectizeInput("cities_select", "Select Cities:", choices = unique(df_plot_long$city), 
                                       multiple = TRUE,
                                       options = list(maxItems = 6),
                                       selected = df_plot_long$city %>% unique() %>% sample(12))
  ),
  nav_panel(
    title = "Smogstripes",
    plotlyOutput(outputId = "main_plot")
  )
)


# Server
server <- function(input, output) {
  output$main_plot <- renderPlotly({
    filtered_data <- df_plot_long[df_plot_long$city %in% input$cities_select & df_plot_long$who_year == input$who_select, ]
    
    # Your ggplot code
    aq_stripe <- ggplot(filtered_data,
                        aes(x = year, y = 1, fill = who_val))+ 
      geom_tile() +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_manual(values = get(paste0('who_palette_', unique(filtered_data$who_year)))) +
      guides(fill = guide_colorbar(barwidth = 1))+
      guides(fill = guide_legend(title = paste0("WHO Guidelines (", unique(filtered_data$who_year),")"))) +
      labs(title = paste0("1998-2022 AIR QUALITY BY WHO GUIDELINES (", unique(filtered_data$who_year),")"))+
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.title = element_blank(),
            panel.grid.major = element_blank(),
            legend.title = element_blank(),
            legend.position = 'top',
            legend.box = "horizontal",
            #axis.text.x = element_text(vjust = 3),
            axis.text.x = element_blank(),
            # axis.text.x = element_text(angle=90),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_textbox_simple(size = 10),
            plot.margin = margin(1, 0, 0, 0, "cm"),
            strip.text = ggtext::element_markdown(),
            strip.text.x = element_text(size=11, face="bold")
      )+# Adjust theme as needed
      facet_wrap(~trend)
    
    # Convert ggplot object to plotly
    p <- ggplotly(aq_stripe, tooltip= "all")
      # layout(title = list(text = paste0("1998-2022 AIR QUALITY BY WHO GUIDELINES (", unique(filtered_data$who_year),")") ,
      #                     x = 0, y = 1,
      #                     pad = list(b = 60, l = 0, r = 0 )))
    
    p
  })
}

# Run the app
shinyApp(ui, server)
