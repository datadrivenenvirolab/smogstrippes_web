# Install and load required packages
if (!require("shiny")) install.packages("shiny")
if (!require("plotly")) install.packages("plotly")

library(shiny)
library(plotly)
library(tidyr)
library(broom)
library(ggtext)

df_plot_1 <- read_csv('Data/data_w_aqg.csv') %>% 
  pivot_longer(cols = c('who_2021', 'who_2005'), names_to = 'who_year', values_to = 'who_val')%>%
  mutate(who_year = str_replace(who_year, 'who_', ''))

df_plot_2 <- df_plot_1 %>%
  group_by(uesi_id) %>% mutate(year= year-min(year))%>%
  do(tidy(lm(pm25_mean~ year, data= .)))%>%
  ungroup()%>% filter(term == 'year')

df_plot_long <- df_plot_1 %>% left_join(df_plot_2, by = c('uesi_id'))%>%
  mutate(who_val = factor(who_val,
                             levels = c ("Within Recommended Value of 5µg/m^3",
                                    "Within Recommended Value of 10µg/m^3",
                                    "Within WHO Interim Target 4 of 10µg/m^3",
                                    "Within WHO Interim Target 3 of 15µg/m^3",
                                    "Within WHO Interim Target 2 of 25µg/m^3",
                                    "Within WHO Interim Target 1 of 35µg/m^3",
                                    "Exceeding All Recommended Guidelines & Targets")))%>%
  mutate(trend = case_when(estimate > 0 & p.value < 0.05 ~ paste0(city, "(<span style='color: red;'>&#x25B2;</span>)"),
                           estimate < 0 & p.value < 0.05 ~ paste0(city, "(<span style='color: green;'>&#x25BC;</span>)"),
                           p.value > 0.05 ~ paste0(city, "(<span style='color: orange;'>&#x25C0;&#x25B6;</span>)")))


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
  nav_panel(
    title = "Smogstripes",
    grid_container(
      layout = c(
        "area0"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area0",
        card_body(plotlyOutput(outputId = "main_plot"))
      )
    )
  ),
  nav_panel(
    title = "Parameters",
    grid_container(
      layout = c(
        "facetOption",
        "facetOption"
      ),
      row_sizes = c(
        "370px",
        "1fr"
      ),
      col_sizes = c(
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "facetOption",
        card_header("Selection for visualization"),
        card_body(
          radioButtons(
            "who_select", "Select WHO Standard:", choices = unique(df_plot_long$who_year), selected = unique(df_plot_long$who_year)[1],
            width = "100%"
          ),
          selectizeInput("cities_select", "Select Cities:", choices = unique(df_plot_long$city), 
                         multiple = TRUE,
                         options = list(maxItems = 12),
                         selected = df_plot_long$city %>% unique() %>% sample(12))
        )
      )
    )
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
