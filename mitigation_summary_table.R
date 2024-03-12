library(tidyverse)
library(reactable)
library(reactablefmtr)
library(htmltools) # for the download button functionality
library(fontawesome) # for the download button icon

# Set up table theme ####
# adapted from fivethrityeight() https://github.com/kcuilla/reactablefmtr/blob/HEAD/R/themes.R
table_theme <-  reactableTheme(
  style = list(fontFamily = "Arial, sans-serif"),
  cellStyle =  NULL,
  color =  "#222222",
  backgroundColor = NULL,
  borderWidth = "1px",
  borderColor = "#dddddd",
  stripedColor = "#dddddd",
  highlightColor = "#f0f0f0",
  cellPadding = 5,
  tableStyle = list(fontSize = 14,
                    borderBottom = "3px solid #222222"),
  headerStyle = list(
    # Align headers to the bottom
    display = "flex",
    flexDirection = "column",
    justifyContent = "flex-end",
    borderWidth = "3px",
    paddingTop = "12px",
    verticalAlign = "bottom",
    textAlign = "bottom",
    background = "#ffffff",
    borderColor = "#222222",
    color = "#000000",
    "&:hover" = list(background = "#dddddd"),
    "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "#5b5e5f", color = "#ffffff"),
    borderColor = "#333",
    fontSize = 16
  ),
  groupHeaderStyle = list(
    "&:not(:empty)" = list(
      paddingBottom = "3px",
      paddingTop = "12px",
      verticalAlign = "bottom",
      textAlign = "bottom",
      backgroundColor = "#ffffff",
      fontSize = 18,
      color = "#222222"
    )
  ),
  searchInputStyle = list(color = "#222222",
                          fontSize = "14px"),
  inputStyle = list(
    backgroundColor = "#ffffff",
    color = "#222222"
  ),
  rowSelectedStyle = list(backgroundColor = "#dddddd"),
  pageButtonStyle = list(fontSize = "14px"),
  paginationStyle = list(fontSize = "14px")
)

# Prepare the table ####
mitigation_summary <-
  read_csv("mitigation_summary.csv") %>%
  mutate(
    color_mitigation_result = ifelse(mitigation_result == "Mitigated", "#998ec3", "#f1a340"),
    # extract units from metric column
    unit = str_extract(metric,  "(?<=\\().+?(?=\\))"),
    unit = case_when(
      unit == "number of facilities" ~ "healthcare facilities",
      unit == "number of access points" ~ "park access points",
      unit == "number of jobs" ~ "jobs",
      TRUE ~ unit
    ),
    # round with significant integers for numeric values
    mitigation_goal_conveyal_ej = signif(mitigation_goal_conveyal_ej, 4),
    conveyal_change_comp = signif(conveyal_change_comp, digits = 4),
    
    # re-code mitigation direction to match the header refrasing as question
    mitigation_direction = ifelse(mitigation_direction == "good", "Yes", "No") ,
    color_mitigation_direction =  case_when(
      mitigation_direction == "Yes" ~ "#d8daeb",
      mitigation_direction == "No" ~ "#fee0b6"
    )
  ) %>%
  select(-metric_type, -mode)

# Make the table ####
mitigation_sumamry_table <- mitigation_summary %>%
  reactable(
    theme = table_theme,
    pagination = FALSE,
    compact = TRUE,
    columns = list(
      metric = colDef(name = "Metric"),
      
      demo_type = colDef(name = "Population Type"),
      
      mitigation_goal_conveyal_ej = colDef(
        name = "How much benefit for EJ population is needed to mitigate the DI/DB?",
        # pull in suffix from unit column
        cell = function(value, index) {
          unit <- mitigation_summary$unit[index]
          sprintf("%s %s", value, unit)
        }
      ),
      
      impact_ej = colDef(name = "Impact to EJ population"),
      
      impact_non_ej = colDef(name = "Impact to non-EJ population"),
      
      impact = colDef(name = "Impact of TIP Projects"),
      
      mitigation_direction = colDef(
        name = "Do TIP projects contribute to mitigation need?",
        cell = pill_buttons(
          data = .,
          box_shadow = F,
          color_ref = 'color_mitigation_direction',
          bold_text = T
        )
      ),
      
      conveyal_change_comp = colDef(
        name = "How do TIP projects impact EJ population relative to non-EJ population?",
        cell = function(value, index) {
          unit <- mitigation_summary$unit[index]
          sprintf("%s %s", value, unit)
        }
      ),
      
      mitigation_result = colDef(
        name = "Do TIP projects mitigate the DI/DB?",
        cell = pill_buttons(
          data = .,
          box_shadow =  F,
          color_ref = 'color_mitigation_result',
          bold_text = T
        )
      ),
      
      # Don't show helper columns
      color_mitigation_result = colDef(show = FALSE),
      unit = colDef(show = FALSE),
      color_mitigation_direction = colDef(show = FALSE)
    ),
    
    # set up column group
    columnGroups = list(
      colGroup(
        name = "How do TIP projects impact DI/DB metrics?",
        columns = c(
          "impact_ej",
          "impact_non_ej",
          "impact",
          "mitigation_direction"
        ),
        headerStyle = list(borderBottom = "2px solid #333333")
      )
    ),
    
    # element id for csv download
    elementId = "didb-mitigation-download-table"
  ) %>%
  
  # titles and support text
  add_title("Projected TIP Project Impacts on DI/DB Metrics") %>%
  add_subtitle("Do TIP projects help mitigate the DI/DB findings from Long Range Transportation Plan 2050?") %>%
  add_source(
    source = "Note: Microtransit projects not included in transit project impacts.
             The environmental justice (EJ) population consists of people who identify as minority or who are considered low-income.",
    font_size = 13
  ) %>%
  add_source("EJ = Environmental Justice", font_size = 11) %>%
  add_source("TIP = Transportation Improvement Program", font_size = 11) %>%
  add_source("DI =  Disparate Impact", font_size = 11) %>%
  add_source("DB = Disproportionate Burden", font_size = 11) %>%
  
  # identify support text font
  # potentially unnecessary to update the font in quarto or when placed in shiny app
  google_font("Arimo")


# render the table in the viewer ####
htmltools::browsable(tagList(
  tags$button(tagList(
    fontawesome::fa("download"), "Download as CSV"
  ),
  onclick = "Reactable.downloadDataCSV('didb-mitigation-download-table', 'mitigation-summary.csv')"),
  mitigation_sumamry_table
))
