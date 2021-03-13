# Packages ----
library(shiny)
library(rsconnect)
library(tidyverse)
library(ggrepel)

# Loading data ----
# Import "real" historical GDP growth (will also have projections)
# Step is navigate to https://web.archive.org/web/20191015153905/https://www.imf.org/external/pubs/ft/weo/2019/02/weodata/index.aspx
# Select By Countries and select All Countries, then "Gross domestic product, constant prices, Percent change",
# Then select 1988 as starting year, as historical database starts then
# Also select WEO country code and ISO3 code, don't select Subject Descriptor
# Repeat steps for "By Country Groups"
# CAN THIS BE DONE PROGRAMMATICALLY?
weo_real <- read_csv("Input/WEO_Countries_Oct2020.csv") %>%
    # Drop missing obs that just has metadata in it
    filter(!is.na(Country)) %>%
    # Clean out missing observations, which are coded as "n/a and change columns to numeric"
    mutate(across(`1988`:`2025`, ~as.numeric(str_remove(.x, "n/a")))) %>%
    # Add Country aggregates info
    bind_rows(read_csv("Input/WEO_Aggregates_Oct2020.csv") %>%
                  # Drop missing obs that just has metadata in it
                  filter(!is.na(`Country Group Name`)) %>%
                  # Line up variable names with country dataset
                  rename(Country = `Country Group Name`, `WEO Country Code` = `WEO Country Group Code`) %>%
                  # # Clean out missing observations, which are coded as "n/a and change columns to numeric"
                  mutate(across(`1988`:`1991`, ~as.numeric(str_remove(.x, "n/a"))))) %>%
    # Reshape to long
    pivot_longer(`1988`:`2025`, names_to = "year") %>%
    # Select and rename right variables
    select(country = Country, WEO_Country_Code = `WEO Country Code`, ISOAlpha_3Code = ISO, year, value) %>%
    # Clean up values and years, create placeholder f_year variable
    mutate(f_year = NA_real_,
           year = as.numeric(year),
           WEO_Country_Code = as.numeric(WEO_Country_Code))

# Import historical database directly, hopefully link is stable
# Link of this file
# https://web.archive.org/web/20210313080306/https://www.imf.org/external/pubs/ft/weo/data/WEOhistorical.xlsx
# As of October 2020 update, file is found on right-hand side menu of IMF WEO database 
# https://web.archive.org/web/20210313080306/https://www.imf.org/en/Publications/WEO/weo-database/2020/October
# File is called "Historical WEO Forecasts Database"

# Import excel file directly via httr and readxl
weo_url <- "https://www.imf.org/external/pubs/ft/weo/data/WEOhistorical.xlsx"
httr::GET(weo_url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
weo <- readxl::read_excel(tf, sheet = 2) %>%
    # Keep only fall estimates
    select(country, WEO_Country_Code, ISOAlpha_3Code, year, starts_with("F")) %>%
    # If destringing all estimate variables first
    # mutate_at(vars(starts_with("F")), as.numeric) %>%
    # Reshape to long
    pivot_longer(starts_with("F"), names_to = "f_year") %>%
    # Destring value. Extract year from forecast variable name and make numeric for easy filtering
    mutate(value = as.numeric(value), f_year = as.numeric(str_extract(f_year, pattern = "[0-9]{4}"))) %>%
    # Append "real" GDP growth series
    bind_rows(weo_real) %>%
    # Sort for easier reading of dataset
    arrange(WEO_Country_Code, f_year, year)

weo_graph <- weo %>%
    # Filter for 1. Country/Region 2. Year 3. WEO Version and get rid of "real" series, signified where f_year is NA
    filter(year>=2010, f_year>=2012 & !is.na(f_year)) %>%
    mutate(val_year = case_when(
        !is.na(value) == TRUE ~ year,
        TRUE ~ NA_real_
    )) %>%
    group_by(f_year) %>%
    mutate(label = case_when(
        year == max(val_year, na.rm = TRUE) ~ as.character(f_year),
        TRUE ~ NA_character_
    )) %>%
    ungroup()
    

final_year <- weo %>% summarize(final_year = max(f_year, na.rm = TRUE)) %>% pull()

weo_series_real <- weo %>% 
    filter(year>=2010 & year < final_year, is.na(f_year))

# ui.R ----
ui <- fluidPage(
    position = "right",
    titlePanel("WEO Economic Growth Test"),  # Add a title panel
    sidebarLayout(  # Make the layout a sidebarLayout
        sidebarPanel(
            selectInput(inputId = "country",  # Give the input a name "genotype"
                        label = "1. Select country/region",  # Give the input a label to be displayed in the app
                        choices = unique(weo_graph$country))
        ),  # Inside the sidebarLayout, add a sidebarPanel
        mainPanel(
            plotOutput("plot")
        )  # Inside the sidebarLayout, add a mainPanel
    )
)

# Figure out filter mechanism with pipes
# server.R ----
server <- function(input, output) {
    output$plot <- renderPlot(
        weo_graph %>%
            filter(country == input$country) %>%
            # Draw up mapping, group by WEO version
            ggplot(aes(x = year, y = value, color = as.factor(f_year))) +
            # Select geom
            geom_line() +
            # Add "real" series back in by bringing in weo again, this time keeping NA for f_year
            # Also restrict maximum display to year before the last projection. That value is 2019 in 2019
            geom_line(data = weo_series_real %>% filter(country == input$country),
                      color = "black", size = 0.9) +
            # Add vertical for current year
            geom_vline(xintercept = as.integer(format(Sys.Date(), "%Y")), linetype = "dashed") +
            geom_text_repel(aes(label = label),
                            nudge_x = 0.5,
                            segment.color = "grey",
                            na.rm = TRUE) +
            # Add labels, including for legend, add caption clarifying black line
            labs(x = "", y = "Real GDP Growth", color = "Fall WEO", caption = "Solid black line represents historical estimates of most recent WEO",
                 title = "Growth prospects for input$country") +
            # Make each year display on x axis. Fix for shiny? Need it to look good dynamically
            scale_x_continuous(breaks = seq(min(weo_graph$year, na.rm = TRUE), max(weo_graph$year, na.rm = TRUE), 1)) +
            theme_classic() +
            theme(legend.position = "off")
    )
}

# Run the app ----
shinyApp(ui = ui, server = server)
