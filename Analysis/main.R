# ==== HHPM8 Midterm Assessment Question 3 ====

rm(list=ls())

# Set working directory
setwd("~/University/MSc/Programming/Final/Q3")

# Download necessary packages
library(tidyverse)
library(rvest)
library(purrr) 
library(stringr)
library(janitor)
library(maps)
library(gridExtra)
library(ggplot2)
library(reshape2)
library(texreg)
library(xtable)

####################
### Web scraping ###

# EIA link to scrape
primary_energy <- read_html("https://www.eia.gov/state/seds/data.php?incfile=/state/seds/sep_sum/html/rank_use_source.html")


### Extract data from Table C12.  Primary Energy Consumption Estimates by Source, Ranked by State, 2021 ###

rows <- primary_energy %>%
  html_elements("table") %>% # Select the node containing the table
  html_elements("tr")  # Extract all the table row elements

process_row <- function(row) {
  # Extract the table data cells from given row
  td_content <- html_elements(row, "td") %>% html_text(trim = TRUE)
  # Check if the row has 11 elements and the first element is numeric (rank)
  if (length(td_content) == 11 && !is.na(is.numeric(td_content[1]))) {
    # Process the data in the row removing rank and create matrix with 2 columns (State and Value)
    data <- matrix(td_content[-1], ncol = 2, byrow = TRUE)
    # Create a tibble from the matrix and convert values to numeric
    tibble(
      State = data[, 1],
      Value = as.numeric(gsub(",", "", data[, 2])),
      Energy_Type = rep(c("Coal", "Natural_Gas", "Petroleum", "Nuclear", "Renewable"), length.out = nrow(data))
    )
  } else {
    NULL
  }
}

# Apply the function to each row (<tr>) and combine the results into a single data frame
processed_data <- map_dfr(rows, process_row)  %>% drop_na(Value) 

# Reshape the data so each row is a state and each column is an energy type and rank
energy_data_aggregated <- processed_data %>%
  group_by(State, Energy_Type) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%  
  group_by(Energy_Type) %>%
  filter(State != "United States") %>%
  mutate(Rank = rank(-Value, ties.method = "first")) %>%  # Rank within each Energy_Type
  pivot_wider(
    names_from = Energy_Type,
    values_from = c(Value, Rank),
    names_glue = "{.value}_{.name}"
  ) %>%
  rename_with(~ tolower(gsub("Value_", "", .)), starts_with("Value_")) %>% 
  rename_with(~ tolower(gsub("Rank_", "", .)) %>% paste0(., "_rank"), starts_with("Rank_"))

### Extract data from Table C10.  Total Energy Consumption Estimates, Real Gross Domestic Product (GDP), Energy Consumption Estimates per Real Dollar of GDP, Ranked by State, 2021 ###
energy_gdp <- read_html("https://www.eia.gov/state/seds/data.php?incfile=/state/seds/sep_sum/html/rank_use_gdp.html")


gdp_rows <- energy_gdp %>%
  html_elements("table") %>% # Select the node containing the table
  html_elements("tr") 

process_row_gdp <- function(row) {
  # Extract the table data cells from given row
  td_content <- html_elements(row, "td") %>% html_text(trim = TRUE)
  # Check if the row has 7 elements and the first element is numeric (rank)
  if (length(td_content) == 7 && !is.na(is.numeric(td_content[1]))) {
    data <- matrix(td_content[-1], ncol = 2, byrow = TRUE)
    # Create a tibble from the matrix and convert values to numeric
    tibble(
      State = data[, 1],
      Value = as.numeric(gsub(",", "", data[, 2])),
      Consumption = rep(c("total_energy_consumption", "real_gdp", "energy_consumption_per_gdp"), length.out = nrow(data))
    )
  } else {
    NULL
  }
}

# Apply the function to each row (<tr>) and combine the results into a single data frame
processed_data_gdp <- map_dfr(gdp_rows, process_row_gdp)  %>% drop_na(Value) 

# Reshape the data so each row is a state and each column is a variable from Table C10 and a rank variable
energy_gdp_data_aggregated <- processed_data_gdp %>%
  group_by(State, Consumption) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%  
  group_by(Consumption) %>%
  filter(State != "United States") %>%
  mutate(Rank = rank(-Value, ties.method = "first")) %>%  # Rank within each Energy_Type
  pivot_wider(
    names_from = Consumption,
    values_from = c(Value, Rank),
    names_glue = "{.value}_{.name}"
  ) %>%
  rename_with(~ tolower(gsub("Value_", "", .)), starts_with("Value_")) %>% 
  rename_with(~ tolower(gsub("Rank_", "", .)) %>% paste0(., "_rank"), starts_with("Rank_"))

# Merge energy and gdp data from EIA
merged_energy_data <- energy_data_aggregated %>% 
  left_join(energy_gdp_data_aggregated, by = "State") 

# Save the scraped data to a CSV file
write_csv(merged_energy_data, "merged_energy_data.csv")

############################  
### Download Census data ###
download.file("https://drive.google.com/uc?export=download&id=1f38At3FSIg-e8xfOv6k9SCTtlENLMGly", "census_data.csv")


census_df <- read_csv("census_data.csv")

census_df_clean <- census_df %>% filter(POPGROUP_LABEL =="Total population") %>% clean_names()

# Merge the two data frames
merged_data <- merged_energy_data %>% 
  left_join(census_df_clean, by = c("State" = "name")) %>%
  filter(State != "United States") %>%
  mutate(across(!all_of(c("State", "geo_id", "popgroup", "popgroup_label", "s0201_038e")), as.numeric)) %>%
  mutate(across(c(2:6, energy_consumption_per_gdp:total_energy_consumption) & !all_of(c("geo_id", "s0201_038e")),
                ~log(. + .1),
                .names = "log_{.col}")) # Log-transform the data adding a small constant to avoid log(0)


################
### Analysis ###

# Regressions
mod_coal <- lm(log_coal ~  s0201_008e + s0201_158e + s0201_180e + s0201_018e + I(s0201_018e^2) + s0201_214e + s0201_096e + s0201_058e + log_real_gdp + log_energy_consumption_per_gdp, data = merged_data)   # s0201_008e is female, s0201_158e is unemplolyment rate, s0201_018e is median age, s0201_180e is Natural resources, construction, and maintenance occupations, s0201_214e is Median household income (2021 dollars), s0201_096e is high school degree or higher, s0201_058e is average household size
summary(mod_coal)

mod_nuclear <- lm(log_nuclear ~ s0201_008e + s0201_158e + s0201_180e + s0201_018e + I(s0201_018e^2) + s0201_214e + s0201_096e + s0201_058e + log_real_gdp + log_energy_consumption_per_gdp, data = merged_data)
summary(mod_nuclear)

mod_renewable <- lm(log_renewable ~ s0201_008e + s0201_158e + s0201_180e + s0201_018e + I(s0201_018e^2) + s0201_214e + s0201_096e + s0201_058e + log_real_gdp + log_energy_consumption_per_gdp, data = merged_data)
summary(mod_renewable)

mod_petroleum <- lm(log_petroleum ~ s0201_008e + s0201_158e + s0201_180e + s0201_018e + I(s0201_018e^2) + s0201_214e + s0201_096e + s0201_058e + log_real_gdp + log_energy_consumption_per_gdp, data = merged_data)
summary(mod_petroleum)

mod_natural_gas <- lm(log_natural_gas ~ s0201_008e + s0201_158e + s0201_180e + s0201_018e + I(s0201_018e^2) + s0201_214e + s0201_096e + s0201_058e + log_real_gdp + log_energy_consumption_per_gdp, data = merged_data)
summary(mod_natural_gas)

# Create a LaTeX table of the regression results
latex_table <- texreg(list(mod_coal, mod_nuclear, mod_renewable, mod_petroleum, mod_natural_gas),
                      file = "regression_table.tex", # Use NULL to return the LaTeX code as a character vector
                      custom.model.names = c("Coal", "Nuclear", "Renewable", "Petroleum", "Natural Gas"),
                      custom.coef.names = c("Intercept", "Female", "Unemployment Rate", "NR, Construction, Maintenance Occupations", 
                                            "Median Age", "Median Age^2", "Median Household Income", 
                                            "High School Degree or Higher", "Average Household Size", "Log Real GDP", "Log Energy Consumption per GDP"),
                      include.rsquared = TRUE,
                      include.adjrs = TRUE,
                      include.nobs = TRUE,
                      stars = c(0.01, 0.05, 0.1),
                      caption = "Regression Output for each Energy Type")



## Create energy type maps ##
states_map <- map_data("state")

# Prepare the energy data with a state map
energy_map_data <- energy_data_aggregated %>%
  mutate(State = tolower(State)) %>%
  inner_join(states_map, by = c("State" = "region"))

# Convert to lowercase for maps library
energy_map_data$State <- tolower(energy_map_data$State) 
plot_titles <- c(
  "coal_rank" = "Coal Consumption Ranking",
  "natural_gas_rank" = "Natural Gas Consumption Ranking",
  "petroleum_rank" = "Petroleum Consumption Ranking",
  "nuclear_rank" = "Nuclear Energy Consumption Ranking",
  "renewable_rank" = "Renewable Energy Consumption Ranking"
)

# Create a list of plots using the more descriptive titles
plots <- lapply(names(plot_titles), function(rank_column) {
  ggplot(energy_map_data, aes_string(x = "long", y = "lat", group = "group", fill = rank_column)) +
    geom_polygon(color = "black", size = 0.25) +
    expand_limits(x = states_map$long, y = states_map$lat) +
    scale_fill_continuous(low = "darkred", high = "white", name = "Consumption Rank") +
    labs(title = plot_titles[[rank_column]]) +  # Use the descriptive title
    theme_void() +
    theme(legend.position = "none", aspect.ratio = 3 / 5)  # Adjust the aspect ratio
})

# Arrange the plots into a grid
energy_type_ranks <- do.call(grid.arrange, c(plots, ncol = 2))

# Save the grid to a PDF file
ggsave("energy_type_ranks.pdf", energy_type_ranks, width = 10, height = 6, units = "in")

# Graph GDP and Coal Consumption Ranking
gdp_coal_rank <- ggplot(merged_data, aes(x = log_real_gdp, y = log_coal, color = coal_rank)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "GDP and Coal Consumption Ranking",
    x = "Log Real GDP",
    y = "Log Coal Consumption",
    color = "Coal Consumption Rank"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
  scale_color_continuous(low = "darkred", high = "white", name = "Coal Consumption Rank")

# Descriptive statistics of demographics based on state
demographics_summary <- summary(merged_data[, c("State", "s0201_008e", "s0201_158e", "s0201_018e", "s0201_214e", "s0201_096e", "s0201_058e")])

# Create table of descriptive statistics
descriptive_stats <- merged_data %>% 
  group_by(State) %>%
  summarise(
    Mean_Female = mean(s0201_008e, na.rm = TRUE),
    SD_Female = sd(s0201_008e, na.rm = TRUE),
    Mean_Unemployment_Rate = mean(s0201_158e, na.rm = TRUE),
    SD_Unemployment_Rate = sd(s0201_158e, na.rm = TRUE),
    Mean_Median_Age = mean(s0201_018e, na.rm = TRUE),
    SD_Median_Age = sd(s0201_018e, na.rm = TRUE),
    Mean_Median_Household_Income = mean(s0201_214e, na.rm = TRUE),
    SD_Median_Household_Income = sd(s0201_214e, na.rm = TRUE),
    Mean_High_School_Degree_or_Higher = mean(s0201_096e, na.rm = TRUE),
    SD_High_School_Degree_or_Higher = sd(s0201_096e, na.rm = TRUE),
    Mean_Average_Household_Size = mean(s0201_058e, na.rm = TRUE),
    SD_Average_Household_Size = sd(s0201_058e, na.rm = TRUE)
  )

# Export the table to LaTeX format
summary_stats_table <- xtable(descriptive_stats, file = "descriptive_stats.tex", caption = "State-Based Descriptive Statistics", label = "tab:desc_stats")
print(summary_stats_table, include.rownames = FALSE, hline.after = c(-1,0), comment = FALSE, file = "descriptive_table.tex")
