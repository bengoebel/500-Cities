library(tidyverse)
library(stringr)
library(modelr)
library(maps)
library(ggthemes)

# Description: An analysis of the 500 Cities dataset
# By: Ben Goebel
# Date: April 22, 2019

#--------------------------------------------------------------------------#
# Functions

# Purpose: Visualizes the relationship between two variables.
# Arguments: x_measure: A character, the measure that will be the independent
#                       variable.
#            y_measure: A character, the measure that will be the dependent 
#                       variable.
#            x_label: A character, the label for the x_measure.
#            y_label: A character, the label for the y_measure.
# Returns: Nothing.
y_vs_x_plot <- function(x_measure, y_measure, x_label, y_label){
  y_vs_x <- grouped_cities %>%
    filter(Measure %in% c(x_measure, y_measure),
           Data_Value_Type == "Age-adjusted prevalence") %>%
    spread(key = Measure, value = AVG_Data_Value) %>%
    rename(!!x_label := x_measure, !!y_label := y_measure)
  
  ungrouped_y_vs_x <- y_vs_x %>%
    ungroup()
  r <- cor(ungrouped_y_vs_x[x_label], ungrouped_y_vs_x[y_label], 
           method = "pearson")[[1]]
  
  formula <- as.formula(str_c(y_label, "~", x_label))
  mod <- lm(formula, data = y_vs_x)
  grid <- y_vs_x %>%
    ungroup() %>%
    data_grid(!!sym(x_label) := seq_range(!!sym(x_label), 5)) %>%
    add_predictions(mod, y_label)
  
  ggplot(y_vs_x, aes_string(x = x_label, y = y_label)) +
    geom_point() +
    geom_line(data = grid, color = "red", size = 1.5) +
    labs(title = str_c(str_replace_all(y_label, "_", " "), 
                       " as a function of ", 
                       str_replace_all(x_label, "_", " ")),
         x = str_replace_all(x_label, "_", " "),
         y = str_replace_all(y_label, "_", " "),
         caption = bquote(paste(r, " = ", .(format(r, digits = 3)),
                                ", ", r^2, " = ", 
                                .(format(r^2, digits = 3)), 
                                ", Source: 500 Cities"))) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 10))
}

# Purpose: Maps a state abbreviation to a region in the United States.
# Arguments: state: A character, the state abbreviation in the 
#                   United States being mapped.
# Returns: A character, the region.
state_to_region <- function(state){
  if (state %in% c("PA", "NJ", "NY", "CT",
                   "RI", "MA", "VT", "NH", "ME")) {
    "NORTHEAST"  
  } else if (state %in% c("DE", "MD", "VA", "WV", 
                          "KY", "NC", "TN", "SC", "GA", 
                          "AL", "MS", "AR", "LA", "FL")) {
    "SOUTHEAST"
  } else if (state %in% c("OH", "MI", "IN", "WI", "IL", "MN", 
                          "IA", "MO", "ND", "SD", "NE", "KS")){
    "MIDWEST"
  } else if (state %in% c("OK", "NM", "AZ", "TX")){
    "SOUTHWEST"
  } else if (state %in% c("MT", "ID", "WA", 
                          "OR", "WY", "CO", "UT", 
                          "NV", "CA", "AK", "HI")){
    "WEST"
  } else {
    "NA"
  }
}

# Purpose: Maps a vector of state abbreviations to their 
#          respective regions.
# Arguments: states: A vector of characters, which each is a state 
#                    abbreviation in the United States.
# Returns: A vector of characters, which each is a region in the United States.
vectorized_state_to_region <- function(states){
  map_chr(states, state_to_region)
}

# Purpose: Gets the United States states' rankings based on one measure.
# Arguments: measure: A character, the measure, by which the states
#                     will be ranked.
# Returns: A tibble, the states ranked by the measure.
get_states_ranking <- function(measure, measure_label) {
  ranking_column <- str_c(measure_label, "_Ranking")
  value_column <- str_c(measure_label, "_Value")
  grouped_states_by_region %>%
    filter(Measure == measure,
           Data_Value_Type == "Age-adjusted prevalence") %>%
    arrange(AVG_Data_Value) %>%
    rowid_to_column(ranking_column) %>%
    rename(!!value_column := AVG_Data_Value) %>%
    ungroup() %>%
    select(State_Name, Region, Num_Cities, !!ranking_column, !!value_column)
}

#--------------------------------------------------------------------------#
# Inputting and organizing data

cities <- read_csv("500_Cities_Local_Data_for_Better_Health_2017.csv")
colnames(cities)[colnames(cities) == "StateDesc"] <-  "State_Name"
cities$State_Name[cities$State_Name == "North Carolin"] <- "North Carolina"
cities$State_Name[cities$State_Name == "South Carolin"] <- "South Carolina"

grouped_cities <- cities %>%
  filter(StateAbbr != "US") %>%
  group_by(Year, StateAbbr, State_Name, CityName, Measure, Data_Value_Type) %>%
  select(Year, StateAbbr, State_Name, CityName, Measure, Data_Value_Type, Data_Value) %>%
  summarize(AVG_Data_Value = mean(Data_Value, na.rm = TRUE))

grouped_states <- grouped_cities %>%
  filter(StateAbbr != "DC") %>%
  group_by(Year, StateAbbr, State_Name, Measure, Data_Value_Type) %>%
  select(Year, StateAbbr, State_Name, Measure, Data_Value_Type, AVG_Data_Value) %>%
  summarize(AVG_Data_Value = mean(AVG_Data_Value, na.rm = TRUE),
            Num_Cities = n())

grouped_states_by_region <- grouped_states %>%
  mutate(Region = vectorized_state_to_region(StateAbbr)) %>%
  filter(Region != "NA")

#--------------------------------------------------------------------------#
# Correlation Analysis (Possible Coronary Heart Disease Indicators)

# Coronary Heart Disease as a function of High Blood Pressure
y_vs_x_plot("High blood pressure among adults aged >=18 Years",
            "Coronary heart disease among adults aged >=18 Years",
            "High_Blood_Pressure",
            "Coronary_Heart_Disease")

# Coronary Heart Disease as a function of Diabetes
y_vs_x_plot("Diagnosed diabetes among adults aged >=18 Years",
            "Coronary heart disease among adults aged >=18 Years",
            "Diabetes",
            "Coronary_Heart_Disease")

# Coronary Heart Disease as a function of Smoking
y_vs_x_plot("Current smoking among adults aged >=18 Years",
            "Coronary heart disease among adults aged >=18 Years",
            "Smoking",
            "Coronary_Heart_Disease")

# Coronary Heart Disease as a function of Obesity
y_vs_x_plot("Obesity among adults aged >=18 Years",
            "Coronary heart disease among adults aged >=18 Years",
            "Obesity",
            "Coronary_Heart_Disease")

# Coronary Heart Disease as a function of Poor Mental Health
y_vs_x_plot("Mental health not good for >=14 days among adults aged >=18 Years",
            "Coronary heart disease among adults aged >=18 Years",
            "Poor_Mental_Health",
            "Coronary_Heart_Disease")

# Coronary Heart Disease as a function of No Leisure Time Physical Activity
y_vs_x_plot("No leisure-time physical activity among adults aged >=18 Years",
            "Coronary heart disease among adults aged >=18 Years",
            "No_Leisure_Time_Physical_Activity",
            "Coronary_Heart_Disease")

# Coronary Heart Disease as a function of Chronic Kidney Disease
y_vs_x_plot("Chronic kidney disease among adults aged >=18 Years",
            "Coronary heart disease among adults aged >=18 Years",
            "Chronic_Kidney_Disease",
            "Coronary_Heart_Disease")

# Coronary Heart Disease as a function of Stroke
y_vs_x_plot("Stroke among adults aged >=18 Years",
            "Coronary heart disease among adults aged >=18 Years",
            "Stroke",
            "Coronary_Heart_Disease")

# Coronary Heart Disease as a function of Chronic Obstructive Pulmonary Disease
y_vs_x_plot("Chronic obstructive pulmonary disease among adults aged >=18 Years",
            "Coronary heart disease among adults aged >=18 Years",
            "Chronic_Obstructive_Pulmonary_Disease",
            "Coronary_Heart_Disease")

# Coronary Heart Disease as a function of Poor Physical Health
y_vs_x_plot("Physical health not good for >=14 days among adults aged >=18 Years",
            "Coronary heart disease among adults aged >=18 Years",
            "Poor_Physical_Health",
            "Coronary_Heart_Disease")

#--------------------------------------------------------------------------#
# Correlation Analysis (Possible Coronary Heart Disease Indicators, 
# Unhealthy Behaviors and Health Outcomes)

# Stroke as a function of Smoking
y_vs_x_plot("Current smoking among adults aged >=18 Years",
            "Stroke among adults aged >=18 Years",
            "Smoking",
            "Stroke")

# Stroke as a function of No Leisure Time Physical Activity
y_vs_x_plot("No leisure-time physical activity among adults aged >=18 Years",
            "Stroke among adults aged >=18 Years",
            "No_Leisure_Time_Physical_Activity",
            "Stroke")

# Chronic Kidney Disease as a function of No Leisure Time Physical Activity
y_vs_x_plot("No leisure-time physical activity among adults aged >=18 Years",
            "Chronic kidney disease among adults aged >=18 Years",
            "No_Leisure_Time_Physical_Activity",
            "Chronic_Kidney_Disease")

# Poor Physical Health as a function of No Leisure Time Physical Activity
y_vs_x_plot("No leisure-time physical activity among adults aged >=18 Years",
            "Physical health not good for >=14 days among adults aged >=18 Years",
            "No_Leisure_Time_Physical_Activity",
            "Poor_Physical_Health")

# High Blood Pressure as a function of No Leisure Time Physical Activity
y_vs_x_plot("No leisure-time physical activity among adults aged >=18 Years",
            "High blood pressure among adults aged >=18 Years",
            "No_Leisure_Time_Physical_Activity",
            "High_Blood_Pressure")

# Obesity as a function of No Leisure Time Physical Activity
y_vs_x_plot("No leisure-time physical activity among adults aged >=18 Years",
            "Obesity among adults aged >=18 Years",
            "No_Leisure_Time_Physical_Activity",
            "Obesity")

# Diabetes as a function of No Leisure Time Physical Activity
y_vs_x_plot("No leisure-time physical activity among adults aged >=18 Years",
            "Diagnosed diabetes among adults aged >=18 Years",
            "No_Leisure_Time_Physical_Activity",
            "Diabetes")

# Chronic Obstructive Pulmonary Disease as a function of Smoking
y_vs_x_plot("Current smoking among adults aged >=18 Years",
            "Chronic obstructive pulmonary disease among adults aged >=18 Years",
            "Smoking",
            "Chronic_Obstructive_Pulmonary_Disease")

#--------------------------------------------------------------------------#
# State/Region Heart Health Analysis

# Coronary Heart Disease state ranking
chd_ranking <- get_states_ranking("Coronary heart disease among adults aged >=18 Years",
                                  "Coronary_Heart_Disease")

# High Blood Pressure state ranking
hbp_ranking <- get_states_ranking("High blood pressure among adults aged >=18 Years",
                                  "High_Blood_Pressure")

# Diabetes state ranking
diabetes_ranking <- get_states_ranking("Diagnosed diabetes among adults aged >=18 Years",
                                       "Diabetes")

# Smoking state ranking
smoking_ranking <- get_states_ranking("Current smoking among adults aged >=18 Years", 
                                      "Smoking")

# Obesity state ranking
obesity_ranking <- get_states_ranking("Obesity among adults aged >=18 Years",
                                      "Obesity")

# Poor Mental Health state ranking
pmh_ranking <- get_states_ranking("Mental health not good for >=14 days among adults aged >=18 Years", 
                                  "Poor_Mental_Health")

# No Leisure Time Physical Activity state ranking
nltpa_ranking <- get_states_ranking("No leisure-time physical activity among adults aged >=18 Years",
                                    "No_Leisure_Time_Physical_Activity")

# Chronic Kidney Disease state ranking
ckd_ranking <- get_states_ranking("Chronic kidney disease among adults aged >=18 Years", 
                                  "Chronic_Kidney_Disease")

# Stroke state ranking
stroke_ranking <- get_states_ranking("Stroke among adults aged >=18 Years", 
                                     "Stroke")

# Chronic Obstructive Pulmonary Disease state ranking
copd_ranking <- get_states_ranking("Chronic obstructive pulmonary disease among adults aged >=18 Years", 
                                   "Chronic_Obstructive_Pulmonary_Disease")

# Poor Physical Health state ranking
pph_ranking <- get_states_ranking("Physical health not good for >=14 days among adults aged >=18 Years", 
                                  "Poor_Physical_Health")

# State Heart Health Ranking
heart_ranking_by_state <- reduce(list(chd_ranking, hbp_ranking, diabetes_ranking, 
                                    smoking_ranking, obesity_ranking, pmh_ranking, 
                                    nltpa_ranking, ckd_ranking, stroke_ranking, 
                                    copd_ranking, pph_ranking), full_join) %>%
  mutate(Total_Score = Coronary_Heart_Disease_Ranking + High_Blood_Pressure_Ranking + 
                       Diabetes_Ranking + Smoking_Ranking +
                       Obesity_Ranking + Poor_Mental_Health_Ranking +
                       No_Leisure_Time_Physical_Activity_Ranking +
                       Chronic_Kidney_Disease_Ranking + Stroke_Ranking +
                       Chronic_Obstructive_Pulmonary_Disease_Ranking +
                       Poor_Physical_Health_Ranking) %>%
  select(State_Name, Region, Num_Cities,
         Coronary_Heart_Disease_Ranking, Coronary_Heart_Disease_Value,
         High_Blood_Pressure_Ranking, High_Blood_Pressure_Value,
         Diabetes_Ranking, Diabetes_Value,
         Smoking_Ranking, Smoking_Value,
         Obesity_Ranking, Obesity_Value,
         Poor_Mental_Health_Ranking, Poor_Mental_Health_Value,
         No_Leisure_Time_Physical_Activity_Ranking, No_Leisure_Time_Physical_Activity_Value,
         Chronic_Kidney_Disease_Ranking, Chronic_Kidney_Disease_Value,
         Stroke_Ranking, Stroke_Value,
         Chronic_Obstructive_Pulmonary_Disease_Ranking, Chronic_Obstructive_Pulmonary_Disease_Value,
         Poor_Physical_Health_Ranking, Poor_Physical_Health_Value,
         Total_Score) %>%
  arrange(Total_Score) %>%
  rowid_to_column("State_Heart_Health_Ranking")

# Region Heart Health Ranking
heart_ranking_by_region <- heart_ranking_by_state %>%
  group_by(Region) %>%
  summarize(Num_States = n(),
            Coronary_Heart_Disease_Value = mean(Coronary_Heart_Disease_Value),
            High_Blood_Pressure_Value = mean(High_Blood_Pressure_Value),
            Diabetes_Value = mean(Diabetes_Value),
            Smoking_Value = mean(Smoking_Value),
            Obesity_Value = mean(Obesity_Value),
            Poor_Mental_Health_Value = mean(Poor_Mental_Health_Value),
            No_Leisure_Time_Physical_Activity_Value = mean(No_Leisure_Time_Physical_Activity_Value),
            Chronic_Kidney_Disease_Value = mean(Chronic_Kidney_Disease_Value),
            Stroke_Value = mean(Stroke_Value),
            Chronic_Obstructive_Pulmonary_Disease_Value = mean(Chronic_Obstructive_Pulmonary_Disease_Value),
            Poor_Physical_Health_Value = mean(Poor_Physical_Health_Value)) %>%
  mutate(Coronary_Heart_Disease_Ranking = rank(Coronary_Heart_Disease_Value),
         High_Blood_Pressure_Ranking = rank(High_Blood_Pressure_Value),
         Diabetes_Ranking = rank(Diabetes_Value),
         Smoking_Ranking = rank(Smoking_Value),
         Obesity_Ranking = rank(Obesity_Value),
         Poor_Mental_Health_Ranking = rank(Poor_Mental_Health_Value),
         No_Leisure_Time_Physical_Activity_Ranking = rank(No_Leisure_Time_Physical_Activity_Value),
         Chronic_Kidney_Disease_Ranking = rank(Chronic_Kidney_Disease_Value),
         Stroke_Ranking = rank(Stroke_Value),
         Chronic_Obstructive_Pulmonary_Disease_Ranking = rank(Chronic_Obstructive_Pulmonary_Disease_Value),
         Poor_Physical_Health_Ranking = rank(Poor_Physical_Health_Value),
         Total_Score = Coronary_Heart_Disease_Ranking +
                       High_Blood_Pressure_Ranking +
                       Diabetes_Ranking +
                       Smoking_Ranking +
                       Obesity_Ranking +
                       Poor_Mental_Health_Ranking + 
                       No_Leisure_Time_Physical_Activity_Ranking +
                       Chronic_Kidney_Disease_Ranking +
                       Stroke_Ranking +
                       Chronic_Obstructive_Pulmonary_Disease_Ranking +
                       Poor_Physical_Health_Ranking) %>%
  select(Region, Num_States,
         Coronary_Heart_Disease_Ranking, Coronary_Heart_Disease_Value,
         High_Blood_Pressure_Ranking, High_Blood_Pressure_Value,
         Diabetes_Ranking, Diabetes_Value,
         Smoking_Ranking, Smoking_Value,
         Obesity_Ranking, Obesity_Value,
         Poor_Mental_Health_Ranking, Poor_Mental_Health_Value,
         No_Leisure_Time_Physical_Activity_Ranking, No_Leisure_Time_Physical_Activity_Value,
         Chronic_Kidney_Disease_Ranking, Chronic_Kidney_Disease_Value,
         Stroke_Ranking, Stroke_Value,
         Chronic_Obstructive_Pulmonary_Disease_Ranking, Chronic_Obstructive_Pulmonary_Disease_Value,
         Poor_Physical_Health_Ranking, Poor_Physical_Health_Value,
         Total_Score) %>%
  arrange(Total_Score) %>%
  rowid_to_column("Region_Heart_Health_Ranking")

# State/Region Heart Health Ranking Visualization
state_heart_ranking <- heart_ranking_by_state %>%
  select(State_Name, Region, State_Heart_Health_Ranking)

state_heart_ranking$State_Name <- tolower(state_heart_ranking$State_Name)

colnames(state_heart_ranking)[colnames(state_heart_ranking) == "Region"] <-  "US_Region"
colnames(state_heart_ranking)[colnames(state_heart_ranking) == "State_Name"] <-  "region"

states <- map_data("state")

state_heart_ranking$state_heart_ranking_group <- cut(state_heart_ranking$State_Heart_Health_Ranking, 
                                                     breaks = c(0, 10, 20, 30, 40, 50),
                                                     labels = c("1-10", "11-20", "21-30", "31-40", "41-50"))

# State heart health ranking visualization
state_heart_health_ranking_viz <- ggplot() + 
                                    geom_map(data = states, map = states,
                                             aes(x = long, y = lat, map_id = region)) +
                                    geom_map(data = state_heart_ranking, map = states,
                                             aes(fill = state_heart_ranking_group, map_id = region),
                                             color = "black", size = 0.25) +
                                    scale_fill_manual(name = "State Heart Health Ranking", 
                                                      breaks = c( "1-10", 
                                                                 "11-20", 
                                                                 "21-30", 
                                                                 "31-40",
                                                                 "41-50"),
                                                      values = colorspace::diverge_hsv(5)) +
                                    coord_map("mercator") +
                                    theme_map() +
                                    theme(legend.position = "right")

# Region heart health ranking visualization
heart_ranking_by_region_copy <- heart_ranking_by_region

colnames(heart_ranking_by_region_copy)[colnames(heart_ranking_by_region_copy) == "Region"] <- "US_Region"

region_heart_ranking <- left_join(state_heart_ranking, heart_ranking_by_region_copy, by = "US_Region") %>%
  select(region, US_Region, Region_Heart_Health_Ranking) %>%
  mutate(Region_Heart_Health_Ranking = as.character(Region_Heart_Health_Ranking))

region_heart_health_ranking_viz <- ggplot() + 
                                    geom_map(data = states, map = states,
                                             aes(x = long, y = lat, map_id = region)) +
                                    geom_map(data = region_heart_ranking, map = states,
                                             aes(fill = Region_Heart_Health_Ranking, map_id = region),
                                             color = "black", size = 0.25) +
                                    scale_fill_manual(name = "Region Heart Health Ranking", 
                                                      breaks = c("1", 
                                                                 "2", 
                                                                 "3", 
                                                                 "4",
                                                                 "5"),
                                                      values = colorspace::diverge_hsv(5)) +
                                    coord_map("mercator") +
                                    theme_map() +
                                    theme(legend.position = "right")

#--------------------------------------------------------------------------#
#end

