df <- read.csv("/Users/bryanttsai/Desktop/mlb/pitch_level_2023_segmented.csv")
#
library(dplyr) 
library(stringr) 

# Function to perform t-test and print results #Fulmer, Michael
perform_t_test <- function(df, column, condition) {
  a_data <- df %>%
    filter(str_detect(round_situation, 'A'), player_name == 'Fulmer, Michael') %>%
    select(round_situation, {{ column }}, player_name)
  
  b_data <- df %>%
    filter(str_detect(round_situation, 'B'), player_name == 'Fulmer, Michael') %>%
    select(round_situation, {{ column }}, player_name)
  p
  t_test_result <- t.test(a_data[[column]], b_data[[column]])
  
  cat("Variable:", column, "\n")
  print(t_test_result)
  cat("\n")
}

# List of variables to analyze
variables_to_analyze <- c(
  "effective_speed", "release_spin_rate", "pfx_x", "pfx_z", 
  "plate_x", "plate_z", "release_pos_x", "release_pos_z", 
  "spin_axis", "release_extension", "launch_speed", 
  "launch_speed_angle"
)

# Apply the function to each variable
for (variable in variables_to_analyze) {
  perform_t_test(df, variable, condition)
}

#----------------------------------------------------------------------------------------

# Function to perform t-test and print results #Abreu, Albert
perform_t_test <- function(df, column, condition) {
  a_data <- df %>%
    filter(str_detect(round_situation, 'A'), player_name == 'Abreu, Albert') %>%
    select(round_situation, {{ column }}, player_name)
  
  b_data <- df %>%
    filter(str_detect(round_situation, 'B'), player_name == 'Abreu, Albert') %>%
    select(round_situation, {{ column }}, player_name)
  
  t_test_result <- t.test(a_data[[column]], b_data[[column]])
  
  cat("Variable:", column, "\n")
  print(t_test_result)
  cat("\n")
}

# List of variables to analyze
variables_to_analyze <- c(
  "effective_speed", "release_spin_rate", "pfx_x", "pfx_z", 
  "plate_x", "plate_z", "release_pos_x", "release_pos_z", 
  "spin_axis", "release_extension", "launch_speed", 
  "launch_speed_angle"
)

# Apply the function to each variable
for (variable in variables_to_analyze) {
  perform_t_test(df, variable, condition)
}

###-------------------------------------------------------------
p2 <- df%>% mutate(inZone = ifelse(zone < 11, 1, 0)) 
head(p2) 
# Command 1
a_inZone <-p2%>% 
  filter(str_detect(round_situation, 'A'), player_name == 'Fulmer, Michael') %>% 
  select(round_situation, inZone, player_name) 
b_inZone<-p2%>%  
  filter(str_detect(round_situation, 'B'), player_name == 'Fulmer, Michael') %>% 
  select(round_situation, inZone, player_name) 
# Perform t-test 
t_test_result <- t.test(a_inZone$inZone, b_inZone$inZone) 
# Print the t-test result, there's no significant difference for Buck Farmer's inZone under A or B 
print(t_test_result)

# Command 2
a_inZone <-p2%>% 
  filter(str_detect(round_situation, 'A'), player_name == 'Abreu, Albert') %>% 
  select(round_situation, inZone, player_name) 
b_inZone<-p2%>%  
  filter(str_detect(round_situation, 'B'), player_name == 'Abreu, Albert') %>% 
  select(round_situation, inZone, player_name) 
# Perform t-test 
t_test_result <- t.test(a_inZone$inZone, b_inZone$inZone) 
# Print the t-test result, there's no significant difference for Buck Farmer's inZone under A or B 
print(t_test_result)

#----------------------------------------------------------------------------------------

# Function to perform t-test and print results #Brent, Suter
perform_t_test <- function(df, column, condition) {
  a_data <- df %>%
    filter(str_detect(round_situation, 'B1'), player_name == 'Suter, Brent') %>%
    select(round_situation, {{ column }}, player_name)
  
  b_data <- df %>%
    filter(str_detect(round_situation, 'B2'), player_name == 'Suter, Brent') %>%
    select(round_situation, {{ column }}, player_name)
  
  t_test_result <- t.test(a_data[[column]], b_data[[column]])
  
  cat("Variable:", column, "\n")
  print(t_test_result)
  cat("\n")
}

# List of variables to analyze
variables_to_analyze <- c(
  "effective_speed", "release_spin_rate", "pfx_x", "pfx_z", 
  "plate_x", "plate_z", "release_pos_x", "release_pos_z", 
  "spin_axis", "release_extension", "launch_speed", 
  "launch_speed_angle"
)

# Apply the function to each variable
for (variable in variables_to_analyze) {
  perform_t_test(df, variable, condition)
}
# Command 3
a_inZone <-p2%>% 
  filter(str_detect(round_situation, 'B1'), player_name == 'Suter, Brent') %>% 
  select(round_situation, inZone, player_name) 
b_inZone<-p2%>%  
  filter(str_detect(round_situation, 'B2'), player_name == 'Suter, Brent') %>% 
  select(round_situation, inZone, player_name) 
# Perform t-test 
t_test_result <- t.test(a_inZone$inZone, b_inZone$inZone) 
# Print the t-test result, there's no significant difference for Suter, Brent's inZone under A or B 
print(t_test_result)
#__________________________________________

perform_t_test <- function(df, column, condition) {
  a_data <- df %>%
    filter(str_detect(round_situation, 'A'), player_name == 'Pagán, Emilio') %>%
    select(round_situation, {{ column }}, player_name)
  
  b_data <- df %>%
    filter(str_detect(round_situation, 'B'), player_name == 'Pagán, Emilio') %>%
    select(round_situation, {{ column }}, player_name)
  
  t_test_result <- t.test(a_data[[column]], b_data[[column]])
  
  cat("Variable:", column, "\n")
  print(t_test_result)
  cat("\n")
}

# List of variables to analyze
variables_to_analyze <- c(
  "effective_speed", "release_spin_rate", "pfx_x", "pfx_z", 
  "plate_x", "plate_z", "release_pos_x", "release_pos_z", 
  "spin_axis", "release_extension", "launch_speed", 
  "launch_speed_angle"
)

# Apply the function to each variable
for (variable in variables_to_analyze) {
  perform_t_test(df, variable, condition)
}
###______________________________

perform_t_test <- function(df, column, condition) {
  a_data <- df %>%
    filter(str_detect(round_situation, 'A'), player_name == 'Farmer, Buck') %>%
    select(round_situation, {{ column }}, player_name)
  
  b_data <- df %>%
    filter(str_detect(round_situation, 'B'), player_name == 'Farmer, Buck') %>%
    select(round_situation, {{ column }}, player_name)
  
  t_test_result <- t.test(a_data[[column]], b_data[[column]])
  
  cat("Variable:", column, "\n")
  print(t_test_result)
  cat("\n")
}

# List of variables to analyze
variables_to_analyze <- c(
  "effective_speed", "release_spin_rate", "pfx_x", "pfx_z", 
  "plate_x", "plate_z", "release_pos_x", "release_pos_z", 
  "spin_axis", "release_extension", "launch_speed", 
  "launch_speed_angle"
)

# Apply the function to each variable
for (variable in variables_to_analyze) {
  perform_t_test(df, variable, condition)
}
###______________________________

perform_t_test <- function(df, column, condition) {
  a_data <- df %>%
    filter(str_detect(round_situation, 'A'), player_name == 'Brasier, Ryan') %>%
    select(round_situation, {{ column }}, player_name)
  
  b_data <- df %>%
    filter(str_detect(round_situation, 'B'), player_name == 'Brasier, Ryan') %>%
    select(round_situation, {{ column }}, player_name)
  
  t_test_result <- t.test(a_data[[column]], b_data[[column]])
  
  cat("Variable:", column, "\n")
  print(t_test_result)
  cat("\n")
}

# List of variables to analyze
variables_to_analyze <- c(
  "effective_speed", "release_spin_rate", "pfx_x", "pfx_z", 
  "plate_x", "plate_z", "release_pos_x", "release_pos_z", 
  "spin_axis", "release_extension", "launch_speed", 
  "launch_speed_angle"
)

# Apply the function to each variable
for (variable in variables_to_analyze) {
  perform_t_test(df, variable, condition)
}