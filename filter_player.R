p<- read.csv("/Users/bryanttsai/Desktop/mlb/pitch_level_2023_segmented.csv")
#
library(dplyr) 
library(stringr) 
df2<- subset(df, pitcher %in% c('571656'))
##
p_A_MF <- p_A[p_A$player_name == 'Fulmer, Michael', ]
p_B2_MF<- p_B2[p_B2$player_name=='Fulmer, Michael', ]
p_B1_MF<- p_B1[p_B1$player_name=='Fulmer, Michael', ]  
p_A_plus_plus_MF<- p_A_plus_plus[p_A_plus_plus$player_name=='Fulmer, Michael', ] 
p_A_plus_plus_MF
p_A_plus_MF <- p_A_plus[grepl('Fulmer', p_A_plus$player_name), ] 
# Calculate run difference per pitch 
p_B1$runs_diff = p_B1$post_bat_score - p_B1$bat_score 
p_B2$runs_diff = p_B2$post_bat_score - p_B2$bat_score 
# Convert run difference per pitch to binary event 
p_B1$runs_diff <- ifelse(p_B1$runs_diff >= 1, 1, 0) 
p_B2$runs_diff <- ifelse(p_B2$runs_diff >= 1, 1, 0) 
# Build survival model 
res.cox_MF_A_plus_plus <- coxph(Surv(pitch_number_appearance, runs_diff) ~ pitcher_at_bat_number+ launch_speed_angle, data =  p_A_plus_plus_MF) 
summary(res.cox_MF_A_plus_plus) 
res.cox_MF_A_plus <- coxph(Surv(pitch_number_appearance, runs_diff) ~ pitcher_at_bat_number+ launch_speed_angle, data =  p_A_plus_MF) 
summary(res.cox_MF_A_plus) 
res.cox_MF_B1 <- coxph(Surv(pitch_number_appearance, runs_diff) ~ out_bi+ pitcher_at_bat_number, data =  p_B1_MF) 
summary(res.cox_MF_B1) 
res.cox_MF_B2 <- coxph(Surv(pitch_number_appearance, runs_diff) ~  out_bi+ pitcher_at_bat_number+ launch_speed_angle+ launch_speed, data =  p_B2_MF)
summary(res.cox_MF_B2) 
res.cox_MF_A <- coxph(Surv(pitch_number_appearance, runs_diff) ~  pitcher_at_bat_number+ launch_speed_angle, data =  p_A_MF)
summary(res.cox_MF_A) 
# Create survival curves for each dataset 
surv_curve_A_MF <- survfit(res.cox_MF_A) 
surv_curve_B1_MF <- survfit(res.cox_MF_B1) 
surv_curve_B2_MF <- survfit(res.cox_MF_B2) 
surv_curve_A_plus_MF<- survfit(res.cox_MF_A_plus) 
surv_curve_A_plus_plus_MF <- survfit(res.cox_MF_A_plus_plus) 
# Combine the two survival curves into one plot 
combined_plot_BS <- ggplot() +   geom_step(aes(x = surv_curve_A_BS$time, y = surv_curve_A_BS$surv, color = "Situation A"),
                                           direction = "hv", size = 1) +   geom_step(aes(x = surv_curve_B1_BS$time, y = surv_curve_B1_BS$surv, color = "Situation B1"), 
                                                                                     direction = "hv", size = 1) +   geom_step(aes(x = surv_curve_B2_BS$time, y = surv_curve_B2_BS$surv, color = "Situation B2"), 
                                                                                                                               direction = "hv", size = 1) +   geom_step(aes(x = surv_curve_A_plus_BS$time, y = surv_curve_A_plus_BS$surv, color = "Situation A+"),
                                                                                                                                                                         direction = "hv", size = 1) +   geom_step(aes(x = surv_curve_A_plus_plus_BS$time, y = surv_curve_A_plus_plus_BS$surv, color = "Situation A++"),
                                                                                                                                                                                                                   direction = "hv", size = 1) +         labs(title = 'Probability of Not Allowing an Earned Run - Brent Suter', 
                                                                                                                                                                                                                                                              x = 'Number of Pitches', y = 'Probability of not allowing score') +   scale_color_manual(values = c("Situation A" = "#2E9FDF", "Situation B1" = "#FFA500", "Situation B2" = "red",
                                                                                                                                                                                                                                                                                                                                                                  "Situation A+" = "yellow","Situation A++" = "black")) +   scale_linetype_manual(name = "Current Avg. number of Pitch", values = c("Dashed Line" = "dashed")) +   theme_minimal() +   theme(plot.title = element_text(face = 'bold')) +   guides(color = guide_legend(title = NULL), linetype = guide_legend(title = NULL))
# Print the combined plot 
print(combined_plot_BS)


# speed ＃FarmerBuck
a_effective_speed <- df%>%
  filter(str_detect(round_situation, 'A') & player_name == 'Farmer, Buck') %>%
  select(round_situation, effective_speed, player_name) 
b_effective_speed <- df %>%
  filter(str_detect(round_situation, 'B') & player_name == 'Farmer, Buck') %>%
  select(round_situation, effective_speed, player_name)
# Perform t-test 
t_test_result <- t.test(a_effective_speed$effective_speed, b_effective_speed$effective_speed) 
# Print the t-test result, no significant difference for Buck Farmer's effective speed 
print(t_test_result)
####
# release_spin_rate 
a_release_spin_rate <-df%>% 
  filter(str_detect(round_situation, 'A'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, release_spin_rate, player_name) 
b_release_spin_rate <- df%>% 
  filter(str_detect(round_situation, 'B'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, release_spin_rate, player_name) 
# Perform t-test 
t_test_result <- t.test(a_release_spin_rate $release_spin_rate, b_release_spin_rate$release_spin_rate) 
# Print the t-test result, no significant difference for Buck Farmer's release_spin_rate under A or B 
print(t_test_result) 
# pfx_x(Horizontal Movement)
a_pfx_x <-df%>% 
  filter(str_detect(round_situation, 'A'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, pfx_x, player_name)
b_pfx_x <-df%>% 
  filter(str_detect(round_situation, 'B'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, pfx_x, player_name)
# Perform t-test 
t_test_result <- t.test(a_pfx_x $pfx_x, b_pfx_x$pfx_x) 
# Print the t-test result, no significant difference for Buck Farmer's pfx_x under A or B 
print(t_test_result) 
# pfx_z(Vertical Movement) 
a_pfx_z <-df%>% 
  filter(str_detect(round_situation, 'A'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, pfx_z, player_name)
b_pfx_z <-df%>% 
  filter(str_detect(round_situation, 'B'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, pfx_z, player_name) 
# Perform t-test 
t_test_result <- t.test(a_pfx_z $pfx_z, b_pfx_z$pfx_z) 
# Print the t-test result, no significant difference for Buck Farmer's pfx_z under A or B 
print(t_test_result) 
# plate_x(Horizontal position) 
a_plate_x <-df%>% 
  filter(str_detect(round_situation, 'A'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, plate_x, player_name) 
b_plate_x<-df%>% 
  filter(str_detect(round_situation, 'B'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, plate_x, player_name) 
# Perform t-test 
t_test_result <- t.test(a_plate_x $plate_x, b_plate_x$plate_x)
# Print the t-test result, no significant difference for Buck Farmer's plate_x under A or B 
print(t_test_result) 
# plate_z(Vertical position) 
a_plate_z <-df%>% 
  filter(str_detect(round_situation, 'A'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, plate_z, player_name) 
b_plate_z<-df%>% 
  filter(str_detect(round_situation, 'B'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, plate_z, player_name) 
# Perform t-test 
t_test_result <- t.test(a_plate_z $plate_z, b_plate_z$plate_z) 
# Print the t-test result, no significant difference for Buck Farmer's plate_z under A or B 
print(t_test_result) 
# release_pos_x 
a_release_pos_x <-df%>% 
  filter(str_detect(round_situation, 'A'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, release_pos_x, player_name) 
b_release_pos_x<-df%>% 
  filter(str_detect(round_situation, 'B'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, release_pos_x, player_name) 
# Perform t-test 
t_test_result <- t.test(a_release_pos_x $release_pos_x, b_release_pos_x$release_pos_x) 
### Print the t-test result, there's significant difference for Buck Farmer's release_pos_x under A or B 
print(t_test_result) 
# release_pos_z 
a_release_pos_z <-df%>% 
  filter(str_detect(round_situation, 'A'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, release_pos_z, player_name) 
b_release_pos_z<-df%>% 
  filter(str_detect(round_situation, 'B'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, release_pos_z, player_name)
# Perform t-test
t_test_result <- t.test(a_release_pos_z $release_pos_z, b_release_pos_z$release_pos_z)
### Print the t-test result, there's significant difference for Buck Farmer's release_pos_z under A or B 
print(t_test_result) 
# spin_axis 
a_spin_axis <-df%>% 
  filter(str_detect(round_situation, 'A'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, spin_axis, player_name) 
b_spin_axis<-df%>% 
  filter(str_detect(round_situation, 'B'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, spin_axis, player_name) 
# Perform t-test 
t_test_result <- t.test(a_spin_axis$spin_axis, b_spin_axis$spin_axis) 
#Print the t-test result, there's no significant difference for Buck Farmer's spin_axis under A or B 
print(t_test_result) 
# release_extension
a_release_extension <-df%>% 
  filter(str_detect(round_situation, 'A'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, release_extension, player_name) 
b_release_extension<-df%>% 
  filter(str_detect(round_situation, 'B'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, release_extension, player_name) 
# Perform t-test 
t_test_result <- t.test(a_release_extension$release_extension, b_release_extension$release_extension) 
# Print the t-test result, there's no significant difference for Buck Farmer's release_extension under A or B 
print(t_test_result) 
# launch_speed 
a_launch_speed <-df%>% 
  filter(str_detect(round_situation, 'A'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, launch_speed, player_name) 
b_launch_speed<-df%>% 
  filter(str_detect(round_situation, 'B'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, launch_speed, player_name) 
# Perform t-test 
t_test_result <- t.test(a_launch_speed$launch_speed, b_launch_speed$launch_speed) 
# Print the t-test result, there's no significant difference for Buck Farmer's launch_speed under A or B 
print(t_test_result) 
###
p2 <- df%>% mutate(inZone = ifelse(zone < 11, 1, 0)) head(p2) 
# Command 
a_inZone <-p2%>% 
  filter(str_detect(round_situation, 'A'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, inZone, player_name) b_inZone<-p2%>%  
  filter(str_detect(round_situation, 'B'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, inZone, player_name) 
# Perform t-test 
t_test_result <- t.test(a_inZone$inZone, b_inZone$inZone) 
# Print the t-test result, there's no significant difference for Buck Farmer's inZone under A or B 
print(t_test_result)
# launch_speed_angle 
a_launch_speed_angle <-df%>% 
  filter(str_detect(round_situation, 'A'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, launch_speed_angle, player_name) 
b_launch_speed_angle <-df%>% 
  filter(str_detect(round_situation, 'B'), player_name == 'Farmer, Buck') %>% 
  select(round_situation, launch_speed_angle, player_name) 
# Perform t-test 
t_test_result <- t.test(a_launch_speed_angle$launch_speed_angle, b_launch_speed_angle$launch_speed_angle ) 
# Print the t-test result, there's no significant difference for Buck Farmer's launch_speed_angle under A or B 
print(t_test_result)