# Calculate H/pitch, HR/pitch, WHIP/pitch
df <- read.csv('/Users/bryanttsai/Desktop/mlb/fangraphs_season_level.csv')
#1 Convert to per pitch
df$H_per_pitch = df$H / df$Pitches 
df$HR_per_pitch = df$HR / df$Pitches 
df$WHIP_per_pitch = df$WHIP / df$Pitches 
df$FIP_per_pitch = df$FIP / df$Pitches
df$Barrel_pct_pitch = df$Barrel_pct / df$Pitches

#2 Convert to per pitch_Shawn 
df$ER_per_pitch = df$ER / df$Pitches 
df$H_per_pitch = df$H / df$Pitches 
df$HR_per_pitch = df$HR / df$Pitches 
df$SO_per_pitch = df$SO / df$Pitches 
df$FIP_per_pitch = df$FIP / df$Pitches


# Impute NA values with the calculated median
df$Zone_pct[is.na(df$Zone_pct)] <- median_value
df$Zone_pct<- drop.is.na(df$Zone_pct)

# Impute NA values with the calculated median
median_value <- median(df$Barrel_pct_pitch, na.rm = TRUE)
df$Barrel_pct_pitch[is.na(df$Barrel_pct_pitch)] <- median_value

#Correlation of FIP_per_pitch
library(dplyr)
df2 = df %>%
  select(c('FIP_per_pitch','H_per_pitch', 'HR_per_pitch','WHIP_per_pitch', 'SO_per_pitch',
           'Zone_pct', 'Barrel_pct_pitch'
  ))
cor(df2)

# Run regression_Shawn
model_shawn = lm(FIP_per_pitch ~ H_per_pitch+ HR_per_pitch+ WHIP_per_pitch + SO_per_pitch+ Zone_pct, data = df2)
summary(model_shawn)

# Run regression_2
model_2 = lm(FIP_per_pitch ~ H_per_pitch+ HR_per_pitch+ WHIP_per_pitch + Barrel_pct_pitch, data = df2)
summary(model_2)
