## installations and libraries required for file to run:
install.packages("plot3D")
install.packages("corrplot")
install.packages("leaflet")
install.packages("tidyverse")
install.packages("gpplot2")

library(ggplot2)
library(tidyverse)
library(plot3D)
library(corrplot)
library(leaflet)


##download data, replace quotation parts with file path
#PC
data <- read.csv("C:/Users/Madeline/OneDrive/Documents/.github/Data_Competition_2024/SL_DATA.csv")
#mac
#data <- read.csv("/Users/madelineshah/Documents/Clones/Data_Competition_2024/SL_DATA.csv")

#group by name
data_groups <- split(data, data$Name)

for (name in names(data_groups)) {
  # Replace spaces with underscores in the name
  df_name <- gsub(" ", "_", name)
  
  # Assign the group to a new data frame with the modified name
  assign(df_name, data_groups[[name]])
}

# create new column for m in y = mx + b (average increase of sea level)
All_Ms <- numeric(length(data_groups))

for(i in 1:length(data_groups))
{
  df <- data_groups[[i]]
  
  lm_model <- lm(SL_mm ~ Year, data = df)
  m <- coef(lm_model)[2]
  
  All_Ms[i] <- m
}

#rank each location by average increase in SL (high to lo)
Location_Groups <- data |> group_by(Name) |>
  summarise(
    Longitude = mean(Longitude),
    Latitude = mean(Latitude),
    Max_SL = max(SL_mm),
    Min_SL = min(SL_mm)
  )

Location_Groups$Average_SL_Increase <- All_Ms
Location_Groups <- Location_Groups |>
  arrange(desc(Average_SL_Increase))





#                                 INDIVIDUAL METRICS

#REPLACE dfName WITH LOCATION NAME
dfName = BRIDGEPORT

#calculate total increase
dfName <- dfName[order(dfName$Year), ]
difference <- tail(dfName$SL_mm, 1) - head(dfName$SL_mm, 1)
cat("From the year 1900 to the end of 2021, the sea level in", 
    dfName$Name[1], " changed by", difference, "mm.")

#calculate average "error" caused by inv barometrics
average_error <- mean(dfName$Inv_Bar_Contribution)
cat("The average amount of Sea Level change in milimeters caused by the “inverted barometer” effect
    for", dfName$Name[1], "is", average_error)


#correlation plot
# Subset the DataFrame to include only the required variables
subset_data <- BRIDGEPORT[c("Year", "SL_mm", "Inv_Bar_Contribution", "Inv_Bar_Effects")]

# Calculate the correlation matrix
correlation_matrix <- cor(subset_data)

# Load the 'corrplot' library if not already loaded
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}
library(corrplot)

# Create the correlation heatmap
corrplot(correlation_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         main = "Figure 3: Correlation Heatmap for BRIDGEPORT",
         mar = c(5, 5, 5, 5),  # Adjust top margin (5) for the title
         width = 8, height = 8) 


#plot sea level by year and provide linear regression line
lm_model <- lm(SL_mm ~ Year, data = dfName)
intercept <- coef(lm_model)[1]
slope <- coef(lm_model)[2]

equation <- paste("y =", round(intercept, 2), "+", round(slope, 2), "* x")


ggplot(data = dfName, aes(x = Year, y = SL_mm)) + 
  geom_line() + geom_smooth(method = "lm", se = TRUE, color = "blue") + 
  labs(title = paste(dfName$Name[1], "Sea Level (mm) By Year"), y = "Sea Level (mm)", 
       subtitle = "Linear regression equation displayed below plot", 
       caption = equation)


#long term projection
last_300 <- tail(dfName, 300)

future_years <- seq(max(dfName$Year) + 1, max(dfName$Year) + 10)
predicted_data <- data.frame(Year = future_years)
predicted_data$SL_mm <- predict(lm_model, newdata = predicted_data)

se <- summary(lm_model)$sigma

predicted_data$lwr <- predicted_data$SL_mm - 1.3 * se
predicted_data$upr <- predicted_data$SL_mm + 1.3 * se

ggplot() +
  geom_line(data = last_300, aes(x = Year, y = SL_mm, color = "Actual Data"), show.legend = TRUE) +
  geom_line(data = predicted_data, aes(x = Year, y = SL_mm, color = "Predicted Data"), show.legend = TRUE) +
  geom_ribbon(data = predicted_data,
              aes(x = Year, ymin = lwr, ymax = upr), fill = "grey", alpha = 0.3, show.legend = TRUE) +
  labs(title = paste(dfName$Name[1], "Predicted Future Sea Levels"),
       subtitle = "Predicted data error highlighted in grey",
       x = "Year",
       y = "SL_mm", 
       color = "Legend",
       fill = "Legend") +
  scale_color_manual(values = c("Actual Data" = "black", "Predicted Data" = "red"), labels = c("Actual Data", "Predicted Data")) +
  theme_minimal()


#                                 LOCATION-BASED METRICS


#3D plot of latitude, longitude, and average sl increase
#PC ONLY, comment out on mac to run entire script

scatter3D(Location_Groups$Latitude, Location_Groups$Longitude, Location_Groups$Average_SL_Increase,
          xlab = "Latitude", ylab = "Longitude", zlab = "Average Sea Level Increase",
          main = "Average Sea Level Increase by Location", ticktype="detailed",
          cex.axis = 0.5, cex.lab = 0.8, type = "p", pch = 19)



# Correlation Heatmap

correlation_matrix <- cor(Location_Groups[c("Average_SL_Increase", "Longitude", "Latitude")])
corrplot(correlation_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45)


#Map of average SL increase by Location

colorize <- colorNumeric(palette = "RdYlBu", 
                         domain = Location_Groups$Average_SL_Increase)

map <- leaflet(Location_Groups) %>%
  addTiles() %>% 
  addCircleMarkers(~Longitude, ~Latitude, 
                   color = ~colorize(Average_SL_Increase), 
                   radius = 5, stroke = FALSE, fillOpacity = 0.8) %>%
  addLegend("bottomright", 
            colors = rev(heat.colors(5)), 
            labels = round(seq(min(Location_Groups$Average_SL_Increase), 
                               max(Location_Groups$Average_SL_Increase), 
                               length.out = 5), 2),
            title = "Average SL Increase (mm)")

map









# only new york
NY_Data <- data[data$Latitude >= 39.5 & data$Latitude <= 50 & data$Longitude >= -75 & data$Longitude <= -73, ]

# Group by name
NY_Data_groups <- split(NY_Data, NY_Data$Name)

for (i in seq_along(NY_Data_groups)) {
  # Replace spaces with underscores in the name
  df_name <- paste0("df", i)
  
  # Assign the group to a new data frame with the modified name
  assign(df_name, NY_Data_groups[[i]])
}

# Create a new column for m in y = mx + b (average increase of sea level)
All_Ms <- numeric(length(NY_Data_groups))

for (i in 1:length(NY_Data_groups)) {
  df <- NY_Data_groups[[i]]
  
  lm_model <- lm(SL_mm ~ Year, data = df)
  m <- coef(lm_model)[2]
  
  All_Ms[i] <- m
}

# Rank each location by average increase in SL (high to low)
Location_Groups <- NY_Data |> group_by(Name) |> 
  summarise(
    Longitude = mean(Longitude),
    Latitude = mean(Latitude),
    Max_SL = max(SL_mm),
    Min_SL = min(SL_mm)
  )

Location_Groups$Average_SL_Increase <- All_Ms
Location_Groups <- Location_Groups |> arrange(desc(Average_SL_Increase))

colorize <- colorNumeric(palette = "RdYlBu", 
                         domain = Location_Groups$Average_SL_Increase)

map <- leaflet(Location_Groups) %>%
  addTiles() %>% 
  addCircleMarkers(~Longitude, ~Latitude, 
                   color = ~colorize(Average_SL_Increase), 
                   radius = 5, stroke = FALSE, fillOpacity = 0.8) %>%
  addLegend("bottomright", 
            colors = rev(heat.colors(5)), 
            labels = round(seq(min(Location_Groups$Average_SL_Increase), 
                               max(Location_Groups$Average_SL_Increase), 
                               length.out = 5), 2),
            title = "Average SL Increase (mm)")

map


# Write dataframe to a CSV file
write.csv(df6, "WilletsPoint.csv", row.names = FALSE)

getwd()


