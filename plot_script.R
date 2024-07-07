install.packages('ggplot2')
install.packages('plotly')
install.packages('htmlwidgets')
install.packages('dplyr')

library(ggplot2)
library(plotly)
library(htmlwidgets)
library(dplyr)

# Assuming covid_data and regions_df are already loaded

# Merge the COVID dataset with regions dataframe
merged_data <- merge(covid_data, regions_df, by = 'states')

# Summarize the data by region
summary_by_region <- merged_data %>% 
  group_by(region) %>%
  summarise(total_confirmed = sum(total_confirmed, na.rm = TRUE),
            new_confirmed = sum(new_confirmed, na.rm = TRUE),
            total_recoveries = sum(total_recoveries, na.rm = TRUE),
            new_recoveries = sum(new_recoveries, na.rm = TRUE),
            total_death = sum(total_death, na.rm = TRUE)
  )

summary_by_region2 <- merged_data %>%
  group_by(region) %>%
  summarise(avg_total_death = mean(total_death, na.rm = TRUE)
  )

# Find the mean and standard deviation of total deaths
mean_total_death <- mean(merged_data$total_death, na.rm = TRUE)
sd_total_death <- sd(merged_data$total_death, na.rm = TRUE)

# Comparing Each Region with other Regions
North_Central <- subset(merged_data, region == 'North Central')$total_death
South_South <- subset(merged_data, region == 'South South')$total_death
South_West <- subset(merged_data, region == 'South West')$total_death
South_East <- subset(merged_data, region == 'South East')$total_death
North_East <- subset(merged_data, region == 'North East')$total_death
North_West <- subset(merged_data, region == 'North West')$total_death

# Perform T-Tests
t_test_result1 <- t.test(North_Central, South_South, alternative = 'two.sided')
t_test_result2 <- t.test(North_Central, South_West, alternative = 'two.sided')
t_test_result3 <- t.test(North_Central, South_East, alternative = 'two.sided')
t_test_result4 <- t.test(North_Central, North_East, alternative = 'two.sided')
t_test_result5 <- t.test(North_Central, North_West, alternative = 'two.sided')
t_test_result6 <- t.test(North_West, South_South, alternative = 'two.sided')
t_test_result7 <- t.test(North_West, South_West, alternative = 'two.sided')
t_test_result8 <- t.test(North_West, South_East, alternative = 'two.sided')
t_test_result9 <- t.test(North_West, North_East, alternative = 'two.sided')
t_test_result10 <- t.test(North_East, South_South, alternative = 'two.sided')
t_test_result11 <- t.test(North_East, South_West, alternative = 'two.sided')
t_test_result12 <- t.test(North_East, South_East, alternative = 'two.sided')
t_test_result13 <- t.test(South_South, South_West, alternative = 'two.sided')
t_test_result14 <- t.test(South_South, South_East, alternative = 'two.sided')

# Print T-Test results
print(t_test_result1)
print(t_test_result2)
print(t_test_result3)
print(t_test_result4)
print(t_test_result5)
print(t_test_result6)
print(t_test_result7)
print(t_test_result8)
print(t_test_result9)
print(t_test_result10)
print(t_test_result11)
print(t_test_result12)
print(t_test_result13)
print(t_test_result14)

# Plot total deaths by region
ggplot(summary_by_region, aes(x = region, y = total_death, fill = region)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(title = 'Total Death by Region',
       x = 'Region',
       y = 'Total Death') +
  theme(axis.title.x = element_text(angle = 360, hjust = 0.5))

# Plot average total deaths by region
ggplot(summary_by_region2, aes(x = region, y = avg_total_death, fill = region)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(title = 'Average Total Deaths by Region',
       x = 'Region',
       y = 'Average Total Deaths') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Enhanced ggplot2 plot with annotations and interactivity
p <- ggplot(summary_by_region2, aes(x = region, y = avg_total_death, fill = region, text = paste('Region:', region, '<br>Avg Total Deaths:', avg_total_death))) +
  geom_bar(stat = 'identity', position = position_dodge(), color = 'black') +
  theme_minimal(base_size = 15) +
  labs(
    title = 'Average Total Deaths by Region',
    x = 'Region',
    y = 'Average Total Deaths',
    fill = 'Region'
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = 'bold', size = 20),
    legend.position = 'top'
  ) +
  scale_fill_brewer(palette = 'Set3') +
  geom_text(aes(label = avg_total_death), vjust = -0.3, size = 5)

# Convert the ggplot2 plot to an interactive plotly plot
interactive_plot <- ggplotly(p, tooltip = 'text')

# Display the interactive plot
interactive_plot

# Save the interactive plot as an HTML file
htmlwidgets::saveWidget(interactive_plot, 'interactive_plot.html')
