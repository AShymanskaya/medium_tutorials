library(ggplot2)
library(readxl)

# Load the data
data <- read_excel('cheapest_countries_to_study_Europe_2023.xlsx')

# Calculate relative costs to Germany
germany_costs <- subset(data, Country == "Germany")$`Total yearly living costs and fees (£)`
data$`Relative to Germany (£)` <- data$`Total yearly living costs and fees (£)` - germany_costs

# Plot
ggplot(data, aes(x = reorder(Country, `Relative to Germany (£)`), y = `Relative to Germany (£)`, fill = `Relative to Germany (£)` > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("green", "red")) +
  labs(title = "Total Yearly Student Living Costs Relative to Germany", x = "Country", y = "Relative Costs (£)") +
  theme_minimal() +
  theme(legend.position = "none")

