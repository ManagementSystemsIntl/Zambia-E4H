source("scripts/r prep2.r")
source("scripts/r prep3.r")



# Sample data
df <- data.frame(
  value = c(10, 20, 30, 40),
  category = c("Category A", "Category B", "Category C", "Category D")
)

# Create pie chart
ggplot(df, aes(x = "", y = value, fill = category)) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart Example")
