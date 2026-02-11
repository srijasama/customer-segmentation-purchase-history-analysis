install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
library(dplyr)
library(tidyr)
library(ggplot2)
customer_data <-read.csv("C:\\Users\\samas\\Downloads\\customer_data.csv",stringsAsFactors = FALSE)
print(colnames(customer_data))
print(head(customer_data))
customer_data_cleaned <- customer_data %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%  # Replace blanks with NA for character columns only
  drop_na()  # Drop rows with NA values
if (nrow(customer_data_cleaned) == 0) {
  stop("Error: No valid rows found after cleaning. Please check the dataset.")
}

customer_data_cleaned <- customer_data_cleaned %>%
  mutate(segment = case_when(
    spending_score >= 0.7 ~ "High",
    spending_score >= 0.4 & spending_score < 0.7 ~ "Medium",
    TRUE ~ "Low"
  ))
write.csv(customer_data_cleaned, "cleaned_customer_data.csv", row.names = FALSE)
segment_distribution <- customer_data_cleaned %>%
  count(segment)
print(segment_distribution)
if (nrow(segment_distribution) == 0) {
  stop("Error: No data available for pie chart. Check the cleaned dataset.")
}
pie_chart <- ggplot(segment_distribution, aes(x = "", y = n, fill = segment)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Customer Segment Distribution", fill = "Segment") +
  theme_void()
ggsave("customer_segment_pie_chart.png", plot = pie_chart)
print(head(customer_data_cleaned$segment))
print(segment_distribution)
pie_chart <- ggplot(segment_distribution, aes(x = "", y = n, fill = segment)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Customer Segment Distribution", fill = "Segment") +
  theme_void()
print(pie_chart)
