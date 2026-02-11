# Install necessary packages
install.packages("arules")
install.packages("arulesViz")
install.packages("dplyr")
install.packages("tidyr")

# Load libraries
library(dplyr)
library(tidyr)
library(arules)       # For association rule mining
library(arulesViz)    # For visualizing association rules
library(stringr)      # For string manipulation

# Step 1: Load the CSV Data
data_path <- "C:/Users/samas/Downloads/customer_purchase_history_final (1).csv"
data <- read.csv(data_path, stringsAsFactors = FALSE)

# Step 2: Data Cleaning
# Flatten the data frame into a single vector
product_list <- unlist(data, use.names = FALSE)

# Clean the data: remove NAs, trim whitespace, and standardize
clean_products <- product_list %>%
  na.omit() %>%                            # Exclude NA values
  str_trim() %>%                           # Remove leading/trailing whitespace
  str_replace_all(",\\s*", "") %>%         # Eliminate commas and spaces after them
  str_replace_all("[^a-zA-Z0-9 ]", "") %>% # Strip special characters
  tolower() %>%                            # Convert to lowercase
  .[nchar(.) > 1]                          # Discard single-character entries

# Display a sample of cleaned data
head(clean_products)

# Step 3: Create Transactions
# Group the cleaned products into transactions (each transaction contains 10 items)
group_size <- 10
product_transactions <- split(clean_products, ceiling(seq_along(clean_products) / group_size))

# Convert the list to a 'transactions' object for analysis
trans_data <- as(product_transactions, "transactions")

# Step 4: Frequent Itemset Mining
# Discover frequent itemsets with a minimum support threshold of 1%
frequent_items <- apriori(trans_data, parameter = list(supp = 0.01, target = "frequent itemsets"))

# View the top 10 frequent itemsets sorted by support
inspect(sort(frequent_items, by = "support")[1:10])

# Step 5: Generate Association Rules
# Create rules with minimum support of 1% and confidence of 50%
assoc_rules <- apriori(trans_data, parameter = list(supp = 0.01, conf = 0.5, minlen = 2))

# Check if any rules were generated
if (length(assoc_rules) == 0) {
  stop("No association rules were generated. Please adjust support/confidence values.")
} else {
  # Inspect the top 10 association rules sorted by lift
  inspect(sort(assoc_rules, by = "lift")[1:10])
  
  # Step 6: Visualization (Bar Graph)
  # Generate a bar plot of the top 10 rules based on lift
  top_rules <- head(sort(assoc_rules, by = "lift"), 10)
  
  # Extract lift values and rule labels
  lift_values <- quality(top_rules)$lift
  rule_labels <- labels(top_rules)
  
  # Remove any NA or infinite lift values
  valid_rules <- lift_values[!is.na(lift_values) & is.finite(lift_values)]
  valid_labels <- rule_labels[!is.na(lift_values) & is.finite(lift_values)]
  
  # Create a barplot for the lift values
  if (length(valid_rules) > 0) {
    # Open a new plot window
    dev.new()
    
    # Create the bar plot
    barplot(valid_rules, 
            names.arg = valid_labels, 
            col = "skyblue", 
            las = 2,  # Rotate labels for better readability
            main = "Top 10 Association Rules by Lift", 
            ylab = "Lift", 
            cex.names = 0.7, 
            border = "blue")
  } else {
    stop("No valid lift values to plot.")
  }
}

