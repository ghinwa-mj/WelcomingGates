
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")

library("skimr")
library("tidyverse")
library("janitor")


UNHCR_COL_2020.2024.full <- read.csv("~/Dropbox/Datathon Anas Ideas/Append files/UNHCR_COL_2020-2024 full.csv")

head (UNHCR_COL_2020.2024.full ) #ver las primeras lineas de mis datos
str(UNHCR_COL_2020.2024.full ) #describe los datos
glimpse (UNHCR_COL_2020.2024.full ) #muestra mis datos
colnames (UNHCR_COL_2020.2024.full ) #me da el nombre de todos mis datos
skim_without_charts (UNHCR_COL_2020.2024.full ) #descriptivos estadísticos de mis datos

#If you wanted to create a new data frame that had those changes saved, 
#you would use the assignment operator, <- , as written in the code chunk 
#below to store the arranged data in a data frame named 'name_database_v2'
#add the data frame and the dollar sign in the appropriate places.

filter(UNHCR_COL_2020.2024.full, UNHCR_COL_2020.2024.full$job_change =="1")

UNHCR_COL_2020 <- 
  UNHCR_COL_2020.2024.full %>%
  group_by(yr) %>%
  summarise(mean_n_victims=mean(n_victims_violence_displ),
            mean_n_victims=min(n_victims_violence_displ),
            max_n_victims=max(n_victims_violence_displ))
head (UNHCR_COL_2020)

######visuals Julius


# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Read the Excel file
df <- read_excel("UNHCR_COL_REGIONAL_2023-2024.xls")

# Select relevant columns for types of violence in the country of asylum
violence_cols <- c('homicide_asylum', 'physicalAssault_asylum', 'sexualAssault_asylum',
                   'abductionORkidnapping_asylum', 'explotSex_asylum', 'exploitationwork_asylum',
                   'arrestORdetention_asylum', 'threat_asylum', 'fraud_asylum', 'theft_asylum',
                   'eviction_asylum', 'evictionthreat_asylum')

# Check if these columns exist in the dataframe
violence_cols <- violence_cols[violence_cols %in% colnames(df)]

# Summarize the total number of each type of violence
violence_summary <- colSums(df[violence_cols], na.rm = TRUE)

# Convert to a dataframe for plotting
violence_df <- data.frame(
  Type = names(violence_summary),
  Count = as.numeric(violence_summary)
)

# Print the summary table
print(violence_df)

# Create a mapping of variable names to short names
label_map <- c(
  homicide_asylum = "Homicide",
  physicalAssault_asylum = "Physical assault",
  sexualAssault_asylum = "Sexual assault",
  abductionORkidnapping_asylum = "Abduction/kidnapping",
  explotSex_asylum = "Sexual exploitation",
  exploitationwork_asylum = "Labor exploitation",
  arrestORdetention_asylum = "Arrest/detention",
  threat_asylum = "Threat",
  fraud_asylum = "Fraud",
  theft_asylum = "Theft",
  eviction_asylum = "Eviction",
  evictionthreat_asylum = "Eviction threat"
)

# Add the short names to the dataframe
violence_df_full_all$ShortName <- label_map[violence_df_full_all$Type]

# Show the updated table with short names
print(violence_df_full_all)

# Create a horizontal bar plot with sorted values
ggplot(violence_df_full_all, aes(x = reorder(ShortName, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # This makes it horizontal
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 10)
  ) +
  labs(
    title = "Types of Violence Suffered in the Country of Asylum",
    y = "Number of Cases",
    x = "Type of Violence"
  )

# Create a horizontal bar plot using percentages instead of frequencies
ggplot(df_percent, aes(x = reorder(ShortName, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 10)
  ) +
  labs(
    title = "Types of Violence Suffered in the Country of Asylum (Percentage)",
    y = "Percentage of Cases (%)",
    x = "Type of Violence"
  )
# Print the head to verify the data
print(head(df))

# Read the Excel file and check column names
library(readxl)
df <- read_excel('UNHCR_COL_REGIONAL_2023-2024.xls')
print(names(df))

# Filter for 2023 data and get child_noscho_nomoney by region
df_2023 <- df[df$yr == 2023, ]
child_noscho_by_region <- aggregate(child_noscho_nomoney ~ region, data = df_2023, FUN = mean)
child_noscho_sorted <- child_noscho_by_region[order(-child_noscho_by_region$child_noscho_nomoney), ]

# Create vertical bar chart
library(ggplot2)
ggplot(child_noscho_sorted, aes(x = reorder(region, -child_noscho_nomoney), y = child_noscho_nomoney)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Children Not in School Due to Lack of Money by Region (2023)",
    x = "Region",
    y = "Percentage (%)"
  )