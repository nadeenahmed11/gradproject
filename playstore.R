library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

df <- read_csv("googleplaystore.csv")
head(df)
str(df)
summary(df)
df$Reviews <- as.numeric(df$Reviews)
df$`Last Updated` <- as.Date(df$`Last Updated`, format="%B %d, %Y")
sample_n(df, 1)
sum(is.na(df))
table(df$Price)
df$Price <- gsub("\\$", "", df$Price)
df$Price <- trimws(df$Price)
unique(df$Category)
unique(df$Genres)
df <- df %>% select(-Genres)
df$Category <- gsub("_", "", df$Category)
df$Category <- trimws(df$Category)
df$Category <- tools::toTitleCase(df$Category)

unique(df$App)

df$Rating[is.na(df$Rating)] <- 0
df$Reviews[is.na(df$Reviews)] <- mean(df$Reviews, na.rm = TRUE)
df$Type[is.na(df$Type)] <- 'Free'
df$`Content Rating`[is.na(df$`Content Rating`)] <- 'Everyone'
df$`Current Ver`[is.na(df$`Current Ver`)] <- 'Varies with device'
df$`Android Ver`[is.na(df$`Android Ver`)] <- '4.1 and up'

write_csv(df, "playstore.csv")
df1 <- read_csv("playstore.csv")

ggplot(df1, aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Distribution of App Ratings", x = "Rating", y = "Count")

ggplot(df1, aes(x = Category, y = Rating)) +
  stat_summary(fun = "mean", geom = "bar", fill = "pink") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average Rating by Category", x = "Category", y = "Average Rating")

ggplot(df1, aes(x = `Last Updated`)) +
  geom_histogram(binwidth = 30, fill = "purple", color = "black") +
  labs(title = "Frequency of App Updates Over Time", x = "Date", y = "Number of Apps Updated")
