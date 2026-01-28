library(tidyverse)
library(ggplot2)
library(reshape2)

# Load data
df <- read_csv("titanic_data.csv")

head(df)

# Check the structure
str(df)

# Descriptive statistics for all variables
summary(df)


# Count NA values per column
colSums(is.na(df))

# Check for total duplicate rows
sum(duplicated(df))

# Fill missing Age with the median age
df_clean <- df %>%
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm =TRUE), Age))

# Verify Age is fixed
sum(is.na(df))

# Creating a feature and drop the previous one (cabin)
df_clean <- df %>%
  mutate(HasCabin = ifelse(is.na(Cabin) | Cabin == '', "No", "Yes")) %>%
  select(-Cabin)

# Proportional table of survival by Class
prop.table(table(df$Pclass, df_clean$Survived), margin = 1)

# Visualizing Age, Sex, and Survival
ggplot(df_clean, aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 5, position = "stack") +
  facet_wrap(~Sex) +
  theme_minimal() +
  labs(title = "Survival Distribution by Age and Gender")

# Survival Predictor
surv_model <- glm(Survived ~ Pclass + Sex + Age,
                  data = df,
                  family = "binomial")
summary(surv_model)

model_data <- df%>% filter(!is.na(Age) & !is.na(Survived))

# Predict the probability of survival for each person
model_data$prob <- prediction(surv_model, newdata = model_data, type = "response")

model_data$prob <- ifelse(model_data$prob > 0.5, 1, 0)

conf_matrix <- table(Actual = model_data$Survived, Predicted = model_data$prediciton)
print(conf_matrix)

# Convert the table to a data frame for ggplot
cm_df <- as.data.frame(conf_matrix)

ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "white", size = 8) +
  scale_fill_gradient(low = "#d1e5f0", high = "#2166ac") +
  labs(title = "Titanic Prediction Accuracy",
       subtitle = "Based on Pclass, Sex, and Age",
       x = "Predicted (0 = Died, 1 = Survived)",
       y = "Actual Outcome (0 = Died, 1 = Survived)") +
  theme_minimal()

