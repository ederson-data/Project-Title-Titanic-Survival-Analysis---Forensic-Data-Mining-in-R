# Project-Title-Titanic-Survival-Analysis---Forensic-Data-Mining-in-R
📋 Overview

A comprehensive statistical analysis of the Titanic passenger manifest using R. This project applies Exploratory Data Analysis (EDA) and Logistic Regression to identify socioeconomic and demographic patterns in survival outcomes.

🛠️ Tech Stack

Language: R

Libraries: tidyverse (dplyr, ggplot2), stats (glm), reshape2

🔍 Key Features

ETL Pipeline: Handled missing values (Median Imputation for Age) and feature engineering (Cabin presence indicator).

Statistical Modeling: Developed a Logistic Regression model (glm) to predict survival probability.

Data Visualization: Created multifaceted visualizations including age-density plots, survival rate bar charts, and a predictive Confusion Matrix heatmap.

📈 Results

Achieved ~80% prediction accuracy using Pclass, Sex, and Age as features.

Statistically confirmed the "Women and Children First" protocol (p<0.001).

Visualized the "Social Ladder" effect, showing 1st Class passengers had a survival rate nearly 3x higher than 3rd Class.
