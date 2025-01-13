# Name: Sweekruti Narendra Singh; Date: January 12, 2025; Class: ALY6015 20380 Intermediate Analytics SEC 11 Winter 2025 CPS [BOS-A-HY]

# Loading all the necessary libraries first
library(tidyverse)
library(RColorBrewer)
library(corrplot)
library(psych)
library(dplyr)
library(ggplot2)
library(gtools)
library(ggfortify)
library(GGally)
library(readr)
library(readxl)
library(knitr)
library(modelr)
library(scales)

# Step 1: Loading the dataset
df = read.csv("AmesHousing.csv")

# Step 2: Viewing the summary of the dataset and performing EDA
summary(df)

library(psych)
library(knitr)
library(kableExtra)

# Selecting only numeric variables from the dataset
numeric_data <- df %>% select(where(is.numeric))

# Calculating descriptive statistics
descriptive_stats <- describe(numeric_data)

# Formatting the descriptive statistics table
descriptive_stats <- descriptive_stats[, c("n", "mean", "sd", "min", "median", "max")]

# Adding row names as a new column to make it part of the table
descriptive_stats <- cbind(Variable = rownames(descriptive_stats), descriptive_stats)

# Resetting row names
rownames(descriptive_stats) <- NULL

# Presenting the descriptive statistics table using kableExtra
descriptive_stats %>%
  kbl(caption = "Descriptive Statistics of Ames Housing Dataset", align = "c") %>%
  kable_classic(full_width = FALSE, html_font = "Arial")

# Disabling scientific notation for better readability
options(scipen = 100)

# Creating a histogram to visualize the distribution of house prices 
ggplot(df, aes(x = SalePrice)) +
  geom_histogram(color = "black", fill = "deepskyblue", bins = 50) + 
  scale_x_continuous(labels = comma) +
  labs(title = "Distribution of House Prices", x = "Price", y = "Frequency") +
  theme_minimal()

# Creating a barplot to visualize the conditions of houses on the market
barplot(table(df$Overall.Cond), 
        main = "Conditions of Houses on the Market", 
        xlab = "Year",
        ylab = "Number of Houses",
        col = brewer.pal(10, "PiYG"))

# Creating a dotplot to visualize the median house prices based on the neighborhoods
neighborhoods = tapply(df$SalePrice, df$Neighborhood, median)
neighborhoods = sort(neighborhoods, decreasing = TRUE)

dotchart(neighborhoods, pch = 21, bg = "darkorchid4",
         cex = 0.85,
         xlab="Average Price of a House",
         main = "House Prices Per Neighborhood")

# Step 3: Calculating the number of missing values in each column of the dataset
na_count <-sapply(df, function(df) sum(length(which(is.na(df)))))
na_count

# Step 4: Creating a correlation matrix using only the numerical variables
numeric = df %>% select(where(is.numeric))

ggcorr(numeric)

# Step 5: Creating a new correlation matrix excluding the variables with weaker linear correlation
new_df = numeric %>% select(SalePrice, Open.Porch.SF, Wood.Deck.SF, Garage.Area, Garage.Cars, Garage.Yr.Blt, Fireplaces, TotRms.AbvGrd, Half.Bath, Full.Bath, Bsmt.Full.Bath, Gr.Liv.Area, X2nd.Flr.SF, X1st.Flr.SF, BsmtFin.SF.1, Mas.Vnr.Area, Year.Built, Year.Remod.Add, Overall.Qual, Lot.Area, Lot.Frontage, Enclosed.Porch, PID)

ggcorr(new_df, size = 3)

# Step 6: Determining the continuous variables with the highest, lowest and closest to 0.5 correlation with SalePrice
# Creating a scatterplot to visualize the continuous variable with the highest correlation with SalePrice
cor(df$SalePrice, df$Gr.Liv.Area)

ggplot(df, aes(SalePrice, Gr.Liv.Area)) + 
  geom_point(size = 2, color = "darkgoldenrod1", alpha = 0.6) + 
  theme_minimal() +
  geom_smooth(method = lm, color = "darkcyan", size = 2) + 
  scale_x_continuous(labels = comma)

plot(df$SalePrice, df$Gr.Liv.Area, 
     pch = 21,  cex=1.2,
     xlab = "Price",
     ylab = "Living Area (sq ft)",
     main = "Positive Linear Relationship Between the House Price and House Quality",
     bg = c(rev(heat.colors(10)))[unclass(df$Overall.Qual)])

# Creating a scatterplot to visualize the continuous variable with lowest correlation with SalePrice
cor(df$Enclosed.Porch, df$SalePrice)

plot(df$SalePrice, df$Enclosed.Porch, 
     pch = 21,  cex=1.2,
     xlab = "Price",
     ylab = "Porch Area (sq ft)",
     main = "Relationship Between the House Price and Porch Area",
     bg = "deeppink3")

# Creating a scatterplot to visualize the continuous variable with closest to 0.5 correlation with SalePrice
df1 = numeric %>% select(SalePrice, Garage.Area, X1st.Flr.SF, BsmtFin.SF.1, Mas.Vnr.Area, Lot.Frontage)

ggcorr(df1, size = 3, label = TRUE, label_size = 4, label_round = 2, label_alpha = TRUE)

plot(df$SalePrice, df$Mas.Vnr.Area, 
     pch = 21,  cex=1.2,
     xlab = "Price",
     ylab = "Masonry Veneer Area (sq ft)",
     main = "Relationship Between the House Price and Masonry Veneer Area",
     bg = c("darkcyan"))

# Step 7: Fitting a regression model
df2 = df %>% select(SalePrice, Garage.Area, X1st.Flr.SF, Gr.Liv.Area)

pairs(df2, 
      main = "Relations Between Continuous Variables in Ames Housing Dataset", 
      pch = 21, 
      bg = c("chartreuse"), 
      labels = c("Price","Garage Area","1st Floor Area","Living Area"),
      lower.panel = NULL, 
      font.labels = 2, 
      cex.labels = 2)

model1 = lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + X1st.Flr.SF + Garage.Area + Year.Built + Exter.Qual + Full.Bath, data = df)

summary(model1)

# Step 8: Reporting the model in equation form
# y = -706025.5 + 17818.86 * Overall.Qual + 51.32 * Gr.Liv.Area + 25.8 * X1st.Flr.SF + 42.64 * Garage.Area + 371.5 * Year.Built -79323.14 * Exter.QualFa -64967 * Exter.QualGd -77760.7 * Exter.QualTA -6753.74 * Full.Bath

# Step 9: Using the autoplot() function instead of plot() function to plot my regression model for better readability
autoplot(model1,
         which = 1:4,
         nrow = 4,
         ncol = 1)

# Step 10: Checking the model for multicollinearity
vif(model1)

# Step 11: Checking the model for outliers and removing the observations farthest from the others
library(olsrr)

ols_plot_resid_lev(model1)

houses = df[-c(2182, 2181, 1499, 1761, 1768),]

# Step 12: Attempting to correct issues discovered in the model like heteroscedasticity
model2 <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + X1st.Flr.SF + Garage.Area + Year.Built + Exter.Qual + Full.Bath, data = houses)

summary(model2)

library(nlme)

model3 <- gls(SalePrice ~ Overall.Qual + Gr.Liv.Area + X1st.Flr.SF + Garage.Area + Year.Built + Exter.Qual + Full.Bath,
              # my formula
              data = houses, 
              correlation = corAR1(0.25, form = ~ 1 | Order),
              na.action = na.exclude)

summary(model3)

a = c(AIC(model1), BIC(model1), AIC(model2), BIC(model2), AIC(model3), BIC(model3))

b = c("AIC lm", "BIC lm", "AIC lm2.0", "BIC lm2.0", "AIC gls", "BIC gls")

names(a) = b
print(a)

# Step 13: Using the all subsets regression method to identify the "best" model and creating a table of models showing variables in each model ordered by the selection statistic
library(leaps)

models <- regsubsets(SalePrice ~ Overall.Qual + Gr.Liv.Area + X1st.Flr.SF + Garage.Area + Year.Built + Exter.Qual + Full.Bath, data = houses, nvmax = 9)

plot(models ,scale = "adjr2")

res.sum <- summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

# Step 14: Reporting the "best" model in equation form
# y = -776584.3 + 16383.3 * Overall.Qual + 58.1 * Gr.Liv.Area + 33.7 * X1st.Flr.SF + 40.4 * Garage.Area + 410 * Year.Built - 87069.3 * Exter.QualiFa - 72862.9 * ExterQualGd - 85167.7 * ExterQualTA - 11347.6 * Full.Bath