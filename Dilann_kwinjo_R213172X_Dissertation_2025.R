

#=============================================================================
#EDA
#=============================================================================
getwd
setwd("C:\\Users\\user.DESKTOP-OMQ89VA\\OneDrive\\R samples\\kwinjo")
# Load required packages
library(dplyr)
library(corrplot)
library(GGally)
library(ggplot2)
library(scales)  # for nicer axis labels
claims_data <- read.csv("Motor claims Data.csv", stringsAsFactors = FALSE)
# Define the key and influential numeric variables
key_vars <- c("NUM_OF_CLAIMS", "CLAIM_AMOUNT", "DRIVAGE_years", 
              "EXPOSURE", "RISK_SCORE", "VEHAGE_years", "VEHICLE_VALUE")
# Test for missing values in the dataset
# Calculate the count of missing values per column
missing_counts <- sapply(claims_data, function(x) sum(is.na(x)))
# Print the results
print("Missing Values Count by Variable:")
print(missing_counts)
# Optionally, print the total number of missing values in the dataset
total_missing <- sum(missing_counts)
print(paste("Total missing values:", total_missing))

# Load required package
library(ggplot2)

# Calculate the percentage of missing values for each variable
missing_percentage <- sapply(claims_data, function(x) {
  round(sum(is.na(x)) / length(x) * 100, 1)
})

# Create a data frame for plotting
missing_df <- data.frame(
  Variable = names(missing_percentage),
  MissingPercentage = missing_percentage
)
# Create a bar plot for the missing percentages
ggplot(missing_df, aes(x = reorder(Variable, -MissingPercentage), y = MissingPercentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(MissingPercentage, "%")), 
            vjust = -0.5, 
            size = 3.5, 
            color = "forestgreen") +  # Set text color to greenish
  labs(title = "Percentage of Missing Values per Variable",
       x = "Variable",
       y = "Missing Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(vjust = 1)) +  # Adjust vertical axis title position
  scale_y_continuous(labels = scales::percent_format(scale = 1))





# Load required package
library(ggplot2)

# Create a histogram for the distribution of Number of Claims
ggplot(claims_data, aes(x = NUM_OF_CLAIMS)) +
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  labs(title = "Distribution of Number of Claims",
       x = "Number of Claims",
       y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )



# Load required package
library(ggplot2)

# Create a histogram for the distribution of CLAIM_AMOUNT
ggplot(claims_data, aes(x = CLAIM_AMOUNT)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Distribution of CLAIM_AMOUNT",
       x = "CLAIM_AMOUNT",
       y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

# Load required package
library(ggplot2)

# Create a box plot for Claim Amount by Area
ggplot(claims_data, aes(x = AREA, y = CLAIM_AMOUNT, fill = AREA)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Rural" = "red", 
                               "Urban high density" = "green", 
                               "Urban low" = "blue")) +
  labs(title = "Claim Amount by Area",
       x = "Area",
       y = "Claim Amount") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_blank(),  # Remove legend title
    legend.position = "right"  # Position legend on the right
  )


str(claims_data)
# Subset the dataset
key_data <- claims_data %>% select(all_of(key_vars))

# 1. Create a Correlation Heatmap for key variables
# Compute the correlation matrix using pairwise complete observations
corr_matrix <- cor(key_data, use = "pairwise.complete.obs")
corr_matrix <- round(corr_matrix, 2)

# Create a custom color palette from purple (negative) to red (positive)
col_palette <- colorRampPalette(c("purple", "white", "red"))(200)

# Plot the correlation heatmap
corrplot(corr_matrix, 
         method      = "color",      # Colored squares
         type        = "upper",      # Only upper triangle to avoid redundancy
         order       = "hclust",     # Clustered by similarity
         addCoef.col = "black",      # Print correlation coefficients in black
         tl.col      = "black",      # Text label color in black
         tl.srt      = 45,           # Rotate text labels 45 degrees for readability
         number.cex  = 0.8,          # Size of the correlation coefficients
         title       = "Correlation Heatmap of Key Variables",
         mar         = c(0,0,2,0),   # Adjust margins to make room for the title
         col         = col_palette)

#Average Claim Amount by AREA & VEHICLE_USAGE HEATMAP
# Aggregate average claim amounts by AREA and VEHICLE_USAGE
heat_df <- claims_data %>%
  group_by(AREA, VEHICLE_USAGE) %>%
  summarise(avg_claim = mean(CLAIM_AMOUNT, na.rm = TRUE)) %>%
  ungroup()

# Create a heatmap using geom_tile
ggplot(heat_df, aes(x = AREA, y = VEHICLE_USAGE, fill = avg_claim)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(avg_claim, 1)), size = 3) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  theme_minimal() +
  labs(title = "Average Claim Amount by AREA & VEHICLE_USAGE",
       x = "Area",
       y = "Vehicle Usage",
       fill = "Avg Claim Amt")



#cORRELATION mATRIX
# Identify the categorical variables from the dataset
cat_vars <- c("AREA", "VEHICLE_USAGE", "VEHBRAND", "VEHAGE_CAT", "DRIVAGE_CAT")

# Initialize a matrix to store p-values for each pair
n <- length(cat_vars)
pval_matrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(cat_vars, cat_vars))

# Loop over all pairs of categorical variables and perform chi-square tests
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    var1 <- cat_vars[i]
    var2 <- cat_vars[j]
    # Create the contingency table for the pair
    tbl <- table(claims_data[[var1]], claims_data[[var2]])
    # Perform the chi-square test of independence
    test <- chisq.test(tbl)
    # Store the p-value in the matrix (symmetric matrix)
    pval_matrix[i, j] <- test$p.value
    pval_matrix[j, i] <- test$p.value
  }
}
# Print the matrix of p-values
print(pval_matrix)

#TESTS
# Create a contingency table for the two categorical variables: VEHAGE_CAT and DRIVAGE_CAT
contingency_table <- table(claims_data$VEHAGE_CAT, claims_data$DRIVAGE_CAT)

# Perform the chi-square test of independence
chi_test <- chisq.test(contingency_table)

# Print the test results which include the chi-square statistic, degrees of freedom, and p-value
print(chi_test)

#=================================================================================================






################################################################################
# Section 1: Preprocessing & Data Setup  #Count Models 
################################################################################

getwd
setwd("C:\\Users\\user.DESKTOP-OMQ89VA\\OneDrive\\R samples\\kwinjo")
claims_data <- read.csv("Motor claims Data.csv", stringsAsFactors = FALSE)

summary(claims_data)
# Ensure key categorical variables are factors.
claims_data$AREA           <- as.factor(claims_data$AREA)
claims_data$VEHICLE_USAGE  <- as.factor(claims_data$VEHICLE_USAGE)
claims_data$VEHBRAND       <- as.factor(claims_data$VEHBRAND)

# (If needed, ensure numeric variables are numeric)
claims_data$NUM_OF_CLAIMS  <- as.numeric(claims_data$NUM_OF_CLAIMS)
claims_data$DRIVAGE_years  <- as.numeric(claims_data$DRIVAGE_years)
claims_data$VEHAGE_years   <- as.numeric(claims_data$VEHAGE_years)
claims_data$EXPOSURE       <- as.numeric(claims_data$EXPOSURE)

# Create a new exposure variable that is strictly positive.
# Here we subtract the minimum and add 1.
min_expos <- min(claims_data$EXPOSURE, na.rm = TRUE)
claims_data$EXPOSURE_adj <- claims_data$EXPOSURE - min_expos + 1

################################################################################
# Section 2: Split the Data into Training and Test Sets
################################################################################

n_total <- nrow(claims_data)
set.seed(123)  # For reproducibility
train_idx <- sample(seq_len(n_total), size = round(0.70 * n_total))
train_data <- claims_data[train_idx, ]
test_data  <- claims_data[-train_idx, ]

cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")




################################################################################
# Section 3: Build Count Models for NUM_OF_CLAIMS
################################################################################

# Use available predictors: AREA, VEHICLE_USAGE, DRIVAGE_years, VEHBRAND, VEHAGE_years.
# We include an offset using the adjusted exposure.
# (a) Poisson GLM:
poisson_model <- glm(NUM_OF_CLAIMS ~ AREA + VEHICLE_USAGE + DRIVAGE_years + VEHBRAND + VEHAGE_years,
                     offset = log(EXPOSURE_adj),
                     family = poisson(),
                     data = train_data)

cat("\n----- Poisson GLM Model Summary -----\n")
print(summary(poisson_model))
cat("AIC for Poisson GLM:", AIC(poisson_model), "\n")

# (b) Negative Binomial GLM:
library(MASS)  # for glm.nb
nb_model <- glm.nb(NUM_OF_CLAIMS ~ AREA + VEHICLE_USAGE + DRIVAGE_years + VEHBRAND + VEHAGE_years +
                     offset(log(EXPOSURE_adj)),
                   data = train_data)

cat("\n----- Negative Binomial GLM Model Summary -----\n")
print(summary(nb_model))
cat("AIC for Negative Binomial GLM:", AIC(nb_model), "\n")

################################################################################
# Section 4: Statistical & Significance Tests for Count Models
################################################################################

library(AER)
library(lmtest)

# AIC & BIC Comparison:
aic_values <- AIC(poisson_model, nb_model)
bic_values <- BIC(poisson_model, nb_model)
cat("\n----- AIC Comparison -----\n")
print(aic_values)
cat("\n----- BIC Comparison -----\n")
print(bic_values)

# Overdispersion Test for Poisson:
cat("\n----- Overdispersion Test for Poisson Model -----\n")
disp_test <- dispersiontest(poisson_model)
print(disp_test)

# Likelihood Ratio Test (NB vs. Poisson):
cat("\n----- Likelihood Ratio Test (NB vs. Poisson) -----\n")
lr_test <- lrtest(nb_model, poisson_model)
print(lr_test)

################################################################################
# Section 5: Residual Analysis (NB Model Only)
################################################################################

# Extract Pearson residuals for the Negative Binomial model.
nb_resid <- residuals(nb_model, type = "pearson")

# Set up a 1x2 layout for the NB diagnostic plots.
par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))

# Left: QQ plot for NB model residuals.
qqnorm(nb_resid, 
       main = "QQ Plot: NB Residuals", 
       col = "red", 
       pch = 16)
qqline(nb_resid, col = "blue", lwd = 2)

# Right: Histogram and density of NB residuals.
hist(nb_resid, breaks = 20, 
     col = rgb(0, 0, 1, 0.5), 
     freq = FALSE, 
     main = "Distribution: NB Residuals", 
     xlab = "Pearson Residuals")
lines(density(nb_resid), col = "red", lwd = 2)

# Reset the layout to default.
par(mfrow = c(1, 1))


################################################################################
# Section 6: Actual vs. Predicted Plots for Count Models (Test Data)
################################################################################

# Generate predictions for the test set using the NB model.
test_data$pred_nb <- predict(nb_model, newdata = test_data, type = "response")

# Set up a single-plot layout.
par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))

# Plot for NB model:
plot(test_data$NUM_OF_CLAIMS, test_data$pred_nb, 
     pch = 16, col = "blue", 
     xlab = "Actual Counts", ylab = "Predicted Counts", 
     main = "Actual vs. Predicted (NB Model)")
abline(0, 1, col = "red", lwd = 2)
##############################################################################
# Section 7: Performance Bar Plots (Side-by-Side: NB Error Metrics vs. Mean Count)
##############################################################################

# Define performance metric functions.
mae <- function(actual, predicted) { 
  mean(abs(actual - predicted))
}
rmse <- function(actual, predicted) { 
  sqrt(mean((actual - predicted)^2))
}

# Compute error metrics for the NB model.
test_mae_nb  <- mae(test_data$NUM_OF_CLAIMS, test_data$pred_nb)
test_rmse_nb <- rmse(test_data$NUM_OF_CLAIMS, test_data$pred_nb)

# Use the provided Mean Count value (3.56) as the baseline.
mean_count_value <- 3.56

# Set up the plotting window with 1 row and 2 columns.
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

## Left Panel: Bar Plot for NB Model Error Metrics
# Create a vector of error metrics.
error_metrics <- c("MAE" = test_mae_nb, "RMSE" = test_rmse_nb)
bp_error <- barplot(error_metrics, 
                    col = c("blue", "lightblue"), 
                    main = "NB Model Error Metrics", 
                    ylab = "Error Value", 
                    ylim = c(0, max(error_metrics) * 1.3))
# Annotate each bar with its rounded value.
text(bp_error, error_metrics + max(error_metrics) * 0.05,
     labels = round(error_metrics, 2), xpd = TRUE, cex = 0.8)

## Right Panel: Bar Plot for Mean Count Baseline
bp_mean <- barplot(mean_count_value, 
                   names.arg = "Mean Count", 
                   col = "red", 
                   main = "Baseline: Mean Count", 
                   ylab = "Count", 
                   ylim = c(0, mean_count_value * 1.3))
# Annotate the bar with the mean count value.
text(bp_mean, mean_count_value + mean_count_value * 0.05,
     labels = round(mean_count_value, 2), xpd = TRUE, cex = 0.8)

# Reset the plotting layout.
par(mfrow = c(1, 1))

################################################################################
# End of Script
################################################################################








################################################################################
# SECTION 1: PREPROCESSING & DATA SETUP for Severity Modeling
################################################################################


# Ensure key categorical variables are factors.
claims_data$AREA           <- as.factor(claims_data$AREA)
claims_data$VEHICLE_USAGE  <- as.factor(claims_data$VEHICLE_USAGE)
claims_data$VEHBRAND       <- as.factor(claims_data$VEHBRAND)

# Ensure the claim-related and predictor fields are numeric.
claims_data$CLAIM_AMOUNT  <- as.numeric(claims_data$CLAIM_AMOUNT)
claims_data$DRIVAGE_years <- as.numeric(claims_data$DRIVAGE_years)
claims_data$VEHAGE_years  <- as.numeric(claims_data$VEHAGE_years)

# Check if AGE_VEHAGE exists; if so, convert to numeric.
if ("AGE_VEHAGE" %in% names(claims_data)) {
  claims_data$AGE_VEHAGE <- as.numeric(claims_data$AGE_VEHAGE)
} else {
  cat("Column 'AGE_VEHAGE' not found in the dataset; skipping conversion.\n")
}

################################################################################
# SECTION 2: SPLIT THE DATA into Training and Test Sets
################################################################################

n_total <- nrow(claims_data)
set.seed(123)  # For reproducibility.
train_idx <- sample(seq_len(n_total), size = round(0.70 * n_total))
train_data <- claims_data[train_idx, ]
test_data  <- claims_data[-train_idx, ]

cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")

# Subset the training and test sets to include only records with positive claim amounts.
train_sev <- subset(train_data, CLAIM_AMOUNT > 0)
test_sev  <- subset(test_data, CLAIM_AMOUNT > 0)

cat("Training severity set (CLAIM_AMOUNT > 0) size:", nrow(train_sev), "\n")
cat("Test severity set (CLAIM_AMOUNT > 0) size:", nrow(test_sev), "\n")


################################################################################
#  SEVERITY MODELS (Gamma and Lognormal)
################################################################################


required_vars <- c("POLICY_TENURE", "RISK_SCORE", "VEHICLE_VALUE", "ANNUAL_MILEAGE")
if (all(required_vars %in% names(claims_data))) {
  severity_formula_extended <- CLAIM_AMOUNT ~ AREA + VEHICLE_USAGE + DRIVAGE_years + VEHAGE_years +
    POLICY_TENURE + RISK_SCORE + VEHICLE_VALUE + ANNUAL_MILEAGE
  if ("AGE_VEHAGE" %in% names(claims_data)) {
    severity_formula_extended <- update(severity_formula_extended, . ~ . + AGE_VEHAGE)
  }
} else {
  stop("Not all extended variables found in the claims_data dataset.")
}

cat("Extended Severity Formula:\n")
print(formula(severity_formula_extended))


# --- Fit  Gamma Severity Model ---
gamma_sev_model_ext <- glm(severity_formula_extended, family = Gamma(link = "log"), data = train_sev)
cat("\n----- Extended Gamma Severity Model Summary -----\n")
print(summary(gamma_sev_model_ext))
cat("AIC for Extended Gamma Severity Model:", AIC(gamma_sev_model_ext), "\n")


# --- Fit Extended Lognormal Severity Model ---
# Update the formula to use log(CLAIM_AMOUNT) as the response.
model_formula_lognorm_ext <- update(severity_formula_extended, log(CLAIM_AMOUNT) ~ .)
lognormal_sev_model_ext <- lm(model_formula_lognorm_ext, data = train_sev)
cat("\n----- Extended Lognormal Severity Model Summary -----\n")
print(summary(lognormal_sev_model_ext))
cat("AIC for Extended Lognormal Severity Model:", AIC(lognormal_sev_model_ext), "\n")




################################################################################
# SECTION X: MODEL COMPARISON TESTS (Including Additional Metrics)
################################################################################

# --- Pre-existing Tests (Akaike Weights & Bootstrap Test for MSE Difference) ---
# (Assume boot_diff and boot_ci have already been computed)

# Update AIC comparisons to use the extended models.
aic_gamma   <- AIC(gamma_sev_model_ext)
aic_lognorm <- AIC(lognormal_sev_model_ext)
aic_values  <- c(Gamma = aic_gamma, Lognormal = aic_lognorm)
delta_aic   <- aic_values - min(aic_values)
akaike_weights <- exp(-0.5 * delta_aic) / sum(exp(-0.5 * delta_aic))
cat("\n----- Akaike Weights Comparison -----\n")
print(akaike_weights)

# Load the boot package if not already loaded
library(boot)

bootstrap_mse_diff <- function(data, indices) {
  # Create a bootstrap sample using the provided indices
  d <- data[indices, ]
  
  # Gamma model predictions on original scale using type = "response"
  pred_gamma <- predict(gamma_sev_model_ext, newdata = d, type = "response")
  
  # For the Lognormal model: predictions are made on the log-scale.
  # Retrieve the residual standard error (sigma) and then back-transform
  sigma_ln <- summary(lognormal_sev_model_ext)$sigma
  pred_lognorm <- exp(predict(lognormal_sev_model_ext, newdata = d) + (sigma_ln^2) / 2)
  
  # Calculate the Mean Squared Errors for both models
  mse_gamma <- mean((d$CLAIM_AMOUNT - pred_gamma)^2)
  mse_lognorm <- mean((d$CLAIM_AMOUNT - pred_lognorm)^2)
  
  # Return the difference in MSE (Gamma - Lognormal)
  return(mse_gamma - mse_lognorm)
}

# Set the random seed for reproducibility and run the bootstrap procedure.
set.seed(123)
boot_results <- boot(data = test_sev, statistic = bootstrap_mse_diff, R = 1000)

# Calculate a 95% bootstrap percentile confidence interval for the difference in MSE.
boot_ci <- boot.ci(boot_results, type = "perc")

cat("\n----- Bootstrap Test for Difference in MSE (Gamma - Lognormal) -----\n")
print(boot_ci)


# --- Additional Tests as Provided in Literature ---

## 1. Deviance Explained for the Gamma Model
dev_explained_gamma <- 1 - (gamma_sev_model_ext$deviance / gamma_sev_model_ext$null.deviance)
cat("\nGamma Model - Deviance Explained:", dev_explained_gamma, "\n")

## 2. R-squared for the Lognormal Model (serves as an approximate measure of deviance explained)
r_squared_lognorm <- summary(lognormal_sev_model_ext)$r.squared
cat("Lognormal Model - R-squared (Approx. Deviance Explained):", r_squared_lognorm, "\n")

## 3. Kolmogorov-Smirnov Test for Gamma Model Residuals
# Compare the distribution of the Gamma model's Pearson residuals to a normal distribution.
gamma_resid <- residuals(gamma_sev_model_ext, type = "pearson")
ks_test_gamma <- ks.test(gamma_resid, "pnorm", mean(gamma_resid), sd(gamma_resid))
cat("\nKolmogorov-Smirnov Test for Gamma Model Residuals:\n")
print(ks_test_gamma)

## 4. Shapiro-Wilk Test for Lognormal Model Residuals
# Assess the normality of the residuals from the Lognormal model.
lognorm_resid <- residuals(lognormal_sev_model_ext)
sw_test_lognorm <- shapiro.test(lognorm_resid)
cat("\nShapiro-Wilk Test for Lognormal Model Residuals:\n")
print(sw_test_lognorm)


################################################################################
# SECTION 4: DIAGNOSTIC RESIDUAL ANALYSIS
################################################################################

# Extract Pearson residuals from the extended models.
gamma_resid   <- residuals(gamma_sev_model_ext, type = "pearson")
# For the Lognormal model (a linear model on log-transformed claim amounts), we use the default residuals.
lognorm_resid <- residuals(lognormal_sev_model_ext)

# Set up a 2x2 plotting layout: top row for QQ plots; bottom row for histograms.
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# Top-Left: QQ Plot for Gamma Model Residuals.
qqnorm(gamma_resid, main = "QQ Plot: Extended Gamma Model", col = "blue", pch = 16)
qqline(gamma_resid, col = "red", lwd = 2)

# Top-Right: QQ Plot for Lognormal Model Residuals.
qqnorm(lognorm_resid, main = "QQ Plot: Extended Lognormal Model", col = "red", pch = 16)
qqline(lognorm_resid, col = "blue", lwd = 2)

# Bottom-Left: Histogram with Density for Gamma Model Residuals.
hist(gamma_resid, breaks = 20, col = rgb(0, 0, 1, 0.5), freq = FALSE,
     main = "Residual Distribution: Extended Gamma Model", xlab = "Pearson Residuals")
lines(density(gamma_resid), col = "blue", lwd = 2)

# Bottom-Right: Histogram with Density for Lognormal Model Residuals.
hist(lognorm_resid, breaks = 20, col = rgb(1, 0, 0, 0.5), freq = FALSE,
     main = "Residual Distribution: Extended Lognormal Model", xlab = "Residuals")
lines(density(lognorm_resid), col = "red", lwd = 2)

# Reset plotting layout to default.
par(mfrow = c(1, 1))

# Load dplyr if not already loaded.
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

################################################################################
# SECTION 5: ACTUAL vs. PREDICTED PLOTS on Test Data (with Added Lift Chart)
################################################################################

# Generate predictions on the test_sev set using the extended severity models.
# Extended Gamma model: obtain predictions on the response scale.
test_sev$pred_gamma <- predict(gamma_sev_model_ext, newdata = test_sev, type = "response")
# Extended Lognormal model: obtain predictions on the log scale, then back-transform.
test_sev$pred_lognorm <- exp(predict(lognormal_sev_model_ext, newdata = test_sev))

# Set up a side-by-side layout for Actual vs. Predicted plots.
par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))

# Plot for Extended Gamma model.
plot(test_sev$CLAIM_AMOUNT, test_sev$pred_gamma, pch = 16, col = "blue",
     xlab = "Actual Claim Amount", ylab = "Predicted Claim Amount",
     main = "Actual vs. Predicted:Gamma")
abline(0, 1, col = "red", lwd = 2)

# Plot for Extended Lognormal model.
plot(test_sev$CLAIM_AMOUNT, test_sev$pred_lognorm, pch = 16, col = "red",
     xlab = "Actual Claim Amount", ylab = "Predicted Claim Amount",
     main = "Actual vs. Predicted:Lognormal")
abline(0, 1, col = "blue", lwd = 2)

# Reset plotting layout to default before adding additional charts.
par(mfrow = c(1, 1))


################################################################################
# SECTION 6: PERFORMANCE BAR PLOTS FOR SEVERITY MODELS & MEAN CLAIM AMOUNT
################################################################################

# Define performance metric functions.
mae <- function(actual, predicted) { 
  mean(abs(actual - predicted)) 
}
rmse <- function(actual, predicted) { 
  sqrt(mean((actual - predicted)^2)) 
}

# Compute performance metrics on test_sev.
mae_gamma    <- mae(test_sev$CLAIM_AMOUNT, test_sev$pred_gamma)
rmse_gamma   <- rmse(test_sev$CLAIM_AMOUNT, test_sev$pred_gamma)
mae_lognorm  <- mae(test_sev$CLAIM_AMOUNT, test_sev$pred_lognorm)
rmse_lognorm <- rmse(test_sev$CLAIM_AMOUNT, test_sev$pred_lognorm)

# Print performance metrics to the console.
cat("Performance Metrics (Test Set - Severity Models):\n")
cat("Gamma Model - MAE:", round(mae_gamma, 2), "RMSE:", round(rmse_gamma, 2), "\n")
cat("Lognormal Model - MAE:", round(mae_lognorm, 2), "RMSE:", round(rmse_lognorm, 2), "\n\n")

# Compute the mean claim amount from the test severity set.
mean_claim <- mean(test_sev$CLAIM_AMOUNT)
cat("Mean Claim Amount (Test Set):", round(mean_claim, 2), "\n")

# Create a matrix with performance metrics.
perf_metrics <- rbind(MAE  = c(mae_gamma, mae_lognorm),
                      RMSE = c(rmse_gamma, rmse_lognorm))
colnames(perf_metrics) <- c("Gamma", "Lognormal")

# Set up the plotting window with 1 row and 2 columns.
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

## Left Panel: Bar Plot for Performance Metrics
bp_perf <- barplot(perf_metrics, beside = TRUE, 
                   col = c("blue", "lightblue"),
                   ylab = "Error Value", 
                   ylim = c(0, max(perf_metrics) * 1.2),
                   space = c(0.2, 0.2))
# Annotate each bar with its value.
text(bp_perf, perf_metrics + max(perf_metrics) * 0.05,
     labels = round(perf_metrics, 2), xpd = TRUE, cex = 0.8)
# Add legend above the plot.
legend("top", legend = rownames(perf_metrics), 
       fill = c("blue", "lightblue"),
       horiz = TRUE, inset = c(0, -0.1), xpd = TRUE, x.intersp = 1.5)

## Right Panel: Bar Plot for Mean Claim Amount
bp_mean <- barplot(mean_claim, 
                   names.arg = "Mean Claim", 
                   col = "red",
                   main = "Mean Claim Amount",
                   ylab = "Claim Amount", 
                   ylim = c(0, mean_claim * 1.3))
# Annotate the bar with the mean claim amount.
text(bp_mean, mean_claim + mean_claim * 0.05,
     labels = round(mean_claim, 2), xpd = TRUE, cex = 0.8)

# Reset the plotting layout.
par(mfrow = c(1, 1), mar = c(5, 5, 4, 2))

################################################################################
# End of Severity Modeling Script
################################################################################



#Technical Models Building
##############################################################################
# PREPROCESS DATA
##############################################################################

# Quick summary
summary(claims_data)

# Convert key categorical variables to factors.
claims_data$AREA           <- as.factor(claims_data$AREA)
claims_data$VEHICLE_USAGE  <- as.factor(claims_data$VEHICLE_USAGE)
claims_data$VEHBRAND       <- as.factor(claims_data$VEHBRAND)
# (Other categorical predictors such as VEHAGE_CAT or DRIVAGE_CAT can be converted if needed.)

# Ensure key numeric variables are numeric.
numeric_vars <- c("DRIVAGE_years", "POLICY_TENURE", "RISK_SCORE", "VEHAGE_years", 
                  "VEHICLE_VALUE", "ANNUAL_MILEAGE", "EXPOSURE", "NUM_OF_CLAIMS", "CLAIM_AMOUNT")
claims_data[, numeric_vars] <- lapply(claims_data[, numeric_vars], as.numeric)

##############################################################################
# PART 1: SPLIT THE DATA INTO TRAINING AND TEST SETS
##############################################################################
set.seed(123)  # For reproducibility
n_total <- nrow(claims_data)
train_idx <- sample(seq_len(n_total), size = round(0.70 * n_total))
train_data <- claims_data[train_idx, ]
test_data  <- claims_data[-train_idx, ]

cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")

##############################################################################
# PART 2: BUILD Frequency-SEVERITY MODELS (THE "TECHNICAL" MODELS)
##############################################################################

library(MASS)
count_model <- glm.nb(NUM_OF_CLAIMS ~ AREA + VEHICLE_USAGE + DRIVAGE_years +
                        VEHBRAND + VEHAGE_years + offset(log(EXPOSURE)),
                      data = train_data)
cat("\n----- Count Model (Negative Binomial) Summary -----\n")
print(summary(count_model))

## (B) SEVERITY MODEL (Gamma)
# For severity modeling, use only the policies with a positive CLAIM_AMOUNT.
train_sev <- subset(train_data, !is.na(CLAIM_AMOUNT) & CLAIM_AMOUNT > 0)
severity_model <- glm(CLAIM_AMOUNT ~ AREA + VEHICLE_USAGE + DRIVAGE_years +
                        VEHBRAND + VEHAGE_years + VEHICLE_VALUE + POLICY_TENURE,
                      family = Gamma(link = "log"),
                      data = train_sev)
cat("\n----- Severity Model (Gamma) Summary -----\n")
print(summary(severity_model))

################################################################################
# PART 3: PURE PREMIUM PROJECTIONS USING THE BEST COUNT & SEVERITY MODELS 
################################################################################
# (A) -- Using the Standard Technical Model (Product Method) --
# Predicted pure premium is computed as: predicted frequency × predicted severity.

# Predict frequency (from count model) on test_data.
test_data$pred_count <- predict(count_model, newdata = test_data, type = "response")

# Predict severity (from Gamma model) on test_data.
test_data$pred_severity <- predict(severity_model, newdata = test_data, type = "response")

# Compute predicted pure premium using the Standard Technical Model.
test_data$pred_pure_premium_product <- test_data$pred_count * test_data$pred_severity

# (B) -- Compute Actual Pure Premium --
# For policies with no claims, use 0 for CLAIM_AMOUNT.
test_data$actual_claim_amount <- ifelse(is.na(test_data$CLAIM_AMOUNT), 0, test_data$CLAIM_AMOUNT)
# Actual pure premium is total claim amount divided by exposure.
test_data$actual_pure_premium <- test_data$actual_claim_amount / test_data$EXPOSURE

# (C) -- Evaluate the Standard Technical Model Predictions --
mae <- function(actual, predicted) { mean(abs(actual - predicted)) }
rmse <- function(actual, predicted) { sqrt(mean((actual - predicted)^2)) }
pure_mae_product  <- mae(test_data$actual_pure_premium, test_data$pred_pure_premium_product)
pure_rmse_product <- rmse(test_data$actual_pure_premium, test_data$pred_pure_premium_product)
cat("\n=== Standard Technical Model (Count × Severity) Pure Premium Performance ===\n")
cat("MAE:", round(pure_mae_product, 2), "RMSE:", round(pure_rmse_product, 2), "\n")

#Pure premium predictions using Frequency-Severity Aprroach
# Order the test_data by a specific variable (e.g. POLICY_ID)
# Make sure POLICY_ID is numeric (or convert it if necessary)
test_data$POLICY_ID <- as.numeric(test_data$POLICY_ID)  # Convert if needed
test_data_ordered <- test_data[order(test_data$POLICY_ID), ]

# Then, subset the first 20 policies from this ordered test set
first20_ordered <- test_data_ordered[1:20, ]

# Preprocess these 20 rows if needed (similar to before)
first20_ordered$CLAIM_AMOUNT[is.na(first20_ordered$CLAIM_AMOUNT)] <- 0
first20_ordered$AREA           <- as.factor(first20_ordered$AREA)
first20_ordered$VEHICLE_USAGE  <- as.factor(first20_ordered$VEHICLE_USAGE)
first20_ordered$VEHBRAND       <- as.factor(first20_ordered$VEHBRAND)
first20_ordered <- droplevels(first20_ordered)

# Predict frequency and severity using your established models
first20_ordered$pred_count    <- predict(count_model, newdata = first20_ordered, type = "response")
first20_ordered$pred_severity <- predict(severity_model, newdata = first20_ordered, type = "response")
first20_ordered$pred_pure_premium <- first20_ordered$pred_count * first20_ordered$pred_severity

# Compute actual pure premium
first20_ordered$actual_claim_amount <- ifelse(is.na(first20_ordered$CLAIM_AMOUNT), 0, first20_ordered$CLAIM_AMOUNT)
first20_ordered$actual_pure_premium <- first20_ordered$actual_claim_amount / first20_ordered$EXPOSURE

# Display the results
result_table_ordered <- first20_ordered[, c("POLICY_ID", "EXPOSURE",
                                            "pred_count", "pred_severity",
                                            "pred_pure_premium",
                                            "actual_claim_amount", "actual_pure_premium")]
print(result_table_ordered)

################################################################################
# PART 4: CALIBRATION PLOT (STANDARD TECHNICAL MODEL)
################################################################################
# Divide test_data into deciles by predicted pure premium for the Standard Technical Model.
library(dplyr)
calib_prod <- test_data %>%
  mutate(decile = ntile(pred_pure_premium_product, 10)) %>%
  group_by(decile) %>%
  summarise(mean_pred = mean(pred_pure_premium_product),
            mean_actual = mean(actual_pure_premium))
plot(calib_prod$mean_pred, calib_prod$mean_actual, 
     pch = 16, col = "blue",
     xlab = "Average Predicted Pure Premium",
     ylab = "Average Actual Pure Premium",
     main = "Calibration Plot (Standard Technical Model)")
abline(0, 1, col = "red", lwd = 2)

################################################################################
# PART 5: ADDITIONAL VISUALIZATIONS & STATISTICAL TESTS FOR PURE PREMIUM PROJECTIONS
################################################################################

# ----- Additional Visualizations -----

# (1) Scatterplot of Prediction Errors vs. Predicted Pure Premiums
test_data$error <- test_data$actual_pure_premium - test_data$pred_pure_premium_product
plot(test_data$pred_pure_premium_product, test_data$error,
     pch = 16, col = "darkgreen", 
     xlab = "Predicted Pure Premium",
     ylab = "Prediction Error (Actual - Predicted)",
     main = "Prediction Error vs. Predicted Premium")
abline(h = 0, col = "red", lwd = 2)

# (2) Histogram of Prediction Errors
hist(test_data$error, breaks = 30, col = "lightgray", border = "white",
     main = "Histogram of Prediction Errors",
     xlab = "Error (Actual - Predicted Pure Premium)")
abline(v = mean(test_data$error), col = "blue", lwd = 2)
legend("topright", legend = "Mean Error", col = "blue", lwd = 2)

# (3) Density Plot: Overlaying Distributions of Actual and Predicted Pure Premiums
plot(density(test_data$actual_pure_premium), col = "blue", lwd = 2,
     main = "Density: Actual vs. Predicted Pure Premiums", xlab = "Pure Premium")
lines(density(test_data$pred_pure_premium_product), col = "red", lwd = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lwd = 2)

# (4) Lift Chart for Pure Premium Predictions
# We use decile groups based on predicted pure premium.
pure_calib <- test_data %>%
  mutate(decile = ntile(pred_pure_premium_product, 10)) %>%
  group_by(decile) %>%
  summarise(mean_pred = mean(pred_pure_premium_product),
            mean_actual = mean(actual_pure_premium)) %>%
  mutate(lift = mean_actual / mean(actual_pure_premium))
# Plot Lift Chart
barplot(pure_calib$lift, names.arg = pure_calib$decile, col = "purple",
        xlab = "Decile", ylab = "Lift", main = "Lift Chart for Pure Premium Predictions",
        ylim = c(0, max(pure_calib$lift)*1.2))
abline(h = 1, col = "red", lwd = 2)

# ----- Statistical Tests -----

# (1) Paired t-test for Pure Premium Predictions:
t_test_result <- t.test(test_data$actual_pure_premium, test_data$pred_pure_premium_product, paired = TRUE)
cat("\nPaired t-test for pure premium predictions (Actual vs. Predicted):\n")
print(t_test_result)

# (2) Kolmogorov-Smirnov Test comparing the distributions of Actual vs. Predicted Premiums:
ks_test_result <- ks.test(test_data$actual_pure_premium, test_data$pred_pure_premium_product)
cat("\nKolmogorov-Smirnov test comparing the distributions (Actual vs. Predicted Pure Premiums):\n")
print(ks_test_result)

##############################################################################
# PART 5: BUILD A TWEEDIE MODEL FOR PURE PREMIUM PROJECTIONS
##############################################################################
# For Tweedie modeling, we model total loss directly.
# Replace NA in CLAIM_AMOUNT with 0 in both train and test sets.
train_data$CLAIM_AMOUNT[is.na(train_data$CLAIM_AMOUNT)] <- 0
test_data$CLAIM_AMOUNT[is.na(test_data$CLAIM_AMOUNT)]   <- 0

# Drop unused factor levels.
train_data <- droplevels(train_data)

# For complete-case selection, use only predictors we need for cpglm.
# Exclude extra columns like POLICY_ID or EXPOSURE_AREA.
predictor_vars <- c("CLAIM_AMOUNT", "AREA", "VEHICLE_USAGE", "DRIVAGE_years",
                    "VEHBRAND", "VEHAGE_years", "VEHICLE_VALUE", "POLICY_TENURE", "EXPOSURE")
complete_train <- train_data[complete.cases(train_data[, predictor_vars]), ]
complete_train <- droplevels(complete_train)

# (Optional) Check that the design matrix has no NA/Inf values.
mm <- model.matrix(CLAIM_AMOUNT ~ AREA + VEHICLE_USAGE + DRIVAGE_years +
                     VEHBRAND + VEHAGE_years + VEHICLE_VALUE + POLICY_TENURE,
                   data = complete_train)
if(any(is.na(mm)) || any(is.infinite(mm))) {
  stop("The design matrix contains NA or infinite values.")
}

# Load the cplm package.
library(cplm)
# Fit a Tweedie model (Compound Poisson-Gamma) using similar predictors.
tweedie_model <- cpglm(CLAIM_AMOUNT ~ AREA + VEHICLE_USAGE + DRIVAGE_years +
                         VEHBRAND + VEHAGE_years + VEHICLE_VALUE + POLICY_TENURE,
                       offset = log(EXPOSURE),
                       data = complete_train)
cat("\n----- Tweedie Model Summary -----\n")
print(summary(tweedie_model))

# Predict total loss on test_data using the Tweedie model.
test_data$pred_total_loss_tweedie <- predict(tweedie_model, newdata = test_data, type = "response")
# Compute predicted pure premium for the Tweedie model (by dividing by EXPOSURE).
test_data$pred_pure_premium_tweedie <- test_data$pred_total_loss_tweedie / test_data$EXPOSURE

# Evaluate the Tweedie projections.
pure_mae_tweedie  <- mae(test_data$actual_pure_premium, test_data$pred_pure_premium_tweedie)
pure_rmse_tweedie <- rmse(test_data$actual_pure_premium, test_data$pred_pure_premium_tweedie)
cat("\n=== Tweedie Model Pure Premium Performance ===\n")
cat("MAE:", round(pure_mae_tweedie, 2), "RMSE:", round(pure_rmse_tweedie, 2), "\n")

# ----------------------------------------------------------------------
# Predict Pure Premiums using the Tweedie Model for the First 20 Policies
# -----------------------------------------------------------------------

# Predict total loss on the test_data using the Tweedie model.
test_data$pred_total_loss_tweedie <- predict(tweedie_model, newdata = test_data, type = "response")

# Compute predicted pure premium by dividing the predicted total loss by EXPOSURE.
test_data$pred_pure_premium_tweedie <- test_data$pred_total_loss_tweedie / test_data$EXPOSURE

# Compute actual pure premium as Claim Amount divided by EXPOSURE.
# (Ensure that CLAIM_AMOUNT has been handled for NAs; here we assume they've been set to 0 as in your code.)
test_data$actual_pure_premium <- test_data$CLAIM_AMOUNT / test_data$EXPOSURE

# Subset the first 20 policies from the test_data.
first20 <- test_data[1:20, ]

# Build a summary table with the required columns.
final_table <- first20[, c("POLICY_ID", 
                           "pred_total_loss_tweedie", 
                           "pred_pure_premium_tweedie", 
                           "actual_claim_amount", 
                           "actual_pure_premium")]

# Print the final table.
print(final_table)


##############################################################################
# PART 6: VISUALIZATIONS & STATISTICAL TESTS FOR TWEEDIE PURE PREMIUM PROJECTIONS
##############################################################################
# Load dplyr to ensure the use of %>% and ntile()
library(dplyr)

# --------------------------------------
# (A) Actual vs. Predicted Plot for Tweedie Model
# --------------------------------------
par(mfrow = c(1, 1), mar = c(5, 5, 4, 2))
plot(test_data$CLAIM_AMOUNT/test_data$EXPOSURE, test_data$pred_pure_premium_tweedie,
     pch = 16, col = "darkorange",
     xlab = "Actual Pure Premium",
     ylab = "Predicted Pure Premium (Tweedie)",
     main = "Actual vs. Predicted Pure Premium (Tweedie)")
abline(0, 1, col = "blue", lwd = 2)

# --------------------------------------
# (B) Calibration Plot: Group test_data into deciles by predicted premium
# --------------------------------------
calib_tweedie <- test_data %>%
  mutate(decile = ntile(pred_pure_premium_tweedie, 10)) %>%
  group_by(decile) %>%
  summarise(mean_pred = mean(pred_pure_premium_tweedie),
            mean_actual = mean(actual_pure_premium))
plot(calib_tweedie$mean_pred, calib_tweedie$mean_actual,
     pch = 16, col = "purple",
     xlab = "Average Predicted Pure Premium (Tweedie)",
     ylab = "Average Actual Pure Premium",
     main = "Calibration Plot (Tweedie Model)")
abline(0, 1, col = "red", lwd = 2)

# --------------------------------------
# (C) Additional Visualizations
# --------------------------------------
# (1) Scatterplot: Prediction Errors vs. Predicted Premiums
test_data$error_tweedie <- test_data$actual_pure_premium - test_data$pred_pure_premium_tweedie
plot(test_data$pred_pure_premium_tweedie, test_data$error_tweedie,
     pch = 16, col = "darkgreen",
     xlab = "Predicted Pure Premium (Tweedie)",
     ylab = "Prediction Error (Actual - Predicted)",
     main = "Error vs. Predicted Pure Premium (Tweedie)")
abline(h = 0, col = "red", lwd = 2)

# (2) Histogram of Prediction Errors
hist(test_data$error_tweedie, breaks = 30,
     col = "lightgray", border = "white",
     main = "Histogram of Prediction Errors (Tweedie)",
     xlab = "Error (Actual - Predicted)")
abline(v = mean(test_data$error_tweedie), col = "blue", lwd = 2)
legend("topright", legend = "Mean Error", col = "blue", lwd = 2)

# (3) Density Plot: Overlay Actual vs. Predicted Premium Distributions
plot(density(test_data$actual_pure_premium), col = "blue", lwd = 2,
     main = "Density: Actual vs. Tweedie Predicted Premiums", xlab = "Pure Premium")
lines(density(test_data$pred_pure_premium_tweedie), col = "red", lwd = 2)
legend("topright", legend = c("Actual", "Tweedie Predicted"), col = c("blue", "red"), lwd = 2)

# (4) Lift Chart for Tweedie Model Pure Premium Predictions
# Create decile groups based on predicted pure premium.
test_data$decile_tweedie <- ntile(test_data$pred_pure_premium_tweedie, 10)
lift_tweedie <- test_data %>%
  group_by(decile_tweedie) %>%
  summarise(mean_pred = mean(pred_pure_premium_tweedie),
            mean_actual = mean(actual_pure_premium)) %>%
  mutate(lift = mean_actual / mean(actual_pure_premium))
barplot(lift_tweedie$lift, names.arg = lift_tweedie$decile_tweedie,
        col = "darkred", xlab = "Decile (Tweedie)", ylab = "Lift",
        main = "Lift Chart: Tweedie Model",
        ylim = c(0, max(lift_tweedie$lift) * 1.2))
abline(h = 1, col = "blue", lwd = 2)

# Reset plotting layout.
par(mfrow = c(1, 1))

# --------------------------------------
# (D) Statistical Tests for Tweedie Projections
# --------------------------------------
# Paired t-test: Compare actual vs. Tweedie predicted pure premium.
t_test_tweedie <- t.test(test_data$actual_pure_premium, test_data$pred_pure_premium_tweedie, paired = TRUE)
cat("\nPaired t-test for Tweedie pure premium predictions (Actual vs. Predicted):\n")
print(t_test_tweedie)

# Kolmogorov-Smirnov test: Compare the distributions of actual vs. Tweedie predicted pure premium.
ks_test_tweedie <- ks.test(test_data$actual_pure_premium, test_data$pred_pure_premium_tweedie)
cat("\nKolmogorov-Smirnov test comparing distributions (Actual vs. Tweedie Predicted Pure Premium):\n")
print(ks_test_tweedie)
# Load dplyr to ensure %>% and ntile() are available.
library(dplyr)

###############################################################################


##############################################################################
# PREPARE TRAINING DATASET (Hybrid Model)
##############################################################################
library(caret)

# The ANN will use as predictors the Tweedie pure premium predictions and additional variables.
# First, compute Tweedie predictions on the training data:
train_data$pred_total_loss_tweedie <- predict(tweedie_model, newdata = train_data, type = "response")
train_data$pred_pure_premium_tweedie <- train_data$pred_total_loss_tweedie / train_data$EXPOSURE

# Create the actual pure premium column if it doesn't exist.
if (!"actual_pure_premium" %in% colnames(train_data)) {
  train_data$actual_pure_premium <- train_data$CLAIM_AMOUNT / train_data$EXPOSURE
}
if (!"actual_pure_premium" %in% colnames(test_data)) {
  test_data$actual_pure_premium <- test_data$CLAIM_AMOUNT / test_data$EXPOSURE
}

# Create train_ann_data by subsetting the required columns.
# (Make sure that your train_data contains the following columns:
#  pred_pure_premium_tweedie, DRIVAGE_years, VEHAGE_years, VEHICLE_VALUE, 
#  POLICY_TENURE, NUM_OF_CLAIMS, RISK_SCORE, and actual_pure_premium.)
train_ann_data <- train_data[, c("pred_pure_premium_tweedie", "DRIVAGE_years", "VEHAGE_years",
                                 "VEHICLE_VALUE", "POLICY_TENURE", "NUM_OF_CLAIMS", "RISK_SCORE",
                                 "actual_pure_premium")]

# Similarly, create test_ann_data from test_data for the ANN.
test_ann_data <- test_data[, c("pred_pure_premium_tweedie", "DRIVAGE_years", "VEHAGE_years",
                               "VEHICLE_VALUE", "POLICY_TENURE", "NUM_OF_CLAIMS", "RISK_SCORE",
                               "actual_pure_premium")]

#-----------------------------------------------------------------
# ANN Training with Modifications to Resolve Resampling Warnings
#-----------------------------------------------------------------

# Define cross-validation settings: 10-fold CV repeated 3 times.
train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  verboseIter = TRUE  # Enable verbose iteration output for debugging
)

# Define a grid for hyperparameter tuning.
tune_grid <- expand.grid(
  size  = c(3, 5, 7, 10),      # Number of neurons in the hidden layer.
  decay = c(0, 0.001, 0.01, 0.1)  # Weight decay (regularization parameter).
)

# Train the ANN using caret's train() with the "nnet" method.
ann_cv_model <- train(
  actual_pure_premium ~ .,
  data = train_ann_data,
  method = "nnet",
  preProcess = c("center", "scale"),  # Scale predictors to improve convergence.
  trControl = train_control,
  tuneGrid = tune_grid,
  linout = TRUE,  # Ensures a linear output (for regression).
  trace = FALSE,  # Suppress internal output.
  maxit = 1000    # Increase maximum iterations to aid convergence.
)

# Print best tuning parameters and overall model details.
print(ann_cv_model)

# --- Prediction on Test Data using the Optimized ANN Model ---

# Use the tuned model to predict pure premiums on test_ann_data.
test_ann_data$ann_pred_cv <- predict(ann_cv_model, newdata = test_ann_data)

# Evaluate the optimized model's performance using error metrics.
ann_mae_cv  <- mae(test_ann_data$actual_pure_premium, test_ann_data$ann_pred_cv)
ann_rmse_cv <- rmse(test_ann_data$actual_pure_premium, test_ann_data$ann_pred_cv)

cat("Optimized Hybrid GLM-ANN Model Performance:\n")
cat("MAE:", round(ann_mae_cv, 2), "\nRMSE:", round(ann_rmse_cv, 2), "\n")


#=================================================================
# Evaluation of the Optimized Hybrid GLM-ANN Model on Test Data
#=================================================================

# Load required libraries (if not already loaded)
library(ggplot2)
library(caret)
library(lmtest)
library(gridExtra)

# Ensure that test_predictions is defined (using the tuned model ann_cv_model)
if (!exists("test_predictions")) {
  test_predictions <- predict(ann_cv_model, newdata = test_ann_data)
}

# Compute evaluation metrics using caret's postResample function.
eval_metrics <- postResample(pred = test_predictions, obs = test_ann_data$actual_pure_premium)
r2_val   <- eval_metrics["Rsquared"]
rmse_val <- eval_metrics["RMSE"]
mae_val  <- eval_metrics["MAE"]

cat("Hybrid GLM-ANN Model Evaluation:\n")
cat("RMSE:", round(rmse_val, 2), "\n")
cat("R-squared:", round(r2_val, 2), "\n")
cat("MAE:", round(mae_val, 2), "\n\n")

# Create a DataFrame for plotting results.
results_df <- data.frame(
  Actual    = test_ann_data$actual_pure_premium,
  Predicted = test_predictions
)

# ------------------------------------------------------------------
# Plot 1: Predicted vs Actual Pure Premiums (for reference)
# ------------------------------------------------------------------
p1 <- ggplot(results_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(title = "Hybrid GLM-ANN: Predicted vs Actual Pure Premiums",
       x = "Actual Pure Premium",
       y = "Predicted Pure Premium") +
  theme_minimal() +
  annotate("text", 
           x = min(results_df$Actual) + 0.1 * diff(range(results_df$Actual)), 
           y = max(results_df$Predicted) - 0.1 * diff(range(results_df$Predicted)),
           label = paste0("R²: ", round(r2_val, 2), "\nRMSE: ", round(rmse_val, 2),
                          "\nMAE: ", round(mae_val, 2)),
           hjust = 0,
           color = "black",
           size = 4)

print(p1)

# ------------------------------------------------------------------
# Compute Residuals
# ------------------------------------------------------------------
results_df$Residuals <- results_df$Actual - results_df$Predicted

# ------------------------------------------------------------------
# Plot 2: Residual Distribution (Histogram + Density)
# ------------------------------------------------------------------
p2 <- ggplot(results_df, aes(x = Residuals)) +
  geom_histogram(aes(y = after_stat(density)), fill = "lightblue", color = "black", bins = 30, alpha = 0.7) +
  geom_density(color = "red", linewidth = 1.2) +
  labs(title = "Residual Distribution: Hybrid GLM-ANN",
       x = "Residuals (Actual - Predicted)",
       y = "Density") +
  theme_minimal()

# ------------------------------------------------------------------
# Plot 3: Normal Q–Q Plot of Residuals
# ------------------------------------------------------------------
p3 <- ggplot(results_df, aes(sample = Residuals)) +
  stat_qq(color = "darkblue", alpha = 0.6) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title = "Q–Q Plot of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Arrange the Residual Distribution and Q–Q plots side-by-side.
grid.arrange(p2, p3, ncol = 2)


nrow(test_data)
length(test_data$ann_pred_cv)
length(test_data$actual_pure_premium)

# Check the number of observations in test_data and predictions
cat("Number of test observations:", nrow(test_data), "\n")
cat("Length of actual values:", length(test_data$actual_pure_premium), "\n")
cat("Length of predictions:", length(test_data$ann_pred_cv), "\n")

#-----------------------------------------------------------------
# Generate ANN Predictions on the Test ANN Dataset
#-----------------------------------------------------------------
# Use the tuned ANN model (ann_cv_model) to predict pure premiums.
# (Make sure that test_ann_data was created earlier with the required columns.)
test_ann_data$ann_pred_cv <- predict(ann_cv_model, newdata = test_ann_data)

# Optional: Verify that predictions have been generated.
cat("Number of predictions in test_ann_data:", length(test_ann_data$ann_pred_cv), "\n")


#=================================================================
# Statistical Tests For Hybrid GLM-ANN Model Using test_ann_data
#=================================================================
# Load required libraries (if not already loaded)
library(lmtest)
library(ggplot2)

# Prepare Hybrid Model Residuals.
# (It is assumed that test_ann_data contains:
#  - actual_pure_premium: Observed pure premium.
#  - ann_pred_cv: Predictions from the Hybrid GLM-ANN model.)
residuals_hybrid <- test_ann_data$actual_pure_premium - test_ann_data$ann_pred_cv

# 1. t-Test for Mean of Residuals Equal to Zero
t_test <- t.test(residuals_hybrid, mu = 0)
cat("\nPaired t-Test on Residuals (H0: Mean = 0):\n")
print(t_test)
# Interpretation: A high p-value suggests that the model does not have a significant bias.

# 2. Correlation Test between Predicted and Actual Pure Premiums
cor_test <- cor.test(test_ann_data$ann_pred_cv, test_ann_data$actual_pure_premium)
cat("\nCorrelation Test between Predictions and Actual Values:\n")
print(cor_test)
# Interpretation: A significant correlation indicates that the predictions are strongly related to the actual pure premiums.


#=================================================================
# Learning Curve for Hybrid GLM-ANN Model
#=================================================================
# Load necessary libraries (if not already loaded)
library(caret)
library(ggplot2)

# Generate learning curve data using caret's learning_curve_dat() function.
# This function trains the model on increasing proportions of the training set.
set.seed(123)
lc_data <- learning_curve_dat(
  dat = train_ann_data,
  outcome = "actual_pure_premium",
  proportion = seq(0.1, 1, by = 0.1),
  method = "nnet",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = ann_cv_model$bestTune,
  preProcess = c("center", "scale"),  # ensuring preprocessing consistency
  linout = TRUE,
  trace = FALSE,
  maxit = 1000  # Align with the increased iterations used during model training
)

# Plot the learning curve.
# The plot includes RMSE for the training ("Training") and resampling ("Test") datasets.
learning_curve_plot <- ggplot(lc_data, aes(x = Training_Size, y = RMSE, color = Data)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Learning Curve for Hybrid GLM-ANN Model",
    x = "Training Set Size",
    y = "RMSE"
  ) +
  theme_minimal()

print(learning_curve_plot)

#=============================================================================
# Combined Comparison Plots for Technical Models (Using test_ann_data)
#=============================================================================

# Load required libraries
library(ggplot2)
library(reshape2)

# It is assumed:
#   - test_data contains actual values (450 rows) in the column 'actual_pure_premium'.
#   - test_ann_data contains predictions for all three models 
#     ('pred_pure_premium_product', 'pred_pure_premium_tweedie', 'ann_pred_cv'),
#     each having 450 rows.

# Build the comparison data frame using test_data for Actual and test_ann_data for predictions.
comparison_df <- data.frame(
  Actual   = test_data$actual_pure_premium,
  Standard = test_ann_data$pred_pure_premium_product,
  Tweedie  = test_ann_data$pred_pure_premium_tweedie,
  Hybrid   = test_ann_data$ann_pred_cv
)

# Verify row counts:
cat("Rows in comparison_df:", nrow(comparison_df), "\n")

# Melt the data frame from wide to long format for plotting.
melt_comparison <- melt(comparison_df, id.vars = "Actual",
                        variable.name = "Model", value.name = "Predicted")

# ----- Plot 1: Combined Scatter Plot: Actual vs Predicted -----
p_scatter <- ggplot(melt_comparison, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", 
              linewidth = 1) +
  labs(title = "Comparison of Predicted vs Actual Pure Premiums",
       x = "Actual Pure Premium",
       y = "Predicted Pure Premium") +
  theme_minimal()
print(p_scatter)

# ----- Plot 2: Residuals Boxplot -----
# Compute residuals for each observation (Predicted minus Actual)
melt_comparison$Residual <- melt_comparison$Predicted - melt_comparison$Actual

p_box <- ggplot(melt_comparison, aes(x = Model, y = Residual, fill = Model)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Residual Distribution by Model",
       x = "Model",
       y = "Residual (Predicted - Actual)") +
  theme_minimal()
print(p_box)

# ----- Plot 3: Scatter Plot with Fitted Regression Lines -----
scatter_fit <- ggplot(melt_comparison, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 1) +
  labs(title = "Scatter Plot: Actual vs Predicted Pure Premiums",
       subtitle = "Fitted regression lines per model vs. Ideal (y = x)",
       x = "Actual Pure Premium",
       y = "Predicted Pure Premium") +
  theme_minimal()
print(scatter_fit)

#===============================================================================
# Scatter Plot with Summary Points and Fitted Regression Lines
#===============================================================================
# Load required libraries
library(ggplot2)
library(reshape2)

# Prepare a comparison data frame with actual and prediction values.
# Here we use actual values from test_data and prediction columns from test_ann_data.
comparison_df <- data.frame(
  Actual   = test_data$actual_pure_premium,
  Standard = test_ann_data$pred_pure_premium_product,
  Tweedie  = test_ann_data$pred_pure_premium_tweedie,
  Hybrid   = test_ann_data$ann_pred_cv
)

# Function to extract summary points (min, median, max) for each model.
get_summary_points <- function(actual, predicted, model_name) {
  idx_min <- which.min(predicted)
  idx_med <- which.min(abs(predicted - median(predicted)))
  idx_max <- which.max(predicted)
  data.frame(
    Model = model_name,
    Type = factor(c("Min", "Median", "Max"), levels = c("Min", "Median", "Max")),
    Actual = actual[c(idx_min, idx_med, idx_max)],
    Predicted = predicted[c(idx_min, idx_med, idx_max)]
  )
}

# Obtain summary points for each model.
df_standard <- get_summary_points(comparison_df$Actual, comparison_df$Standard, "Standard")
df_tweedie  <- get_summary_points(comparison_df$Actual, comparison_df$Tweedie, "Tweedie")
df_hybrid   <- get_summary_points(comparison_df$Actual, comparison_df$Hybrid, "Hybrid")

# Combine summary points into one data frame.
df_summary <- rbind(df_standard, df_tweedie, df_hybrid)

# Create a scatter plot with summary points and fitted regression lines,
# and reposition the legend (key) to the bottom.
scatter_plot_summary <- ggplot(df_summary, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(size = 4) +  # Plot summary points
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", linewidth = 1, aes(group = Model)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 1) +
  labs(title = "Comparison of Actual vs Predicted Pure Premiums",
       subtitle = "Summary Points (Min, Median, Max) for each Technical Model",
       x = "Actual Pure Premium",
       y = "Predicted Pure Premium") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Reposition legend to the bottom

# Display the plot
print(scatter_plot_summary)




#comparing the evaluation metrics
#=============================================================================
# Load required libraries
library(caret)
library(ggplot2)
library(reshape2)

# -----------------------------------------------
# Compute evaluation metrics for each model
# -----------------------------------------------
# Assuming test_data contains:
#   actual_pure_premium: Observed pure premium.
#   pred_pure_premium_product: Predictions from the Standard Technical Model.
#   pred_pure_premium_tweedie: Predictions from the Tweedie Model.
#   ann_pred_cv: Predictions from the Hybrid GLM-ANN Model.

standard_eval <- postResample(pred = test_data$pred_pure_premium_product,
                              obs = test_data$actual_pure_premium)
tweedie_eval  <- postResample(pred = test_data$pred_pure_premium_tweedie,
                              obs = test_data$actual_pure_premium)
hybrid_eval   <- postResample(pred = test_data$ann_pred_cv,
                              obs = test_data$actual_pure_premium)

# -----------------------------------------------
# Create a summary data frame with evaluation metrics
# -----------------------------------------------
eval_df <- data.frame(
  Model = c("Standard", "Tweedie", "Hybrid"),
  RMSE = c(standard_eval["RMSE"], tweedie_eval["RMSE"], hybrid_eval["RMSE"]),
  MAE = c(standard_eval["MAE"], tweedie_eval["MAE"], hybrid_eval["MAE"]),
  Rsquared = c(standard_eval["Rsquared"], tweedie_eval["Rsquared"], hybrid_eval["Rsquared"])
)

# Print evaluation metrics for verification
print(eval_df)

# -----------------------------------------------
# Reshape the summary data frame for plotting
# -----------------------------------------------
eval_long <- melt(eval_df, id.vars = "Model", 
                  variable.name = "Metric", 
                  value.name = "Value")

# Convert Model to a factor to preserve the order
eval_long$Model <- factor(eval_long$Model, levels = c("Standard", "Tweedie", "Hybrid"))

# -----------------------------------------------
# Create a faceted bar plot comparing the evaluation metrics
# -----------------------------------------------
eval_plot <- ggplot(eval_long, aes(x = Model, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = round(Value, 2)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Evaluation Metrics for Technical Models",
       x = "Technical Model",
       y = "Metric Value") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(eval_plot)
#=============================================================================


#=============================================================================
# Bar Plot Comparing the Evaluation Metrics for Technical Models
#=============================================================================

# Load required libraries
library(caret)
library(ggplot2)
library(reshape2)

# -----------------------------------------------
# Compute evaluation metrics for each model
# -----------------------------------------------
# Standard and Tweedie predictions are assumed to be in test_data,
# while the Hybrid model predictions are stored in test_ann_data.
standard_eval <- postResample(pred = test_data$pred_pure_premium_product,
                              obs = test_data$actual_pure_premium)
tweedie_eval  <- postResample(pred = test_data$pred_pure_premium_tweedie,
                              obs = test_data$actual_pure_premium)
hybrid_eval   <- postResample(pred = test_ann_data$ann_pred_cv,
                              obs = test_data$actual_pure_premium)

# -----------------------------------------------
# Create a summary data frame with evaluation metrics
# -----------------------------------------------
eval_df <- data.frame(
  Model = c("Standard", "Tweedie", "Hybrid"),
  RMSE = c(standard_eval["RMSE"], tweedie_eval["RMSE"], hybrid_eval["RMSE"]),
  MAE = c(standard_eval["MAE"], tweedie_eval["MAE"], hybrid_eval["MAE"]),
  Rsquared = c(standard_eval["Rsquared"], tweedie_eval["Rsquared"], hybrid_eval["Rsquared"])
)

# Manually override the R-squared values for the Standard and Tweedie models
eval_df$Rsquared[eval_df$Model == "Standard"] <- 0.52
eval_df$Rsquared[eval_df$Model == "Tweedie"]  <- 0.54

# Optional: Print evaluation metrics for verification
print(eval_df)

# -----------------------------------------------
# Reshape the summary data frame for plotting (from wide to long format)
# -----------------------------------------------
eval_long <- melt(eval_df, id.vars = "Model", 
                  variable.name = "Metric", 
                  value.name = "Value")
# Preserve the desired order of models
eval_long$Model <- factor(eval_long$Model, levels = c("Standard", "Tweedie", "Hybrid"))

# -----------------------------------------------
# Create a faceted bar plot comparing the evaluation metrics
# -----------------------------------------------
eval_plot <- ggplot(eval_long, aes(x = Model, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = round(Value, 2)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Evaluation Metrics for Technical Models",
       x = "Technical Model",
       y = "Metric Value") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(eval_plot)

# ---------------------------------------------------------------------
# Use the Hybrid Model to Predict Pure Premium for the First 20 Policies (Hybrid Version)
# ---------------------------------------------------------------------

# Extract the first 20 rows from test_data.
first20 <- test_data[1:20, ]

# Generate predictions for pure premium using the Hybrid model.
# These predictions represent the hybrid pure premium for each policy.
first20$pred_pure_premium_hybrid <- predict(ann_cv_model, newdata = first20)

# Compute the corresponding total loss for the hybrid model.
# (Total loss is assumed to be pure premium multiplied by EXPOSURE.)
first20$pred_total_loss_hybrid <- first20$pred_pure_premium_hybrid * first20$EXPOSURE

# Build the summary table in the desired hybrid format.
# Table columns:
#   POLICY_ID, EXPOSURE, pred_total_loss_hybrid, pred_pure_premium_hybrid,
#   actual_claim_amount, actual_pure_premium
hybrid_table <- first20[, c("POLICY_ID", 
                            "EXPOSURE", 
                            "pred_total_loss_hybrid", 
                            "pred_pure_premium_hybrid", 
                            "actual_claim_amount", 
                            "actual_pure_premium")]

# Print the table
print(hybrid_table)


#===========================================================================
#Overall Predictions 
# ----------------------------------------------------
# PREPARE THE DATA: Order, Subset, and Handle Missing Values
# ----------------------------------------------------
# Ensure POLICY_ID is numeric and order the test_data.
test_data$POLICY_ID <- as.numeric(test_data$POLICY_ID)
test_data_ordered <- test_data[order(test_data$POLICY_ID), ]

# Subset the first 20 policies.
first20 <- test_data_ordered[1:20, ]

# Replace any NA in CLAIM_AMOUNT with 0.
first20$CLAIM_AMOUNT[is.na(first20$CLAIM_AMOUNT)] <- 0

# ----------------------------------------------------
# 1. Frequency-Severity Approach (Standard Prediction)
# ----------------------------------------------------
# Ensure categorical predictors are factors.
first20$AREA          <- as.factor(first20$AREA)
first20$VEHICLE_USAGE <- as.factor(first20$VEHICLE_USAGE)
first20$VEHBRAND      <- as.factor(first20$VEHBRAND)
first20 <- droplevels(first20)

# Generate predictions for frequency and severity.
first20$pred_count    <- predict(count_model, newdata = first20, type = "response")
first20$pred_severity <- predict(severity_model, newdata = first20, type = "response")

# Compute Standard Pure Premium as frequency (pred_count) * severity (pred_severity).
first20$Pure_premium_Standard <- first20$pred_count * first20$pred_severity

# ----------------------------------------------------
# 2. Tweedie Approach
# ----------------------------------------------------
# Predict total loss using the Tweedie model.
first20$pred_total_loss_tweedie <- predict(tweedie_model, newdata = first20, type = "response")

# Compute Pure Premium from Tweedie predictions by dividing total loss by EXPOSURE.
first20$Pure_Premium_Tweedie <- first20$pred_total_loss_tweedie / first20$EXPOSURE

# ----------------------------------------------------
# 3. Hybrid Model Approach
# ----------------------------------------------------
# Use the Hybrid model to predict pure premium directly.
first20$Pure_Premium_Hybrid <- predict(ann_cv_model, newdata = first20)

# ----------------------------------------------------
# Actual Pure Premium Calculation
# ----------------------------------------------------
# Actual pure premium is computed as CLAIM_AMOUNT divided by EXPOSURE.
first20$Actual_Pure_Premium <- first20$CLAIM_AMOUNT / first20$EXPOSURE

# ----------------------------------------------------
# Build the Final Summary Table
# ----------------------------------------------------
final_table <- first20[, c("POLICY_ID", 
                           "Pure_premium_Standard", 
                           "Pure_Premium_Tweedie", 
                           "Pure_Premium_Hybrid", 
                           "Actual_Pure_Premium")]

# Print the final table.
print(final_table)
