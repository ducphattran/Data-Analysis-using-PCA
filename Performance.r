library(tidyverse)
# The other libraries like 'corrr', 'ggcorrplot', 'FactoMineR', 
# 'factoextra', 'rrcov', and 'mt' are not strictly needed for this
# core PCA and regression task, but we keep the main ones.

# --- PCA using princomp ---
# Note: princomp by default performs scaling (scale = TRUE) and centering (cor = TRUE).
# To match the original prcomp call (center=FALSE, scale.=FALSE), 
# you need to set both cor=FALSE and scale=FALSE.
# However, if your intention for the original prcomp was to match the behavior
# of princomp, you should remove the center/scale arguments or set them to TRUE/cor=TRUE.
# Assuming you want to *strictly match* the original call's parameters:

pca_result_comp <- princomp(gpa_df, cor = FALSE, scores = TRUE) 
# 'cor=FALSE' means scale and center are not applied, matching the prcomp call's center=FALSE, scale.=FALSE.
# 'scores=TRUE' is needed to calculate the principal component scores (PC scores).

# --- Summary and Variance ---
summary(pca_result_comp) # Shows standard deviations, proportion of variance, and cumulative proportion.

# Example code to check cumulative variance:
# princomp stores the standard deviations in $sdev, like prcomp.
variance_explained <- pca_result_comp$sdev^2 / sum(pca_result_comp$sdev^2)
cumulative_variance <- cumsum(variance_explained)
print(cumulative_variance)

# --- Loadings (Rotation) and PC Scores ---
# In princomp, the 'loadings' are the eigenvectors (same as 'rotation' in prcomp).
pca_result_comp$loadings # Equivalent to pca_result$rotation

# In princomp, the principal component scores are stored in $scores.
pc_scores <- as.data.frame(pca_result_comp$scores) # Equivalent to as.data.frame(pca_result$x)

# --- Regression and Plotting ---
# Combine the relevant PC scores and the GPA vector into a new data frame
final_data <- data.frame(
  GPA = gpa_vector, # Assuming gpa_vector is available in the environment
  PC1 = pc_scores$Comp.1, # princomp names components Comp.1, Comp.2, ...
  PC2 = pc_scores$Comp.2,
  PC3 = pc_scores$Comp.3 
)

# using linear regression model to find the coefficients
model <- lm(GPA ~ PC1 + PC2 + PC3, data = final_data)
summary(model)
model$coefficients

#make prediction based on the most impactful PCA factor by plotting
# Note: The original code's titles are based on an *assumption* about which PC 
# is most impactful (PC3) and the *sign* of its coefficient.
# You must check the regression summary for the actual coefficient signs and significance.

plot(y = final_data$GPA, x = final_data$PC3,
     main="Relationship between PC3 and GPA",
     xlab = "PC3",
     ylab = "GPA")
# model$coefficients[1] is the intercept, model$coefficients[4] is the coefficient for PC3
abline(a = model$coefficients[1], b = model$coefficients[4], col = "red")

plot(y = final_data$GPA, x = final_data$PC2,
     main="Relationship between PC2 and GPA",
     xlab = "PC2",
     ylab = "GPA")
# model$coefficients[3] is the coefficient for PC2
abline(a = model$coefficients[1], b = model$coefficients[3], col = "blue")


#Visualized relationship between variables in high impact PCA components
# Use the FactoMineR PCA function
pca_result_fm <- PCA(gpa_df, scale.unit = FALSE, graph = FALSE)
# Plotting should now work without the class error
fviz_pca_var(
  pca_result_fm,
  axes = c(2, 3),
  col.var = "cos2",
  gradient.cols = c("red", "orange", "green"),
  repel = TRUE
)