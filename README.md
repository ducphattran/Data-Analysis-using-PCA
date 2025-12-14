# Data-Analysis-using-PCA

###############################################################
# Performance.r
#
# Purpose:
#   This script performs regression analysis to investigate
#   which types of learners achieve higher GPA based on study
#   behaviors. It uses principal components (PC1, PC2, PC3)
#   derived from PCA and examines their relationship with GPA.
#
# Key Steps:
#   1. Fit a linear regression model: GPA ~ PC1 + PC2 + PC3
#   2. Output regression summary (coefficients, p-values, R²)
#   3. Generate scatter plots:
#        - GPA vs PC1 (Engagement)
#        - GPA vs PC2 (Traditional vs Guided Style)
#   4. Create learner quadrant plot (PC1 vs PC2) to classify
#      students into four study-style categories
#   5. Save plots for inclusion in the Overleaf report appendix
#
# Connection to Report:
#   - Regression table and plots provide statistical and visual
#     evidence for Section 4 (Regression Analysis).
#   - Learner quadrant plot supports Section 3 (Learner Type
#     Classification).
#   - This file is included in the appendix to ensure
#     transparency and reproducibility of the analysis.
###############################################################
