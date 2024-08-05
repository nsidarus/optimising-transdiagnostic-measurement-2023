##########
## CLEAR ALL
rm(list = ls())

# Restore Renv
renv::restore()

#############################
## LOADING LIBRARIES/TOOLS ##
#############################

# For plotting graphs
library(ggplot2)
library(gridExtra)
library(ggpubr)

# For linear regression functions
library(lme4)

# For data manipulation and summarization
library(plyr)
library(doBy)

# For psychometrics, factor analysis, and structural equation modeling
library(psych)
library(GPArotation)
library(paran)
library(nFactors)
library(lavaan)
library(sem)

# For correlation analysis and partial correlations
library(polycor)
library(ppcor)
library(corrplot)

# For handling missing data and imputation
library(missMDA) # This also loads required package FactoMineR

# For data pre-processing and machine learning
library(caret)

# For hypothesis testing, ANOVA and other statistical tests
library(car)

# For reshaping data
library(reshape)

# For reading MATLAB files
library(R.matlab)

###############
## FUNCTIONS ##
###############

# Define a function to create a plot of factor loadings for a given column name and title
loadings_plot <- function(data, col_name, title, color_seq) {
  data$x <- seq_len(nrow(data))  # Create x as a sequence of row numbers
  p <- ggplot(data = data, aes_string(y = col_name, x = "x", fill = "as.factor(x)")) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = color_seq) +
    theme(
      legend.position = "None", panel.background = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    labs(title = title, x = "Item number", y = "Loadings")

  # Return the plot
  return(p)
}

# Plot Score Histograms
histogram_plot <- function(data, column, fill_color) {
  p <- ggplot(data = data, aes_string(x = column)) +
    geom_histogram(bins = 20, col = "black", fill = fill_color, alpha = .2)

  # Return the plot
  return(p)
}

#' Perform Study Analysis
#'
#' This function performs a comprehensive analysis on a given study or all studies. It calculates the correlation matrix, eigenvalues, and performs factor analysis. It also has options to recompute the correlation matrix and save various results such as heterogeneity matrix and eigenvalues. This function also generates plots and histograms as specified by the user.
#'
#' @param data A dataframe containing the study data.
#' @param study_number A numeric value representing the number corresponding to a study. If NULL, the function will run on all data.
#' @param study_name A character string representing the name of the study. Defaults to "AllData".
#' @param n_factors A numeric value indicating the number of factors to be used in the factor analysis.
#' @param loadings_cols A vector of character strings representing the column names to be used for loadings in factor analysis.
#' @param scores_cols A vector of character strings representing the column names to be used for scores in factor analysis.
#' @param recompute_hetcor A logical value indicating whether to recompute the correlation matrix using the \code{hetcor} function. Defaults to FALSE.
#' @param save_hetmat A logical value indicating whether to save the heterogeneity matrix. Defaults to FALSE.
#' @param save_eigenvalues A logical value indicating whether to save the eigenvalues. Defaults to FALSE.
#' @param loadings_color_seq An optional color sequence to be used for the loadings plot.
#' @return A list containing eigenvalues, factor analysis results, factor scores and loadings, and the used data frame.
perform_study_analysis <- function(
    data,
    study_number = NULL,
    study_name = "AllData",
    n_factors, loadings_cols,
    scores_cols,
    recompute_hetcor = FALSE,
    save_hetmat = FALSE,
    save_eigenvalues = FALSE,
    loadings_color_seq = NULL) {
  # If study_number is not NULL, select the specific study, else use the entire dataset
  if (!is.null(study_number)) {
    study_data <- data[data$study == study_number, 3:ncol(data)]
    IDs <- data$subid[data$study == study_number]
  } else {
    study_data <- data[, 3:ncol(data)]
    IDs <- data$subid
  }

  # Compute correlation matrix using hetcor
  if (recompute_hetcor) {
    het.mat <- hetcor(study_data)$cor
  } else {
    load(file.path("data", "hetmats", paste0("hetmat_", study_name, ".RData")))
  }

  # Eigenvalue decomposition
  ev <- eigen(het.mat)

  # Save het.mat if save_hetmat is TRUE
  if (save_hetmat) {
    dir.create(file.path("data", "hetmats"), showWarnings = TRUE, recursive = TRUE)
    save(het.mat, file = file.path("data", "hetmats", paste0("hetmat_", study_name, ".RData")))
  }

  # Save eigenvalues if save_eigenvalues is TRUE
  if (save_eigenvalues) {
    dir.create(file.path("data", "eigenvalues"), showWarnings = FALSE, recursive = TRUE)
    write.csv(data.frame(eigenvalues = ev$values), file = file.path("data", "eigenvalues", paste0("eigenvalues_", study_name, ".csv")))
  }

  # Parallel Analysis
  ap <- parallel(subject = nrow(study_data), var = ncol(study_data), rep = 100, cent = .05)
  nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
  # sPlot <- plotnScree(nS, main = "Scree Test solutions")
  
  # Ensure the directory exists before saving
  dir.create(file.path("figures", study_name), showWarnings = TRUE, recursive = TRUE)
  # ggsave(file.path("figures", study_name, paste0("screePlot", study_name, ".png")), plot = sPlot, width = 10, height = 5)

  # Factor Analysis
  fa_result <- fa(r = het.mat, nfactors = n_factors, n.obs = nrow(study_data), rotate = "oblimin", fm="ml", scores="regression")
  fa_scores <- factor.scores(x=study_data, f=fa_result)
  scores_df <- data.frame("id"=IDs, fa_scores$scores)
  colnames(scores_df) <- c("id", scores_cols)

  # Loadings
  loadings_mat <- fa_result$loadings[, 1:length(loadings_cols)]
  loadings_df <- data.frame(loadings_mat)
  colnames(loadings_df) <- loadings_cols

  # Create and Save Plots
  plots <- lapply(loadings_cols, function(col) loadings_plot(loadings_df, col, col, loadings_color_seq))
  plots_arranged <- do.call(grid.arrange, c(plots, ncol=1))
  ggsave(file.path('figures', study_name, paste0('loadings', study_name, '.png')), plots_arranged, width = 10, height = 5)

  histograms <- lapply(scores_cols, function(col) histogram_plot(scores_df, col, '#2f2f2f'))
  histograms_arranged <- do.call(grid.arrange, c(histograms, ncol=3))
  ggsave(file.path('figures', study_name, paste0('factorScoresHists', study_name, '.png')), histograms_arranged, width = 10, height = 5)

  # Save Factor Scores and Loadings
  scores_dir <- 'data/EFAscores'
  loadings_dir <- 'data/loadings'
  dir.create(scores_dir, showWarnings = TRUE, recursive = TRUE)
  dir.create(loadings_dir, showWarnings = TRUE, recursive = TRUE)
  write.csv(scores_df, file.path(scores_dir, paste0(study_name, 'Scores.csv')))
  write.csv(loadings_df, file.path(loadings_dir, paste0(study_name, 'Loadings.csv')))


  # Create EFAscores directory if it doesn't exist
  dir.create("data/EFAscores", showWarnings = TRUE, recursive = TRUE)

  # Save scores df
  write.csv(scores_df, file.path("data/EFAscores", paste0(study_name, "Scores.csv")))

  # Return the results as a list
  return(list(
    eigenvalues = ev$values,
    IDs = IDs,
    factor_analysis = fa_result,
    scores_df = scores_df,
    loadings_df = loadings_df
  ))
}

plot_eigenvalues <- function(eigenvalues, n_factors, title = "Eigenvalue Plot") {
  # Create a data frame from eigenvalues
  ev_df <- data.frame(
    x = 1:length(eigenvalues),
    eigenvalues = eigenvalues
  )

  # Define colors: the first n_factors bars will be colored differently
  colors <- c(rep("#9CE2EE", n_factors), rep("grey", length(eigenvalues) - n_factors))

  # Create the plot
  p <- ggplot(data = ev_df, aes(y = eigenvalues, x = x, fill = as.factor(x))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colors) +
    labs(x = "Factor number", y = "Eigenvalue", title = title) +
    theme(legend.position = "None", panel.background = element_blank(), axis.line = element_line(colour = "black"))

  return(p)
}


####################################
############ SETTINGS ##########
####################################
reHetCor <- 0 # recompute hetcor or not
options(scipen = 999) # to display variable quantities in decimals (not in scientific notation format)

####################################
############ LOADING DATA ##########
####################################
# Load Data
rtmat_items <- read.csv("data/RTMATitems.csv") # Load items

# Duplicate Data for Further Use
qns <- rtmat_items

# Define the color sequences
color_seq <- c(
  rep("#3BB471", 20), rep("#B9860A", 20), rep("#1C90FE", 18),
  rep("#BA55D3", 24), rep("#FF2F93", 30), rep("#EEE8AC", 43),
  rep("#FE6447", 10), rep("#46D1CC", 26), rep("#0227CD", 18)
)

############################
##### RUN ANALYSIS #########
############################

# All data
result_all_data <- perform_study_analysis(
  qns,
  n_factors = 3,
  loadings_cols = c("AD", "Compul", "SW"),
  scores_cols = c("AD", "Compul", "SW"),
  recompute_hetcor = reHetCor,
  save_hetmat = TRUE,
  save_eigenvalues = TRUE,
  loadings_color_seq = color_seq
)

# Kelley data
result_kelley <- perform_study_analysis(
  qns,
  study_number = 1,
  study_name = "Kelley",
  n_factors = 3,
  loadings_cols = c("AD", "Compul", "SW"),
  scores_cols = c("AD", "Compul", "SW"),
  recompute_hetcor = reHetCor,
  save_hetmat = TRUE,
  save_eigenvalues = TRUE,
  loadings_color_seq = color_seq
)

# Patzelt data
result_patzelt <- perform_study_analysis(
  qns,
  study_number = 2,
  study_name = "Patzelt",
  n_factors = 3,
  loadings_cols = c("AD", "Compul", "SW"),
  scores_cols = c("AD", "Compul", "SW"),
  recompute_hetcor = reHetCor,
  save_hetmat = TRUE,
  save_eigenvalues = TRUE,
  loadings_color_seq = color_seq
)

# Rouault data
result_rouault <- perform_study_analysis(
  qns,
  study_number = 3,
  study_name = "Rouault",
  n_factors = 3,
  loadings_cols = c("AD", "Compul", "SW"),
  scores_cols = c("AD", "Compul", "SW"),
  recompute_hetcor = reHetCor,
  save_hetmat = TRUE,
  save_eigenvalues = TRUE,
  loadings_color_seq = color_seq
)

# Neureka data
result_neureka <- perform_study_analysis(
  qns,
  study_number = 4,
  study_name = "Neureka",
  n_factors = 3,
  loadings_cols = c("AD", "Compul", "SW"),
  scores_cols = c("AD", "Compul", "SW"),
  recompute_hetcor = reHetCor,
  save_hetmat = TRUE,
  save_eigenvalues = TRUE,
  loadings_color_seq = color_seq
)
