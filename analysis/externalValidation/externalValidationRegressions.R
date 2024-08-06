# Rouault*, Seow*, Gillan and Fleming. (2018) Biological Psychiatry
# Psychiatric symptom dimensions are associated with dissociable shifts in metacognition but not task performance.

# Figures for regression data and factor analysis in Experiment 2

################################################################################
## LOAD THE PREDICTED FACTOR SCORES FOR ROUAULT BASED ON THE REDUCED ITEMS    ##
## PUT THESE INTO THE REGRESSIONS AND SEE IF SAME RESULTS                     ##
## Code here is adapted from the original code                                ##
################################################################################

# NOTE - the data used for the main EFA has one subject excluded, based on their MRatio being negative.
# In the original analysis, this subject was included in every other analysis. Here, we exclude them
# for simplicity. This does not change the results.

# Clear Workspace
rm(list = ls())

# Restore Renv
renv::restore()

# Load Libraries
# Libraries for plotting and graphics
library(ggplot2)        # For creating plots
library(gridExtra)      # For arranging multiple grid-based figures on one page
library(ggpubr)         # For arranging multiple figures on one page  

# Libraries for statistical analysis and modeling
library(lme4)           # For linear mixed-effects models
library(plyr)           # For data manipulation
library(psych)          # For psychometric analysis and data cleaning
library(polycor)        # For polychoric and polyserial correlations
library(nFactors)       # For factor analysis and computing the number of factors

# Libraries for data transformation and summary
library(reshape)        # For reshaping data
library(doBy)           # For descriptive statistics and group-wise computations
library(dplyr)          # For data manipulation and transformation
library(purrr)          # For functional programming

# Libraries for specific statistical techniques and other utilities
library(GPArotation)    # For factor rotation methods
library(paran)          # For Horn's Test of principal components/factors
library(R.matlab)       # For reading and writing MAT files in MATLAB

# Set Options
options(scipen = 999)   # Display variable quantities in decimals (not in scientific notation)

# Define a function to plot the regression coefficients
plot_factor_fig <- function(data, y_intercept = 0, ylim_lower = -0.3, ylim_upper = 0.3, fill_colors = c("#8dd3c7", "#ffffbc", "#bebada")) {
  
  ggplot(data = data, aes(x = Label, y = Estimate, group = Type)) +
    geom_bar(aes(fill = Type), colour = "black", size = 1.2, stat = "identity", position = "dodge", width = 0.8) +
    geom_errorbar(aes(ymin = Estimate - StdError, ymax = Estimate + StdError), colour = "black", width = 0.3, size = 1.2, position = position_dodge(.8)) +
    geom_hline(yintercept = y_intercept, size = 1) +
    theme_classic() +
    labs(title = " ", x = " ", y = "Regression Coefficient") +
    theme(
      axis.title.y = element_text(size = rel(2.5), angle = 90, margin = margin(0, 20, 0, 0)),
      axis.title.x = element_text(size = rel(3), angle = 0, margin = margin(20, 0, 0, 0)),
      plot.title = element_text(size = rel(3), angle = 0),
      legend.text = element_text(size = 20),
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, size = 20),
      axis.text.y = element_text(angle = 0, size = 25),
      axis.line.x = element_line(color = "black", size = 1.2),
      axis.line.y = element_line(color = "black", size = 1.2),
      axis.ticks.y = element_line(size = (1.5)),
      axis.ticks.x = element_line(size = (1.5)),
      axis.ticks.length = unit(0.4, "cm")
    ) +
    scale_x_discrete(expand = c(0, 0.5)) +
    scale_fill_manual(values = fill_colors) +
    theme(legend.position = "none") +
    ylim(ylim_lower, ylim_upper)
}


## LOADING DATA ##
# Load Data from GitHub
# Define the URL of the .mat file on GitHub
qnData_url <- "https://github.com/metacoglab/RouaultSeowGillanFleming/raw/master/ME_phase2_excludqnadata_all.mat"
taskData_url <- "https://github.com/metacoglab/RouaultSeowGillanFleming/raw/master/ME_phase2_excludanalyseddat_all.mat"

# Download and load the questionnaire data .mat file
temp_qnData <- tempfile(fileext = ".mat")
download.file(qnData_url, temp_qnData, mode = "wb")
qnData <- readMat(temp_qnData)

# Download and load the task performance data .mat file
temp_taskData <- tempfile(fileext = ".mat")
download.file(taskData_url, temp_taskData, mode = "wb")
taskData <- readMat(temp_taskData)

# Load the HDDM data .csv file directly
HDDM <- read.csv(url("https://raw.githubusercontent.com/metacoglab/RouaultSeowGillanFleming/master/subjParams_2k_3chain.csv"))

# Transform HDDM data
HDDMpara <- data.frame(t(HDDM[1:nrow(HDDM), 2:ncol(HDDM)]))
colnames(HDDMpara) <- c("a", "t", "v_inter", "v_delta")

## LOAD PREDICTED FACTOR SCORES
factorScores <- read.csv('data/predictions_70item.csv') # load HDDM data
# select where study == 3 (Rouault)
factorScores <- factorScores[factorScores$study == 3,]

# Initialize Task Performance Data Variables
n_taskData <- length(taskData$analyseddata)
task_perf_vars <- c("id", "age", "gender", "accuracy", "mRatio", "confMean")
taskPerfData <- as.data.frame(matrix(0, n_taskData, length(task_perf_vars)))
colnames(taskPerfData) <- task_perf_vars

# Initialize Questionnaire Data Variables
n_qnData <- length(qnData$allqna)
qn_data_vars <- c("qnid", "zung", "anxiety", "ocir", "leb", "iq", "bis", "schizo", "eat", "apathy", "alcohol")
qnDataMatrix <- as.data.frame(matrix(0, n_qnData, length(qn_data_vars)))
colnames(qnDataMatrix) <- qn_data_vars


# Extract Data from allqna Data File
extract_vars <- c("id", "zung", "anxiety", "ocir", "leb", "iq", "bis", "schizo", "eat", "apathy", "alcohol")

qnDataList <- lapply(qnData$allqna, function(x) 
  sapply(extract_vars, function(v) {
    if (v == "id") {
      return(x[[1]][,,1][[v]])
    } else if (v == "bis" || v == "schizo" || v == "eat") {
      return(x[[1]][,,1][[v]][,,1]$score[,,1]$total)
    } else {
      return(x[[1]][,,1][[v]][,,1]$score)
    }
  })
)

qnFrame <- as.data.frame(do.call(rbind, qnDataList), stringsAsFactors = FALSE)

# Extract Data from analysed Data File
# Using lapply to extract individual pieces of data for each subject
extract_vars_task <- c("id", "age", "gender", "confMean", "accuracy", "mRatio")
taskDataList <- lapply(taskData$analyseddata, function(x) {
  data <- x[[1]][,,1]$data
  c(
    id = data[1, 4],
    age = data[1, 2],
    gender = data[1, 3],
    confMean = mean(data[, 9], na.rm = TRUE),
    accuracy = mean(data[, 6], na.rm = TRUE),
    mRatio = x[[1]][,,1]$mratio
  )
})
taskFrame <- as.data.frame(do.call(rbind, taskDataList), stringsAsFactors = FALSE)

# Set gender as factor (male or female)
taskFrame$gender <- factor(taskFrame$gender, labels = c("male", "female"))

allData <- taskFrame %>%
  left_join(qnFrame, by = "id") %>%
  bind_cols(HDDMpara)

# Scaling Data
# List of columns to be scaled
scale_cols <- c("age", "confMean", "accuracy", "zung", "anxiety", "ocir", "leb", "iq", "schizo", "bis", "eat", "apathy", "alcohol", "a", "t", "v_inter", "v_delta")
log_transform_cols <- c("zung", "anxiety", "ocir", "leb", "schizo", "bis", "eat", "apathy", "alcohol")  # List of columns that need a log transformation

# Scaling and transformation where needed
allData <- allData %>%
  mutate(across(all_of(log_transform_cols), ~log(.x + 1), .names = "{col}.sc")) %>%
  mutate(across(all_of(scale_cols), scale, .names = "{col}.sc"))

# Exclude negative mRatios and scale the mRatios of the subjects left
mrExcludedData <- allData %>%
  filter(mRatio > 0) %>%
  mutate(mRatio.sc = scale(log(mRatio)))

# Replace factor score id with the id from the task data
factorScores$id <- mrExcludedData$id

##  FACTOR ANALYSIS ##
# Create a function to extract raw data from the given list
extract_raw_data <- function(data_list, variable_name) {
  sapply(data_list, function(x) x[[1]][,,1][[variable_name]][,,1]$raw)
}

# Loop through every subject and extract raw data
qn_list <- qnData$allqna
qnIndivid <- sapply(qn_list, function(x) x[[1]][,,1]$id)

# Extract raw data for each variable
zungAll <- extract_raw_data(qn_list, 'zung')
anxietyAll <- extract_raw_data(qn_list, 'anxiety')
ocirAll <- extract_raw_data(qn_list, 'ocir')
lebAll <- extract_raw_data(qn_list, 'leb')
bisAll <- extract_raw_data(qn_list, 'bis')
schizoAll <- extract_raw_data(qn_list, 'schizo')
eatAll <- extract_raw_data(qn_list, 'eat')
apathyAll <- extract_raw_data(qn_list, 'apathy')
alcoholAll <- extract_raw_data(qn_list, 'alcohol')

# Combine extracted data into a data frame
qns <- data.frame(
  qnid = qnIndivid
)

# adding transposed matrices to the data frame
qns$zung <- I(t(zungAll))
qns$anxiety <- I(t(anxietyAll))
qns$ocir <- I(t(ocirAll))
qns$leb <- I(t(lebAll))
qns$bis <- I(t(bisAll))
qns$schizo <- I(t(schizoAll))
qns$alcohol <- I(t(alcoholAll))
qns$eat <- I(t(eatAll))
qns$apathy <- I(t(apathyAll))

# Set up the regression analyses for evaluating the associations between factor scores and several dependent variables
# The dependent variables include task performance, HDDM parameters, and others. 

# Centering the predicted scores for normality
factorScores$AD <- scale(factorScores$AD, center = TRUE, scale = TRUE)
factorScores$Compul <- scale(factorScores$Compul, center = TRUE, scale = TRUE)
factorScores$SW <- scale(factorScores$SW, center = TRUE, scale = TRUE)

# Concatenate mrExcludedData and factorScores
mrExcludFactorData <- cbind(mrExcludedData, factorScores)

# Define a list of dependent variables and their corresponding regression models
dependent_vars <- c("accuracy.sc", "confMean.sc", "mRatio.sc", "a.sc", "t.sc", "v_delta.sc", "v_inter.sc")
regression_models <- lapply(dependent_vars, function(dep_var) {
  formula_str <- paste(dep_var, "~ AD + Compul + SW + iq.sc + age.sc + gender")
  lm(as.formula(formula_str), data = mrExcludFactorData)
})

# Test magnitude of contrasts for mean confidence
confMeanFactorReg <- regression_models[[which(dependent_vars == "confMean.sc")]]
lambda1 <- c(0,1,0,0,0,0,0)
esticon(confMeanFactorReg, lambda1, beta0 = 0)

# Extract coefficients into data frames and store in a list
regression_summaries <- lapply(regression_models, function(model) {
  coefficients_summary <- summary(model)$coefficients[2:4, 1:4]
  as.data.frame(coefficients_summary)
})

# Naming the list elements
names(regression_summaries) <- dependent_vars


############################################################
# REGRESSIONS & PLOTS: PERFORMANCE/METCOG/HDDM ~ FACTOR SCORES 
############################################################

# Define a mapping of the list names to the Labels you want
name_to_label <- c(
  "accuracy.sc" = "Accuracy",
  "confMean.sc" = "Confidence Level",
  "mRatio.sc" = "Metacognitive Efficiency",
  "a.sc" = "a",
  "t.sc" = "t",
  "v_delta.sc" = "v delta",
  "v_inter.sc" = "v intercept"
)

# Function to set labels and types
set_labels_and_types <- function(df, label) {
  df$Label <- label
  df$Type <- rownames(df)
  return(df)
}

# Apply transformations and combine dataframes
factorRegFig <- regression_summaries %>%
  imap(~ set_labels_and_types(.x, name_to_label[[.y]])) %>%
  bind_rows()

# Plotting
factorRegFig$Label <- factor(factorRegFig$Label, levels = c("Accuracy", "Confidence Level", "Metacognitive Efficiency", "a", "t", "v delta", "v intercept"))
factorRegFig$Type[factorRegFig$Type == "AD"] <- "Anxious"
factorRegFig$Type[factorRegFig$Type == "Compul"] <- "Compulsivity"
factorRegFig$Type[factorRegFig$Type == "SW"] <- "Social Withdrawal"

# Rename std error column
names(factorRegFig)[names(factorRegFig) == "Std. Error"] <- "StdError"


# Use the function
factorFig <- plot_factor_fig(factorRegFig)
factorFig

# Create figure directory
dir.create("figures/external_validation", showWarnings = TRUE, recursive = TRUE)

# Save the plot
ggsave(factorFig, file = paste('figures/external_validation/marionRegsFinal.eps', sep = ''), device = "eps")
# And as png
ggsave(factorFig, file = paste('figures/external_validation/marionRegsFinal.png', sep = ''), device = "png")

# Create directory for regressions
dir.create("data/regressions", showWarnings = FALSE, recursive = TRUE)

# Save factorRegFig to csv
write.csv(factorRegFig, file = "data/regressions/RouaultFactorReg_predictedScores.csv", row.names = FALSE)

############################################################
# ADDING ORIGINAL RESULTS TO PLOT 
############################################################

# Load Data
scoresOriginal <- read.csv("data/EFAscores/RouaultScores.csv") # Load items

# Concatenate mrExcludedData and factorScores
mrExcludFactorData2 <- cbind(mrExcludedData, scoresOriginal)

# Performing linear regressions
accuFactorReg2 <- lm(accuracy.sc ~ AD + Compul + SW + iq.sc + age.sc + gender, data = mrExcludFactorData2)
confMeanFactorReg2 <- lm(confMean.sc ~ AD + Compul + SW + iq.sc + age.sc + gender, data = mrExcludFactorData2)
mRatioFactorReg2 <- lm(mRatio.sc ~ AD + Compul + SW + iq.sc + age.sc + gender, data = mrExcludFactorData2)

# Testing magnitudes of contrasts for mean confidence
esticon(confMeanFactorReg2, c(0, 1, rep(0, 5)))

# Extracting coefficients into dataframes and labeling them
extract_and_label <- function(model, label) {
  df <- data.frame(summary(model)$coefficients[2:4, 1:4], Label = label, Type = rownames(summary(model)$coefficients[2:4, ]))
}

accuFactorRegFig2 <- extract_and_label(accuFactorReg2, 'Accuracy')
confMeanFactorRegFig2 <- extract_and_label(confMeanFactorReg2, 'Confidence Level')
mRatioFactorRegFig2 <- extract_and_label(mRatioFactorReg2, 'Metacognitive Efficiency')

# Plotting Task Performance, HDDM, Metacognition against Factor Scores
factorRegFig2 <- rbind(accuFactorRegFig2, confMeanFactorRegFig2, mRatioFactorRegFig2)
factorRegFig2$Label <- factor(factorRegFig2$Label, levels = c("Accuracy", 'Confidence Level', 'Metacognitive Efficiency'))
factorRegFig2$Type[factorRegFig2$Type == "AD"] <- "Anxious"
factorRegFig2$Type[factorRegFig2$Type == "Compul"] <- "Compulsivity"
factorRegFig2$Type[factorRegFig2$Type == "SW"] <- "Social Withdrawal"

# Rename std error column
names(factorRegFig2)[names(factorRegFig2) == "Std..Error"] <- "StdError"

# Save factorRegFig2 to csv
write.csv(factorRegFig2, file = "data/regressions/RouaultFactorReg_originalScores.csv", row.names = FALSE)

# Use the function
factorFig2 <- plot_factor_fig(factorRegFig2)
plot

# Put plots together
ggarrange(factorFig, factorFig2)

# Save the plot
ggsave(factorFig2, file = paste('figures/external_validation/marionRegsFinal2.eps', sep = ''), device = "eps")

