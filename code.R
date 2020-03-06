# install and load packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(klaR)) install.packages("klaR", repos = "http://cran.us.r-project.org")
if(!require(caretEnsemble)) install.packages("caretEnsemble", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(compareGroups)) install.packages("compareGroups", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(stepPlr)) install.packages("stepPlr", repos = "http://cran.us.r-project.org")
if(!require(yardstick)) install.packages("yardstick", repos = "http://cran.us.r-project.org")


# load the dataset from github repo
liver_data <- read.csv("https://raw.githubusercontent.com/udaniel/capstone_DS_liver/master/indian_liver_patient.csv")

# basic structure of the dataset
liver_data %>% dim()
liver_data %>% head()
str(liver_data)

# properly factor the outcome variable
liver_data$Dataset <- factor(liver_data$Dataset, levels = c(1, 2), 
                             labels = c("disease", "no_disease"))
# change outcome name
liver_data <- 
    liver_data %>% 
    dplyr::rename(outcome = Dataset)
# fix the misspelling
liver_data <- 
    liver_data %>% 
    dplyr::rename(Total_Proteins = Total_Protiens)

# summary
summary(liver_data)

# age distribution
liver_data %>% 
    ggplot(aes(x = Age)) +
    geom_histogram(aes(y = ..density..), binwidth = 10) +
    geom_density(lwd = 2) +
    theme_classic()

# Total_Proteins distribution
liver_data %>% 
    ggplot(aes(x = Total_Proteins)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.8) +
    geom_density(lwd = 2) +
    theme_classic()

# Albumin distribution
liver_data %>% 
    ggplot(aes(x = Albumin)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.5) +
    geom_density(lwd = 2) +
    theme_classic()

# Albumin_and_Globulin_Ratio distribution
liver_data %>% 
    ggplot(aes(x = Albumin_and_Globulin_Ratio)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.1) +
    geom_density(lwd = 2) +
    theme_classic()


# Total_Bilirubin distribution
liver_data %>% 
    ggplot(aes(x = Total_Bilirubin)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.4) +
    geom_density(lwd = 2) +
    theme_classic()

# Direct_Bilirubin distribution
liver_data %>% 
    ggplot(aes(x = Direct_Bilirubin)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.4) +
    geom_density(lwd = 2) +
    theme_classic()

# Alkaline_Phosphotase distribution
liver_data %>% 
    ggplot(aes(x = Alkaline_Phosphotase)) +
    geom_histogram(aes(y = ..density..), binwidth = 30) +
    geom_density(lwd = 2) +
    theme_classic()

# Alamine_Aminotransferase distribution
liver_data %>% 
    ggplot(aes(x = Alamine_Aminotransferase)) +
    geom_histogram(aes(y = ..density..), binwidth = 30) +
    geom_density(lwd = 2) +
    theme_classic()

# Aspartate_Aminotransferase distribution
liver_data %>% 
    ggplot(aes(x = Aspartate_Aminotransferase)) +
    geom_histogram(aes(y = ..density..), binwidth = 30) +
    geom_density(lwd = 2) +
    theme_classic()

# 90% quantile
# Albumin_and_Globulin_Ratio
liver_data %>% 
    filter(Albumin_and_Globulin_Ratio > quantile(Albumin_and_Globulin_Ratio, 0.9, na.rm = T)) %>% 
    summary()

# Total_Bilirubin distribution
liver_data %>% 
    filter(Total_Bilirubin > quantile(Total_Bilirubin, 0.9)) %>% summary()

# Direct_Bilirubin distribution
liver_data %>% 
    filter(Direct_Bilirubin > quantile(Direct_Bilirubin, 0.9)) %>% summary()

# Alkaline_Phosphotase distribution
liver_data %>% 
    filter(Alkaline_Phosphotase > quantile(Alkaline_Phosphotase, 0.9)) %>% summary()

# Alamine_Aminotransferase distribution
liver_data %>% 
    filter(Alamine_Aminotransferase > quantile(Alamine_Aminotransferase, 0.9)) %>% summary()

# Aspartate_Aminotransferase distribution
liver_data %>% 
    filter(Aspartate_Aminotransferase > quantile(Aspartate_Aminotransferase, 0.9)) %>% summary()


# correlation plot among numerical variables
liver_data %>% 
    select_if(is.numeric) %>% 
    filter(!is.na(Albumin_and_Globulin_Ratio)) %>% 
    cor() %>% 
    corrplot()

# calculate p-values
comp <- compareGroups(outcome ~ ., data = liver_data)
tab <- createTable(comp, extra.labels = c("", ""))
tab

# remove 4 missing values
liver_data_naomit <- liver_data %>% na.omit()

# data split
set.seed(1)
ind <- createDataPartition(liver_data_naomit$outcome, times = 1, p = 0.8, list = F)
train_set <- liver_data_naomit[ind, ]
test_set <- liver_data_naomit[-ind, ]

# pre-process first step
# 10-folds cross-validation to tune the models
x <- trainControl(
    method = "cv",
    number = 10,
    classProbs = T,
    summaryFunction = twoClassSummary
)


set.seed(1)
# train the model
logistic <- train(
    outcome ~ ., data = train_set, method = "plr",
    trControl = x, 
    # standardize the data
    preProc = c("center", "scale"), 
    # we use ROC AUC as a metric
    metric = "ROC"
)
logistic

set.seed(1)
# train the model
nb <- train(
    outcome ~ ., data = train_set, method = "nb",
    trControl = x, 
    # standardize the data
    preProc = c("center", "scale"), 
    # we use ROC AUC as a metric
    metric = "ROC"
)
nb

# train multiple models at once
set.seed(1)
models_list <- 
    caretList(
        outcome ~ ., data = train_set,
        # standardize the data
        preProc = c("center", "scale"), 
        trControl = x, 
        # we use ROC AUC as a metric
        metric = "ROC",
        # list the base classifiers
        methodList = c("plr", "nb", "rf", "xgbTree")
    )

set.seed(1)
model_ensemble <-
    caretEnsemble(models_list, 
                  trControl = x,
                  metric = "ROC")

# predict probabilities
pred_log <- predict(model_ensemble$models$plr, test_set, type = "prob")
pred_nb <- predict(model_ensemble$models$nb, test_set, type = "prob")
pred_rf <- predict(model_ensemble$models$rf, test_set, type = "prob")
pred_xgbTree <- predict(model_ensemble$models$xgbTree, test_set, type = "prob")
# final model prediction
pred_ensemble <- predict(model_ensemble, test_set, type = "prob")

# make temporary dataframes
logis_df_tmp <- tibble(truth = test_set$outcome) %>%
    bind_cols(pred_log)
nb_df_tmp <- tibble(truth = test_set$outcome) %>%
    bind_cols(pred_nb)
rf_df_tmp <- tibble(truth = test_set$outcome) %>%
    bind_cols(pred_rf)
xgbTree_df_tmp <- tibble(truth = test_set$outcome) %>%
    bind_cols(pred_xgbTree)
ensemble_df_tmp <- tibble(truth = test_set$outcome) %>%
    bind_cols(disease = 1 - pred_ensemble, 
              no_disease = pred_ensemble)

# performance for each generated model
roc_logis <- roc_auc(data = logis_df_tmp, truth = truth, disease)$.estimate
roc_nb <- roc_auc(data = nb_df_tmp, truth = truth, disease)$.estimate
roc_rf <- roc_auc(data = rf_df_tmp, truth = truth, disease)$.estimate
roc_xgbTree <- roc_auc(data = xgbTree_df_tmp, truth = truth, disease)$.estimate
roc_ensemble <- roc_auc(data = ensemble_df_tmp, truth = truth, disease)$.estimate

# save the final performance into a tibble data frame
roc_results <-
    tibble(
        method = c("Penalized Logistic Regression", "Naive Bayes", "Random Forest",
                   "Extreme Gradient Boosting Tree", "Ensemble"),
        ROCAUC = c(roc_logis, roc_nb, roc_rf, roc_xgbTree, roc_ensemble)
    )

# plot a roc curve of the ensemble model
roc_curve(data = ensemble_df_tmp, truth = truth, disease) %>% 
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path() +
    geom_abline(lty = 3) +
    coord_equal() +
    ggtitle("ROC plot of the ensemble model") +
    theme_classic()

roc_results
