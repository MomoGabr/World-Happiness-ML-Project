
# WORLD HAPPINESS REPORT ANALYSIS 
# Machine Learning Project


#  1. SETUP & LIBRARY LOADING ----

#load necessary libraries
library(future)
library(ggplot2)
library(ggcorrplot)
library(patchwork)
library(readr)
library(MASS)
library(caTools)
library(readxl)
library(tidyr)
library(ranger)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3tuning)
library(paradox)
library(dplyr)
library(countrycode)

# 2. DATA LOADING & INITIAL EXPLORATION ----

#read and summarize the file
whr <- read_excel('WHR_data.xlsx', sheet="Dataset")
summary(whr)
str(whr)

#view variable sources and types. if you want to see the methodology of the WHR (i.e., what the WHR variables mean): https://files.worldhappiness.report/WHR25_Ch02_Appendix_B.pdf?_gl=1*hivwmg*_gcl_au*MTcyNjQzNjgwNy4xNzYxNjQyOTYy 
variables <- read_excel("WHR_data.xlsx", sheet="Read_me")
variables

# 3. DATA PREPROCESSING ----
## 3.1 Add Geographic Information ----

#add regions and subregions. we will need this for our hierarchical imputation later:)
whr$subregion <- countrycode(whr$country,
                             "country.name",
                             "un.regionsub.name")
whr$region <- countrycode(whr$country,
                          "country.name",
                          "un.region.name")

#for the two missing countries, we assign the region/subregion manually
whr$subregion <- dplyr::case_when(
  whr$country == "Kosovo" ~ "Southern Europe",
  whr$country == "Taiwan Province of China" ~ "Eastern Asia",
  TRUE ~ whr$subregion)

whr$region <- dplyr::case_when(
  whr$country == "Kosovo" ~ "Europe",
  whr$country == "Taiwan Province of China" ~ "Asia",
  TRUE ~ whr$region)

## 3.2 Missing Values Analysis ----

#analyse N/A values
na_count <- sapply(whr, function(j) { sum(is.na(j)) })
na_count

#..and the N/A values per country

country_na_count<-whr%>%
  group_by(country) %>%
  summarise(nb_NA = sum(is.na(across(everything()))))%>%
  arrange(desc(nb_NA))
country_na_count


#drop countries (i.e., these rows) with too many N/As. we will explain later how the others will be imputed
whr <- whr %>%
  filter(!country %in% c("Somalia", "Afghanistan", "Jamaica", "Luxembourg", "Yemen", "Saudi Arabia", "State of Palestine", "Venezuela", "Tajikistan"))

# 4. EXPLORATORY DATA ANALYSIS (EDA) ----
## 4.1 Correlation Analysis ----

#The data has a lot of variables. Let's check if any of them are strongly correlated and see if something can be dropped
numerical <- c('log_gdp_per_capita', 'social_support','healthy_life_expectancy',"generosity", 'freedom', "corruption", "gov_effectiveness", "no_violence", "regulatory_quality", "rule_law", "voice_accountability", "positive_emotions", "negative_emotions", "donated", "volunteered", "helped_a_stranger", "meals_shared", "household_size")
whr_corr <- subset(whr, select=numerical)

cor(whr_corr, use = "p")
corr <- round(cor(whr_corr, use = "complete.obs"), 1)

ggcorrplot(
  corr,
  hc.order = TRUE,        # clusters similar variables
  type = "lower",         # only lower triangle
  lab = FALSE,            # remove labels for cleaner look
  colors = c("#6D9EC1", "white", "#E46726"),  # nicer palette
  tl.cex = 10,            # bigger text
  tl.srt = 45             # rotate labels
)


## 4.2 Feature Engineering: World Governance Indicators (WGI) ----

#notice how there are a few indicators that are highly correlated: it is "corruption", "gov_effectiveness", "no_violence", "regulatory_quality", "rule_law", "voice_accountability". This makes sense, because all 6 of these are elements of the World Bank's World Governance Indicators (WGI) set. So, let's generalize them and take the standardized average WGI instead:
stand<-c("corruption", "gov_effectiveness", "regulatory_quality", "rule_law", "voice_accountability", "no_violence")
wgi_scaled<-scale(whr[stand])
whr$wgi <- rowMeans(wgi_scaled, na.rm = TRUE)

#drop generosity (its correlated with donations) and the 6 constituents of wgi 
whr$generosity <- NULL
whr$gov_effectiveness <- NULL
whr$corruption <- NULL
whr$regulatory_quality <- NULL
whr$rule_law <- NULL
whr$voice_accountability <- NULL
whr$no_violence <- NULL

## 4.3 Convert Ranking Variables to Percentiles ----

#notice that some variables are just ranked. this is the way the WHR gives out this data, but it inconvenient to use. we can transform these variables
ranking_var <- c(
  "social_support",
  "freedom",
  "positive_emotions",
  "negative_emotions",
  "donated",
  "volunteered",
  "helped_a_stranger"
)

#loop to convert all variables
for (var in ranking_var) {
  x <- as.numeric(whr[[var]]) 
  N <- max(x, na.rm = TRUE) 
  whr[[paste0(var, "_pct")]] <- 1 - ((x - 0.5) / N) #function to convert 
}

for (var in ranking_var) {
  whr[[var]] <- whr[[paste0(var, "_pct")]]  # écrase la colonne originale
  whr[[paste0(var, "_pct")]] <- NULL 
}

## 4.4 Exploratory Visualizations ----

#Let's have a first overview of distributions and correlations
#Let's have a first overview of distributions and correlations
life_evaluation <-ggplot(whr, aes(x = life_evaluation)) +
  geom_histogram(aes(x=life_evaluation, y = ..density..),
                 colour = 1, fill = "white") +
  geom_density()+
  labs(title="Distribution of Life evaluation")

log_GDP <-ggplot(whr, aes(x = log_gdp_per_capita)) +
  geom_histogram(aes(x=log_gdp_per_capita, y = ..density..),
                 colour = 1, fill = "white") +
  geom_density()log_GDP <-ggplot(whr, aes(x = log_gdp_per_capita)) +
  geom_histogram(aes(x=log_gdp_per_capita, y = ..density..),
                 colour = 1, fill = "white") +
  geom_density()+
  labs(title="Distribution of log GDP per capita")

health <-ggplot(whr, aes(x = healthy_life_expectancy)) +
  geom_histogram(aes(x=healthy_life_expectancy, y = ..density..),
                 colour = 1, fill = "white") +
  geom_density()+
  labs(title="healthy life expectancy")

wgi <-ggplot(whr, aes(x = wgi)) +
  geom_histogram(aes(x=wgi, y = ..density..),
                 colour = 1, fill = "white") +
  geom_density()+
  labs(title="Distribution of World Governance Indicator index")

life_evaluation+log_GDP+health+wgi

#Relations of happiness with main variables
GDP<-ggplot(data=whr, aes(x=log_gdp_per_capita,y=life_evaluation,color=region))+
  geom_point() #by GDP

social_supp<-ggplot(data=whr, aes(x=social_support,y=life_evaluation,color=region))+
  geom_point() #by social support

health2<-ggplot(data=whr, aes(x=healthy_life_expectancy,y=life_evaluation,color=region))+
  geom_point() #by health expectancy

GDP+social_supp+health2

#see how distributions differ from one region to another
Life_eval<-ggplot(whr,aes(x=region,y=life_evaluation))+
  geom_boxplot()+
  labs(title="Happiness by region")

GDP_reg<-ggplot(whr,aes(x=region,y=log_gdp_per_capita))+
  geom_boxplot()+
  labs(title="GDP per capita by region")

Life_eval+GDP_reg

# 5. DATA PREPARATION FOR MODELING ----
## 5.1 Custom Hierarchical Imputation Pipeline ----

#there are still a few N/As in the dataset. we will make our imputer use the subregional medians, then the regional medians, and then finally the global medians to impute the datapoints in case the subregional / regional data was not available

PipeOpHierImpute = R6::R6Class(
  "PipeOpHierImpute",
  inherit = PipeOpTaskPreproc,
  
  public = list(
    vars = NULL,   # store your variables here
    
initialize = function(id = "hier_impute") 
  {
      super$initialize(
        id = id,
        param_set = ParamSet$new(list())
      )
    }
  ),
  
  private = list(
    .train_dt = function(dt, levels, target) {
      dt = as.data.table(dt)
      v = self$vars
      
      self$state$sub_m <- dt[, lapply(.SD, median, na.rm = TRUE),
                             by = subregion, .SDcols = v]
      self$state$reg_m <- dt[, lapply(.SD, median, na.rm = TRUE),
                             by = region, .SDcols = v]
      self$state$glob_m <- dt[, lapply(.SD, median, na.rm = TRUE),
                              .SDcols = v]
      
      private$.impute(dt)
    },
    
    .predict_dt = function(dt, levels) {
      dt = as.data.table(dt)
      private$.impute(dt)
    },
    
    .impute = function(dt) {
      v = self$vars
      sub_m = self$state$sub_m
      reg_m = self$state$reg_m
      glob  = self$state$glob_m
      
      for (col in v) {
        idx_sub <- match(dt$subregion, sub_m$subregion)
        sub_val <- sub_m[[col]][idx_sub]
        
        idx_reg <- match(dt$region, reg_m$region)
        reg_val <- reg_m[[col]][idx_reg]
        
        glob_val <- glob[[col]]
        
        dt[[col]] <- dplyr::coalesce(dt[[col]], sub_val, reg_val, glob_val)
      }
      
      dt[, c("region", "subregion") := NULL]
      dt
    }
  )
)

vars_to_impute <- c("healthy_life_expectancy", "household_size", "meals_shared")

po_hier = PipeOpHierImpute$new()
po_hier$vars = vars_to_impute

## 5.2 Define Features and Create Task ----

features <- c("log_gdp_per_capita",
              "healthy_life_expectancy",
              "meals_shared",
              "household_size",
              "wgi",
              "social_support",
              "freedom",
              "positive_emotions",
              "negative_emotions",
              "donated",
              "volunteered",
              "helped_a_stranger",
              "region",
              "subregion"
              )

#create the task
task_whr = as_task_regr(
  whr[, c("life_evaluation", features)],
  target = "life_evaluation",
  id = "whr"
)

# 6. MODEL TRAINING & EVALUATION ----
## 6.1 Train-Test Split Evaluation ----

#linear Model with no CV
set.seed(123)
n = task_whr$nrow
train_idx = sample(seq_len(n), size = 0.75 * n)
test_idx  = setdiff(seq_len(n), train_idx)

train_task = task_whr$clone()$filter(train_idx)
test_task  = task_whr$clone()$filter(test_idx)

lm_split = as_learner(po_hier %>>% lrn("regr.lm"))
lm_split$train(train_task)
pred_lm_split = lm_split$predict(test_task)

measures = msrs(c("regr.rsq", "regr.mse"))
perf_lm_split = pred_lm_split$score(measures)

perf_lm_split

# Baseline model (predicts mean) with no CV
baseline_split <- as_learner(po_hier %>>% lrn("regr.featureless"))
baseline_split$train(train_task)
pred_base_split <- baseline_split$predict(test_task)
perf_base_split <- pred_base_split$score(measures)

# Decision Tree with no CV
tree_split <- as_learner(po_hier %>>% lrn("regr.rpart"))
tree_split$train(train_task)
pred_tree_split <- tree_split$predict(test_task)
perf_tree_split <- pred_tree_split$score(measures)

# Random Forest with no CV
rf_split <- as_learner(po_hier %>>% lrn("regr.ranger", num.trees = 500))
rf_split$train(train_task)
pred_rf_split <- rf_split$predict(test_task)
perf_rf_split <- pred_rf_split$score(measures)

## 6.2 Cross-Validation Evaluation ----

#Introduce Cross Validation
cv10 = rsmp("cv", folds = 10)
measures = msrs(c("regr.rsq", "regr.mse"))

baseline  = as_learner(po_hier %>>% lrn("regr.featureless"))
lm_model  = as_learner(po_hier %>>% lrn("regr.lm"))
tree_model = as_learner(po_hier %>>% lrn("regr.rpart"))
rf_model  = as_learner(po_hier %>>% lrn("regr.ranger", num.trees = 500))

res_base = resample(task_whr, baseline, cv10)
perf_base = res_base$aggregate(measures)

res_lm = resample(task_whr, lm_model, cv10)
perf_lm = res_lm$aggregate(measures)

res_tree = resample(task_whr, tree_model, cv10)
perf_tree = res_tree$aggregate(measures)

res_rf = resample(task_whr, rf_model, cv10)
perf_rf = res_rf$aggregate(measures)

## 6.3 Performance Comparison Table ----

performance_table <- rbind(
  # --- Test Split ---
  Baseline_TestSplit             = perf_base_split,
  Linear_Regression_TestSplit    = perf_lm_split,
  Decision_Tree_TestSplit        = perf_tree_split,
  Random_Forest_TestSplit        = perf_rf_split,
  
  # --- Cross Validation ---
  Baseline_CV                    = perf_base,
  Linear_Regression_CV           = perf_lm,
  Decision_Tree_CV               = perf_tree,
  Random_Forest_CV               = perf_rf
)

performance_table


# 7. HYPERPARAMETER TUNING WITH NESTED RESAMPLING ----


# Turn on parallelization
library(parallel)
cores = detectCores() - 1
plan("multisession", workers = cores)

## 7.1 Define Tuning Space ----
 
# Define tuning parameters for Random Forest
rf_ps = ps(
  mtry = p_int(lower = 1, upper = 12),  # max is number of features
  min.node.size = p_int(lower = 1, upper = 50),
  num.trees = p_int(lower = 100, upper = 1000)
)

## 7.2 Configure Nested Resampling ----

# Define inner and outer resampling
res_inner = rsmp("cv", folds = 4)
res_outer = rsmp("cv", folds = 3)
mes_inner = msr("regr.mse")
terminator = trm("evals", n_evals = 100)
tuner = tnr("random_search")

## 7.3 Create AutoTuner and Run Nested CV ----

# Create AutoTuner for Random Forest
rf_learner = lrn("regr.ranger")
rf_at = AutoTuner$new(
  learner = rf_learner,
  resampling = res_inner,
  measure = mes_inner,
  search_space = rf_ps,
  terminator = terminator,
  tuner = tuner
)

# Combine with imputation pipeline
rf_at_imp = as_learner(po_hier %>>% rf_at)

# Nested resampling for Random Forest
set.seed(123)
nested_res_rf = resample(
  task = task_whr,
  learner = rf_at_imp,
  resampling = res_outer
)

# Get results
cat("\n=== Random Forest Tuned Performance ===\n")
print(nested_res_rf$aggregate(measures))

# Turn off parallelization
plan("sequential")

# 8. VARIABLE IMPORTANCE ----


cat("\n=== Computing Variable Importance ===\n")

# Train final models on entire dataset
lm_final = as_learner(po_hier %>>% lrn("regr.lm"))
rf_final = as_learner(po_hier %>>% lrn("regr.ranger", num.trees = 500))

lm_final$train(task_whr)
rf_final$train(task_whr)

## 8.1 Linear Regression: Coefficient Analysis ----

cat("\n--- Linear Regression Coefficients ---\n")
lm_coefs <- coef(lm_final$model$regr.lm$model)
lm_df <- data.frame(
  Feature = names(lm_coefs),
  Coefficient = lm_coefs
)
lm_df <- lm_df[order(abs(lm_df$Coefficient), decreasing = TRUE), ]
print(lm_df)

# Plot Linear Regression coefficients
lm_plot <- ggplot(lm_df[lm_df$Feature != "(Intercept)", ], 
                  aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Linear Regression Coefficients",
       x = "Feature",
       y = "Coefficient (Beta)") +
  theme_minimal()

print(lm_plot)

##  8.2 Random Forest: Permutation Importance ----
cat("\n--- Random Forest Permutation Importance ---\n")

library(iml)

# Train a simple RF model without the pipeline for iml
# First, we need to prepare the data manually
whr_for_importance <- whr[, c("life_evaluation", features)]

# Remove region/subregion before manual imputation
whr_for_importance$subregion <- NULL
whr_for_importance$region <- NULL

# Manual imputation using global medians
for(var in vars_to_impute) {
  if(var %in% names(whr_for_importance)) {
    med_val <- median(whr_for_importance[[var]], na.rm = TRUE)
    whr_for_importance[[var]][is.na(whr_for_importance[[var]])] <- med_val
  }
}

# Separate X and y
y <- whr_for_importance$life_evaluation
X <- whr_for_importance[, -which(names(whr_for_importance) == "life_evaluation")]

# Train a simple ranger model (without mlr3 wrapper) for iml
library(ranger)
rf_model_iml <- ranger(
  life_evaluation ~ .,
  data = whr_for_importance,
  num.trees = 500,
  importance = "permutation"
)

# Create predictor object with the ranger model
mod_rf <- Predictor$new(
  model = rf_model_iml, 
  data = X, 
  y = y
)

# Compute permutation importance
importance_rf = FeatureImp$new(mod_rf, loss = "mse", n.repetitions = 10)

# Plot results
rf_imp_plot <- importance_rf$plot() +
  labs(title = "Random Forest Permutation Importance") +
  theme_minimal()

print(rf_imp_plot)

# Also print ranger's built-in variable importance for comparison
cat("\n--- Ranger Built-in Variable Importance ---\n")
ranger_imp <- sort(rf_model_iml$variable.importance, decreasing = TRUE)
print(ranger_imp)

