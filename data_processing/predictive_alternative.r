# first, make sure that we are in the correct environment to run the script
script_env <- Sys.getenv("GP_SCRIPT_FOLDER")
if (nchar(script_env) == 0 && !file.exists("environment_setup.r")) {
  stop(
    paste0(
      "To run this script please navigate to the directory that ",
      "contains 'environment_setup.r' or set the GP_SCRIPT_FOLDER environment",
      "variable"
    )
  )
  # load the environment setters
} else if (script_env == "") {
  # check for the local directory
  source(file.path("environment_setup.r"))
} else {
  source(file.path(script_env, "environment_setup.r"))
}

load_lib_quietly("dplyr")
load_lib_quietly("ggplot2")
load_lib_quietly("tibble")
load_lib_quietly("corrplot")
load_lib_quietly("ggcorrplot")
load_lib_quietly("rstanarm") # bayesian linear regression
load_lib_quietly("glmnet") # ridge regression
load_lib_quietly("xtable")

# make sure we have a proper environment for this script
load_environment_if_necessary()
script_dir <- Sys.getenv("GP_SCRIPT_FOLDER")
output_dir <- Sys.getenv("GP_OUTPUT_DATA_FOLDER")
data_dir <- Sys.getenv("GP_INPUT_DATA_FOLDER")
figure_dir <- Sys.getenv("GP_PLOTTING_FOLDER")
table_dir <- ""

source(file.path(script_dir, "experiment_helpers.r"))
source(file.path(script_dir, "aggregate_data_read.r"))
source(file.path(script_dir, "gp_data_read.r"))
source(file.path(script_dir, "behaviourspace_helpers.r"))
source(file.path(script_dir, "bspace_table_exports.r"))
source(file.path(script_dir, "experiments", "synth_pops", "helpers.r"))

# used in file/folder creation
output_name <- "regression"

figure_file <- ""
archive_file <- ""

# if we have a figure output folder, set the script to write to it
if (nchar(figure_dir) != 0) {
  figure_file <- file.path(
    figure_dir, paste0(output_name, "_figures", ".pdf")
  )
  archive_file <- file.path(
    figure_dir,
    "archive",
    paste0(output_name, "_figures_", archive_date(), ".pdf")
  )
  print(paste("Writing figures to:", figure_file))
  pdf(file = figure_file)
  table_dir <- file.path(figure_dir, "tables")
  if (create_folder_if_needed(table_dir)) {
    print(paste0("Writing tables to: ", table_dir))
  }
}

# load in the raw data on all areas
openinfo <- read.csv(
  file.path(data_dir, "marginals", "openinfo-most-recent-data.csv")
)
sb_gp_results <- read.csv(
  file.path(data_dir, "sb_resultaten", "ground_truth_21-01-2025.csv")
)

# filter out/combine the areas in the sample set
openinfo_sb <- load_all_sb_zones(openinfo)

# add the relative opt-in rate (opt-in by household according to GP)
sb_gp_results <- enrich_ground_truth(sb_gp_results)

# check the data for possible outliers
# this looks somewhat normal
print(ggplot(sb_gp_results, aes(x = first_year_percent_opt_in_gp)) +
  geom_dotplot(binwidth = 5))
summary(sb_gp_results$first_year_percent_opt_in_gp)
sd(sb_gp_results$first_year_percent_opt_in_gp)

# we check the consistency of the data
# TODO: fix this check as it is worth talking about
# sanity <- select(openinfo_pca, households, owner, own_percent) |>
#   mutate(adjusted_percent = owner / households * 100) |>
#   mutate(diff = abs(own_percent - adjusted_percent))
# this shows an up to 8 percent difference in recorded percentages and computed
# based on households/owner counts

# do PCA as an exploratory step
# due to the inconsistency in the percentages, drop those from the table
# TODO: get meaningful information from the PCA that can be reported
openinfo_pca <- openinfo_sb |>
  select(everything() & !own_percent & !rent_percent) |>
  select_if(is.numeric)
# this shows that most of the variance in the data set can be explained by a
# combination of only a few variables (what does it tell us really?)
pca_out <- prcomp(openinfo_pca, scale. = TRUE, tol = 0.1, rank = 5)
# assuming that this a valid interpretation of the rotation matrix, it appears
# that the principal component is most strongly influenced by variables that
# are related to income
# The second principal component is most strongly influenced by variables
# relating to vehicles
# The third principal component is most strongly influenced by habitation counts
# (as opposed to business structures), but this variable only accounts for less
# than 10% of the variance
print(summary(pca_out))
print(pca_out)
# Almost all variables are very strongly correlated to household counts, so we
# cherry-pick the top principal components, excluding household counts

# combine the data from the openinfo and GP datasets
combined_set <- left_join(
  openinfo_sb, sb_gp_results,
  by = c("name" = "area_name")
)

# add a column that gives the difference between target households by GP and in
# the OpenInfo dataset
combined_set$household_delta <- abs(
  combined_set$household_count_gp - combined_set$households
)

# normalized combination (eliminate the correlations with the household count)
normalized_set <- combined_set |> mutate(
  across(
    c(b_plus, c_d, e_minus, pre_1970, mid_1970_1999, post_2000),
    ~ .x / households
  )
)

# split the data in train and test sets (zones of interest as test set)
# TODO: this is not necessary, so get rid of this split
zoi_names <- c(
  "Haren", "Lewenborg", "De Hunze van Starkenborgh", "Paddepoel", "De Wijert",
  "Helpman",
  "Korrewegwijk",
  "Oosterhoogebrug-Ulgersmaborg",
  "Oosterpark",
  "Selwerd"
)
train_zones <- normalized_set |> filter(!(name %in% zoi_names))
test_zones <- normalized_set |> filter(name %in% zoi_names)

# analyse correlations in the data set
# print a plot of the correlations in the data
ggcorrplot::ggcorrplot(cor(openinfo_pca))

# learn the regression models using all parameters (naive)
lm_in <- normalized_set |>
  select(everything() & !own_percent & !rent_percent) |>
  select(
    !contains(match = "percent_owner") &
      !contains(match = "rent_") &
      !contains(match = "own_") &
      !contains(match = "opt_in_2") &
      !contains("target") &
      !contains("results")
  ) |>
  select_if(is.numeric) |>
  # drop anything that we still can't find data for
  # TODO: check the ground truth for NA's in the household count
  filter(!is.na(first_year_percent_opt_in_gp) & !is.na(household_count_gp))

# this doesn't work, we need to get rid of the correlates first (demonstrate)
naive_lm <- lm(
  formula = first_year_percent_opt_in_gp ~ ., data = lm_in, na.action = na.omit
)
summary(naive_lm)

print(ggcorrplot::ggcorrplot(cor(lm_in)))
lm_cols <- c(
  # using the counts from GP would invalidate the combinations of openinfo data
  # TODO: consider if it is possible to add the gp_counts or the delta back in
  # "owner_household_count_gp",
  "households",
  "household_delta",
  "Gemiddelde.woningwaarde",
  "b_plus",
  "c_d",
  "e_minus",
  "pre_1970",
  "mid_1970_1999",
  "post_2000",
  "Gemiddeld.inkomen.per.inkomensontvanger",
  "Personenauto.s.per.huishouden",
  "first_year_percent_opt_in_gp"
)
filtered_lm_in <- lm_in |> select({{ lm_cols }})

print(ggcorrplot::ggcorrplot(cor(filtered_lm_in)))

# 'regular' linear regression
filtered_lm <- lm(
  formula = first_year_percent_opt_in_gp ~ .,
  data = filtered_lm_in, na.action = na.omit
)

# Bayesian regression
bayrm <- stan_glm(
  formula = first_year_percent_opt_in_gp ~ .,
  data = filtered_lm_in, na.action = na.omit
)

# ridge regression
# This call generates a family of models with various lambda's,
# the leading error parameter of the method
# alpha determines the penalty type (0 = ridge, 1 = lasso)
# is not final until validated thoroughly
x_ridge <- data.matrix(select(filtered_lm_in, !first_year_percent_opt_in_gp))
y_ridge <- filtered_lm_in$first_year_percent_opt_in_gp
alpha_ridge <- 0
# perform cross-validation to determine the best value of lambda
ridgerm_best <- cv.glmnet(x = x_ridge, y = y_ridge, alpha = alpha_ridge)
# this selects the smallest value of lambda for the final model
# lambda here is the penalty factor applied to the coefficients. The glmnet call
# creates a family of models, and we select the model
best_lambda <- ridgerm_best$lambda.min
best_ridge <- glmnet(
  x = x_ridge, y = y_ridge, alpha = alpha_ridge, lambda = best_lambda
)

# summarise the regression models
print(summary(filtered_lm))
print(summary(bayrm))
# this summary does not seem to give much
print(summary(best_ridge))

# perform predictions on posterior using the Bayesian model
# this produces distributions
bay_preds <- posterior_predict(bayrm, test_zones)
plot(as.data.frame(bay_preds) |>
  pivot_longer(everything()) |>
  ggplot(aes(x = value, color = name)) +
  # TODO: add the means as verticals
  # TODO: add the zones as names
  geom_density()) + scale_fill_discrete(test_zones$name)

# try another dataset for linear modeling
lm_2_cols <- c(
  "owner_household_count_gp",
  "e_minus",
  "first_year_percent_opt_in_gp",
  "Gemiddeld.inkomen.per.inkomensontvanger"
)
lm_in_two <- lm_in |> select({{ lm_2_cols }})

print(ggcorrplot(cor(lm_in_two)))
small_lm <- lm(
  formula = first_year_percent_opt_in_gp ~ ., data = lm_in_two, na.action = na.omit
)
print("Smaller linear model")
print(summary(small_lm))

print("Best linear model (in terms of predictions)")
print(summary(filtered_lm))

train_lm_in <- train_zones |> select({{ lm_cols }})
test_lm_in <- test_zones |> select({{ lm_cols }})

# benchmark model to demonstrate effect of removing zoi from the training set
bench_lm <- lm(
  formula = first_year_percent_opt_in_gp ~ ., data = train_lm_in, na.action = na.omit
)

# generate the predictions of all the models
test_zones$lm_predictions <- predict.lm(bench_lm, test_zones)
test_zones$full_lm_predictions <- predict.lm(filtered_lm, test_zones)
test_zones$small_lm_predictions <- predict.lm(small_lm, test_zones)
test_zones$bayes_means_preds <- apply(bay_preds, 2, mean)
test_zones$ridge_preds <- predict(
  best_ridge,
  s = best_lambda, newx = data.matrix(
    select(test_lm_in, !first_year_percent_opt_in_gp)
  )
)

# compare the predictive outcomes and the true outcomes
true_ratios <- test_zones$first_year_percent_opt_in_gp
lm_cor_list <- list(
  cor.test(true_ratios, test_zones$lm_predictions, method = "spearman"),
  cor.test(true_ratios, test_zones$full_lm_predictions, method = "spearman"),
  cor.test(true_ratios, test_zones$small_lm_predictions, method = "spearman"),
  cor.test(true_ratios, test_zones$bayes_means_preds, method = "spearman"),
  cor.test(true_ratios, test_zones$ridge_preds, method = "spearman")
)
lm_cors_table <- combine_correlation_results(lm_cor_list)

# test regular correlation (for completeness)
lm_cor_list_alt <- list(
  cor.test(true_ratios, test_zones$lm_predictions, method = "pearson"),
  cor.test(true_ratios, test_zones$full_lm_predictions, method = "pearson"),
  cor.test(true_ratios, test_zones$small_lm_predictions, method = "pearson"),
  cor.test(true_ratios, test_zones$bayes_means_preds, method = "pearson"),
  cor.test(true_ratios, test_zones$ridge_preds, method = "pearson")
)
lm_cors_table_alt <- combine_correlation_results(lm_cor_list_alt)

# tables for nl_pres (skip the benchmark, it is irrelevant)
lm_cors_table_nl <- lm_cors_table[-1, ]
lm_cors_table_nl$methode <- c(
  "Groot lineair model",
  "Klein lineair model",
  "Bayesiaanse regressie",
  "`Ridge' regressie"
)
RMSE <- function(residuals) {
  sqrt(mean(residuals^2))
}
# add RMSE for regression models
# TODO: do this for all models in regular table
lm_cors_table_nl$RMSE <- c(
  RMSE(filtered_lm$residuals),
  RMSE(small_lm$residuals),
  RMSE(true_ratios - test_zones$bayes_means_preds),
  RMSE(true_ratios - test_zones$ridge_preds)
)

# dutch pred. correlation table for presentation
print(
  xtable(
    lm_cors_table_nl |> select(methode, estimate, p_value, RMSE),
    caption = paste0(
      "Resultaten correlatie testen regressie modellen."
    ),
    label = paste0("tbl:lm_cors_sim_preds")
  ),
  include.rownames = FALSE,
  file = file.path(table_dir, "lm_cors_sim_preds_nl.tex")
)

# dutch predictions table for presentation
print(
  xtable(
    test_zones |>
      select(
        name, first_year_percent_opt_in_gp, full_lm_predictions,
        small_lm_predictions, bayes_means_preds, ridge_preds
      ) |>
      arrange(desc(first_year_percent_opt_in_gp)) |>
      mutate(name = abbreviate(name, minlength = 7)) |>
      rename("Behaald \\%" = first_year_percent_opt_in_gp) |>
      rename(
        "Groot LM" = full_lm_predictions,
        "Klein LM" = small_lm_predictions,
        "Bayes." = bayes_means_preds,
        "Ridge" = ridge_preds
      ),
    caption = paste0(
      "Voorspellingen van de regressie modellen naast de behaalde resultaten. "
    ),
    label = paste0("tbl:lm_preds_nl")
  ),
  include.rownames = FALSE, file = file.path(table_dir, "lm_preds_nl.tex"),
  sanitize.colnames.function = identity
)

# results of correlation testing on the predictions and the ground truth for all
# linear models
# tables for nl_pres (skip the benchmark, it is irrelevant)
lm_cors_table_filter <- lm_cors_table[-1, ]
lm_cors_table_filter$method <- c(
  "Large linear model",
  "Small linear model",
  "Bayesian regression",
  "Ridge regression"
)
lm_cors_table_filter$RMSE <- c(
  RMSE(filtered_lm$residuals),
  RMSE(small_lm$residuals),
  RMSE(true_ratios - test_zones$bayes_means_preds),
  RMSE(true_ratios - test_zones$ridge_preds)
)
print(
  xtable(
    lm_cors_table_filter |> select(method, estimate, p_value, RMSE) |> rename(
      "Spearman's $\\rho$" = estimate,
      "$p$-value" = p_value
    ),
    caption = paste0(
      "Results of correlation test using Spearman's $\\rho$ (estimate). ",
      "Regression target is ground truth opt-in ratios for the zones of ",
      "interest. Correlations are computed between the ground truth and ",
      " the predictions generated by the various (linear) regression models.",
      " Residuals are also given as root mean square error."
    ),
    label = paste0("tbl:lm_cors_sim_preds")
  ),
  include.rownames = FALSE,
  file = file.path(table_dir, "lm_cors_sim_preds.tex"), sanitize.colnames.function = identity
)

# generate tables from the results
print(
  xtable(
    test_zones |>
      select(
        name, first_year_percent_opt_in_gp, full_lm_predictions
      ) |>
      arrange(desc(first_year_percent_opt_in_gp)) |>
      rename("Opt-in \\%\\footnotemark{}" = first_year_percent_opt_in_gp) |>
      rename("Opt-in \\% (best LM)" = full_lm_predictions),
    caption = paste0(
      "Predictions from the best linear regression model side by ",
      "side with the ground truth."
    ),
    label = paste0("tbl:lm_preds")
  ),
  include.rownames = FALSE, file = file.path(table_dir, "lm_preds.tex"),
  sanitize.colnames.function = identity
)

# this footnote goes with the table above, but has to be printed separately
cat(
  paste0(
    "\\footnotetext{The opt-in \\% is the number of realized measures ",
    "proportional to the number of households in an area as measured by GP. ",
    "I.e. 1\\% is equivalent to one realized measure per one hundred ",
    "households.}"
  ),
  file = file.path(table_dir, "lm_preds.tex"),
  append = TRUE
)

# print the predictions from the Bayesian and ridge regression models
print(
  xtable(
    test_zones |>
      select(
        name, first_year_percent_opt_in_gp, bayes_means_preds, ridge_preds
      ) |>
      arrange(desc(first_year_percent_opt_in_gp)) |>
      rename(
        "Opt-in \\%" = first_year_percent_opt_in_gp,
        "Bayesian pred." = bayes_means_preds,
        "Ridge pred." = ridge_preds
      ),
    caption = paste0(
      "Predictions from the Bayesian and ridge regression models side by ",
      "side with the ground truth."
    ),
    label = paste0("tbl:bay-rdg-rm")
  ),
  include.rownames = FALSE,
  file = file.path(table_dir, "bayes_ridge_preds.tex"),
  sanitize.colnames.function = identity
)

# TODO: get an expected ordering from the experts

# TODO: set the levels of the local initiative factor

# if we have a figure output folder, set the script to write to it
if (nchar(figure_dir) != 0) {
  print(paste("Wrote figures to:", figure_dir))
  dev.off()
  file.copy(figure_file, archive_file, overwrite = TRUE)
  print(paste0("Wrote tables to: ", table_dir))
}
