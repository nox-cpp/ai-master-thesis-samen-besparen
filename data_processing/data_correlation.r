require(DescTools)

# perform the summary for bouwjaar and energielabel
categorical_correlation_summary <- function(x, y) {
    print(table(x, y))
    # calculate chi-squares for all the categorical data
    print(chisq.test(x, y))
    # calculate thiels u for all the categorical data
    # NOTE: The r documentation lies, the default for UncertCoef is the symmetrical U
    print(paste("Theil's U (uncertainty coefficient): ", UncertCoef(x, y, direction = "row")))
    print(paste("Theil's U for reverse case: ", UncertCoef(y, x, direction = "row")))
    # calculate Spearman's rho
    print(paste("Spearmans rho: ", SpearmanRho(table(x, y))))
    # calculate Kendall's tau
    print(paste("Kendall's tau: ", KendallTauA(x, y)))
}

# writes a correlation summary of selected variables from the dataset
correlation_summary_on_dataset <- function(dataset) {
  print("build year and energy label")
  categorical_correlation_summary(dataset$bouwjaar, dataset$energielabel)
  print("tenancy status and energy label")
  categorical_correlation_summary(dataset$woonsituatie, dataset$energielabel)
  print("present interventions and energy label")
  categorical_correlation_summary(dataset$aanwezigheid_duurzame_interventies, dataset$energielabel)
  # NOTE: there is no shared data between houding and energielabel, hence why there can no be correlation computation
  # categorical_correlation_summary(result_targets$houding_verduurzaming, result_targets$energielabel)
  print("attitude to sustainability and tenancy status")
  categorical_correlation_summary(dataset$houding_verduurzaming, dataset$woonsituatie)
}