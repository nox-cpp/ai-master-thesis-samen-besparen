load_lib_quietly("dplyr")

# formats a delta table to have signs in the table cells (except the first)
# NOTE: this converts most numerics to string values
format_delta_table <- function(delta_summary) {
  formatted <- delta_summary[1, ] |> mutate(across(where(is.numeric), ~ format(.x, digits = 2, justified = "none")))
  formatted <- bind_rows(formatted, delta_summary[-1, ] |>
    mutate(
      across(
        where(is.numeric),
        ~ formatC(round(.x, 3), digits = 2, width = 0, flag = "+-")
      )
    ))
  formatted
}

# formats a delta summary to have signs in the table cells (except the first)
# NOTE: this converts most numerics to string values
format_delta_summary <- function(delta_summary) {
  lapply(delta_summary, format_delta_table)
}

# prints a generated summary as tex input
print_summary <- function(summary) {
  lapply(summary, function(x) {
    print(xtable(x), include.rownames = FALSE)
  })
}

# prints the behaviour space result summary in latex form with labels and
# captions
# By default, writes to stdout. If <file> is given, writes to file. If <append>
# is false, clears <file> prior to adding the tables.
# If <dir> is supplied, file is ignored and tables are instead written to
# separate table files in that directory, with the <file_name_prefix> prepended
# to the table file name.
# <float> may be set to determine whether or not the table is wrapped in a float
# object (LaTeX)
# NOTE: this only works for summaries in list form with valid labels
print_labeled_summary <- function(
    summary, sub_name = "std", file = "", append = FALSE,
    dir = "", file_name_prefix = sub_name, float = TRUE) {
  # local function for file paths
  tbl_path <- function(tbl_name) {
    file.path(dir, paste0(file_name_prefix, "_", tbl_name, ".tex"))
  }

  # first, collect all tables
  xtables <- list()
  file_names <- list()
  add_tbl_path <- function(tbl_name) {
    file_names <<- append(file_names, tbl_path(tbl_name))
  }
  if (!is.null(summary[["agent-choice"]])) {
    xtables <- append(
      xtables,
      list(format_choice_table(summary[["agent-choice"]], sub_name))
    )
    add_tbl_path("choice")
  }
  if (!is.null(summary[["communications"]])) {
    xtables <- append(
      xtables,
      list(format_comms_table(summary[["communications"]], sub_name))
    )
    add_tbl_path("comms")
  }
  if (!is.null(summary[["stabilization"]])) {
    xtables <- append(
      xtables,
      list(format_stabilization_table(summary[["stabilization"]], sub_name))
    )
    add_tbl_path("stab")
  }
  if (!is.null(summary[["agent-satisfaction"]])) {
    xtables <- append(
      xtables,
      list(format_satisfaction_table(summary[["agent-satisfaction"]], sub_name))
    )
    add_tbl_path("sats")
  }
  if (!is.null(summary[["variance"]])) {
    xtables <- append(
      xtables,
      list(format_variance_table(summary[["variance"]], sub_name)),
    )
    add_tbl_path("var")
  }
  if (!is.null(summary[["group-choice-A"]])) {
    xtables <- append(
      xtables,
      list(format_grp_choice_a_table(summary[["group-choice-A"]], sub_name))
    )
    add_tbl_path("ch-a")
  }
  if (!is.null(summary[["group-choice-B"]])) {
    xtables <- append(
      xtables,
      list(format_grp_choice_b_table(summary[["group-choice-B"]], sub_name))
    )
    add_tbl_path("ch-b")
  }
  if (!is.null(summary[["comms-group"]])) {
    xtables <- append(
      xtables,
      list(format_grp_comms_table(summary[["comms-group"]], sub_name))
    )
    add_tbl_path("grp-comms")
  }

  # then, print all tables according to the proper settings
  if (dir == "") {
    # normal print
    if (file != "" & !append) {
      # clear the file if necessary
      cat("", file = file)
    }
    outputs <- lapply(
      xtables,
      function(table, file) {
        print(table,
          include.rownames = FALSE, file = file, append = TRUE,
          # we set the colnames sanitizer to the identity, as we have already
          # provided suitable labels
          sanitize.colnames.function = identity, floating = float
        )
      }, file
    )
  } else {
    # print to separate files
    outputs <- mapply(function(table, path) {
      print(
        table,
        include.rownames = FALSE,
        file = path,
        sanitize.colnames.function = identity, floating = float
      )
    }, xtables, file_names)
  }

  # return the outputs for debugging purposes, but don't display
  invisible(outputs)
}

# NOTE: All xtable markups contain latex math characters, and are pre-sanitized
# Disable sanitization when printing these tables
format_choice_table <- function(table, sub_name = "") {
  table <- display_names_choice(table)

  test <- xtable(table,
    caption = paste0(
      "Agent-choice (", sub_name, "). Results are given as ",
      "ratios of the total population, averaged across runs. `Convinced' ",
      "humats prefer opting in and experience no dilemma. $S'(x)$ is the ",
      "humat's expected satisfaction of their current alternative."
    ),
    label = paste0("tbl:hch-", sub_name)
  )

  test
}

format_comms_table <- function(table, sub_name = "") {
  table <- display_names_communication(table)
  xtable(table,
    caption = paste0(
      "Agent communications (", sub_name, "). Results given ",
      "as the number of communications per agent per time-step, averaged ",
      "across runs."
    ),
    label = paste0("tbl:hcc-", sub_name)
  )
}

format_stabilization_table <- function(table, sub_name = "") {
  table <- display_names_convergence(table)
  xtable(table,
    caption = paste0(
      "Stabilization (", sub_name, "). Ratio of simulation runs that ",
      "converged before reaching the maximum step count."
    ),
    label = paste0("tbl:hcs-", sub_name)
  )
}
format_satisfaction_table <- function(table, sub_name = "") {
  table <- display_names_satisfaction(table)
  xtable(table,
    caption = paste0(
      "Agent satisfaction (", sub_name, "). Results given as average ",
      "value in the population, averaged across runs. $E$ is the ",
      "humat evaluation of their preferred alternative. $S'$ is the ",
      "normalized expected satisfaction of their preferred alternative.",
      "(A) and (B) indicate subsets of the humat population that prefer ",
      "opting in and out respectively."
    ),
    label = paste0("tbl:hcsat-", sub_name)
  )
}

format_grp_choice_a_table <- function(table, sub_name = "") {
  # shorten the long names
  table <- display_names_group_choice_a(table)
  xtable(table,
    caption = paste0(
      "Group A (opt-in) choice (", sub_name, "). Ratios of population with a ",
      "given relative expected satisfaction for group A. High values ",
      " exceed 0.5, low values are below -0.5."
    ),
    label = paste0("tbl:hcgca-", sub_name)
  )
}
format_grp_choice_b_table <- function(table, sub_name = "") {
  # shorten the long names
  table <-
    display_names_group_choice_b(table)

  xtable(table,
    caption = paste0(
      "Group B (opt-out) choice (", sub_name, ") Ratios of population with a ",
      "given relative expected satisfaction for group B. High values ",
      " exceed 0.5, low values are below -0.5."
    ),
    label = paste0("tbl:hcgcb-", sub_name)
  )
}

format_grp_comms_table <- function(table, sub_name = "") {
  # shorten the long names
  table <-
    display_names_comms_group(table)
  xtable(table,
    caption = paste0(
      "Communications by group (A/B, i.e. opt-in/opt-out) (", sub_name,
      "). Results given ",
      "as the number of communications per agent per time-step, averaged ",
      "across runs."
    ),
    label = paste0("tbl:hccg-", sub_name)
  )
}

format_variance_table <- function(table, sub_name = "") {
  table <- display_names_variances(table)
  xtable(table,
    caption = paste0(
      "Variances (", sub_name, "). Standard deviations (SD) are computed for ",
      "key statistics across run ends (final world states). Unless ",
      "otherwise indicated the metrics are ratios relative to the size of the ",
      "population and standard deviations are reported for those."
    ),
    label = paste0("tbl:hcv-", sub_name)
  )
}

# add columns to behavior space data for the purposes of evaluating predictions
add_prediction_columns <- function(bspace) {
  bspace |>
    mutate(rel_opt_in = Optin.Count / Household.Count) |>
    mutate(rel_convs = Fully.Convinced.Count / Household.Count) |>
    mutate(percent_opt_in = rel_opt_in * 100) |>
    mutate(percent_convs = rel_convs * 100)
}

# TODO: run development summary (i.e. state over time summarized)
# TODO: add time to stabilization (where relevant)

combine_correlation_results <- function(correlations) {
  data.frame(
    name = get_model_params(correlations, "data.name"),
    estimate = get_model_params(correlations, "estimate"),
    p_value = get_model_params(correlations, "p.value"),
    statistic = get_model_params(correlations, "statistic")
  )
}

# changes the column names for choice summary tables
display_names_choice <- function(choice_tbl) {
  # subset the labels we want to adapt
  knowns <- c(
    "avg_opt_in", "avg_trdill", "avg_pos", "avg_conv"
  )
  target_cols <- colnames(choice_tbl) %in% knowns

  # pre-sanitize anything that we don't account for
  colnames(choice_tbl)[!target_cols] <-
    sanitize(colnames(choice_tbl)[!target_cols])

  # replace everything we want
  choice_tbl <- rename(choice_tbl,
    "Prefers A (opt-in)" = avg_opt_in, "Dilemma" = avg_trdill,
    "Positive $S'(x)$" = avg_pos, "Convinced" = avg_conv
  )

  choice_tbl
}

display_names_communication <- function(comms_tbl) {
  # subset the labels we want to adapt
  knowns <- c("avg_comms", "avg_sigs", "avg_inqs")
  target_cols <- colnames(comms_tbl) %in% knowns

  # pre-sanitize anything that we don't account for
  colnames(comms_tbl)[!target_cols] <-
    sanitize(colnames(comms_tbl)[!target_cols])

  # replace everything we want
  comms_tbl <- rename(comms_tbl,
    "Communications" = avg_comms, "Signals" = avg_sigs, "Inquiries" = avg_inqs,
  )

  comms_tbl
}

display_names_convergence <- function(table) {
  # subset the labels we want to adapt
  knowns <- c("cvg_rate")
  target_cols <- colnames(table) %in% knowns

  # pre-sanitize anything that we don't account for
  colnames(table)[!target_cols] <-
    sanitize(colnames(table)[!target_cols])

  # replace everything we want
  table <- rename(table, "Convergence rate" = cvg_rate)

  table
}

display_names_satisfaction <- function(table) {
  # subset the labels we want to adapt
  knowns <- c(
    "avg_eval", "avg_exps", "avg_eval_a",
    "avg_exps_a", "avg_eval_b", "avg_exps_b"
  )
  target_cols <- colnames(table) %in% knowns

  # pre-sanitize anything that we don't account for
  colnames(table)[!target_cols] <-
    sanitize(colnames(table)[!target_cols])

  # replace everything we want
  table <- rename(table,
    "$E$" = avg_eval, "$S'$" = avg_exps,
    "$E$ (A)" = avg_eval_a, "$S'$ (A)" = avg_exps_a,
    "$E$ (B)" = avg_eval_b, "$S'$ (B)" = avg_exps_b
  )

  table
}

display_names_group_choice_a <- function(table) {
  # subset the labels we want to adapt
  knowns <- c(
    "Positive.Satisfaction.Count.A",
    "High.Satisfaction.Count.A",
    "Low.Satisfaction.Count.A",
    "Fully.Convinced.Count.A"
  )
  target_cols <- colnames(table) %in% knowns

  # pre-sanitize anything that we don't account for
  colnames(table)[!target_cols] <-
    sanitize(colnames(table)[!target_cols])

  # replace everything we want
  table <- rename(table,
    "Positive $S'(A)$" = Positive.Satisfaction.Count.A,
    "High $S'(A)$" = High.Satisfaction.Count.A,
    "Low $S'(A)$" = Low.Satisfaction.Count.A,
    "Fully convinced" = Fully.Convinced.Count.A
  )

  table
}

display_names_group_choice_b <- function(table) {
  # subset the labels we want to adapt
  knowns <- c(
    "Positive.Satisfaction.Count.B",
    "High.Satisfaction.Count.B",
    "Low.Satisfaction.Count.B"
  )
  target_cols <- colnames(table) %in% knowns

  # pre-sanitize anything that we don't account for
  colnames(table)[!target_cols] <-
    sanitize(colnames(table)[!target_cols])

  # replace everything we want
  table <- rename(table,
    "Positive $S'(B)$" = Positive.Satisfaction.Count.B,
    "High $S'(B)$" = High.Satisfaction.Count.B,
    "Low $S'(B)$" = Low.Satisfaction.Count.B
  )

  table
}

display_names_comms_group <- function(table) {
  # subset the labels we want to adapt
  knowns <- c("avg_sigs_a", "avg_sigs_b", "avg_inqs_a", "avg_inqs_b")
  target_cols <- colnames(table) %in% knowns

  # pre-sanitize anything that we don't account for
  colnames(table)[!target_cols] <-
    sanitize(colnames(table)[!target_cols])

  # replace everything we want
  table <- rename(table,
    "Signals (A)" = avg_sigs_a,
    "Signals (B)" = avg_sigs_b,
    "Inquiries (A)" = avg_inqs_a,
    "Inquiries (B)" = avg_inqs_b
  )

  table
}

display_names_variances <- function(table) {
  # subset the labels we want to adapt
  knowns <- c(
    "Optin.Count_var",
    "true_dill_count_var",
    "Fully.Convinced.Count_var",
    "comms_per_epoch_var",
    "Positive.Satisfaction.Count_var"
  )
  target_cols <- colnames(table) %in% knowns

  # pre-sanitize anything that we don't account for
  colnames(table)[!target_cols] <-
    sanitize(colnames(table)[!target_cols])

  # replace everything we want
  table <- rename(table,
    "Prefers A" = Optin.Count_var,
    "Dilemma" = true_dill_count_var,
    "F.Conv." = Fully.Convinced.Count_var,
    "Comm. rate" = comms_per_epoch_var,
    "Pos. $S'(A)$" = Positive.Satisfaction.Count_var,
  )

  table
}
