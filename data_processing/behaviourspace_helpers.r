# contains functions that help with reading and processing the data as it is
# found in the output of behaviourspace experiments in NetLogo

load_lib_quietly("dplyr")
load_lib_quietly("ggplot2")

# Reads the behaviourspace data file into a dataframe
read_behaviourspace_data <- function(file_path, num_rows = -1) {
  read.csv(file_path, header = TRUE, skip = 6, nrows = num_rows)
}

#
pop_name_from_paths <- function(paths) {
  # we look for the file name (end of path) which contains word characters but
  # can also contain dashes (not captured by the \w class)
  regex_patt <- ".*/([\\w\\+\\-\\ ]*)\\.csv"
  # add a pop name column
  sub(
    pattern = regex_patt,
    replacement = "\\1",
    paths,
    perl = TRUE
  )
}

# adds a population name column to the data for experiments parameterized by
pop_name <- function(bspace) {
  pop_name_from_paths(bspace$agent.file.path)
}

# adds a column that decodes the population name to get the main experiment
# condition in the mapping space experiment. These conditions correspond to
# subexperiments
main_condition <- function(bspace) {
  # aspiration_mapping
  bspace <- bspace |>
    # does not look great but runs fast
    mutate(
      condition =
        ifelse(startsWith(pop_name, "imp"), "imp",
          ifelse(startsWith(pop_name, "asp"), "asp",
            ifelse(startsWith(pop_name, "baseline"), "baseline",
              ifelse(startsWith(pop_name, "var"), "var_incl",
                ifelse(
                  startsWith(pop_name, "most_pess") |
                    startsWith(pop_name, "low_ret") |
                    startsWith(pop_name, "wealth"), "fin_map",
                  ifelse(
                    startsWith(pop_name, "optimistic") |
                      startsWith(pop_name, "diminishing"),
                    "comf_map",
                    ifelse(
                      startsWith(pop_name, "drastic") |
                        startsWith(pop_name, "idealistic") |
                        startsWith(pop_name, "relative"),
                      "value_map", NA
                    )
                  )
                )
              )
            )
          )
        )
    )
  as.factor(bspace$condition)
}

# returns a subset of behaviorspace data that corresponds to the end of the runs
subset_run_ends <- function(bspace) {
  bspace |>
    # we keep any pre-existing groups
    group_by(X.run.number., .add = TRUE) |>
    slice_max(order_by = X.step.)
}

# Returns a subset of behaviorspace data that corresponds to the end of the runs
# Accounts for duplicate run numbers when combining data from multiple experiments
# Assumes that the same duplication is not used in multiple experiments
subset_run_ends_multi_exp <- function(bspace) {
  bspace |>
    mutate(run_pop_comb = paste0(X.run.number., pop_name)) |>
    # we keep any pre-existing groups
    group_by(run_pop_comb, .add = TRUE) |>
    slice_max(order_by = X.step.)
}

# tries to decode the population name to find the experiment setup parameters
# results in NA for model runs outside of the importance distribution subexperiment
decode_imp_setup <- function(bspace) {
  # importance experiment parameter capture
  imp_patt <- "[a-z]{2,3}_"
  imp_capture <- "([a-z]{2,3})"
  imp_prefix <- "imp_map_"
  n_imp_patts <- function(n) {
    paste0(rep(imp_patt, n), collapse = "")
  }
  # skips n parameters and captures the next
  regex_target <- function(n = 0) {
    paste0(imp_prefix, n_imp_patts(n), imp_capture, ".*")
  }
  # full gsub call in wrapper
  gsub_wrap <- function(n = 0) {
    gsub(regex_target(n), replacement = "\\1", bspace$pop_name, perl = TRUE)
  }

  # decode the setup parameters from the name and add to the data
  bspace$mu_f <- gsub_wrap(0)
  bspace$sig_f <- gsub_wrap(1)
  bspace$mu_c <- gsub_wrap(2)
  bspace$sig_c <- gsub_wrap(3)
  bspace$mu_v <- gsub_wrap(4)
  bspace$sig_v <- gsub_wrap(5)
  bspace$mu_s <- gsub_wrap(6)
  bspace$sig_s <- gsub_wrap(7)

  bspace
}

decode_asp_setup <- function(bspace) {
  # importance experiment parameter capture
  asp_patt <- "[a-z]{2,3}_"
  asp_capture <- "([a-z]{2,3})"
  asp_prefix <- "asp_map_"
  n_asp_patts <- function(n) {
    paste0(rep(asp_patt, n), collapse = "")
  }
  # skips n parameters and captures the next
  regex_target <- function(n = 0) {
    paste0(asp_prefix, n_asp_patts(n), asp_capture, ".*")
  }
  # full gsub call in wrapper
  gsub_wrap <- function(n = 0) {
    gsub(regex_target(n), replacement = "\\1", bspace$pop_name, perl = TRUE)
  }

  # decode the setup parameters from the name and add to the data
  bspace$mu_a <- gsub_wrap(0)
  bspace$sig_a <- gsub_wrap(1)

  bspace
}

# add a column to the behavior space data to differentiate between population
# generation methods used (if any)
decode_pop_setup <- function(bspace) {
  # zone/pop synth experiment condition parameter capture
  setup_capture_class <- "[a-zA-Z0-9\\s-]"
  setup_word_capture <- "[a-zA-Z0-9\\s-]*"
  mapping_capture <- paste0("^(", setup_capture_class, "*)_.*")
  # these regexes are strongly tied to the expected input
  zone_capture <- paste0(
    # skip the first word
    "^", setup_word_capture,
    "_(", setup_word_capture, ")_.*"
  )
  type_capture <- paste0(
    # skip first two words
    "^(?:", setup_word_capture, "_){2}",
    # capture the third
    "(", setup_word_capture, ").*"
  )
  gsub_wrap <- function(target) {
    gsub(target, "\\1", bspace$pop_name, perl = TRUE)
  }

  # decode the setup parameters from the name and add to the data
  bspace$mapping <- gsub_wrap(mapping_capture)
  bspace$zone <- gsub_wrap(zone_capture)
  bspace$gen_type <- gsub_wrap(type_capture)

  bspace
}

# get n run samples for each unique population
sample_condition_runs <- function(importance_space, condition_var = "pop_name", n = 1) {
  lapply(unique(importance_space[[condition_var]]), function(x) sample(importance_space$X.run.number.[importance_space[[condition_var]] == x], n))
}

# get n run samples for each unique population
sample_population_runs <- function(bspace, n = 1) {
  sample_condition_runs(bspace, "pop_name", n = 1)
}

# get a wide form (variable-per-time-stamp) of the data where each row is a run
# and the columns correspond to time-steps
bspace_wide <- function(bspace, variable_name) {
  pivot_wider(bspace[bspace$ui.campaign.enabled == "true", ], id_cols = "X.run.number.", names_from = "X.step.", values_from = variable_name, names_prefix = "step_")
}

# creates a dataset that combines each agent in the output (world_state)
# population with variables of the model initialization
cross_ref_pop_bspace <- function(bspace, population) {
  # cross-reference the populations with the model run data for source pops
  reference_data <- bspace |> select(X.run.number., agent.file.path, ui.campaign.enabled)
  left_join(population, reference_data, by = c("run_nr" = "X.run.number."))
}

## ggplot primitives
## NOTE: *_base functions are plot objects without a geom, just aesthetics
# Most of these plots track change over time (i.e. x = step)
optin_count_plot_base <- function(bspace) {
  bspace |> ggplot(aes(x = X.step., y = Optin.Count))
}

optin_ratio_plot_base <- function(bspace) {
  bspace |> ggplot(aes(x = X.step., y = Optin.Count / Household.Count))
}

dilemma_count_plot <- function(bspace) {
  bspace |> ggplot(aes(x = X.step.)) +
    geom_line(aes(linetype = "dotted", y = True.Non.Social.Dilemma.Count + True.Social.Dilemma.Count + True.Dual.Dilemma.Count)) +
    geom_line(aes(linetype = "solid", y = Non.Dilemma.Count))
}

dilemma_ratio_plot <- function(bspace) {
  bspace |> ggplot(aes(x = X.step.)) +
    geom_line(aes(linetype = "dotted", y = (True.Non.Social.Dilemma.Count + True.Social.Dilemma.Count + True.Dual.Dilemma.Count) / Household.Count)) +
    geom_line(aes(linetype = "solid", y = Non.Dilemma.Count / Household.Count))
}

# choice ratio breakdowns
choice_ratios <- function(run_ends) {
  run_ends |>
    summarise(
      avg_opt_in = mean(Optin.Count / Household.Count),
      avg_dill = mean(1 - (Non.Dilemma.Count / Household.Count)),
      avg_trdill = mean((True.Social.Dilemma.Count + True.Non.Social.Dilemma.Count + True.Dual.Dilemma.Count) / Household.Count),
      avg_pos = mean(Positive.Satisfaction.Count / Household.Count),
      avg_high = mean(High.Satisfaction.Count / Household.Count),
      avg_low = mean(Low.Satisfaction.Count / Household.Count),
      avg_conv = mean(Fully.Convinced.Count / Household.Count)
      # this stat is just zero across the board
      # ,avg_alms = mean(Almost.Convinced.Count / Household.Count)
    )
}

# alternative population choice summary, more concise
choice_ratios_alt <- function(run_ends) {
  run_ends |>
    summarise(
      avg_opt_in = mean(Optin.Count / Household.Count),
      avg_trdill = mean((True.Social.Dilemma.Count + True.Non.Social.Dilemma.Count + True.Dual.Dilemma.Count) / Household.Count),
      avg_pos = mean(Positive.Satisfaction.Count / Household.Count),
      avg_conv = mean(Fully.Convinced.Count / Household.Count)
    )
}

choice_ratios_A <- function(run_ends) {
  run_ends |>
    summarise(
      # prevent division by zero for empty groups
      across(
        c(
          Positive.Satisfaction.Count.A,
          High.Satisfaction.Count.A,
          Low.Satisfaction.Count.A,
          Fully.Convinced.Count.A,
        ),
        ~ mean(ifelse(Optin.Count > 0, .x / Optin.Count, 0))
      )
    )
}

choice_ratios_B <- function(run_ends) {
  run_ends |> summarise(
    across(
      c(
        Positive.Satisfaction.Count.B,
        High.Satisfaction.Count.B,
        Low.Satisfaction.Count.B
        # these are zero by definition, not good reporters
        # Fully.Convinced.Count.B,
        # Almost.Convinced.Count.B,
        # Partially.Convinced.Count.B
      ),
      ~ mean(ifelse(Household.Count - Optin.Count > 0, .x / (Household.Count - Optin.Count), 0))
    )
  )
}

# communication rates
comm_ratios <- function(run_ends) {
  run_ends |>
    summarise(
      avg_comms = mean(Total.Communications / Household.Count / X.step.),
      avg_sigs = mean(Total.Signals / Household.Count / X.step.),
      avg_inqs = mean(Total.Inquiries / Household.Count / X.step.),
      avg_sigs_a = mean(Total.Signals.A / Optin.Count / X.step.),
      avg_sigs_b = mean(Total.Signals.B / (Household.Count - Optin.Count) / X.step.),
      avg_inqs_a = mean(Total.Inquiries.A / Optin.Count / X.step.),
      avg_inqs_b = mean(Total.Inquiries.B / (Household.Count - Optin.Count) / X.step.)
    )
}

# concise summary of communication
comm_ratios_alt <- function(run_ends) {
  run_ends |>
    summarise(
      avg_comms = mean(Total.Communications / Household.Count / X.step.),
      avg_sigs = mean(Total.Signals / Household.Count / X.step.),
      avg_inqs = mean(Total.Inquiries / Household.Count / X.step.),
    )
}

# summary of communications by group
comm_ratios_by_group <- function(run_ends) {
  run_ends |> summarise(
    avg_sigs_a = mean(Total.Signals.A / Optin.Count / X.step.),
    avg_sigs_b = mean(Total.Signals.B / (Household.Count - Optin.Count) / X.step.),
    avg_inqs_a = mean(Total.Inquiries.A / Optin.Count / X.step.),
    avg_inqs_b = mean(Total.Inquiries.B / (Household.Count - Optin.Count) / X.step.)
  )
}

# convergence tallies
conv_tallies <- function(run_ends) {
  run_ends |> summarize(cvg_rate = sum(Detect.Convergence. == "true") / n())
}

# evaluation and satisfaction averages
sats_avgs <- function(run_ends) {
  run_ends |>
    summarise(
      avg_eval = mean(Average.Population.Evaluation),
      avg_exps = mean(Average.Population.Expected.Satisfaction),
      avg_eval_a = mean(Average.Sub.Population.Evaluation.With.A),
      avg_exps_a = mean(Average.Sub.Population.Expected.Satisfaction.With.A),
      avg_eval_b = mean(Average.Sub.Population.Evaluation.With.B),
      avg_exps_b = mean(Average.Sub.Population.Expected.Satisfaction.With.B)
    )
}

# takes in a behaviourspace (subset) and generates a summary for it
# NOTE: does not preserve groupings on input
summarize_experiment_ends <- function(run_ends) {
  # choice: opt-ins and statuses
  choice_table <- choice_ratios_alt(run_ends)

  # choices per group
  group_choice_A_table <- choice_ratios_A(run_ends)
  group_choice_B_table <- choice_ratios_B(run_ends)

  # communication: signals and inquiries
  comms_table <- comm_ratios_alt(run_ends)

  # communications per group
  comms_group_table <- comm_ratios_by_group(run_ends)

  # stabilization: stabilized at end
  stabilization <- conv_tallies(run_ends)

  # satisfaction: evaluation and expected satisfaction (across groups)
  sats_table <- sats_avgs(run_ends)

  # return the summaries in a list structure
  return(list(
    "agent-choice" = choice_table,
    "communications" = comms_table,
    "stabilization" = stabilization,
    "agent-satisfaction" = sats_table,
    "group-choice-A" = group_choice_A_table,
    "group-choice-B" = group_choice_B_table,
    "comms-group" = comms_group_table
  ))
}

# creates a summary with the variances of key statistics
# It calculates the rates (count / population) for some key variables and then
# calculates the variance of the rates
summarize_variance <- function(run_ends) {
  list("variance" = run_ends |>
    mutate(true_dill_count = True.Social.Dilemma.Count +
      True.Non.Social.Dilemma.Count + True.Dual.Dilemma.Count) |>
    mutate(comms_per_epoch = Total.Communications / X.step.) |>
    summarize(
      across(
        c(
          Optin.Count,
          true_dill_count,
          Fully.Convinced.Count,
          comms_per_epoch,
          Positive.Satisfaction.Count,
        ),
        list(var = ~ sd(.x / Household.Count))
      )
    ))
}

# creates a table using the <baseline> as baseline and providing the
# other entries (from <other>) as deltas relative to the baseline
# NOTE: uses the first row in the baseline table if there are multiple, this
# returns the <other> table
# TODO: check if it is possible to make this work using staggered/lagged input
delta_table <- function(baseline, other) {
  if (nrow(baseline) == 1) {
    # join the tables, keep columns that are not shared
    combination <- bind_rows(baseline, other)
    deltas <- combination |>
      mutate(across(where(is.numeric), ~ .x - .x[1]))
    deltas[1, ] <- combination[1, ]
    # convert the
    return(deltas)
  } else {
    return(other)
  }
}

# creates a summary using the <summary_baseline> tables as baselines and
# providing the other entries (from <summary>) as deltas relative to the
# baselines
# NOTE: uses the first row in the baseline table if there are multiple
delta_summary <- function(summary_baseline, summary) {
  mapply(delta_table, summary_baseline, summary)
}
