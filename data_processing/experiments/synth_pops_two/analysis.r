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
load_lib_quietly("xtable")
load_lib_quietly("ggplot2")
load_lib_quietly("gridExtra") # grid plots

# make sure we have a proper environment for this script
load_environment_if_necessary()
script_dir <- Sys.getenv("GP_SCRIPT_FOLDER")
output_dir <- Sys.getenv("GP_OUTPUT_DATA_FOLDER")
data_dir <- Sys.getenv("GP_INPUT_DATA_FOLDER")
figure_dir <- Sys.getenv("GP_PLOTTING_FOLDER")
table_dir <- ""

# used in file/folder creation
experiment_name <- "synth_pops_two"

# load the required functions
source(file.path(script_dir, "behaviourspace_helpers.r"))
source(file.path(script_dir, "experiment_helpers.r"))
source(file.path(script_dir, "gp_data_read.r"))
source(file.path(script_dir, "aggregate_data_read.r"))
source(file.path(script_dir, "bspace_table_exports.r"))
# reuses helpers from other synth pop experiment
source(file.path(script_dir, "experiments", "synth_pops", "helpers.r"))

# if we have a figure output folder, set the script to write to it
figure_file <- ""
archive_file <- ""
if (nchar(figure_dir) != 0) {
  figure_file <- file.path(
    figure_dir, paste0(experiment_name, "_figures", ".pdf")
  )
  archive_file <- file.path(
    figure_dir,
    "archive",
    paste0(experiment_name, "_figures_", archive_date(), ".pdf")
  )
  print(paste("Writing figures to:", figure_file))
  pdf(file = figure_file)
  table_dir <- file.path(figure_dir, "tables")
  if (create_folder_if_needed(table_dir)) {
    print(paste0("Writing tables to: ", table_dir))
  }
}

# load the data into a table
experiment_dir <- file.path(output_dir, "experiments", experiment_name)
data_path <- file.path(experiment_dir, paste0(experiment_name, ".csv"))

# get the behavior space data (and enrich it)
# adds the experiment conditions as decoded from the input file name
bspace <- read_behaviourspace_data(data_path)
bspace$pop_name <- pop_name(bspace)
bspace <- bspace |> decode_pop_setup()

# get the final opt-ins per group
run_ends <- subset_run_ends(bspace)

# Load the ground truth data
ground_truth <- read.csv(
  file.path(data_dir, "sb_resultaten", "ground_truth_21-01-2025.csv")
)

# add the relative opt-in rate (opt-in by household according to GP)
ground_truth <- enrich_ground_truth(ground_truth)

# Visualize ground truth for zones of interest and sort by relative opt-in
zones_of_interest <- ground_truth |>
  filter(
    area_name %in%
      c(
        "Helpman",
        "Korrewegwijk",
        "Oosterhoogebrug-Ulgersmaborg",
        "Oosterpark",
        "Selwerd"
      )
  ) |>
  arrange(desc(percent_opt_in_2023_gp), desc(percent_opt_in_2024_gp))

# print the ground truths used in the statistical test
print(
  zones_of_interest |>
    select(
      area_name,
      household_count_gp,
      results_2023,
      intermediate_results_2024_21.01.2025,
      percent_opt_in_2023_gp,
      percent_opt_in_2024_gp
    )
)

# add the predictor variables to the data
run_ends <- run_ends |> add_prediction_columns()
# check that the spread of simulation outcomes is somewhat normal visually
print(run_ends |>
  filter(gen_type == "ipf") |>
  ggplot(aes(x = rel_opt_in, color = pop_name)) +
  geom_density())
print(run_ends |>
  filter(gen_type == "bn") |>
  ggplot(aes(x = rel_opt_in, color = pop_name)) +
  geom_density())

# Summarize the outcomes by key indicators by zone and generation method
model_zone_ordering <- run_ends |>
  select(
    zone, Optin.Count, Household.Count, percent_opt_in, percent_convs, gen_type
  ) |>
  group_by(zone, gen_type) |>
  summarise(
    household = mean(Household.Count), # really this is just a static number
    mean_opt_in = mean(percent_opt_in),
    median_opt_in = median(percent_opt_in),
    sd = sd(percent_opt_in),
    mean_convs = mean(percent_convs),
    sd_convs = sd(percent_convs),
    median_convs = median(percent_convs)
  ) |>
  arrange(desc(mean_opt_in))
ground_truth_ordering <- zones_of_interest |>
  select(
    area_name, household_count_gp, percent_opt_in_2023_gp,
    percent_opt_in_2024_gp, first_year_percent_opt_in_gp
  ) |>
  arrange(desc(first_year_percent_opt_in_gp))
print(model_zone_ordering |> filter(gen_type == "ipf"))
print(model_zone_ordering |> filter(gen_type == "bn"))
print(ground_truth_ordering)


# demonstrate correlation between opt-in and convinced
print("Optins and convinced agents are correlated, but not perfectly:")
print(cor(model_zone_ordering$mean_opt_in, model_zone_ordering$mean_convs))

# Join the ground truth and model results in a table
joint_table <- left_join(
  left_join(
    ground_truth_ordering,
    model_zone_ordering |>
      filter(gen_type == "bn") |>
      select(mean_opt_in, sd, mean_convs, sd_convs, zone),
    by = c("area_name" = "zone")
  ),
  model_zone_ordering |>
    filter(gen_type == "ipf") |>
    select(mean_opt_in, sd, mean_convs, sd_convs, zone),
  by = c("area_name" = "zone"),
  suffix = c("_bn", "_ipf")
)

# perform the Spearman rank correlation test using the opt-in preferences
x <- joint_table$first_year_percent_opt_in_gp
y_bn <- joint_table$mean_opt_in_bn
y_ipf <- joint_table$mean_opt_in_ipf
y_avg <- (joint_table$mean_opt_in_bn + joint_table$mean_opt_in_ipf) / 2
print(cor.test(x, y_bn, method = "spearman"))
print(cor.test(x, y_ipf, method = "spearman"))
print(cor.test(x, y_avg, method = "spearman"))

# perform Spearman rank correlation using the `convinced' rates
y_bn_convs <- joint_table$mean_convs_bn
y_ipf_convs <- joint_table$mean_convs_ipf
y_avg_convs <- (joint_table$mean_convs_bn + joint_table$mean_convs_ipf) / 2
print(cor.test(x, y_bn_convs, method = "spearman"))
print(cor.test(x, y_ipf_convs, method = "spearman"))
print(cor.test(x, y_avg_convs, method = "spearman"))

# combine the results into tables and write to file
cors <- list(
  cor.test(x, y_bn, method = "spearman"),
  cor.test(x, y_ipf, method = "spearman"),
  cor.test(x, y_avg, method = "spearman"),
  cor.test(x, y_bn_convs, method = "spearman"),
  cor.test(x, y_ipf_convs, method = "spearman"),
  cor.test(x, y_avg_convs, method = "spearman")
)
cors_table <- combine_correlation_results(cors)
print(
  xtable(
    cors_table,
    caption = paste0(
      "Results of correlation test using Spearman's $\\rho$. \\code{x} are the ",
      " ground truth opt-in ratios for the zones of interest. Y's ",
      "with \\_convs use the ratio of fully convinced humats and",
      "the other use the ratio of preference within the population"
    ),
    label = paste0("tbl:cors_sim_preds"),
  ),
  include.rownames = FALSE,
  file = file.path(table_dir, "cors_sim_preds_batch_two.tex")
)

# write the predictions to file
print(
  xtable(
    joint_table |>
      select(
        area_name, first_year_percent_opt_in_gp,
        mean_opt_in_bn, mean_opt_in_ipf
      ),
    caption = paste0(
      "This table shows the outcomes for the first year of the",
      " Samen Besparen campaign for a given area (2023 or 2024) and the results",
      " from the simulation model. The metric is the opt-in percentage of",
      " households."
    ),
    label = "tbl:jnt_sim_pred"
  ),
  include.rownames = FALSE,
  file = file.path(table_dir, "joint_sim_pred_table_batch_two.tex")
)

print(
  xtable(
    joint_table |>
      select(
        area_name, first_year_percent_opt_in_gp,
        mean_convs_bn, mean_convs_ipf
      ),
    caption = paste0(
      "This table shows the outcomes for the first year of the",
      " Samen Besparen campaign for a given area (2023 or 2024) and the results",
      " from the simulation model. The metric is the percentage of",
      " `Fully Convinced' households."
    ),
    label = "tbl:jnt_sim_pred_convs"
  ),
  include.rownames = FALSE,
  file = file.path(table_dir, "joint_sim_pred_table_convs_batch_two.tex")
)

# as a reference, give the values side-by-side again (opt-in pred.)
# DUPLICATE (for notes)
print(xtable(
  joint_table |>
    select(
      area_name,
      first_year_percent_opt_in_gp,
      mean_opt_in_bn,
      mean_opt_in_ipf,
    ),
  label = "tbl:pred_outcomes",
  caption = paste0(
    "This table shows the outcomes for the first year of the",
    " Samen Besparen campaign for a given area (2023 or 2024) and the results",
    " from the simulation model. The metric is the opt-in percentage of",
    " households."
  )
), include.rownames = FALSE)

# as a reference, give the values side-by-side again (fully-convinced pred.)
print(xtable(
  joint_table |>
    select(
      area_name,
      first_year_percent_opt_in_gp,
      mean_convs_bn,
      mean_convs_ipf,
    ),
  label = "tbl:pred_conv_outcomes",
  caption = paste0(
    "This table shows the outcomes for the first year of the",
    " Samen Besparen campaign for a given area (2023 or 2024) and the results",
    " from the simulation model. The metric is the percentage of",
    " `Fully Convinced' households."
  )
), include.rownames = FALSE)


# produce an overview of the results of the simulation in tabular form (deltas)
# TODO: load the baseline results/summary and get the deltas
bn_runs <- run_ends |> filter(gen_type == "bn")
bn_summary <- bn_runs |>
  group_by(zone) |>
  summarize_experiment_ends()

print_labeled_summary(
  bn_summary,
  dir = table_dir,
  file_name_prefix = "bayesian_batch_two",
  sub_name = "bayesian-two"
)

ipf_runs <- run_ends |> filter(gen_type == "ipf")

ipf_summary <- ipf_runs |>
  group_by(zone) |>
  summarize_experiment_ends()

print_labeled_summary(
  ipf_summary,
  dir = table_dir,
  file_name_prefix = "ipf_batch_two",
  sub_name = "ipf-two"
)

# TODO: Visualize the average progression of the model over time for each zone

# TODO: Do a three-ways comparison between the ground truth, model and
# alternative predictive method

# if we have a figure output folder, set the script to write to it
if (nchar(figure_dir) != 0) {
  print(paste("Wrote figures to:", figure_dir))
  dev.off()
  file.copy(figure_file, archive_file, overwrite = TRUE)
}

# TODO: give plots of key characteristics with pop gen conditions side-by-side
# TODO: include the baseline in the plots as well
# agent choice
ggsave(
  filename = file.path(figure_dir, "batch_two_choice_plots.pdf"),
  annotate_figure(
    ggarrange(
      bn_summary[["agent-choice"]] |>
        rename(condition = zone) |>
        plot_choice_summary() +
        labs(
          title = NULL,
          subtitle = paste0("Bayesian network synthetic populations")
        ) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)),
      ipf_summary[["agent-choice"]] |>
        rename(condition = zone) |>
        plot_choice_summary() +
        labs(
          title = NULL,
          subtitle = paste0("IPF synthetic populations")
        ) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)),
      ncol = 2, common.legend = TRUE, legend = "bottom"
    ), "Population choice summary plots (synthetic populations)"
  ), width = 7, height = 7
)

# communication
ggsave(
  filename = file.path(figure_dir, "batch_two_comm_plots.pdf"),
  annotate_figure(
    ggarrange(
      bn_summary[["communications"]] |>
        rename(condition = zone) |>
        plot_comm_summary() +
        labs(
          title = NULL,
          subtitle = paste0("Bayesian network synthetic populations")
        ) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)),
      ipf_summary[["communications"]] |>
        rename(condition = zone) |>
        plot_comm_summary() +
        labs(
          title = NULL,
          subtitle = paste0("IPF synthetic populations")
        ) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)),
      ncol = 2, common.legend = TRUE, legend = "bottom"
    ), "Population communication summary plots (synthetic populations)"
  ), width = 7, height = 7
)

ggsave(
  filename = file.path(figure_dir, "batch_two_group_comm_plots.pdf"),
  annotate_figure(
    ggarrange(
      bn_summary[["comms-group"]] |>
        rename(condition = zone) |>
        plot_group_comm_summary() +
        labs(
          title = NULL,
          subtitle = paste0("Bayesian network synthetic populations")
        ) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)),
      ipf_summary[["comms-group"]] |>
        rename(condition = zone) |>
        plot_group_comm_summary() +
        labs(
          title = NULL,
          subtitle = paste0("IPF synthetic populations")
        ) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)),
      ncol = 2, common.legend = TRUE, legend = "bottom"
    ), "Groupwise communication summary plots (synthetic populations)"
  ), width = 7, height = 7
)

# satisfaction
ggsave(
  filename = file.path(figure_dir, "batch_two_satisfaction_plots.pdf"),
  annotate_figure(
    ggarrange(
      bn_summary[["agent-satisfaction"]] |>
        rename(condition = zone) |>
        select(!c(avg_eval, avg_eval_a, avg_eval_b)) |>
        plot_satisfaction_summary() + ylim(0.0, 1) +
        labs(
          title = NULL,
          subtitle = paste0("Bayesian network synthetic populations")
        ) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)),
      ipf_summary[["agent-satisfaction"]] |>
        rename(condition = zone) |>
        select(!c(avg_eval, avg_eval_a, avg_eval_b)) |>
        plot_satisfaction_summary() + ylim(0.0, 1) +
        labs(
          title = NULL,
          subtitle = paste0("IPF synthetic populations")
        ) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)),
      ncol = 2, common.legend = TRUE, legend = "bottom"
    ), "Population satisfaction summary plots (synthetic populations)"
  ), width = 7, height = 7
)

# extra graph that shows the difference in satisfaction breakdown for group b
sat_labels <- c("High S'(x)", "Low S'(x)", "Positive S'(x)")
ggsave(
  filename = file.path(figure_dir, "synthetics_group_b_sats_plots.pdf"),
  annotate_figure(
    ggarrange(
      bn_summary[["group-choice-B"]] |>
        rename(condition = zone) |>
        plot_summary_bars() +
        scale_fill_discrete(
          name = "Metric",
          labels = sat_labels
        ) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        labs(
          title = NULL,
          x = "Zone",
          y = "Ratio of population",
          subtitle = paste0("Bayesian network synthetic populations")
        ) +
        # ylims need to be expanded for this condition
        ylim(0.0, 1.0),
      ipf_summary[["group-choice-B"]] |>
        rename(condition = zone) |>
        plot_summary_bars() +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        labs(
          title = NULL,
          x = "Zone",
          y = "Ratio of population",
          subtitle = paste0("IPF synthetic populations")
        ) +
        # ylims need to be expanded for this condition
        ylim(0.0, 1.0),
      ncol = 2, common.legend = TRUE, legend = "bottom"
    ), "Satisfaction levels within group B (opt-out) (synthetic populations)"
  ), width = 7, height = 7
)

# TODO: do plots with all areas (both batches) in one plot for the appendix

# TODO: make plots that show variance
test_space <- run_ends |>
  filter(gen_type == "ipf")

test_data <- test_space |>
  rename(condition = zone) |>
  # sum the true dilemma's to indicate only true dilemma's
  mutate(
    dilemma_count =
      True.Non.Social.Dilemma.Count + True.Social.Dilemma.Count +
        True.Dual.Dilemma.Count
  ) |>
  select(
    Optin.Count,
    Fully.Convinced.Count,
    dilemma_count,
    Household.Count,
    X.run.number.,
    condition
  ) |>
  pivot_longer(cols = !c(condition, X.run.number.)) |>
  group_by(condition, name) |>
  summarise(across(where(is.numeric) & !X.run.number., list(mean = mean, sd = sd)))
