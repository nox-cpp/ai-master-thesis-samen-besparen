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
load_lib_quietly("gridExtra") # grid plots
load_lib_quietly("xtable")
load_lib_quietly("ggpubr")

# make sure we have a proper environment for this script
load_environment_if_necessary()
script_dir <- Sys.getenv("GP_SCRIPT_FOLDER")
output_dir <- Sys.getenv("GP_OUTPUT_DATA_FOLDER")
data_dir <- Sys.getenv("GP_INPUT_DATA_FOLDER")
figure_dir <- Sys.getenv("GP_PLOTTING_FOLDER")
table_dir <- ""


# load the required functions
source(file.path(script_dir, "behaviourspace_helpers.r"))
source(file.path(script_dir, "bspace_table_exports.r"))
source(file.path(script_dir, "experiment_helpers.r"))
source(file.path(script_dir, "gp_data_read.r"))
source(file.path(script_dir, "behaviourspace_plot_exports.r"))

# used in file/folder creation
experiment_name <- "mapping_space"

figure_file <- ""
archive_file <- ""

# if we have a figure output folder, set the script to write to it
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
bspace <- read_behaviourspace_data(data_path)
bspace$pop_name <- pop_name(bspace)
# enrich the data by grouping into main experiment condition
bspace$condition <- main_condition(bspace)
# get the states at run ends
run_ends <- subset_run_ends(bspace)

# TODO: enrich the data by grouping into experiment sub conditions

# summarize the baseline experiment
baseline_run_ends <- run_ends |> filter(condition == "baseline")
baseline_summary_by_campaign <- baseline_run_ends |>
  # rename for cleaner tables
  rename(campaign = ui.campaign.enabled) |>
  group_by(campaign) |>
  summarize_experiment_ends()

# generate summary over whole condition
baseline_summary <- baseline_run_ends |>
  group_by(condition) |>
  summarize_experiment_ends()

# print the baseline results to a (latex) table file
baseline_tbl_file <- file.path(table_dir, "baseline.tex")
print_labeled_summary(baseline_summary, "baseline", dir = table_dir)
print_labeled_summary(
  baseline_summary_by_campaign,
  "baseline-by-campaign",
  dir = table_dir
)
baseline_variance_summary <-
  summarize_variance(baseline_run_ends |> group_by(condition))
print_labeled_summary(
  baseline_variance_summary, "baseline",
  dir = table_dir
)

baseline_variance_summary_campaignwise <- summarize_variance(
  baseline_run_ends |>
    # rename for cleaner tables
    rename(campaign = ui.campaign.enabled) |>
    group_by(condition, campaign)
)
print_labeled_summary(baseline_variance_summary_campaignwise,
  "baseline-by-campaign",
  dir = table_dir
)

# demonstrate the relative variances between counts and communication totals
print(baseline_run_ends |> ggplot(aes(x = Total.Communications)) +
  geom_histogram(binwidth = 100))
print(baseline_run_ends |> ggplot(aes(x = Total.Communications, color = ui.campaign.enabled)) +
  geom_histogram(binwidth = 100))
print(baseline_run_ends |> ggplot(aes(x = Total.Communications / X.step., color = ui.campaign.enabled)) +
  geom_histogram(binwidth = 1))
print(baseline_run_ends |> filter(ui.campaign.enabled == "false" & Detect.Convergence. == "true") |> ggplot(aes(x = X.step., color = ui.campaign.enabled)) +
  geom_histogram(binwidth = 5))
print(baseline_run_ends |> ggplot(aes(x = Optin.Count)) +
  geom_histogram(binwidth = 1))
print(baseline_run_ends |> ggplot(aes(x = Fully.Convinced.Count)) +
  geom_histogram(binwidth = 1))
print(baseline_run_ends |> ggplot(aes(x = True.Dual.Dilemma.Count + True.Social.Dilemma.Count + True.Non.Social.Dilemma.Count)) +
  geom_histogram(binwidth = 1))

# generate a summary for the financial sub-experiment
financial_subexp_summary <- run_ends |>
  filter(condition == "fin_map") |>
  mutate(condition = pop_name) |>
  group_by(condition) |>
  summarize_experiment_ends()
print_labeled_summary(
  format_delta_summary(
    delta_summary(baseline_summary, financial_subexp_summary)
  ),
  "financial",
  dir = table_dir
)

# comfort sub experiment
comfort_summary <- run_ends |>
  filter(condition == "comf_map") |>
  mutate(condition = pop_name) |>
  group_by(condition) |>
  summarize_experiment_ends()
print_labeled_summary(
  format_delta_summary(
    delta_summary(baseline_summary, comfort_summary)
  ), "comfort",
  dir = table_dir
)

# value sub experiment
value_summary <- run_ends |>
  filter(condition == "value_map") |>
  mutate(condition = pop_name) |>
  group_by(condition) |>
  summarize_experiment_ends()
print_labeled_summary(
  format_delta_summary(
    delta_summary(baseline_summary, value_summary)
  ), "value",
  dir = table_dir
)

# load the population setups from file (so we can match them by name)\
# Unused atm
configs <- read.csv(
  file.path(experiment_dir, "mapping_space_cfg.csv")
)

# Demonstrate the limited variance by means of a plot
variance_bin_plot_path <- file.path(figure_dir, "variance_demo.pdf")
variance_bin_hist <- baseline_run_ends |>
  rename(campaign = ui.campaign.enabled) |>
  ggplot(aes(x = Optin.Count, color = campaign)) +
  geom_bar() +
  # median collected, but realistically all populations are the same size
  labs(
    x = paste0(
      "Number of humats that prefer opting in at simulation run end ",
      "(Baseline)"
    ),
    y = "Number of runs",
    title = "Histogram of humat preferences across runs",
    subtitle = paste0(
      nrow(baseline_run_ends), " runs, median indicated by line. Population ",
      "size: ", median(baseline_run_ends$Household.Count)
    ),
  ) +
  scale_color_discrete(
    name = "campaign", labels = c("disabled", "enabled"), drop = FALSE
  ) +
  geom_vline(aes(xintercept = median(Optin.Count)))

ggsave(
  plot = variance_bin_hist,
  file = variance_bin_plot_path, width = 7, height = 7
)

# add the same type of plot for some other variables for in the appendix
variance_multiplot_path <- file.path(figure_dir, "variance_demo_app.pdf")
var_plot_trdill <- baseline_run_ends |>
  rename(campaign = ui.campaign.enabled) |>
  mutate(
    true_dill =
      True.Dual.Dilemma.Count +
        True.Social.Dilemma.Count +
        True.Non.Social.Dilemma.Count
  ) |>
  ggplot(aes(x = true_dill, color = campaign)) +
  geom_bar() +
  # median collected, but realistically all populations are the same size
  labs(
    x = "True dilemma",
    # title = "Histogram of humat dilemma counts",
    # subtitle = paste0(
    #   nrow(baseline_run_ends), " runs, median indicated by line. Population size: ",
    #   median(baseline_run_ends$Household.Count)
    # )
  ) +
  geom_vline(aes(xintercept = median(true_dill)))

var_plot_poseval <- baseline_run_ends |>
  rename(campaign = ui.campaign.enabled) |>
  ggplot(aes(x = Positive.Satisfaction.Count, color = campaign)) +
  geom_bar() +
  # median collected, but realistically all populations are the same size
  labs(
    x = "Positive S'(x)",
    # title = "Histogram of humat expected satisfaction",
    # subtitle = paste0(
    #   nrow(baseline_run_ends), " runs, median indicated by line. Population size: ",
    #   median(baseline_run_ends$Household.Count)
    # )
  ) +
  geom_vline(aes(xintercept = median(Positive.Satisfaction.Count)))

var_plot_conv <- baseline_run_ends |>
  rename(campaign = ui.campaign.enabled) |>
  ggplot(aes(x = Fully.Convinced.Count, color = campaign)) +
  geom_bar() +
  # median collected, but realistically all populations are the same size
  labs(
    x = "`Fully Convinced'",
    # title = "Histogram of fully convinced humats",
    # subtitle = paste0(
    #   nrow(baseline_run_ends), " runs, median indicated by line. Population size: ",
    #   median(baseline_run_ends$Household.Count)
    # )
  ) +
  geom_vline(aes(xintercept = median(Fully.Convinced.Count)))

ggsave(
  variance_multiplot_path,
  plot = annotate_figure(
    ggarrange(
      var_plot_conv, var_plot_poseval, var_plot_trdill,
      variance_bin_hist + labs(x = "Prefers A", title = NULL, subtitle = NULL),
      ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom"
    ),
    # add a title on top of the multiplot
    top = paste0(
      "Histograms of key metrics. Medians highlighted by lines. ",
      # household count taken as median, but is the same across runs
      "Population size: ", median(bspace$Household.Count)
    )
  ),
  width = 7, height = 7
  # grid.arrange(var_plot_conv, var_plot_poseval, var_plot_trdill, variance_bin_hist, nrow = 2)
)

# add a plot that shows the differences between distribution for campaign on/off
pref_hist_campaign <- baseline_run_ends |>
  mutate(campaign = factor(ui.campaign.enabled, levels = c("false", "true"))) |>
  filter(campaign == "true") |>
  ggplot(aes(x = Optin.Count, color = campaign)) +
  labs(x = "Prefers A") +
  geom_bar() +
  scale_color_discrete(
    name = "campaign", labels = c("disabled", "enabled"), drop = FALSE
  ) +
  xlim(500, 515) +
  ylim(0, 25) +
  geom_vline(aes(xintercept = median(Optin.Count)))

pref_hist_no_campaign <- baseline_run_ends |>
  mutate(campaign = ui.campaign.enabled) |>
  filter(campaign == "false") |>
  ggplot(aes(x = Optin.Count, color = campaign)) +
  labs(x = "Prefers A", title = NULL, subtitle = NULL) +
  scale_color_discrete(
    name = "campaign", labels = c("disabled", "enabled"), drop = FALSE
  ) +
  xlim(500, 515) +
  ylim(0, 25) +
  geom_bar() +
  geom_vline(aes(xintercept = median(Optin.Count)))

campaign_prefs_plot_path <- file.path(figure_dir, "campaign_prefs.pdf")
ggsave(
  campaign_prefs_plot_path,
  annotate_figure(
    ggarrange(
      pref_hist_campaign, pref_hist_no_campaign,
      common.legend = TRUE, legend = "bottom", ncol = 2
    ),
    top =
      paste0(
        "Plots of humat preference outcomes across runs.\nSplit by ",
        "campaign condition. Population size: ",
        median(baseline_run_ends$Household.Count)
      )
  ),
  width = 7, height = 7
)

# TODO: show the dynamics over time

# shows the dynamics as averages for a condition
# baseline experiment (dotplot)
bspace |>
  filter(condition == "baseline") |>
  ggplot() +
  xlim(1, NA) +
  geom_point(aes(x = X.step., y = Optin.Count / Household.Count)) +
  geom_point(aes(x = X.step., y = Fully.Convinced.Count / Household.Count)) +
  geom_point(aes(x = X.step., y = Average.Population.Evaluation)) +
  geom_point(aes(x = X.step., y = Average.Population.Expected.Satisfaction))


baseline_summ_vals_wide <- bspace |>
  filter(condition == "baseline") |>
  # take averages at each timestep
  group_by(X.step.) |>
  summarise(across(where(is.numeric), .fns = list(mean = mean, sd = sd, upper = ~ mean(.x) + sd(.x), lower = ~ mean(.x) - sd(.x)), .names = "{.col}_{.fn}"), households = mean(Household.Count)) |>
  select(
    starts_with("Optin.Count"),
    starts_with("Fully.Convinced.Count_"),
    starts_with("Average.Population.Evaluation"),
    starts_with("Average.Population.Expected.Satisfaction"),
    households,
    starts_with("X.step.")
  ) |>
  mutate(
    across(
      # reminder: mean(x) / c == mean(x / c), same for sd
      c(starts_with("Optin.Count"), starts_with("Fully.Convinced.Count")),
      ~ .x / households
    )
  ) |>
  select(!households)

baseline_summ_vals_long <- baseline_summ_vals_wide |> pivot_longer(cols = !X.step.)

# keep this around for the appendix and end of the presentation
# plots the mean for select variables as well as one sd above/below as line
ggplot() +
  xlim(1, NA) +
  geom_line(data = baseline_summ_vals_long |> filter(grepl("mean", name)), aes(x = X.step., y = value, colour = name)) +
  geom_line(data = baseline_summ_vals_long |> filter(grepl("upper", name)), aes(x = X.step., y = value, colour = name)) +
  geom_line(data = baseline_summ_vals_long |> filter(grepl("lower", name)), aes(x = X.step., y = value, colour = name))

# keep this around for the appendix and end of the presentation
# plots the mean for select variables as well as one sd above/below as ribbons
baseline_summ_vals_wide |>
  ggplot() +
  xlim(1, NA) +
  geom_line(aes(x = X.step., y = Optin.Count_mean)) +
  geom_ribbon(aes(x = X.step., ymin = Optin.Count_lower, ymax = Optin.Count_lower, alpha = 0.1)) +
  geom_line(aes(x = X.step., y = Fully.Convinced.Count_mean)) +
  geom_ribbon(aes(x = X.step., ymin = Fully.Convinced.Count_lower, ymax = Fully.Convinced.Count_upper, alpha = 0.1)) +
  geom_line(aes(x = X.step., y = Average.Population.Evaluation_mean)) +
  geom_ribbon(aes(x = X.step., ymin = Average.Population.Evaluation_lower, ymax = Average.Population.Evaluation_upper, alpha = 0.1)) +
  geom_line(aes(x = X.step., y = Average.Population.Expected.Satisfaction_mean)) +
  geom_ribbon(aes(x = X.step., ymin = Average.Population.Expected.Satisfaction_lower, ymax = Average.Population.Expected.Satisfaction_upper, alpha = 0.1))

# Average plots for baseline condition
bspace_baseline <- bspace |>
  filter(condition == "baseline")

ggsave(
  filename = file.path(figure_dir, "baseline_choice_time.pdf"),
  bspace_baseline |> plot_humat_choice_summary("baseline"),
  width = 7, height = 7
)
ggsave(
  filename = file.path(figure_dir, "baseline_rest_time.pdf"),
  multiplot_characteristics_bspace(bspace_baseline, "baseline"),
  width = 7, height = 10
)

# TODO: break down the conditions to subconditions (or add them or something)
multiplot_characteristics_bspace(bspace |> filter(pop_name == "idealistic_bl"))
multiplot_characteristics_bspace(bspace |> filter(pop_name == "idealistic_eq"))
multiplot_characteristics_bspace(bspace |> filter(pop_name == "optimistic_bl"))
multiplot_characteristics_bspace(bspace |> filter(pop_name == "optimistic_eq"))
multiplot_characteristics_bspace(bspace |> filter(pop_name == "wealth_bl"))
multiplot_characteristics_bspace(bspace |> filter(pop_name == "wealth_eq"))

# give a multiplot for a typical run
sample_run_nr <- sample(bspace_baseline$X.run.number., 1)
ggsave(
  filename = file.path(figure_dir, "baseline_rest_time_sample.pdf"),
  multiplot_characteristics_bspace(
    bspace_baseline |>
      filter(X.run.number. == sample_run_nr)
  ), width = 7, height = 10
)

ggsave(
  filename = file.path(figure_dir, "baseline_choice_time_sample.pdf"),
  plot_humat_choice_summary(bspace_baseline |>
    filter(X.run.number. == sample_run_nr)),
  width = 7, height = 7
)

# make a plot with a histogram of convergence times
stable_runs <- run_ends |>
  filter(condition == "baseline" &
    ui.campaign.enabled == "false" &
    Detect.Convergence. == "true")

ggsave(
  filename = file.path(figure_dir, "baseline_stabilization_times.pdf"),
  stable_runs |>
    ggplot() +
    geom_histogram(aes(x = X.step.), binwidth = 1) +
    labs(
      x = "Timestep", y = "N",
      title = paste0(
        "Histogram of baseline stabilization times (", nrow(stable_runs),
        " runs)"
      ),
      subtitle = paste0(
        "Includes baseline runs where campaign is disabled (N = ",
        nrow(run_ends |>
          filter(condition == "baseline" & ui.campaign.enabled == "false")),
        ") and the run stabilized (N = ", nrow(stable_runs), ")"
      )
    ),
  width = 7, height = 7
)

# if we have a figure output folder, set the script to write to it
if (nchar(figure_dir) != 0) {
  print(paste("Wrote figures to:", figure_dir))
  dev.off()
  file.copy(figure_file, archive_file, overwrite = TRUE)
}

# financial summary plots
{
  financial_labels <- c("Low return", "Pessimistic", "Wealth")
  financial_bl_labels <- c("Baseline", financial_labels)
  # choice summary bar-plot side-by-side for imp. dists. (financials)
  fin_choice_summary <- rbind(
    financial_subexp_summary[["agent-choice"]], baseline_summary[["agent-choice"]]
  )
  ggsave(
    filename = file.path(figure_dir, "financial_choice_plots.pdf"),
    annotate_figure(
      ggarrange(
        fin_choice_summary |>
          filter(!grepl("eq", condition)) |>
          plot_choice_summary() +
          scale_x_discrete(
            labels = financial_bl_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Baseline importance distribution (bl)"
            )
          ),
        fin_choice_summary |>
          filter(!grepl("bl", condition), condition != "baseline") |>
          plot_choice_summary() +
          scale_x_discrete(
            labels = financial_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Equal importance distribution (eq)"
            )
          ),
        ncol = 2, common.legend = TRUE, legend = "bottom"
      ), "Population choice summary plots (financial scenarios)"
    ), width = 7, height = 7
  )

  # comms summary for financial subexperiments
  fin_comms_summary <- rbind(
    financial_subexp_summary[["communications"]],
    baseline_summary[["communications"]]
  )

  ggsave(
    filename = file.path(figure_dir, "financial_comms_plots.pdf"),
    annotate_figure(
      ggarrange(
        fin_comms_summary |>
          filter(!grepl("eq", condition)) |>
          plot_comm_summary() +
          scale_x_discrete(
            labels = financial_bl_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Baseline importance distribution (bl)"
            )
          ),
        fin_comms_summary |>
          filter(!grepl("bl", condition), condition != "baseline") |>
          plot_comm_summary() +
          scale_x_discrete(
            labels = financial_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Equal importance distribution (eq)"
            )
          ),
        ncol = 2, common.legend = TRUE, legend = "bottom"
      ), "Population communication summary plots (financial scenarios)"
    ), width = 7, height = 7
  )

  # group comss summary for financial subexperiments
  fin_group_comms_summary <- rbind(
    financial_subexp_summary[["comms-group"]], baseline_summary[["comms-group"]]
  )
  ggsave(
    filename = file.path(figure_dir, "financial_group_comms_plots.pdf"),
    annotate_figure(
      ggarrange(
        fin_group_comms_summary |>
          filter(!grepl("eq", condition)) |>
          plot_group_comm_summary() +
          scale_x_discrete(
            labels = financial_bl_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Baseline importance distribution (bl)"
            )
          ),
        fin_group_comms_summary |>
          filter(!grepl("bl", condition), condition != "baseline") |>
          plot_group_comm_summary() +
          scale_x_discrete(
            labels = financial_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Equal importance distribution (eq)"
            )
          ),
        ncol = 2, common.legend = TRUE, legend = "bottom"
      ), "Groupwise communication summary plots (financial scenarios)"
    ), width = 7, height = 7
  )

  # satisfaction summary for financial subexperiments
  fin_satisfaction_summary <- rbind(
    financial_subexp_summary[["agent-satisfaction"]],
    baseline_summary[["agent-satisfaction"]]
  ) |> select(!c(avg_eval, avg_eval_a, avg_eval_b))
  ggsave(
    filename = file.path(figure_dir, "financial_satisfaction_plots.pdf"),
    annotate_figure(
      ggarrange(
        fin_satisfaction_summary |>
          filter(!grepl("eq", condition)) |>
          plot_satisfaction_summary() +
          scale_x_discrete(
            labels = c("Baseline", "Low return", "Pessimistic", "Wealth")
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Baseline importance distribution (bl)"
            )
          ),
        fin_satisfaction_summary |>
          filter(!grepl("bl", condition), condition != "baseline") |>
          plot_satisfaction_summary() +
          scale_x_discrete(
            labels = c("Low return", "Pessimistic", "Wealth")
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Equal importance distribution (eq)"
            )
          ),
        ncol = 2, common.legend = TRUE, legend = "bottom"
      ), "Groupwise communication summary plots (financial scenarios)"
    ), width = 7, height = 7
  )
}

# comfort summary plots
{
  comfort_labels <- c("Diminishing returns", "Optimistic")
  comfort_bl_labels <- c("Baseline", comfort_labels)
  # choice summary bar-plot side-by-side for imp. dists. (financials)
  comfort_choice_summary <- rbind(
    comfort_summary[["agent-choice"]], baseline_summary[["agent-choice"]]
  )
  ggsave(
    filename = file.path(figure_dir, "comfort_choice_plots.pdf"),
    annotate_figure(
      ggarrange(
        comfort_choice_summary |>
          filter(!grepl("eq", condition)) |>
          plot_choice_summary() +
          scale_x_discrete(
            labels = comfort_bl_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Baseline importance distribution (bl)"
            )
          ),
        comfort_choice_summary |>
          filter(!grepl("bl", condition), condition != "baseline") |>
          plot_choice_summary() +
          scale_x_discrete(
            labels = comfort_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Equal importance distribution (eq)"
            )
          ),
        ncol = 2, common.legend = TRUE, legend = "bottom"
      ), "Population choice summary plots (comfort scenarios)"
    ), width = 7, height = 7
  )

  # comms summary for comfort subexperiments
  comfort_comms_summary <- rbind(
    comfort_summary[["communications"]],
    baseline_summary[["communications"]]
  )

  ggsave(
    filename = file.path(figure_dir, "comfort_comms_plots.pdf"),
    annotate_figure(
      ggarrange(
        comfort_comms_summary |>
          filter(!grepl("eq", condition)) |>
          plot_comm_summary() +
          scale_x_discrete(
            labels = comfort_bl_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Baseline importance distribution (bl)"
            )
          ),
        comfort_comms_summary |>
          filter(!grepl("bl", condition), condition != "baseline") |>
          plot_comm_summary() +
          scale_x_discrete(
            labels = comfort_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Equal importance distribution (eq)"
            )
          ),
        ncol = 2, common.legend = TRUE, legend = "bottom"
      ), "Population communication summary plots (comfort scenarios)"
    ), width = 7, height = 7
  )

  # group comss summary for comfort subexperiments
  comfort_group_comms_summary <- rbind(
    comfort_summary[["comms-group"]], baseline_summary[["comms-group"]]
  )
  ggsave(
    filename = file.path(figure_dir, "comfort_group_comms_plots.pdf"),
    annotate_figure(
      ggarrange(
        comfort_group_comms_summary |>
          filter(!grepl("eq", condition)) |>
          plot_group_comm_summary() +
          scale_x_discrete(
            labels = comfort_bl_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Baseline importance distribution (bl)"
            )
          ),
        comfort_group_comms_summary |>
          filter(!grepl("bl", condition), condition != "baseline") |>
          plot_group_comm_summary() +
          scale_x_discrete(
            labels = comfort_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Equal importance distribution (eq)"
            )
          ),
        ncol = 2, common.legend = TRUE, legend = "bottom"
      ), "Groupwise communication summary plots (comfort scenarios)"
    ), width = 7, height = 7
  )

  # satisfaction summary for comfort subexperiments
  comfort_satisfaction_summary <- rbind(
    comfort_summary[["agent-satisfaction"]],
    baseline_summary[["agent-satisfaction"]]
  ) |> select(!c(avg_eval, avg_eval_a, avg_eval_b))
  ggsave(
    filename = file.path(figure_dir, "comfort_satisfaction_plots.pdf"),
    annotate_figure(
      ggarrange(
        comfort_satisfaction_summary |>
          filter(!grepl("eq", condition)) |>
          plot_satisfaction_summary() +
          scale_x_discrete(
            labels = comfort_bl_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Baseline importance distribution (bl)"
            )
          ) +
          # ylims need to be expanded for this condition
          ylim(0.0, 1.0),
        comfort_satisfaction_summary |>
          filter(!grepl("bl", condition), condition != "baseline") |>
          plot_satisfaction_summary() +
          scale_x_discrete(
            labels = comfort_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Equal importance distribution (eq)"
            )
          ) +
          # ylims need to be expanded for this condition
          ylim(0.0, 1.0),
        ncol = 2, common.legend = TRUE, legend = "bottom"
      ), "Groupwise communication summary plots (comfort scenarios)"
    ), width = 7, height = 7
  )
}

# value summary plots
{
  # choice summary bar-plot side-by-side for imp. dists. (financials)
  value_choice_summary <- rbind(
    value_summary[["agent-choice"]], baseline_summary[["agent-choice"]]
  )
  value_labels <- c("Drastic", "Idealistic", "Relative")
  value_bl_labels <- c("Baseline", value_labels)
  ggsave(
    filename = file.path(figure_dir, "value_choice_plots.pdf"),
    annotate_figure(
      ggarrange(
        value_choice_summary |>
          filter(!grepl("eq", condition)) |>
          plot_choice_summary() +
          scale_x_discrete(
            labels = value_bl_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Baseline importance distribution (bl)"
            )
          ),
        value_choice_summary |>
          filter(!grepl("bl", condition), condition != "baseline") |>
          plot_choice_summary() +
          scale_x_discrete(
            labels = value_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Equal importance distribution (eq)"
            )
          ),
        ncol = 2, common.legend = TRUE, legend = "bottom"
      ), "Population choice summary plots (value scenarios)"
    ), width = 7, height = 7
  )

  # comms summary for value subexperiments
  value_comms_summary <- rbind(
    value_summary[["communications"]],
    baseline_summary[["communications"]]
  )

  ggsave(
    filename = file.path(figure_dir, "value_comms_plots.pdf"),
    annotate_figure(
      ggarrange(
        value_comms_summary |>
          filter(!grepl("eq", condition)) |>
          plot_comm_summary() +
          scale_x_discrete(
            labels = value_bl_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Baseline importance distribution (bl)"
            )
          ),
        value_comms_summary |>
          filter(!grepl("bl", condition), condition != "baseline") |>
          plot_comm_summary() +
          scale_x_discrete(
            labels = value_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Equal importance distribution (eq)"
            )
          ),
        ncol = 2, common.legend = TRUE, legend = "bottom"
      ), "Population communication summary plots (value scenarios)"
    ), width = 7, height = 7
  )

  # group comss summary for value subexperiments
  value_group_comms_summary <- rbind(
    value_summary[["comms-group"]], baseline_summary[["comms-group"]]
  )
  ggsave(
    filename = file.path(figure_dir, "value_group_comms_plots.pdf"),
    annotate_figure(
      ggarrange(
        value_group_comms_summary |>
          filter(!grepl("eq", condition)) |>
          plot_group_comm_summary() +
          scale_x_discrete(
            labels = value_bl_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Baseline importance distribution (bl)"
            )
          ),
        value_group_comms_summary |>
          filter(!grepl("bl", condition), condition != "baseline") |>
          plot_group_comm_summary() +
          scale_x_discrete(
            labels = value_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Equal importance distribution (eq)"
            )
          ),
        ncol = 2, common.legend = TRUE, legend = "bottom"
      ), "Groupwise communication summary plots (value scenarios)"
    ), width = 7, height = 7
  )

  # satisfaction summary for value subexperiments
  value_satisfaction_summary <- rbind(
    value_summary[["agent-satisfaction"]],
    baseline_summary[["agent-satisfaction"]]
  ) |> select(!c(avg_eval, avg_eval_a, avg_eval_b))
  ggsave(
    filename = file.path(figure_dir, "value_satisfaction_plots.pdf"),
    annotate_figure(
      ggarrange(
        value_satisfaction_summary |>
          filter(!grepl("eq", condition)) |>
          plot_satisfaction_summary() +
          scale_x_discrete(
            labels = value_bl_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Baseline importance distribution (bl)"
            )
          ) +
          # ylims need to be expanded for this condition
          ylim(0.0, 1.0),
        value_satisfaction_summary |>
          filter(!grepl("bl", condition), condition != "baseline") |>
          plot_satisfaction_summary() +
          scale_x_discrete(
            labels = value_labels
          ) +
          labs(
            title = NULL,
            subtitle = paste0(
              "Equal importance distribution (eq)"
            )
          ) +
          # ylims need to be expanded for this condition
          ylim(0.0, 1.0),
        ncol = 2, common.legend = TRUE, legend = "bottom"
      ), "Groupwise communication summary plots (value scenarios)"
    ), width = 7, height = 7
  )

  # extra graph that shows the difference in satisfaction breakdown for group b
  value_b_sats_summary <- rbind(
    value_summary[["group-choice-B"]],
    baseline_summary[["group-choice-B"]]
  )
  sat_labels <- c("High S'(x)", "Low S'(x)", "Positive S'(x)")
  ggsave(
    filename = file.path(figure_dir, "value_group_b_sats_plots.pdf"),
    annotate_figure(
      ggarrange(
        value_b_sats_summary |>
          filter(!grepl("eq", condition)) |>
          plot_summary_bars() +
          scale_x_discrete(
            labels = value_bl_labels
          ) +
          scale_fill_discrete(
            name = "Metric",
            labels = sat_labels
          ) +
          labs(
            title = NULL,
            x = "Experiment condition",
            y = "Ratio of population",
            subtitle = paste0(
              "Baseline importance distribution (bl)"
            )
          ) +
          # ylims need to be expanded for this condition
          ylim(0.0, 1.0),
        value_b_sats_summary |>
          filter(!grepl("bl", condition), condition != "baseline") |>
          plot_summary_bars() +
          scale_x_discrete(
            labels = value_labels
          ) +
          scale_fill_discrete(
            name = "Metric", labels = sat_labels
          ) +
          labs(
            title = NULL,
            x = "Experiment condition",
            y = "Ratio of population",
            subtitle = paste0(
              "Equal importance distribution (eq)"
            )
          ) +
          # ylims need to be expanded for this condition
          ylim(0.0, 1.0),
        ncol = 2, common.legend = TRUE, legend = "bottom"
      ), "Satisfaction levels within group B (opt-out) (value scenarios)"
    ), width = 7, height = 7
  )
}
