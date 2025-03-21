load_lib_quietly("ggplot2")
load_lib_quietly("dplyr")

#### Plots for report
# nicely formatted summary graph of agent choice, including legend and titles
# computes averages per time-step for the given behaviorspace runs.
# assumes that the population size is equivalent for the purpose of the title
# counts
plot_humat_choice_summary <- function(bspace, condition = "") {
  bspace |>
    group_by(X.step.) |>
    summarise(across(where(is.numeric), mean)) |>
    # sum the true dilemma's to indicate only true dilemma's
    mutate(
      dilemma_count =
        True.Non.Social.Dilemma.Count + True.Social.Dilemma.Count +
          True.Dual.Dilemma.Count
    ) |>
    # select statistics relevant to population choice
    select(
      Optin.Count,
      Fully.Convinced.Count,
      dilemma_count,
      Household.Count,
      X.step.
    ) |>
    # normalize counts to obtain population rates
    mutate(
      across(
        c(Optin.Count, Fully.Convinced.Count, dilemma_count),
        ~ .x / Household.Count
      )
    ) |>
    select(!Household.Count) |>
    # turn data to long form for easier plot generation
    pivot_longer(cols = !X.step.) |>
    ggplot() +
    geom_line(aes(x = X.step., y = value, colour = name)) +
    xlim(1, NA) +
    # add labels
    labs(
      x = "Time step (0-52)",
      y = "Proportion of population",
      title = paste0("Humat choice development", ifelse(condition != "", paste0(" (", condition, ")"), "")),
      subtitle = paste0(
        "Averages taken over ", length(unique(bspace$X.run.number.)), " runs. Population ",
        "size: ", median(bspace$Household.Count), ". Line indicates ",
        "start communication"
      )
    ) +
    scale_color_discrete(name = "Variable", labels = c(
      "True dilemma",
      "Fully convinced",
      "Prefers opting in"
    )) +
    # add a line indicating the start of communications
    geom_vline(xintercept = 2)
}

plot_humat_evaluation_summary <- function(bspace, condition = "") {
  # base the y_max on the expected sat, for the sake of keeping the same scale
  # across the different plots
  y_max <- ifelse(max(bspace$Average.Population.Expected.Satisfaction, bspace$Average.Sub.Population.Expected.Satisfaction.With.A, bspace$Average.Sub.Population.Expected.Satisfaction.With.B) > 0.5, 1.0, 0.5)
  # nicely formatted summary graph of agent evaluation, including legend and titles
  bspace |>
    # take averages at each timestep
    group_by(X.step.) |>
    summarise(across(where(is.numeric), mean)) |>
    mutate(dilemma_count = Household.Count - Non.Dilemma.Count) |>
    select(
      contains("Evaluation") & (starts_with("Average.Population") |
        starts_with("Average.Sub.Population")),
      X.step.
    ) |>
    pivot_longer(cols = !X.step.) |>
    ggplot() +
    geom_line(aes(x = X.step., y = value, colour = name)) +
    xlim(1, NA) +
    labs(
      x = "Time step (0-52)",
      y = "Average of population",
      title = paste0("Humat evaluation (E(x)) development", ifelse(condition != "", paste0(" (", condition, ")"), "")),
      subtitle = paste0(
        "Averages taken over ", length(unique(bspace$X.run.number.)), " runs. Population ",
        "size: ", median(bspace$Household.Count), ". Line indicates ",
        "start communication"
      )
    ) +
    scale_color_discrete(name = "Group", labels = c(
      "Global",
      "Humats with opt-in preference",
      "Humats with opt-out preference"
    )) +
    ylim(0, y_max) +
    # add a line indicating the start of communications
    geom_vline(xintercept = 2)
}

plot_humat_satisfaction_summary <- function(bspace, condition = "") {
  y_max <- ifelse(max(bspace$Average.Population.Expected.Satisfaction, bspace$Average.Sub.Population.Expected.Satisfaction.With.A, bspace$Average.Sub.Population.Expected.Satisfaction.With.B) > 0.5, 1.0, 0.5)
  # nicely formatted summary graph of agent evaluation, including legend and titles
  bspace |>
    # take averages at each timestep
    group_by(X.step.) |>
    summarise(across(where(is.numeric), mean)) |>
    select(
      contains("Satisfaction") & (starts_with("Average.Population") |
        starts_with("Average.Sub.Population")),
      X.step.
    ) |>
    pivot_longer(cols = !X.step.) |>
    ggplot() +
    geom_line(aes(x = X.step., y = value, colour = name)) +
    xlim(1, NA) +
    labs(
      x = "Time step (0-52)",
      y = "Average of population",
      title = paste0("Humat expected satisfaction (S'(x)) development", ifelse(condition != "", paste0(" (", condition, ")"), "")),
      subtitle = paste0(
        "Averages taken over ", length(unique(bspace$X.run.number.)), " runs. Population ",
        "size: ", median(bspace$Household.Count), ". Line indicates ",
        "start communication"
      )
    ) +
    scale_color_discrete(name = "Group", labels = c(
      "Global",
      "Humats with opt-in preference",
      "Humats with opt-out preference"
    )) +
    ylim(0, y_max) +
    # add a line indicating the start of communications
    geom_vline(xintercept = 2)
}

plot_humat_communication_summary <- function(bspace, condition = "") {
  # communication statistics graph (comms/household/timestep)
  bspace |>
    # take averages at each timestep
    group_by(X.step.) |>
    summarise(across(where(is.numeric), mean)) |>
    # we take the incremental values instead of the totals for the communications
    mutate(
      across(contains("Total"), ~ c(0, diff(.x)))
    ) |>
    select(
      Total.Communications,
      Total.Communications.A,
      Total.Communications.B,
      Household.Count,
      X.step.
    ) |>
    # we want the relative rate at any given time step, not the counts
    mutate(
      across(
        c(Total.Communications, Total.Communications.A, Total.Communications.B),
        ~ .x / Household.Count
      )
    ) |>
    select(!Household.Count) |>
    pivot_longer(cols = !X.step.) |>
    filter(X.step. > 1) |>
    ggplot() +
    xlim(0, NA) +
    ylim(0, 0.5) +
    labs(
      x = "Time step (0-52)",
      y = "Communications per humat per timestep",
      title = paste0("Humat communication rates", ifelse(condition != "", paste0(" (", condition, ")"), "")),
      subtitle = paste0(
        "Averages taken over ", length(unique(bspace$X.run.number.)), " runs. Population ",
        "size: ", median(bspace$Household.Count), ". Line indicates "
      )
    ) +
    geom_line(aes(x = X.step., y = value, colour = name)) +
    # add a line indicating the start of communications
    geom_vline(xintercept = 2) +
    geom_text(aes(
      label = "start communication (t = 2)",
      x = 1, y = 0.3, angle = 90
    ), check_overlap = TRUE) +
    scale_color_discrete(name = "Group", labels = c(
      "Global",
      "Humats with opt-in preference",
      "Humats with opt-out preference"
    ))
}

# generates a figure of four plots that characterize the behaviourspace over time
multiplot_characteristics_bspace <- function(bspace, condition = "") {
  annotate_figure(
    annotate_figure(
      ggarrange(
        # bspace |> plot_humat_evaluation_summary() + labs(subtitle = NULL),
        bspace |> plot_humat_satisfaction_summary() + labs(subtitle = NULL),
        bspace |> plot_humat_communication_summary() + labs(subtitle = NULL),
        ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom"
      ),
      top = paste0(
        "Start of communication indicated by vertical. N(humats) = ",
        median(bspace$Household.Count), ". N(runs) = ",
        length(unique(bspace$X.run.number.))
      )
    ),
    top = paste0(
      "Population dynamics over time",
      ifelse(condition != "", paste0(" (", condition, ")"), "")
    )
  )
}

# plot for the humat choice summary in bar-style, to compare across multiple
# conditions will split into group by condition, this must be available
plot_summary_bars <- function(summary) {
  longer <- summary |> pivot_longer(!condition)
  longer |>
    ggplot(aes(x = condition, y = value, fill = name)) +
    geom_col(position = position_dodge(1)) +
    ylim(0.0, 1)
}

# returns a ggplot element with a scale fill for choice summary plots
choice_scale_fill <- function() {
  scale_fill_discrete(
    name = "Metric",
    labels = c(
      "Convinced",
      "Prefers A (opt-in)",
      "Positive evaluation",
      "True dilemma"
    )
  )
}

# returns a ggplot element with labels for choice summary plots
choice_labs <- function() {
  labs(
    title = "Population choice summary",
    x = "Experiment condition",
    y = "Ratio of population",
  )
}

comms_scale_fill <- function() {
  scale_fill_discrete(
    name = "Metric",
    labels = c(
      "All communications",
      "Inquiries",
      "Signals"
    )
  )
}

comms_group_scale_fill <- function() {
  scale_fill_discrete(
    name = "Metric",
    labels = c(
      "Inquiries (opt-in)",
      "Inquiries (opt-out)",
      "Signals (opt-in)",
      "Signals (opt-out)"
    )
  )
}

satisfaction_group_scale_fill <- function() {
  scale_fill_discrete(
    name = "Metric",
    labels = c(
      "Global S'(x)",
      "Opt-in S'(x)",
      "Opt-out S'(x)"
    )
  )
}

comms_labs <- function() {
  labs(
    title = "Communication summary",
    x = "Experiment condition",
    y = "Communications per humat per time step",
  )
}

satisfaction_labs <- function() {
  labs(
    title = "Humat satisfaction summary",
    x = "Experiment condition",
    y = "Expected satisfaction/Evaluation",
  )
}

plot_choice_summary <- function(summary) {
  summary |> plot_summary_bars() + choice_scale_fill() + choice_labs()
}

plot_comm_summary <- function(summary) {
  summary |> plot_summary_bars() + comms_scale_fill() + comms_labs()
}

plot_group_comm_summary <- function(summary) {
  summary |> plot_summary_bars() + comms_group_scale_fill() + comms_labs() +
    labs(title = "Groupwise communication summary")
}

plot_satisfaction_summary <- function(summary) {
  summary |> plot_summary_bars() + satisfaction_group_scale_fill() + satisfaction_labs() + ylim(-0.5, 0.5)
}
