# TODO: Clean this up
script_dir <- Sys.getenv(script_env)
data_dir <- Sys.getenv(input_env)
output_dir <- Sys.getenv(output_env)
figure_dir <- Sys.getenv("GP_PLOTTING_FOLDER")
table_dir <- ""

# NOTE: We use an older version of bnlearn
# Available by running: install.packages("https://cran.r-project.org/src/contrib/Archive/bnlearn/bnlearn_4.9.tar.gz")
# we do this to avoid having to upgrade our r version (bnlearn 5.0 requires r 4.4, debian stable has 4.2)

# required libs
load_lib_quietly("dplyr")
load_lib_quietly("tidyr")
load_lib_quietly("ggplot2")
load_lib_quietly("mipfp")
load_lib_quietly("purrr") # provides reduce
load_lib_quietly("bnlearn")
load_lib_quietly("tibble")
load_lib_quietly("BiocManager") # following packages are dependencies
load_lib_quietly("Rgraphviz") # of Rgraphviz
load_lib_quietly("gRain")
load_lib_quietly("xtable")

# Loads the scripts that do the reading and replacing
source(file.path(script_dir, "gp_data_read.r"))
source(file.path(script_dir, "aggregate_data_read.r"))
source(file.path(script_dir, "experiment_helpers.r"))
source(file.path(script_dir, "data_mapping.r"))
source(file.path(script_dir, "humat_mapping_tools.r"))
source(file.path(script_dir, "parameterized_humat_mapping.r"))
source(file.path(script_dir, "ipf_functions.r"))
source(file.path(script_dir, "analysis_helpers.r"))

# Returns the configred table directory. Returns an empty string if no directory
# has been configured.
# TODO: duplicate, clean up
get_table_dir_from_env <- function() {
  figure_dir <- Sys.getenv("GP_PLOTTING_FOLDER")
  # if we have a figure output folder, set the script to write to it
  if (nchar(figure_dir) != 0) {
    return(file.path(figure_dir, "tables"))
  }
  return("")
}

generate_bn_structure <- function(base_population, blacklist) {
  # tenancy should not be affected by dwelling related variables

  # NOTE: The hc algorithm does not take order of levels into account (ordinals)
  network <- bnlearn::tabu(bn_seed, blacklist = blacklist)

  # network only gives us the structure of the graph, but not the probabilities.
  # the next step learns the probabilites and produces the fitted graph
  fitted <- bn.fit(network, bn_seed)
}

generate_bn_populations <- function(
    base_population,
    target_marginals,
    pop_output_dir = "",
    write_summary_tables = TRUE,
    table_output_file_suffix = "bn",
    population_file_suffix = "bn") {
  source_data <- base_population
  plotting_folder <- Sys.getenv(plot_env)

  # get/set the dwelling factor if necessary
  # TODO: check whether this is done already and skip if possible
  source_data$dwelling_factor <- dwelling_factor(
    source_data$energielabel, source_data$bouwjaar
  )

  # generate a Bayesian network from the population for the given variables
  bn_seed <- source_data |>
    select(
      houding_verduurzaming,
      woonsituatie,
      aanwezigheid_duurzame_interventies,
      dwelling_factor
    )

  # generate white/blacklists
  blacklist <- data.frame(
    from = c("aanwezigheid_duurzame_interventies", "dwelling_factor"),
    to = c("woonsituatie", "woonsituatie")
  )

  # fit the network structure
  fitted <- generate_bn_structure(bn_seed, blacklist)

  # visualize the network graph and write it to file
  pdf(file.path(plotting_folder, "bayesian_network.pdf"))
  graphviz.chart(fitted)
  graphviz.plot(fitted, fontsize = 40)
  dev.off()

  # also add a copy to the archive
  archive_name <- generate_file_name("bayesian_network", file_ext = ".pdf")
  pdf(file.path(plotting_folder, "archive", archive_name))
  graphviz.chart(fitted)
  graphviz.plot(fitted, fontsize = 40)
  dev.off()

  # output folder
  humat_output_folder <- file.path(pop_output_dir, "humat_config")
  if (!create_folder_if_needed(humat_output_folder)) {
    print(paste0("Could not create folder: ", humat_output_folder))
    error("Unable to create directory for humat output, exiting.")
  }

  # check for undirected arcs and report on it
  if (length(undirected.arcs(network)) > 0) {
    print("Learned network structure contains undirected arcs.")
    print("Cannot generate samples without intervention, skipping it.")
  } else {
    # TODO: Consider rejection sampling for better population fits

    # we format the conditional probabilities in the zones of interest data
    target_marginals <- target_marginals |>
      mutate(p_rent = rent_percent / 100) |>
      mutate(p_own = own_percent / 100)

    # add renter/owner counts for validation
    target_marginals <- target_marginals |>
      mutate(renters = p_rent * households) |>
      mutate(owners = (1 - p_rent) * households)

    # use the BN to generate a population of N
    bn_sample <- rbn(fitted, 100)

    # give the agents id's
    bn_sample$agent_id <- paste0("bayesian", seq.int(nrow(bn_sample)))

    # convert the population to HUMAT agents
    bn_agents <- humat_convert_data_to_agents(bn_sample)

    # write the population to file
    bn_output_file_name <- generate_file_name("random", population_file_suffix)
    bn_output_file_path <- file.path(pop_output_dir, bn_output_file_name)

    # collect aggregates on the synthetic populations
    synth_aggs <- tibble()

    # sample using the aggregates and create populations
    # TODO: Vectorize
    for (idx in seq_len(nrow(target_marginals)))
    {
      target <- target_marginals[idx, ]

      # create an adapted network by adding known marginals as probability
      # TODO: check whether this is methodologically valid
      adapted_fitted <- fitted
      p_tenancy <- c(target$p_rent, 1 - target$p_rent)
      adapted_fitted$woonsituatie <- matrix(
        p_tenancy,
        ncol = 2, dimnames = list(NULL, c("huurder", "eigenaar"))
      )

      # sample the adapted population according to its desired size
      bn_adapted_sample <- rbn(adapted_fitted, target$households)

      # record the aggregate statistics
      synth_aggs <- synth_aggs |> bind_rows(
        bn_adapted_sample |> rename(group = woonsituatie) |> count(group)
          |>
          mutate(source = target$name) |>
          mutate(households_fit = nrow(bn_adapted_sample)) |>
          spread(group, n)
      )

      # add id's to the population
      bn_adapted_sample$agent_id <- paste0(
        "bayesian_", target$name, "_", seq.int(nrow(bn_adapted_sample))
      )

      # write the population basis to file
      bn_output_file_name <- generate_file_name(
        target["name"], population_file_suffix
      )
      bn_output_path <- file.path(pop_output_dir, bn_output_file_name)
      print(paste0("Writing BN population to: ", bn_output_path))
      write.csv(bn_adapted_sample, bn_output_path, row.names = FALSE)

      # convert the population to HUMAT agents
      bn_agents <- humat_convert_data_to_agents(bn_adapted_sample)

      # write the HUMAT population to file
      bn_adapted_output_file_name <-
        generate_file_name(
          "agent_generation_bn", target$name,
          suffix = population_file_suffix
        )
      bn_adapted_output_file_path <-
        file.path(humat_output_folder, bn_adapted_output_file_name)
      write.csv(bn_agents, bn_adapted_output_file_path, row.names = FALSE)
    }
  }

  bn_write_summary_tables <- function(synth_aggs, target_marginals, table_output_file_suffix = "bn") {
    table_dir <- get_table_dir_from_env()
    # evaluate the population fitness
    # combine the fitted aggregates with the constraints
    validation_table <- target_marginals |>
      select(
        name, households, renters, owners
      ) |>
      inner_join(synth_aggs, by = c("name" = "source"))

    # separate the constraints and fitted values as a convenience
    constraints <- validation_table |>
      select(
        households, renters, owners
      )
    fitted <- synth_aggs |>
      select(
        households_fit, huurder, eigenaar
      )

    ## Error metrics per zone
    # this is essentially just a loop
    zonewise_cors <- unlist(lapply(1:nrow(constraints), function(idx) {
      cor(t(constraints[idx, ]), t(fitted[idx, ]), method = "pearson")
    })) |> setNames(validation_table$name)

    # calculate TAE and RE for constraints/aggregates
    zonewise_re <- unlist(lapply(1:nrow(constraints), function(idx) {
      RE(constraints[idx, ], fitted[idx, ], constraints$households[idx])
    })) |> setNames(validation_table$name)

    # calculate the RMSE of the constraints/fitted
    zonewise_rmse <- unlist(lapply(1:nrow(constraints), function(idx) {
      RMSE(constraints[idx, ], fitted[idx, ])
    })) |> setNames(validation_table$name)

    # combine the fit metrics and export to a table
    combined_zone_metrics <- data.frame(zonewise_cors, zonewise_re, zonewise_rmse)
    colnames(combined_zone_metrics) <- c("Pearson's R", "RE", "RMSE")
    combined_zone_metrics <- rownames_to_column(combined_zone_metrics, "zone")
    zone_fit_table_path <-
      file.path(
        table_dir,
        paste0("zone_fit_errors", table_output_file_suffix, ".tex")
      )
    print(
      xtable(
        combined_zone_metrics |> arrange(zone),
        caption = paste0(
          "Table of goodness-of-fit measures for Bayesian network synthetic populations ",
          " as compared to the constraint aggregates computed per zone. ",
          "Listed are the RMSE, TAE and Pearson's $\rho$"
        ),
        label = paste0("tbl:bn_gof"),
        align = "llrrr"
      ),
      file = zone_fit_table_path,
      include.rownames = FALSE,
      sanitize.colnames.function = identity
    )

    # give a table as an example of what the fit is based on
    fit_example <- rbind(
      c(name = validation_table$name[1], constraints[1, ]),
      c(name = paste0(validation_table$name[1], " fitted"), fitted[1, ])
    ) |> data.frame()
    example_table_path <- file.path(
      table_dir, paste0("example_fit", table_output_file_suffix, ".tex")
    )
    print(xtable(
      fit_example |> rename(zone = name) |> select(zone, households, renters, owners),
      caption = paste0("Partial example of zone constraints and Bayesian network fitted values."),
      label = paste0("tbl:bn_fit_example"),
      align = "llrrr"
    ), file = example_table_path, include.rownames = FALSE)

    # Error metrics by variable
    # this is essentially just a loop
    varwise_cors <- unlist(lapply(1:ncol(constraints), function(idx) {
      cor((constraints[, idx]), (fitted[, idx]), method = "pearson")
    })) |> setNames(colnames(constraints))

    # calculate TAE and RE for constraints/aggregates
    varwise_re <- unlist(lapply(1:ncol(constraints), function(idx) {
      RE(constraints[, idx], fitted[, idx], sum(constraints[, idx]))
    })) |> setNames(colnames(constraints))

    # calculate the RMSE of the constraints/fitted
    varwise_rmse <- unlist(lapply(1:ncol(constraints), function(idx) {
      RMSE(constraints[, idx], fitted[, idx])
    })) |> setNames(colnames(constraints))

    combined_var_metrics <- data.frame(varwise_cors, varwise_re, varwise_rmse)
    var_fit_table_path <- file.path(
      table_dir, paste0("var_fit_errors", table_output_file_suffix, ".tex")
    )
    print(xtable(combined_var_metrics), file = var_fit_table_path)
  }
}
