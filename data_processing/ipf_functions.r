# algorithm from Ballas et al., code from Lovelace
# Takes in a vector of real numbers, returns a vector of integer numbers based
# on the input vector using the TRS algorithm
# Algorithm based on the one in the book by Lovelace
trs_integerization <- function(weights) {
  # truncate
  int_weights <- floor(weights)
  rest_weights <- weights - int_weights
  deficit <- round(sum(rest_weights))

  # now use replacement to determine which weights gain based on decimal parts
  replacements <- sample(length(weights), size = deficit, prob = rest_weights)
  int_weights[replacements] <- int_weights[replacements] + 1

  # return the integer weights
  int_weights
}

# integerize reweighted data (functions from Lovelace book)
# Takes a vector of real numbers and returns a vector of integer numbers, using
# a proportional integerization algorithm
proportional_integerization <- function(x) {
  sample(length(x),
    size = round(sum(x)),
    prob = x,
    replace = T
  )
}

# Simple function that scales all elements of the vector x to match the sum
# provided in n. This is useful if not all target marginals (x) for IPF match
# the population size (n)
scale_row_sums_to_n <- function(x, n) {
  lapply(x, function(row) {
    (row / sum(row)) * n
  })
}

generate_ipfp_seed_table_from_population <- function(seed_population) {
  # create the seed table (from the base population)
  seed_table <- seed_population |>
    select(woonsituatie, energielabel, bouwjaar) |>
    table()

  # in case there are structural zeroes, set those fields to one
  if (0 %in% seed_table) {
    print(paste(
      "Seed table has structural zeroes, this may impact fit and convergence"
    ))
  }
  seed_table
}

generate_ipfp_target_table <- function(target_marginals) {
  # construct the input marginals for the ipfp function
  tenancy <- c(target_marginals$renter, target_marginals$owner)
  names(tenancy) <- tenancy_factors
  energy_label <- c(
    target_marginals$e_minus,
    target_marginals$c_d,
    target_marginals$b_plus
  )
  names(energy_label) <- energielabel_factors
  building_era <- c(
    target_marginals$pre_1970,
    target_marginals$mid_1970_1999,
    target_marginals$post_2000
  )
  names(building_era) <- bouwjaar_factors

  # convert the marginals to ipf targets (for mipfp)
  target_base <- list(tenancy, energy_label, building_era)
  target_n <- target_marginals$households

  # report the marginals prior to and after scaling
  print(paste0("Marginal sums prior to scaling: ", lapply(target_base, sum)))

  # we apply a scaling to ensure that the marginals are consistent
  # there could be many reasons why the marginals are inconsistent, but they
  # must be consistent prior to fitting
  target <- scale_row_sums_to_n(target_base, target_n)

  # report the marginals post-scaling
  print(paste0("Marginal sums after scaling: ", lapply(target, sum)))

  target
}

ipfp_get_weights_from_result <- function(ipfp_result) {
  print("weights")
  print(ipfp_result$x.hat)

  print("vectorized weights")
  vector_xhat <- unlist(as.list(ipfp_result$x.hat))
  print(vector_xhat)

  vector_xhat
}

ipfp_integerize_weights <- function(weights) {
  # NOTE: this integerization introduces biases
  trs_weights <- trs_integerization(weights)

  # for evaluation, print the difference between TRS weights and rounded weights
  rounded_weights <- round(weights)

  print("Difference between rounded and TRS weights")
  print(trs_weights - rounded_weights)

  trs_weights
}

# creates a vector that contains a name of all the bins in the target table
get_bin_names_from_result <- function(result) {
  paste(
    data.table(result$x.hat)[[1]],
    data.table(result$x.hat)[[2]],
    data.table(result$x.hat)[[3]],
    sep = "."
  )
}

# takes in the generated frequency table and the population from which to sample
# returns a population
sample_population_according_to_table <- function(
    seed_population, fitted_frequencies) {
  # create an empty data frame with the right dimensions
  ipf_population_base <- data.frame(matrix(
    ncol = length(names(seed_population)), nrow =
      0
  ))
  names(ipf_population_base) <- names(seed_population)
  ipf_populations <- ipf_population_base

  # for each bin, sample the required number of individuals from the base
  # population that are a member of that bin
  ipf_populations <- apply(fitted_frequencies, 1, function(x) {
    # check if this bin is present in the source population
    if (sum(seed_population$bin == x[1], na.rm = TRUE) > 0) {
      # this bin is represented, sample the seed population with this bin
      seed_population[
        sample(which(seed_population$bin == x[1]), x[2], replace = TRUE),
      ]
    } else {
      # bin is not represented; sample nothing
      NA
    }
  })

  # if there are any bins that do not have representatives in the base, they
  # cannot be sampled and are NA. Drop those bins
  # Give feedback to the user that this is the case
  if (any(is.na(ipf_populations))) {
    print(paste0(
      "At least one of the bins in the aggregates is not",
      " represented in the base population."
    ))
    # this doesn't make too much sense with how the rest of the code works
    print(paste0(
      "These bins are dropped, resulting in a smaller population if",
      " the bins are part of the targets"
    ))
    ipf_populations <- ipf_populations[!is.na(ipf_populations)]
  }

  # reduce the bin populations to the final generated population
  collapsed_pop <- purrr::quietly(purrr::reduce)(
    ipf_populations, bind_rows)$result

  collapsed_pop
}

# returns an ipf synthetic population based on the seed population and target
# marginals. The seed population is expected to have the form of the `combined -
# data set`, i.e. the uniform micro-data records with complete data (no NA's!)
# The `target_marginals` should be a dataset/list of a single row containing the
# following fields by name:
# the number of `households'
# the number of `owners'
# the number of `b_plus` energy labels
# the number of `c_d` energy labels
# the number of `e_minus` energy labels
# the number of `post_2000` build years
# the number of `mid_1970_1999` build years
# the number of `pre_1970` build years
# these will be the target marginals
# TODO: Vectorize the function
ipfp_population <- function(seed_population, target_marginals) {
  # this argument reports the shape of the marginal data to the ipfp function
  descript <- list(1, 2, 3)

  # get the table with the target aggregates
  target <- generate_ipfp_target_table(target_marginals)

  print("Target table of aggregates:")
  print(target)

  # prepare the seed table
  seed_table <- generate_ipfp_seed_table_from_population(seed_population)

  # perform the reweighting
  ipfp_result <- mipfp::Ipfp(seed_table, descript, target)

  # pre-integerized weights
  weights <- ipfp_get_weights_from_result(ipfp_result)

  # integerize the weights
  integerized_weights <- ipfp_integerize_weights(weights)

  # get names for the bins (makes tables easier to read)
  bin_names <- get_bin_names_from_result(ipfp_result)

  # add names to the bin weights for easy evaluation
  weighted_freq <- tibble(bin_names, integerized_weights)
  print(weighted_freq)

  ## We now have a vector of weights, that is to say counts for each bin (group)
  ## For each group in the seed population we need to sample agents that belong
  ## to these groups a number of times equal to group's weight to get the final
  ## population.
  ## The groups are all possible combinations of tenancy, energy_label and
  ## building_era).
  ## In practice we don't have all this information for each micro-record which
  ## is a bit of a bother. For now, we assume that all micro-data is (or can be)
  ## put in its proper bin, and sample using that.
  ## I.e.: The following code assumes that the input dataset is complete such
  ## that each record can be assigned to a bin.

  # generate id's for each micro-data record
  generated_data <- humat_generate_agent_ids(seed_population)

  # add the bins to the micro-data
  # NOTE: this code is tightly linked with the bin_names
  generated_data <- generated_data |>
    mutate(bin = factor(
      paste(sep = ".", woonsituatie, energielabel, bouwjaar),
      levels = bin_names
    ))

  # TODO: (optional) add copies of each individual that _could_ match the bin to
  # the set with properties matching that bin such that the integer amount is
  # reached. i.e. if the input data is not complete, we add each agent that has
  # a missing variable to each bin it could theoretically fall into.

  # get a vector shape count for the bins
  # (essentially seed_table in vector form)
  ipf_bin_freq <- data.table(seed_table) |>
    transmute(bin = paste(woonsituatie, energielabel, bouwjaar, sep = "."), N)

  # combine the frequencies in the seed population with the ipfp weights
  joined_frequencies <- left_join(
    weighted_freq, ipf_bin_freq,
    by = c("bin_names" = "bin")
  )

  # replace NA's in the table with zeroes (structural zeroes)
  # TODO: this approach is highly impactful on the population fit, a better solution
  # should be explored and if feasible implemented to give the IPF a better shot
  joined_frequencies$N[is.na(joined_frequencies$N)] <- 0
  joined_frequencies <- joined_frequencies |>
    rename(target_freq = integerized_weights, base_freq = N)

  print("Joined frequencies:")
  print(joined_frequencies)

  ## now we are ready to  start sampling the seed population to get the
  ## synthetic population
  sampled_pop <- sample_population_according_to_table(
    generated_data, joined_frequencies
  )
  print("Sampled population")

  # generate different id's for each repeated agent (not done by default)
  sampled_pop <- sampled_pop |>
    mutate(agent_id = make.unique(agent_id, sep = "_"))

  print("Generated agent id's")

  # return the population
  sampled_pop
}
