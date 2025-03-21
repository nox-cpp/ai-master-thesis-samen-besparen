load_lib_quietly("dplyr")
load_lib_quietly("tidyr")

# requires the mapping base to have the correct shape
# TODO: generate the base as a function
generate_mapping_configurations <- function(mapping_base)
{
  # generate the mapping permutations
  positive_mapping <- c(-1, 0, 1)
  negative_mapping <- rev(positive_mapping)
  positive_mapping_short <- c(-1, 1)
  negative_mapping_short <- rev(positive_mapping_short)
  exclusion_mapping <- c(0, 0, 0)
  mapping_names <- c('+', '0', '-', '+', '-')
  mappings <- list(positive_mapping, exclusion_mapping, negative_mapping, positive_mapping_short, negative_mapping_short)
  
  # generate the permutations (-1 = negative map, 0 = excluded, 1 = positive map, 4 = short pos, 5 = short neg)
  combinations <- expand.grid(1:3, c(4,5), 1:3, c(4,5), 1:3)
  # build up a list for the new row
  for (idx in 1:nrow(combinations)) {
    # convert it to vector form
    config <- as.numeric(combinations[idx,])
    # this gets us some defaults
    new_row <- as.list(mapping_base[1,])
    new_row[["cfg_id"]] <- paste0("var_exp", paste0(mapping_names[config], collapse=""))
    # then we map the variables based on the config
    # NOTE: these indices are dependent on the mapping config spec
    new_row[15:27] <- unlist(mappings[config])
    mapping_cfg <- mapping_cfg |> rbind(new_row)
  }
  mapping_cfg
}

generate_importance_mapping_configurations <- function(mapping_base) {
  mean_space <- c(0.25, 0.75)
  sdev_space <- c(0, 0.25, 0.5)
  mappings <- c(mean_space, sdev_space)
  
  # each configuration gets a (shorthand) descriptive id
  mapping_names <- c(
    "qm", "tqm", "zs", "qs", "hs")
  
  # compute all the permutations
  combinations <- 
    expand.grid(c(1,2), 3:5, c(1,2), 3:5, c(1,2), 3:5, c(1,2), 3:5)
  
  # generate the complete config from the permutations
  mapping_cfg <- mapping_base
  for (idx in 1:nrow(combinations)) {
    # convert the config to vector form
    config <- as.numeric(combinations[idx, ])
    # get the default values
    new_row <- as.list(mapping_base[1, ])
    # generate the config id
    new_row[["cfg_id"]] <- paste0("imp_map_", paste0(mapping_names[config], collapse="_"))
    # map the variables based on the config
    # NOTE: these indices are dependent on the mapping config spec
    new_row[7:14] <- mappings[config]
    # add the config to the set
    mapping_cfg <- mapping_cfg |> rbind(new_row)
  }
  
  # return all the mappings
  mapping_cfg
}

generate_aspiration_mapping_configurations <- function(mapping_base) {
  mean_space <- c(0, 0.25, 0.5, 0.75, 1)
  sdev_space <- c(0, 0.25, 0.5)
  mappings <- c(mean_space, sdev_space)
  
  # each configuration gets a (shorthand) descriptive id
  mapping_names <- c(
    "zm", "qm", "hm", "tqm", "om", "zs", "qs", "hs")
  
  # compute all the permutations
  combinations <- 
    expand.grid(1:5, 6:8)
  
  # generate the complete config from the permutations
  mapping_cfg <- mapping_base
  for (idx in 1:nrow(combinations)) {
    # convert the config to vector form
    config <- as.numeric(combinations[idx, ])
    # get the default values
    new_row <- as.list(mapping_base[1, ])
    # generate the config id
    new_row[["cfg_id"]] <- paste0("asp_map_", paste0(mapping_names[config], collapse="_"))
    # map the variables based on the config
    # NOTE: these indices are dependent on the mapping config spec
    new_row[2:3] <- mappings[config]
    # add the config to the set
    mapping_cfg <- mapping_cfg |> rbind(new_row)
  }
  
  # return all the mappings
  mapping_cfg
}
