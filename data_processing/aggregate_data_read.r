# Reads in the table with aggregates from the given path
load_aggregates_table_from_path <- function(file_path) {
  read.csv(file = file_path, header = TRUE)
}

# Reads in the table with aggregates for the given year
load_aggregates_table_of_year <- function(year, aggregates_folder) {
  # note: makes assumption of file names
  table_path <- file.path(
    aggregates_folder, paste0("openinfo_areas_", year, ".csv")
  )
  load_aggregates_table_from_path(table_path)
}

# Reads in the default table with aggregates
load_aggregates_table <- function(aggregates_folder) {
  # select the latest `areas' file in the folder
  files <- dir(path = aggregates_folder)
  files <- files[grepl("areas", files)]
  # this `sort' assumes that the files end in the (full) year
  latest <- first(sort(files, decreasing = TRUE))
  # return the table from the latest file
  load_aggregates_table_from_path(
    file_path = file.path(aggregates_folder, latest)
  )
}

percent_as_numeric <- function(data) {
  as.numeric(as.character(sub("%", "", data)))
}

# Takes in aggregate data as is available in the OpenInfo dataset and adapts it
# such that it can be used as input for the IPF generation function
adapt_aggregates_to_make_targets <- function(aggregate_data) {
  adapted_aggregates <- aggregate_data |>
    mutate(households = Huishoudens.totaal) |>
    mutate(rent_percent = Huurwoningen.totaal) |>
    mutate(own_percent = Koopwoningen) |>
    mutate(own_percent = percent_as_numeric(own_percent)) |>
    mutate(rent_percent = percent_as_numeric(rent_percent)) |>
    mutate(b_plus = Energielabels.B + Energielabels.A + Energielabels.A.. + Energielabels.A... + Energielabels.A.... + Energielabels.A.....) |>
    mutate(c_d = Energielabels.C + Energielabels.D) |>
    mutate(e_minus = Energielabels.E + Energielabels.F + Energielabels.G) |>
    mutate(pre_1970 = Panden.voor.1700 + Panden.1700.tot.1900 + Panden.1900.tot.1925 + Panden.1925.tot.1950 + Panden.1950.tot.1970) |>
    mutate(mid_1970_1999 = Panden.1970.tot.1980 + Panden.1980.tot.1990 + Panden.1990.tot.2000) |>
    mutate(post_2000 = Panden.2000.tot.2010 + Panden.2010.tot.2020 + Panden.2020.en.later) |>
    filter(!(is.na(rent_percent) | is.na(own_percent) | is.na(households) | households == 0)) |>
    filter(!(Soort.regio == "Land" | Soort.regio == "Provincie")) |>
    mutate(renter = round(rent_percent * households / 100)) |>
    mutate(owner = round(own_percent * households / 100)) |>
    mutate(name = Regionaam)

  adapted_aggregates
}

# takes a data frame with multiple areas and combines them into a summary area
create_summary_area <- function(areas, name) {
  areas <- areas |>
    mutate(Percentage.eengezinswoning = percent_as_numeric(Percentage.eengezinswoning)) |>
    mutate(Percentage.meergezinswoning = percent_as_numeric(Percentage.meergezinswoning)) |>
    mutate(Percentage.bewoond = percent_as_numeric(Percentage.bewoond)) |>
    mutate(Percentage.onbewoond = percent_as_numeric(Percentage.onbewoond)) |>
    mutate(Percentage.woningen.met.stadsverwarming = percent_as_numeric(Percentage.woningen.met.stadsverwarming)) |>
    mutate(Percentage.werknemers = percent_as_numeric(Percentage.werknemers)) |>
    mutate(Percentage.zelfstandigen. = percent_as_numeric(Percentage.zelfstandigen.)) |>
    mutate(X40..personen.met.laagste.inkomen = percent_as_numeric(X40..personen.met.laagste.inkomen)) |>
    mutate(X20..personen.met.hoogste.inkomen = percent_as_numeric(X20..personen.met.hoogste.inkomen)) |>
    mutate(X40..huishoudens.met.laagste.inkomen = percent_as_numeric(X40..huishoudens.met.laagste.inkomen)) |>
    mutate(X20..huishoudens.met.hoogste.inkomen = percent_as_numeric(X20..huishoudens.met.hoogste.inkomen)) |>
    mutate(Huishoudens.met.een.laag.inkomen = percent_as_numeric(Huishoudens.met.een.laag.inkomen)) |>
    mutate(Huishoudens.onder.of.rond.sociaal.minimum = percent_as_numeric(Huishoudens.onder.of.rond.sociaal.minimum)) |>
    mutate(Huishoudens.tot.110..van.sociaal.minimum = percent_as_numeric(Huishoudens.tot.110..van.sociaal.minimum)) |>
    mutate(Huishoudens.tot.120..van.sociaal.minimum = percent_as_numeric(Huishoudens.tot.120..van.sociaal.minimum)) |>
    mutate(Actieven.15.75.jaar = percent_as_numeric(Actieven.15.75.jaar)) |>
    mutate(Bouwjaar.voor.2000 = percent_as_numeric(Bouwjaar.voor.2000)) |>
    mutate(Bouwjaar.vanaf.2000 = percent_as_numeric(Bouwjaar.vanaf.2000)) |>
    mutate(Netto.arbeidsparticipatie = percent_as_numeric(Netto.arbeidsparticipatie)) |>
    mutate(rent_percent = Huurwoningen.totaal) |>
    mutate(own_percent = Koopwoningen) |>
    mutate(own_percent = percent_as_numeric(own_percent)) |>
    mutate(rent_percent = percent_as_numeric(rent_percent))
  summary <- areas |>
    summarise(
      across(c(households, renter, owner), sum),
      across(c(rent_percent, own_percent), mean),
      across(c(b_plus:post_2000, Woningvoorraad), sum),
      across(c(
        Inwoners,
        Woningvoorraad,
        Opleidingsniveau.laag,
        Opleidingsniveau.middelbaar,
        Opleidingsniveau.hoog,
        Personenauto.s.totaal,
        Personenauto.s..brandstof.benzine,
        Personenauto.s..overige.brandstof,
        Personenauto.s.per.huishouden,
        Personenauto.s.naar.oppervlakte,
        Motorfietsen,
        Oppervlakte.totaal,
        Oppervlakte.land,
        Oppervlakte.water,
        Mate.van.stedelijkheid,
        Omgevingsadressendichtheid,
        Adressen.met.panden,
        Appartement,
        Tussen.of.geschakelde.woning,
        Hoekwoning,
        Tweeonder1kap,
        Vrijstaande.woning
      ), sum),
      across(c(
        # TODO: some of these variables require a different metric than mean
        Bevolkingsdichtheid,
        Percentage.eengezinswoning,
        Percentage.meergezinswoning,
        Percentage.bewoond,
        Percentage.onbewoond,
        Percentage.werknemers,
        Percentage.zelfstandigen.,
        X40..personen.met.laagste.inkomen,
        X20..personen.met.hoogste.inkomen,
        X40..huishoudens.met.laagste.inkomen,
        X20..huishoudens.met.hoogste.inkomen,
        Huishoudens.met.een.laag.inkomen,
        Huishoudens.onder.of.rond.sociaal.minimum,
        Huishoudens.tot.110..van.sociaal.minimum,
        Huishoudens.tot.120..van.sociaal.minimum,
        Actieven.15.75.jaar,
        Bouwjaar.voor.2000,
        Bouwjaar.vanaf.2000,
        Netto.arbeidsparticipatie,
        Gemiddelde.woningwaarde,
        Gemiddeld.elektriciteitsverbruik.totaal,
        Gemiddeld.elektriciteitsverbruik.huurwoning,
        Gemiddeld.elektriciteitsverbruik.eigen.woning,
        Gemiddeld.aardgasverbruik.totaal,
        Gemiddeld.aardgasverbruik.huurwoning,
        Gemiddeld.aardgasverbruik.eigen.woning,
        Aantal.inkomensontvangers,
        Gemiddeld.inkomen.per.inkomensontvanger,
        Gemiddeld.inkomen.per.inwoner
      ), mean),
      across(Mediaan.vermogen.van.particuliere.huishoudens, mean),
    )
  tibble_row(Regiocode = paste0(areas$Regiocode, collapse = "|"), old_names = paste0(areas$name, collapse = "|"), name = name, Soort.regio = "Combination", summary)
}

# filters the areas with the given names from the base and creates a summary entry
create_summary_area_from_names <- function(names, new_name, base) {
  areas <- base |> filter(name %in% names)
  create_summary_area(areas, new_name)
}

# Here we select only those zones that are of interest to the research, as well
# as combining some of the data to more closely match the zones used by GP
filter_zones_of_interest <- function(aggregates) {
  # Haren
  haren_zoi <- c("Haren-Noord", "Haren-Centrum", "Haren-Zuidoost", "Haren-Zuidwest")

  # Lewenborg
  lewenborg_zoi <- c("Lewenborg-Noord", "Lewenborg-West", "Lewenborg-Zuid")

  # Hunze-van Starkenborgh
  hvs_zoi <- c("De Hunze", "Van Starkenborgh")

  # Paddepoel
  paddepoel_zoi <- c("Paddepoel-Zuid", "Paddepoel-Noord")

  # De Wijert
  wijert_zoi <- c("De Wijert-Zuid", "De Wijert")

  # generate and return the summary zones
  tibble(create_summary_area_from_names(haren_zoi, "Haren", aggregates)) |>
    add_row(create_summary_area_from_names(lewenborg_zoi, "Lewenborg", aggregates)) |>
    add_row(create_summary_area_from_names(hvs_zoi, "De Hunze van Starkenborgh", aggregates)) |>
    add_row(create_summary_area_from_names(paddepoel_zoi, "Paddepoel", aggregates)) |>
    add_row(create_summary_area_from_names(wijert_zoi, "De Wijert", aggregates))
}

# Here we select only those zones that are of interest to the research, as well
# as combining some of the data to more closely match the zones used by GP
filter_zones_of_interest_batch_two <- function(aggregates) {
  # Beijum (GP_N: 6000)
  beijum_zoi <- c("Beijum-West", "Beijum-Oost")

  # Helpman (GP_N: 5800)
  helpman_zoi <- "Helpman en omgeving"

  # Korrewegwijk (GP_N: 10300)
  korreweg_zoi <- c("Indische buurt", "Professorenbuurt")

  # Oosterhoogebrug-Ulgersmaborg (GP_N: 1900)
  ohbrug_uborg_zoi <- c("Oosterhoogebrug", "Ulgersmaborg")

  # Oosterpark (GP_N: 7400)
  oosterpark_zoi <- "Oosterparkwijk"

  # Selwerd (GP_N: 3100)
  selwerd_zoi <- "Selwerd"

  # generate and return the summary zones
  tibble(
    create_summary_area_from_names(beijum_zoi, "Beijum", aggregates)
  ) |>
    add_row(create_summary_area_from_names(helpman_zoi, "Helpman", aggregates)) |>
    add_row(create_summary_area_from_names(korreweg_zoi, "Korrewegwijk", aggregates)) |>
    add_row(create_summary_area_from_names(ohbrug_uborg_zoi, "Oosterhoogebrug-Ulgersmaborg", aggregates)) |>
    add_row(create_summary_area_from_names(oosterpark_zoi, "Oosterpark", aggregates)) |>
    add_row(create_summary_area_from_names(selwerd_zoi, "Selwerd", aggregates))
}

select_fields_for_generation <- function(aggregates) {
  aggregates |>
    select(
      Regiocode, name, Soort.regio,
      households, renter, owner, rent_percent, own_percent, b_plus,
      c_d, e_minus, pre_1970, mid_1970_1999, post_2000, Panden, Woningvoorraad
    )
}

load_zones_of_interest <- function(aggregates_folder) {
  # read in the latest table of aggregates
  aggregates <- load_aggregates_table(aggregates_folder)

  # modify the aggregates so they contain the fields that we require
  adapted_aggregates <- adapt_aggregates_to_make_targets(aggregates)

  # select the zones of interest from the aggregates
  filter_zones_of_interest(adapted_aggregates)
}

load_zones_of_interest_batch_two <- function(aggregates_folder) {
  # read in the latest table of aggregates
  aggregates <- load_aggregates_table(aggregates_folder)

  # modify the aggregates so they contain the fields that we require
  adapted_aggregates <- adapt_aggregates_to_make_targets(aggregates)

  # select the zones of interest from the aggregates
  filter_zones_of_interest_batch_two(adapted_aggregates)
}

load_all_sb_zones <- function(aggregates) {
  # modify the aggregates so they contain the fields that we require
  aggregates <- adapt_aggregates_to_make_targets(aggregates)

  # first load the zones of interest
  sb_zones <- filter_zones_of_interest(aggregates)

  # add all the other zones we want here
  sb_zones <- sb_zones |>
    add_row(create_summary_area_from_names(
      c("Beijum-West", "Beijum-Oost"), "Beijum", aggregates
    ) |>
      add_row(create_summary_area_from_names(
        c("Indische buurt", "Professorenbuurt"), "Korrewegwijk", aggregates
      ))) |>
    add_row(create_summary_area_from_names(
      c("Oosterhoogebrug", "Ulgersmaborg"), "Oosterhoogebrug-Ulgersmaborg", aggregates
    )) |>
    add_row(create_summary_area_from_names(
      c("Selwerd", "Selwerderhof"), "Selwerd", aggregates
    )) |>
    add_row(create_summary_area_from_names(
      c("Vinkhuizen-Noord", "Vinkhuizen-Zuid"), "Vinkhuizen", aggregates
    )) |>
    add_row(create_summary_area_from_names(
      c("Noorderplantsoenbuurt", "Oranjebuurt"), "Noorderplantsoenbuurt/Oranjebuurt", aggregates
    )) |>
    add_row(create_summary_area_from_names("Helpman", "Helpman", aggregates)) |>
    add_row(create_summary_area_from_names("Oosterparkwijk", "Oosterpark", aggregates)) |>
    add_row(create_summary_area_from_names("De Held", "De Held", aggregates)) |>
    add_row(create_summary_area_from_names("Glimmen-Onnen-Noordlaren", "Glimmen-Noordlaren-Onnen", aggregates)) |>
    add_row(create_summary_area_from_names("Hoogkerk en omgeving", "Hoogkerk en omgeving", aggregates)) |>
    add_row(create_summary_area_from_names("Kostverloren", "Kostverloren", aggregates)) |>
    add_row(create_summary_area_from_names("Oosterpoort", "Oosterpoort", aggregates)) |>
    add_row(create_summary_area_from_names("Piccardthof", "Piccardthof", aggregates)) |>
    add_row(create_summary_area_from_names("Ten Boer en omgeving", "Ten Boer", aggregates)) |>
    add_row(create_summary_area_from_names("Laanhuizen", "Laanhuizen", aggregates))

  sb_zones
}

find_area <- function(aggregates, name) {
  aggregates |> filter(grepl(name, Regionaam))
}

# TODO: this is incomplete
enrich_ground_truth <- function(ground_truth) {
  # add the relative opt-in rate (opt-in by household according to GP)
  ground_truth$relative_opt_in_2023_gp <-
    ground_truth$results_2023 / ground_truth$household_count_gp
  ground_truth$percent_opt_in_2023_gp <-
    ground_truth$results_2023 / ground_truth$household_count_gp * 100
  ground_truth$relative_opt_in_2024_gp <-
    ground_truth$intermediate_results_2024_21.01.2025 /
      ground_truth$household_count_gp
  ground_truth$percent_opt_in_2024_gp <-
    ground_truth$intermediate_results_2024_21.01.2025 /
      ground_truth$household_count_gp * 100
  ground_truth$relative_opt_in_buyers_2023_gp <-
    ground_truth$results_2023 / ground_truth$owner_household_count_gp
  ground_truth$percent_opt_in_buyers_2023_gp <-
    ground_truth$results_2023 / ground_truth$owner_household_count_gp * 100
  ground_truth$relative_opt_in_buyers_2024_gp <-
    ground_truth$intermediate_results_2024_21.01.2025 /
      ground_truth$owner_household_count_gp
  ground_truth$percent_opt_in_buyers_2024_gp <-
    ground_truth$intermediate_results_2024_21.01.2025 /
      ground_truth$owner_household_count_gp * 100
  # add a column for first year results
  ground_truth <- ground_truth |> mutate(first_year_percent_opt_in_gp = if_else(is.na(percent_opt_in_2023_gp),
    percent_opt_in_2024_gp, percent_opt_in_2023_gp
  ))
  ground_truth <- ground_truth |> mutate(first_year_percent_opt_in_buyers_gp = if_else(is.na(percent_opt_in_2023_gp),
    percent_opt_in_buyers_2024_gp, percent_opt_in_buyers_2023_gp
  ))
  # match the data from the two different input sets
  ground_truth <- ground_truth |> mutate(model_zone = case_when(
    area_name == "Haren" ~ "Haren_Comb",
    area_name == "Paddepoel" ~ "Paddepoel_Comb",
    area_name == "De Wijert" ~ "Wijert_Comb",
    area_name == "Lewenborg" ~ "Lewenborg_Comb",
    area_name == "De Hunze van Starkenborgh" ~ "HvS_Comb"
  ))

  ground_truth
}
