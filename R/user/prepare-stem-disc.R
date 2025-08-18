
## Stem disc preparation:
## - wood density for wood
## - wood density for bark
## - group disc quarters

## Requires:
## 1. user inputs
## 2. initiation  script
## 3. load initial data script

## Load preparation scripts ####

if (!"usr" %in% ls())      source("R/user/user-inputs.R")
if (!"nlme_out" %in% ls()) source("R/setup/init.R")
if ("data_init" %in% ls() & length(data_init) == 0) source("R/setup/get-data.R")

tmp <- list()

## Group disc quarters ####
nrow(data_init$stem_disc)
table(data_init$stem_disc$quarter_code, useNA = "ifany")
table(data_init$stem_disc$disc_quarter, useNA = "ifany")
names(data_init$stem_disc)

tmp$sd_single <- data_init$stem_disc |>
  filter(disc_quarter == "Single" | is.na(disc_quarter)) |>
  select(-disc_quarter, -fresh_wt_QOB, - fresh_wt_QUB, -dry_wt_QUB, -quarter_code)


tmp$sd_quarter_field <- data_init$stem_disc |>
  filter(!(disc_quarter == "Single" | is.na(disc_quarter))) |>
  filter(!(is.na(fresh_wt_OB) & is.na(fresh_wt_UB))) |>
  select(
    no, serial, updated_tree_code, species_code, date, disc_no, dia_OB, dia_UB, 
    fresh_wt_OB, fresh_wt_UB
    )

tmp$sd_quarter_lab <- data_init$stem_disc |>
  filter(!(disc_quarter == "Single" | is.na(disc_quarter))) |>
  select(
  updated_tree_code, disc_no, fresh_wt_QOB, fresh_wt_QUB, dry_wt_QUB, quarter_code,
  dry_wt_UB, vol_OB, vol_UB, fresh_wt_lab_bark, dry_wt_lab_bark
  )

## Checks
nrow(tmp$sd_single) + nrow(tmp$sd_quarter_lab) == nrow(data_init$stem_disc)

nrow(tmp$sd_quarter_field)
nrow(tmp$sd_quarter_lab)


nrow(tmp$sd_single) + nrow(tmp$sd_quarter_field)

data_init$stem_disc |> distinct(updated_tree_code, disc_no) |> nrow()

data_clean$stem_disc <- data_init$stem_disc |>
  
  mutate(
  
  )
