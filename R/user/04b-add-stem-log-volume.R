
if (!"usr" %in% ls())      source("R/user/user-inputs.R")
if (!"nlme_out" %in% ls()) source("R/setup/init.R")
if (length(data_init) == 0) source("R/setup/get-data.R")
if (!("stem_log" %in% names(data_clean))) source("R/user/01-prepare-stem-log.R")
if (!("hr" %in% names(data_clean$stem_log))) source("R/user/02-add-stem-log-profile.R")

tmp <- list()

## !!! Update clean stems by removing buttress Diam measurement !!!
data_clean$stem_log <- data_clean$stem_log |>
  mutate(
    log_diam_ob = case_when(
      updated_tree_code == "227Lp036" & log_no == 1 ~ 21.8,
      updated_tree_code == "361Cs054" & log_no == 1 ~ 12.3,
      updated_tree_code == "460An054" & log_no == 1 ~ 15.8,
      TRUE ~ log_diam_ob
    )
  )

tmp$log_base <- data_clean$stem_log |>
  select(
    updated_tree_code, log_no, 
    log_base_pom = log_base_pom, 
    log_base_diam_ob = log_diam_ob, 
    log_base_diam_ub = log_diam_ub
    )

# tmp$tree_info <- data_clean$stem_log |>
#   select(-starts_with("log_"), -measurement_type, -hr, -dr) |>
#   distinct()

data_clean$stem_logv <- data_clean$stem_log |>
  select(
    updated_tree_code, log_no, 
    log_top_pom = log_base_pom, 
    log_top_diam_ob = log_diam_ob, 
    log_top_diam_ub = log_diam_ub,
    everything()
    ) |>
  group_by(updated_tree_code) |>
  mutate(
    log_no_v = log_no - 1,
    log_no = if_else(log_no == 1, 1, log_no - 1)
    ) |>
  ungroup() |>
  mutate(
    log_base_pom = NA,
    log_base_diam_ob = NA,
    log_base_diam_ub = NA,
  ) |>
  left_join(tmp$log_base, by = join_by(updated_tree_code, log_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    log_base_pom = if_else(log_no_v == 0, 0, log_base_pom),
    log_length = log_top_pom - log_base_pom,
    log_vob = round(pi / 80000 * log_length * (log_base_diam_ob^2 + log_top_diam_ob^2), 4),
    log_vub = round(pi / 80000 * log_length * (log_base_diam_ub^2 + log_top_diam_ub^2), 4),
    log_vbark = log_vob - log_vub,
    ratio_vbark = round(log_vbark / log_vob, 4),
  ) |>
  select(updated_tree_code, starts_with("tree_"), log_no_v, log_length, log_vob, log_vub, log_vbark, starts_with("log_base"), starts_with("log_top"), everything())

# summary(data_clean$stem_logv)
# tmp$check <- data_clean$stem_logv |> filter(is.na(log_base_pom))
# tt <- tmp$check2 <- data_clean$stem_logv |> filter(updated_tree_code == "227Lp036")
# tt <- tmp$check2 <- tmp$log_base |> filter(updated_tree_code == "227Lp036")

data_clean_gg$stem_log_vob <- data_clean$stem_logv |>
  ggplot(aes(x = log_base_diam_ob, y = log_vob)) +
  geom_point() +
  facet_wrap(~tree_species_group)
print(data_clean_gg$stem_log_vob)


## check ratio bark wood
data_clean$stem_logv |>
  ggplot(aes(x = log_vob, y = ratio_vbark)) +
  geom_point() +
  facet_wrap(~tree_species_group)

