
# if (!"usr" %in% ls())      source("R/user/user-inputs.R")
# if (!"nlme_out" %in% ls()) source("R/setup/init.R")
# if (length(data_init) == 0) source("R/setup/get-data.R")
# if (!("stem_log" %in% names(data_clean))) source("R/user/01-prepare-stem-log.R")
# if (!("hr" %in% names(data_clean$stem_log))) source("R/user/02-add-stem-log-profile.R")

tmp <- list()


##
## Corrections ####
##

## + Update clean stems by removing buttress Diam measurement ####
tmp$stem_log <- data_clean$stem_log |>
  mutate(
    log_diam_ob = case_when(
      updated_tree_code == "227Lp036" & log_no == 1 ~ 21.8,
      updated_tree_code == "361Cs054" & log_no == 1 ~ 12.3,
      updated_tree_code == "460An054" & log_no == 1 ~ 15.8,
      TRUE ~ log_diam_ob
    )
  )


## 
## Make log table ####
##

tmp$log_base <- tmp$stem_log |>
  select(
    updated_tree_code, log_no, 
    log_base_pom, 
    log_base_diam_ob = log_diam_ob, 
    log_base_diam_ub = log_diam_ub
    )

stem_logv <- tmp$stem_log |>
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

## Checks
summary(stem_logv)
# tmp$check <- stem_logv |> filter(is.na(log_base_pom))
# tt <- tmp$check2 <- stem_logv |> filter(updated_tree_code == "227Lp036")
# tt <- tmp$check2 <- tmp$log_base |> filter(updated_tree_code == "227Lp036")

##
## Checks ####
##

## + Check log length ####
gg <- stem_logv |>
  ggplot(aes(x = log_base_diam_ob, y = log_length)) +
  geom_point() +
  facet_wrap(~tree_species_group)
print(gg)

tt <- tmp$check_length <- stem_logv |>
  filter(log_length > 2) 

tmp$check_length |> pull(updated_tree_code) |> unique() |> sort()
## >> 177Ta018 tree broke during felling resulting in 5 m with no data

## + Check log volume ####
stem_logv |>
  filter(log_length <=1.5 ) |>
  ggplot(aes(x = log_base_diam_ob, y = log_vob)) +
  geom_point(aes(color = log_length)) +
  facet_wrap(~tree_species_group)

stem_logv |>
  filter(log_length <=1.5, tree_species_code == "Sr") |>
  ggplot(aes(x = log_base_diam_ob, y = log_vob)) +
  geom_point(aes(color = log_length)) +
  facet_wrap(~log_meas_type) +
  scale_color_viridis_c()

## + Check log wood volume ####
stem_logv |>
  ggplot(aes(x = log_base_diam_ob, y = log_vub)) +
  geom_point() +
  facet_wrap(~tree_species_group)

## Aborted, only 3 values concerned, they were removed
# tmp$vec_missing <- stem_logv |> 
#   filter(is.na(log_base_diam_ub)) |>
#   pull(updated_tree_code)
# tmp$vec_missing
# 
# stem_logv |>
#   filter(updated_tree_code %in% tmp$vec_missing) |>
#   ggplot(aes(x = log_vob, y = log_vub)) +
#   geom_point() +
#   facet_wrap(~updated_tree_code)
# 
# ## >> fill log_vub based on linear projection
# dat_060 <- stem_logv |> filter(updated_tree_code == "060An006")
# lm_060 <- lm(log_vub ~ log_vob, data = dat_060)


## + check V bark
stem_logv |>
  ggplot(aes(x = log_vob, y = log_vbark)) +
  geom_point() +
  facet_wrap(~tree_species_group)

##
## Assign clean table ####
##

data_clean$stem_log <- stem_logv

gg <- data_clean$stem_log |>
  filter(log_length <=2 ) |>
  ggplot(aes(x = log_base_diam_ob, y = log_vob)) +
  geom_point() +
  facet_wrap(~tree_species_group)

print(gg)

rm(stem_logv, tmp, gg)
