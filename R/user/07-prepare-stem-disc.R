
## Stem disc preparation:
## - group disc quarters
## - wood density for wood
## - wood density for bark
## - Check and eliminate outliers
## - Fill with computation or means

##
## Load preparation scripts ####
##

## Requires:
## 1. user inputs
## 2. initiation  script
## 3. load initial data script

if (!"usr" %in% ls())      source("R/user/user-inputs.R")
if (!"nlme_out" %in% ls()) source("R/setup/init.R")
if ("data_init" %in% ls() & length(data_init) == 0) source("R/setup/get-data.R")
#if ("data_clean" %in% ls() & length(data_clean) == 0) source("R/user/prepare-stem-profile.R")

tmp <- list()

##
## Calculate disc level vars ####
##

## + Group disc quarters ####
nrow(data_init$stem_disc)
table(data_init$stem_disc$disc_quarter, useNA = "ifany")
table(data_init$stem_disc$disc_no, useNA = "ifany")
names(data_init$stem_disc)

tmp$sd_single <- data_init$stem_disc |>
  filter(disc_quarter == "Single" | is.na(disc_quarter)) |>
  select(
    updated_tree_code, disc_no, dry_weight_UB_inputed, fresh_wt_OB, fresh_wt_UB, dry_wt_UB, fresh_vol_OB, 
    fresh_vol_UB, fresh_wt_lab_bark, dry_wt_lab_bark
    )

tmp$sd_quarter <- data_init$stem_disc |>
  filter(!(disc_quarter == "Single" | is.na(disc_quarter))) |>
  group_by(updated_tree_code, disc_no, dry_weight_UB_inputed) |>
  summarise(
    fresh_wt_OB = sum(fresh_wt_QOB),
    fresh_wt_UB = sum(fresh_wt_QUB),
    dry_wt_UB = sum(dry_wt_UB), 
    fresh_vol_OB = sum(fresh_vol_OB), 
    fresh_vol_UB = sum(fresh_vol_UB), 
    fresh_wt_lab_bark = sum(fresh_wt_lab_bark), 
    dry_wt_lab_bark = sum(dry_wt_lab_bark),
    .groups = "drop"
  )

stem_disc_init <- bind_rows(tmp$sd_single, tmp$sd_quarter) 

## + Calculate densities and fresh to dry ratios ####
stem_disc <- stem_disc_init |>
  mutate(
    dry_weight_UB_inputed = as.logical(dry_weight_UB_inputed),
    tree_species_code = str_sub(updated_tree_code, 4, 5),
    disc_code = disc_no, 
    disc_no = as.numeric(str_sub(disc_code, 2)),
    log_base_pom = case_when(
      disc_no == 1 ~ 0.8,
      disc_no == 2 ~ 1.8,
      disc_no == 3 ~ 3.3,
      disc_no == 4 ~ 5.3,
      disc_no == 5 ~ 7.3,
      TRUE ~ 3.3 + (disc_no - 3) * 2
    ),
    wd_wood_init = round(dry_wt_UB / fresh_vol_UB, 3),
    wd_bark_init = round(dry_wt_lab_bark / (fresh_vol_OB - fresh_vol_UB), 3),
    fd_wood_init = round(dry_wt_UB / fresh_wt_UB, 3),
    fd_bark_init = round(dry_wt_lab_bark / fresh_wt_lab_bark, 3)
  )

## Check
table(stem_disc$disc_no)
summary(stem_disc$wd_wood_init)
summary(stem_disc$wd_bark_init)
summary(stem_disc$dry_weight_UB_inputed)

nrow(stem_disc) == nrow(distinct(data_init$stem_disc, updated_tree_code, disc_no)) 

## + Add ancillary ####
tmp$stem_d_maxh <- stem_disc |>
  group_by(updated_tree_code) |>
  summarise(max_h = max(log_base_pom, na.rm = T), .groups = "drop")
# tmp$stem_d_diam <- data_clean$stem |> 
#   select(updated_tree_code, log_base_pom, log_diam_ob, log_diam_ub)

stem_disc <- stem_disc |>
  mutate(
    log_diam_ob = NA,
    log_diam_ub = NA,
    max_h = NA
  ) |>
  #left_join(tmp$stem_d_diam, by = join_by(updated_tree_code, log_base_pom), suffix = c("_rm", "")) |>
  left_join(tmp$stem_d_maxh, by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    rh = log_base_pom / max_h,
    rh_cat = round(rh/0.25)/0.25,
  )



##
## Cleaning ####
##

## + Remove obvious outliers ####
stem_disc <- stem_disc |> 
  mutate(
    wd_wood = case_when(
      dry_weight_UB_inputed ~ NA_real_,
      wd_wood_init > 1 | wd_wood_init < 0.2 ~ NA_real_,
      log_diam_ob < 10 ~ NA_real_,
      TRUE ~ wd_wood_init
    ),
    wd_wood_status = case_when(
      dry_weight_UB_inputed ~ "missing OD",
      wd_wood_init > 1 | wd_wood_init < 0.2 ~ "outlier obvious",
      log_diam_ob < 10 ~ "outlier disc too small",
      TRUE ~ "unchanged"
    ),
    wd_bark = case_when(
      dry_weight_UB_inputed ~ NA_real_,
      #wd_bark_init > 1 | wd_bark_init < 0.1 ~ NA_real_,
      fresh_wt_lab_bark < 10 ~ NA_real_,
      TRUE ~ wd_bark_init
    ),
    wd_bark_status = case_when(
      dry_weight_UB_inputed ~ "missing OD",
      #wd_bark_init > 1 | wd_bark_init < 0.1 ~ "outlier obvious",
      fresh_wt_lab_bark < 10 ~ "outlier sample too small",
      TRUE ~ "unchanged"
    ),
    # fd_wood = case_when(
    #   dry_weight_UB_inputed ~ NA_real_,
    #   fd_wood_init > 0.95 | fd_wood_init < 0.15 ~ NA_real_,
    #   log_diam_ob < 10 ~ NA_real_,
    #   TRUE ~ fd_wood_init
    # ),
    # fd_wood_status = case_when(
    #   dry_weight_UB_inputed ~ "missing OD",
    #   fd_wood_init > 0.95 | fd_wood_init < 0.15 ~ "outlier obvious",
    #   log_diam_ob < 10 ~ "outlier sample too small",
    #   TRUE ~ "unchanged"
    # )
  )

table(stem_disc$wd_wood_status, useNA = "ifany")
table(stem_disc$wd_bark_status, useNA = "ifany")



## + Calc species level IQR for outlier detection ####

tmp$species_disc <- stem_disc |>
  group_by(tree_species_code) |>
  summarise(
    wd_wood_q1  = quantile(wd_wood, 0.25, na.rm = T),
    wd_wood_q3  = quantile(wd_wood, 0.75, na.rm = T),
    wd_bark_q1 = quantile(wd_bark, 0.25, na.rm = T),
    wd_bark_q3 = quantile(wd_bark, 0.75, na.rm = T),
    # fd_wood_q1  = quantile(fd_wood, 0.25, na.rm = T),
    # fd_wood_q3  = quantile(fd_wood, 0.75, na.rm = T),
    .groups = "drop"
  ) |>
  mutate(
    wd_wood_iqr        = wd_wood_q3 - wd_wood_q1,
    wd_wood_iqr_lower  = wd_wood_q1 - 1.5 * wd_wood_iqr,
    wd_wood_iqr_upper  = wd_wood_q3 + 1.5 * wd_wood_iqr,
    wd_bark_iqr       = wd_bark_q3 - wd_bark_q1,
    wd_bark_iqr_lower = wd_bark_q1 - 1.5 * wd_bark_iqr,
    wd_bark_iqr_upper = wd_bark_q3 + 1.5 * wd_bark_iqr,
    # fd_wood_iqr        = fd_wood_q3 - fd_wood_q1,
    # fd_wood_iqr_lower  = fd_wood_q1 - 1.5 * fd_wood_iqr,
    # fd_wood_iqr_upper  = fd_wood_q3 + 1.5 * fd_wood_iqr
  )

stem_disc <- stem_disc |>
  mutate(
    wd_wood_iqr_lower  = NA,
    wd_wood_iqr_upper  = NA,
    wd_bark_iqr_lower = NA,
    wd_bark_iqr_upper = NA,
    # fd_wood_iqr_lower  = NA,
    # fd_wood_iqr_upper  = NA
  ) |> 
  left_join(select(tmp$species_disc, tree_species_code, ends_with(c("_lower", "_upper"))), by = join_by(tree_species_code), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    wd_wood = case_when(
      !is.na(wd_wood) & wd_wood_init < wd_wood_iqr_lower ~ NA_real_,
      !is.na(wd_wood) & wd_wood_init > wd_wood_iqr_upper ~ NA_real_,
      TRUE ~ wd_wood
    ),
    wd_wood_status = case_when(
      wd_wood_status == "unchanged" & wd_wood_init < wd_wood_iqr_lower ~ "outlier IQR",
      wd_wood_status == "unchanged" & wd_wood_init > wd_wood_iqr_upper ~ "outlier IQR", 
      TRUE ~ wd_wood_status
    ),
    wd_bark = case_when(
      !is.na(wd_bark) & wd_bark_init < wd_bark_iqr_lower ~ NA_real_,
      !is.na(wd_bark) & wd_bark_init > wd_bark_iqr_upper ~ NA_real_,
      TRUE ~ wd_bark
    ),
    wd_bark_status = case_when(
      wd_bark_status == "unchanged" & wd_bark_init < wd_bark_iqr_lower ~ "outlier IQR",
      wd_bark_status == "unchanged" & wd_bark_init > wd_bark_iqr_upper ~ "outlier IQR", 
      TRUE ~ wd_bark_status
    ),
    # fd_wood = case_when(
    #   !is.na(fd_wood) & fd_wood_init < fd_wood_iqr_lower ~ NA_real_,
    #   !is.na(fd_wood) & fd_wood_init > fd_wood_iqr_upper ~ NA_real_,
    #   TRUE ~ fd_wood
    # ),
    # fd_wood_status = case_when(
    #   fd_wood_status == "unchanged" & fd_wood_init < fd_wood_iqr_lower ~ "outlier IQR",
    #   fd_wood_status == "unchanged" & fd_wood_init > fd_wood_iqr_upper ~ "outlier IQR", 
    #   TRUE ~ fd_wood_status
    # )
  )

table(stem_disc$wd_wood_status, useNA = "ifany")
#table(stem_disc$fd_wood_status, useNA = "ifany")
table(stem_disc$wd_bark_status, useNA = "ifany")

# stem_disc |>
#   filter(wd_wood_init < 1.1) |>
#   ggplot(aes(x = rh, y = wd_wood_init)) +
#   geom_point(aes(color = wd_wood_status), size = 0.8) +
#   geom_line(aes(y = wd_wood_iqr_lower)) +
#   geom_line(aes(y = wd_wood_iqr_upper)) +
#   facet_wrap(~tree_species_code) +
#   scale_color_viridis_d()

# stem_disc |>
#   filter(wd_bark_init < 1.1, fresh_wt_lab_bark > 20) |>
#   ggplot(aes(x = rh, y = wd_bark_init)) +
#   geom_point(aes(color = wd_bark_status), size = 0.8) +
#   geom_line(aes(y = wd_bark_iqr_lower)) +
#   geom_line(aes(y = wd_bark_iqr_upper)) +
#   facet_wrap(~tree_species_code) +
#   scale_color_viridis_d()


##
## Correct missing wd_wood ####
##

## + Visual Checks ####
stem_disc |>
  filter(wd_wood_init <= 1.2, wd_wood_init >= 0.2) |>
  ggplot(aes(x = rh, y = wd_wood_init)) +
  geom_point(aes(color = wd_wood_status), size = 0.8) +
  geom_line(aes(y = wd_wood_iqr_lower)) +
  geom_line(aes(y = wd_wood_iqr_upper)) +
  facet_wrap(~tree_species_code) +
  scale_color_viridis_d()

stem_disc |>
  filter(!is.na(wd_wood)) |>
  ggplot(aes(x = rh, y = wd_wood)) +
  geom_point(size = 0.8, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~tree_species_code)

## + Test lm for pinus ####
# stem_disc |>
#   filter(tree_species_code == "Pr", !is.na(wd_wood)) |>
#   left_join(select(data_init$tree, updated_tree_code, tree_dbh), by = join_by(updated_tree_code)) |>
#   mutate(dbh_class = round(tree_dbh / 20) * 20) |>
#   ggplot(aes(x = rh, y = wd_wood)) +
#   #geom_point(aes(color = tree_dbh), size = 0.8) +
#   geom_point(aes(color = as.character(dbh_class)), size = 0.8) +
#   geom_smooth(aes(group = dbh_class), method = "lm", se = F) +
#   facet_wrap(~tree_species_code)
# 
# 
# lm_pr_dat <- stem_disc |>
#   filter(tree_species_code == "Pr", !is.na(wd_wood)) |>
#   left_join(select(data_init$tree, updated_tree_code, tree_dbh), by = join_by(updated_tree_code)) |>
#   mutate(dbh_class = floor(tree_dbh / 20) * 20)
# 
# lm_pr <- lm(wd_wood~rh, data = lm_pr_dat)
# lm_pr <- lme(wd_wood~rh, random = ~1 | dbh_class, data = lm_pr_dat)
# summary(lm_pr)
# ranef(lm_pr)
## Not used - for research purpose

## + Correction ####
## Method: tree or species avg for missing values in all species
## (For Pinus, slight downward trend not accounted for)

tmp$wd_meantree <- stem_disc |>
  filter(!is.na(wd_wood)) |>
  summarise(wd_wood_meantree = mean(wd_wood, na.rm = T), .by = updated_tree_code)

tmp$wd_meansp <- stem_disc |>
  filter(!is.na(wd_wood)) |>
  summarise(wd_wood_meansp = mean(wd_wood, na.rm = T), .by = tree_species_code)

tmp$vec_wd_tree <- stem_disc |>
  filter(!is.na(wd_wood)) |>
  summarise(count = n(), .by = updated_tree_code) |>
  filter(count > 3) |>
  pull(updated_tree_code)


stem_disc <- stem_disc |>
  mutate(
    wd_wood_meantree = NA,
    wd_wood_meansp = NA
  ) |>
  left_join(tmp$wd_meantree, by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  left_join(tmp$wd_meansp, by = join_by(tree_species_code), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    wd_wood_corr = case_when(
      is.na(wd_wood) & !(updated_tree_code %in% tmp$vec_wd_tree) ~ wd_wood_meansp,
      is.na(wd_wood) ~ wd_wood_meantree,
      TRUE ~ wd_wood 
    ),
    wd_wood_corr_status = case_when(
      is.na(wd_wood) & !(updated_tree_code %in% tmp$vec_wd_tree) ~ "species mean",
      is.na(wd_wood) ~ "tree mean",
      TRUE ~ "measured" 
    )
  )

table(stem_disc$wd_wood_corr_status, useNA = "ifany") 

## Check
data_clean_gg$stem_disc_log_check <- stem_disc |>
  #filter(tree_species_code %in% c("Pr")) |>
  #filter(wd_wood_init <= 0.95, wd_wood_init >= 0.15, log_diam_ob >= 10) |>
  ggplot(aes(x = rh, y = wd_wood_corr)) +
  geom_point(aes(color = wd_wood_status), size = 0.8) +
  geom_line(aes(y = wd_wood_iqr_lower)) +
  geom_line(aes(y = wd_wood_iqr_upper)) +
  facet_wrap(~tree_species_code) +
  scale_color_viridis_d()

print(data_clean_gg$stem_disc_log_check)

##
## Correct missing Bark density ####
##

## + visual checks ####
stem_disc |>
  #filter(tree_species_code %in% c("Pr", "Sr")) |>
  filter(wd_bark_init <= 1.2, wd_bark_init >= 0.1) |>
  ggplot(aes(x = rh, y = wd_bark_init)) +
  geom_point(aes(color = wd_bark_status), size = 0.8) +
  geom_line(aes(y = wd_bark_iqr_lower)) +
  geom_line(aes(y = wd_bark_iqr_upper)) +
  facet_wrap(~tree_species_code) +
  scale_color_viridis_d()

stem_disc |>
  #filter(tree_species_code == "Sr") |>
  #left_join(select(data_init$tree, updated_tree_code, tree_dbh), by = join_by(updated_tree_code)) |>
  filter(!is.na(wd_bark), fresh_wt_lab_bark >= 10) |>
  ggplot(aes(x = rh, y = wd_bark)) +
  geom_point(size = 0.8, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~tree_species_code)


## + Correction ####

## Similar correction as wd_wood, using tree or species averages. Expecting slight 
## overestimate for Pr and Ta

tmp$wd_bark_meantree <- stem_disc |>
  filter(!is.na(wd_bark)) |>
  summarise(wd_bark_meantree = mean(wd_bark, na.rm = T), .by = updated_tree_code)

tmp$wd_bark_meansp <- stem_disc |>
  filter(!is.na(wd_bark)) |>
  summarise(wd_bark_meansp = mean(wd_bark, na.rm = T), .by = tree_species_code)

tmp$vec_wd_bark_tree <- stem_disc |>
  filter(!is.na(wd_bark)) |>
  summarise(count = n(), .by = updated_tree_code) |>
  filter(count > 3) |>
  pull(updated_tree_code)

stem_disc <- stem_disc |>
  mutate(
    wd_bark_meantree = NA,
    wd_bark_meansp = NA
  ) |>
  left_join(tmp$wd_bark_meantree, by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  left_join(tmp$wd_bark_meansp, by = join_by(tree_species_code), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    wd_bark_corr = case_when(
      is.na(wd_bark) & !(updated_tree_code %in% tmp$vec_wd_bark_tree) ~ wd_bark_meansp,
      is.na(wd_bark) ~ wd_bark_meantree,
      TRUE ~ wd_bark 
    ),
    wd_bark_corr_status = case_when(
      is.na(wd_bark) & !(updated_tree_code %in% tmp$vec_wd_bark_tree) ~ "species mean",
      is.na(wd_bark) ~ "tree mean",
      TRUE ~ "measured" 
    )
  )

table(stem_disc$wd_bark_corr_status, useNA = "ifany")
summary(stem_disc$wd_bark_corr)

data_clean_gg$stem_disc_bark_check <- stem_disc |>
  #filter(tree_species_code %in% c("Pr")) |>
  #filter(fd_wood_init <= 0.95, fd_wood_init >= 0.15, log_diam_ob >= 10) |>
  ggplot(aes(x = rh, y = wd_bark_corr)) +
  geom_point(aes(color = wd_bark_status), size = 0.8) +
  geom_line(aes(y = wd_bark_iqr_lower)) +
  geom_line(aes(y = wd_bark_iqr_upper)) +
  facet_wrap(~tree_species_code) +
  scale_color_viridis_d()
print(data_clean_gg$stem_disc_bark_check)

## 
## Assign to clean data ####
##

data_clean$stem_disc <- stem_disc

rm(stem_disc_init, stem_disc, tmp)


