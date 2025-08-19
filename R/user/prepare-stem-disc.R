
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

source("R/user/prepare-stem-profile.R")
source("R/user/prepare-stem-volume.R")

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
    species_code = str_sub(updated_tree_code, 4, 5),
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
    wd_log_init = round(dry_wt_UB / fresh_vol_UB, 3),
    wd_bark_init = round(dry_wt_lab_bark / (fresh_vol_OB - fresh_vol_UB), 3),
    fd_log_init = round(dry_wt_UB / fresh_wt_UB, 3),
    fd_bark_init = round(dry_wt_lab_bark / fresh_wt_lab_bark, 3)
  )

## Check
table(stem_disc$disc_no)
summary(stem_disc$wd_log_init)
summary(stem_disc$wd_bark_init)
summary(stem_disc$dry_weight_UB_inputed)

nrow(stem_disc) == nrow(distinct(data_init$stem_disc, updated_tree_code, disc_no)) 

## + add ancillary ####
tmp$stem_d_maxh <- stem_disc |>
  group_by(updated_tree_code) |>
  summarise(max_h = max(log_base_pom, na.rm = T), .groups = "drop")
tmp$stem_d_diam <- data_clean$stem |> 
  select(updated_tree_code, log_base_pom, log_diam_ob, log_diam_ub)

tt <- stem_disc <- stem_disc |>
  mutate(
    log_diam_ob = NA,
    log_diam_ub = NA,
    max_h = NA
  ) |>
  left_join(tmp$stem_d_diam, by = join_by(updated_tree_code, log_base_pom), suffix = c("_rm", "")) |>
  left_join(stem_d_maxh, by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    rh = log_base_pom / max_h,
    rh_cat = round(rh/0.25)/0.25,
  )



##
## Cleaning ####
##

## + Remove obvious outliers ####
stem_disc |> 
  ggplot() +
  geom_point(aes(x = disc_no, y = wd_log_init), alpha = 0.2) +
  facet_wrap(~dry_weight_UB_inputed)

stem_disc |> 
  #filter(wd_bark_init < 1) |>
  ggplot() +
  geom_point(aes(x = rh, y = wd_bark_init), alpha = 0.2) +
  theme(legend.position = "none") +
  facet_wrap(~species_code)

stem_disc |> 
  ggplot() +
  geom_point(aes(x = disc_no, y = fd_log_init), alpha = 0.2) +
  facet_wrap(~dry_weight_UB_inputed)

stem_disc <- stem_disc |> 
  mutate(
    wd_log = case_when(
      dry_weight_UB_inputed ~ NA_real_,
      wd_log_init > 1 | wd_log_init < 0.2 ~ NA_real_,
      log_diam_ob < 10 ~ NA_real_,
      TRUE ~ wd_log_init
    ),
    wd_log_status = case_when(
      dry_weight_UB_inputed ~ "missing OD",
      wd_log_init > 1 | wd_log_init < 0.2 ~ "outlier obvious",
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
    fd_log = case_when(
      dry_weight_UB_inputed ~ NA_real_,
      fd_log_init > 0.95 | fd_log_init < 0.15 ~ NA_real_,
      log_diam_ob < 10 ~ NA_real_,
      TRUE ~ fd_log_init
    ),
    fd_log_status = case_when(
      dry_weight_UB_inputed ~ "missing OD",
      fd_log_init > 0.95 | fd_log_init < 0.15 ~ "outlier obvious",
      log_diam_ob < 10 ~ "outlier sample too small",
      TRUE ~ "unchanged"
    )
  )

table(stem_disc$fd_log_status, useNA = "ifany")
table(stem_disc$wd_bark_status, useNA = "ifany")



## + Calc species level IQR for outlier detection ####

tmp$species_disc <- stem_disc |>
  group_by(species_code) |>
  summarise(
    wd_log_q1  = quantile(wd_log, 0.25, na.rm = T),
    wd_log_q3  = quantile(wd_log, 0.75, na.rm = T),
    wd_bark_q1 = quantile(wd_bark, 0.25, na.rm = T),
    wd_bark_q3 = quantile(wd_bark, 0.75, na.rm = T),
    fd_log_q1  = quantile(fd_log, 0.25, na.rm = T),
    fd_log_q3  = quantile(fd_log, 0.75, na.rm = T),
    .groups = "drop"
  ) |>
  mutate(
    wd_log_iqr        = wd_log_q3 - wd_log_q1,
    wd_log_iqr_lower  = wd_log_q1 - 1.5 * wd_log_iqr,
    wd_log_iqr_upper  = wd_log_q3 + 1.5 * wd_log_iqr,
    wd_bark_iqr       = wd_bark_q3 - wd_bark_q1,
    wd_bark_iqr_lower = wd_bark_q1 - 1.5 * wd_bark_iqr,
    wd_bark_iqr_upper = wd_bark_q3 + 1.5 * wd_bark_iqr,
    fd_log_iqr        = fd_log_q3 - fd_log_q1,
    fd_log_iqr_lower  = fd_log_q1 - 1.5 * fd_log_iqr,
    fd_log_iqr_upper  = fd_log_q3 + 1.5 * fd_log_iqr
  )

stem_disc <- stem_disc |>
  mutate(
    wd_log_iqr_lower  = NA,
    wd_log_iqr_upper  = NA,
    wd_bark_iqr_lower = NA,
    wd_bark_iqr_upper = NA,
    fd_log_iqr_lower  = NA,
    fd_log_iqr_upper  = NA
  ) |> 
  left_join(select(tmp$species_disc, species_code, ends_with(c("_lower", "_upper"))), by = join_by(species_code), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    wd_log = case_when(
      !is.na(wd_log) & wd_log_init < wd_log_iqr_lower ~ NA_real_,
      !is.na(wd_log) & wd_log_init > wd_log_iqr_upper ~ NA_real_,
      TRUE ~ wd_log
    ),
    wd_log_status = case_when(
      wd_log_status == "unchanged" & wd_log_init < wd_log_iqr_lower ~ "outlier IQR",
      wd_log_status == "unchanged" & wd_log_init > wd_log_iqr_upper ~ "outlier IQR", 
      TRUE ~ wd_log_status
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
    fd_log = case_when(
      !is.na(fd_log) & fd_log_init < fd_log_iqr_lower ~ NA_real_,
      !is.na(fd_log) & fd_log_init > fd_log_iqr_upper ~ NA_real_,
      TRUE ~ fd_log
    ),
    fd_log_status = case_when(
      fd_log_status == "unchanged" & fd_log_init < fd_log_iqr_lower ~ "outlier IQR",
      fd_log_status == "unchanged" & fd_log_init > fd_log_iqr_upper ~ "outlier IQR", 
      TRUE ~ fd_log_status
    )
  )

#table(stem_disc$wd_log_status, useNA = "ifany")
table(stem_disc$fd_log_status, useNA = "ifany")
table(stem_disc$wd_bark_status, useNA = "ifany")

# stem_disc |>
#   filter(wd_log_init < 1.1) |>
#   ggplot(aes(x = rh, y = wd_log_init)) +
#   geom_point(aes(color = wd_log_status), size = 0.8) +
#   geom_line(aes(y = wd_log_iqr_lower)) +
#   geom_line(aes(y = wd_log_iqr_upper)) +
#   facet_wrap(~species_code) +
#   scale_color_viridis_d()

# stem_disc |>
#   filter(wd_bark_init < 1.1, fresh_wt_lab_bark > 20) |>
#   ggplot(aes(x = rh, y = wd_bark_init)) +
#   geom_point(aes(color = wd_bark_status), size = 0.8) +
#   geom_line(aes(y = wd_bark_iqr_lower)) +
#   geom_line(aes(y = wd_bark_iqr_upper)) +
#   facet_wrap(~species_code) +
#   scale_color_viridis_d()


##
## Correct missing fd_log ####
##

## + Visual Checks ####
stem_disc |>
  filter(fd_log_init <= 0.95, fd_log_init >= 0.15) |>
  ggplot(aes(x = rh, y = fd_log_init)) +
  geom_point(aes(color = fd_log_status), size = 0.8) +
  geom_line(aes(y = fd_log_iqr_lower)) +
  geom_line(aes(y = fd_log_iqr_upper)) +
  facet_wrap(~species_code) +
  scale_color_viridis_d()

stem_disc |>
  filter(!is.na(fd_log)) |>
  ggplot(aes(x = rh, y = fd_log)) +
  geom_point(size = 0.8, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~species_code)

## + Test lm for pinus ####
# stem_disc |>
#   filter(species_code == "Pr", !is.na(fd_log)) |>
#   left_join(select(data_init$tree, updated_tree_code, tree_dbh), by = join_by(updated_tree_code)) |>
#   mutate(dbh_class = round(tree_dbh / 20) * 20) |>
#   ggplot(aes(x = rh, y = fd_log)) +
#   #geom_point(aes(color = tree_dbh), size = 0.8) +
#   geom_point(aes(color = as.character(dbh_class)), size = 0.8) +
#   geom_smooth(aes(group = dbh_class), method = "lm") +
#   facet_wrap(~species_code)
# 
# 
# lm_pr_dat <- stem_disc |> 
#   filter(species_code == "Pr", !is.na(fd_log)) |>
#   left_join(select(data_init$tree, updated_tree_code, tree_dbh), by = join_by(updated_tree_code)) |>
#   mutate(dbh_class = floor(tree_dbh / 20) * 20)
#   
# lm_pr <- lm(fd_log~rh, data = lm_pr_dat)
# lm_pr <- lme(fd_log~rh, random = ~1 | dbh_class, data = lm_pr_dat)
# summary(lm_pr)
# ranef(lm_pr)
## Not used - for research purpose ####

## + Correction ####
## Method: tree or species avg for missing values in all species
## (For Pinus, slight downward trend not accounted for)

fd_meantree <- stem_disc |>
  filter(!is.na(fd_log)) |>
  summarise(fd_log_meantree = mean(fd_log, na.rm = T), .by = updated_tree_code)
fd_meansp <- stem_disc |>
  filter(!is.na(fd_log)) |>
  summarise(fd_log_meansp = mean(fd_log, na.rm = T), .by = species_code)

vec_fd_missing <- stem_disc |>
  filter(!is.na(fd_log)) |>
  summarise(count = n(), .by = updated_tree_code) |>
  filter(count < 3) |>
  pull(updated_tree_code)


stem_disc <- stem_disc |>
  mutate(
    fd_log_meantree = NA,
    fd_log_meansp = NA
  ) |>
  left_join(fd_meantree, by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  left_join(fd_meansp, by = join_by(species_code), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    fd_log_corr = case_when(
      is.na(fd_log) & updated_tree_code %in% vec_fd_missing ~ fd_log_meansp,
      is.na(fd_log) ~ fd_log_meantree,
      TRUE ~ fd_log 
    ),
    fd_log_corr_status = case_when(
      is.na(fd_log) & updated_tree_code %in% vec_fd_missing ~ "species mean",
      is.na(fd_log) ~ "tree mean",
      TRUE ~ "measured" 
    )
  )

## Check
stem_disc |>
  #filter(species_code %in% c("Pr")) |>
  #filter(fd_log_init <= 0.95, fd_log_init >= 0.15, log_diam_ob >= 10) |>
  ggplot(aes(x = rh, y = fd_log_corr)) +
  geom_point(aes(color = fd_log_corr_status), size = 0.8) +
  geom_line(aes(y = fd_log_iqr_lower)) +
  geom_line(aes(y = fd_log_iqr_upper)) +
  facet_wrap(~species_code) +
  scale_color_viridis_d(direction = -1)



##
## Correct missing Bark density ####
##

## + visual checks ####
stem_disc |>
  #filter(species_code %in% c("Pr", "Sr")) |>
  filter(wd_bark_init <= 1.2, wd_bark_init >= 0.1) |>
  ggplot(aes(x = rh, y = wd_bark_init)) +
  geom_point(aes(color = wd_bark_status), size = 0.8) +
  geom_line(aes(y = wd_bark_iqr_lower)) +
  geom_line(aes(y = wd_bark_iqr_upper)) +
  facet_wrap(~species_code) +
  scale_color_viridis_d()

stem_disc |>
  #filter(species_code == "Sr") |>
  #left_join(select(data_init$tree, updated_tree_code, tree_dbh), by = join_by(updated_tree_code)) |>
  filter(!is.na(wd_bark), fresh_wt_lab_bark >= 10) |>
  ggplot(aes(x = rh, y = wd_bark)) +
  geom_point(size = 0.8, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~species_code)


## + Correction ####



## mess ####
# 
# stem_disc |> 
#   ggplot() +
#   geom_point(aes(x = disc_no, y = wd_log), alpha = 0.2) +
#   theme(legend.position = "none")
#   
# 
# stem_disc |> 
#   left_join(select(data_clean$stem, updated_tree_code, log_base_pom, log_diam_ub), by = join_by(updated_tree_code, log_base_pom)) |>
#   mutate(
#     species_code = str_sub(updated_tree_code, 4, 5)
#   ) |>
#   filter(wd_log <= 0.95, wd_log >= 0.3) |>
#   ggplot() +
#   geom_point(aes(x = log_diam_ub, y = wd_log), alpha = 0.2) +
#   facet_wrap(~species_code) +
#   theme(legend.position = "none")
# 
# stem_disc |> 
#   left_join(select(data_clean$stem, updated_tree_code, log_base_pom, log_diam_ub), by = join_by(updated_tree_code, log_base_pom)) |>
#   mutate(
#     species_code = str_sub(updated_tree_code, 4, 5)
#   ) |>
#   filter(wd_log <= 0.95, wd_log >= 0.3, fresh_wt_UB >= 10) |>
#   ggplot() +
#   geom_point(aes(x = log_diam_ub, y = wd_log), alpha = 0.2) +
#   facet_wrap(~species_code) +
#   theme(legend.position = "none")
# 
# 
# ## RULE: remove all disc with fresh weight < 10 g, assign stem disc value from neighbor
# 
# 
# ## Bark density
# 
# stem_disc |> 
#   left_join(select(data_clean$stem, updated_tree_code, log_base_pom, log_diam_ob, log_diam_ub), by = join_by(updated_tree_code, log_base_pom)) |>
#   mutate(
#     species_code = str_sub(updated_tree_code, 4, 5)
#   ) |>
#   filter(wd_bark <= 0.95, wd_bark >= 0.3, fresh_wt_lab_bark >= 10) |>
#   ggplot() +
#   geom_point(aes(x = log_diam_ob - log_diam_ub, y = wd_bark), alpha = 0.2) +
#   facet_wrap(~species_code, scale = "free") +
#   theme(legend.position = "none")
# 
# 
# stem_disc |> 
#   left_join(select(data_clean$stem, updated_tree_code, log_base_pom, log_diam_ob, log_diam_ub), by = join_by(updated_tree_code, log_base_pom)) |>
#   mutate(
#     species_code = str_sub(updated_tree_code, 4, 5),
#     bark_thickness_cat = round((log_diam_ob - log_diam_ub)/1)*1
#   ) |>
#   filter(wd_bark <= 0.95, wd_bark >= 0.3, fresh_wt_lab_bark >= 10) |>
#   ggplot() +
#   geom_boxplot(aes(x = bark_thickness_cat, y = wd_bark, group = bark_thickness_cat), alpha = 0.2) +
#   #facet_wrap(~species_code, scale = "free") +
#   facet_wrap(~species_code) +
#   theme(legend.position = "none")
# 
# 
# stem_d_maxh <- stem_disc |>
#   group_by(updated_tree_code) |>
#   summarise(max_h = max(log_base_pom, na.rm = T), .groups = "drop")
# 
# stem_d_fdmean <- stem_disc |>
#   group_by(updated_tree_code) |>
#   summarise(mean_fd = mean(fd_log, na.rm = T), .groups = "drop")
# 
# stem_d_fdsp <- stem_disc |>
#   group_by(species_code) |>
#   summarise(
#     fd_meansp = mean(fd_log_init, na.rm = T), 
#     fd_sdsp   = sd(fd_log_init, na.rm = T),
#     .groups = "drop"
#     )
# 
# 
# 
# stem_d <- stem_disc |> 
#   filter(wd_log_init <= 0.9, wd_log_init >= 0.25 ) |>
#   filter(fresh_wt_UB >= 10) |>
#   left_join(select(data_clean$stem, updated_tree_code, log_base_pom, log_diam_ob, log_diam_ub), by = join_by(updated_tree_code, log_base_pom)) |>
#   left_join(stem_d_maxh, by = join_by(updated_tree_code)) |>
#   left_join(stem_d_fdmean, by = join_by(updated_tree_code)) |>
#   left_join(stem_d_fdsp, by = join_by(species_code)) |>
#   mutate(
#     species_code = str_sub(updated_tree_code, 4, 5),
#     rh = log_base_pom / max_h,
#     rh_cat = round(rh/0.1)/0.1,
#     bark_thickness_cat = round((log_diam_ob - log_diam_ub)/1)*1
#   ) 
# 
# # stem_d |>
# #   ggplot(aes(x = rh_cat, y = wd_log)) +
# #   geom_boxplot(aes(group = rh_cat)) +
# #   facet_wrap(~species_code)
# # 
# # stem_d |>
# #   filter(fresh_wt_lab_bark >= 10, wd_bark <= 0.8) |>
# #   ggplot(aes(x = rh_cat, y = wd_bark)) +
# #   geom_boxplot(aes(group = rh_cat)) +
# #   facet_wrap(~species_code)
# # 
# # stem_d |>
# #   #filter(fresh_wt_lab_bark >= 10, wd_bark <= 0.8) |>
# #   ggplot(aes(x = rh_cat, y = fd_log)) +
# #   geom_boxplot(aes(group = rh_cat)) +
# #   facet_wrap(~species_code)
# 
# stem_d |>
#   filter(species_code == "Pr") |>
#   #filter(fresh_wt_lab_bark >= 10, wd_bark <= 0.8) |>
#   ggplot(aes(x = rh_cat, y = fd_log_init)) +
#   geom_line(aes(y = fd_meansp, group = updated_tree_code), color = "grey60", linetype = "dashed") +
#   geom_line(aes(group = updated_tree_code)) +
#   geom_point(aes(color = dry_weight_UB_inputed)) +
#   #theme(legend.position = "none") +
#   facet_wrap(~updated_tree_code)
# 
# 
# 
# tree_wd <- stem_disc |>
#   filter(wd_log <= 0.9, wd_log >= 0.25 ) |>
#   filter(fresh_wt_UB >= 10) |>
#   left_join(select(data_clean$stem, updated_tree_code, log_base_pom, log_diam_ob, log_diam_ub), by = join_by(updated_tree_code, log_base_pom)) |>
#   mutate(species_code = str_sub(updated_tree_code, 4, 5)) |>
#   group_by(updated_tree_code, species_code) |>
#   summarise(
#     mean_wd = mean(wd_log, na.rm = T),
#     .groups = "drop"
#   )
# 
# tree_wd |>
#   ggplot() +
#   geom_boxplot(aes(x = species_code, y = mean_wd)) +
#   geom_jitter(aes(x = species_code, y = mean_wd), alpha = 0.1)
# 
# tree_wd |>
#   left_join(select(data_clean$tree_stem_v, updated_tree_code, tree_stem_v)) |>
#   ggplot(aes(x = tree_stem_v, y = mean_wd)) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(se = F, method = "lm") +
#   facet_wrap(~species_code)


