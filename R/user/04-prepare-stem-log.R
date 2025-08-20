
if (!"usr" %in% ls())      source("R/user/user-inputs.R")
if (!"nlme_out" %in% ls()) source("R/setup/init.R")
if ("data_init" %in% ls() & length(data_init) == 0) source("R/setup/get-data.R")

tmp <- list()

## Subset of cols for plot
tmp$plot <- data_init$plot |>
  select(
    updated_tree_code, plot_physio = physiographic_region, plot_prov = province,
    plot_dist = district, plot_alt = altitude, plot_ftype = forest_type
    )

tmp$tree_species <- data_init$tree |>
  select(updated_tree_code, tree_species_name) |>
  mutate(tree_species_code = str_sub(updated_tree_code, 4, 5))

tmp$tree_length <- data_init$stem |> 
  select(updated_tree_code, tree_dbh, tree_total_length) |>
  distinct()

## Add a table with tree total length and diam = 0
## Make new cols for log top measurements, with DBH 0 for last measurement
vec_treeid <- sort(unique( data_clean$stem_log_init$updated_tree_code))

tmp$tree_add_top <- tmp$tree_length |>
  mutate(
    log_diam_ob = 0,
    log_diam_ub = 0,
    log_meas_type = "top") |>
  select(updated_tree_code, log_base_pom = tree_total_length, log_diam_ob, log_diam_ub, log_meas_type)



##
## Split stem log measurements to collect of POMs and diameters ######
##

## Normal measurement
tmp$stem_normal <- data_init$stem |>
  filter(!is.na(log_base_pom), !is.na(log_diam_ob)) |>
  mutate(
    log_meas_type = "normal",
    log_base_pom = if_else(!is.na(log_disk_pom), log_disk_pom, log_base_pom)
  ) |>
  select(updated_tree_code, log_base_pom, log_diam_ob, log_diam_ub, log_meas_type)


## Abnormal measurements
tmp$stem_abnormal <- data_init$stem |> 
  filter(!is.na(log_base_pom_abnormal), !is.na(log_diam_ob_abnormal)) |>
  mutate(
    log_meas_type = "abnormal",
    #log_base_pom_abnormal = if_else(is.na(log_base_pom_abnormal) & !is.na(log_base_pom), log_base_pom, log_base_pom_abnormal)
  ) |>
  select(updated_tree_code, ends_with("_abnormal"), -log_length_abnormal, log_meas_type) |>
  rename_with(.cols = ends_with("_abnormal"), str_remove, "_abnormal")

## Top 20 and Top 10 measurements
tmp$vec_end <- c("_below20", "_mid20", "_above20", "_below10", "_mid10", "_above10")

tmp$stem_adj_top <- map(tmp$vec_end, function(x){
  
  data_init$stem |>
    select(updated_tree_code, ends_with(x)) |>
    filter(if_all(ends_with(x), ~!is.na(.))) |>
    rename_with(.cols = ends_with(x), str_remove, pattern = x) |>
    mutate(log_meas_type = "adj_top")
  
}) |> list_rbind()


## Group all
tmp$stem_bind <- bind_rows(tmp$stem_abnormal, tmp$stem_normal, tmp$stem_adj_top, tmp$tree_add_top) |>
  distinct(updated_tree_code, log_base_pom, log_diam_ob, .keep_all = T) |>
  arrange(updated_tree_code, log_base_pom)

tmp$stem_cleantop <- tmp$stem_bind |>
  left_join(tmp$tree_length, by = "updated_tree_code") |>
  filter(!(log_base_pom == tree_total_length & log_diam_ob != 0)) |>
  filter(log_base_pom <= tree_total_length) |>
  distinct(updated_tree_code, log_base_pom, .keep_all = T)

## Check duplicates
# check <- tmp$stem_cleantop |>
#   group_by(updated_tree_code, log_base_pom) |>
#   summarise(count = n(), .groups = "drop")
#   
# table(check$count)  
# 
# check2 <- check |> 
#   filter(count > 1) |>
#   select(updated_tree_code, log_base_pom)
# check2
# 
# check3 <- tmp$stem_bind |> filter(updated_tree_code == "015Sr009")

##
## Make final data ####
##

## No NA allowed in diam OB and UB
tmp$stem_cleantop |> filter(is.na(log_diam_ob))
tmp$stem_cleantop |> filter(is.na(log_diam_ub))
## >> only 3 values for UB, remove

 data_clean$stem_log_init <- tmp$stem_cleantop |>
  filter(!is.na(log_diam_ob), !is.na(log_diam_ub)) |>
  group_by(updated_tree_code) |>
  mutate(
    log_no = row_number(),
    updated_tree_code = case_when(
      updated_tree_code == "211Lp032" ~ "211Lp034",
      TRUE ~ updated_tree_code
      ),
    log_bark_thickness = log_diam_ob - log_diam_ub,
    log_barkwood = round(log_bark_thickness / log_diam_ob, 4)
    ) |>
  ungroup() |>
  left_join(tmp$tree_species, by = "updated_tree_code") |>
  left_join(tmp$plot, by = "updated_tree_code") |>
  mutate(
    tree_species_group = if_else(str_detect(tree_species_name, "Castonopsis"), "Castonopsis group", tree_species_name),
    tree_conif = if_else(tree_species_name == "Pinus roxburghii", "conif", "broadl")
  )

##
## Checks ####
##

## + Check missing under bark diameters 
tmp$vec_missing <-  data_clean$stem_log_init |> 
  filter(is.na(log_diam_ub)) |>
  pull(updated_tree_code)
tmp$vec_missing
## >> Checked that records are missing cause discs broke

#tt <-  data_clean$stem_log_init |> filter(updated_tree_code == "272Ta039")


## + Check oB vs UB ####
## >> Few manual corrections, now ok, maybe still be few errors
 data_clean$stem_log_init |>
  ggplot(aes(x = log_diam_ob, y = log_diam_ub)) +
  geom_point(aes(color = log_meas_type)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~tree_species_code)

 data_clean$stem_log_init |>
  ggplot(aes(x = log_diam_ob, y = log_bark_thickness)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~tree_species_code)

 data_clean$stem_log_init |>
  filter(log_diam_ob > 20) |>
  ggplot(aes(x = log_diam_ob, y = log_barkwood)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~tree_species_code)

## !! For manual correction
# tmp$vec_error <-  data_clean$stem_log_init |>
#   #filter(log_diam_ob > 20, tree_species_code %in% c("An", "Cs", "Lp"), log_barkwood > 0.2) |>
#   filter(log_diam_ob > 20, tree_species_code %in% c("Pr", "Sr", "Sw"), log_barkwood > 0.31) |>
#   pull(updated_tree_code) |>
#   unique() |>
#   sort()
# tmp$vec_error
# 
# tt <-  data_clean$stem_log_init |> 
#   filter(updated_tree_code == "250Sw028" & log_barkwood > 0.31, log_diam_ob > 20)
# 
#  data_clean$stem_log_init |>
#   filter(log_diam_ob > 20) |>
#   ggplot(aes(x = log_diam_ob, y = log_barkwood)) +
#   geom_point(alpha = 0.2) +
#   geom_label_repel(data = tt, aes(label = updated_tree_code),color = "red", min.segment.length = 0) +
#   facet_wrap(~tree_species_code)
## !!



##
## old checks ####
##

## Checks NAs from joins
# summary( data_clean$stem_log_init)
# summary( data_clean$stem_log_init$tree_total_length)
# table( data_clean$stem_log_init$species_name, useNA = "ifany")  
# table( data_clean$stem_log_init$physiographic_region, useNA = "ifany") 
# table( data_clean$stem_log_init$district, useNA = "ifany") 

## Check species and physio for NAs
#  data_clean$stem_log_init |> 
#   filter(is.na(species_name)) |> 
#   pull(updated_tree_code) |> 
#   unique()
# 
#  data_clean$stem_log_init |> 
#   filter(is.na(physiographic_region)) |> 
#   pull(updated_tree_code) |> 
#   unique()

## CHECKS
# ggplot( data_clean$stem_log_init) +
#   geom_point(aes(x = log_base_pom, y = log_diam_ob, color = log_meas_type), size = 0.1)

# ggplot( data_clean$stem_log_init) +
#   geom_line(aes(x = log_base_pom, y = log_diam_ob, color = updated_tree_code)) +
#   theme(legend.position = "none") +
#   facet_wrap(~ physiographic_region)

# ggplot( data_clean$stem_log_init) +
#   geom_line(aes(x = log_base_pom, y = log_diam_ob, color = updated_tree_code)) +
#   theme(legend.position = "none") +
#   facet_wrap(~species_name)

#  data_clean$stem_log_init |>
#   #filter(log_meas_type %in% c("normal", "top")) |>
#   ggplot() +
#   geom_line(aes(x = log_base_pom, y = log_diam_ob, color = updated_tree_code)) +
#   theme(legend.position = "none") +
#   facet_wrap(~ district)

## Check for district with visible error
#  data_clean$stem_log_init |>
#   filter(log_meas_type %in% c("normal", "top")) |>
#   filter(district == "Jhapa") |>
#   ggplot() +
#   geom_line(aes(x = log_base_pom, y = log_diam_ob, color = updated_tree_code)) +
#   theme(legend.position = "none") +
#   facet_wrap(~ updated_tree_code)


