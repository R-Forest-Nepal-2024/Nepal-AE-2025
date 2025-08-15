


tmp <- list()

## Subset of cols for plot
tmp$plot <- data_init$plot |>
  select(updated_tree_code, physiographic_region, province, district, altitude, forest_type)

tmp$tree_species <- data_init$tree |>
  select(updated_tree_code, species_name, species_code)

tmp$tree_length <- data_init$stem |> 
  select(updated_tree_code, tree_total_length, tree_dbh) |>
  distinct()

## Add a table with tree total length and diam = 0
## Make new cols for log top measurements, with DBH 0 for last measurement
vec_treeid <- sort(unique(tmp$stem_all$updated_tree_code))

tmp$tree_add_top <- tmp$tree_length |>
  mutate(
    log_diam_ob = 0,
    log_diam_ub = 0,
    measurement_type = "top") |>
  select(updated_tree_code, log_base_pom = tree_total_length, log_diam_ob, log_diam_ub, measurement_type)



##
## Split stem log measurements to collect of POMs and diameters ######
##

## Normal measurement
tmp$stem_normal <- data_init$stem |>
  filter(!is.na(log_base_pom), !is.na(log_diam_ob)) |>
  mutate(
    measurement_type = "normal",
    log_base_pom = if_else(!is.na(log_disk_pom), log_disk_pom, log_base_pom)
  ) |>
  select(updated_tree_code, log_base_pom, log_diam_ob, log_diam_ub, measurement_type)


## Abnormal measurements
tmp$stem_abnormal <- data_init$stem |> 
  filter(!is.na(log_base_pom_abnormal), !is.na(log_diam_ob_abnormal)) |>
  mutate(
    measurement_type = "abnormal",
    #log_base_pom_abnormal = if_else(is.na(log_base_pom_abnormal) & !is.na(log_base_pom), log_base_pom, log_base_pom_abnormal)
  ) |>
  select(updated_tree_code, ends_with("_abnormal"), -log_length_abnormal, measurement_type) |>
  rename_with(.cols = ends_with("_abnormal"), str_remove, "_abnormal")

## Top 20 and Top 10 measurements
tmp$vec_end <- c("_below20", "_mid20", "_above20", "_below10", "_mid10", "_above10")

tmp$stem_adj_top <- map(tmp$vec_end, function(x){
  
  data_init$stem |>
    select(updated_tree_code, ends_with(x)) |>
    filter(if_all(ends_with(x), ~!is.na(.))) |>
    rename_with(.cols = ends_with(x), str_remove, pattern = x) |>
    mutate(measurement_type = "adj_top")
  
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

tmp$stem_all <- tmp$stem_cleantop |>
  group_by(updated_tree_code) |>
  mutate(log_no = row_number()) |>
  ungroup() |>
  left_join(tmp$tree_species, by = "updated_tree_code") |>
  left_join(tmp$plot, by = "updated_tree_code")

## Checks NAs from joins
# summary(tmp$stem_all)
# summary(tmp$stem_all$tree_total_length)
# table(tmp$stem_all$species_name, useNA = "ifany")  
# table(tmp$stem_all$physiographic_region, useNA = "ifany") 
# table(tmp$stem_all$district, useNA = "ifany") 

## Check species and physio for NAs
# tmp$stem_all |> 
#   filter(is.na(species_name)) |> 
#   pull(updated_tree_code) |> 
#   unique()
# 
# tmp$stem_all |> 
#   filter(is.na(physiographic_region)) |> 
#   pull(updated_tree_code) |> 
#   unique()

## CHECKS
# ggplot(tmp$stem_all) +
#   geom_point(aes(x = log_base_pom, y = log_diam_ob, color = measurement_type), size = 0.1)

# ggplot(tmp$stem_all) +
#   geom_line(aes(x = log_base_pom, y = log_diam_ob, color = updated_tree_code)) +
#   theme(legend.position = "none") +
#   facet_wrap(~ physiographic_region)

# ggplot(tmp$stem_all) +
#   geom_line(aes(x = log_base_pom, y = log_diam_ob, color = updated_tree_code)) +
#   theme(legend.position = "none") +
#   facet_wrap(~species_name)

# tmp$stem_all |>
#   #filter(measurement_type %in% c("normal", "top")) |>
#   ggplot() +
#   geom_line(aes(x = log_base_pom, y = log_diam_ob, color = updated_tree_code)) +
#   theme(legend.position = "none") +
#   facet_wrap(~ district)

## Check for district with visible error
# tmp$stem_all |>
#   filter(measurement_type %in% c("normal", "top")) |>
#   filter(district == "Jhapa") |>
#   ggplot() +
#   geom_line(aes(x = log_base_pom, y = log_diam_ob, color = updated_tree_code)) +
#   theme(legend.position = "none") +
#   facet_wrap(~ updated_tree_code)



##
## Add relative D and H ######
##

## Check species
# tmp$stem_all |>
#   filter(str_detect(species_name, pattern = "Castonopsis")) |>
#   ggplot() +
#   geom_line(aes(x = log_base_pom, y = log_diam_ob, color = updated_tree_code)) +
#   theme(legend.position = "none") +
#   facet_wrap(~ species_name, ncol = 1)

## !!! Final data for stem profile !!!
data_clean$stem <- tmp$stem_all |>
  mutate(
    species_group = if_else(str_detect(species_name, "Castonopsis"), "Castonopsis group", species_name),
    conif = if_else(species_name == "Pinus roxburghii", "conif", "broadl"),
    dr = round(log_diam_ob / tree_dbh, 3),
    hr = round(log_base_pom / tree_total_length, 3)
  )


## Check relative D and H
# data_clean$stem |>
#   filter(measurement_type %in% c("normal", "top")) |>
#   ggplot(aes(x = hr, y = dr)) +
#   geom_line(aes(color = updated_tree_code), alpha = 0.6) +
#   theme(legend.position = "none") +
#   facet_wrap(~species_group)


## Checks
data_clean$stem |>
  ggplot() +
  geom_line(data = data_clean$stem, aes(x = hr, y = dr, color = updated_tree_code)) +
  facet_wrap(~species_group) +
  theme(legend.position = "none")

data_clean_gg$stem_check <- data_clean$stem |>
  filter(dr > 1.5 & hr < 0.2) |>
  ggplot() +
  geom_point(data = data_clean$stem, aes(x = hr, y = dr, color = species_group), size = 0.2) +
  geom_point(aes(x = hr, y = dr), shape = 21, col = "red", size = 4) +
  geom_text_repel(aes(x = hr, y = dr, label = paste(updated_tree_code, log_no)), min.segment.length = 0, max.overlaps = 12) +
  facet_wrap(~species_group) +
  #facet_grid(measurement_type~conif) +
  theme(legend.position = "none")

print(data_clean_gg$stem_check)
# ggsave(
#   plot = data_clean_gg$stem_check, paste0("res/cleaning-examples/check_stem-", Sys.time(), ".png"),
#   width = 15, height = 12, units = "cm", dpi = 300
# )


tmp$check <- data_clean$stem |> filter(updated_tree_code == "505Sr123")


## !!! Remove for taper due to early fork - KEEP for volume !!!
data_clean$stem_taper <- data_clean$stem |>
  filter(
    !(updated_tree_code == "460An054" & log_no == 1),
    !(updated_tree_code == "361Cs054" & log_no == 1),
    !(updated_tree_code == "227Lp036" & log_no == 1)
  ) |>
  filter(
    !updated_tree_code %in% c(
      "551Pr100", "474Sr112", "300Sr083", "359Cs053", "372Cs056", "365Sw048",
      "508Lp058"
    )
  )


## Check points too low
tt <- data_clean$stem_taper |> 
  filter(updated_tree_code %in% c("252Pr029", "365Sw048"))

data_clean_gg$stem_check2 <- data_clean$stem_taper |>
  filter(dr < 0.95 & hr < 0.05) |>
  ggplot(aes(x = hr, y = dr)) +
  geom_point(data = data_clean$stem, aes(color = species_group), size = 0.2) +
  geom_point(shape = 21, col = "red", size = 4) +
  geom_text_repel(aes(label = paste(updated_tree_code, log_no)), min.segment.length = 0, max.overlaps = 12) +
  geom_line(data = tt) +
  facet_wrap(~species_group) +
  theme(legend.position = "none")

print(data_clean_gg$stem_check2)
# ggsave(
#   plot = data_clean_gg$stem_check2, paste0("res/cleaning-examples/check_stem-", Sys.time(), ".png"),
#   width = 15, height = 12, units = "cm", dpi = 300
#   )

## Line check
data_clean_gg$stem_check3 <- data_clean$stem_taper |>
  filter(measurement_type %in% c("normal", "top")) |>
  ggplot(aes(x = hr, y = dr)) +
  geom_line(aes(color = updated_tree_code)) +
  facet_grid(species_group~province) +
  theme(legend.position = "none")

print(data_clean_gg$stem_check3)

## More outlier checks
# data_clean_gg$stem_check4 <- data_clean$stem_taper |>
#   filter(measurement_type %in% c("normal", "top")) |>
#   filter(province == "Madhesh", str_detect(species_name, "Shorea")) |>
#   ggplot(aes(x = hr, y = dr)) +
#   geom_line(aes(color = updated_tree_code)) +
#   facet_wrap(~updated_tree_code) +
#   theme(legend.position = "none")
# 
# print(data_clean_gg$stem_check4)
# 
# tt <- tmp$check206 <- data_clean$stem_taper |> filter(updated_tree_code == "073Sr021")


## Save output table
write_csv(data_clean$stem, "data/data-clean/stem.csv")
write_csv(data_clean$stem_taper, "data/data-clean/stem_taper.csv")


## Remove temporary objects
rm(tmp)




## Volume calculation
# log_tree_top <- stem_clean |>
#   group_by(updated_tree_code) |>
#   summarise(log_tree_top = max(log_no))
# 
# dim_top <- stem_clean |>
#   select(updated_tree_code, log_no, log_base_pom, log_diam_ob)
# 
# stem_v <- stem_clean |>
#   left_join(log_tree_top, by = "updated_tree_code") |>
#   mutate(log_no_top = if_else(log_no == log_tree_top, log_no, log_no + 1))


