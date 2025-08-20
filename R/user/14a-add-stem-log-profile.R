
# if (!"usr" %in% ls())      source("R/user/user-inputs.R")
# if (!"nlme_out" %in% ls()) source("R/setup/init.R")
# if (length(data_init) == 0) source("R/setup/get-data.R")
# if (!("stem_log" %in% names(data_clean))) source("R/user/01-prepare-stem-log.R")

tmp <- list()

##
## Add relative D and H ######
##

## Check species
# data_clean$stem_log |>
#   filter(str_detect(species_name, pattern = "Castonopsis")) |>
#   ggplot() +
#   geom_line(aes(x = log_base_pom, y = log_diam_ob, color = updated_tree_code)) +
#   theme(legend.position = "none") +
#   facet_wrap(~ species_name, ncol = 1)

## !!! Final data for stem profile !!!
data_clean$stem_log <- data_clean$stem_log_init |>
  mutate(
    dr = round(log_diam_ob / tree_dbh, 3),
    hr = round(log_base_pom / tree_total_length, 3)
  )


## Check relative D and H
# data_clean$stem_log |>
#   filter(log_meas_type %in% c("normal", "top")) |>
#   ggplot(aes(x = hr, y = dr)) +
#   geom_line(aes(color = updated_tree_code), alpha = 0.6) +
#   theme(legend.position = "none") +
#   facet_wrap(~species_group)


## Checks
data_clean$stem_log |>
  ggplot() +
  geom_line(data = data_clean$stem_log, aes(x = hr, y = dr, color = updated_tree_code)) +
  facet_wrap(~tree_species_group) +
  theme(legend.position = "none")

data_clean_gg$stem_check <- data_clean$stem_log |>
  filter(dr > 1.5 & hr < 0.2) |>
  ggplot() +
  geom_point(data = data_clean$stem_log, aes(x = hr, y = dr, color = tree_species_group), size = 0.2) +
  geom_point(aes(x = hr, y = dr), shape = 21, col = "red", size = 4) +
  geom_text_repel(aes(x = hr, y = dr, label = paste(updated_tree_code, log_no)), min.segment.length = 0, max.overlaps = 12) +
  facet_wrap(~tree_species_group) +
  #facet_grid(log_meas_type~conif) +
  theme(legend.position = "none")

print(data_clean_gg$stem_check)
# ggsave(
#   plot = data_clean_gg$stem_check, paste0("res/cleaning-examples/check_stem-", Sys.time(), ".png"),
#   width = 15, height = 12, units = "cm", dpi = 300
# )


tmp$check <- data_clean$stem_log |> filter(updated_tree_code == "505Sr123")


## !!! Remove for taper due to early fork - KEEP for volume !!!
data_clean$stem_log_taper <- data_clean$stem_log |>
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
tt <- data_clean$stem_log_taper |> 
  filter(updated_tree_code %in% c("252Pr029", "365Sw048"))

data_clean_gg$stem_check2 <- data_clean$stem_log_taper |>
  filter(dr < 0.95 & hr < 0.05) |>
  ggplot(aes(x = hr, y = dr)) +
  geom_point(data = data_clean$stem_log, aes(color = tree_species_group), size = 0.2) +
  geom_point(shape = 21, col = "red", size = 4) +
  geom_text_repel(aes(label = paste(updated_tree_code, log_no)), min.segment.length = 0, max.overlaps = 12) +
  geom_line(data = tt) +
  facet_wrap(~tree_species_group) +
  theme(legend.position = "none")

print(data_clean_gg$stem_check2)
# ggsave(
#   plot = data_clean_gg$stem_check2, paste0("res/cleaning-examples/check_stem-", Sys.time(), ".png"),
#   width = 15, height = 12, units = "cm", dpi = 300
#   )

## Line check
data_clean_gg$stem_check3 <- data_clean$stem_log_taper |>
  filter(log_meas_type %in% c("normal", "top")) |>
  ggplot(aes(x = hr, y = dr)) +
  geom_line(aes(color = updated_tree_code)) +
  facet_grid(tree_species_group~plot_prov) +
  theme(legend.position = "none")

print(data_clean_gg$stem_check3)

## More outlier checks
# data_clean_gg$stem_check4 <- data_clean$stem_log_taper |>
#   filter(log_meas_type %in% c("normal", "top")) |>
#   filter(province == "Madhesh", str_detect(species_name, "Shorea")) |>
#   ggplot(aes(x = hr, y = dr)) +
#   geom_line(aes(color = updated_tree_code)) +
#   facet_wrap(~updated_tree_code) +
#   theme(legend.position = "none")
# 
# print(data_clean_gg$stem_check4)
# 
# tt <- tmp$check206 <- data_clean$stem_log_taper |> filter(updated_tree_code == "073Sr021")

# 
# ## Save output table
# write_csv(data_clean$stem_log, "data/data-clean/stem.csv")
# write_csv(data_clean$stem_log_taper, "data/data-clean/stem_taper.csv")


## Remove temporary objects
rm(tmp)

