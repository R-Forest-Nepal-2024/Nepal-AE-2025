

##
## Read final data from Nepal team ####
##

source("R/setup/init.R")

tree_init <- read_csv("data/final_tree_data_all_23_08_2025.csv", show_col_types = F)


names(tree_init)
summary(tree_init)

## Rename cols
tree <- tree_init |>
  rename(
    D = dbh,
    H = ht,
    WD = wd_final,
    V = vol_stem_ob,
    V10 = vol_stem_10_ub,
    V20 = vol_stem_20_ub,
    Bst = biom_stem,
    Bbb = biom_bb,
    Bsb = dry_sb_biom_kg,
    Bdb = biom_dead_b,
    Ble = biom_foliage,
    Btot_orig = biom_tree_total
  ) |>
  mutate(
    Ble = if_else(is.na(Ble), 0, Ble),
    Btot = Bst + Bbb + Bsb + Bdb + Ble,
    D2H = (D/100)^2*H,
    D2HWD = D2H * WD / 1000 #conv kg/m3 to t/m3
    ) |>
  mutate(across(starts_with("B"), \(x) x / 1000))

tree |>
  ggplot(aes(x = Btot_orig, y = Btot)) +
  geom_point()

tree |>
  ggplot(aes(x = Bst, y = Btot)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "darkred")


tree |>
  ggplot(aes(x = Bbb, y = Btot)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "darkred")

tree |>
  ggplot(aes(x = Bsb, y = Btot)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "darkred")

tree |>
  ggplot(aes(x = Bsb, y = Btot)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "darkred")


## 
## Checks ####
##

## +  Check stem volume ####
tree |>
  ggplot(aes(x = D, y = V, color = species)) +
  geom_point(shape = 1) +
  theme(legend.position = "none") +
  labs(color = "")

tree |>
  filter(D < 110) |>
  ggplot(aes(D, color = species)) +
  geom_point(aes(y = V), shape = 1) +
  theme(legend.position = "none") +
  labs(color = "")

tree |>
  #filter(D < 70) |>
  ggplot(aes(x = D2H, color = species)) +
  geom_point(aes(y = V), shape = 1) +
  theme(legend.position = "none") +
  labs(color = "")

tree |>
  filter(D < 110) |>
  ggplot(aes(x = D2H, color = species)) +
  geom_point(aes(y = V), shape = 1) +
  theme(legend.position = "none") +
  labs(color = "")

tree |>
  #filter(D2H < 40) |>
  ggplot(aes(x = D2H, color = species)) +
  geom_point(aes(y = V), shape = 1) +
  theme(legend.position = "none") +
  labs(color = "")

tree |>
  filter(D < 110) |>
  ggplot(aes(x = D2H, color = species)) +
  geom_point(aes(y = V), shape = 1) +
  theme(legend.position = "none") +
  labs(color = "") +
  facet_wrap(~species)


tmp$outlier <- tree |> filter(D2H > 40)
tmp$outlier2 <- tree |> filter(D > 110)

data_clean_gg$check_V <- tree |>
  #filter(D <= 110) |>
  ggplot(aes(x = D2H, y = V, color = species)) +
  geom_point() +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outlier2, shape = 22, size = 6, col = "red") +
  geom_text_repel(data = tmp$outlier, aes(label = tree_id)) +
  theme(legend.position = "none") +
  labs(color = "")
print(data_clean_gg$check_V)

# ggsave(
#   plot = data_clean_gg$check_v, paste0("res/cleaning-examples/check_v-", Sys.time(), ".png"),
#   width = 15, height = 12, units = "cm", dpi = 300
#   )


## + Check species level Volume ####

tmp$outliers <- tree |>
  filter(D2H <= 40) |>
  filter(
    (species == "Lp" & D > 60) |
      (species == "Sw" & D > 60) |
      (species == "Sr" & D > 110)
  ) 

data_clean_gg$check_V_sp <- tree |>
  #filter(D2H <= 40) |>
  ggplot(aes(x = D, y = V)) +
  geom_point(aes(color = species), shape = 21) + 
  geom_point(data = tmp$outlier, shape = 21, col = "red", size = 6) +
  geom_point(data = tmp$outliers, shape = 22, col = "red", size = 6) +
  theme(legend.position = "none") +
  facet_wrap(~species)
print(data_clean_gg$check_V_sp)

tree |>
  filter(D2H <= 40) |>
  ggplot(aes(x = D, y = V)) +
  geom_point(aes(color = species), shape = 21) + 
  geom_point(data = tmp$outliers, shape = 22, col = "red", size = 6) +
  theme(legend.position = "none")


## + Check stem biomass ####

tree |>
  ggplot(aes(x = D, y = Bst, color = species)) +
  geom_point(shape = 21) +
  theme(legend.position = "none")

tree |>
  ggplot(aes(x = D2H, y = Bst, color = species)) +
  geom_point(shape = 21) +
  theme(legend.position = "none")

tree |>
  ggplot(aes(x = D2HWD, y = Bst, color = species)) +
  geom_point(shape = 21) +
  theme(legend.position = "none")


tree |>
  filter(D2H <= 40) |>
  ggplot(aes(x = D2HWD, y = Bst, color = species)) +
  geom_point(shape = 21) +
  theme(legend.position = "none")

tree |>
  filter(D2H <= 40) |>
  ggplot(aes(x = D, y = Bst, color = species)) +
  geom_point(shape = 21) +
  theme(legend.position = "none") +
  facet_wrap(~species)

tree |>
  filter(D2H <= 40) |>
  ggplot(aes(x = D2H, y = Bst, color = species)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none") +
  facet_wrap(~species)

tree |>
  filter(D2H <= 40) |>
  ggplot(aes(x = D2H, y = Bst, color = species)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")

tree |>
  ggplot(aes(x = D2HWD, y = Bst, color = species)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")


data_clean_gg$check_Bst <- tree |>
  #filter(D2H <= 40) |>
  ggplot(aes(x = D2HWD, y = Bst, color = species)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none") +
  facet_wrap(~species)
print(data_clean_gg$check_Bst)

## + Check branch biomass #### 

tree |>
  ggplot(aes(x = D, y = Bbb, color = species)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")

data_clean_gg$check_Bbb <- tree |>
  #filter(D2H <= 40) |>
  ggplot(aes(x = D2HWD, y = Bbb, color = species)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none") +
  facet_wrap(~species)
print(data_clean_gg$check_Bbb)


## + Check total biomass ####

tree |>
  ggplot(aes(x = D, y = Btot, color = species)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")

tree |>
  ggplot(aes(x = D2H, y = Btot, color = species)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")

tree |>
  ggplot(aes(x = D2HWD, y = Btot, color = species)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")

tree |>
  ggplot(aes(x = D2HWD, y = Btot, color = species)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none") +
  facet_wrap(~species)


data_clean_gg$check_Btot <- tree |>
  filter(D2HWD <= 30) |>
  ggplot(aes(x = D2HWD, y = Btot, color = species)) +
  geom_point(shape = 21) +
  #geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none") +
  facet_wrap(~species)
print(data_clean_gg$check_Btot)


data_clean_gg$Btot <- tree |>
  #filter(D2H <= 40) |>
  ggplot(aes(x = D2HWD, y = Btot, color = species)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")
print(data_clean_gg$Btot)




