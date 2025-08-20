

source("R/setup/init.R")

tree <- read_csv("res/tree-2025-08-20.csv", show_col_types = F)

##
## V0isual Checks ####
##

## + all trees ####
tree |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v)) +
  geom_point()

tree |>
  ggplot(aes(x = tree_d2h, y = tree_stem_v)) +
  geom_point() #+
  #geom_smooth(aes(group = tree_species_code, color = tree_species_code))


## + species level ####
tree |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~tree_species_code)

tree |>
  ggplot(aes(x = tree_d2h, y = tree_stem_v)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~tree_species_code)

## + filter outliers ####
## >> when not enough big trees, they are removed
tree |>
  filter(
    !(tree_species_name == "Largestroemia parviflora" & tree_dbh > 60),
    !(tree_species_name == "Schima Wallichi" & tree_dbh > 60),
    !(tree_species_name == "Shorea robusta" & tree_dbh > 110),
    !is.na(tree_species_name)
  ) |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~tree_species_code)


## 
## Prepare data ####
##

## For all data 4 outliers cause big trees too few
dat_md <- tree |>
  #filter(tree_dbh <= 60) |>
  filter(tree_d2h <= 50) |>
  mutate(no_group = "a") |>
  rename(V0 = tree_stem_v, D = tree_dbh, D2H = tree_d2h, H = tree_total_length) |>
  mutate(
    tree_conif = if_else(tree_species_code %in% c("Pr"), "conif", "broadl")
  )



##
## Run models ####
##

## + V0 = a * DBH^b ####

start <- coef(lm(log(V0) ~ log(D), data = dat_md))
start[1] <- exp(start[1])
# start <- c(0.001, 1)

md <- nlme(
  model = V0 ~ a * D^b, 
  data = dat_md,
  fixed = a + b ~ 1,
  groups = ~no_group,
  start = start, 
  weights = varPower(form = ~D)#,
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

spall_v_d <- nlme_out(
  .data = dat_md,
  .out_var = V0,
  .in_var = D,
  .start = start,
  .md = md,
  .name_dev = "GS"
)
spall_v_d$md_info$AIC
spall_v_d$graph
spall_v_d$md_summary


## + V0 = a * D2H^b ####

start <- coef(lm(log(V0) ~ log(D2H), data = dat_md))
start[1] <- exp(start[1])
start <- c(0.001, 1)

md <- nlme(
  model = V0 ~ a * D2H^b, 
  data = dat_md,
  fixed = a + b ~ 1,
  groups = ~no_group,
  start = start, 
  weights = varPower(form = ~D),
  control = nlmeControl(maxIter = 100)
)

AIC(md)

spall_v_d2h <- nlme_out(
  .data = dat_md,
  .out_var = V0,
  .in_var = D2H,
  .start = start,
  .md = md,
  .name_dev = "GS"
)
spall_v_d2h$md_info$AIC
spall_v_d2h$graph
spall_v_d2h$md_summary

## + V0 = a * D2H^b, species group ####

start <- coef(lm(log(V0) ~ log(D2H), data = dat_md))
start[1] <- exp(start[1])
# start <- c(0.001, 1)

md <- nlme(
  model = V0 ~ a * D2H^b, 
  data = dat_md,
  fixed = a + b ~ 1,
  groups = ~tree_species_code,
  start = start, 
  weights = varPower(form = ~D)#,
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

spall_v_d2h_sp <- nlme_out(
  .data = dat_md,
  .out_var = V0,
  .in_var = D2H,
  .start = start,
  .md = md,
  .name_dev = "GS"
)
spall_v_d2h$md_info$AIC
spall_v_d2h$graph
spall_v_d2h$md_summary


## + V0 = a * DBH^b * H^c ####

start <- coef(lm(log(V0) ~ log(D/100) + log(H), data = dat_md))
start[1] <- exp(start[1])
# start <- c(0.001, 1)

md <- nlme(
  model = V0 ~ a * (D/100)^b * H^c, 
  data = dat_md,
  fixed = a + b + c ~ 1,
  groups = ~no_group,
  start = start, 
  weights = varPower(form = ~D)#,
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

spall_v_dxh <- nlme_out(
  .data = dat_md,
  .out_var = V0,
  .in_var = D,
  .start = start,
  .md = md,
  .name_dev = "GS"
)
spall_v_dxh$md_info$AIC
spall_v_dxh$graph
spall_v_dxh$md_summary



##
## Group results ####
##

spall_v <- str_subset(ls(), pattern = "spall_v_") %>%
  set_names() %>%                   
  map(~ get(.x)$md_info) %>%
  bind_rows(.id = "source") 
spall_v

gg_spall_v <- str_subset(ls(), pattern = "spall_v_") %>%
  set_names() %>%                   
  map(~ get(.x)$graph) %>%
  ggarrange(plotlist = ., ncol = 1) 
gg_spall_v

## no filter: c(a = "0.3690", b = "0.9438")
## D2H <= 50: c(a = "0.3691", b = "0.9445")


