

source("R/setup/init.R")

tree <- read_csv("res/tree-2025-08-20.csv", show_col_types = F)

##
## Visual Checks ####
##
tmp <- list()
tmp$missing <- tree |> filter(is.na(tree_stem_v20))

## + all trees ####
tree |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v20)) +
  geom_point()

tree |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v)) +
  geom_point() +
  geom_point(aes(y = tree_stem_v20), shape = 4, col = "darkred")

tree |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v)) +
  geom_point() +
  geom_smooth(aes(group = tree_species_code, color = tree_species_code))

tree |>
  ggplot(aes(x = tree_d2h, y = tree_stem_v20)) +
  geom_point() +
  geom_smooth(aes(group = tree_species_code, color = tree_species_code))


## + species level ####
tree |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v20)) +
  geom_point() +
  #geom_smooth() +
  facet_wrap(~tree_species_code)

tree |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v)) +
  geom_point() +
  geom_point(aes(y = tree_stem_v20), shape = 4, col = "darkred") +
  geom_point(data = tmp$missing, aes(y = tree_stem_v), shape = 23, size = 4, col = "darkred") +
  #geom_smooth() +
  facet_wrap(~tree_species_code)

tree |>
  ggplot(aes(x = tree_d2h, y = tree_stem_v20)) +
  geom_point() +
  #geom_smooth() +
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
  filter(!is.na(tree_stem_v20)) |>
  #filter(tree_dbh <= 110) |>
  #filter(tree_d2h <= 50) |>
  mutate(no_group = "a") |>
  rename(V20 = tree_stem_v20, D = tree_dbh, D2H = tree_d2h, H = tree_total_length) |>
  mutate(
    tree_conif = if_else(tree_species_code %in% c("Pr"), "conif", "broadl")
  )



##
## Run models ####
##

## + V20 = a * DBH^b ####

start <- coef(lm(log(V20) ~ log(D), data = dat_md))
start[1] <- exp(start[1])
# start <- c(0.001, 1)

md <- nlme(
  model = V20 ~ a * D^b, 
  data = dat_md,
  fixed = a + b ~ 1,
  groups = ~no_group,
  start = start, 
  weights = varPower(form = ~D)#,
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

spall_v20_d <- nlme_out(
  .data = dat_md,
  .out_var = V20,
  .in_var = D,
  .start = start,
  .md = md,
  .name_dev = "GS"
)
spall_v20_d$md_info$AIC
spall_v20_d$graph
spall_v20_d$md_summary


## + V20 = a * D2H^b ####

start <- coef(lm(log(V20) ~ log(D2H), data = dat_md))
start[1] <- exp(start[1])
#start <- c(0.001, 1)

md <- nlme(
  model = V20 ~ a * D2H^b, 
  data = dat_md,
  fixed = a + b ~ 1,
  groups = ~no_group,
  start = start, 
  weights = varPower(form = ~D),
  control = nlmeControl(maxIter = 100, msMaxIter = 200)
)

AIC(md)

spall_v20_d2h <- nlme_out(
  .data = dat_md,
  .out_var = V20,
  .in_var = D2H,
  .start = start,
  .md = md,
  .name_dev = "GS"
)
spall_v20_d2h$graph
spall_v20_d2h$md_summary

## + V20 = a * D2H^b, species group ####

start <- coef(lm(log(V20) ~ log(D2H), data = dat_md))
start[1] <- exp(start[1])
start <- c(0.1, 1)

md <- nlme(
  model = V20 ~ a * D2H^b, 
  data = dat_md,
  fixed = a + b ~ 1,
  random = a ~ 1,
  groups = ~tree_conif,
  start = start, 
  weights = varPower(form = ~D2H),
  control = nlmeControl(maxIter = 100, msMaxIter = 200)
)

AIC(md)
summary(md)
ranef(md)

dat_md |>
  ggplot(aes(x = D2H, y = V20)) +
  geom_point() +
  geom_line(aes(y = predict(md), color = tree_species_code))# +
  #facet_wrap(~tree_species_code) +
  #theme(legend.position = "none")


spall_v20_d2h_sp <- nlme_out(
  .data = dat_md,
  .out_var = V20,
  .in_var = D2H,
  .start = start,
  .md = md,
  .name_dev = "GS"
)
spall_v20_d2h$md_info$AIC
spall_v20_d2h$graph
spall_v20_d2h$md_summary


## + V20 = a * DBH^b * H^c ####

start <- coef(lm(log(V20) ~ log(D/100) + log(H), data = dat_md))
start[1] <- exp(start[1])
# start <- c(0.001, 1)

md <- nlme(
  model = V20 ~ a * (D/100)^b * H^c, 
  data = dat_md,
  fixed = a + b + c ~ 1,
  groups = ~no_group,
  start = start, 
  weights = varPower(form = ~D)#,
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

spall_v20_dxh <- nlme_out(
  .data = dat_md,
  .out_var = V20,
  .in_var = D,
  .start = start,
  .md = md,
  .name_dev = "GS"
)
spall_v20_dxh$md_info$AIC
spall_v20_dxh$graph
spall_v20_dxh$md_summary


## + V20 = a * DBH^b * H^c, species ####

start <- coef(lm(log(V20) ~ log(D/100) + log(H), data = dat_md))
start[1] <- exp(start[1])
# start <- c(0.001, 1)

md <- nlme(
  model = V20 ~ a * (D/100)^b * H^c, 
  data = dat_md,
  fixed = a + b + c ~ 1,
  #random = a ~ 1,
  groups = ~tree_species_code,
  start = start, 
  weights = varPower(form = ~D),
  control = nlmeControl(maxIter = 100, msMaxIter = 200)
)

AIC(md)
#summary(md)
ranef(md)

spall_v20_dxh_sp <- nlme_out(
  .data = dat_md,
  .out_var = V20,
  .in_var = D,
  .start = start,
  .md = md,
  .name_dev = "GS"
)

spall_v20_dxh_sp$graph
spall_v20_dxh_sp$md_summary

spall_v20_dxh_sp$data |>
  ggplot(aes(x = D2H, y = V20)) +
  geom_point(size = 2) +
  geom_point(aes(y = pred), color = "darkred") + 
  geom_line(data = spall_v20_d2h_sp$data, aes(y = pred), color = "darkgreen") +
  facet_wrap(~tree_species_code) +
  xlim(0, 40)

## Compa D2H
spall_v20_d2h_sp$data |>
  ggplot(aes(x = D2H, y = V20)) +
  geom_point(size = 2) +
  geom_line(aes(y = pred, color = tree_conif)) 

##
## Group results ####
##

spall_v20 <- str_subset(ls(), pattern = "spall_v20_") %>%
  set_names() %>%                   
  map(~ get(.x)$md_info) %>%
  bind_rows(.id = "source") 
spall_v20

gg_spall_v20 <- str_subset(ls(), pattern = "spall_v20_") %>%
  set_names() %>%                   
  map(~ get(.x)$graph) %>%
  ggarrange(plotlist = ., ncol = 1) 
gg_spall_v20

## no filter: c(a = "0.1225", b = "1.8320", c = "1.2600"+ me on species
## D2H <= 50: c(a = "0.1535", b = "1.8801", c = "1.2047")
## D <= 110: c(a = "0.1535", b = "1.8801", c = "1.2047")

#View(spall_v20_d2h_sp$md_info)



