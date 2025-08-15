
library(readxl)
library(nlme)
library(systemfit)
library(tidyverse)

theme_set(theme_bw())

#stem <- read_xlsx("data/AE-raw-2025-03-25.xlsx", sheet = "04-stem-portion_corr", na = "NA")
plot_init <- read_xlsx("data/AE-raw-2025-03-25.xlsx", sheet = "01-general-info", na = "NA")

tree_init <- read_xlsx("data/AE-raw-2025-03-25.xlsx", sheet = "03-tree-info", na = "NA") 

plot <- plot_init |>
  select(tree_code_old, physiographic_region, province, district, altitude, forest_type)


table(plot$physiographic_region, useNA = "ifany")


tree <- tree_init |>
  filter(!is.na(stem_B)) |>
  left_join(plot, by = "tree_code_old") |>
  mutate(
    tree_d2h = tree_dbh^2 * tree_total_height,
    tree_d2hwd = tree_dbh^2 * tree_total_height * stem_wd
  ) |>
  filter(
    #tree_dbh < 80,
    !is.na(physiographic_region), 
    !is.na(forest_type), 
    physiographic_region != "High mountain")


ggplot(tree) +
  geom_point(aes(x = tree_dbh, y = stem_B))

ggplot(tree) +
  geom_point(aes(x = stem_V, y = stem_B))

ggplot(tree) +
  geom_point(aes(x = tree_d2h, y = stem_V, color = species_name))

ggplot(tree) +
  geom_point(aes(x = tree_dbh, y = stem_B)) +
  facet_wrap(~species_name)

ggplot(tree) +
  geom_point(aes(x = tree_dbh, y = stem_B, color = species_name)) +
  facet_wrap(~physiographic_region) +
  theme(legend.position = "bottom")

ggplot(tree) +
  geom_point(aes(x = tree_d2h, y = stem_B)) +
  facet_wrap(~forest_type)

ggplot(tree) +
  geom_point(aes(x = tree_d2h, y = stem_B, color = altitude)) 


## Test model stem B

lm_bd <- lm(log(stem_B) ~ log(tree_dbh), data = tree)
summary(lm_bd)
coef(lm_bd)

coef_bd <- c(exp(coef(lm_bd)[1]), coef(lm_bd)[2])
names(coef_bd) <- c("a", "b")

#coef_bd <- c(0.099, 2.36)

tree_nlme <- tree |>
  mutate(
    #group = "a",
    #group = physiographic_region,
    group = species_name,
    #group = altitude,
    #group = forest_type
  )

nlme_bd <- nlme(
  model = stem_B ~ a * tree_dbh^b,
  data = tree_nlme, 
  start = coef_bd,
  fixed = a + b ~ 1,
  groups = ~group, 
  weights = varPower(form = ~tree_dbh),
  control = nlmeControl(maxIter = 100)
)

AIC(nlme_bd)

summary(nlme_bd)
fixef(nlme_bd)
ranef(nlme_bd)

tree_res <- tree_nlme |>
  mutate(
    pred_bd = predict(nlme_bd),
    res_bd  = residuals(nlme_bd)
  )

tree_res |>
  ggplot(aes(x = pred_bd, y = res_bd)) +
  geom_point() +
  geom_smooth()

tree_res |>
  ggplot(aes(x = pred_bd, y = stem_B)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

tree_res |>
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = stem_B, color = group)) +
  #geom_line(aes(y = pred_bd, color = group)) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d()



