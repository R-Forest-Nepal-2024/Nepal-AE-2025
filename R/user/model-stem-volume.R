
tmp <- list()

data_md <- data_clean$tree_stem_v |>
  filter(
    !(species_name == "Largestroemia parviflora" & tree_dbh > 60),
    !(species_name == "Schima Wallichi" & tree_dbh > 60),
    !(species_name == "Shorea robusta" & tree_dbh > 110),
    !is.na(species_name)
  ) |>
  mutate(no_group = "a")

## Check
ggplot(data_md, aes(x = tree_dbh, y = tree_stem_v)) +
  geom_point() +
  facet_wrap(~species_name)


##
## First model ####
##

## Step by step
## 1. Define model characteristics: equation, random paramters, variance function
## 2. prepare data
## 3. find starting values
## 4. run model
## 5. get AIC, errors and fitted values
## 6. make graphs 
 
## 1. V = a*DBH^b, no group, for Terminalia alata

## 2. prepare data 
data_ter <- data_md |> filter(species_name == "Terminalia alata")

## 3. find starting values
start <- coef(lm(log(tree_stem_v) ~ log(tree_dbh), data = data_ter))
start[1] <- exp(start[1])
# start <- c(0.001, 1)
# start <- c(10, 100)

## 4. run model
md <- nlme(
  model = tree_stem_v ~ a * tree_dbh^b, 
  data = data_ter,
  fixed = a + b ~ 1,
  groups = ~no_group,
  start = start, 
  weights = varPower(form = ~tree_dbh),
  control = nlmeControl(maxIter = 100)
)

## 5. get AIC, errors and fitted values
AIC(md)
fixef(md)

c_exp <- round(summary(md)$modelStruct$varStruct[1], 4)

data_ter <- data_ter |>
  mutate(
    pred    = predict(md),
    res     = residuals(md),
    res_std = residuals(md, type = "pearson"),
    res_w   = residuals(md) * 1 / tree_dbh^c_exp
  )

## 6. make graphs 
gg1 <- ggplot(data_ter, aes(x = tree_dbh)) +
  geom_point(aes(y = tree_stem_v), size = 0.1) +
  geom_line(aes(y = pred)) +
  theme(legend.position = "none")
print(gg1)

gg2 <- ggplot(data_ter, aes(x = pred)) +
  geom_point(aes(y = tree_stem_v), size = 0.1) +
  geom_abline(intercept = 0, slope = 1, col = "lightgreen")
print(gg2)

gg3 <- ggplot(data_ter, aes(x = pred, y = res)) +
  geom_point(size = 0.1) +
  geom_smooth(se = FALSE)
print(gg3)

gg4 <- ggplot(data_ter, aes(x = pred, y = res_std)) +
  geom_point(size = 0.1) +
  geom_smooth(se = FALSE)
print(gg4)

gg5 <- ggplot(data_ter, aes(x = pred, y = res_w)) +
  geom_point(size = 0.1) +
  geom_smooth(se = FALSE)
print(gg5)


## Exercise ####
## Develop model V = a*(DBH^2.H)^b and compare AIC
## Develop similar model for all species

