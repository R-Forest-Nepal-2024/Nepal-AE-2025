

table(data_clean$tree_stem_v$species_group)

data_md <- data_clean$tree_stem_v |>
  filter(
    !(species_name == "Largestroemia parviflora" & tree_dbh > 60),
    !(species_name == "Schima Wallichi" & tree_dbh > 60),
    !(species_name == "Shorea robusta" & tree_dbh > 110)
    ) |>
  mutate(no_group = "a")

## Check
tmp$outliers <- data_clean$tree_stem_v |>
  filter(
    (species_name == "Largestroemia parviflora" & tree_dbh > 60) |
    (species_name == "Schima Wallichi" & tree_dbh > 60) |
    (species_name == "Shorea robusta" & tree_dbh > 110)
  ) 

data_clean$tree_stem_v |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v)) +
  geom_point(aes(color = species_name), shape = 21) + 
  geom_point(data = tmp$outlier, shape = 21, col = "red", size = 6) +
  theme(legend.position = "none") +
  facet_wrap(~species_group)

## Start group modelling
vec_sp <- sort(unique(data_md$species_group))

md_v_sp <- map(vec_sp, function(x){
  
  data_sp <- filter(data_md, species_group == x)
  
  start <- coef(lm(log(tree_stem_v) ~ log(tree_d2h), data = data_sp))
  start[1] <- exp(start[1])
  #start <- c(1, 1)
  names(start) <- c("a", "b")
  
  md_sp <- nlme(
    model = tree_stem_v ~ a * tree_d2h^b, 
    data = data_sp,
    fixed = a + b ~ 1,
    groups = ~no_group,
    start = start, 
    weights = varPower(form = ~tree_dbh),
    #control = nlmeControl(maxIter = 100)
  )
  
  out2 <- nlme_out(.data = data_sp, .out_var = tree_stem_v, .in_var = tree_d2h, .start = start, .md = md_sp, .name_dev = "gs")
  out2$md_info |> mutate(species = x) |> select(-gr)
  
}) |> list_rbind()

## Check
a <- round(md_2p_d2h_nogp$parameters$value[1], 4)
b <- round(md_2p_d2h_nogp$parameters$value[2], 4)

p_aln <- md_v_sp |> filter(species == "Alnus nepalensis") |> pull(m_fixef) |> unlist()
p_cas <- md_v_sp |> filter(species == "Castonopsis group") |> pull(m_fixef) |> unlist()
p_lar <- md_v_sp |> filter(species == "Largestroemia parviflora") |> pull(m_fixef) |> unlist()
p_pin <- md_v_sp |> filter(species == "Pinus roxburghii") |> pull(m_fixef) |> unlist()
p_sch <- md_v_sp |> filter(species == "Schima Wallichi") |> pull(m_fixef) |> unlist()
p_sho <- md_v_sp |> filter(species == "Shorea robusta") |> pull(m_fixef) |> unlist()
p_ter <- md_v_sp |> filter(species == "Terminalia alata") |> pull(m_fixef) |> unlist()

data_md_sp <- data_md |>
  mutate(
    tree_md_vall = a * tree_d2h^b,
    tree_md_vsp  = case_when(
      species_group == "Alnus nepalensis"         ~ p_aln[1] * tree_d2h^p_aln[2],
      species_group == "Castonopsis group"        ~ p_cas[1] * tree_d2h^p_cas[2],
      species_group == "Largestroemia parviflora" ~ p_lar[1] * tree_d2h^p_lar[2],
      species_group == "Pinus roxburghii"         ~ p_pin[1] * tree_d2h^p_pin[2],
      species_group == "Schima Wallichi"          ~ p_sch[1] * tree_d2h^p_sch[2],
      species_group == "Shorea robusta"           ~ p_sho[1] * tree_d2h^p_sho[2],
      species_group == "Terminalia alata"         ~ p_ter[1] * tree_d2h^p_ter[2]        
    )
  )

data_md_sp |>
  ggplot(aes(x = tree_d2h)) +
  geom_point(aes(y = tree_stem_v)) +
  geom_line(aes(y = tree_md_vall), col = "darkred", linetype = "dashed") +
  geom_line(aes(y = tree_md_vsp), col = "lightgreen") +
  facet_wrap(~species_group)


##
## Details on species model
##

data_pin <- data_md |> filter(species_group == "Pinus roxburghii")

start <- coef(lm(log(tree_stem_v) ~ log(tree_d2h), data = data_pin))
start[1] <- exp(start[1])
#start <- c(1, 1)
names(start) <- c("a", "b")

md_pin <- nlme(
  model = tree_stem_v ~ a * tree_d2h^b, 
  data = data_pin,
  fixed = a + b ~ 1,
  groups = ~no_group,
  start = start, 
  weights = varPower(form = ~tree_dbh),
  #control = nlmeControl(maxIter = 100)
)

md_pin_2p <- nlme_out(.data = data_pin, .out_var = tree_stem_v, .in_var = tree_d2h, .start = start, .md = md_pin, .name_dev = "gs")
md_pin_2p



## 
## Species model with 3 params
##

md_v_sp_3p <- map(vec_sp, function(x){
  
  data_sp <- filter(data_md, species_group == x)
  
  start <- coef(lm(log(tree_stem_v) ~ log(tree_dbh) + log(tree_total_length), data = data_sp))
  start[1] <- exp(start[1])
  #start <- c(1, 1)
  names(start) <- c("a", "b", "c")
  
  md_sp <- nlme(
    model = tree_stem_v ~ a * tree_dbh^b * tree_total_length^c, 
    data = data_sp,
    fixed = a + b + c ~ 1,
    groups = ~no_group,
    start = start, 
    weights = varPower(form = ~tree_dbh),
    #control = nlmeControl(maxIter = 100)
  )
  
  out2 <- nlme_out(.data = data_sp, .out_var = tree_stem_v, .in_var = tree_dbh, .start = start, .md = md_sp, .name_dev = "gs")
  out2$md_info |> mutate(species = x) |> select(-gr)
  
}) |> list_rbind()


md_v_sp_2p_prov <- map(vec_sp, function(x){
  
  data_sp <- filter(data_md, species_group == x)
  
  start <- coef(lm(log(tree_stem_v) ~ log(tree_d2h), data = data_sp))
  start[1] <- exp(start[1])
  #start <- c(1, 1)
  names(start) <- c("a", "b")
  
  md_sp <- nlme(
    model = tree_stem_v ~ a * tree_d2h^b, 
    data = data_sp,
    fixed = a + b ~ 1,
    groups = ~province,
    start = start, 
    weights = varPower(form = ~tree_dbh),
    #control = nlmeControl(maxIter = 100)
  )
  
  out2 <- nlme_out(.data = data_sp, .out_var = tree_stem_v, .in_var = tree_d2h, .start = start, .md = md_sp, .name_dev = "gs")
  out2$md_info |> mutate(species = x) |> select(-gr)
  
}) |> list_rbind()



md_v_sp_all <- bind_rows(md_v_sp, md_v_sp_2p_prov, md_v_sp_3p) |> arrange(species)
md_v_sp_all

write_csv(md_v_sp_all, "res/v_species_compa.csv")
