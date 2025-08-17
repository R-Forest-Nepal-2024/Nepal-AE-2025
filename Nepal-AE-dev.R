
## Allometric equation development for Nepal
## Master script
## 2025-03
## Gael Sola, FAO, Thakur Sudebi, FRTC


## Initiate objects
usr <- list()
usr$data_file <- "./data/AE-raw-2025-04-01.xlsx"

data_init <- list()
data_clean <- list()
data_clean_gg <- list()

## Model storage
stem_taper <- list()

source("R/setup/init.R")

source("R/setup/get-data.R")

source("R/user/prepare-stem-profile.R")

source("R/user/prepare-stem-volume.R")

source("R/user/prepare-stem-biomass.R")


## Models

source("R/user/model-stem-profile.R")

#source("R/user/model-stem-volume.R")
