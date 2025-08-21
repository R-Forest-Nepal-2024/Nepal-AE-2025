
## Allometric equation development for Nepal
## Master script
## 2025-03
## Gael Sola, FAO, Thakur Sudebi, FRTC

## Setup
source("R/user/user-inputs.R")
source("R/setup/init.R")
source("R/setup/get-data.R")

## Clean data 
tic()
source("R/user/04-prepare-stem-log.R")
source("R/user/05-prepare-branch-log.R")
source("R/user/07-prepare-stem-disc.R")
source("R/user/08-prepare-branch-disc.R")
toc()

## Add calculations 
tic()
source("R/user/14a-add-stem-log-profile.R")
source("R/user/14b-add-stem-log-volume.R")
source("R/user/14c-add-stem-log-biomass.R")

source("R/user/15a-add-branch-log-biomass.R")
toc()

## Aggregate 
tic()
source("R/user/21-aggregate-tree.R")
toc()

## Models >> see R/models
# tic()
# source("R/user/model-stem-biomass.R")
# toc()

#source("R/user/model-stem-profile.R")

#source("R/user/model-stem-volume.R")
