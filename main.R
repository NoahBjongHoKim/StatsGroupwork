library(dplyr)
library(tidyverse)
library(tidyr)

load("ESS_2018_CH.Rda")
ess <- ESS9
de <- ESS9 %>%
  filter(cntry == "DE") %>%
  filter(agea >= 18)

# N for DE: 2358
# N >= 18: 2273