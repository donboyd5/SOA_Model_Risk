
library(rlang)
library(tidyverse)
tprint <- 75  # default tibble print
options(tibble.print_max = tprint, tibble.print_min = tprint) # show up to tprint rows
library(skimr)
library(pryr)
library(fs)

# probability and statistics
library(stats)
library(moments)

# fast random number generators
library(dqrng)
library(RcppZiggurat)

# tools
library(vroom)
library(readxl)
library(openxlsx) # for writing xlsx files
library(lubridate)
library(RColorBrewer)
library(RcppRoll)
library(fredr)
library(tidycensus)

# boyd libraries
library(btools)
library(bdata)
library(bggtools)
library(bmaps)

# graphics
library(scales)
library(ggbeeswarm)
library(patchwork)
library(gridExtra)
library(ggrepel)
library(ggbreak)
library(GGally)

# tables
library(knitr)
library(kableExtra)
library(DT)
library(gt)
library(janitor)
