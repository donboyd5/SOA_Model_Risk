# basic libraries
library(rlang)
library(tidyverse)
tprint <- 75 # default tibble print
options(tibble.print_max = tprint, tibble.print_min = tprint) # show up to tprint rows
library(skimr)
library(pryr)
library(fs)

# enhancements to tidyverse
# library(tidymodels)
# library(infer)

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
library(RcppRoll)
# library(fredr)
# library(tidycensus)

# portfolio analysis
library(PortfolioAnalytics)

# boyd libraries
# library(btools)
# library(bdata)
# library(bggtools)
# library(bmaps)

# colors for graphs and tables
library(showtext)
# Register font (regular style)
font_add("Calibri", regular = "C:/Windows/Fonts/calibri.ttf") # CAUTION: Windows-specific
showtext_auto()

# graphics
library(scales)
# library(ggbeeswarm)
library(patchwork)
library(gridExtra)
library(ggrepel)
library(ggbreak)
library(GGally)
library(RColorBrewer)


# tables
library(knitr)
library(kableExtra)
library(DT)
library(gt)
library(janitor)
