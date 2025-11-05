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

if (.Platform$OS.type == "windows") {
  font_add("Calibri", regular = "C:/Windows/Fonts/calibri.ttf")
} else if (.Platform$OS.type == "unix") {
  # Common locations on macOS or Linux, adjust as needed
  font_add("Calibri", regular = "/Library/Fonts/Calibri.ttf")
}

showtext_auto()
showtext_opts(dpi = 300)


# graphics
library(scales)
# library(ggbeeswarm)
library(patchwork)
library(gridExtra)
library(ggrepel)
library(ggbreak)
library(GGally)
library(RColorBrewer)
library(webshot2)

# tables
library(knitr)
library(kableExtra)
library(DT)
library(gt)
library(janitor)
