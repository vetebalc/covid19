## packages I want loaded for all pages of my site
suppressPackageStartupMessages({
  library(tidyverse)
})

## knitr options I want set as default for all ('global') code chunks
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, fig.show='hold')
options(scipen=999)
ggplot2::theme_set(cowplot::theme_minimal_grid() %+replace% 
                     theme(plot.caption = element_text(hjust = 0, face= "italic",
                                                       color = "gray30", size = 10)))
# ggplot2::theme_set(hrbrthemes::theme_ipsum(base_size = 10)%+replace% 
#                      theme(panel.grid.minor = element_blank()))