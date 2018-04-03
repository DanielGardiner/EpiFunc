---
output: github_document
---

# EpiFunc


## Overview

EpiFunc is a collection of functions which output simnple epidemilogical visualisations. 


## Installation

```{r, eval = FALSE}
# You can install by typing:

devtools::install_github("DanielGardiner/EpiFunc")

# If you have issues with permissions try typing:

install.packages("httr")

httr::set_config(httr::config(ssl_verifypeer =0L))

devtools::install_github("DanielGardiner/EpiFunc")

```

## Examples 


```{r, eval = FALSE}
# set dummy data

set.seed(5)

data = data.frame(dates = sample(seq(as.Date('2014-01-01'), as.Date('2016-04-01'), by="day"), 200, replace = TRUE),
                  sex = sample(c("Male", "Female", "Unknown"), 200, replace = TRUE),
                  conf = sample(c("Confirmed", "Probable"), 200, replace = TRUE),
                  age = sample(c(NA, 1:100), 200, replace = TRUE),
                  geog = sample(c("Vienna", "Vienna", "Vienna", "Vienna",
                                  "Salzburg", "Innsbruck", "Graz", "Graz",
                                  "Linz", "Klagenfurt", "Villach"), 200, replace = TRUE))

data$age.grp = cut(as.numeric(data$age), breaks = c(0, 6, 16, 26, 46, 66, Inf),
                   include.lowest = TRUE)

data$month = format(data$dates, "%m")

# plot epicurve

EpiFunc::epicurve(data, date.col = "dates", time.period = "month",
                  fill.by="sex", split.by="conf", shade.by=NULL,
                  start.at = "2014-01-01", stop.at = "2016-04-20",
                  xlab="Month", ylab="Count",
                  fill.by.legend.title = NULL, shade.by.legend.title = NULL, 
                  angle=0, col.pal=7, label.breaks = 0, epi.squares = TRUE, 
                  blank.background = TRUE, na.rm = TRUE) 
```

<img src="man/figures/epicurve1.png" width="600" height="400"/>


