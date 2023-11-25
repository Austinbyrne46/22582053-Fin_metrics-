# Purpose

This is the final README, it is a compilation of questions one through
seven.

``` r
rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
```

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 467586 25.0    1005767 53.8   660385 35.3
    ## Vcells 864407  6.6    8388608 64.0  1769630 13.6

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

``` r
#Loading all relevant packages. 
install.packages("pacman")
pacman::p_load(tidyverse)
pacman::p_load(readxl)
pacman::p_load(readr)
pacman::p_load(ggplot2)
pacman::p_load(gt)
library(gt)
library(tidyverse)
```

``` r
#Loading all relevant code from each questions code folder. 
list.files('C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/22582053 (Fin_metrics)/Question 1/code', full.names = T, recursive = T) %>% as.list() %>% walk(~source(.))
list.files('C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/22582053 (Fin_metrics)/Question 2/code', full.names = T, recursive = T) %>% as.list() %>% walk(~source(.))
list.files('C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/22582053 (Fin_metrics)/Question 3/code', full.names = T, recursive = T) %>% as.list() %>% walk(~source(.))
list.files('C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/22582053 (Fin_metrics)/Question 4/code', full.names = T, recursive = T) %>% as.list() %>% walk(~source(.))
list.files('C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/22582053 (Fin_metrics)/Question 5/code', full.names = T, recursive = T) %>% as.list() %>% walk(~source(.))
list.files('C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/22582053 (Fin_metrics)/Question 6/code', full.names = T, recursive = T) %>% as.list() %>% walk(~source(.))
list.files('C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/22582053 (Fin_metrics)/Question 7/code', full.names = T, recursive = T) %>% as.list() %>% walk(~source(.))
```

# Question 1

# Question 2

## Setting up the Texevier document for question 2

``` r
Texevier::create_template(directory = "C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/22582053 (Fin_metrics)",
                          template_name = "Question 2",
                          build_project = T, open_project = T)
```

# Question 3

## Setting up the Texevier documnet for question 3

``` r
Texevier::create_template(directory = "C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/22582053 (Fin_metrics)",
                          template_name = "Question 3",
                          build_project = T, open_project = T)
```

# Question 4

## Setting up the Texevier document for question 4

``` r
Texevier::create_template(directory = "C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/22582053 (Fin_metrics)",
                          template_name = "Question 4",
                          build_project = T, open_project = T)
```

# Question 5

## Setting up the Texevier document for question 5

``` r
Texevier::create_template(directory = "C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/22582053 (Fin_metrics)",
                          template_name = "Question 5",
                          build_project = T, open_project = T)
```

# Question 6

## Setting up the Texevier document for question 6

``` r
Texevier::create_template(directory = "C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/22582053 (Fin_metrics)",
                          template_name = "Question 6",
                          build_project = T, open_project = T)
```

# Question 7

## Setting up the TExevier document for question 7

``` r
Texevier::create_template(directory = "C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/22582053 (Fin_metrics)",
                          template_name = "Question 7",
                          build_project = T, open_project = T)
```
