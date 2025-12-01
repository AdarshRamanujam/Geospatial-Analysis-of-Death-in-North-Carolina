Untitled
================

# Introduction

My research question is if the percentage of unvaccinated people against
the flu specifically is directly affecting the number of deaths in a
county, and whether other confounding factors like access to healthcare,
socioeconomic status, and other 3rd party factors that may affect this.
We will explore this with ANOVA and a bit of spatial correlation. The
location of the study is with data from 2020-2025 across all of North
Carolina with data from the US County Health Rankings.

# Background

In August 2025, the USHHS (US Health and Human Services Department) said
that they were discontinuing 22 mRNA vaccines that terminated production
of new flu and COVID vaccines from companies like Pfizer and Safoni
Pasteur (Source: USHHS). Additionally, with the spread of misinformation
about the benefits and the harms of vaccines floating around in online
spaces like X, Instagram, Facebook, etc, I wanted to examine whether the
relation between unvaccinated people specifically against the flu had a
positive or negative correlation with higher deaths rates per population
within the county, and whether confounding factors like socioeconomic
status, which would limit the amount of healthcare people in a lower
earning county may be able to obtain, would affect it more (which I
suspect will also contribute to vaccination rate as well). According to
the NIH, during COVID, once both lower and higher socioeconomic counties
had a relatively equal access to COVID vaccines, the unvaccinated people
started dying at twice the rate of the vaccinated people, as they didn’t
have the necessary antibodies to fight back. According to the American
Academy of Allergy Asthma and Immunology, they also help with
maintaining herd immunity against deadly diseases like measles and
pertussis (which have reemerged as of late). This study will either draw
a similar conclusion to the NIH, which will be my main hypothesis, that
the rate of vaccinations will directly correspond to rate of death
within a county regardless of confounding factors.

# Data

## Loading the Data

``` r
library(sf)
```

    ## Linking to GEOS 3.13.1, GDAL 3.11.0, PROJ 9.6.0; sf_use_s2() is TRUE

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(tmap)
library(tigris)
```

    ## To enable caching of data, set `options(tigris_use_cache = TRUE)`
    ## in your R script or .Rprofile.

``` r
library(ggplot2)
library(arcpullr)
```

    ## Warning: package 'arcpullr' was built under R version 4.5.2

In the following blocks of code, I clean and prep the data to use later
on.

``` r
nationalHealthData <- read.csv("https://drive.google.com/uc?export=view&id=1SkhzlEZJyRiCIoV69ws5s29X6j73IamO")

nc_counties_map <- counties('NC')
```

    ## Retrieving data for the year 2024

    ##   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |==                                                                    |   4%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |=========                                                             |  14%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |================                                                      |  24%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |=======================                                               |  34%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  39%  |                                                                              |============================                                          |  40%  |                                                                              |============================                                          |  41%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |==============================                                        |  44%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |===================================                                   |  51%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |=====================================                                 |  54%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |==========================================                            |  61%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |============================================                          |  64%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |========================================================              |  81%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |==========================================================            |  84%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |=================================================================     |  94%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%

``` r
nc_counties_map <- st_transform(nc_counties_map, "EPSG:32119")
plot(nc_counties_map$geometry)
```

![](project3github_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
nationalHealthData <- nationalHealthData |>
  filter(X.1 == 'North Carolina') 

nationalHealthData <- nationalHealthData[-1, ]

names(nationalHealthData)[names(nationalHealthData) == 'X.2'] <- 'NAME'

nc_health_map <- nc_counties_map |>
  inner_join(nationalHealthData, by='NAME')

nc_health_map <- nc_health_map |>
    filter(X.1 == 'North Carolina') 
```

The following map shows the vaccination rates across the NC counties.

``` r
  nc_health_map |>
  tm_shape(nc_counties_map) +
  tm_polygons(fill= 'Flu.Vaccinations',
              style="jenks",
              n = 7,
              title="% Flu Vaccinated (Jenks)\n2020-2025 USCHR",
              palette="Purples"
  ) +
  tm_layout(frame = TRUE,
            frame.color = "White",
            outer.margins=0,
            legend.position = tm_pos_out("right", "center"),
            legend.frame = TRUE,
            legend.frame.color = "White",
            title = "% Flu Vaccinated",
            title.position = tm_pos_out("center", "top")
  ) +
  tm_credits("Adarsh Ramanujam, November 25th\nData: USCHR\nGEOG 215",
             position = tm_pos_in("left", "bottom"))
```

    ## 

    ## ── tmap v3 code detected ───────────────────────────────────────────────────────

    ## [v3->v4] `tm_polygons()`: instead of `style = "jenks"`, use fill.scale =
    ## `tm_scale_intervals()`.
    ## ℹ Migrate the argument(s) 'style', 'n', 'palette' (rename to 'values') to
    ##   'tm_scale_intervals(<HERE>)'
    ## [v3->v4] `tm_polygons()`: migrate the argument(s) related to the legend of the
    ## visual variable `fill` namely 'title' to 'fill.legend = tm_legend(<HERE>)'
    ## [v3->v4] `tm_layout()`: use `tm_title()` instead of `tm_layout(title = )`
    ## [cols4all] color palettes: use palettes from the R package cols4all. Run
    ## `cols4all::c4a_gui()` to explore them. The old palette name "Purples" is named
    ## "brewer.purples"
    ## Multiple palettes called "purples" found: "brewer.purples", "matplotlib.purples". The first one, "brewer.purples", is returned.
    ## 
    ## [plot mode] fit legend/component: Some legend items or map compoments do not
    ## fit well, and are therefore rescaled.
    ## ℹ Set the tmap option `component.autoscale = FALSE` to disable rescaling.

![](project3github_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

As we can see, there is much variability within the state of NC for the
rate of vaccinations. According to the original dataset, there is an
average of 51% of vaccinations throughout the state.

Now, we will look at the death rates across the state.

``` r
ncCounties <- read_sf("https://drive.google.com/uc?export=download&id=1Ks38Gmdlp58GBhOgb6FBmx8tIAfu0QDw")

ncCounties <- ncCounties |>
  mutate(NAME = str_remove(NAME, " County, North Carolina"))

ncCounties_data <- st_drop_geometry(ncCounties)

nc_health_map <- nc_health_map |>
  inner_join(ncCounties_data, by = 'NAME')

library(stringr)

nc_health_map <- nc_health_map |>
  mutate(X.3 = str_remove_all(X.3, ","),
         X.3 = as.numeric(X.3)) |>
  
  mutate(death_rate = X.3 / popE * 100)

  nc_health_map |>
  tm_shape(nc_counties_map) +
  tm_polygons(fill= 'death_rate',
              style="jenks",
              n = 7,
              title="Death Rate in NC (Jenks)\n2020-2025 USCHR",
              palette="Reds"
  ) +
  tm_layout(frame = TRUE,
            frame.color = "White",
            outer.margins=0,
            legend.position = tm_pos_out("right", "center"),
            legend.frame = TRUE,
            legend.frame.color = "White",
            title = "Death Rate in NC",
            title.position = tm_pos_out("center", "top")
  ) +
  tm_credits("Adarsh Ramanujam, November 25th\nData: USCHR\nGEOG 215",
             position = tm_pos_in("left", "bottom"))
```

    ## 

    ## ── tmap v3 code detected ───────────────────────────────────────────────────────

    ## [v3->v4] `tm_polygons()`: instead of `style = "jenks"`, use fill.scale =
    ## `tm_scale_intervals()`.
    ## ℹ Migrate the argument(s) 'style', 'n', 'palette' (rename to 'values') to
    ##   'tm_scale_intervals(<HERE>)'
    ## [v3->v4] `tm_polygons()`: migrate the argument(s) related to the legend of the
    ## visual variable `fill` namely 'title' to 'fill.legend = tm_legend(<HERE>)'
    ## [v3->v4] `tm_layout()`: use `tm_title()` instead of `tm_layout(title = )`
    ## [cols4all] color palettes: use palettes from the R package cols4all. Run
    ## `cols4all::c4a_gui()` to explore them. The old palette name "Reds" is named
    ## "brewer.reds"
    ## Multiple palettes called "reds" found: "brewer.reds", "matplotlib.reds". The first one, "brewer.reds", is returned.

![](project3github_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
nc_health_map <- nc_health_map |>
  mutate(Flu.Vaccinations = as.numeric(Flu.Vaccinations))
```

As we can visually see, there is definitely some slight inverse
correlation between the vaccinations and death rates by observing with
our eyes, but I’m still not convinced. Let’s continue with our linear
regression to see if there is a concrete relation between the two.

# Methods

Here, we will test for the linear regression relation between
vaccination rates, death rates, and income using multiple linear
regressions.

This first graph will be one comparing median income and death rate per
county.

``` r
model1 <- lm(death_rate ~ median.incomeE, data=nc_health_map)
slope1 <- round(coef(model1)[2], digits=3)

nc_health_map |>
  ggplot(aes(x=median.incomeE, y=death_rate)) +
  geom_point() +
  geom_smooth() +
  annotate("text", label=slope1, x=2000, y=5, size=1, color="red")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](project3github_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
first.model.county <- lm(death_rate ~ median.incomeE, data=nc_health_map)
summary(first.model.county)
```

    ## 
    ## Call:
    ## lm(formula = death_rate ~ median.incomeE, data = nc_health_map)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.41478 -0.17132  0.02812  0.20492  1.11616 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     3.724e+00  2.154e-01  17.287  < 2e-16 ***
    ## median.incomeE -6.522e-05  7.947e-06  -8.207 9.15e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3334 on 98 degrees of freedom
    ## Multiple R-squared:  0.4073, Adjusted R-squared:  0.4013 
    ## F-statistic: 67.36 on 1 and 98 DF,  p-value: 9.154e-13

The r-square value is 0.4013, which means that 40.13% of the variation
of the data can be represented by the model. The p value of much less
than 0.05 (9.15\*10^-13), signifies that the data is statistically
significant.

Let’s see the relation between death rate and vaccination rate for the
flu.

``` r
model2 <- lm(death_rate ~ Flu.Vaccinations, data=nc_health_map)
slope2 <- round(coef(model1)[2], digits=3)

nc_health_map |>
  ggplot(aes(x=Flu.Vaccinations, y=death_rate)) +
  geom_point() +
  geom_smooth() +
  annotate("text", label=slope1, x=15, y=5, size=1, color="red")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](project3github_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
second.model.county <- lm(death_rate ~ Flu.Vaccinations, data=nc_health_map)
summary(second.model.county)
```

    ## 
    ## Call:
    ## lm(formula = death_rate ~ Flu.Vaccinations, data = nc_health_map)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.85619 -0.25552  0.01817  0.24373  1.01278 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.484409   0.293036  11.891  < 2e-16 ***
    ## Flu.Vaccinations -0.031778   0.006126  -5.187 1.15e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3836 on 98 degrees of freedom
    ## Multiple R-squared:  0.2154, Adjusted R-squared:  0.2074 
    ## F-statistic: 26.91 on 1 and 98 DF,  p-value: 1.152e-06

I had to troubleshoot since I didn’t realize that the data was also a
string for the flu vaccination percentage. 20.74% of the variation in
the data in the model can be explained by the model and the relation,
but that’s less than the median income with death rate. However, this is
still statistically significant, as obviously both are important for the
death rates and whether or not someone has access to healthcare.

Here, we will map out the residuals.

``` r
nc_health_map$resids <- first.model.county$residuals

tm_shape(nc_health_map) +
  tm_polygons(fill="resids", col=NULL,
              fill.scale = tm_scale_intervals(midpoint=0, values="-tableau.orange_blue_white_diverging"),
              fill.legend = tm_legend("Residuals - County 2020",
                                      position=tm_pos_out(pos.h="right")))
```

![](project3github_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
nc_health_map$resids <- second.model.county$residuals

tm_shape(nc_health_map) +
  tm_polygons(fill="resids", col=NULL,
              fill.scale = tm_scale_intervals(midpoint=0, values="-tableau.orange_blue_white_diverging"),
              fill.legend = tm_legend("Residuals - County 2020",
                                      position=tm_pos_out(pos.h="right")))
```

![](project3github_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

The residuals are much different, as expected.

``` r
as.data.frame(nc_health_map) |>
  select(Flu.Vaccinations,
         death_rate, median.incomeE) |>
  pairs(col="#69b3a2", lower.panel=panel.smooth)
```

![](project3github_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

# Results

Finally, the actual linear regression part across all three variables.

``` r
second.model.county <- lm(death_rate ~
                            Flu.Vaccinations + median.incomeE,
                          data=nc_health_map)
summary(second.model.county)
```

    ## 
    ## Call:
    ## lm(formula = death_rate ~ Flu.Vaccinations + median.incomeE, 
    ##     data = nc_health_map)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.32346 -0.18928  0.03673  0.20644  1.05513 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.919e+00  2.648e-01  14.801  < 2e-16 ***
    ## Flu.Vaccinations -8.401e-03  6.670e-03  -1.259    0.211    
    ## median.incomeE   -5.763e-05  9.956e-06  -5.789 8.71e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3324 on 97 degrees of freedom
    ## Multiple R-squared:  0.4169, Adjusted R-squared:  0.4049 
    ## F-statistic: 34.67 on 2 and 97 DF,  p-value: 4.359e-12

# Conclusion

As we can see from the linear regression model, flu vaccinations don’t
actually pose that much of an effect on overall effect on death rates
compared to median income across a county, since the p-value of the flu
vaccinations was .211 while the the median income was much less than
0.01 (8.71 \* 10^-8) which makes a ton of sense. People who don’t make
as much money as other people in different counties will often not only
not have access to better healthcare, but oftentimes they won’t have
access to good living conditions, food deserts, and just overall have an
environment that contributes to a poorer quality of life. However, this
doesn’t mean testing for the relation between death rate and
vaccinations was trivial, as earlier there was a relation between them
with an inversely correlated vaccination rate with the death rate,
however it isn’t necessarily as important as improving the quality of
life for our fellow citizens. Finally, I don’t reject my original
hypothesis, however, I need to subject it to further testing. Thank you
for reading, this was an amazing project to do and changed my view on
certain things.

# References

*HHS winds down mrna vaccine development under Barda*. HHS Winds Down
mRNA Vaccine Development Under BARDA \| HHS.gov. (2025, August 5).
<https://www.hhs.gov/press-room/hhs-winds-down-mrna-development-under-barda.html>

Matveeva, O., Ogurtsov, A. Y., & Shabalina, S. A. (2024, August 12).
*Impact of vaccination rates, pre-pandemic life expectancy, economic
status and age on covid-19 excess mortality across United States*.
medRxiv : the preprint server for health sciences.
<https://pmc.ncbi.nlm.nih.gov/articles/PMC10836123/>

*Vaccines: The myths and the facts*. American Academy of Allergy Asthma
& Immunology. (2025, October 2).
<https://www.aaaai.org/tools-for-the-public/conditions-library/allergies/vaccine-myth-fact>
