# Does the Labour Market Subdivision Work?

Hedwig Nora Nordlinder

## Introduction

The standard administrative subdivision of Sweden is Country -\> County
-\> Municipality. All of these have legal significance and their borders
have historically been determined by political decisions. Alongside the
standard administrative subdivisions Statistics Sweden (Statistiska
Centralbyrån) as well as other government agencies have created
subdivisions for statistical purposes, such as the Regional Statistical
Areas (RegSO), Demographic Statistical Areas (DeSO) and local labour
markets (lokala arbetsmarknader). I will in this post take a special
interest in the local labour market subdivision. The material is quite
technical (admittedly more than initially intended) so I envision the
primary target audience to be the people responsible for developing and
maintaining the local labour market standard in various countries.

The local labour markets are created by first determining which
municipalities can count as “local centers”. A local center is a
municipality in which

- at least 80% of the population works in it

- at most 7,5% of the population commutes to any other specific
  municipality

After the local centers are created all remaining municipalities are
assigned to the labour market created by the local center that most of
its commuting residents commute to. (Construction and use of labour
market areas in Sweden, Statistics Sweden 2010).

Quoting directly from the aforementioned SCB report: “The purpose of
local labour markets is to describe the functioning of the labour market
for geographical areas that are relatively independent of the outside
world with respect to supply and demand of labour” (translation my own,
original
[source](https://share.scb.se/ov9993/data/publikationer/statistik/_publikationer/am0207_2009a01_br_am95br1001.pdf)
in Swedish). The definition of local labour market used in this post can
be found
[here](https://www.scb.se/hitta-statistik/statistik-efter-amne/arbetsmarknad/sysselsattning-forvarvsarbete-och-arbetstider/befolkningens-arbetsmarknadsstatus/produktrelaterat/fordjupad-information/lokala-arbetsmarknader-la/)

This post will ask and answer the question: Does this method of
subdivision work by testing whether or not the local labour markets are
mostly independent of the outside world. To perform this test this I
will analyse how this subdivision type interacted with the COVID-19
pandemic.

## The COVID-19 pandemic - A novel stress test

In 2020 the COVID pandemic swept over the world, with Sweden being no
exception. It is perhaps the best monitored and documented pandemic in
the history of the world. In particular, the granular data collected by
the Swedish Public Health Agency (Folkhälsomyndigheten) provides an
excellent opportunity to test if the labour market regions live up to
their purpose of being “relatively independent of the outside world”.
([This](https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/pxweb/sv/A_Folkhalsodata/A_Folkhalsodata__H_Sminet__covid19__falldata/bcov19Kom.px/)
source was used to download COVID-19 case data. To replicate, download
the result of [this
query](https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/sq/02907364-efc9-4436-ab67-3c66c8c0e0b1))

If labour market regions are truly independent economic units we would
expected COVID-19 spread patterns between adjacent labour markets to be
less correlated than say, between counties (whose borders are not drawn
with the explicit purpouse of defining self contained economic zones)

This formulation will lead the layman to ask questions such as “What
counts as ‘less correlated’” and the expert asking questions such as
“How do we define ‘correlation’”. I will make explicit my modeling
assumptions later in the post, but first, let us visualise the spread of
COVID through Swedish municipalities. While I wish to keep the amount of
code displayed to a minimum, credit is due to Filip Wästberg for the
creation of the R package “swemaps2”, which I make heavy use of.

``` r
library(remotes)
remotes::install_github("filipwastberg/swemaps2")
```

Now that we have installed swemaps2 we can make a map of Swedens labour
market regions. They changed ever so slightly during the pandemic, so we
will have to account for that in our modeling.

<figure>
<img src="la_markets.png" alt="Swedish labour markets, 2020 - 2023" />
<figcaption aria-hidden="true">Swedish labour markets, 2020 -
2023</figcaption>
</figure>

<figure>
<img src="municipal_spread.gif"
alt="Geographic evolution of the COVID-19 pandemic" />
<figcaption aria-hidden="true">Geographic evolution of the COVID-19
pandemic</figcaption>
</figure>

While certainly pretty it would be hard to discern any meaningful
patterns by just peering over these visualisations, even if we were to
overlay the borders of our subdivisions of interest. This creates the
need for a good statistical model to estimate the effect adjacent labour
markets have on each other. We also need to answer the question of what
counts as a strong relationship, and how this relationship will be
measured.

## Modeling

The most appropriate model from my limited experience and understanding
would probably be zero-inflated negative binomial regression. However,
to avoid having to verify complicated modeling assumptions in an area
where I lack expertise, as well as the computational cost of fitting
such a model I have instead resorted to the following:

We first define our modeling target, which will be the log geometric
change in cases in a municipality between time (measured in weeks) $t$
and $t-1$. We call this
`[log case ratio]_{i,t} = α + β*1_{time = t} + γ*1_{county of municipality i} + ε_{i,t}`.
The reason we set our modeling target to the log geometric change in
cases is partially to avoid having to model count data and partially to
avoid (or at least reduce) the autocorrelation of case counts over time.
With our modeling target defined we can now define our model, which will
be

`[log case ratio]_{i,t} = α + β*1_{time = t} + γ*1_{county of municipality i} + ε_{i,t}`.

This represents a fixed effects model with a time effect and a labour
market effect.

We have defined our modeling target, great. But now to the question of
what counts as a “weak relationship” between adjacent labour markets. A
priori we have very limited information about how such a relationship
would look, so to gain information we will run the same model but using
adjacent counties instead. In theory county borders should be largely
unrelated (or at least less related) to labour market, since most county
borders stem from the Axel Oxenstierna county reform of 1614. While
Oxenstierna undeniably was a great statesman I doubt that he managed to
predict the structure of the Swedish labour market in the 21st century,
so his borders should serve as a good benchmark. This gives us the
“Oxenstierna” or county model

$[\text{log case ratio}]_{i,t} = \alpha + \beta\mathbf{1}_{time = t} + \gamma\mathbf{1}_{\text{county of municipality i}} + \epsilon_{i,t}$.

We will not pay too much attention to the standard modelling assumptions
of linear regression (conditionally independent response variable, iid
normal errors with constant variance) since we will not be performing
frequentist tests or relying on the likelihood. Instead, we will measure
the correlation of the models residuals to the log case count in
adjacent labour markets/counties.

It may seem counterintuitive, but if labor market regions are truly
functioning as independent units, we would expect adjacent regions to
have less predictive power for COVID-19 spread compared to the
predictive power of a county based model.

To make this comparison we will need to handle both the fact that the
amount of labour market regions changed and that the constituent
municipalities changed during the pandemic. There were 69 labour market
regions in 2020, which shrunk to 67 for 2021 - 2023 (perhaps due to
decreased travel caused by the pandemic). We also have to handle that
2020 was a leap year with 53 weeks. I have also decided to use the 2023
labour market regions for 2024 since the updated ones were not available
at the time of writing. Finally, we will also have to remove Gotland
since it both as a labour market and as a county lacks adjacent labour
markets / counties, making models including it un-identifiable.

Several hundred lines of cumbersome data wrangling later and we are
finally ready to run our models, which are shown in the code below

We will start by computing Spearmans $\rho$ for each municipality and
plotting it

``` r
source("correlation_functions.R")

la_anova_model <- lm(log(case_ratios) ~ as.factor(time) + as.factor(la_region), data = compound_la_adjacency_frame)
county_anova_model <- lm(log(case_ratios) ~ as.factor(time) + as.factor(county), data = compound_county_adjacency_frame)

compound_la_adjacency_frame$fe_residuals <- resid(la_anova_model)
compound_county_adjacency_frame$fe_residuals <- resid(county_anova_model)


la_municipal_correlations <- municipal_spearman_rho(compound_la_adjacency_frame)
county_municipal_correlations <- municipal_spearman_rho(compound_county_adjacency_frame)

la_municipal_correlations <- merge(la_municipal_correlations, municipality, by.x = "municipal_code", by.y = "kn_kod")
county_municipal_correlations <- merge(county_municipal_correlations, municipality, by.x = "municipal_code", by.y = "kn_kod")
```

![](main_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Again while the map form of visualisation certainly is pleasing to the
eye a histogram can’t hurt.

![](main_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

## Conclusions

The rank correlations for the county based model show a fatter right
tail and mean shifted to the right of the labour market model,
consistent with labour markets being more independent entities than
counties. That is, at least directionally, the relationship is
consistent with Statistics Swedens goals and provides some evidence that
they function as intended.

This finding should of course be interpreted with several important
caveats, some of the following are listed:

- COVID-19 spread should provide a very good proxy to how human movement
  patterns look on small timescales. However, an economic shock like the
  COVID-19 pandemic is not representative for how the labour market
  normally functions. Work from home increased and commuting patterns
  probably radically changed as a consequence of the pandemic, with a
  potential impact on the conclusions drawn

- It is hard to interpret if the effect size is any meaningful, since it
  is measured on the *ranks* of the data, causing us to loose
  information about the magnitude of deviations.

Initially I tried adding the adjacent cases as a parameter to our linear
model, but getting the dataset to adhere in any way to the necessary
modelling assumptions was almost impossible. Given more time I would
have created a zero inflated negative binomial model.

Despite these limitations it is highly encouraging that the quantitative
evidence from the COVID-19 pandemic seems to vindicate the methodology
of Statistics Sweden, especially since many other countries have adopted
this methodology (such as Finland). In any case it would be highly
worrying if adjacent labour markets showed higher interdependence than
adjacent counties, which would seriously call into question the
effectiveness of the method of creating supposedly self-contained labour
markets.

My findings show that despite the crudeness of the approach of
Statistics Sweden they seem to be able to capture true economic patterns
in the Swedish labour market, as evidenced by the consistently lower
rank correlation of adjacent labour markets compared to adjacent
counties. I also believe that this novel way of evaluating the validity
of regional sub-divisions with pandemics could be extended to both more
advanced pandemic models and other countries where data is granular
enough.

## How to replicate

- Clone this repository
- Run the main.Rmd file
