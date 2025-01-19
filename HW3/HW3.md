# IRIS data

We will investigate the relationship between sepal and petal dimensions using a scatterplot. We begin by loading necessary libraries and data


``` r
library(ggplot2)
library(patchwork)
library(cowplot)
iris_data <- read.csv("IRIS.csv")

colour_values =  c("Iris-setosa" = "blue", "Iris-versicolor" = "green", "Iris-virginica" = "red")

length_plot <- ggplot(data = iris_data, aes(x = sepal_length, y = petal_length, col = species)) + scale_color_manual(values = colour_values) + geom_point(show.legend = FALSE) + theme_classic()

width_plot <- ggplot(data = iris_data, aes(x = sepal_width, y = petal_width, col = species)) + scale_color_manual(values = colour_values) + geom_point() + theme_classic()
length_plot + width_plot
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

By visual inspection of the figure it looks like there is a linear relationship between sepal length and petal length for Iris-virginica and Iris-versicolor, and no relationship for Iris-setosa. The same holds for sepal width and petal width, except the relationship looks weaker.

We will now investigate the distributions of these four variables


``` r
sepal_width_histogram <- ggplot(data = iris_data, aes(x = species, y = sepal_width,fill=species)) + geom_boxplot(show.legend = FALSE)
sepal_length_histogram <- ggplot(data = iris_data, aes(x = species, y = sepal_length,fill=species)) + geom_boxplot(show.legend = FALSE)
petal_width_histogram <- ggplot(data = iris_data, aes(x = species, y = petal_width,fill=species)) + geom_boxplot(show.legend = FALSE)
petal_length_histogram <- ggplot(data = iris_data, aes(x = species, y = petal_length,fill=species)) + geom_boxplot(show.legend = FALSE)
(sepal_width_histogram + sepal_length_histogram) / (petal_width_histogram + petal_length_histogram)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

From this figure we conclude that iris-setosa has lower variance in petal width and petal length

We will now generate a pairs plot for all variables


``` r
library(GGally)
ggpairs(iris_data, aes(color=species, alpha = 0.5),columns=1:4, upper = list(continuous = "points"), legend = 1, switch = "both") + guides(alpha = "none")
```

```
## plot: [1, 1] [===>-----------------------------------------------------] 6% est: 0s
## plot: [1, 2] [======>--------------------------------------------------] 12% est: 0s
## plot: [1, 3] [==========>----------------------------------------------] 19% est: 0s
## plot: [1, 4] [=============>-------------------------------------------] 25% est: 0s
## plot: [2, 1] [=================>---------------------------------------] 31% est: 0s
## plot: [2, 2] [====================>------------------------------------] 38% est: 0s
## plot: [2, 3] [========================>--------------------------------] 44% est: 0s
## plot: [2, 4] [===========================>-----------------------------] 50% est: 0s
## plot: [3, 1] [===============================>-------------------------] 56% est: 0s
## plot: [3, 2] [===================================>---------------------] 62% est: 0s
## plot: [3, 3] [======================================>------------------] 69% est: 0s
## plot: [3, 4] [==========================================>--------------] 75% est: 0s
## plot: [4, 1] [=============================================>-----------] 81% est: 0s
## plot: [4, 2] [=================================================>-------] 88% est: 0s
## plot: [4, 3] [====================================================>----] 94% est: 0s
## plot: [4, 4] [=========================================================]100% est: 0s
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

From this plot we can draw the conclusion that sepal length and width has a positive linear relationship for all species, petal length and petal width has a general linear trend that looks similar across all species and that the variance of petal length and width is low for Iris Setosa

# Artportalen

We will begin by loading data from Artportalen


``` r
library(tidyverse)
artportalen <- read.csv("artportalen.csv")

artportalen <- artportalen %>% mutate(Antal = ifelse(is.na(as.numeric(Antal)),1, as.numeric(Antal)))
```

```
## Warning: There were 2 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `Antal = ifelse(is.na(as.numeric(Antal)), 1, as.numeric(Antal))`.
## Caused by warning in `ifelse()`:
## ! NAs introduced by coercion
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
```

``` r
species_counts <- artportalen %>% group_by(Artnamn) %>% summarise(count = sum(Antal))
sorted_species_counts <- species_counts[order(-species_counts$count),]
most_common_species <- sorted_species_counts[1:3,]
rarest_species <- sorted_species_counts[sorted_species_counts$count == 1,]
most_common_species
```

```
## # A tibble: 3 × 2
##   Artnamn   count
##   <chr>     <dbl>
## 1 Grönsiska 20237
## 2 Sothöna    8393
## 3 Gräsand    7296
```

``` r
rarest_species
```

```
## # A tibble: 20 × 2
##    Artnamn                   count
##    <chr>                     <dbl>
##  1 Bivråk                        1
##  2 Bändelkorsnäbb                1
##  3 Ejder                         1
##  4 Fjällvråk                     1
##  5 Forsärla                      1
##  6 Gravand                       1
##  7 Lappsparv                     1
##  8 Ob. korsnäbb                  1
##  9 Ormvråk, underarten buteo     1
## 10 Rosenfink                     1
## 11 Röd glada                     1
## 12 Rödbena                       1
## 13 Spillkråka                    1
## 14 Storlom                       1
## 15 Strömstare                    1
## 16 Tornfalk                      1
## 17 Trädlärka                     1
## 18 Varfågel                      1
## 19 Vinterhämpling                1
## 20 Ägretthäger                   1
```

``` r
monthly_distribution <- artportalen %>% filter(Artnamn %in% most_common_species$Artnamn) %>% mutate(month = format(as.Date(Startdatum), "%m")) %>% group_by(Artnamn, month) %>% summarise(count = sum(Antal))
```

```
## `summarise()` has grouped output by 'Artnamn'. You can override using the `.groups`
## argument.
```

``` r
blames_distr <- monthly_distribution %>% filter(Artnamn == "Grönsiska")
koltrast_distr <- monthly_distribution %>% filter(Artnamn == "Sothöna")
talgoxe_distr <- monthly_distribution %>% filter(Artnamn == "Gräsand")
blames_plot <- ggplot(blames_distr, aes(x = month, y = count)) + geom_bar(stat="identity")
koltrast_plot <- ggplot(koltrast_distr, aes(x = month, y = count)) + geom_bar(stat="identity")
talgoxe_plot <- ggplot(talgoxe_distr, aes(x = month, y = count)) + geom_bar(stat="identity")
plot_grid(blames_plot, koltrast_plot, talgoxe_plot, ncol = 3)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Grönsiska, Sothöna and Gräsand are the three most common species. The rarest species are seen in the table rarest_species.

Now we will come up with three questions of our own. One interesting question would be: What distribution do the three most common species follow?


``` r
sothona <- artportalen[artportalen$Artnamn == "Sothöna",]$Antal
gronsiska <- artportalen[artportalen$Artnamn == "Grönsiska",]$Antal
grasand <- artportalen[artportalen$Artnamn == "Gräsand",]$Antal
hist(sothona, prob = TRUE)
k = 1:200
pdf = dpois(k,mean(sothona))
points(k, pdf, col="red")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

``` r
hist(gronsiska, prob = TRUE)
k = 1:200
pdf = dpois(k,mean(gronsiska))
points(k, pdf, col="red")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png)

``` r
hist(grasand, prob = TRUE)
k = 1:200
pdf = dpois(k,mean(grasand))
points(k, pdf, col="red")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-3.png)

A Poission distribution is a poor fit for Sothöna, Grönsiska, Gräsand, probably since there is a large concentration at n = 1 for observations and a large tail. We will also investigate which species are the most overrepresented in Stockholm


``` r
stockholm_species_counts <- artportalen %>% filter(Kommun == "Stockholm") %>% group_by(Artnamn) %>% summarise(stockholm_count = sum(Antal))
merged_frame <- merge(species_counts, stockholm_species_counts)
merged_frame$stockholm_share <- merged_frame$stockholm_count / merged_frame$count
merged_frame <- merged_frame %>% filter(count != 1)
sort_stockholm_share <- merged_frame[order(-merged_frame$stockholm_share),]
knitr::kable(sort_stockholm_share[1:40,])
```



|    |Artnamn               | count| stockholm_count| stockholm_share|
|:---|:---------------------|-----:|---------------:|---------------:|
|6   |Bläsand               |   108|             108|       1.0000000|
|7   |Bläsgås               |    40|              40|       1.0000000|
|9   |Brun kärrhök          |     3|               3|       1.0000000|
|11  |Buskskvätta           |     7|               7|       1.0000000|
|16  |Enkelbeckasin         |   169|             169|       1.0000000|
|18  |Europeisk skata       |     4|               4|       1.0000000|
|20  |Fiskgjuse             |     2|               2|       1.0000000|
|24  |Gök                   |     7|               7|       1.0000000|
|27  |Grågås x kanadagås    |     4|               4|       1.0000000|
|35  |Grönbena              |    36|              36|       1.0000000|
|46  |Hussvala              |     8|               8|       1.0000000|
|50  |Kärrsångare           |     6|               6|       1.0000000|
|61  |Lärkfalk              |     2|               2|       1.0000000|
|62  |Ljungpipare           |     2|               2|       1.0000000|
|64  |Måsfåglar             |   200|             200|       1.0000000|
|65  |Mellanskarv           |   200|             200|       1.0000000|
|72  |Nordlig gulärla       |     2|               2|       1.0000000|
|73  |Nordsjösilltrut       |     2|               2|       1.0000000|
|76  |Ob. ansergås          |    56|              56|       1.0000000|
|77  |Ob. bo-/bergfink      |    14|              14|       1.0000000|
|78  |Ob. fisk-/silvertärna |    12|              12|       1.0000000|
|79  |Ob. gås               |     2|               2|       1.0000000|
|84  |Prutgås               |    40|              40|       1.0000000|
|92  |Sädgås                |    30|              30|       1.0000000|
|99  |Silvertärna           |     3|               3|       1.0000000|
|111 |Snösparv              |    13|              13|       1.0000000|
|114 |Spetsbergsgås         |     2|               2|       1.0000000|
|119 |Stenskvätta           |    54|              54|       1.0000000|
|127 |Svarthakedopping      |   117|             117|       1.0000000|
|144 |Tundrasädgås          |     3|               3|       1.0000000|
|145 |Vattenrall            |    14|              14|       1.0000000|
|103 |Skedand               |   138|             136|       0.9855072|
|3   |Bergfink              |   489|             481|       0.9836401|
|28  |Gråhäger              |  2328|            2261|       0.9712199|
|19  |Fasan                 |    53|              51|       0.9622642|
|26  |Grågås                |  1768|            1701|       0.9621041|
|136 |Tofsvipa              |   759|             724|       0.9538867|
|33  |Gråsparv              |  1545|            1468|       0.9501618|
|69  |Mindre strandpipare   |    17|              16|       0.9411765|
|39  |Grönsiska             | 20237|           18801|       0.9290409|

There are a lot of species that have only been observed in Stockholm.

Finally we check if Benfords law is a good fit for total counts


``` r
sorted_species_counts$first_digit <- floor(sorted_species_counts$count / 10^(floor(log10(sorted_species_counts$count))))
hist(sorted_species_counts$first_digit)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

We do not obtain a Benford distribution, despite the values having a range spanning several orders of magnitude. Perhaps people hap-hazardly just put in "1000" for "A huge flock"

# Stroke data

We begin by loading stroke data


``` r
stroke_data <- read.csv("stroke-data.csv")
knitr::kable(head(stroke_data))
```



|    id|gender | age| hypertension| heart_disease|ever_married |work_type     |Residence_type | avg_glucose_level|bmi  |smoking_status  | stroke|
|-----:|:------|---:|------------:|-------------:|:------------|:-------------|:--------------|-----------------:|:----|:---------------|------:|
|  9046|Male   |  67|            0|             1|Yes          |Private       |Urban          |            228.69|36.6 |formerly smoked |      1|
| 51676|Female |  61|            0|             0|Yes          |Self-employed |Rural          |            202.21|N/A  |never smoked    |      1|
| 31112|Male   |  80|            0|             1|Yes          |Private       |Rural          |            105.92|32.5 |never smoked    |      1|
| 60182|Female |  49|            0|             0|Yes          |Private       |Urban          |            171.23|34.4 |smokes          |      1|
|  1665|Female |  79|            1|             0|Yes          |Self-employed |Rural          |            174.12|24   |never smoked    |      1|
| 56669|Male   |  81|            0|             0|Yes          |Private       |Urban          |            186.21|29   |formerly smoked |      1|


``` r
nrow(stroke_data)
```

```
## [1] 5110
```

``` r
nrow(stroke_data[stroke_data$gender == "Other",])
```

```
## [1] 1
```

Our dataset is quite large (n = 5110) observations. We have several categorical predictors of our variable of interest, some binary (ever_married) and some with several factors (work_type, gender). Three interesting questions come to mind:

A)  Is there any relationship between marriage status and stroke, and does this relationship differ by gender?

B)  Same question as A but for type of employment

C)  How well can we predict stroke?

We will create contingency tables for A and B, and use logistic regression for C


``` r
cleaned_stroke_data <- stroke_data %>% filter(gender != "Other") # Only one observation, meaningless to speak of

contingency_table_gender <- table(data.frame(cleaned_stroke_data$gender, cleaned_stroke_data$stroke))


married_stroke_data <- cleaned_stroke_data[cleaned_stroke_data$ever_married == "Yes",]
contingency_table_marriage <- table(data.frame(married_stroke_data$gender, married_stroke_data$stroke))
contingency_table_gender
```

```
##                           cleaned_stroke_data.stroke
## cleaned_stroke_data.gender    0    1
##                     Female 2853  141
##                     Male   2007  108
```

``` r
contingency_table_marriage
```

```
##                           married_stroke_data.stroke
## married_stroke_data.gender    0    1
##                     Female 1881  120
##                     Male   1252  100
```

From this we can conclude that having ever been married seems to predict lower stroke risk for women.

We now continue to employment


``` r
model <- glm(stroke ~ gender*as.factor(work_type), data = cleaned_stroke_data, family = "binomial")
summary(model)
```

```
## 
## Call:
## glm(formula = stroke ~ gender * as.factor(work_type), family = "binomial", 
##     data = cleaned_stroke_data)
## 
## Coefficients:
##                                               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                                    -5.0876     0.7093  -7.173 7.35e-13 ***
## genderMale                                    -13.4785   343.2975  -0.039 0.968682    
## as.factor(work_type)Govt_job                    2.2935     0.7411   3.095 0.001970 ** 
## as.factor(work_type)Never_worked              -13.4785  1966.6497  -0.007 0.994532    
## as.factor(work_type)Private                     1.9930     0.7189   2.772 0.005568 ** 
## as.factor(work_type)Self-employed               2.6366     0.7282   3.621 0.000294 ***
## genderMale:as.factor(work_type)Govt_job        13.0617   343.2977   0.038 0.969650    
## genderMale:as.factor(work_type)Never_worked    13.4785  2802.3693   0.005 0.996162    
## genderMale:as.factor(work_type)Private         13.8632   343.2975   0.040 0.967788    
## genderMale:as.factor(work_type)Self-employed   13.4785   343.2976   0.039 0.968682    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1990.3  on 5108  degrees of freedom
## Residual deviance: 1911.1  on 5099  degrees of freedom
## AIC: 1931.1
## 
## Number of Fisher Scoring iterations: 17
```

Who would have thought that Self-employed men suffer dramatically increased stroke risk? Well, anyone who has ever observed one in the wild.

Now lets throw in all of our variables and see how good our predictive validity can get given our dataset


``` r
set.seed(050903)
library(pROC)

stroke_data_cleaned <- stroke_data %>% mutate(bmi = as.numeric(bmi), avg_glucose_level = as.numeric(avg_glucose_level), age = as.numeric(age), gender = as.factor(gender), ever_married = as.factor(ever_married), Residence_type = as.factor(Residence_type), smoking_status = as.factor(smoking_status)) 
```

```
## Warning: There was 1 warning in `mutate()`.
## ℹ In argument: `bmi = as.numeric(bmi)`.
## Caused by warning:
## ! NAs introduced by coercion
```

``` r
final_cleaned_data <- stroke_data_cleaned[complete.cases(stroke_data_cleaned),]

training_indicies <- runif(nrow(final_cleaned_data)) < 0.8
testing_indicies <- !training_indicies

training_data <- final_cleaned_data[training_indicies,]
testing_data <- final_cleaned_data[testing_indicies,]



giga_model <- glm(stroke ~ ., data = training_data, family = "binomial")


summary(giga_model)
```

```
## 
## Call:
## glm(formula = stroke ~ ., family = "binomial", data = training_data)
## 
## Coefficients:
##                              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                -7.062e+00  1.096e+00  -6.446 1.15e-10 ***
## id                          1.520e-06  3.908e-06   0.389  0.69732    
## genderMale                  1.597e-01  1.721e-01   0.928  0.35347    
## genderOther                -1.118e+01  2.400e+03  -0.005  0.99628    
## age                         7.774e-02  7.290e-03  10.663  < 2e-16 ***
## hypertension                5.656e-01  1.958e-01   2.888  0.00388 ** 
## heart_disease               3.100e-01  2.338e-01   1.326  0.18474    
## ever_marriedYes            -2.047e-01  2.699e-01  -0.758  0.44831    
## work_typeGovt_job          -9.015e-01  1.144e+00  -0.788  0.43083    
## work_typeNever_worked      -1.113e+01  5.773e+02  -0.019  0.98461    
## work_typePrivate           -9.631e-01  1.133e+00  -0.850  0.39524    
## work_typeSelf-employed     -1.404e+00  1.157e+00  -1.214  0.22489    
## Residence_typeUrban         4.625e-02  1.683e-01   0.275  0.78341    
## avg_glucose_level           3.666e-03  1.468e-03   2.497  0.01253 *  
## bmi                         2.442e-04  1.374e-02   0.018  0.98582    
## smoking_statusnever smoked -8.771e-02  2.086e-01  -0.420  0.67421    
## smoking_statussmokes        2.587e-01  2.559e-01   1.011  0.31216    
## smoking_statusUnknown      -4.159e-01  2.833e-01  -1.468  0.14217    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1384.2  on 3958  degrees of freedom
## Residual deviance: 1082.2  on 3941  degrees of freedom
## AIC: 1118.2
## 
## Number of Fisher Scoring iterations: 15
```

``` r
predictions <- predict(giga_model, testing_data, type = "response")

ggroc(roc(testing_data$stroke, predictions))
```

```
## Setting levels: control = 0, case = 1
```

```
## Setting direction: controls < cases
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

Our ROC curve looks good but not great. Also, since we have major class imbalance (minority class being strokes) we probably have an even worse precision-recall metric :/

# Cleaning data

A lot of countries have NA between 1965 and 1970 as well as between 1970 and 1975, where they have zero cell phones. It seems quite reasonable that if the amount of cell phones was 0 in 1970 and 0 in 1975 it was probably 0 in the years in between. So this is the first thing we handle


``` r
cell_phone_data <- read.csv("cell_phones_total.csv")

na_to_zero_pre_1975 <- function(row) {
  fixed_row = row
  for(i in 2:13) {
    if(is.na(row[i])) {
      fixed_row[i] = 0
    }
  }
  
 return(fixed_row) 
}

pre_1975_na_fixed <- cell_phone_data

for(i in 1:nrow(cell_phone_data)) {
  pre_1975_na_fixed[i,] <- na_to_zero_pre_1975(pre_1975_na_fixed[i,])
}
```

Now we need to fix the "k, M, B" problem and add "missing" when an entry is missing (instead of null). We then forward fill with the last valid value in case a value is missing 


``` r
fix_kmb_problem <- function(row) {
  fixed_row = row
  last_valid_value <- NA
  
  for(i in 2:length(row)) {
    if(grepl("k",row[i])) {
      fixed_row[i] <- as.numeric(gsub("k","",row[i]))*1000
      last_valid_value <- fixed_row[i]
    }
    else if(grepl("M",row[i])) {
      fixed_row[i] <- as.numeric(gsub("M","",row[i]))*1000*1000
      last_valid_value <- fixed_row[i]
    }
    else if(grepl("B",row[i])) {
      fixed_row[i] <- as.numeric(gsub("B","",row[i]))*1000*1000*1000
      last_valid_value <- fixed_row[i]
    }
    else if(is.na(row[i]) || row[i] == "") {
      fixed_row[i] = last_valid_value
    }
    else {
      last_valid_value <- fixed_row[i]
    }
  }
  return(fixed_row)
}

kmb_fixed <- pre_1975_na_fixed
for(i in 1:nrow(cell_phone_data)) {
  kmb_fixed[i,] <- fix_kmb_problem(pre_1975_na_fixed[i,])
}

table_to_present <- kmb_fixed %>% 
  select(iso.3, X2015, X2016, X2017, X2018, X2019) %>% 
  filter(iso.3 %in% c("CHN","IND","USA","IDN","BRA"))

knitr::kable(table_to_present)
```



|iso.3 |X2015    |X2016    |X2017    |X2018    |X2019    |
|:-----|:--------|:--------|:--------|:--------|:--------|
|BRA   |2.58e+08 |2.44e+08 |2.18e+08 |2.07e+08 |2.07e+08 |
|CHN   |1.29e+09 |1.36e+09 |1.47e+09 |1.65e+09 |1.73e+09 |
|IDN   |3.39e+08 |3.86e+08 |4.35e+08 |3.19e+08 |3.45e+08 |
|IND   |1e+09    |1.13e+09 |1.17e+09 |1.18e+09 |1.15e+09 |
|USA   |3.82e+08 |3.96e+08 |4e+08    |4.22e+08 |4.22e+08 |

Now we have cleaned the data
