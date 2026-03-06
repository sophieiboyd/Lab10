Lab 10 - Grading the professor
================
Sophie Boyd
3-6-26

Here is a link to the [lab
instructions](https://datascience4psych.github.io/DataScience4Psych/lab10.html).

## Load Packages and Data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
```

# Part 1: Getting to know the outcome

## Exercise 1

``` r
evals %>%
  ggplot(aes(x = score)) +
  geom_histogram(binwidth = .1) +
  labs(x = 'Score',
       y = 'Count',
       title = 'Distribution of Evaluation Scores')
```

![](lab-10_files/figure-gfm/exercise1-plot-1.png)<!-- -->

``` r
mean(evals$score)
```

    ## [1] 4.17473

``` r
median(evals$score)
```

    ## [1] 4.3

``` r
min(evals$score)
```

    ## [1] 2.3

``` r
max(evals$score)
```

    ## [1] 5

Most students provided high evaluation ratings for their professors. The
distribution of scores is skewed left, with most scores on the high end
and a few scores on the low end.

## Exercise 2

``` r
evals %>%
  ggplot(aes(x = bty_avg, y = score)) +
  geom_point() + 
  labs(x = 'Beauty Rating',
       y = 'Score',
       title = 'Evaluation Scores by Beauty Rating')
```

![](lab-10_files/figure-gfm/exercise2-scatter-1.png)<!-- -->

I don’t see an especially clear trend in the relationship between beauty
ratings and evaluation ratings on the scatterplot. There appears to be
some organization into columns, where groups of professors with the same
beauty ratings have a range of evaluation scores.

## Exercise 3

``` r
evals %>%
  ggplot(aes(x = bty_avg, y = score)) +
  geom_jitter() + 
  labs(x = 'Beauty Rating',
       y = 'Score',
       title = 'Evaluation Scores by Beauty Rating')
```

![](lab-10_files/figure-gfm/exercise3-jitter-1.png)<!-- -->

Jittering separates points that overlap each other on the scatterplot,
providing more information about the density of the points. By
jittering, we can see that scores are more concentrated at high values
than low values across different beauty ratings, and especially among
higher beauty ratings. (A positive association between beauty ratings
and evaluation scores becomes clearer.) If someone were to only see the
non-jittered plot, they might mistakenly assume that evaluation scores
were evenly distributed among high and low values within groups of
professors with the same beauty rating, when in fact higher evaluation
ratings tended to be more common.

# Part 2: Beauty as a predictor

## Exercise 1

``` r
m_bty <- lm(score ~ bty_avg, data = evals)
tidy(m_bty)
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   3.88      0.0761     51.0  1.56e-191
    ## 2 bty_avg       0.0666    0.0163      4.09 5.08e-  5

For each 1-point increase in beauty rating, the model predicts a
.067-point increase in evaluation score.

## Exercise 2

``` r
evals %>%
  ggplot(aes(x = bty_avg, y = score)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = 'Beauty Rating',
       y = 'Evaluation Score')
```

![](lab-10_files/figure-gfm/m-bty-plot-1.png)<!-- -->

## Exercise 3

- The slope is relatively small, indicating only a .067-point increase
  in evaluation score for each 1-point increase in beauty rating.
- The predicted evaluation score for a professor with a beauty rating of
  0 is 3.88. In context, I don’t think this value is particularly
  meaningful. I believe that beauty ratings ranged from 1-10, in which
  case a rating of 0 would not be possible in our dataset.
- The R-square value is .035, meaning that beauty ratings explain 3.5%
  of the variance in evaluation scores. Overall, it seems that a
  professor’s attractiveness is not especially influential in student
  evaluations.
- At this stage, leaving the shading on the plot may lead someone to
  overstate how well the model fits the data. By looking at the line
  only, it is easier to where the model does not match up with the
  actual data.

# Part 3: Linear regression with a categorical predictor

\##Exercise 1

``` r
m_gen <- lm(score ~ gender, data = evals)
tidy(m_gen)
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)    4.09     0.0387    106.   0      
    ## 2 gendermale     0.142    0.0508      2.78 0.00558

The reference level for gender is female. The model predicts that, on
average, male professors’ evaluation ratings will be .14 points higher
than female professors’ evaluation ratings.

Predicted evaluation score for female professor = 4.09 (intercept)
Predicted evaluation score for male professor = 4.23 (intercept + slope)

## Exercise 2

``` r
evals <- evals %>%
  mutate(rank_relevel = relevel(rank, ref = "tenure track"))

evals <- evals %>%
  mutate(tenure_eligible = case_when(
    rank == "teaching" ~ "no",
    rank == "tenure track" ~ "yes",
    rank == "tenured" ~ "yes"
  ))
```

``` r
m_rank <- lm(score ~ rank, data = evals)
tidy(m_rank)
```

    ## # A tibble: 3 × 5
    ##   term             estimate std.error statistic   p.value
    ##   <chr>               <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         4.28     0.0537     79.9  1.02e-271
    ## 2 ranktenure track   -0.130    0.0748     -1.73 8.37e-  2
    ## 3 ranktenured        -0.145    0.0636     -2.28 2.28e-  2

``` r
glance(m_rank)$r.squared
```

    ## [1] 0.01162894

``` r
m_rank_relevel <- lm(score ~ rank_relevel, data = evals)
tidy(m_rank_relevel)
```

    ## # A tibble: 3 × 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            4.15      0.0521    79.7   2.58e-271
    ## 2 rank_relevelteaching   0.130     0.0748     1.73  8.37e-  2
    ## 3 rank_releveltenured   -0.0155    0.0623    -0.249 8.04e-  1

``` r
glance(m_rank_relevel)$r.squared
```

    ## [1] 0.01162894

``` r
m_tenure <- lm(score ~ tenure_eligible, data = evals)
tidy(m_tenure)
```

    ## # A tibble: 2 × 5
    ##   term               estimate std.error statistic   p.value
    ##   <chr>                 <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           4.28     0.0536     79.9  2.72e-272
    ## 2 tenure_eligibleyes   -0.141    0.0607     -2.32 2.10e-  2

``` r
glance(m_tenure)$r.squared
```

    ## [1] 0.01149589

## Exercise 3

##### Model using original “rank” variable:

- Intercept: The average predicted evaluation score for teaching
  professors is 4.28.
- Slopes: On average, tenure track professors are expected to have
  evaluation scores .13 points lower than teaching professors. Tenured
  professors are expected to have evaluation scores .15 points lower
  than teaching professors.

##### Model using “rank_relevel”:

- Intercept: The average predicted evaluation score for tenure track
  professors is 4.15.
- On average, teaching professors are expected to have evaluation scores
  .13 points higher than tenure track professors. Tenured professors are
  expected to have evaluation scores .02 points lower than tenure track
  professors.

##### Model using “tenure_eligible”:

- Intercept: The average predicted evaluation score for teaching
  professors is 4.28.
- Slope: Compared to teaching professors, tenure eligible professors are
  predicted to have evaluation scores that are, on average, .14 points
  lower.

Taken together, the three models communicate that teaching professors
perform best on student evaluations.

## Exercise 4

The R-square value is .012, meaning that rank only explains about 1.2%
of the variance in evaluation scores. Overall, rank seems to play a
small role.

# Part 4: Multiple linear regression

``` r
m_bty <- lm(score ~ bty_avg, data = evals)
tidy(m_bty)
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   3.88      0.0761     51.0  1.56e-191
    ## 2 bty_avg       0.0666    0.0163      4.09 5.08e-  5

``` r
glance(m_bty)$adj.r.squared
```

    ## [1] 0.03292903

``` r
m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
tidy(m_bty_gen)
```

    ## # A tibble: 3 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   3.75      0.0847     44.3  6.23e-168
    ## 2 bty_avg       0.0742    0.0163      4.56 6.48e-  6
    ## 3 gendermale    0.172     0.0502      3.43 6.52e-  4

``` r
glance(m_bty_gen)$adj.r.squared
```

    ## [1] 0.05503202

## Exercise 1

The coefficient for beauty increased slightly and the associated p-value
decreased after adding gender as a predictor.

## Exercise 2

Yes, gender is an individual significant predictor of evaluation score.

## Exercise 3

I would say that adding gender as a predictor was somewhat helpful. The
model with beauty alone explained 3.3% of the variance in evaluation
scores, whereas the model that added gender explained 5.5% of the
variance in evaluation scores.

## Exercise 4

``` r
m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)
tidy(m_bty_rank)
```

    ## # A tibble: 4 × 5
    ##   term             estimate std.error statistic   p.value
    ##   <chr>               <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)        3.98      0.0908     43.9  2.92e-166
    ## 2 bty_avg            0.0678    0.0165      4.10 4.92e-  5
    ## 3 ranktenure track  -0.161     0.0740     -2.17 3.03e-  2
    ## 4 ranktenured       -0.126     0.0627     -2.01 4.45e-  2

- Holding rank constant, a one-unit increase in beauty rating predicts a
  .07-point increase in evaluation score.

- Holding beauty rating constant, tenure-track professors are predicted
  to have evaluation scores .016 points lower than teaching professors.

# Part 5: The search for the best model

## Exercise 1

I would expect the number of professors teaching sections of a course to
be the weakest predictor of evaluation scores because students are
typically only aware of/focused on the professor who taught their
section when completing evaluations.

## Exercise 2

``` r
m_cls_profs <- lm(score ~ cls_profs, data = evals)
tidy(m_cls_profs)
```

    ## # A tibble: 2 × 5
    ##   term            estimate std.error statistic p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)       4.18      0.0311   134.      0    
    ## 2 cls_profssingle  -0.0292    0.0534    -0.547   0.585

``` r
glance(m_cls_profs)$adj.r.squared
```

    ## [1] -0.0015192

## Exercise 3

If the model already included cls_perc_eval and cls_students, it would
be redundant to include cls_did_eval, as the number of students who
completed the evaluation does not communicate any new information beyond
the total number of students in the class and the percentage of students
who completed the evaluation. It would not explain additional variance
in evaluation scores.

## Exercise 4

``` r
m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
tidy(m_full)
```

    ## # A tibble: 13 × 5
    ##    term                   estimate std.error statistic  p.value
    ##    <chr>                     <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)            3.53      0.241       14.7   4.65e-40
    ##  2 ranktenure track      -0.107     0.0820      -1.30  1.93e- 1
    ##  3 ranktenured           -0.0450    0.0652      -0.691 4.90e- 1
    ##  4 ethnicitynot minority  0.187     0.0775       2.41  1.63e- 2
    ##  5 gendermale             0.179     0.0515       3.47  5.79e- 4
    ##  6 languagenon-english   -0.127     0.108       -1.17  2.41e- 1
    ##  7 age                   -0.00665   0.00308     -2.16  3.15e- 2
    ##  8 cls_perc_eval          0.00570   0.00155      3.67  2.68e- 4
    ##  9 cls_students           0.000445  0.000358     1.24  2.15e- 1
    ## 10 cls_levelupper         0.0187    0.0556       0.337 7.37e- 1
    ## 11 cls_profssingle       -0.00858   0.0514      -0.167 8.67e- 1
    ## 12 cls_creditsone credit  0.509     0.117        4.35  1.70e- 5
    ## 13 bty_avg                0.0613    0.0167       3.67  2.68e- 4

``` r
glance(m_full)$adj.r.squared
```

    ## [1] 0.1412172

## Exercise 5

``` r
m_best <- lm(score ~ ethnicity + gender + language + age + cls_perc_eval + cls_credits + bty_avg, data = evals)
tidy(m_best)
```

    ## # A tibble: 8 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)            3.45      0.203       17.0  2.26e-50
    ## 2 ethnicitynot minority  0.205     0.0747       2.74 6.38e- 3
    ## 3 gendermale             0.185     0.0499       3.70 2.38e- 4
    ## 4 languagenon-english   -0.161     0.103       -1.56 1.18e- 1
    ## 5 age                   -0.00501   0.00261     -1.92 5.53e- 2
    ## 6 cls_perc_eval          0.00509   0.00144      3.54 4.36e- 4
    ## 7 cls_creditsone credit  0.515     0.105        4.91 1.26e- 6
    ## 8 bty_avg                0.0650    0.0163       3.98 7.99e- 5

``` r
glance(m_best)$adj.r.squared
```

    ## [1] 0.1445918

Predicted eval score = 3.45 + .20(ethnicity) + .18(gender) -
.16(language) - .01(age) + .01(cls_perc_eval) + .52(cls_credits) +
.06(bty_avg)

## Exercise 6

- beauty rating (numerical): Holding other predictors constant, each
  one-unit increase in beauty rating predicts a .06-point increase in
  evaluation score.

- class credits (categorical): Holding other predictors constant,
  professors of one-credit classes are predicted to receive evaluation
  scores .52 points higher than professors of multicredit classes.

## Exercise 7

A professor at UT Austin with a high evaluation score would be a young,
white, male, English-speaking professor teaching a one-credit class with
a high percentage of students who completed the course evaluation and a
high beauty rating.

## Exercise 8

I would not be entirely comfortable with generalizing these results to
other universities, the makeup of the student body varies widely across
different universities, which would likely influence how students
evaluate their professors. For example, given that UT Austin is a large
state university, the results might not apply to a smaller liberal arts
school.
