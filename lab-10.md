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
summary(m_bty)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

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

## Hint

For Exercise 12, the `relevel()` function can be helpful!
