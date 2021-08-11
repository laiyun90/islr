Chapter 3: Linear Regression
================
2021-08-10

# 3.1 Simple Linear Regression

## 3.1.1 Estimating the Coefficients

To re-create Figure 3.1, import the `Advertising` data set:

``` r
advertising <- read_csv(here("data", "Advertising.csv"))
glimpse(advertising)
```

    ## Rows: 200
    ## Columns: 5
    ## $ X1        <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 1~
    ## $ TV        <dbl> 230.1, 44.5, 17.2, 151.5, 180.8, 8.7, 57.5, 120.2, 8.6, 199.~
    ## $ radio     <dbl> 37.8, 39.3, 45.9, 41.3, 10.8, 48.9, 32.8, 19.6, 2.1, 2.6, 5.~
    ## $ newspaper <dbl> 69.2, 45.1, 69.3, 58.5, 58.4, 75.0, 23.5, 11.6, 1.0, 21.2, 2~
    ## $ sales     <dbl> 22.1, 10.4, 9.3, 18.5, 12.9, 7.2, 11.8, 13.2, 4.8, 10.6, 8.6~

Fit the simple linear model and draw the residuals to the line of best
fit:

``` r
lm_sales_tv <- lm(sales ~ TV, data = advertising)
advertising %>%
  bind_cols(
    pred_sales = predict(lm_sales_tv, data = advertising)
  ) %>%
  ggplot(aes(x = TV)) +
  geom_point(aes(y = sales), color = "red") +
  geom_smooth(aes(y = sales), method = "lm", formula = "y ~ x", se = FALSE) +
  geom_linerange(aes(ymin = sales, ymax = pred_sales))
```

![](chapter03_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
summary(lm_sales_tv)
```

    ## 
    ## Call:
    ## lm(formula = sales ~ TV, data = advertising)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.3860 -1.9545 -0.1913  2.0671  7.2124 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 7.032594   0.457843   15.36   <2e-16 ***
    ## TV          0.047537   0.002691   17.67   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.259 on 198 degrees of freedom
    ## Multiple R-squared:  0.6119, Adjusted R-squared:  0.6099 
    ## F-statistic: 312.1 on 1 and 198 DF,  p-value: < 2.2e-16

We recover the same regression coefficients: *β*<sub>0</sub> = 7.03 and
*β*<sub>1</sub> = 0.0475.

## 3.1.2 Assessing the Accuracy of the Coefficient Estimates

The quickest way to get the 95% confidence intervals for the
coefficients is with `stats::confint()`:

``` r
confint(lm_sales_tv)
```

    ##                  2.5 %     97.5 %
    ## (Intercept) 6.12971927 7.93546783
    ## TV          0.04223072 0.05284256

Computing them manually requires the standard errors of the
coefficients. For this, I prefer `broom::tidy`:

``` r
library(broom)
tidy(lm_sales_tv)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)   7.03     0.458        15.4 1.41e-35
    ## 2 TV            0.0475   0.00269      17.7 1.47e-42

Then double the SEs to approximate the intervals:

``` r
tidy(lm_sales_tv) %>%
  transmute(
    term, estimate,
    ci_lower = estimate - 2*std.error, ci_upper = estimate + 2*std.error
  )
```

    ## # A tibble: 2 x 4
    ##   term        estimate ci_lower ci_upper
    ##   <chr>          <dbl>    <dbl>    <dbl>
    ## 1 (Intercept)   7.03     6.12     7.95  
    ## 2 TV            0.0475   0.0422   0.0529

The *t*-statistics are returned by `broom::tidy` as the `statistic`
variable. We can manually compute them with the estimates and SEs, and
compute the *p*-value based on a *t*-distribution:

``` r
tidy(lm_sales_tv) %>%
  transmute(
    term, estimate, se = std.error,
    t = estimate / se,
    p_value = 2 * pt(-t, df = nrow(advertising) - 2)
  )
```

    ## # A tibble: 2 x 5
    ##   term        estimate      se     t  p_value
    ##   <chr>          <dbl>   <dbl> <dbl>    <dbl>
    ## 1 (Intercept)   7.03   0.458    15.4 1.41e-35
    ## 2 TV            0.0475 0.00269  17.7 1.47e-42

## 3.1.3 Assessing the Accuracy of the Model

The `broom::glance` function gives summary statistics of a model:

``` r
glance(lm_sales_tv)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.612         0.610  3.26      312. 1.47e-42     1  -519. 1044. 1054.
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

The residual standard error (RSE) is `sigma`, variance explained
*R*<sup>2</sup> is `r.squared`, and the *F*-statistic is `statistic`.
With this, we can re-create Table 3.2:

``` r
glance(lm_sales_tv) %>%
  transmute(`Residual standard error` = round(sigma, 2),
            `R2` = round(r.squared, 3), `F-statistic` = round(statistic, 1)) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(everything(), names_to = "Quantity", values_to = "Value") %>%
  gt()
```

<div id="pnwdxjwqjl" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#pnwdxjwqjl .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#pnwdxjwqjl .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#pnwdxjwqjl .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#pnwdxjwqjl .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#pnwdxjwqjl .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pnwdxjwqjl .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#pnwdxjwqjl .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#pnwdxjwqjl .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#pnwdxjwqjl .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#pnwdxjwqjl .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#pnwdxjwqjl .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#pnwdxjwqjl .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#pnwdxjwqjl .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#pnwdxjwqjl .gt_from_md > :first-child {
  margin-top: 0;
}

#pnwdxjwqjl .gt_from_md > :last-child {
  margin-bottom: 0;
}

#pnwdxjwqjl .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#pnwdxjwqjl .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#pnwdxjwqjl .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pnwdxjwqjl .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#pnwdxjwqjl .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pnwdxjwqjl .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#pnwdxjwqjl .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#pnwdxjwqjl .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pnwdxjwqjl .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#pnwdxjwqjl .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#pnwdxjwqjl .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#pnwdxjwqjl .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#pnwdxjwqjl .gt_left {
  text-align: left;
}

#pnwdxjwqjl .gt_center {
  text-align: center;
}

#pnwdxjwqjl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#pnwdxjwqjl .gt_font_normal {
  font-weight: normal;
}

#pnwdxjwqjl .gt_font_bold {
  font-weight: bold;
}

#pnwdxjwqjl .gt_font_italic {
  font-style: italic;
}

#pnwdxjwqjl .gt_super {
  font-size: 65%;
}

#pnwdxjwqjl .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Quantity</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Residual standard error</td>
<td class="gt_row gt_left">3.26</td></tr>
    <tr><td class="gt_row gt_left">R2</td>
<td class="gt_row gt_left">0.612</td></tr>
    <tr><td class="gt_row gt_left">F-statistic</td>
<td class="gt_row gt_left">312.1</td></tr>
  </tbody>
  
  
</table>
</div>

# 3.2 Multiple Linear Regression

Before performing the multiple linear regression, fit the other two
simple linear regression models on radio and newspaper budgets:

``` r
lm_sales_radio <- lm(sales ~ radio, data = advertising)
lm_sales_newspaper <- lm(sales ~ newspaper, data = advertising)
```

The `gtsummary` package provides functions to quickly create regression
summary tables, similar to Table 3.4:

``` r
library(gtsummary)
tbl_regression(lm_sales_radio, intercept = TRUE)
```

<div id="dxlqzyeakj" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#dxlqzyeakj .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#dxlqzyeakj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dxlqzyeakj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#dxlqzyeakj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#dxlqzyeakj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dxlqzyeakj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dxlqzyeakj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#dxlqzyeakj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#dxlqzyeakj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#dxlqzyeakj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#dxlqzyeakj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#dxlqzyeakj .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#dxlqzyeakj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#dxlqzyeakj .gt_from_md > :first-child {
  margin-top: 0;
}

#dxlqzyeakj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#dxlqzyeakj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#dxlqzyeakj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#dxlqzyeakj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dxlqzyeakj .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#dxlqzyeakj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dxlqzyeakj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#dxlqzyeakj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#dxlqzyeakj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dxlqzyeakj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dxlqzyeakj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#dxlqzyeakj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dxlqzyeakj .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#dxlqzyeakj .gt_left {
  text-align: left;
}

#dxlqzyeakj .gt_center {
  text-align: center;
}

#dxlqzyeakj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#dxlqzyeakj .gt_font_normal {
  font-weight: normal;
}

#dxlqzyeakj .gt_font_bold {
  font-weight: bold;
}

#dxlqzyeakj .gt_font_italic {
  font-style: italic;
}

#dxlqzyeakj .gt_super {
  font-size: 65%;
}

#dxlqzyeakj .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">(Intercept)</td>
<td class="gt_row gt_center">9.3</td>
<td class="gt_row gt_center">8.2, 10</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">radio</td>
<td class="gt_row gt_center">0.20</td>
<td class="gt_row gt_center">0.16, 0.24</td>
<td class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="4">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          CI = Confidence Interval
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>

``` r
tbl_regression(lm_sales_newspaper, intercept = TRUE)
```

<div id="cdoeukpaxu" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#cdoeukpaxu .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#cdoeukpaxu .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cdoeukpaxu .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#cdoeukpaxu .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#cdoeukpaxu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cdoeukpaxu .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cdoeukpaxu .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#cdoeukpaxu .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#cdoeukpaxu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#cdoeukpaxu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#cdoeukpaxu .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#cdoeukpaxu .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#cdoeukpaxu .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#cdoeukpaxu .gt_from_md > :first-child {
  margin-top: 0;
}

#cdoeukpaxu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#cdoeukpaxu .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#cdoeukpaxu .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#cdoeukpaxu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cdoeukpaxu .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#cdoeukpaxu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cdoeukpaxu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#cdoeukpaxu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#cdoeukpaxu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cdoeukpaxu .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cdoeukpaxu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#cdoeukpaxu .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cdoeukpaxu .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#cdoeukpaxu .gt_left {
  text-align: left;
}

#cdoeukpaxu .gt_center {
  text-align: center;
}

#cdoeukpaxu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#cdoeukpaxu .gt_font_normal {
  font-weight: normal;
}

#cdoeukpaxu .gt_font_bold {
  font-weight: bold;
}

#cdoeukpaxu .gt_font_italic {
  font-style: italic;
}

#cdoeukpaxu .gt_super {
  font-size: 65%;
}

#cdoeukpaxu .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">(Intercept)</td>
<td class="gt_row gt_center">12</td>
<td class="gt_row gt_center">11, 14</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">newspaper</td>
<td class="gt_row gt_center">0.05</td>
<td class="gt_row gt_center">0.02, 0.09</td>
<td class="gt_row gt_center">0.001</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="4">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          CI = Confidence Interval
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>

Now fit the multiple regression model:

``` r
lm_sales_mult <- lm(sales ~ TV + radio + newspaper, data = advertising)
tbl_regression(lm_sales_mult, intercept = TRUE)
```

<div id="knoffofgld" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#knoffofgld .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#knoffofgld .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#knoffofgld .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#knoffofgld .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#knoffofgld .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#knoffofgld .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#knoffofgld .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#knoffofgld .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#knoffofgld .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#knoffofgld .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#knoffofgld .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#knoffofgld .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#knoffofgld .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#knoffofgld .gt_from_md > :first-child {
  margin-top: 0;
}

#knoffofgld .gt_from_md > :last-child {
  margin-bottom: 0;
}

#knoffofgld .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#knoffofgld .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#knoffofgld .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#knoffofgld .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#knoffofgld .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#knoffofgld .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#knoffofgld .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#knoffofgld .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#knoffofgld .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#knoffofgld .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#knoffofgld .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#knoffofgld .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#knoffofgld .gt_left {
  text-align: left;
}

#knoffofgld .gt_center {
  text-align: center;
}

#knoffofgld .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#knoffofgld .gt_font_normal {
  font-weight: normal;
}

#knoffofgld .gt_font_bold {
  font-weight: bold;
}

#knoffofgld .gt_font_italic {
  font-style: italic;
}

#knoffofgld .gt_super {
  font-size: 65%;
}

#knoffofgld .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">(Intercept)</td>
<td class="gt_row gt_center">2.9</td>
<td class="gt_row gt_center">2.3, 3.6</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">TV</td>
<td class="gt_row gt_center">0.05</td>
<td class="gt_row gt_center">0.04, 0.05</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">radio</td>
<td class="gt_row gt_center">0.19</td>
<td class="gt_row gt_center">0.17, 0.21</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">newspaper</td>
<td class="gt_row gt_center">0.00</td>
<td class="gt_row gt_center">-0.01, 0.01</td>
<td class="gt_row gt_center">0.9</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="4">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          CI = Confidence Interval
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>

The lack of association between sales and newspaper advertising can be
explained by co-linearity with the other predictors, as shown in Table
3.5:

``` r
advertising %>%
  pivot_longer(cols = -X1) %>%
  full_join(., ., by = "X1") %>%
  group_by(name.x, name.y) %>%
  summarise(
    corr_coef = cor(value.x, value.y) %>%
      round(4) %>%
      as.character(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = name.y, values_from = corr_coef) %>%
  gt(rowname_col = "name.x")
```

<div id="ttwflsvjdw" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ttwflsvjdw .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ttwflsvjdw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ttwflsvjdw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ttwflsvjdw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ttwflsvjdw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ttwflsvjdw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ttwflsvjdw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ttwflsvjdw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ttwflsvjdw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ttwflsvjdw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ttwflsvjdw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ttwflsvjdw .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ttwflsvjdw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ttwflsvjdw .gt_from_md > :first-child {
  margin-top: 0;
}

#ttwflsvjdw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ttwflsvjdw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ttwflsvjdw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ttwflsvjdw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ttwflsvjdw .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ttwflsvjdw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ttwflsvjdw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ttwflsvjdw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ttwflsvjdw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ttwflsvjdw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ttwflsvjdw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ttwflsvjdw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ttwflsvjdw .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ttwflsvjdw .gt_left {
  text-align: left;
}

#ttwflsvjdw .gt_center {
  text-align: center;
}

#ttwflsvjdw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ttwflsvjdw .gt_font_normal {
  font-weight: normal;
}

#ttwflsvjdw .gt_font_bold {
  font-weight: bold;
}

#ttwflsvjdw .gt_font_italic {
  font-style: italic;
}

#ttwflsvjdw .gt_super {
  font-size: 65%;
}

#ttwflsvjdw .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">newspaper</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">radio</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">sales</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">TV</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left gt_stub">newspaper</td>
<td class="gt_row gt_left">1</td>
<td class="gt_row gt_left">0.3541</td>
<td class="gt_row gt_left">0.2283</td>
<td class="gt_row gt_left">0.0566</td></tr>
    <tr><td class="gt_row gt_left gt_stub">radio</td>
<td class="gt_row gt_left">0.3541</td>
<td class="gt_row gt_left">1</td>
<td class="gt_row gt_left">0.5762</td>
<td class="gt_row gt_left">0.0548</td></tr>
    <tr><td class="gt_row gt_left gt_stub">sales</td>
<td class="gt_row gt_left">0.2283</td>
<td class="gt_row gt_left">0.5762</td>
<td class="gt_row gt_left">1</td>
<td class="gt_row gt_left">0.7822</td></tr>
    <tr><td class="gt_row gt_left gt_stub">TV</td>
<td class="gt_row gt_left">0.0566</td>
<td class="gt_row gt_left">0.0548</td>
<td class="gt_row gt_left">0.7822</td>
<td class="gt_row gt_left">1</td></tr>
  </tbody>
  
  
</table>
</div>

Consider the hypothesis test:

$$
\\begin{align}
H\_0:& \\beta\_1 = \\beta\_2 = \\dots = \\beta\_p = 0 \\\\
H\_a:& \\text{at least one of } \\beta\_j \\text{ is non-zero.}
\\end{align}
$$
This is performed by computing the *F*-statistic as in equation (3.23).
Instead of computing manually, we again use `broom::glance` to re-create
Table 3.6:

``` r
glance(lm_sales_mult) %>%
  transmute(`Residual standard error` = round(sigma, 2),
            `R2` = round(r.squared, 3), `F-statistic` = round(statistic, 1)) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(everything(), names_to = "Quantity", values_to = "Value") %>%
  gt()
```

<div id="jmsegsbkzl" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#jmsegsbkzl .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#jmsegsbkzl .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jmsegsbkzl .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#jmsegsbkzl .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#jmsegsbkzl .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jmsegsbkzl .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jmsegsbkzl .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#jmsegsbkzl .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#jmsegsbkzl .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jmsegsbkzl .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jmsegsbkzl .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#jmsegsbkzl .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#jmsegsbkzl .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#jmsegsbkzl .gt_from_md > :first-child {
  margin-top: 0;
}

#jmsegsbkzl .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jmsegsbkzl .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#jmsegsbkzl .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#jmsegsbkzl .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jmsegsbkzl .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#jmsegsbkzl .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jmsegsbkzl .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jmsegsbkzl .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jmsegsbkzl .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jmsegsbkzl .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jmsegsbkzl .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#jmsegsbkzl .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jmsegsbkzl .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#jmsegsbkzl .gt_left {
  text-align: left;
}

#jmsegsbkzl .gt_center {
  text-align: center;
}

#jmsegsbkzl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jmsegsbkzl .gt_font_normal {
  font-weight: normal;
}

#jmsegsbkzl .gt_font_bold {
  font-weight: bold;
}

#jmsegsbkzl .gt_font_italic {
  font-style: italic;
}

#jmsegsbkzl .gt_super {
  font-size: 65%;
}

#jmsegsbkzl .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Quantity</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Residual standard error</td>
<td class="gt_row gt_left">1.69</td></tr>
    <tr><td class="gt_row gt_left">R2</td>
<td class="gt_row gt_left">0.897</td></tr>
    <tr><td class="gt_row gt_left">F-statistic</td>
<td class="gt_row gt_left">570.3</td></tr>
  </tbody>
  
  
</table>
</div>

The value of 570 is far larger than 1, which is compelling evidence
against the null *H*<sub>0</sub>. The *F*-statistic follows the
*F*-distribution, so we can get a *p*-value using the values of *n* and
*p*. Or automatically with `glance`:

``` r
glance(lm_sales_mult) %>%
  select(statistic, p.value)
```

    ## # A tibble: 1 x 2
    ##   statistic  p.value
    ##       <dbl>    <dbl>
    ## 1      570. 1.58e-96

Another way to do this is to explicitly fit the null model (no
predictors), and perform an analysis of variance with the two models
using `anova`:

``` r
lm_sales_null <- lm(sales ~ 1, data = advertising)
anova(lm_sales_null, lm_sales_mult)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: sales ~ 1
    ## Model 2: sales ~ TV + radio + newspaper
    ##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
    ## 1    199 5417.1                                  
    ## 2    196  556.8  3    4860.3 570.27 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

This also makes it easy to compare models with different subsets of
variables, as in equation (3.24). For example, the model with and
without `newspaper`:

``` r
anova(
  lm(sales ~ TV + radio, data = advertising),
  lm_sales_mult
)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: sales ~ TV + radio
    ## Model 2: sales ~ TV + radio + newspaper
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1    197 556.91                           
    ## 2    196 556.83  1  0.088717 0.0312 0.8599

# 3.3 Other Considerations in the Regression Model

## 3.3.1 Qualitative Predictors

Load the `credit` data set and regress credit card balance on home
ownership (two levels):

``` r
credit <- ISLR2::Credit
lm_balance_own <- lm(Balance ~ Own, data = credit)
tbl_regression(lm_balance_own, intercept = TRUE,
               show_single_row = "Own")
```

<div id="uzyqngilov" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#uzyqngilov .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#uzyqngilov .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uzyqngilov .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uzyqngilov .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uzyqngilov .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uzyqngilov .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uzyqngilov .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#uzyqngilov .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#uzyqngilov .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uzyqngilov .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uzyqngilov .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#uzyqngilov .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#uzyqngilov .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#uzyqngilov .gt_from_md > :first-child {
  margin-top: 0;
}

#uzyqngilov .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uzyqngilov .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#uzyqngilov .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#uzyqngilov .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uzyqngilov .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#uzyqngilov .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uzyqngilov .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uzyqngilov .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uzyqngilov .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uzyqngilov .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uzyqngilov .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#uzyqngilov .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uzyqngilov .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#uzyqngilov .gt_left {
  text-align: left;
}

#uzyqngilov .gt_center {
  text-align: center;
}

#uzyqngilov .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uzyqngilov .gt_font_normal {
  font-weight: normal;
}

#uzyqngilov .gt_font_bold {
  font-weight: bold;
}

#uzyqngilov .gt_font_italic {
  font-style: italic;
}

#uzyqngilov .gt_super {
  font-size: 65%;
}

#uzyqngilov .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">(Intercept)</td>
<td class="gt_row gt_center">510</td>
<td class="gt_row gt_center">445, 575</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">Own</td>
<td class="gt_row gt_center">20</td>
<td class="gt_row gt_center">-71, 110</td>
<td class="gt_row gt_center">0.7</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="4">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          CI = Confidence Interval
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>

And with region (three levels):

``` r
lm_balance_region <- lm(Balance ~ Region, data = credit)
tbl_regression(lm_balance_region, intercept = TRUE)
```

<div id="cigcjbgxni" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#cigcjbgxni .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#cigcjbgxni .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cigcjbgxni .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#cigcjbgxni .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#cigcjbgxni .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cigcjbgxni .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cigcjbgxni .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#cigcjbgxni .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#cigcjbgxni .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#cigcjbgxni .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#cigcjbgxni .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#cigcjbgxni .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#cigcjbgxni .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#cigcjbgxni .gt_from_md > :first-child {
  margin-top: 0;
}

#cigcjbgxni .gt_from_md > :last-child {
  margin-bottom: 0;
}

#cigcjbgxni .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#cigcjbgxni .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#cigcjbgxni .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cigcjbgxni .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#cigcjbgxni .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cigcjbgxni .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#cigcjbgxni .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#cigcjbgxni .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cigcjbgxni .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cigcjbgxni .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#cigcjbgxni .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cigcjbgxni .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#cigcjbgxni .gt_left {
  text-align: left;
}

#cigcjbgxni .gt_center {
  text-align: center;
}

#cigcjbgxni .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#cigcjbgxni .gt_font_normal {
  font-weight: normal;
}

#cigcjbgxni .gt_font_bold {
  font-weight: bold;
}

#cigcjbgxni .gt_font_italic {
  font-style: italic;
}

#cigcjbgxni .gt_super {
  font-size: 65%;
}

#cigcjbgxni .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">(Intercept)</td>
<td class="gt_row gt_center">531</td>
<td class="gt_row gt_center">440, 622</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">Region</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">East</td>
<td class="gt_row gt_center">—</td>
<td class="gt_row gt_center">—</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">South</td>
<td class="gt_row gt_center">-13</td>
<td class="gt_row gt_center">-124, 99</td>
<td class="gt_row gt_center">0.8</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">West</td>
<td class="gt_row gt_center">-19</td>
<td class="gt_row gt_center">-147, 109</td>
<td class="gt_row gt_center">0.8</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="4">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          CI = Confidence Interval
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>

Note that the model `summary()` returns the results of the *F*-test
below:

``` r
summary(lm_balance_region)
```

    ## 
    ## Call:
    ## lm(formula = Balance ~ Region, data = credit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -531.00 -457.08  -63.25  339.25 1480.50 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   531.00      46.32  11.464   <2e-16 ***
    ## RegionSouth   -12.50      56.68  -0.221    0.826    
    ## RegionWest    -18.69      65.02  -0.287    0.774    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 460.9 on 397 degrees of freedom
    ## Multiple R-squared:  0.0002188,  Adjusted R-squared:  -0.004818 
    ## F-statistic: 0.04344 on 2 and 397 DF,  p-value: 0.9575

Which is the same as that returned by `anova()`:

``` r
anova(lm_balance_region)
```

    ## Analysis of Variance Table
    ## 
    ## Response: Balance
    ##            Df   Sum Sq Mean Sq F value Pr(>F)
    ## Region      2    18454    9227  0.0434 0.9575
    ## Residuals 397 84321458  212397

## 3.3.2 Extensions of the Linear Model

The effect on sales, with an interaction term between TV and radio:

``` r
lm_sales_inter <- lm(sales ~ radio * TV, data = advertising)
tbl_regression(lm_sales_inter, intercept = TRUE)
```

<div id="qrweqkcpnp" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#qrweqkcpnp .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#qrweqkcpnp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qrweqkcpnp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#qrweqkcpnp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#qrweqkcpnp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qrweqkcpnp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qrweqkcpnp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#qrweqkcpnp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#qrweqkcpnp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#qrweqkcpnp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#qrweqkcpnp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#qrweqkcpnp .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#qrweqkcpnp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#qrweqkcpnp .gt_from_md > :first-child {
  margin-top: 0;
}

#qrweqkcpnp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qrweqkcpnp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#qrweqkcpnp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#qrweqkcpnp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qrweqkcpnp .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#qrweqkcpnp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qrweqkcpnp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#qrweqkcpnp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#qrweqkcpnp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qrweqkcpnp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qrweqkcpnp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#qrweqkcpnp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qrweqkcpnp .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#qrweqkcpnp .gt_left {
  text-align: left;
}

#qrweqkcpnp .gt_center {
  text-align: center;
}

#qrweqkcpnp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qrweqkcpnp .gt_font_normal {
  font-weight: normal;
}

#qrweqkcpnp .gt_font_bold {
  font-weight: bold;
}

#qrweqkcpnp .gt_font_italic {
  font-style: italic;
}

#qrweqkcpnp .gt_super {
  font-size: 65%;
}

#qrweqkcpnp .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">(Intercept)</td>
<td class="gt_row gt_center">6.8</td>
<td class="gt_row gt_center">6.3, 7.2</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">radio</td>
<td class="gt_row gt_center">0.03</td>
<td class="gt_row gt_center">0.01, 0.05</td>
<td class="gt_row gt_center">0.001</td></tr>
    <tr><td class="gt_row gt_left">TV</td>
<td class="gt_row gt_center">0.02</td>
<td class="gt_row gt_center">0.02, 0.02</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">radio * TV</td>
<td class="gt_row gt_center">0.00</td>
<td class="gt_row gt_center">0.00, 0.00</td>
<td class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="4">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          CI = Confidence Interval
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>

Compare the model with and without the interaction term:

``` r
lm_sales_radio_tv <- lm(sales ~ radio + TV, data = advertising)

glance(lm_sales_radio_tv) %>%
  mutate(model = "additive") %>%
  bind_rows(
    glance(lm_sales_inter) %>%
      mutate(model = "interaction")
  ) %>%
  select(model, r.squared, AIC, BIC)
```

    ## # A tibble: 2 x 4
    ##   model       r.squared   AIC   BIC
    ##   <chr>           <dbl> <dbl> <dbl>
    ## 1 additive        0.897  780.  794.
    ## 2 interaction     0.968  550.  567.

Re-crate Figure 3.7, comparing the balance model with and without an
interaction term:

``` r
lm_balance_income_student <-
  lm(Balance ~ Income + Student, data = credit)
lm_balance_income_student_inter <-
  lm(Balance ~ Income * Student, data = credit)

d <- tibble(Income = seq(0, 150, 0.1)) %>%
  crossing(Student = factor(c("No", "Yes")))
augment(lm_balance_income_student, newdata = d) %>%
  mutate(model = "additive") %>%
  bind_rows(
    augment(lm_balance_income_student_inter, newdata = d) %>%
      mutate(model = "interaction")
  ) %>%
  ggplot(aes(x = Income, y = .fitted, color = Student)) +
  geom_line() +
  facet_wrap(~model, nrow = 1) +
  labs(y = "Balance") +
  theme_td_grid()
```

![](chapter03_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

Fit mpg to horsepower with a linear term, quadratic term, and up to the
fifth degree:

``` r
auto <- ISLR2::Auto

lm_mpg_hp <- lm(mpg ~ horsepower, data = auto)
lm_mpg_hp2 <- lm(mpg ~ horsepower + I(horsepower^2), data = auto)
lm_mpg_hp5 <-
  lm(
    mpg ~ horsepower + I(horsepower^2) + I(horsepower^3) +
      I(horsepower^4) + I(horsepower^5),
    data = auto
  )

d <- tibble(horsepower = seq(1, 250, 0.1))

augment(lm_mpg_hp, newdata = d) %>% mutate(model = "Linear") %>%
  bind_rows(
    augment(lm_mpg_hp2, newdata = d) %>% mutate(model = "Degree 2")
  ) %>%
  bind_rows(
    augment(lm_mpg_hp5, newdata = d) %>% mutate(model = "Degree 5")
  ) %>%
  ggplot(aes(x = horsepower, y = .fitted, color = model)) +
  geom_point(
    aes(y = mpg), data = auto, color = "lightgrey", shape = 21, size = 4
  ) +
  geom_line(size = 1.5) +
  coord_cartesian(xlim = c(40, 230), ylim = c(8, 52)) +
  add_facet_borders() +
  theme(legend.position = c(0.7, 0.8)) +
  labs(y = "mpg", color = NULL)
```

![](chapter03_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
glance(lm_mpg_hp) %>% mutate(model = "Linear") %>%
  bind_rows(
    glance(lm_mpg_hp2) %>% mutate(model = "Degree 2")
  ) %>%
  bind_rows(
    glance(lm_mpg_hp5) %>% mutate(model = "Degree 5")
  ) %>%
  select(model, r.squared, AIC, BIC) %>%
  gt()
```

<div id="wcjfbazofc" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#wcjfbazofc .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#wcjfbazofc .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wcjfbazofc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#wcjfbazofc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#wcjfbazofc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wcjfbazofc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wcjfbazofc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#wcjfbazofc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#wcjfbazofc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wcjfbazofc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wcjfbazofc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#wcjfbazofc .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#wcjfbazofc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#wcjfbazofc .gt_from_md > :first-child {
  margin-top: 0;
}

#wcjfbazofc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wcjfbazofc .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#wcjfbazofc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#wcjfbazofc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wcjfbazofc .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#wcjfbazofc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wcjfbazofc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wcjfbazofc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wcjfbazofc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wcjfbazofc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wcjfbazofc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#wcjfbazofc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wcjfbazofc .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#wcjfbazofc .gt_left {
  text-align: left;
}

#wcjfbazofc .gt_center {
  text-align: center;
}

#wcjfbazofc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wcjfbazofc .gt_font_normal {
  font-weight: normal;
}

#wcjfbazofc .gt_font_bold {
  font-weight: bold;
}

#wcjfbazofc .gt_font_italic {
  font-style: italic;
}

#wcjfbazofc .gt_super {
  font-size: 65%;
}

#wcjfbazofc .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">model</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">r.squared</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">AIC</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">BIC</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Linear</td>
<td class="gt_row gt_right">0.6059483</td>
<td class="gt_row gt_right">2363.324</td>
<td class="gt_row gt_right">2375.237</td></tr>
    <tr><td class="gt_row gt_left">Degree 2</td>
<td class="gt_row gt_right">0.6875590</td>
<td class="gt_row gt_right">2274.354</td>
<td class="gt_row gt_right">2290.239</td></tr>
    <tr><td class="gt_row gt_left">Degree 5</td>
<td class="gt_row gt_right">0.6967390</td>
<td class="gt_row gt_right">2268.663</td>
<td class="gt_row gt_right">2296.462</td></tr>
  </tbody>
  
  
</table>
</div>

## 3.3 Potential Problems

1.  Non-linearity of the Data

``` r
plot(lm_mpg_hp, 1)
```

![](chapter03_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
plot(lm_mpg_hp2, 1)
```

![](chapter03_files/figure-gfm/unnamed-chunk-27-2.png)<!-- -->

2.  Correlation of Error Terms

3.  Non-constant Variance of Error Terms

``` r
plot(lm_mpg_hp)
```

![](chapter03_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->![](chapter03_files/figure-gfm/unnamed-chunk-28-2.png)<!-- -->![](chapter03_files/figure-gfm/unnamed-chunk-28-3.png)<!-- -->![](chapter03_files/figure-gfm/unnamed-chunk-28-4.png)<!-- -->

4.  Outliers

5.  High Leverage Points

6.  Collinearity

``` r
credit %>%
  select(Limit, Age, Rating) %>%
  pivot_longer(cols = c(Age, Rating)) %>%
  ggplot(aes(x = Limit, y = value)) +
  geom_point() +
  facet_wrap(~name, nrow = 1, scales = "free_y")
```

![](chapter03_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
lm_balance_age_limit <- lm(Balance ~ Age + Limit, data = credit)
lm_balance_rating_limit <- lm(Balance ~ Rating + Limit, data = credit)

tbl_regression(lm_balance_age_limit)
```

<div id="ymcsumvzcq" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ymcsumvzcq .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ymcsumvzcq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ymcsumvzcq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ymcsumvzcq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ymcsumvzcq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ymcsumvzcq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ymcsumvzcq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ymcsumvzcq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ymcsumvzcq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ymcsumvzcq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ymcsumvzcq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ymcsumvzcq .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ymcsumvzcq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ymcsumvzcq .gt_from_md > :first-child {
  margin-top: 0;
}

#ymcsumvzcq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ymcsumvzcq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ymcsumvzcq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ymcsumvzcq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ymcsumvzcq .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ymcsumvzcq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ymcsumvzcq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ymcsumvzcq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ymcsumvzcq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ymcsumvzcq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ymcsumvzcq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ymcsumvzcq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ymcsumvzcq .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ymcsumvzcq .gt_left {
  text-align: left;
}

#ymcsumvzcq .gt_center {
  text-align: center;
}

#ymcsumvzcq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ymcsumvzcq .gt_font_normal {
  font-weight: normal;
}

#ymcsumvzcq .gt_font_bold {
  font-weight: bold;
}

#ymcsumvzcq .gt_font_italic {
  font-style: italic;
}

#ymcsumvzcq .gt_super {
  font-size: 65%;
}

#ymcsumvzcq .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Age</td>
<td class="gt_row gt_center">-2.3</td>
<td class="gt_row gt_center">-3.6, -1.0</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">Limit</td>
<td class="gt_row gt_center">0.17</td>
<td class="gt_row gt_center">0.16, 0.18</td>
<td class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="4">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          CI = Confidence Interval
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>

``` r
tbl_regression(lm_balance_rating_limit)
```

<div id="vhwrukwsgw" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#vhwrukwsgw .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#vhwrukwsgw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vhwrukwsgw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vhwrukwsgw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vhwrukwsgw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vhwrukwsgw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vhwrukwsgw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#vhwrukwsgw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#vhwrukwsgw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vhwrukwsgw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vhwrukwsgw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vhwrukwsgw .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#vhwrukwsgw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#vhwrukwsgw .gt_from_md > :first-child {
  margin-top: 0;
}

#vhwrukwsgw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vhwrukwsgw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#vhwrukwsgw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#vhwrukwsgw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vhwrukwsgw .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#vhwrukwsgw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vhwrukwsgw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vhwrukwsgw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vhwrukwsgw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vhwrukwsgw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vhwrukwsgw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#vhwrukwsgw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vhwrukwsgw .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#vhwrukwsgw .gt_left {
  text-align: left;
}

#vhwrukwsgw .gt_center {
  text-align: center;
}

#vhwrukwsgw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vhwrukwsgw .gt_font_normal {
  font-weight: normal;
}

#vhwrukwsgw .gt_font_bold {
  font-weight: bold;
}

#vhwrukwsgw .gt_font_italic {
  font-style: italic;
}

#vhwrukwsgw .gt_super {
  font-size: 65%;
}

#vhwrukwsgw .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Rating</td>
<td class="gt_row gt_center">2.2</td>
<td class="gt_row gt_center">0.33, 4.1</td>
<td class="gt_row gt_center">0.021</td></tr>
    <tr><td class="gt_row gt_left">Limit</td>
<td class="gt_row gt_center">0.02</td>
<td class="gt_row gt_center">-0.10, 0.15</td>
<td class="gt_row gt_center">0.7</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="4">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          CI = Confidence Interval
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>

To calculate variance inflation factors (VIF), there are R functions
such as `car::vif` which can be used, but it is fairly simple to
calculate by hand:

``` r
lm_rating_age_limit <- lm(Rating ~ Age + Limit, data = credit)
lm_age_rating_limit <- lm(Age ~ Rating + Limit, data = credit)
lm_limit_age_rating <- lm(Limit ~ Age + Rating, data = credit)
tribble(
  ~Predictor, ~`R^2`,
  "Age", 1 / (1 - summary(lm_age_rating_limit)$r.squared),
  "Rating", 1 / (1 - summary(lm_rating_age_limit)$r.squared),
  "Limit", 1 / (1 - summary(lm_limit_age_rating)$r.squared)
) %>%
  gt()
```

<div id="zqowmuqwiv" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#zqowmuqwiv .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zqowmuqwiv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zqowmuqwiv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zqowmuqwiv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zqowmuqwiv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zqowmuqwiv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zqowmuqwiv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zqowmuqwiv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zqowmuqwiv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zqowmuqwiv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zqowmuqwiv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zqowmuqwiv .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#zqowmuqwiv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zqowmuqwiv .gt_from_md > :first-child {
  margin-top: 0;
}

#zqowmuqwiv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zqowmuqwiv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zqowmuqwiv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#zqowmuqwiv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zqowmuqwiv .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#zqowmuqwiv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zqowmuqwiv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zqowmuqwiv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zqowmuqwiv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zqowmuqwiv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zqowmuqwiv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#zqowmuqwiv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zqowmuqwiv .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#zqowmuqwiv .gt_left {
  text-align: left;
}

#zqowmuqwiv .gt_center {
  text-align: center;
}

#zqowmuqwiv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zqowmuqwiv .gt_font_normal {
  font-weight: normal;
}

#zqowmuqwiv .gt_font_bold {
  font-weight: bold;
}

#zqowmuqwiv .gt_font_italic {
  font-style: italic;
}

#zqowmuqwiv .gt_super {
  font-size: 65%;
}

#zqowmuqwiv .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Predictor</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">R^2</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Age</td>
<td class="gt_row gt_right">1.011385</td></tr>
    <tr><td class="gt_row gt_left">Rating</td>
<td class="gt_row gt_right">160.668301</td></tr>
    <tr><td class="gt_row gt_left">Limit</td>
<td class="gt_row gt_right">160.592880</td></tr>
  </tbody>
  
  
</table>
</div>

# 3.4 The Marketing Plan

# 3.5 Comparison of Linear Regression with *K*-Nearest Neighbors

# 3.6 Lab: Linear Regression

## 3.6.1 Libraries

``` r
boston <- ISLR2::Boston
```

## 3.6.2 Simple Linear Regression

Regression median value of owner-occupied homes `medv` on lower status
of the population `lstat`:

``` r
lm_medv_lstat <- lm(medv ~ lstat, data = boston)
confint(lm_medv_lstat)
```

    ##                 2.5 %     97.5 %
    ## (Intercept) 33.448457 35.6592247
    ## lstat       -1.026148 -0.8739505

``` r
nd <- tibble(lstat = c(5, 10, 15))

predict(lm_medv_lstat, nd, interval = "confidence")
```

    ##        fit      lwr      upr
    ## 1 29.80359 29.00741 30.59978
    ## 2 25.05335 24.47413 25.63256
    ## 3 20.30310 19.73159 20.87461

``` r
predict(lm_medv_lstat, nd, interval = "prediction")
```

    ##        fit       lwr      upr
    ## 1 29.80359 17.565675 42.04151
    ## 2 25.05335 12.827626 37.27907
    ## 3 20.30310  8.077742 32.52846

``` r
boston %>%
  ggplot(aes(x = lstat, y = medv)) +
  geom_point() +
  geom_smooth(method = "lm" , formula = "y ~ x")
```

![](chapter03_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

To display model diagnostics, we can call `plot()` on the model object
as we have before. I like the `performance` package because it uses
`ggplot2`:

``` r
performance::check_model(lm_medv_lstat)
```

    ## Registered S3 methods overwritten by 'parameters':
    ##   method                           from      
    ##   as.double.parameters_kurtosis    datawizard
    ##   as.double.parameters_skewness    datawizard
    ##   as.double.parameters_smoothness  datawizard
    ##   as.numeric.parameters_kurtosis   datawizard
    ##   as.numeric.parameters_skewness   datawizard
    ##   as.numeric.parameters_smoothness datawizard
    ##   print.parameters_distribution    datawizard
    ##   print.parameters_kurtosis        datawizard
    ##   print.parameters_skewness        datawizard
    ##   summary.parameters_kurtosis      datawizard
    ##   summary.parameters_skewness      datawizard

    ## Loading required namespace: qqplotr

![](chapter03_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

## 3.6.3 Multiple Linear Regression

Fit to all predictors and check VIF:

``` r
lm_medv_all <- lm(medv ~ ., data = boston)
performance::check_collinearity(lm_medv_all)
```

    ## # Check for Multicollinearity
    ## 
    ## Low Correlation
    ## 
    ##     Term  VIF Increased SE Tolerance
    ##     crim 1.77         1.33      0.57
    ##       zn 2.30         1.52      0.44
    ##    indus 3.99         2.00      0.25
    ##     chas 1.07         1.03      0.93
    ##      nox 4.37         2.09      0.23
    ##       rm 1.91         1.38      0.52
    ##      age 3.09         1.76      0.32
    ##      dis 3.95         1.99      0.25
    ##  ptratio 1.80         1.34      0.56
    ##    lstat 2.87         1.69      0.35
    ## 
    ## Moderate Correlation
    ## 
    ##  Term  VIF Increased SE Tolerance
    ##   rad 7.45         2.73      0.13
    ##   tax 9.00         3.00      0.11

The `rad` (accessibility to radial highways) and `tax` (property tax
rate) variables have moderate VIF.

## 3.6.4 Interaction Terms

## 3.6.5 Non-linear Transformations of the Predictors

Perform a regression of `medv` onto `lstat` and `lstat^2`, and compare
with `anova`:

``` r
lm_medv_lstat2 <- lm(medv ~ lstat + I(lstat^2), data = boston)
tbl_regression(lm_medv_lstat2)
```

<div id="zajsxafwme" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#zajsxafwme .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zajsxafwme .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zajsxafwme .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zajsxafwme .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zajsxafwme .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zajsxafwme .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zajsxafwme .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zajsxafwme .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zajsxafwme .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zajsxafwme .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zajsxafwme .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zajsxafwme .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#zajsxafwme .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zajsxafwme .gt_from_md > :first-child {
  margin-top: 0;
}

#zajsxafwme .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zajsxafwme .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zajsxafwme .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#zajsxafwme .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zajsxafwme .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#zajsxafwme .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zajsxafwme .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zajsxafwme .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zajsxafwme .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zajsxafwme .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zajsxafwme .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#zajsxafwme .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zajsxafwme .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#zajsxafwme .gt_left {
  text-align: left;
}

#zajsxafwme .gt_center {
  text-align: center;
}

#zajsxafwme .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zajsxafwme .gt_font_normal {
  font-weight: normal;
}

#zajsxafwme .gt_font_bold {
  font-weight: bold;
}

#zajsxafwme .gt_font_italic {
  font-style: italic;
}

#zajsxafwme .gt_super {
  font-size: 65%;
}

#zajsxafwme .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Beta</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">lstat</td>
<td class="gt_row gt_center">-2.3</td>
<td class="gt_row gt_center">-2.6, -2.1</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">I(lstat^2)</td>
<td class="gt_row gt_center">0.04</td>
<td class="gt_row gt_center">0.04, 0.05</td>
<td class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="4">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          CI = Confidence Interval
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>

``` r
anova(lm_medv_lstat, lm_medv_lstat2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: medv ~ lstat
    ## Model 2: medv ~ lstat + I(lstat^2)
    ##   Res.Df   RSS Df Sum of Sq     F    Pr(>F)    
    ## 1    504 19472                                 
    ## 2    503 15347  1    4125.1 135.2 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## 3.6.6 Qualitative Predictors

``` r
carseats <- ISLR2::Carseats
lm_sales <- lm(Sales ~ . + Income:Advertising + Price:Age,
               data = carseats)

contrasts(carseats$ShelveLoc)
```

    ##        Good Medium
    ## Bad       0      0
    ## Good      1      0
    ## Medium    0      1

# 3.7 Exercises

## 8. Simple linear regression with `Auto`

-   Model summary

``` r
summary(lm_mpg_hp)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ horsepower, data = auto)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.5710  -3.2592  -0.3435   2.7630  16.9240 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 39.935861   0.717499   55.66   <2e-16 ***
    ## horsepower  -0.157845   0.006446  -24.49   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.906 on 390 degrees of freedom
    ## Multiple R-squared:  0.6059, Adjusted R-squared:  0.6049 
    ## F-statistic: 599.7 on 1 and 390 DF,  p-value: < 2.2e-16

    * There is a relationship between `mpg` and `horsepower`.

    * Is is highly significant: $p$ <2e-16.

    * The relationship is negative. Every unit of horsepower is associated with a -0.16 reduction in miles per gallon.

    * The confidence and prediction intervals of predicted `mpg` given `horsepower` = 98:

``` r
predict(lm_mpg_hp, tibble(horsepower = 98), interval = "confidence")
```

    ##        fit      lwr      upr
    ## 1 24.46708 23.97308 24.96108

``` r
predict(lm_mpg_hp, tibble(horsepower = 98), interval = "prediction")
```

    ##        fit     lwr      upr
    ## 1 24.46708 14.8094 34.12476

-   Plot with best fit line:

``` r
auto %>%
  ggplot(aes(x = horsepower)) +
  geom_point(aes(y = mpg)) +
  geom_line(
    data = augment(lm_mpg_hp, newdata = tibble(horsepower = 50:220)),
    aes(y = .fitted), size = 2, color = td_colors$nice$emerald
  )
```

![](chapter03_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

-   Diagnostic plots:

``` r
performance::check_model(lm_mpg_hp)
```

![](chapter03_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

Two potential problems: non-linearity (top left plot) and homogeneity of
variance (top right).

## 9. Multiple linear regression with `Auto`

-   Scatterplot of all variables.

There are some advanced ways to produce these bivariate correlation
plots in `gglpot2`:

``` r
auto %>%
  select(-name) %>%
  rowid_to_column() %>%
  pivot_longer(cols = -rowid) %>%
  mutate(name = factor(name)) %>%
  full_join(., ., by = "rowid") %>%
  filter(as.integer(name.x) > as.integer(name.y)) %>%
  ggplot(aes(x = value.x, y = value.y)) +
  geom_point() +
  facet_wrap(name.x ~ name.y, scales = "free") +
  add_facet_borders()
```

![](chapter03_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

But they take a lot of effort to look nice. The `GGally::ggpairs()`
function quickly makes a scatterplot matrix from a given data set, and
also gives the correlation coefficients:

``` r
GGally::ggpairs(auto %>% select(-name))
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

![](chapter03_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

-   Compute the correlations.

``` r
cor(auto %>% select(-name))
```

    ##                     mpg  cylinders displacement horsepower     weight
    ## mpg           1.0000000 -0.7776175   -0.8051269 -0.7784268 -0.8322442
    ## cylinders    -0.7776175  1.0000000    0.9508233  0.8429834  0.8975273
    ## displacement -0.8051269  0.9508233    1.0000000  0.8972570  0.9329944
    ## horsepower   -0.7784268  0.8429834    0.8972570  1.0000000  0.8645377
    ## weight       -0.8322442  0.8975273    0.9329944  0.8645377  1.0000000
    ## acceleration  0.4233285 -0.5046834   -0.5438005 -0.6891955 -0.4168392
    ## year          0.5805410 -0.3456474   -0.3698552 -0.4163615 -0.3091199
    ## origin        0.5652088 -0.5689316   -0.6145351 -0.4551715 -0.5850054
    ##              acceleration       year     origin
    ## mpg             0.4233285  0.5805410  0.5652088
    ## cylinders      -0.5046834 -0.3456474 -0.5689316
    ## displacement   -0.5438005 -0.3698552 -0.6145351
    ## horsepower     -0.6891955 -0.4163615 -0.4551715
    ## weight         -0.4168392 -0.3091199 -0.5850054
    ## acceleration    1.0000000  0.2903161  0.2127458
    ## year            0.2903161  1.0000000  0.1815277
    ## origin          0.2127458  0.1815277  1.0000000

2.  Fit the multiple linear regression.

``` r
lm_mpg <- lm(mpg ~ . -name, data = auto)
summary(lm_mpg)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ . - name, data = auto)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.5903 -2.1565 -0.1169  1.8690 13.0604 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -17.218435   4.644294  -3.707  0.00024 ***
    ## cylinders     -0.493376   0.323282  -1.526  0.12780    
    ## displacement   0.019896   0.007515   2.647  0.00844 ** 
    ## horsepower    -0.016951   0.013787  -1.230  0.21963    
    ## weight        -0.006474   0.000652  -9.929  < 2e-16 ***
    ## acceleration   0.080576   0.098845   0.815  0.41548    
    ## year           0.750773   0.050973  14.729  < 2e-16 ***
    ## origin         1.426141   0.278136   5.127 4.67e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.328 on 384 degrees of freedom
    ## Multiple R-squared:  0.8215, Adjusted R-squared:  0.8182 
    ## F-statistic: 252.4 on 7 and 384 DF,  p-value: < 2.2e-16

-   There is a relationship between the predictors and `mpg`: *F* =
    252.4280453
-   The following terms are statistically significant: (Intercept),
    displacement, weight, year, origin
-   The coefficient for `year` suggests that, for every increment in car
    model year, `mpg` increases by 0.75

# Reproducibility

<details>
<summary>
Reproducibility receipt
</summary>

``` r
Sys.time()
```

    ## [1] "2021-08-10 20:34:23 AST"

``` r
if ("git2r" %in% installed.packages()) {
  if (git2r::in_repository()) {
    git2r::repository()
  }
}
```

    ## Local:    main C:/Users/tdunn/Documents/learning/islr
    ## Remote:   main @ origin (https://github.com/taylordunn/islr)
    ## Head:     [0cb60a9] 2021-08-10: Double curly bracers around title to get title case

``` r
sessioninfo::session_info()
```

    ## - Session info ---------------------------------------------------------------
    ##  setting  value                       
    ##  version  R version 4.1.0 (2021-05-18)
    ##  os       Windows 10 x64              
    ##  system   x86_64, mingw32             
    ##  ui       RTerm                       
    ##  language (EN)                        
    ##  collate  English_Canada.1252         
    ##  ctype    English_Canada.1252         
    ##  tz       America/Curacao             
    ##  date     2021-08-10                  
    ## 
    ## - Packages -------------------------------------------------------------------
    ##  package       * version date       lib source                           
    ##  assertthat      0.2.1   2019-03-21 [1] CRAN (R 4.1.0)                   
    ##  backports       1.2.1   2020-12-09 [1] CRAN (R 4.1.0)                   
    ##  bayestestR      0.10.5  2021-07-26 [1] CRAN (R 4.1.0)                   
    ##  broom         * 0.7.8   2021-06-24 [1] CRAN (R 4.1.0)                   
    ##  broom.helpers   1.3.0   2021-04-10 [1] CRAN (R 4.1.0)                   
    ##  cellranger      1.1.0   2016-07-27 [1] CRAN (R 4.1.0)                   
    ##  checkmate       2.0.0   2020-02-06 [1] CRAN (R 4.1.0)                   
    ##  cli             3.0.0   2021-06-30 [1] CRAN (R 4.1.0)                   
    ##  colorspace      2.0-2   2021-06-24 [1] CRAN (R 4.1.0)                   
    ##  commonmark      1.7     2018-12-01 [1] CRAN (R 4.1.0)                   
    ##  crayon          1.4.1   2021-02-08 [1] CRAN (R 4.1.0)                   
    ##  datawizard      0.1.0   2021-06-18 [1] CRAN (R 4.1.0)                   
    ##  DBI             1.1.1   2021-01-15 [1] CRAN (R 4.1.0)                   
    ##  dbplyr          2.1.1   2021-04-06 [1] CRAN (R 4.1.0)                   
    ##  DEoptimR        1.0-9   2021-05-24 [1] CRAN (R 4.1.0)                   
    ##  digest          0.6.27  2020-10-24 [1] CRAN (R 4.1.0)                   
    ##  dplyr         * 1.0.7   2021-06-18 [1] CRAN (R 4.1.0)                   
    ##  dunnr         * 0.2.0   2021-08-06 [1] Github (taylordunn/dunnr@0a41433)
    ##  effectsize      0.4.5   2021-05-25 [1] CRAN (R 4.1.0)                   
    ##  ellipsis        0.3.2   2021-04-29 [1] CRAN (R 4.1.0)                   
    ##  evaluate        0.14    2019-05-28 [1] CRAN (R 4.1.0)                   
    ##  extrafont       0.17    2014-12-08 [1] CRAN (R 4.1.0)                   
    ##  extrafontdb     1.0     2012-06-11 [1] CRAN (R 4.1.0)                   
    ##  fansi           0.5.0   2021-05-25 [1] CRAN (R 4.1.0)                   
    ##  farver          2.1.0   2021-02-28 [1] CRAN (R 4.1.0)                   
    ##  forcats       * 0.5.1   2021-01-27 [1] CRAN (R 4.1.0)                   
    ##  fs              1.5.0   2020-07-31 [1] CRAN (R 4.1.0)                   
    ##  generics        0.1.0   2020-10-31 [1] CRAN (R 4.1.0)                   
    ##  GGally          2.1.2   2021-06-21 [1] CRAN (R 4.1.0)                   
    ##  ggplot2       * 3.3.5   2021-06-25 [1] CRAN (R 4.1.0)                   
    ##  ggrepel         0.9.1   2021-01-15 [1] CRAN (R 4.1.0)                   
    ##  ggridges        0.5.3   2021-01-08 [1] CRAN (R 4.1.0)                   
    ##  git2r           0.28.0  2021-01-10 [1] CRAN (R 4.1.0)                   
    ##  glue          * 1.4.2   2020-08-27 [1] CRAN (R 4.1.0)                   
    ##  gridExtra       2.3     2017-09-09 [1] CRAN (R 4.1.0)                   
    ##  gt            * 0.3.0   2021-05-12 [1] CRAN (R 4.1.0)                   
    ##  gtable          0.3.0   2019-03-25 [1] CRAN (R 4.1.0)                   
    ##  gtsummary     * 1.4.2   2021-07-13 [1] CRAN (R 4.1.0)                   
    ##  haven           2.4.1   2021-04-23 [1] CRAN (R 4.1.0)                   
    ##  here          * 1.0.1   2020-12-13 [1] CRAN (R 4.1.0)                   
    ##  highr           0.9     2021-04-16 [1] CRAN (R 4.1.0)                   
    ##  hms             1.1.0   2021-05-17 [1] CRAN (R 4.1.0)                   
    ##  htmltools       0.5.1.1 2021-01-22 [1] CRAN (R 4.1.0)                   
    ##  httr            1.4.2   2020-07-20 [1] CRAN (R 4.1.0)                   
    ##  insight         0.14.2  2021-06-22 [1] CRAN (R 4.1.0)                   
    ##  ISLR2           1.0     2021-07-22 [1] CRAN (R 4.1.0)                   
    ##  jsonlite        1.7.2   2020-12-09 [1] CRAN (R 4.1.0)                   
    ##  knitr           1.33    2021-04-24 [1] CRAN (R 4.1.0)                   
    ##  labeling        0.4.2   2020-10-20 [1] CRAN (R 4.1.0)                   
    ##  labelled        2.8.0   2021-03-08 [1] CRAN (R 4.1.0)                   
    ##  lattice         0.20-44 2021-05-02 [2] CRAN (R 4.1.0)                   
    ##  lifecycle       1.0.0   2021-02-15 [1] CRAN (R 4.1.0)                   
    ##  lubridate       1.7.10  2021-02-26 [1] CRAN (R 4.1.0)                   
    ##  magrittr        2.0.1   2020-11-17 [1] CRAN (R 4.1.0)                   
    ##  MASS            7.3-54  2021-05-03 [2] CRAN (R 4.1.0)                   
    ##  Matrix          1.3-3   2021-05-04 [2] CRAN (R 4.1.0)                   
    ##  mgcv            1.8-35  2021-04-18 [2] CRAN (R 4.1.0)                   
    ##  modelr          0.1.8   2020-05-19 [1] CRAN (R 4.1.0)                   
    ##  munsell         0.5.0   2018-06-12 [1] CRAN (R 4.1.0)                   
    ##  nlme            3.1-152 2021-02-04 [2] CRAN (R 4.1.0)                   
    ##  parameters      0.14.0  2021-05-29 [1] CRAN (R 4.1.0)                   
    ##  patchwork     * 1.1.1   2020-12-17 [1] CRAN (R 4.1.0)                   
    ##  performance     0.7.3   2021-07-21 [1] CRAN (R 4.1.0)                   
    ##  pillar          1.6.2   2021-07-29 [1] CRAN (R 4.1.0)                   
    ##  pkgconfig       2.0.3   2019-09-22 [1] CRAN (R 4.1.0)                   
    ##  plyr            1.8.6   2020-03-03 [1] CRAN (R 4.1.0)                   
    ##  purrr         * 0.3.4   2020-04-17 [1] CRAN (R 4.1.0)                   
    ##  qqplotr         0.0.5   2021-04-23 [1] CRAN (R 4.1.0)                   
    ##  R6              2.5.0   2020-10-28 [1] CRAN (R 4.1.0)                   
    ##  RColorBrewer    1.1-2   2014-12-07 [1] CRAN (R 4.1.0)                   
    ##  Rcpp            1.0.7   2021-07-07 [1] CRAN (R 4.1.0)                   
    ##  readr         * 1.4.0   2020-10-05 [1] CRAN (R 4.1.0)                   
    ##  readxl          1.3.1   2019-03-13 [1] CRAN (R 4.1.0)                   
    ##  reprex          2.0.0   2021-04-02 [1] CRAN (R 4.1.0)                   
    ##  reshape         0.8.8   2018-10-23 [1] CRAN (R 4.1.0)                   
    ##  rlang           0.4.11  2021-04-30 [1] CRAN (R 4.1.0)                   
    ##  rmarkdown       2.9     2021-06-15 [1] CRAN (R 4.1.0)                   
    ##  robustbase      0.93-8  2021-06-02 [1] CRAN (R 4.1.0)                   
    ##  rprojroot       2.0.2   2020-11-15 [1] CRAN (R 4.1.0)                   
    ##  rstudioapi      0.13    2020-11-12 [1] CRAN (R 4.1.0)                   
    ##  Rttf2pt1        1.3.8   2020-01-10 [1] CRAN (R 4.1.0)                   
    ##  rvest           1.0.0   2021-03-09 [1] CRAN (R 4.1.0)                   
    ##  sass            0.4.0   2021-05-12 [1] CRAN (R 4.1.0)                   
    ##  scales          1.1.1   2020-05-11 [1] CRAN (R 4.1.0)                   
    ##  see             0.6.4   2021-05-29 [1] CRAN (R 4.1.0)                   
    ##  sessioninfo     1.1.1   2018-11-05 [1] CRAN (R 4.1.0)                   
    ##  stringi         1.6.2   2021-05-17 [1] CRAN (R 4.1.0)                   
    ##  stringr       * 1.4.0   2019-02-10 [1] CRAN (R 4.1.0)                   
    ##  survival        3.2-11  2021-04-26 [2] CRAN (R 4.1.0)                   
    ##  tibble        * 3.1.3   2021-07-23 [1] CRAN (R 4.1.0)                   
    ##  tidyr         * 1.1.3   2021-03-03 [1] CRAN (R 4.1.0)                   
    ##  tidyselect      1.1.1   2021-04-30 [1] CRAN (R 4.1.0)                   
    ##  tidyverse     * 1.3.1   2021-04-15 [1] CRAN (R 4.1.0)                   
    ##  utf8            1.2.2   2021-07-24 [1] CRAN (R 4.1.0)                   
    ##  vctrs           0.3.8   2021-04-29 [1] CRAN (R 4.1.0)                   
    ##  withr           2.4.2   2021-04-18 [1] CRAN (R 4.1.0)                   
    ##  xfun            0.24    2021-06-15 [1] CRAN (R 4.1.0)                   
    ##  xml2            1.3.2   2020-04-23 [1] CRAN (R 4.1.0)                   
    ##  yaml            2.2.1   2020-02-01 [1] CRAN (R 4.1.0)                   
    ## 
    ## [1] C:/Users/tdunn/Documents/R/win-library/4.1
    ## [2] C:/Program Files/R/R-4.1.0/library

</details>

# References

<div id="refs" class="references csl-bib-body hanging-indent"
line-spacing="2">

<div id="ref-James2021" class="csl-entry">

James, G., Witten, D., Hastie, T., & Tibshirani, R. (2021). *<span
class="nocase">An Introduction to Statistical Learning with Applications
in R</span>* (2nd ed.). Springer.
https://doi.org/<https://doi.org/10.1007/978-1-0716-1418-1_1>

</div>

</div>
