## SI 15 - Zero Inflated Negative Binomial Regression Summary Table
*n_dams refers to the number of observed dams
*n_dams_mod refers to the modelled dam capacity

Call:

zeroinfl(formula = n_dams ~ n_dams_mod, data = Act_reaches, dist = "negbin")

*Pearson residuals:*

|     Min |      1Q  |  Median  |     3Q   |    Max   |
| ------- | -------- | -------- | -------- | -------- |
|-0.32620 | -0.21442 | -0.01955 | -0.01955 | 19.54168 |

*Count model coefficients (negbin with log link):*

| predictor   | Estimate Std. | Error z | value  | Pr(>z)        |    
| ----------- | ------------- | ------- | ------ | ------------- |
| (Intercept) |  -2.4588      | 0.2560  | -9.605 |  < 2e-16 \*** |
| n_dams_mod  |   0.2655      |  0.1072 |  2.477 |  0.0132 \*    |
| Log(theta)  | -1.9394       | 0.2701  | -7.180 | 6.98e-13 \*** |

*Zero-inflation model coefficients (binomial with logit link):*

| predictor   | Estimate |Std. Error | z value | Pr(>z)       |
| ----------- | -------- | --------- | ------- | ------------ |
| (Intercept) |   4.884  | 1.202     | 4.065   | 4.8e-05 \*** |
| n_dams_mod  | -54.411  | 19.741    | -2.756  | 0.00585 \**  |

\---

Signif. codes:  0 '\*\*\*' 0.001 '\*\*' 0.01 '\*' 0.05 '.' 0.1 ' ' 1 

Theta = 0.1438 
Number of iterations in BFGS optimization: 45 
Log-likelihood:  -258 on 5 Df
