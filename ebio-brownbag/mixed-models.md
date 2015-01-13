<style type="text/css">
.small-code pre code {
font-size: 1.1em;
}
</style>

Mixed models
========================================================
author: Max Joseph
date: Jan 14, 2014
font-family: 'Helvetica', serif
transition: fade
transition-speed: fast

========================================================

### Overview

1. Why bother?

2. Random intercept models

3. Random slope and intercept models

4. Other resources

========================================================

### Why bother?

![plot of chunk unnamed-chunk-1](mixed-models-figure/unnamed-chunk-1-1.png) 

========================================================

### Why bother? 

- increasing use
- broader scope of inference



========================================================

### Why bother? 

- increasing use
- broader scope of inference
- *improved estimates*

========================================================

Estimate group means $\alpha_j$ with data $y_{ij}$ from $J$ groups

Tragically unequal sample sizes


![plot of chunk unnamed-chunk-2](mixed-models-figure/unnamed-chunk-2-1.png) 


===========================================================

### Optimist's ANOVA perspective

Choose between two models

1. Grand mean/total pooling: $\bar Y_{..}$ 

$$ \mu_1 = \mu_2 = ... = \mu_K $$

![plot of chunk unnamed-chunk-3](mixed-models-figure/unnamed-chunk-3-1.png) 


===========================================================

### Optimist's ANOVA perspective

Choose between two models

1. Grand mean: $\bar Y_{..}$

2. Indep. means/no pooling: $\bar Y_{j.}$

![plot of chunk unnamed-chunk-4](mixed-models-figure/unnamed-chunk-4-1.png) 

==========================================================
class: small-code

### Overly optimistic analysis


```r
anova(mod1, mod2)
```

```
Analysis of Variance Table

Model 1: Y ~ 1
Model 2: Y ~ 1 + factor(id)
  Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
1    186 808.96                                  
2    167 562.72 19    246.24 3.8462 1.038e-06 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

==========================================================

What's the deal with small sample sizes $n_j$?

![plot of chunk unnamed-chunk-6](mixed-models-figure/unnamed-chunk-6-1.png) 


==========================================================

### Overfitting much?

High parameter to data ratio for small $n_j$

![plot of chunk unnamed-chunk-7](mixed-models-figure/unnamed-chunk-7-1.png) 


==========================================================

Is there an option better than $\bar y_{j.}$?

Oddly, yes! When we have > 2 groups (see [Stein's paradox](http://statweb.stanford.edu/~ckirby/brad/other/Article1977.pdf))

![plot of chunk unnamed-chunk-8](mixed-models-figure/unnamed-chunk-8-1.png) 


==========================================================

### Conceptualizing a better estimate

Which of these estimates do we trust least?

What information can we use to improve the estimate?

![plot of chunk unnamed-chunk-9](mixed-models-figure/unnamed-chunk-9-1.png) 

==========================================================

### A better estimate

Mixture of sample and grand mean:

$$ \hat \alpha_j = \lambda_j \bar y_{j.} + (1 - \lambda_j) \bar y_{..}$$

$$ 0 < \lambda < 1$$

==========================================================

### A better estimate

Mixture of sample and grand mean:

$$ \hat \alpha_j = \lambda_j \bar y_{j.} + (1 - \lambda_j) \bar y_{..}$$

$$ 0 < \lambda < 1$$

Compromise b/t: 

no pooling ($H_A, \lambda = 1$) & total pooling ($H_0, \lambda=0$)


========================================================

### A better estimate

Mixture of sample and grand mean:

$$ \hat \alpha_j = \lambda_j \bar y_{j.} + (1 - \lambda_j) \bar y_{..}$$

Compromise between no pooling ($H_A, \lambda = 1$) and total pooling ($H_0 \lambda=0$)

$\lambda$ controls degree of shrinkage

===================================================

### Hierarchical models

Random effects impose shrinkage!

$$y_{ij} \sim Normal(\alpha_j, \sigma_y)$$

$$ \alpha_j \sim Normal(\mu_{\alpha}, \sigma_{\alpha})$$

![](http://pngimg.com/upload/star_PNG1597.png)

===================================================

### Hierarchical models

$$y_{ij} \sim Normal(\alpha_j, \sigma_y)$$

$$ \alpha_j \sim Normal(\mu_{\alpha}, \sigma_{\alpha})$$

Amt shrinkage:
- information in group j (e.g. $n_j$)
- variance attributable to groups

$$\dfrac{\sigma_\alpha}{\sigma_\alpha + \sigma_y}$$

=========================================================

### Connection to ANOVA

$$y_{ij} \sim Normal(\alpha_j, \sigma_y)$$

$$ \alpha_j \sim Normal(\mu_{\alpha}, \sigma_{\alpha})$$

$$0 < \sigma_\alpha < \infty$$

Compromise between 
- Total pooling: $\sigma_\alpha = 0$

- No pooling: $\sigma_\alpha = \infty$

Synonyms
================================================

- "partial pooling" 
- "semi-pooling"
- "hierarchical pooling"
- "shrinkage"
- "borrowing information"
- "borrowing strength (of information)"

=========================================================

Demo: [shrinkage.R](https://github.com/mbjoseph/hierarchical_models/blob/master/R_examples/shrinkage.R)

========================================================

### Recap

Random effects impose structure on parameters

$$y_{ij} \sim Normal(\alpha_j, \sigma_y)$$

$$ \alpha_j \sim Normal(\mu_{\alpha}, \sigma_{\alpha})$$


========================================================

### Mixed effects

Combination of fixed *and* random effects

e.g. let's say I'm studying Alot blood parasites

![](ALOT.png)

* courtesy of [Hyperbole and a Half](http://hyperboleandahalf.blogspot.com/)

====================================================================

###  Questions & sampling scenario

Do large-bodied Alots have more blood parasites?

Random sample of $n_j$ individuals at each of $J$ sites.

![](alot_of_alots.png)

====================================================================

(Alot example)[]
