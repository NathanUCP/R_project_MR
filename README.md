## Asymmetric Mean Reversion in Low Liquid Markets: Evidence from BRVM

This web page give access to the R script that have been used for the article : _**Asymmetric Mean Reversion in Low Liquid Markets: Evidence from BRVM**_.

The article can be donwload at the following [link]( https://www.mdpi.com/1911-8074/12/1/38). The artcile used GARCH methods based on the R package [_rugarch_](https://cran.r-project.org/web/packages/rugarch/index.html).

### R script
The full R script is given in the file [**Main program**](https://github.com/NathanUCP/R_project_MR/blob/master/Main_prog.R). Here, we describe a part of that program. We focus on the part that estimate the model of the paper. The main steps is estimating a GARCH model using this package is done in two steps which are :

```markdownblock
Definition of the model : use of **ugarchspec function**

spec <- rugarch::ugarchspec(variance.model = list(model = "eGARCH", 
                                         garchOrder = c(1, 1), 
                                         submodel = NULL, 
                                         external.regressors = NULL  , 
                                         variance.targeting = FALSE), 
                    mean.model     = list(armaOrder = c(1, 1), 
                                          external.regressors = lag.return, 
                                          distribution.model="norm", 
                                          start.pars = list(), 
                                          fixed.pars = list()))

There is two parts, the mean equation and the variance equation. Once the model have been specified, the estimation is done using the function **ugarchfit** :

garch <- ugarchfit(spec = spec, data = data.frame(dep), solver.control = list(trace=0))
```
For more details see [the manual of the package](https://cran.r-project.org/web/packages/rugarch/rugarch.pdf).

### Contact

Having trouble with the program? you can send me an email (ngbenro@gmail.com).
