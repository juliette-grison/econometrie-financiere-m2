# Estimer la volatilité du titre avec les 8 modèles suivants : Riskmetrics, GARCH, 
# IGARCH et GJR et les 2 distributions : Normale et Student  
# Résultats sous forme de tableau.  
# Identifier les modèles valides (contraintes de stationnarité et de positivité 
# ainsi que la significativité des variables).  
#Commenter uniquement le meilleur modèle sur l’ensemble des modèles validés des 2 
#distributions (modélisation de la volatilité, paramètres, résidus …) 

library(quantmod)
library(PerformanceAnalytics)
library(robustbase)
library(rugarch)

# Riskmetrics
## Normale
y = crGSPC
spec = ugarchspec(variance.model=list(model = "iGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), fixed.pars=list(omega=0,alpha1=0.06,beta1=0.94))
mod = ugarchfit(data = y, spec = spec)
mod

# Conditionnal variance
return_var <- xts(mod@fit$var, order.by = as.Date(index(rGSPC)))

# Graphique des rentabilités au carré et de la variance conditionnelle estimée
par(mfrow=c(2,1))
plot.xts(r2GSPC,legend.loc = "top", main = "Rentabilités au carré du SP500", col = rainbow(4))
plot.xts(return_var,legend.loc = "top", main = "Variance conditionnelle du modèle Riskmetrics", col = "blue")

## Student

# GARCH
## Normale
y = crGSPC
spec = ugarchspec(variance.model=list(model = "sGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE))
mod = ugarchfit(data = y, spec = spec)
mod

# Compute persistence
pers = persistence(mod)
show(pers)

# Compute half-life
hl = halflife(mod)
show(hl)

# Conditionnal variance
return_var <- xts(mod@fit$var, order.by = as.Date(index(rGSPC)))
plot(return_var, main = "Variance conditionnelle du modèle GARCH", col = "blue")

# Graphique des rentabilités au carré et de la variance conditionnelle estimée
par(mfrow=c(2,1))
plot.xts(r2GSPC,legend.loc = "top", main = "Rentabilités au carré du SP500", col = rainbow(4))
plot.xts(return_var,legend.loc = "top", main = "Variance conditionnelle du modèle GARCH", col = "blue")


## Student
spec = ugarchspec(variance.model=list(model = "sGARCH"),mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model = "std")
fit = ugarchfit(data = y, spec = spec)
fit

# IGARCH
## Normale
y = crGSPC
spec = ugarchspec(variance.model=list(model = "iGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE))
mod = ugarchfit(data = y, spec = spec)
mod

# Conditionnal variance
return_var <- xts(mod@fit$var, order.by = as.Date(index(rGSPC)))

# Graphique des rentabilités au carré et de la variance conditionnelle estimée
par(mfrow=c(2,1))
plot.xts(r2GSPC,legend.loc = "top", main = "Rentabilités au carré du SP500", col = rainbow(4))
plot.xts(return_var,legend.loc = "top", main = "Variance conditionnelle du modèle IGARCH", col = "blue")

## Student

# GJR
## Normale
y = crGSPC
spec = ugarchspec(variance.model=list(model = "gjrGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE))
mod = ugarchfit(data = y, spec = spec)
mod

# Conditionnal variance
return_var <- xts(mod@fit$var, order.by = as.Date(index(rGSPC)))

# Graphique des rentabilités au carré et de la variance conditionnelle estimée
par(mfrow=c(2,1))
plot.xts(r2GSPC,legend.loc = "top", main = "Rentabilités au carré du SP500", col = rainbow(4))
plot.xts(return_var,legend.loc = "top", main = "Variance conditionnelle du modèle GJR(1,1)", col = "blue")

## Student