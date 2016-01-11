## mobilite.R
## Application CFA/SEM (Beaujean, 2014, exercice 3.2)
##

library(lavaan)

## saisie des données
mobility.cov <- c(0.77, 0.38, 0.65, 0.39, 0.39, 0.62,  -0.25,  -0.32,  -0.27, 6.09, 0.31,
                  0.29, 0.26,  -0.36, 7.67, 0.24, 0.25, 0.19, -0.18, 0.51, 1.69, -3.16,  -3.56,  -2.63, 6.09,
                  -3.12,  -4.58, 204.79,  -0.92,  -0.88, -0.72, 0.88,  -1.49,  -1.41,  16.53, 7.24)
mobility.cov <- lower2full(mobility.cov)
rownames(mobility.cov) <- colnames(mobility.cov) <- c("Dep.1", "Dep.2", "Dep.3", "SocActivity",
                                                      "Falls", "Chronic", "TotActivity", "PersMobility")

## modèle structurel + mesure
mobility.model <- '
PsychSocLV =~ Dep.1 + Dep.2 + Dep.3 + SocActivity
PsyHealthLV =~ Falls + Chronic + TotActivity
PersMobility ~ PsychSocLV + PsyHealthLV
'
mobility.fit <- sem(mobility.model, sample.cov = mobility.cov, sample.nobs = 6053)
summary(mobility.fit, fit.measures = TRUE, standardized = TRUE)