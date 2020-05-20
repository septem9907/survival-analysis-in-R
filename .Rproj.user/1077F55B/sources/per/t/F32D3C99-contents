data(GBSG2, package = "TH.data")

# Compute Cox model
library(survival)
cxmod <- coxph(Surv(time, cens) ~ horTh + tsize, data = GBSG2)

# Imaginary patients
newdat <- expand.grid(
  horTh = levels(GBSG2$horTh),
  tsize = quantile(GBSG2$tsize, probs = c(0.25, 0.5, 0.75))
)

rownames(newdat) <- letters[1:6]

# Compute survival curves
cxsf <- survfit(cxmod, data = GBSG2, newdata = newdat)

head(cxsf$surv)

# Compute data.frame needed for plotting
library(survminer)

surv_cxmod0 <- surv_summary(cxsf)

head(surv_cxmod0)

# Get a character vector of patient letters (patient IDs)
pid <- as.character(surv_cxmod0$strata)

# Multiple of the rows in newdat so that it fits with surv_cxmod0
m_newdat <- newdat[pid, ]

# Add patient info to data.frame
surv_cxmod <- cbind(surv_cxmod0, m_newdat)
head(surv_cxmod)

# Plot
ggsurvplot_df(surv_cxmod, linetype = "horTh", color = "tsize",
           legend.title = NULL, censor = FALSE)