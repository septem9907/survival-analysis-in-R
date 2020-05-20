data(lung)

# Compute Cox model and survival curves
library(survival)
cxmod <- coxph(Surv(time, status) ~ ph.karno, data = lung)
new_lung <- data.frame(ph.karno = c(60, 70, 80, 90))
cxsf <- survfit(cxmod, data = lung, newdata = new_lung)

# Use the summary of cxsf to take a vector of patient IDs
library(survminer)
surv_cxmod0 <- surv_summary(cxsf)
pid <- as.character(surv_cxmod0$strata)

# Duplicate rows in newdat to fit with surv_cxmod0 and add them in
m_newdat <- new_lung[pid, , drop = FALSE]
surv_cxmod <- cbind(surv_cxmod0, m_newdat)

# Plot
ggsurvplot_df(surv_cxmod, color = "ph.karno", legend.title = NULL, censor = FALSE)

# Compute Kaplan-Meier curve
km <- survfit(Surv(time, status) ~ 1, data = lung)

# Plot Kaplan-Meier curve
ggsurvplot(km, conf.int = FALSE)

# Plot Cox model survival
ggsurvplot_df(surv_cxmod, censor = FALSE)