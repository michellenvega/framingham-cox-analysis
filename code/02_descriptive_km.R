library(haven)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(gtsummary)
library(webshot2)
library(broom)
library(gridExtra)
library(patchwork)


# load in the data

frmgham2_data <- read_dta("/Users/michellenavarretevega/Desktop/spring 2026/518a/framingham-cox-analysis/data/frmgham2.dta") %>%
mutate(
  diabetes = factor(as.numeric(diabetes), levels = c(0,1), 
                    labels = c("No Diabetes", "Diabetes")),
  sex      = factor(as.numeric(sex), levels = c(1,2), 
                    labels = c("Male", "Female")),
  educ     = factor(as.numeric(educ), levels = c(1,2,3,4),
                    labels = c("0-11 years", "High School/GED",
                               "Some College", "College Graduate+"))
)
head(frmgham2_data)

# ------------------------------------------------------------
# ---------------------------- 3A ----------------------------
# ------------------------------------------------------------

## create Table 1 stratified by diabetes

table1 <- frmgham2_data %>%
  select(age, totchol, sex, educ, diabetes) %>%
  tbl_summary(
    by = diabetes,
    label = list(
      age     ~ "Age (years)",
      totchol ~ "Total Cholesterol (mg/dL)",
      sex     ~ "Sex",
      educ    ~ "Education"
    ),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "ifany",
    missing_text = "Missing"
  )

getwd()

## check missing values
sum(is.na(frmgham2_data$lasttime))
sum(is.na(frmgham2_data$mi_fchd))
sum(is.na(frmgham2_data$diabetes))

table1 %>%
  as_gt() %>%
  gt::gtsave("output/tables/Table1.png", vwidth = 800)


frmgham2_data %>%
  select(age, totchol, sex, educ, diabetes) %>%
  group_by(diabetes) %>%
  summarise(
    age_mean      = mean(age, na.rm = TRUE),
    age_sd        = sd(age, na.rm = TRUE),
    chol_mean     = mean(totchol, na.rm = TRUE),
    chol_sd       = sd(totchol, na.rm = TRUE),
    n_male        = sum(sex == "Male", na.rm = TRUE),
    pct_male      = mean(sex == "Male", na.rm = TRUE) * 100,
    n_missing_chol = sum(is.na(totchol)),
    n_missing_educ = sum(is.na(educ))
  )

#   diabetes    age_mean age_sd chol_mean chol_sd n_male pct_male n_missing_chol
#   <fct>          <dbl>  <dbl>     <dbl>   <dbl>  <int>    <dbl>          <int>
# 1 No Diabetes     49.7   8.63      237.    44.3   1821     42.9             51
# 2 Diabetes        55.2   7.56      248.    56.3     57     50                1

# ------------------------------------------------------------
# ---------------------------- 3B ----------------------------
# ------------------------------------------------------------

# Fit KM
km_fit <- survfit(Surv(lasttime, mi_fchd) ~ diabetes, 
                  data = frmgham2_data,
                  conf.type = "log-log")

km_df <- tidy(km_fit) %>%
  mutate(strata = case_when(
    strata == "diabetes=No Diabetes" ~ "No Diabetes",
    strata == "diabetes=Diabetes"    ~ "Diabetes"
  ))

# Risk table data
risk_df <- summary(km_fit, times = c(0, 5, 10, 15, 20))$table

# Plot
# Main plot
p_curve <- ggplot(km_df, aes(x = time, y = estimate, 
                              color = strata, fill = strata)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.15, color = NA) +
  geom_step(linewidth = 0.8) +
  scale_color_manual(values = c("Diabetes" = "darkred", 
                                "No Diabetes" = "navy")) +
  scale_fill_manual(values  = c("Diabetes" = "darkred", 
                                "No Diabetes" = "navy")) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20), limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Kaplan-Meier Survival Curves by Diabetes Status",
    x = "Time (years)",
    y = "Survival Probability",
    color = NULL, fill = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position      = c(0.15, 0.2),
    legend.background    = element_rect(fill = "white", color = "grey80"),
    legend.key.width     = unit(1.5, "cm"),
    plot.title           = element_text(hjust = 0.5, size = 13),
    axis.title           = element_text(size = 11)
  )


# Risk table
risk_data <- summary(km_fit, times = c(0, 5, 10, 15, 20))
risk_df <- data.frame(
  time   = risk_data$time,
  n.risk = risk_data$n.risk,
  strata = ifelse(risk_data$strata == "diabetes=No Diabetes",
                  "No Diabetes", "Diabetes")
) %>%
  mutate(strata = factor(strata, levels = c("No Diabetes", "Diabetes")))

# Risk table plot
p_risk <- ggplot(risk_df, aes(x = time, y = strata, label = n.risk)) +
  geom_text(size = 3.5) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20), limits = c(0, 20)) +
  labs(x = NULL, y = NULL, title = "Number at Risk") +
  theme_classic(base_size = 11) +
  theme(
    axis.line        = element_blank(),
    axis.ticks       = element_blank(),
    axis.text.x      = element_blank(),
    panel.grid       = element_blank(),
    plot.title       = element_text(size = 10, face = "bold"),
    plot.margin      = margin(0, 5, 5, 5)
  )

# Combine
png("/Users/michellenavarretevega/Desktop/spring 2026/518a/framingham-cox-analysis/output/tables/3b_km_plot.png", 
    width = 900, height = 700, res = 120)
p_curve / p_risk + plot_layout(heights = c(4, 1))
dev.off()


# ------------------------------------------------------------
# ---------------------------- 3C ----------------------------
# ------------------------------------------------------------

# Median survival, survival estimates, tests

# (i) Median survival + 95% CI by group
summary(km_fit)$table

# (ii) 5-, 10-, 15-, 20-year survival + 95% CI by group
summary(km_fit, times = c(0, 5, 10, 15, 20))

# (iii) Test if survival differs at each timepoint
# Using Greenwood variance: Var(S(t)) = S(t)^2 * sum(d_j / n_j(n_j - d_j))

risk_times <- summary(km_fit, times = c(0, 5, 10, 15, 20))

surv_df <- data.frame(
  time   = risk_times$time,
  surv   = risk_times$surv,
  std.err = risk_times$std.err,  # Greenwood SE
  strata = ifelse(risk_times$strata == "diabetes=No Diabetes",
                  "No Diabetes", "Diabetes")
)

for (t in c(0, 5, 10, 15, 20)) {
  s0 <- surv_df %>% filter(strata == "No Diabetes", time == t)
  s1 <- surv_df %>% filter(strata == "Diabetes",    time == t)
  
  # Greenwood variance = SE^2
  var0 <- s0$std.err^2
  var1 <- s1$std.err^2
  
  # Z-test
  z <- (s0$surv - s1$surv) / sqrt(var0 + var1)
  p <- 2 * (1 - pnorm(abs(z)))
  
  cat(sprintf("t = %d years: S_nodiab = %.4f, S_diab = %.4f, z = %.3f, p = %.4f (%s)\n",
              t, s0$surv, s1$surv, z, p,
              ifelse(p < 0.01, "Reject H0", "Fail to reject H0")))
}



## for stata code -> need to correct 3c! the way of calculating it is wrong. need to follow the steps from lecture

