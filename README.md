# Cardiovascular Risk Modeling — Framingham Heart Study

## Overview
Longitudinal survival analysis examining the contribution of diabetes 
and cholesterol levels to myocardial infarction and coronary heart 
disease (CHD) risk using data from the Framingham Heart Study cohort 
(n = 4,354).

## Methods
- Kaplan-Meier survival curves with log-rank testing
- Cox proportional hazards models (univariate and multivariable)
- Martingale residuals for linearity assessment
- Proportional hazards assumption testing
- Bonferroni correction for multiple comparisons

## Software
Stata

## Data
Data accessed through BioLINCC (Framingham Heart Study teaching 
dataset). Raw data not included per BioLINCC usage restrictions.

## Key variables
- Outcome: time to MI or coronary death (years since baseline)
- Primary exposure: diabetes status
- Secondary exposure: total cholesterol (per 10-unit increase, 
  centered at sample mean)
- Covariates: age, sex, education
