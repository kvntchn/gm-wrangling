---
title: "Readme"
subtitle: "GM-UAW Cohort Study"
author: "Kevin Chen"
date: "July 2, 2020"
---

This repository contains the relevant code for loading and reshaping the UAW-GM Cohort data in the `R` programming environment. The main scripts are found in the directory named `wrangling`. Sourcing `00-hello.R` will source all relevant scripts in the directory in the correct order.

Once sourced, the user will have access to the original cohort data `dta`, a cleaned up version `cohort`, and all the necessary functions for setting up a "long" analytic file for common epidemiologic analyses. The function `get.cohort_analytic()` returns an analytic dataset starting at hire and ending with death. Use flags such as `right.censored` and `immortal` to select the correct rows, as deemed appropriate.
