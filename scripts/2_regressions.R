

# load packages -----------------------------------------------------------

library(tidyverse)
library(readr)
library(stargazer)


# set directory -----------------------------------------------------------

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dirname(rstudioapi::getSourceEditorContext()$path)


# regression --------------------------------------------------------------

## regressors --------------------------------------------------------------

### for 529 combinations; lambda by = 0.4, beta by = 0.02
### for 2116 combinations; lambda by = 0.2, beta by = 0.01

lambda_values <-
  seq(
    1,10,
    by = (0.8/2) #0.2
  )

beta_values <-
  seq(
    0.5,0.95,
    by = (0.04/2) #0.01
  )


lambda_df <-
  lambda_values %>%
  rep(
    .,
    length(lambda_values)
  ) %>%
  as.data.frame() %>%
  dplyr::rename(
    lambda =
      colnames(.)[1]
  )

beta_df <-
  beta_values %>%
  rep(
    .,
    each = length(lambda_values)
  ) %>%
  as.data.frame() %>%
  dplyr::rename(
    beta =
      colnames(.)[1]
  )

reg_df <-
  cbind(
    beta_df,
    lambda_df
  ) %>%
  dplyr::mutate(
    num_periods =
      50,
    gamma = 
      0.7,
    cost =
      0.5
  ) %>%
  tibble::rowid_to_column("id") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    correlation =
      get_correlation(
        num_periods,
        beta,
        lambda,
        gamma,
        cost,
        nrow(beta_df),
        id
      )
  )
# 144 rows
# starts: 12:06
# ends: 13:09

# 529 rows
# starts: 13:17
# ends: 

# run regression ----------------------------------------------------------

linear_regression <-
  lm(
    correlation ~ beta + lambda,
    data = reg_df
  )

stargazer(
  linear_regression,
  type = 
    "text"
)

stargazer(
  linear_regression
)

# print dataframe ---------------------------------------------------------

write_excel_csv(
  reg_df,
  file =
    "144_reg_df.csv"
)


# read dataframe ----------------------------------------------------------

reg_df <- read.csv(
  "../144_reg_df.csv"
)
