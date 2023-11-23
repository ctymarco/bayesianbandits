
# test get_sampling_data --------------------------------------------------------

sampling_data <-
  get_sampling_data(
    num_periods = 100,
    beta = 0.9,
    lambda = 2,
    gamma = 0.7,
    cost = 0.5,
    data_size = 144
  ) 

## data size = 144, started - 11:52, ends - 11:54, 14400 obs.

# get entropy -------------------------------------------------------------

get_entropy(
  sampling_data$choice[1:10]
)

get_entropy(
  sampling_data$choice[1:90]
)


# correlations ------------------------------------------------------------

get_correlation(
  num_periods = 50,
  beta = 0.9,
  lambda = 2,
  gamma = 0.7,
  cost = 0.5
)

View(
  get_sampling_data(
    num_periods = 50,
    beta = 0.9,
    lambda = 2,
    gamma = 0.7,
    cost = 0.5
  )
)

get_correlation(
  num_periods = 50,
  beta = 0.5,
  lambda = 9,
  gamma = 0.7,
  cost = 0.5
)

View(
  get_sampling_data(
    num_periods = 50,
    beta = 0.5,
    lambda = 9,
    gamma = 0.7,
    cost = 0.5
  )
)

View(
  sampling_data %>%
    dplyr::filter(
      sim == 
        4
    )
)

get_correlation(
  num_periods = 50,
  beta = 0.9,
  lambda = 5,
  gamma = 0.5,
  cost = 0.5,
  data_size = nrow(beta_df),
  5
)
