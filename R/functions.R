
# load packages -----------------------------------------------------------

library(contextual)
library(tidyverse)
library(Hmisc)

# shannon entropy function ------------------------------------------------

get_entropy <- function(target) {
  freq <- table(target)/length(target)
  # vectorize
  vec <- as.data.frame(freq)[,2]
  #drop 0 to avoid NaN resulting from log2
  vec<-vec[vec>0]
  #compute entropy
  -sum(vec * log2(vec))
}


# utility  ----------------------------------------------------------------

utility <- function(x){
  ifelse(
    x < 0,
    (-lambda*((-1*(10*x))^gamma)),
    ((10*x)^gamma)
  )
}


# get_sampling_data -------------------------------------------------------

get_sampling_data <- function(num_periods, beta, lambda, gamma, cost, data_size){

  utility <- function(x){
    ifelse(
      x < 0,
      (-lambda*((-1*(10*x))^gamma)),
      ((10*x)^gamma)
    )
  }

  bandit <-
    contextual::BasicBernoulliBandit$new(
      weights =
        c(
          0.4,
          0.5,
          0.6,
          0.7
        )
    )

  policy <-
    contextual::ThompsonSamplingPolicy$new(
      alpha = 1,
      beta = 1
    )

  agent <-
    contextual::Agent$new(
      policy,
      bandit
    )

  simulator <-
    contextual::Simulator$new(
      agent,
      horizon =
        num_periods, # time periods
      simulations =
        data_size,
      do_parallel =
        TRUE
    )

  history <-
    simulator$run()

  sampling_data <-
    history$data %>%
    dplyr::group_by(
      sim
    ) %>%
    dplyr::arrange(
      .,
      sim
    ) %>%
    dplyr::mutate(
      search_cost =
        ifelse(
          choice ==
            Hmisc::Lag(choice, 1),
          0,
          cost
        )
    ) %>%
    dplyr::mutate(
      search_cost =
        replace_na(
          search_cost,
          cost
        )
    ) %>%
    dplyr::mutate(
      cum_search_cost =
        cumsum(
          search_cost
        )
    ) %>%
    dplyr::mutate(
      profit =
        reward - search_cost
    ) %>%
    dplyr::mutate(
      cum_profit =
        cum_reward-cum_search_cost
    ) %>%
    dplyr::mutate(
      discount_factor =
        beta^t
    ) %>%
    dplyr::mutate(
      u =
        utility(
          profit
        )
    ) %>%
    dplyr::mutate(
      discounted_u =
        (beta^t)*u
    ) %>%
    dplyr::mutate(
      cum_u =
        cumsum(
          discounted_u
        )
    )

  for(i in 1:nrow(sampling_data)){

    sampling_data$cum_entropy[i] <-
      get_entropy(
        sampling_data$choice[1:i]
      )
  }

  sampling_data <-
    sampling_data %>%
    dplyr::select(
      sim,
      t,
      k,
      choice,
      reward,
      search_cost,
      profit,
      u,
      discounted_u,
      cum_u,
      cum_entropy,
      cum_reward,
      cum_search_cost,
      cum_profit,
      discount_factor,
      regret,
      cum_regret_rate,
      everything()
    )

  return(
    sampling_data
  )
}

# get_correlation ---------------------------------------------------------

get_correlation <-
  function(num_periods, beta, lambda, gamma, cost, data_size, sim_n){

    sampling_data <-
      get_sampling_data(num_periods, beta, lambda, gamma, cost, data_size) %>%
      dplyr::filter(
        sim ==
          sim_n
      )

    return(
      cor(
        sampling_data$cum_entropy,
        sampling_data$cum_profit
      )
    )
  }

