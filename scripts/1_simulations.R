rm(list=ls())

# load packages -----------------------------------------------------------

library(devtools)
devtools::install_github('Nth-iteration-labs/contextual')
library(contextual)
library(tidyverse)

library(Hmisc)
library(desc)

(.packages())



# simulation --------------------------------------------------------------

num_periods = 50
num_sims = 144

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
      num_sims,
    do_parallel =
      TRUE
  )

history <-
  simulator$run()

# data frame --------------------------------------------------------------
## parameters --------------------------------------------------------------
cost = 0.5
beta = 0.9
lambda = 2
gamma = 0.7
cost = 0.5

sampling_data <-
  history$data %>%
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
  dplyr::group_by(
    sim
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
      utility(profit)
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
  ) %>%
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
    cum_reward,
    cum_search_cost,
    cum_profit,
    discount_factor,
    regret,
    cum_regret_rate,
    everything()
  )


# plots -------------------------------------------------------------------

plot(
  history,
  type = "average",
  regret = FALSE,
  lwd = 2,
  legend_position = "bottomright"
)

plot(
  history,
  type = "cumulative",
  regret = "FALSE",
  rate = TRUE,
  lwd = 2
)

plot(
  history,
  type = "arms"
)

cum_u_plot <-
  ggplot(
    sampling_data %>% filter(sim == 1),
    aes(
      x = t,
      y = cum_u
    ) 
  ) +
  geom_line() +
  theme_classic() +
  labs(
    x = 
      "t",
    y = 
      "Cumulated Discounted Utility"
  )

ggplot(
  sampling_data %>% filter(sim == 1),
  aes(
    x = t,
    y = exp(cum_u)
  ) 
) +
  geom_line() +
  theme_classic() +
  labs(
    x = 
      "t",
    y = 
      "Cumulated Discounted Utility"
  )

