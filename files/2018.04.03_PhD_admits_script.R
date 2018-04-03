# Required libraries
library(tidyverse)
library(ggthemes)
library(scales)
library(Cairo)

# per application cycle how many times do you get admitted?
admissions_run <- function(rejection_rate = 0.8, admission_attempts = 20) {
  sum(runif(admission_attempts) >= rejection_rate)
}

# overall function for running admissions scenarios, plotting a histogram, and printing summary statistics
admissions_scenario <- function(rejection_rate = 0.8, admission_attempts = 20, scenario_runs = 1000, create_pic = TRUE) {
  # call the admissions_run function the appropriate number of times
  results <- tibble(admits = replicate(scenario_runs, admissions_run(rejection_rate, admission_attempts)))
  # Theme setting - I chose the economist theme
  theme_set(theme_economist(base_size = 10, base_family = "Proxima Nova", dkpanel = TRUE) +
              theme(axis.text =  element_text(size = 12), axis.title = element_text(size = 14, face = "bold")))
  # prettifying function to get rid of decimal places
  x_labels_function <- function(x) sprintf("%.0f", x)
  # histogram creation (I manipulate the bins to make them have the normal visual width)
  results_plot <- ggplot(results, aes(x = admits)) +
    geom_histogram(aes(y = (..count..)/sum(..count..)), bins = (2*(max(results$admits) + 1)-1)) + 
    scale_y_continuous(name = "Likelihood", labels=scales::percent) +
    ggtitle(paste("Admissions distribution with", percent(rejection_rate),
                  "rejection rate,", admission_attempts, "apps,", "&", scenario_runs, "runs")) +
    scale_x_continuous(name = "Admissions", labels = x_labels_function, breaks = seq(0, max(results$admits)))
  print(results_plot)
  # create a memento of the run (or not!)
  if (create_pic) {
    ggsave(results_plot, filename = "admissions_run.png", dpi = 200, width = 7.5, height = 6, type = "cairo",  units = "in")
  }
  # return some summary statistics
  results %>% summary() %>% print()
}

# Function that creates a tibble of outcome names and associated probability
outcome_tibble_creator <- function(outcomes = c("UC Irvine", "Brandeis", "UT Austin", "BU", "Normie"),
                                   probability = c(.99, 0.00, 0.00, 0.00, 0.01)) {
  if (length(outcomes) == length(probability)) {
    return(outcomes_tibble <- tibble(outcomes, probability))
  } else {print("Match your outcomes and probabilities you stupid fucker")}
}

# Function that takes in an outcome name and associated probability tibble, and produces outcome histogram
outcomes_scenario <- function(outcomes_tibble = outcome_tibble_creator()) {
  # Theme setting - I chose the economist theme
  theme_set(theme_economist(base_size = 8, base_family = "Proxima Nova", dkpanel = TRUE) +
              theme(axis.title = element_text(size = 11, face = "bold")) + 
              theme(plot.title = element_text(hjust = 0.5)))
  # prettifying function to get rid of decimal places
  x_labels_function <- function(x) sprintf("%.0f", x)
  # histogram creation (I manipulate the bins to make them have the normal visual width)
  outcomes_plot <- ggplot(outcomes_tibble, aes(x = outcomes, y = probability)) +
    geom_bar(stat = "identity") + 
    scale_y_continuous(name = "Likelihood", labels=scales::percent) +
    ggtitle("PhD or Normie?") +
    scale_x_discrete(name = "Outcomes")
  print(outcomes_plot)
  # create a memento of the run 
  ggsave(outcomes_plot, filename = "outcomes.png", dpi = 300, width = 4, height = 3, type = "cairo",  units = "in")
}

# simple model that just uses admits and denies so far to extrapolate to remaining decisions
naive_admit_estimation <- function(admission_attempts = 19, admits = 4, denies = 15,begin_date = as.Date("2018/01/20"),
                                   today = as.Date(Sys.Date()), end_date = as.Date("2018/03/20")) {
  # timespan calculations
  timespan = as.numeric(difftime(end_date, begin_date, units = c("days")))
  elapsed_span = as.numeric(difftime(today, begin_date, units = c("days")))
  # decision pace checking
  if ((elapsed_span/timespan - (admits+denies)/admission_attempts) > 0) {
    print("Hmm, you ought to have heard back from more places")
  } else {
    print("Decisions pace is on track...")
  }
  # future admits and denies tracking
  future_admits <- floor((admission_attempts - (admits+denies)) * admits/(admits+denies))
  future_denies <- admission_attempts - (future_admits + admits + denies)
  print(paste("And the naive expectation is that you'll get admitted to", future_admits, "more PhD program(s)"))
  print(paste("And that you'll get denied to", future_denies, "more PhD program(s)"))
}

# runs the distribution scenario and the naive model functions
admissions_scenario()
outcomes_scenario()
naive_admit_estimation()