library(fitdistrplus) # For fitting distributions
library(ggplot2) # For plotting

# Function to generate Q-Q plot for a specified distribution
qqplot_distribution <- function(data, distribution) {
  # Fit the specified distribution to the data
  fit <- fitdist(data, distribution)
  
  # Calculate theoretical quantiles
  theoretical_quantiles <- switch(
    distribution,
    "norm" = qnorm(ppoints(length(data)), mean = fit$estimate["mean"], sd = fit$estimate["sd"]),
    "lnorm" = qlnorm(ppoints(length(data)), meanlog = fit$estimate["meanlog"], sdlog = fit$estimate["sdlog"]),
    "exp" = qexp(ppoints(length(data)), rate = fit$estimate["rate"]),
    "beta" = qbeta(ppoints(length(data)), shape1 = fit$estimate["shape1"], shape2 = fit$estimate["shape2"]),
    "pois" = qpois(ppoints(length(data)), lambda = fit$estimate["lambda"]),
    stop("Unsupported distribution")
  )
  
  # Calculate empirical quantiles
  empirical_quantiles <- sort(data)
  
  # Create a data frame for the Q-Q plot
  qqplot_df <- data.frame(
    Theoretical = theoretical_quantiles,
    Empirical = empirical_quantiles
  )
  
  # Create the Q-Q plot
  ggplot(qqplot_df, aes(x = Theoretical, y = Empirical)) +
    geom_point() +
    geom_smooth(method = 'lm', color = 'black', linewidth = 0.5) +
    labs(title = paste("Q-Q Plot -", distribution),
         x = "Theoretical Quantiles",
         y = "Empirical Quantiles") +
    theme_minimal()
}

# Example usage:
# Generate some example data
set.seed(123)
example_data_normal <- rnorm(100, mean = 0, sd = 1)
example_data_lognormal <- rlnorm(100, meanlog = 0, sdlog = 1)

# Generate Q-Q plot for normal distribution
qqplot_distribution(example_data_normal, "norm")

# Generate Q-Q plot for lognormal distribution
qqplot_distribution(example_data_lognormal, "lnorm")
