# Example data
group1 <- rpois(50, lambda = 5) # Counts for group 1
group2 <- rpois(50, lambda = 7) # Counts for group 2

# Create a data frame
data <- data.frame(
  counts = c(group1, group2),
  group = factor(rep(c("Group1", "Group2"), each = 50))
)

# Fit a Poisson regression model
model <- glm(counts ~ group, family = poisson(link = "log"), data = data)


# Summary of the model
summary(model)

# Perform ANOVA to compare groups
anova(model, test = "Chisq")
