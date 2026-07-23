# R script to simulate the segmented relationship from Figure 4
# (River Right, 100% ALAN intensity treatment)

# 1. Simulate x values (Minutes past sunset)
x_val <- seq(-50, 300, by = 10)

# 2. Simulate y values (Relative Rainbow Trout Density)
# Based on Figure 4: Breakpoint 1 at ~0 min, Breakpoint 2 at ~60 min
# Density starts near 0.1, rises exponentially to ~1.2 at 60 min, and stabilizes.
y_val <- ifelse(x_val < 0, 0.1,
         ifelse(x_val <= 60, 0.1 + (1.1 / 60) * x_val,
                1.2))

# 3. Add quantifiable uncertainty (95% CI spread estimated from figure)
# Confidence intervals are narrowest prior to sunset and widen after twilight.
ci_spread <- ifelse(x_val < 0, 0.05,
             ifelse(x_val <= 60, 0.05 + (0.25 / 60) * x_val,
                    0.3))

lower_limit <- pmax(0, y_val - ci_spread)
upper_limit <- y_val + ci_spread

# 4. Create dataframe following 'Extracted Data' metadata guidance
df <- data.frame(
  curve.id = rep("RR_100_ALAN", length(x_val)),
  stressor.label = rep("minutes_past_sunset", length(x_val)),
  stressor.x = x_val,
  units.x = rep("minutes", length(x_val)),
  response.label = rep("relative_rainbow_trout_density", length(x_val)),
  response.y = round(y_val, 3),
  units.y = rep("unitless", length(x_val)),
  stressor.value = rep("100_percent_intensity", length(x_val)),
  lower.limit = round(lower_limit, 3),
  upper.limit = round(upper_limit, 3),
  plot.type = rep("curve", length(x_val))
)

# 5. Save to a machine-readable CSV for the e-library
write.csv(df, "Nelson_2022_Fig4_Simulated.csv", row.names = FALSE)

# 6. Plot the simulated figure to verify
plot(df$stressor.x, df$response.y, type = "l", col = "blue", lwd = 2,
     ylim = c(0, 2), xlab = "Minutes Past Sunset",
     ylab = "Relative Rainbow Trout Density",
     main = "Simulated Figure 4: River Right (100% ALAN)")
lines(df$stressor.x, df$lower.limit, lty = 2, col = "lightblue")
lines(df$stressor.x, df$upper.limit, lty = 2, col = "lightblue")
