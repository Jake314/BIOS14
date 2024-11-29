# Question: What is the effect of land use heterogeneity, taking altitude into account?

rm(list = ls())
library(MASS)

# This function takes a vector of values and translates them to a colour ranging from blue to red
colourscale <- function(vals) {
    result <- c()
    if (min(vals) < 0) vals <- vals - min(vals)
    for (val in vals) result <- c(result, rgb(val, 0, max(vals) - val, maxColorValue = max(vals)))
    return(result)
}

# Import data
dat = read.csv("Data/Eulaema.csv")
dat$mcaltitude <- dat$altitude - mean(dat$altitude)

# 'plotingx'/'pseudox', used for producing the model prediction lines
px <- seq(0, 1.5, length.out = 1000)

# Plots, very simply, land use vs. bee count
plot(unlist(dat$lu_het, use.names = F), unlist(dat$Eulaema_nigrita, use.names = F),
     xlab = "Land Use Heterogeneity (Shannon Diversity)", ylab = "Number of Bees", main = "Land Use Heterogeneity vs. Number of Bees",
     pch = 16)

# Gets summary statistics for bees = b0 + b1*lut_het + E (E normal)
m <- lm(Eulaema_nigrita ~ lu_het, data = dat)

hist(m$residuals, breaks = 20, xlab = "Bee Count Residual",
     main = "Residuals from Simple Land Use Heterogeneity Prediction")
# Notice that 1Q and 3Q in residuals are uneven (non-normal)

# Gets summary statistics for log(bees) = b0 + b1*lu_het + E (E nonnormal)
m2 <- glm.nb(Eulaema_nigrita ~ lu_het, data = dat)

# This includes the mean-centred altitude in the model
m3 = glm.nb(Eulaema_nigrita ~ lu_het + mcaltitude, data = dat)

# Plots land use vs. bee count including colouring for altitude
# The following 'layout' and 'parameters' are to set the plot up to have a gradient legend on the right
layout(matrix(1:2, nrow = 1), widths = c(0.8, 0.2))
op <- par(mar = c(5.1, 4.1, 4.1, 2.1))
plot(unlist(dat$lu_het, use.names = F), unlist(dat$Eulaema_nigrita, use.names = F),
     xlab = "Land Use Heterogeneity (Shannon Diversity)", ylab = "Number of Bees", main = "Land Use Heterogeneity vs. Number of Bees",
     pch = 16, col = colourscale(unlist(dat$altitude, use.names = F)))
legend("topleft",
       c("Altitude = Mean", "Altitude = Mean + SD", "Altitude = Mean - SD"),
       col = c("black", "lightgrey", "lightgrey"),
       lty = 1:3,
       cex = 0.7
)
# Plot 3 lines, one for altitude = mean, altitude = mean + sd, altitude = mean - sd
alt_dev <- 0.0002730
lines(px, exp(m3$coef[1] + m3$coef[2] * px + m3$coef[3] + 0))
lines(px, exp(m3$coef[1] + m3$coef[2] * px + m3$coef[3] + alt_dev), lty = 2, col = "lightgrey")
lines(px, exp(m3$coef[1] + m3$coef[2] * px + m3$coef[3] - alt_dev), lty = 3, col = "lightgrey")
# This piece will add a gradient legend to the right of the plot
# This method specifically was written by user @thelatemail on stackoverflow and it can be found at
# https://stackoverflow.com/questions/13355176/gradient-legend-in-base
par(mar = c(5.1, 0.5, 4.1, 4))
plot(NA, type = "n", ann = FALSE, xlim = c(1,1.5), ylim = c(1,2), xaxt = "n", yaxt = "n", bty = "n")
gradient <- colorRampPalette(c("blue","red"))
xl <- 1
yb <- 1
xr <- 1.5
yt <- 2
rect(
    xl,
    head(seq(yb, yt, (yt - yb)/10), -1),
    xr,
    tail(seq(yb, yt, (yt - yb)/10), -1),
    col = gradient(10)
)
mtext(round(seq(min(dat$mcaltitude), max(dat$mcaltitude), length.out = 11), -2), side = 2, at = seq(yb, yt, length.out = 11),
     cex = 0.6, las = 1)
mtext("Deviation from Mean Altitude (m)", side = 4, cex = 0.7, las = 3)

# Plots land use vs. log(bee count). Identical to previous plot but y-axis is log(y)
layout(matrix(1:2, nrow = 1), widths = c(0.8, 0.2))
par(mar = c(5.1, 4.1, 4.1, 2.1))
plot(unlist(dat$lu_het, use.names = F), log(unlist(dat$Eulaema_nigrita, use.names = F)),
     xlab = "Land Use Heterogeneity (Shannon Diversity)", ylab = "Ln(Number of Bees)", main = "Land Use Heterogeneity vs. Logged Abundance",
     pch = 16, col = colourscale(unlist(dat$altitude, use.names = F)))

# Plot 3 lines, one for altitude = mean, altitude = mean + sd, altitude = mean - sd
lines(px, m3$coef[1] + m3$coef[2] * px + m3$coef[3] + 0)
lines(px, m3$coef[1] + m3$coef[2] * px + m3$coef[3] + alt_dev, lty = 2, col = "lightgrey")
lines(px, m3$coef[1] + m3$coef[2] * px + m3$coef[3] - alt_dev, lty = 3, col = "lightgrey")

par(mar = c(5.1, 0.5, 4.1, 4))
plot(NA, type = "n", ann = FALSE, xlim = c(1,1.5), ylim = c(1,2), xaxt = "n", yaxt = "n", bty = "n")
gradient <- colorRampPalette(c("blue","red"))
xl <- 1
yb <- 1
xr <- 1.5
yt <- 2
rect(
    xl,
    head(seq(yb, yt, (yt - yb)/10), -1),
    xr,
    tail(seq(yb, yt, (yt - yb)/10), -1),
    col = gradient(10)
)
mtext(round(seq(min(dat$mcaltitude), max(dat$mcaltitude), length.out = 11), -2), side = 2, at = seq(yb, yt, length.out = 11),
      cex = 0.6, las = 1)
mtext("Deviation from Mean Altitude (m)", side = 4, cex = 0.7, las = 3)
