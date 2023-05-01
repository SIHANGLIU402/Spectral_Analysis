#measles cases in the UK in the pre-vaccine era
#install.packages("tsutils")
library(tsutils)
library(stats)
library(tseries)
meas <- read.table("https://web.stanford.edu/class/earthsys214/data/nycmeas.dat.txt", header=FALSE)
dimnames(meas)[[2]] <- c("Date","Cases")
meas_ts <- ts(meas$Cases, start = c(1928, 1), frequency = 12)
# Plot the time series
plot(meas_ts, main = "Measles Cases (1928-1963)", ylab = "Cases")

# Perform the ADF test
adf_test <- adf.test(meas_ts)
# Print the test results
print(adf_test)

# Plot the decomposed components
par(mfrow=c(4,1))
plot(decomp$seasonal, main="Seasonal Component")
plot(decomp$trend, main="Trend Component")
plot(decomp$random, main="Random Component")
plot(decomp$x, main="Original Time Series")

par(mfrow=c(2,1))
# Create an autocorrelation plot
acf(meas_ts, main = "Autocorrelation Plot of Measles Cases")
# Create a partial autocorrelation plot
pacf(meas_ts, main = "Partial Autocorrelation Plot of Measles Cases")


# Calculate the periodogram
meas_periodogram <- spec.pgram(meas_ts)
# Plot the periodogram
plot(meas_periodogram, main = "Periodogram of Measles Cases")
kernel("modified.daniell",2)
kernel("modified.daniell",c(1,1))

# Compute the spectral density using the spectrum function
mspect <- spectrum(meas$Cases, log="no", spans=c(2,2), plot=FALSE)
# Set the sampling interval
delta <- 1/12
# Compute the frequency and power spectra
specx <- mspect$freq/delta
specy <- 2*mspect$spec
# Plot the power spectrum
plot(specx, specy, xlab="Period (years)", ylab="Spectral Density", type="l")

#Frequency Analysis
# Find the dominant frequencies
dominant_freq <- meas_periodogram$freq[order(meas_periodogram$spec, decreasing = TRUE)[1:k]]
# Convert frequencies to periods
dominant_periods <- 1 / (dominant_freq * 12)
print(dominant_periods)









# Create a design matrix with harmonic terms
time <- 1:length(meas_ts)
n_harmonics <- 4  # number of harmonics to include
harmonics <- sapply(1:n_harmonics, function(i) sin(2 * pi * i * time / 12))
design_matrix <- cbind(1, harmonics)
# Fit linear model with the design matrix
fit <- lm(meas_ts ~ design_matrix)
# Create a time series object of the fitted values
fitted_ts <- ts(fit$fitted.values, start = c(1928, 1), frequency = 12)
# Plot the time series and fitted values
plot(meas_ts, main = "Measles Cases (1928-1963)", ylab = "Cases")
lines(fitted_ts, col = "red")



