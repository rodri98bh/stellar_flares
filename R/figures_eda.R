
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(stats)
library(TSA)

# Load the dataset
df <- read.csv("Data_brightness.csv")
df$pdcsap_flux <- as.numeric(df$pdcsap_flux)
df <- df[order(df$time), ]

# Select relevant columns
df <- df %>%
  select(time, pdcsap_flux) %>%
  arrange(time)

# Time Series Plot
ggplot(df, aes(x = time, y = pdcsap_flux)) +
  geom_line(color = "blue", alpha = 0.7) +
  labs(title = "",
       x = "Time (Days)", y = "Brightness (Flux)") +
  theme_minimal()

# Rolling Mean and Standard Deviation
window_size <- 50

df <- df %>%
  mutate(
    rolling_mean = zoo::rollmean(pdcsap_flux, k = window_size, fill = NA, align = "right"),
    rolling_std = zoo::rollapply(pdcsap_flux, width = window_size, FUN = sd, fill = NA, align = "right")
  )

# Remove NA values before plotting
df_clean <- df %>% drop_na(rolling_mean, rolling_std)

# Plot Rolling Mean and Standard Deviation
ggplot(df_clean, aes(x = time)) +
  geom_line(aes(y = pdcsap_flux), color = "blue", alpha = 0.5) +
  geom_line(aes(y = rolling_mean), color = "red", size = 1) +
  geom_ribbon(aes(ymin = rolling_mean - rolling_std, ymax = rolling_mean + rolling_std), 
              fill = "gray", alpha = 0.3) +
  labs(title = "",
       x = "Time (Days)", y = "Brightness (Flux)") +
  theme_minimal()

# Fourier Transform (Power Spectrum)
df$pdcsap_flux <- df$pdcsap_flux - mean(df$pdcsap_flux, na.rm = TRUE)

# Calculate the sampling frequency (fs) as in Python
time_diff <- diff(df$time)
fs <- 1 / median(time_diff, na.rm = TRUE)  # Sampling frequency

# Apply FFT
fft_values <- fft(df$pdcsap_flux)
n <- length(fft_values)

# Compute frequencies (matching Python np.fft.fftfreq)
freqs <- seq(0, fs / 2, length.out = floor(n / 2) + 1)

# Compute power spectrum (magnitude squared and normalized)
power_spectrum <- abs(fft_values[1:length(freqs)])^2 / n

# Create a data frame for plotting
fft_df <- data.frame(freq = freqs, power = power_spectrum)

# Plot the Power Spectrum
ggplot(fft_df, aes(x = freq, y = power)) +
  geom_line(color = "purple") +
  labs(title = "",
       x = "Frequency (1/Days)", y = "Power Spectral Density") +
  xlim(0, 10) +  # Focus on relevant frequency range
  theme_minimal()

# Autocorrelation and Partial Autocorrelation
acf(df$pdcsap_flux, lag.max = 100, main = "")
pacf(df$pdcsap_flux, lag.max = 50, main = "")


# Seasonal Decomposition
df_ts <- ts(df$pdcsap_flux, frequency = round(1 / 0.167))  # Set frequency based on period ~0.167 days
decomp <- decompose(df_ts)

# Plot Decomposition Components
plot(df_ts, main = "", col = "blue", ylab = "")
plot(decomp$trend, main = "", col = "red", ylab = "")
plot(decomp$seasonal, main = "", col = "green", ylab = "")
plot(decomp$random, main = "Residuals", col = "gray", ylab = "")
