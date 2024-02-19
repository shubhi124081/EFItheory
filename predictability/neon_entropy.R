# Calculating neon's raw data permutation entropy
# Generating some preliminary results

data <- read.csv("~/predictability/data/efi-neon-data/terrestrial-30-mins.csv")
data <- data[complete.cases(data), ]
data$year_month <- substr(data$datetime, 1, 7)
data$year <- substr(data$year_month, 1, 4)

vars <- unique(data$variable)
m <- matrix(0, nrow = length(unique(data$site_id)), ncol = length(vars))
df <- data.frame(
  "site" = unique(data$site_id)
)
df <- cbind(df, m)
colnames(df) <- c("site", paste0("wpe_", vars))

pb <- txtProgressBar()
for (j in seq_len(length(unique(data$variable)))) {
  data1 <- data[data$variable == vars[j], ]
  print(vars[j])
  for (i in seq_len(nrow(df))) {
    sub <- data1[data1$site_id == df$site[i], ]
    sub <- aggregate(observation ~ year_month, data = sub, FUN = mean)
    if (nrow(sub) == 0) {
      df[i, j + 1] <- NA
    } else {
      df[i, j + 1] <- WPE(sub$observation, dim = 5)
    }
    setTxtProgressBar(pb, (i / nrow(df)))
  }
}

# write.csv(df, "~/predictability/int/wpe_terrestrial-year.csv", row.names = FALSE) # nolint

# Figures
library(ggplot2)
load("~/RColorways/data/ALL.Rdata")
col <- ALL$Sequential$Peacock

sites <- read.csv("~/predictability/data/efi-neon-data/site_data.csv")
sites <- sites[, c("field_site_id", "field_latitude", "field_longitude")]

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
usa <- rnaturalearth::ne_countries(country = "united states of america")

df_cood <- merge(df, sites, by.x = "site", by.y = "field_site_id")
ggplot(data = df_cood) +
  geom_point(aes(
    x = field_longitude, y = field_latitude,
    color = wpe_le
  ), size = 2) +
  geom_sf(data = world, fill = NA, col = "black") +
  coord_sf(xlim = c(-66, -124), ylim = c(24, 49)) +
  scale_color_gradientn(colors = col, limits = c(0, 1)) +
  theme_bw() +
  labs(color = "WPE") +
  ggtitle("WPE for Daily Latent Heat Flux")


# Time series
data1 <- aggregate(observation ~ year_month, data = data, FUN = mean)

mean_gcc_90 <- data1 %>%
  dplyr::group_by(year_month) %>%
  dplyr::summarize(mean_gcc_90 = mean(observation, na.rm = TRUE))


ggplot(data = data1) +
  geom_line(aes(x = year_month, y = observation, group = 1, )) +
  theme_bw() +
  labs(
    x = "Date",
    y = "Green chromatic coordinate (90th percentile)"
  )
#  shape = 21, alpha = 0.2, fill = "green"
