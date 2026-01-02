# packages 
library(viridis)
library(maps)
library(giscoR)
library(scales)
library(tidyverse)

# Get the world polygon and filter UK
UK <- gisco_get_countries(country = "UK", resolution = 1)

# mean salaries (see README.md) across major cities in the UK 

salaries <- data.frame (
  city = c('London', 'Edinburgh', 'Brighton', 'Bristol', 'Reading', 'Cardiff',
           'Leeds', 'Portsmouth', 'Glasgow', 'Derby', 'Sheffield', 
           'Manchester', 'Southampton', 'Liverpool', 'Newcastle', 'Birmingham',
           'Coventry', 'Plymouth', 'Bradford', 'Hull', 'Nottingham'),
  salary = c(38281, 34694, 33219, 32785, 32667, 31621, 31450, 30732,
             30690, 30157, 30155, 29973, 29857, 29731, 29634,28995,
             28828, 28363, 27521, 27059, 25252)
)

salaries

summary(salaries)
# dataset
# write the file to your folder setwd(choose.file)
write.csv(salaries, "uk_mean_salaries.csv")

# filter UK from the world.cities dataset contained in the package 'maps'
data <- world.cities %>% filter(country.etc == "UK")

# remove 'capital' & 'country.etc' columns 
data$capital = NULL
data$country.etc = NULL

# rename 'pop' to 'population'
data <- data %>% rename(population = 'pop')
# rename 'name' to 'city'
data <- data %>% rename(city = 'name')
# combine the datasets
data <- data %>% inner_join(salaries)

plot <- ggplot() +
  geom_sf(data = UK, fill = 'grey', alpha = 0.3) +
  geom_point(data = data, aes(x = long, y = lat, size = population, 
                              color = salary),
             alpha = 0.9) +
  scale_size_continuous(range = c(1, 15),
                        label = unit_format(unit = "M", scale = 1e-6)) +
  scale_color_viridis_c(trans = "log",
                        label = unit_format(
                          prefix = '£', unit = 'K', 
                          scale = 1e-3)) +
  ylim(50, 59) + 
  ggtitle(label = "Mean salaries & population in major cities across the UK") + 
  theme_void() + 
  theme(plot.title = element_text(face = "bold", color = '#1F618D'))

# save the plot
# write the file to your folder setwd(choose.file)
ggsave("plot.png", plot, 
       dpi = 400,
       limitsize = TRUE, width = 27, height = 17, units = "cm")

