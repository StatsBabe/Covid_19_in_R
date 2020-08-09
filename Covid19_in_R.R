
# load the package
library(COVID19)

# additional packages to replicate the examples
library(ggplot2)
library(directlabels)
library(tidyverse)

x <- covid19(verbose = FALSE)

x <- covid19()
print(x)

ggplot(data = x, aes(x = date, y = confirmed)) +
  geom_line(aes(color = id)) +
  #geom_dl(aes(label = administrative_area_level_1), method = list("last.points", cex = .75, hjust = 1, vjust = 0)) +
  scale_y_continuous(trans = 'log10') +
  theme(legend.position = "none") +
  ggtitle("Confirmed cases (log scale)")

ggplot(data = x, aes(x = date, y = confirmed/population)) +
  geom_line(aes(color = id)) +
  #geom_dl(aes(label = administrative_area_level_1), method = list("last.points", cex = .75, hjust = 1, vjust = 0)) +
  scale_y_continuous(trans = 'log10') +
  theme(legend.position = "none") +
  ggtitle("Confirmed cases - Fraction of total population (log scale)")

ggplot(data = x, aes(x = date, y = confirmed/tests)) +
  geom_line(aes(color = id)) +
  # geom_dl(aes(label = administrative_area_level_1), method = list("last.points", cex = .75, hjust = 1, vjust = 0)) +
  scale_y_continuous(trans = 'log10') +
  theme(legend.position = "none") +
  ggtitle("Confirmed cases - Fraction of tests (log scale)")

ggplot(data = x, aes(x = date, y = deaths/confirmed)) +
  geom_line(aes(color = id)) +
  #geom_dl(aes(label = administrative_area_level_1), method = list("last.points", cex = .75, hjust = 1, vjust = 0)) +
  scale_y_continuous(trans = 'log10') +
  theme(legend.position = "none") +
  ggtitle("Mortality rate (log scale)")

# retrieve vintage data on 2020-06-02 
x <- covid19(end = "2020-06-02", vintage = TRUE, verbose = FALSE)

y <- covid19(verbose = FALSE)

# add type
x$type <- "vintage"
y$type <- "latest"

# bind and filter
x <- rbind(x, y)
x <- x[x$iso_alpha_3=="GBR",]

# plot
ggplot(data = x, aes(x = date, y = deaths)) +
  geom_line(aes(color = type)) +
  theme(legend.position = "right") +
  ggtitle("UK fatalities")

# load data for United States, Italy, and Switzerland
x <- covid19(c("United States", "ITA", "ch"), verbose = FALSE)
# Example: plot the mortality rate.

ggplot(data = x, aes(x = date, y = deaths/confirmed)) +
  geom_line(aes(color = id)) +
  # geom_dl(aes(label = administrative_area_level_1), method = list("last.points", cex = .75, hjust = 1, vjust = 0)) +
  scale_y_continuous(trans = 'log10') +
  theme(legend.position = "none") + 
  ggtitle("Mortality rate (log scale)")

# italy admin area level 2
x <- covid19("ITA", level = 2, verbose = FALSE)

# plot
ggplot(data = x, aes(x = date, y = deaths/confirmed)) +
  geom_line(aes(color = id)) +
  # geom_dl(aes(label = administrative_area_level_2), method = list("last.points", cex = .75, hjust = 1, vjust = 0)) +
  scale_y_continuous(trans = 'log10') +
  theme(legend.position = "none") +
  ggtitle("Mortality rate by region (log scale)")

# italy and switzerland admin area level 2
x <- covid19(c("ITA","CHE"), level = 2, verbose = FALSE)

# plot
ggplot(data = x, aes(x = date, y = deaths/confirmed)) +
  geom_line(aes(color = administrative_area_level_1, group = administrative_area_level_2)) +
  # geom_dl(aes(label = administrative_area_level_2), method = list("last.points", cex = .75, hjust = 1, vjust = 0)) +
  scale_y_continuous(trans = 'log10') +
  theme(legend.position = "top", legend.title = element_blank()) +
  ggtitle("Mortality rate by region (log scale)")

# US data
x <- covid19("USA", verbose = FALSE)

# detect changes in testing policy
testing_policy_dates <- x$date[diff(x$testing_policy)!=0]

# plot mortality rate and changes in testing policy
ggplot(data = x, aes(x = date, y = deaths/confirmed)) +
  geom_line(aes(color = id)) +
  # geom_dl(aes(label = administrative_area_level_1), method = list("last.points", cex = .75, hjust = 1, vjust = 0)) +
  # geom_vline(xintercept = testing_policy_dates, linetype = 4) +
  scale_y_continuous(trans = 'log10') +
  theme(legend.position = "none") +
  ggtitle("US mortality rate and changes in testing policy")

wb <- c("gdp" = "NY.GDP.MKTP.CD", "hosp_beds" = "SH.MED.BEDS.ZS")
x  <- covid19(wb = wb, raw = TRUE, verbose = FALSE)
# Example: plot the mortality rate in function of the number of hospital beds.

ggplot(data = x, aes(x = hosp_beds, y = deaths/confirmed)) +
  geom_line(aes(color = id)) +
  # geom_dl(aes(label = administrative_area_level_1), method = list("last.points", cex = .75, hjust = 1, vjust = 0)) +
  scale_y_continuous(trans = 'log10') +
  xlab("Hospital beds per 1,000 people") +
  theme(legend.position = "none") +
  ggtitle("Worldwide mortality rates (log scale) and number of hospital beds")

gmr <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
x   <- covid19("ITA", gmr = gmr, raw = TRUE, verbose = FALSE)

# detect changes in transport policy
transport_dates <- x$date[diff(x$transport_closing)!=0]

# plot
ggplot(x, aes(x = date)) +
  geom_line(aes(y = confirmed/tests*100, color = "Confirmed/Tested"), size = 1.2) +
  geom_line(aes(y = deaths/confirmed*100, color = "Deaths/Confirmed"), size = 1.2) +
  geom_line(aes(y = residential_percent_change_from_baseline, color = "Residential")) +
  geom_line(aes(y = workplaces_percent_change_from_baseline, color = "Workplaces")) +
  geom_line(aes(y = transit_stations_percent_change_from_baseline, color = "Transit Stations")) 
