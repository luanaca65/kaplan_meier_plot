# Install necessary packages
install.packages(c("survival", "ggsurvfit", "readr"))

# Load libraries
library(survival)
library(ggsurvfit)
library(readr)
library(dplyr)

# Read data
adtte <- read.csv("adtte.csv")  
# Filter ADTTE for PARAMCD = TTDE
adtte_filtered <- adtte %>%
  filter(paramcd == "TTDE")

# Fit the Kaplan-Meier Survival Model
# Define the Survival Object: Surv(Time to Event, Event Status) ~ Grouping Variable
# 'aval' (time to event), '1 - cnsr' (event status: 1=event, 0=censored)
km_plot <- survfit2(Surv(aval, 1 - cnsr) ~ trtp, data = adtte_filtered) %>% 
  # Generate the Kaplan-Meier Plot
  ggsurvfit () +
  add_confidence_interval() +
  labs(title = "Kaplan-Meier Curve for Time to First Dermatologic Event",
       x = "Time (Days)",
       color = "Treatment Group",
       fill = "Treatment Group") +
  theme(legend.position ="top") +
  add_pvalue(location = "annotation", x = 10.5, y = 0.20) +
  scale_x_continuous(
    breaks = c(0, 50, 100, 150, 200),
    limits = c(0, 200)) +
  add_risktable(risktable_stats = "n.risk",
                stats_label = "Number at risk") +
  add_risktable_strata_symbol()

# Save the Plot
ggsave("kaplan-meier_plot.png", plot = km_plot, device = "png", dpi = 300)


