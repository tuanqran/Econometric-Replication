# Article/Study: A systematic review and meta-analysis of the evidence on learning during the COVID-19 pandemic
# Link: https://www.nature.com/articles/s41562-022-01506-4
# Authors: Bastian A. Betthäuser, Anders M. Bach-Mortensen, & Per Engzell
# Replication Start & End Date: Nov 2023 - Dec 2023 

# Required packages & R libraries
install.packages("estimatr")
install.packages("jtools")

library(ggplot2)
library(estimatr)
library(jtools)

# Import estimates of learning deficit
data <- read_excel("extraction_data.xlsx", sheet = "Sheet1")
data$date <- as.Date(data$date, format = "%b %Y")
data$ln_z <- log(data$z)

# Figure 1: Study identification and selection process (PRISMA diagram): COMPILED MANUALLY BY THE AUTHORS

# Figure 2a: Risk of bias rating using ROBINS-I: COMPILED MANUALLY BY THE AUTHORS
  
# Figure 2b: Distribution of z-scores across estimates 
hist_data <- hist(data$ln_z, breaks = 25, plot = FALSE)
x_vals <- seq(-2.53, max(hist_data$breaks), by = 15)
curve_vals <- dnorm(x_vals, mean = mean(data$ln_z), sd = sd(data$ln_z))
plot(hist_data, col = "lightblue", xlab = "z-statistic (log scale)", ylab = "Density")
lines(x_vals, curve_vals, type = "l", lwd = 2, col = "darkred")
abline(v = 0.67294447, lty = 2, lwd = 2, col = "darkred")
text(0.27, -0.2, "z = 1.96", cex = 0.8, col = "darkred")

# Figure 3: Forest plot showing individual estimates by study: Replication N/A

# Figure 4: Estimates by date of measurement 
set.seed(86688)

# Define color palette
mycolors <- c("#000000", "#FFFFFF", "#3CB44B", "#FF7F00", "#42D4F4", "#BFEF45", 
              "#815375", "#FFC81D", "#CE8BAE", "#999999", "#BF862B", "#E41A1C", 
              "#FFD8B1", "#BD6253", "#3A85A8")

ggplot(data = data) + 
  geom_smooth(mapping = aes(date, es), color = "darkgrey", method = 'lm') +
  geom_jitter(mapping = aes(x = date, y = es, fill = country, size = n_study), 
              shape = 21, alpha = 1, position = position_jitter(width = 5, height = 0)) + 
  scale_fill_manual(values = mycolors) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(-0.8, 0.4, by = 0.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Date") + ylab("Learning deficit (SD)") +
  guides(size = "none", fill = guide_legend(title = "", override.aes = list(size = 5))) +
  theme_bw()

# Regression Table
model <- lm(es ~ date, data = data)
summary(model)

# Regression Table with Package "jtools"
summ(model)

# Figure 5: Harvest plot of evidence on educational inequality: COMPILED MANUALLY BY THE AUTHORS

# Figure 6: Variation by individual and country-level characteristics 
data$math <- as.factor(data$subject == 'Math')
data$sec <- as.factor(data$prim_sec == 'Secondary')
data$midinc <- as.factor(data$country %in% c('Brazil', 'Colombia', 'Mexico', 'South Africa'))

p_math_reading <- ggplot(data, aes(x=math, y=es, fill=math)) +
  geom_violin(trim=FALSE) +
  labs(x="", y="Effect Size (d)", title="Effect Size by Subject") +
  scale_x_discrete(labels=c("Reading", "Math")) +
  scale_fill_discrete(name="Subject", labels=c("Reading", "Math")) +
  theme_minimal()

p_primary_secondary <- ggplot(data, aes(x=sec, y=es, fill=sec)) +
  geom_violin(trim=FALSE) +
  labs(x="", y="Effect Size (d)", title="Effect Size by Education Level") +
  scale_x_discrete(labels=c("Primary", "Secondary")) +
  scale_fill_discrete(name="Education Level", labels=c("Primary", "Secondary")) +
  theme_minimal()

p_income <- ggplot(data, aes(x=midinc, y=es, fill=midinc)) +
  geom_violin(trim=FALSE) +
  labs(x="", y="Effect Size (d)", title="Effect Size by Country Income Level") +
  scale_x_discrete(labels=c("High Income", "Middle Income")) +
  scale_fill_discrete(name="Income Level", labels=c("High Income", "Middle Income")) +
  theme_minimal()

print(p_math_reading)
print(p_primary_secondary)
print(p_income)