#####  Blood Cell Analysis #####

## Load Libraries
library(lme4)
library(ggplot2)
library(dplyr)

## Read in Blood Cell Data
blood_data <- read.csv(file = "/Users/nataliehaydt/Desktop/Blood_Data_Counted.csv", header = TRUE, stringsAsFactors = FALSE)


## Add columns for H:L ratio, percent WBCs, and delete comments
blood_data <- blood_data %>%
  select(-c(comments, comments.1, FOV, smear)) %>%
  mutate(hl_ratio = heterophil / lymphocyte,
         thromb_10000 = round((thromb / total)*10000), digits = 0,
         het_10000 = round((heterophil / total)*10000), digits = 0,
         eosin_10000 = round((eosinophil / total)*10000), digits = 0,
         bas_10000 = round((basophil / total)*10000), digits = 0,
         lymph_10000 = round((lymphocyte / total)*10000), digits = 0,
         mono_10000 = round((monocyte / total)*10000), digits = 0,
         parasite_10000 = round((parasite / total)*10000), digits = 0)
## changed to 10000 instead of 1000 because numbers were so small that rounding didn't capture differences well (round function rounds to closest even if rounding number is 5)


## Join morphometric and leech data per sampled individual

EDF <- read.csv(file = "/Users/nataliehaydt/cocanal-turtles/Data/EDF.csv", stringsAsFactors = FALSE)
names(EDF)[5]<-"id"
EDF <- EDF %>%
  select(-c(date, site, trap_id, trap, day, edits)) %>%
  filter(recap == "N")
  
blood_data <- left_join(blood_data, EDF, by = c("species", "id"))

## Summary for all data
blood_summary <- summary(blood_data)


## Filter data per species
blood_data_cpic <- blood_data %>%
  filter(species == "CPIC")
summary_cpic <- summary(blood_data_cpic)

blood_data_cser <- blood_data %>%
  filter(species == "CSER")
summary_cser <- summary(blood_data_cser)

blood_data_sodo <- blood_data %>%
  filter(species == "SODO")
summary_sodo <- summary(blood_data_sodo)

blood_data_prub <- blood_data %>%
  filter(species == "PRUB")
summary_prub <- summary(blood_data_prub)

blood_data_tscr <- blood_data %>%
  filter(species == "TSCR")
summary_tscr <- summary(blood_data_tscr)


## Summarize data per family
# Emydidae
blood_data_emyd <- blood_data %>%
  filter(species == "CPIC" | species == "PRUB" | species == "GINS" | species == "TSCR")

#blood_data_emyd <- blood_data_emyd[which(blood_data_emyd$id != "510"), ]

# Kinosternidae
blood_data_kin <- blood_data_sodo

# Chelydridae
blood_data_chelyd <- blood_data_cser

## Summarize data per family
emydidae_summary <- summary(blood_data_emyd)
kin_summary <- summary(blood_data_kin)
chelyd_summary <- summary(blood_data_chelyd)


################ Tables ##################
library(table1)
table1::label(blood_data$parasite_10000) <- "Parasites (Per 10000)"
table1::label(blood_data$thromb_10000) <- "Thrombocytes (Per 10000)"
table1::label(blood_data$het_10000) <- "Heterophils (Per 10000)"
table1::label(blood_data$eosin_10000) <- "Eosinophils (Per 10000)"
table1::label(blood_data$bas_10000) <- "Basophils (Per 10000)"
table1::label(blood_data$lymph_10000) <- "Lymphocytes (Per 10000)"
table1::label(blood_data$mono_10000) <- "Monocytes (Per 10000)"


Blood_Table <- table1::table1(~parasite_10000 + thromb_10000 + het_10000 + eosin_10000 + bas_10000 + lymph_10000 + mono_10000 | species, data = blood_data)
 
###################################################

## Graphs

#par(mfrow = c(3,1))
# Percent parasites per family per site - BOXPLOT
g1 <- ggplot(data = blood_data, aes(x = family, y = percent_parasite)) + geom_boxplot(aes(fill = family)) +
  ggtitle("Frequency of Intracellular Parasites By Family") + ylab("Percent Parasites") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))
g1

g1 + coord_cartesian(ylim = c(0, 1)) ## instead of putting ylim as own function (changes data)

blood_data_no_out <- blood_data
boxplot(blood_data_emyd$percent_parasite)$out
blood_data_no_out <- blood_data_no_out %>%
  mutate(percent_parasite = replace(percent_parasite, id == 4, NA))
boxplot(blood_data_no_out$percent_parasite)$out


boxplot(blood_data_chelyd$percent_parasite)$out
blood_data_no_out <- blood_data_no_out %>%
  mutate(percent_parasite = replace(percent_parasite, id == 350, NA))
boxplot(blood_data_no_out$percent_parasite)$out

boxplot(blood_data_kin$percent_parasite)$out
# no SODO outliers!!
hist(blood_data_kin$percent_parasite)



# Percent parasites per species per site - BOXPLOT - without major outliers
ggplot(data = blood_data_no_out, aes(x = family, y = percent_parasite)) + geom_boxplot(aes(fill = family)) +
  ggtitle("Frequency of Intracellular Parasites By Family") + ylab("Percent Parasites") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

# All sites together
ggplot(data = blood_data_no_out, aes(x = family, y = percent_parasite)) + geom_boxplot(aes(fill = family), position = "identity") + facet_wrap(~site) +
  ggtitle("Frequency of Intracellular Parasites Observed") + xlab("Percent Parasites") +
  scale_y_continuous(limits = c(lower = 0, upper = 1.0)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))


# Histograms
boxplot(blood_data$leeches)
boxplot(blood_data$percent_parasite)

# Percent of Parasites (out of total blood cells) per family (all sites together)
ggplot(data = blood_data_no_out, aes(percent_parasite)) + geom_histogram(aes(fill = family), binwidth = 0.10, position = "stack") +
  ggtitle("Frequency of Intracellular Parasites By Family") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

ggplot(data = blood_data_no_out, aes(percent_parasite)) + geom_histogram(aes(fill = species), binwidth = 0.10, position = "stack") +
  ggtitle("Frequency of Intracellular Parasites By Family") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

# Per species per site
ggplot(data = blood_data_no_out, aes(percent_parasite)) + geom_histogram(aes(fill = species), binwidth = 0.15, position = "identity") + facet_wrap(~site) + xlab("Percent Parasites") +
  ggtitle("Percent Intracellular Parasites") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

# Per family per site
ggplot(data = blood_data_no_out, aes(percent_parasite)) + geom_histogram(aes(fill = family), binwidth = 0.15, position = "stack") + facet_wrap(~site) +
  ggtitle("Percent Intracellular Parasites") + xlab("Percent Parasites") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))


############## LEECHES ################

# Number of leeches per family per site - BOXPLOT
ggplot(data = blood_data, aes(x = family, y = leeches)) + geom_boxplot(aes(fill = family)) +
  ggtitle("Number of Leeches By Family") + ylab("Number of Leeches") + facet_wrap(~site) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

ggplot(data = blood_data, aes(x = family, y = leeches)) + geom_boxplot(aes(fill = family)) +
  ggtitle("Number of Leeches By Family") + ylab("Number of Leeches") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))


boxplot(blood_data_emyd$leeches)$out
blood_data_no_out <- blood_data_no_out %>%
  mutate(leeches = replace(leeches, leeches == 40, NA))
boxplot(blood_data_no_out$leeches)$out

boxplot(blood_data_chelyd$leeches)$out
blood_data_no_out <- blood_data_no_out %>%
   mutate(leeches = replace(leeches, leeches == 86, NA))
boxplot(blood_data_no_out$leeches)$out

# Number of leeches per family per site - BOXPLOT - Without outlier
ggplot(data = blood_data_no_out, aes(x = family, y = leeches)) + geom_boxplot(aes(fill = family)) +
  ggtitle("Number of Leeches By Family") + ylab("Number of Leeches") + facet_wrap(~site) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

ggplot(data = blood_data_no_out, aes(x = family, y = leeches)) + geom_boxplot(aes(fill = family)) +
  ggtitle("Number of Leeches By Family") + ylab("Number of Leeches") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

###############################################################


## Number of leeches by percent parasites - SCATTER
ggplot(blood_data_no_out, aes(x=leeches, y=percent_parasite)) + 
  geom_point(aes(size=carapace, color = family)) +
  ggtitle("Percent Parasties as a Function of Number of Leeches") + ylab("Percent Parasites") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))


#### HERE ####  #### Check in residuals... not in data.
## Can /1000 and then round to get whole number integers... then use poisson distribution to model -- leeches and blood cells
## Check diffs b/w species (filter out species with low sample sizes vs. aggregating to family)
## Checking Assumptions
par(mar=c(1,1,1,1))
hist(blood_data_no_out[blood_data_no_out$family == "em", ]$percent_parasite)  # All towards lower prop -- like count data, but not whole integers
hist(blood_data_no_out[blood_data_no_out$family == "ch", ]$percent_parasite)
hist(blood_data_no_out[blood_data_no_out$family == "ki", ]$percent_parasite)
blood_data_no_out$log_percent_parasite <- log(blood_data_no_out$percent_parasite)
hist(blood_data_no_out$log_percent_parasite)
hist(blood_data_no_out[blood_data_no_out$family == "em", ]$log_percent_parasite)
hist(blood_data_no_out[blood_data_no_out$family == "ch", ]$log_percent_parasite)

####

###### LEECHES BY FAMILY with random site
mod1 <- glmer(leeches ~ family + (1|site), data = blood_data_no_out, family = poisson)
summary(mod1) # makes dummy variables for you, always relative to number of chelydridae

blood_data_scaled <- blood_data_no_out %>%
   mutate(mass.s = (mass - mean(mass, na.rm = TRUE)) / sd(mass, na.rm = TRUE))
summary(blood_data_scaled)

###### LEECHES BY FAMILY and MASS with random site
 mod2 <- glmer(leeches ~ family*mass.s + (1|site), data = blood_data_scaled, family = poisson)
 summary(mod2)

######## Percent Parasies by Family with random site
mod3 <- lmer(percent_parasite ~ family + (1|site), data = blood_data_no_out)
summary(mod3)

mod3 <- lmer(percent_parasite ~ (1|family), data = blood_data_no_out)
summary(mod3)

mod4 <- aov(data = blood_data_no_out, percent_parasite ~ family) # no autocorrelation in data, same sample size... don't do anovas
#mod4 <- aov(data = blood_data_no_out, percent_parasite ~ family + Error(site)) ??
summary(mod4)
mod4_posthhoc <- TukeyHSD(mod4, 'family', conf.level = 0.95)

mod5 <- lmer(percent_parasite ~ leeches + (1|family) + (1|site), data = blood_data_no_out)
summary(mod5)
mod6 <- lm(data = blood_data_no_out, percent_parasite ~ leeches)
summary(mod6)
mod7 <- lm(data = blood_data_no_out, hl_ratio ~ percent_parasite + leeches)
summary(mod7)




####### TESTS/MODELS to RUN #######

## linear regression b/w leeches and parasites (control with random for species and site)
## linear regression b/w parasites and hl ratio (control with random for species and site)

## See whether hets or lymphs are driving hl ratio changes...
## linear regression b/w parasites and hets (control with random for species and site)
## linear regression b/w parasites and lymphs (control with random for species and site)

## linear regression b/w parasites and eosins (control for species and site)
## linear regression b/w leeches and eosins (control for species and site)
## ANOVA for parasites b/w sites (control with random for species)
## ANOVA for leeches b/w sites ("")
## ANOVA for parasites b/w species (control with random for site)
## ANOVA for leeches b/w species ("")
## ANOVA for hl ratio between sites (control for species)
## ANOVA for hl ratio between species (control for site)
