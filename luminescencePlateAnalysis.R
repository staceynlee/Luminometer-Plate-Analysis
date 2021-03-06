### The purpose of this experiment was to determine the effect of drug treatment on cell energy status.
### This script analyzes data acquired from a luminomiter on a 96-well plate containing cells expressing a
### fluorescent biomarker. The data is normalized to protein conentration from a BSA assay plate reading.

# Load libraries
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotrix)

# Read and Extract Luminescence Data
d1 <- read_excel("sl062116_Luminescence Protocol_6-21-2016_11-45-37 AM.xlsx", sheet=1, col_names = FALSE)
alphaList <- c("A", "B", "C", "D", "E", "F", "G", "H")
d1Clean <- d1 %>% filter(X1 %in% alphaList) %>% select(-X0) %>% select(-X1)

id <- c(1,2:ncol(d1Clean)) 
d1Clean[,id] <- as.numeric(as.character(unlist(d1Clean[,id])))

# Read and Extract BSA Data
d2 <- read_excel("sl062116 bca.xls", sheet=1, col_names = FALSE)
d2Clean <- d2 %>% filter(X1 %in% alphaList) %>% select(-X14) %>% select(-X1)

# Add Column Names
colnames(d1Clean) <- c("Media", "1 nM T3", "2 nM T3", "3 nM T3", "5 nM T3", "10 nM T3", 
                       "1 nM Vehicle", "2 nM Vehicle", "3 nM Vehicle", "5 nM Vehicle", "10 nM Vehicle", "Media2")

colnames(d2Clean) <- c("Media", "1 nM T3", "2 nM T3", "3 nM T3", "5 nM T3", "10 nM T3", 
                       "1 nM Vehicle", "2 nM Vehicle", "3 nM Vehicle", "5 nM Vehicle", "10 nM Vehicle", "Media2")

# Normalize Data
dfNorm <- d1Clean/d2Clean

# Descriptive Statistics
dfMean <- summarise_each(dfNorm, funs(mean))
dfMean <- gather(dfMean, "treatment", "mean", 1:12)

dfSD <- summarise_each(dfNorm, funs(sd))
dfSD <- gather(dfSD, "treatment", "sd", 1:12)

dfSE <- summarise_each(dfNorm, funs(std.error))
dfSE <- gather(dfSE, "treatment", "se", 1:12)

dfSummary <- bind_cols(dfMean, dfSD, dfSE)
dfSummary <- dfSummary[, !duplicated(colnames(dfSummary))]

# Add Factor Column
cat1 <- grep("T3", dfSummary$treatment)
dfSummary[cat1, "drug"] <- "T3"

cat2 <- grep("Veh", dfSummary$treatment)
dfSummary[cat2, "drug"] <- "Veh"

cat3 <- grep("Media", dfSummary$treatment)
dfSummary[cat3, "drug"] <- "none"

# Order x-axis
drugOrder <- c("1 nM T3", "1 nM Vehicle", "2 nM T3", "2 nM Vehicle", 
               "3 nM T3", "3 nM Vehicle", "5 nM T3", "5 nM Vehicle",
               "10 nM T3", "10 nM Vehicle", "Media", "Media2")
dfSummary$treatment <- factor(dfSummary$treatment, levels = drugOrder)

# Plot Means with SE
ggplot(dfSummary, aes(x=treatment, y=mean, fill=drug)) + geom_bar(position = "dodge", stat = "identity") + 
  geom_errorbar(aes(ymax = mean + se, ymin=mean - se)) + 
  labs(title = "Mean Response to Drug Treatment", x = "Treatment",
       y = "Fluorescence") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Subtract Media Value
mediaMean <- mean(dfNorm$Media + dfNorm$Media2)/2
dfNorm2 <- sweep(dfNorm, 2, mediaMean,"-")

# Descriptive Statistics
dfMean2 <- summarise_each(dfNorm2, funs(mean))
dfMean2 <- gather(dfMean2, "treatment", "mean", 1:12)

dfSD2 <- summarise_each(dfNorm2, funs(sd))
dfSD2 <- gather(dfSD2, "treatment", "sd", 1:12)

dfSE2 <- summarise_each(dfNorm2, funs(std.error))
dfSE2 <- gather(dfSE2, "treatment", "se", 1:12)

dfSummary2 <- bind_cols(dfMean2, dfSD2, dfSE2)
dfSummary2 <- dfSummary2[, !duplicated(colnames(dfSummary2))]

# Add Factor Column
cat1 <- grep("T3", dfSummary2$treatment)
dfSummary2[cat1, "drug"] <- "T3"

cat2 <- grep("Veh", dfSummary2$treatment)
dfSummary2[cat2, "drug"] <- "Veh"

# Remove Media Data
dfSummary3 <- filter(dfSummary2, !grepl("Media", treatment))

# Order x-axis
drugOrder2 <- c("1 nM T3", "1 nM Vehicle", "2 nM T3", "2 nM Vehicle", 
               "3 nM T3", "3 nM Vehicle", "5 nM T3", "5 nM Vehicle",
               "10 nM T3", "10 nM Vehicle")
dfSummary3$treatment <- factor(dfSummary3$treatment, levels = drugOrder2)

# Plot Means with SE
ggplot(dfSummary3, aes(x=treatment, y=mean, fill=drug)) + geom_bar(position = "dodge", stat = "identity") + 
  geom_errorbar(aes(ymax = mean + se, ymin=mean - se)) + 
  labs(title = "Mean Response to Drug Treatment Normalized to Media Response", x = "Treatment",
       y = "Fluorescence") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

