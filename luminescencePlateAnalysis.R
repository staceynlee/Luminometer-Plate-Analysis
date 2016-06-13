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
d1 <- read_excel("staceyy_Luminescence Protocol_6-9-2016_10-17-48 AM.xlsx", sheet=1, col_names = FALSE)
alphaList <- c("A", "B", "C", "D", "E", "F", "G", "H")
d1Clean <- d1 %>% filter(X1 %in% alphaList) %>% select(-X0) %>% select(-X1)

id <- c(1,2:ncol(d1Clean)) 
d1Clean[,id] <- as.numeric(as.character(unlist(d1Clean[,id])))

# Read and Extract BSA Data
d2 <- read_excel("stacey BSA 6-9-2016.xls", sheet=1, col_names = FALSE)
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

# Plot Means with SE
ggplot(dfSummary, aes(x=treatment, y=mean, fill=drug)) + geom_bar(position = "dodge", stat = "identity") + 
  geom_errorbar(aes(ymax = mean + se, ymin=mean - se)) + 
  labs(title = "Mean Response to Drug Treatment", x = "Treatment",
       y = "Fluorescence") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
