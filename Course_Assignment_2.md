---
title: "How Storms and Severe Weather Events Affect Public Health and Local Economies Across the United States"
author: "Lontum E. Nchadze"
date: "10/30/2020"
output:
  html_document: 
    keep_md: true    
pandoc_args: [
      "--number-sections",
    ]
---

## Synopsis

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern for authorities. This study seeks to assist in this effort by identifying the weather events that are most harmful to public health and the economy. Using data from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database, I show that Tornados and Lightening are by far the most responsible weather events for injuries and are also the identifiable events most responsible for fatalities in the United States. I also find that floods, tornados, and hail are most responsible for property and crop damages in the United States over the same period. Anyone who seeks to reproduce this project can find all the code and data on my GitHub Repository.

- - -

# Data Processing

The data used for this analysis comes from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage. It comes in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size.  
Below, I will check for the availability of the data object in my work space, unzip and load the file, and cache the procedure to facilitate any future runs.


```r
# Load Knitr library
library(knitr)
# Remove comment symbol from all results
opts_chunk$set(comment = NA)
# Check for data and load
if(!exists("noaa.data")) {
noaa.data <- read.csv(bzfile("repdata_data_StormData.csv.bz2"), header = TRUE)
}
# Check dimentions of Dataframe
dim(noaa.data)
```

```
## [1] 902297     37
```

## Data Reduction

As shown above, the data contains 902,297 observations and 37 variables. For this analysis, I am interested only in those variables related to weather events, public health, and the economy. These variables are: *EVTYPE* (weather event); *FATALITIES* (approximate number of deaths); *INJURIES* (approximate number of injuries); *PROPDMG* (approximate property damages); *PROPDMGEXP* (unit value of property damage); *CROPDMG* (approximate crop damages); *CROPDMGEXP* (units value of crop damage).  
I will create a new dataframe with only these variables included.


```r
# select variables relevant for the assignment
data <- noaa.data[, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
# check missing values in data
colSums(is.na(data))
```

```
    EVTYPE FATALITIES   INJURIES    PROPDMG PROPDMGEXP    CROPDMG CROPDMGEXP 
         0          0          0          0          0          0          0 
```

## Tidying Values

We find that there are no missing values in the data, but the events variable (EVTYPE), and the variables for dollar units of damages (PROPDMGEXP and CROPDMGEXP) are very untidy, with inconsistent capitalisation and spacing that makes several values appear as different when they are infact the same.  I will now fix such values. For the *EVTYPE* variable, I will identify the most recurring values since the variable has 985 values. I will then collapse the values that relate to these. I will categorise the rest as "OTHER EVENT" For the other two, I will collapse all Ks, Ms and Bs into 1000, 1000,000 and 1,000,000,000 respectively irrespective of case. all the other values will be collapsed into 1.


```r
# identify the most recurring values in EVTYPE
sort(table(data$EVTYPE), decreasing = TRUE)[1:20]
```

```

                    HAIL                TSTM WIND        THUNDERSTORM WIND 
                  288661                   219940                    82563 
                 TORNADO              FLASH FLOOD                    FLOOD 
                   60652                    54277                    25326 
      THUNDERSTORM WINDS                HIGH WIND                LIGHTNING 
                   20843                    20212                    15754 
              HEAVY SNOW               HEAVY RAIN             WINTER STORM 
                   15708                    11723                    11433 
          WINTER WEATHER             FUNNEL CLOUD         MARINE TSTM WIND 
                    7026                     6839                     6175 
MARINE THUNDERSTORM WIND               WATERSPOUT              STRONG WIND 
                    5812                     3796                     3566 
    URBAN/SML STREAM FLD                 WILDFIRE 
                    3392                     2761 
```

```r
# Tidy up values and collapse values that mean the same
data$EVTYPE <- gsub("^   ", "", data$EVTYPE)
data$EVTYPE <- gsub("^ ", "", data$EVTYPE)
data$EVTYPE[grep("^ABNORMAL", data$EVTYPE, ignore.case = TRUE)] <- "ABNORMAL WEATHER"
data$EVTYPE[grep("^ASTRONOMICAL", data$EVTYPE, ignore.case = TRUE)] <- "UNUSUAL TIDE"
data$EVTYPE[grep("^AVALANC", data$EVTYPE, ignore.case = TRUE)] <- "AVALANCHE"
data$EVTYPE[grep("^BEACH", data$EVTYPE, ignore.case = TRUE)] <- "BEACH EVENT"
data$EVTYPE[grep("^BLIZZARD", data$EVTYPE, ignore.case = TRUE)] <- "BLIZZARD EVENT"
data$EVTYPE[grep("^BLOWING SNOW", data$EVTYPE, ignore.case = TRUE)] <- "BLOWING SNOW"
data$EVTYPE[grep("^COASTAL", data$EVTYPE, ignore.case = TRUE)] <- "COASTAL EVENT"
data$EVTYPE[grep("^COLD", data$EVTYPE, ignore.case = TRUE)] <- "COLD CONDITIONS"
data$EVTYPE[grep("^DRY", data$EVTYPE, ignore.case = TRUE)] <- "DRY WEATHER"
data$EVTYPE[grep("^EARLY", data$EVTYPE, ignore.case = TRUE)] <- "EARLY ONSET OF WEATHER CONDITION"
data$EVTYPE[grep("HEAT", data$EVTYPE, ignore.case = TRUE)] <- "HEAT"
data$EVTYPE[grep("WIND", data$EVTYPE, ignore.case = TRUE)] <- "WIND"
data$EVTYPE[grep("RAIN", data$EVTYPE, ignore.case = TRUE)] <- "RAIN"
data$EVTYPE[grep("FLOOD", data$EVTYPE, ignore.case = TRUE)] <- "FLOOD"
data$EVTYPE[grep("FIRE", data$EVTYPE, ignore.case = TRUE)] <- "FIRE"
data$EVTYPE[grep("^WINTER", data$EVTYPE, ignore.case = TRUE)] <- "WINTER WEATHER"
data$EVTYPE[grep("WATER", data$EVTYPE, ignore.case = TRUE)] <- "WATERSPOUT"
data$EVTYPE[grep("HAIL", data$EVTYPE, ignore.case = TRUE)] <- "HAIL"
data$EVTYPE[grep("^TORNADO", data$EVTYPE, ignore.case = TRUE)] <- "TORNADO"
data$EVTYPE[grep("^FUNNEL", data$EVTYPE, ignore.case = TRUE)] <- "FUNNEL CLOUD"
data$EVTYPE[grep("^URBAN", data$EVTYPE, ignore.case = TRUE)] <- "URBAN/SML STREAM FLD"
f <- c("FLASH FLOOD", "FLASH FLOOD/", "FLASH FLOOD/FLOOD", "FLASH FLOODING", "FLASH FLOODING/FLOOD", "FLASH FLOODS", "FLASH FLOOODING", "FLOOD FLASH", "FLOOD FLOOD/FLASH", "FLOOD/FLASH", "Flood/Flash Flood", "FLOOD/FLASH FLOOD", "FLOOD/FLASH FLOODING", "FLOOD/FLASH/FLOOD", "FLOOD/FLASHFLOOD", "FLASH FLOOD - HEAVY RAIN", "FLASH FLOOD FROM ICE JAMS", "FLASH FLOOD LANDSLIDES", "FLASH FLOOD WINDS", "FLASH FLOOD/ STREET", "FLASH FLOOD/HEAVY RAIN", "FLASH FLOOD/LANDSLIDE", "FLASH FLOODING/THUNDERSTORM WI")
for(i in 1:length(f)) {
data$EVTYPE <- gsub(f[i], "FLASH FLOOD", data$EVTYPE)
}
d <- c(" LIGHTNING", "LIGHTNING", "LIGHTNING  WAUSEON", "LIGHTNING AND HEAVY RAIN", "LIGHTNING AND THUNDERSTORM WIN", "LIGHTNING FIRE", "LIGHTNING INJURY", "LIGHTNING THUNDERSTORM WINDS", "LIGHTNING THUNDERSTORM WINDSS", "LIGHTNING.", "LIGHTNING/HEAVY RAIN")
for(i in 1:length(d)) {
data$EVTYPE <- gsub(d[i], "LIGHTNING", data$EVTYPE)
}
```

Because of the very many values in this variable, I will only collapse the variable into the dominant values and group the rest as “OTHER EVENT”. At this point, let us take a look at the most frequent weather events across the United States.


```r
# identify the most recurring values.
sort(table(data$EVTYPE), decreasing = TRUE)[1:7]
```

```

          WIND           HAIL          FLOOD        TORNADO WINTER WEATHER 
        364342         289276          81841          60684          19596 
     LIGHTNING     HEAVY SNOW 
         15759          15708 
```

```r
# Create a character vector of recurring values
recurrent <- c("WIND", "HAIL", "FLOOD", "TORNADO", "WINTER WEATHER", "LIGHTNING", "HEAVY SNOW")
# Create an index of values that are not recurrent
index <- !(data$EVTYPE %in% recurrent)
# Assign non-recurrent events as "Other Values"
data$EVTYPE[index] <- "OTHER EVENT"
# Make EVTYPE variable into a factor
data$EVTYPE <- factor(data$EVTYPE)
```

Next, I will look at the *PROPDMGEXP* variable and fix the units.


```r
# identify values in PROPDMGEXP
sort(table(data$PROPDMGEXP), decreasing = TRUE)
```

```

            K      M      0      B      5      1      2      ?      m      H 
465934 424665  11330    216     40     28     25     13      8      7      6 
     +      7      3      4      6      -      8      h 
     5      5      4      4      4      1      1      1 
```

```r
# collapse values that mean the same
data$PROPDMGEXP[grep("K", data$PROPDMGEXP, ignore.case = TRUE)] <- 1000
data$PROPDMGEXP[grep("M", data$PROPDMGEXP, ignore.case = TRUE)] <- 1000000
data$PROPDMGEXP[grep("B", data$PROPDMGEXP, ignore.case = TRUE)] <- 1000000000
# Create a numeric vector of recurring values
units <- c(1000, 1000000, 1000000000)
# Create an index of values that are not recurrent
index <- !(data$PROPDMGEXP %in% units)
# Assign 1 as the value for non-recurrent units
data$PROPDMGEXP[index] <- 1
```

Finally, I will look at the *CROPDMGEXP* variable and fix the units.


```r
# identify values in CROPDMGEXP
sort(table(data$CROPDMGEXP), decreasing = TRUE)
```

```

            K      M      k      0      B      ?      2      m 
618413 281832   1994     21     19      9      7      1      1 
```

```r
# collapse values that mean the same
data$CROPDMGEXP[grep("K", data$CROPDMGEXP, ignore.case = TRUE)] <- 1000
data$CROPDMGEXP[grep("M", data$CROPDMGEXP, ignore.case = TRUE)] <- 1000000
data$CROPDMGEXP[grep("B", data$CROPDMGEXP, ignore.case = TRUE)] <- 1000000000
# Create an index of values that are not recurrent
index <- !(data$CROPDMGEXP %in% units)
# Assign 1 as the value for non-recurrent units
data$CROPDMGEXP[index] <- 1
```

## Generating New Variables

I will now estimate the value of material and crop damage for each event by multiplying the damage values by their corresponding dollar units.


```r
data$Property.Damage <- data$PROPDMG*as.numeric(data$PROPDMGEXP)
data$Crop.Damage <- data$CROPDMG*as.numeric(data$CROPDMGEXP)
```

- - -

# Results

In this section, I will explore the data to find out which types of weather events are most harmful to public health and the economy.

## Public Health Effects of Weather Events


```r
# load dplyr library
library(dplyr)
# generate average number of injuries and fatalities for each category of weather event
health <- data %>%
group_by(EVTYPE) %>%
summarise(AverageInjuries = mean(INJURIES), AverageFatalities = mean(FATALITIES))
```

```
`summarise()` ungrouping output (override with `.groups` argument)
```

```r
# create a sub dataset for average fatalities only
health1 <- data.frame(Event = health$EVTYPE, Type = "Fatalities", Effect = health$AverageFatalities)
# create a sub dataset for average injuries only
health2 <- data.frame(Event = health$EVTYPE, Type = "Injuries", Effect = health$AverageInjuries)
# combine the injuries and fatalities sub data frames
HealthEffects <- rbind(health1, health2)
# load ggplot2 library
library(ggplot2)
# generate basic esthetics for mapping coordinates of the plot
HealthPlot <- ggplot(HealthEffects, aes(Event, Effect, fill = Type)) +
# Add bar graphs to the plot
geom_bar(stat = "identity") +
# Add labels to the plot
labs(title = "Health Effects of Weather Events in the United States",
subtitle = "1950 to 2011",
x = "Weather Event",
tag = "Figure 1",
y = "Average Effect per Weather Event")
print(HealthPlot)
```

![](Course_Assignment_2_files/figure-html/Public_Health-1.png)<!-- -->

We notice that Tornados and Lightening are by far the most responsible weather events for injuries and are also the identifiable events most responsible for fatalities, slightly lead by “Other Events.

## Economic Effects of Weather Events


```r
# generate average value of property and crop dammage for each category of weather event
economy <- data %>%
group_by(EVTYPE) %>%
summarise(AveragePropertyDammage = mean(Property.Damage), AverageCropDamage = mean(Crop.Damage))
```

```
`summarise()` ungrouping output (override with `.groups` argument)
```

```r
# creat a sub dataframe for average property dammage only
econ1 <- data.frame(Event = economy$EVTYPE, Type = "Property Damage", Effect = economy$AveragePropertyDammage)
# create a sub data frame for average crop dammage only
econ2 <- data.frame(Event = economy$EVTYPE, Type = "Crop Damage", Effect = economy$AverageCropDamage)
# combine average property and crop dammage data frames into one
EconomicEffects <- rbind(econ1, econ2)
# generate basic esthetics for mapping coordinates of the plot
EconomicPlot <- ggplot(EconomicEffects, aes(Event, Effect, fill = Type)) +
# Add bar graphs to the plot
geom_bar(stat = "identity") +
# Add labels to the plot
labs(title = "Economic Effects of Weather Events in the United States",
subtitle = "1950 to 2011",
x = "Weather Event",
tag = "Figure 2",
y = "Natural Log of Average Loss in Dollars per Weather Event")
print(EconomicPlot)
```

![](Course_Assignment_2_files/figure-html/Economic_Effects-1.png)<!-- -->

Hear, we see that Floods inflict the most damage to both property and crops, while followed by Tornados for property and Hail for crops.

- - -
