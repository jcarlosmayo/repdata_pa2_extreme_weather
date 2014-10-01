---
title: "Health and economic impact of weather events"
author: "Juan Carlos Mayo Martinez"
output: html_document
---
**Title:** Your document should have a title that briefly summarizes your data analysis  

**Synopsis:** Immediately after the title, there should be a synopsis which describes and summarizes your analysis in at most 10 complete sentences.  

**Data Processing** which describes (in words and code) how the data were loaded into R and processed for analysis. In particular, your analysis must start from the raw CSV file containing the data.
You cannot do any preprocessing outside the document. If preprocessing is time-consuming you may consider using the cache = TRUE option for certain code chunks. 

**Results** in which your results are presented.

**Plot number**. The analysis document must have at least one figure containing a plot. Your analyis must have no more than three figures. Figures may have multiple plots in them (i.e. panel plots), but there cannot be more than three figures total.


Your data analysis must address the following questions:

- Across the United States, which types of events (as indicated in the **EVTYPE** variable) are most harmful with respect to **population health**?
FATALITIES, INJURIES

- Across the United States, which types of events have the greatest economic consequences?


Consider writing your report as if it were to be read by a government or municipal manager who might be responsible for preparing for severe weather events and will need to prioritize resources for different types of events. However, there is no need to make any specific recommendations in your report.

# Data processing

```r
# Read in data
storm_data <- read.csv("repdata_data_StormData.csv.bz2", stringsAsFactors = FALSE)
# Set all variable names to lower caps
names(storm_data) <- tolower(names(storm_data))
```

The original data set contains 37 variables; however, not all of them are required for the purpose of this assignment so a new data frame will be created containing the 8 variables needed. Within the new data site a new variable will be created using the date of the event to display only the year.

```r
# Create a new data frame using only the variables needed
data <- subset(storm_data, select=c("bgn_date", "evtype", "fatalities", "injuries",
                                    "propdmg", "propdmgexp", "cropdmg", "cropdmgexp"))

# Extract the year to another variable
data$year <- as.numeric(substr(strptime(data$bgn_date, "%m/%d/%Y"), 0, 4))
```

There are 985 different categories of event types in the data set. However, after looking at their values it becomes clear that they can be regrouped into smaller groups. This is due to the fact that the categorization of events does not follow a strict system. There are clarifications, abbreviations and typos that capture the same event, for example thunderstorms are referred to as "THUNDERSTORMS", "GUSTY THUNDERSTORM WIND" and "TSTM". To reduce the number of categories regular expressions can be used first to determine key words associated with different events and second to rename the event type field. For example, keywords like "rain", "storm" and "shower" can be used to determine a storm event that can be renamed simply "Rain" in all cases. Likewise, occurrences of the strings "flood", "flooo", "fld" and "stream" can be renamed simply "Flood".

After examining the different event types collected we can re-assigned them to one of 13 possible groups that correspond to the most common event types, while those that did not correspond to any of the new created categories where assigned to "Other". On the one hand, this results in an oversimplification of facts. For example, cases of snow are grouped under "Cold weather" event types. On the other hand, this is necessary to provide an initial overall understanding that could be later expanded by analzying specific cases alone.

```r
thunderstorm <- grepl("thunder|thude|tstm", ignore.case = TRUE, data$evtype)
data$evtype <- ifelse(thunderstorm == TRUE, "Thunderstorm", data$evtype)

flood <- grepl("flood|flooo|fld|stream", ignore.case = TRUE, data$evtype)
data$evtype <- ifelse(flood == TRUE, "Flood", data$evtype)

tornado <- grepl("tornado|torndao", ignore.case = TRUE, data$evtype)
data$evtype <- ifelse(tornado == TRUE, "Tornado", data$evtype)

rain <- grepl("rain|storm|shower", ignore.case = TRUE, data$evtype)
data$evtype <- ifelse(rain == TRUE, "Rain", data$evtype)

wind <- grepl("wind|gust|hurr|typ", ignore.case = TRUE, data$evtype)
data$evtype <- ifelse(wind == TRUE, "Wind", data$evtype)

heat <- grepl("heat|warm|hot|dry|drought|high", ignore.case = TRUE, data$evtype)
data$evtype <- ifelse(heat == TRUE, "Hot weather", data$evtype)

cold <- grepl("cold|chill|freez|ice|icy|snow|wint|blizzard|cool|frost|low", ignore.case = TRUE, data$evtype)
data$evtype <- ifelse(cold == TRUE, "Cold weather", data$evtype)

fire <- grepl("fire|smoke", ignore.case = TRUE, data$evtype)
data$evtype <- ifelse(fire == TRUE, "Fire", data$evtype)

# Group all other events into 'other'
clean_events <- c("Thunderstorm", "Flood", "Tornado", "Rain", "Wind","Hot weather", "Cold weather", "Fire")

data$evtype <-ifelse(data$evtype %in% clean_events, data$evtype, "Other")
```

While fatalities and injuries have been entered using actual figures, property and crop damages must be converted to their actual figure using the variable 'propdmgexp' and 'cropdmgexp' respectively. These variables consist, except in a few cases that will be overlooked here, of a string character that determines whether the figure provided corresponds to hundreds, "H", thousands, "K", millions, "M" or billions, "B". Combining these we can get the actual property and damage figures, as well as create a variable that sums them together, "total_losses".

```r
# Property and crops damage totals
data$prop_total_damage <- ifelse(data$propdmgexp=="H"|data$propdmgexp=="h", data$propdmg * 100,
                                 ifelse(data$propdmgexp=="K", data$propdmg * 1000,
                                        ifelse(data$propdmgexp=="M"|data$propdmgexp=="m", data$propdmg * 1000000,
                                               ifelse(data$propdmgexp=="B", data$propdmg * 1000000000, data$propdmg))))

data$crop_total_damage <- ifelse(data$cropdmgexp=="k", data$cropdmg * 1000,
                                 ifelse(data$cropdmgexp=="K", data$cropdmg * 1000,
                                        ifelse(data$cropdmgexp=="M", data$cropdmg * 1000000,
                                               ifelse(data$cropdmgexp=="m", data$cropdmg * 1000000, data$cropdmg))))

data$total_losses <- data$prop_total_damage + data$crop_total_damage
```

We can then proceed to create to grouped sets of data, one by event type and year and another one only by event types.

```r
library(dplyr)
evtype_year_groups <- group_by(data, year, evtype)

evtype_year_data <- summarise(evtype_year_groups,
                          fatalities = sum(fatalities),
                          injuries = sum(injuries),
                          propdmg = sum(prop_total_damage),
                          cropdmg = sum(crop_total_damage),
                          totaldmg = sum(total_losses),
                          n = n())

evtype_groups <- group_by(data, evtype)

evtype_data <- summarise(evtype_groups,
                          fatalities = sum(fatalities),
                          injuries = sum(injuries),
                          propdmg = sum(prop_total_damage),
                          cropdmg = sum(crop_total_damage),
                          totaldmg = sum(total_losses),
                          n = n())
```

# Results

## Most harmful events to population health

```r
# Factor the event type variable and arrange the levels.
evtype_data$evtype <- factor(evtype_data$evtype, levels = c("Other", "Fire","Wind",
                                                            "Cold weather", "Flood", "Hot weather","Rain", "Tornado"))

library(ggplot2)
library(gridExtra)
injuries = ggplot(evtype_data, aes(x=evtype, y=injuries)) +
        geom_bar(stat="identity", fill="steelblue") +
        coord_flip() +
        ylab("Number of people") +
        xlab("Event type") +
        ggtitle("Injuries")

fatalities = ggplot(evtype_data, aes(x=evtype, y=fatalities)) +
        geom_bar(stat="identity", fill="#ca8d4f") +
        coord_flip() +
        ylab("Number of people") +
        xlab("") +
        ggtitle("Fatalities")

grid.arrange(injuries, fatalities, ncol=2, main = "People affected")
```

<img src="figure/population health.png" title="plot of chunk population health" alt="plot of chunk population health" style="display: block; margin: auto;" />

## Economic consequences of weather events

```r
property = ggplot(evtype_data, aes(x=evtype, y=propdmg/1000000)) +
        geom_bar(stat="identity", fill="steelblue") +
        coord_flip() +
        ylab("Losses (million dollars") +
        xlab("Event type") +
        ggtitle("Property damages")

crops = ggplot(evtype_data, aes(x=evtype, y=cropdmg/1000000)) +
        geom_bar(stat="identity", fill="#ca8d4f") +
        coord_flip() +
        ylab("Losses (million dollars)") +
        xlab("") +
        ggtitle("Crop damages")

total = ggplot(evtype_data, aes(x=evtype, y=totaldmg/1000000000)) +
        geom_bar(stat="identity", fill="#ca8d4f") +
        coord_flip() +
        ylab("Losses (billion dollars)") +
        xlab("") +
        ggtitle("Total damages")

# Print several plots
pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.5, 5, 5), "null"))))

print(property, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(crops, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(total, vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2))
grid.text("Economic damages", vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
```

![plot of chunk economic consequences](figure/economic consequences.png) 

## Event frequency

```r
ggplot(evtype_year_data, aes(x=year, y=n, color=evtype)) +
        geom_line()
```

![plot of chunk event frequency](figure/event frequency.png) 

