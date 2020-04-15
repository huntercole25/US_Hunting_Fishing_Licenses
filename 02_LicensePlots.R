library(ggplot2)
library(data.table)
library(lubridate)
library(plotly)

HuntingLicenses <- fread("./HuntingLicensesFull.csv")
StatePops <- fread("./StatePopulations_txt/GatheredStateData.csv")

setnames(HuntingLicenses, old = c("StateAbbr", "StateName", "Year", "Type", "LicenseCount"), new = c("State", "Full State Name", "Year", "License Type", "Licenses Sold"))

HuntingLicenses <- merge(HuntingLicenses, StatePops, by = c("Year", "State"))

HuntingLicenses[,Population := Population_in_Thousands * 1000]

HuntingLicenses[,`Percent Hunters` := `Licenses Sold` / Population * 100]

YearlySum <- aggregate(`Licenses Sold` ~ Year, data = HuntingLicenses, FUN = sum)

UsPopEsts <- fread("./StatePopulations_txt/TotalUsbyYear.csv")

YearlySum <- as.data.table(merge(YearlySum, UsPopEsts, by = "Year"))

YearlySum[,Population := Population_in_Thousands*1000]

YearlySum[,`Percent Hunters` := `Licenses Sold`/Population * 100]

ggplotly(ggplot(data = YearlySum, aes(x = Year, y = `Licenses Sold`)) +
           geom_line()
)

ggplotly(ggplot(data = YearlySum, aes(x = Year, y = `Percent Hunters`)) +
           geom_line())

ggplotly(ggplot(data = HuntingLicenses, aes(x  = Year, y = `Licenses Sold`, color = State)) +
           geom_line()
)

ggplotly(ggplot(data = HuntingLicenses, aes(x = Year, y = `Percent Hunters`, color = State)) + 
  geom_line())
