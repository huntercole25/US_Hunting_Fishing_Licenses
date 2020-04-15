library(data.table)
library(stringi)
library(tidyr)

####Aggregating Census Data by State and Country####
RawPops <- fread("./StatePopulations_txt/StatePopsFull_Annual.txt")
RawPops[,DATE := regmatches(RawPops$DATE, regexpr("^\\d{4}", RawPops$DATE))]
setnames(RawPops, old = names(RawPops[,2:length(RawPops)]), new = substr(names(RawPops[,2:length(RawPops)]), 1, 2))

GatheredData <- as.data.table(gather(RawPops, key = "State", value = "Population_in_Thousands", 2:length(RawPops)))
setnames(GatheredData, old = "DATE", new = "Year")

write.csv(GatheredData, "./StatePopulations_txt/GatheredStateData.csv", row.names = F)

TotalUs <- aggregate(Population_in_Thousands ~ Year, data = GatheredData, FUN = sum)

write.csv(TotalUs, "./StatePopulations_txt/TotalUsbyYear.csv", row.names = F)

####Hunting Licenses####
FileNames <- list.files("~/Documents/HuntingLicenseCsvs", full.names = T)

States <- data.table(Name = state.name, Abbr = state.abb)

HuntingList <- list()

YearAppend <- function(filename, licensetype, emptylist){
  tmp <- fread(filename, header = F)
  tmp <- tmp[!tmp[[1]] == "" & !tmp[[1]]=="Total:"]
  
  if(nchar(tmp[,1])[[1]] < 1 | all(is.na(tmp[[1]]))){
    tmp[,1] <- NULL
  }
  
  if(length(tmp) > 2){
    if(nchar(tmp[,3])[[1]] < 1 | all(is.na(tmp[[3]]))){
    tmp[,3] <- NULL
    }
  }
  
  base <- basename(filename)
  
  year <- sub("(\\d+)\\.{1}[[:alnum:]]+$", "\\1", base)
  
  if(median(stri_length(tmp[[1]])) > 2){
    setnames(tmp, c("StateName", "LicenseCount"))
  }else{
    setnames(tmp, c("StateAbbr", "LicenseCount"))
  }
  
  tmp[,LicenseCount := as.numeric(gsub(",", "", LicenseCount))]
  tmp[,Year := year]
  
  if("StateName" %in% names(tmp)){
    tmp <- tmp[StateName %in% States$Name]
    tmp <- merge(tmp, States, by.x = "StateName", by.y = "Name")
    setnames(tmp, old = "Abbr", new = "StateAbbr")
  }else{
    tmp <- tmp[StateAbbr %in% States$Abbr]
    tmp <- merge(tmp, States, by.x = "StateAbbr", by.y = "Abbr")
    setnames(tmp, old = "Name", new = "StateName")
  }
  
  tmp[,Type := licensetype]
  setcolorder(tmp, c("StateAbbr", "StateName", "Year", "Type", "LicenseCount"))
  
  TmpList <- get(deparse(substitute(emptylist)))
  ComboList <- c(list(tmp), TmpList)
  
  assign(deparse(substitute(emptylist)), ComboList, pos = .GlobalEnv)
}

lapply(X = FileNames, FUN = YearAppend, licensetype = "Hunting", emptylist = HuntingList)

FullHuntingList <- rbindlist(HuntingList)

write.csv(FullHuntingList, "./HuntingLicensesFull.csv", row.names = F)
