# Data Paper Reboot - FERC Data
# Author: Debs
# dmaiasil@purdue.edu

getSeason <- function(date) {
  
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(date, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE,("Winter"),
          ifelse (d >= SE & d < SS, return("Spring"),
                  ifelse (d >= SS & d < FE, return("Summer"), return("Fall"))))
}

library(dplyr)
library(stringr)
library(tidyr)

#raw data downloaded from: 09/14/20 https://www.ferc.gov/industries-data/electric/general-information/electric-industry-forms/form-no-714-annual-electric/data 

# respondent_id : respondent_id, respondent_name, eia_code
respondents_id <- read.table("Respondent IDs.csv", header = T, sep = ',')

# data 2006-2019
ferc_demand <- read.table("Part 3 Schedule 2 - Planning Area Hourly Demand.csv", sep=',', h=T, stringsAsFactors = F)

# adjusting the date column
ferc_demand$plan_date <- as.Date(ferc_demand$plan_date, '%m/%d/%Y')

# selecting columns
ferc_demand <- cbind(ferc_demand[, c("respondent_id","report_yr","plan_date")], ferc_demand[,8:31])

# the only one not in respondents_id is id 2 which is a test (all zeroes as demand)
# adding respondent_name and eia_code to the dataset
ferc_demand <- merge(x = ferc_demand, y = respondents_id, by = "respondent_id")

# organizing order of columns for better visual inspection
ferc_demand <- ferc_demand[,c(1,28,29,2:27)]

# excluding any respondents with "test" in their name - Test Company A, B C ....
ferc_demand <- ferc_demand%>%
  filter(!grepl('Test', respondent_name))

# there are no missing values 
# ferc_complete <- ferc_demand[complete.cases(ferc_demand),]

# days reported with zero load were not excluded 

# negative demand reflect 'Other generation within the balancing authority area not 
# reported here and thus not included within the total reported output would then presumably
# be reflected in the demand as "negative demand."' (form 714 instructions)
# based on that, we will exclude any days that reported negative values
# 2931 rows with negative values
# https://stackoverflow.com/questions/19665929/selecting-rows-based-on-multiple-columns-in-r
ferc_demand <- ferc_demand[rowSums(sapply(ferc_demand[,c(6:29)],`<`,e2=0)) == 0,]


# check rows with missing EIA code from the final ferc data
# delete those entries 
#no_eia <- ferc_demand[ferc_demand$eia_code == 0 ,]
#table(no_eia$respondent_id)
ferc_demand <- ferc_demand[ferc_demand$eia_code != 0 ,]

# for a better analysis, split dataset by year
ferc_year <- split(ferc_demand, f = ferc_demand$report_yr)

#### 2006 ####

file4_2006 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file4_2006.csv", sep = ",", header = T)
file1_2006 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file1_cao_2006.csv", sep = ",", header = T)


# in file4, utility_ID == EIA_code on ferc_demand
colnames(file4_2006)[2] <- "eia_code"

file1_2006 <- file1_2006[, 2:3]
colnames(file1_2006) <- c("eia_utility","eia_BA")

# analysis of territories that do not have a direct match in our ferc dataset
# the other files would just merge and then we would have the locations
no_match_2006 <- anti_join(ferc_year$`2006`, file4_2006, by="eia_code") # no_match 26644 rows

# from the no_matches, some of them are BAs that we need to break into
# utilities usinf file1
colnames(no_match_2006)[3] <- "eia_BA" # changing colname for join

no_match_2006 <- left_join(no_match_2006, file1_2006, by = "eia_BA") # no_match 524504 rows

# rows with NAs in utility are the rows that are still no_match
still_no_match_2006 <- no_match_2006[is.na(no_match_2006$eia_utility),] # still_no 17154 rows

# BAs that were completed (No NAs)
no_match_2006 <- no_match_2006 %>% drop_na() # no_match 507350 rows0

# checking with string match if theres anyone
table(still_no_match_2006$respondent_name)

# better analysis with excel for visual inspection
write.csv(still_no_match_2006, file = "snm_2006.csv",row.names = F)

# for integrity of the data, manual matching while checking in the companies' website can have multiple problems
# the main one being not knowing the years such changes happened in each company
# I do not wish to dive that deep in years of reports for the public companies
# to match exactly each of the counties
# so the utilities not registered in file4 or file1 will be excluded

# after visual inspection, changes to be made:
# 1: Alliant Energy-West == Alliant West from file1; changing BA code to 193 to find a match in file 1
still_no_match_2006$eia_BA[still_no_match_2006$respondent_id == 106] <- 193
# 2: Duke Energy Corp. == Duke Energy Corporation in file1; changing BA code to 5416 to find a match in file 1
still_no_match_2006$eia_BA[still_no_match_2006$respondent_id == 134] <- 5416
# 3: Great River Energy == Great River Energy in file1; changing BA code to 7570 to find a match in file 1
still_no_match_2006$eia_BA[still_no_match_2006$respondent_id == 175] <- 7570
# 4: Louisiana Generating == Louisiana Generating LLC in file1; changing BA code to 2777 to find a match in file 1
still_no_match_2006$eia_BA[still_no_match_2006$respondent_id == 196] <- 2777
# 5: MidAmerican Energy Company == MidAmerican Energy Company && MidAmerican Energy Co in file1; changing BA code to 12341 to find a match in file 1
still_no_match_2006$eia_BA[still_no_match_2006$respondent_id == 203] <- 12341


# in still no match: exclude utility_eia column, rematch with file1 and select complete cases 
still_no_match_2006 <- still_no_match_2006[,-30]
still_no_match_2006 = left_join(still_no_match_2006, file1_2006, by = "eia_BA")
still_no_match_2006 <- still_no_match_2006 %>% drop_na()

# to merge (still_no + no_match + ferc_year) -> file4 (there should be no NAs)
# final detaset for 2006 is that final merged file. write CSV 
no_match_2006 <- no_match_2006[,-3]
no_match_2006 <- no_match_2006[,c(1,2,29,3:28)]
colnames(no_match_2006)[3] <- "eia_code"
still_no_match_2006 <- still_no_match_2006[,-3]
still_no_match_2006 <- still_no_match_2006[,c(1,2,29,3:28)]
colnames(still_no_match_2006)[3] <- "eia_code"

complete_2006 <- rbind(ferc_year$`2006`, no_match_2006, still_no_match_2006)


# new columns added
complete_2006$DailyPeak <- do.call(pmax, complete_2006[6:29])
complete_2006$PeakHour <- apply(complete_2006[,6:29], 1, which.max)
complete_2006$DayWeek <- weekdays(as.Date(complete_2006$plan_date))
complete_2006$Season <- apply(complete_2006[5], 1, getSeason)

complete_2006 = left_join(complete_2006, file4_2006, by = "eia_code")
complete_2006 <- complete_2006[,-34]


complete_2006 <- complete_2006 %>% drop_na()
complete_2006 <- complete_2006[,-34]
write.csv(complete_2006, file = "ferc_2006.csv",row.names = F)

#### 2007 ####

file4_2007 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file4_2007.csv", sep = ",", header = T)
file1_2007 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file1_cao_2007.csv", sep = ",", header = T)

colnames(file4_2007)[2] <- "eia_code"

file1_2007 <- file1_2007[, 2:3]
colnames(file1_2007) <- c("eia_utility","eia_BA")

no_match_2007 <- anti_join(ferc_year$`2007`, file4_2007, by="eia_code") 

colnames(no_match_2007)[3] <- "eia_BA"

no_match_2007 <- left_join(no_match_2007, file1_2007, by = "eia_BA") 

still_no_match_2007 <- no_match_2007[is.na(no_match_2007$eia_utility),] 

no_match_2007 <- no_match_2007 %>% drop_na() 


# checking with string match if theres anyone
table(still_no_match_2007$respondent_name)

# better analysis with excel for visual inspection
write.csv(still_no_match_2007, file = "snm_2007.csv",row.names = F)

# 1: Alliant Energy-West == Alliant West from file1; changing BA code to 193 to find a match in file 1
still_no_match_2007$eia_BA[still_no_match_2007$respondent_id == 106] <- 193
# 2: Duke Energy Corp. == Duke Energy Corporation in file1; changing BA code to 5416 to find a match in file 1
still_no_match_2007$eia_BA[still_no_match_2007$respondent_id == 134] <- 5416
# 3: Great River Energy == Great River Energy in file1; changing BA code to 7570 to find a match in file 1
still_no_match_2007$eia_BA[still_no_match_2007$respondent_id == 175] <- 7570
# 4: MidAmerican Energy Company == MidAmerican Energy Company && MidAmerican Energy Co in file1; changing BA code to 12341 to find a match in file 1
still_no_match_2007$eia_BA[still_no_match_2007$respondent_id == 203] <- 12341

still_no_match_2007 <- still_no_match_2007[,-30]
still_no_match_2007 = left_join(still_no_match_2007, file1_2007, by = "eia_BA")
still_no_match_2007 <- still_no_match_2007 %>% drop_na()

no_match_2007 <- no_match_2007[,-3]
no_match_2007 <- no_match_2007[,c(1,2,29,3:28)]
colnames(no_match_2007)[3] <- "eia_code"
still_no_match_2007 <- still_no_match_2007[,-3]
still_no_match_2007 <- still_no_match_2007[,c(1,2,29,3:28)]
colnames(still_no_match_2007)[3] <- "eia_code"

complete_2007 <- rbind(ferc_year$`2007`, no_match_2007, still_no_match_2007)

##
complete_2007$DailyPeak <- do.call(pmax, complete_2007[6:29])
complete_2007$PeakHour <- apply(complete_2007[,6:29], 1, which.max)
complete_2007$DayWeek <- weekdays(as.Date(complete_2007$plan_date))
complete_2007$Season <- apply(complete_2007[5], 1, getSeason)

complete_2007 = left_join(complete_2007, file4_2007, by = "eia_code")
complete_2007 <- complete_2007[,-34]


complete_2007 <- complete_2007 %>% drop_na()
complete_2007 <- complete_2007[,-34]

write.csv(complete_2007, file = "ferc_2007.csv",row.names = F)

#### 2008 ####
file4_2008 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file4_2008.csv", sep = ",", header = T)
file1_2008 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file1_cao_2008.csv", sep = ",", header = T)

colnames(file4_2008)[2] <- "eia_code"

file1_2008 <- file1_2008[, 2:3]
colnames(file1_2008) <- c("eia_utility","eia_BA")

no_match_2008 <- anti_join(ferc_year$`2008`, file4_2008, by="eia_code") # no_match 20862 rows

colnames(no_match_2008)[3] <- "eia_BA"

no_match_2008 <- left_join(no_match_2008, file1_2008, by = "eia_BA") 

still_no_match_2008 <- no_match_2008[is.na(no_match_2008$eia_utility),] 

no_match_2008 <- no_match_2008 %>% drop_na() # no_match 439035 rows


# checking with string match if theres anyone
table(still_no_match_2008$respondent_name)

# better analysis with excel for visual inspection
write.csv(still_no_match_2008, file = "snm_2008.csv",row.names = F)

# 1: Alliant Energy-West == Alliant West from file1; changing BA code to 193 to find a match in file 1
still_no_match_2008$eia_BA[still_no_match_2008$respondent_id == 106] <- 193
# 2: Duke Energy Corp. == Duke Energy Corporation in file1; changing BA code to 5416 to find a match in file 1
still_no_match_2008$eia_BA[still_no_match_2008$respondent_id == 134] <- 5416
# 3: Great River Energy == Great River Energy in file1; changing BA code to 7570 to find a match in file 1
still_no_match_2008$eia_BA[still_no_match_2008$respondent_id == 175] <- 7570
# 4: MidAmerican Energy Company == MidAmerican Energy Company && MidAmerican Energy Co in file1; changing BA code to 12341 to find a match in file 1
still_no_match_2008$eia_BA[still_no_match_2008$respondent_id == 203] <- 12341


still_no_match_2008 <- still_no_match_2008[,-30]
still_no_match_2008 = left_join(still_no_match_2008, file1_2008, by = "eia_BA")
still_no_match_2008 <- still_no_match_2008 %>% drop_na()

no_match_2008 <- no_match_2008[,-3]
no_match_2008 <- no_match_2008[,c(1,2,29,3:28)]
colnames(no_match_2008)[3] <- "eia_code"
still_no_match_2008 <- still_no_match_2008[,-3]
still_no_match_2008 <- still_no_match_2008[,c(1,2,29,3:28)]
colnames(still_no_match_2008)[3] <- "eia_code"

complete_2008 <- rbind(ferc_year$`2008`, no_match_2008, still_no_match_2008)

##
complete_2008$DailyPeak <- do.call(pmax, complete_2008[6:29])
complete_2008$PeakHour <- apply(complete_2008[,6:29], 1, which.max)
complete_2008$DayWeek <- weekdays(as.Date(complete_2008$plan_date))
complete_2008$Season <- apply(complete_2008[5], 1, getSeason)

complete_2008 = left_join(complete_2008, file4_2008, by = "eia_code")
complete_2008 <- complete_2008[,c(-34, -35)]


complete_2008 <- complete_2008 %>% drop_na()

write.csv(complete_2008, file = "ferc_2008.csv",row.names = F)

#### 2009 ####
file4_2009 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file4_2009.csv", sep = ",", header = T)
file1_2009 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file1_cao_2009.csv", sep = ",", header = T)

colnames(file4_2009)[2] <- "eia_code"

file1_2009 <- file1_2009[, 2:3]
colnames(file1_2009) <- c("eia_utility","eia_BA")

no_match_2009 <- anti_join(ferc_year$`2009`, file4_2009, by="eia_code") # no_match 20862 rows

colnames(no_match_2009)[3] <- "eia_BA"

no_match_2009 <- left_join(no_match_2009, file1_2009, by = "eia_BA") 

still_no_match_2009 <- no_match_2009[is.na(no_match_2009$eia_utility),] 

no_match_2009 <- no_match_2009 %>% drop_na() # no_match 439035 rows

# checking with string match if theres anyone
table(still_no_match_2009$respondent_name)

# better analysis with excel for visual inspection
write.csv(still_no_match_2009, file = "snm_2009.csv",row.names = F)


# 1: MidAmerican Energy Company == MidAmerican Energy Company && MidAmerican Energy Co in file1; changing BA code to 12341 to find a match in file 1
still_no_match_2009$eia_BA[still_no_match_2009$respondent_id == 203] <- 12341

still_no_match_2009 <- still_no_match_2009[,-30]
still_no_match_2009 = left_join(still_no_match_2009, file1_2009, by = "eia_BA")
still_no_match_2009 <- still_no_match_2009 %>% drop_na()

no_match_2009 <- no_match_2009[,-3]
no_match_2009 <- no_match_2009[,c(1,2,29,3:28)]
colnames(no_match_2009)[3] <- "eia_code"
still_no_match_2009 <- still_no_match_2009[,-3]
still_no_match_2009 <- still_no_match_2009[,c(1,2,29,3:28)]
colnames(still_no_match_2009)[3] <- "eia_code"

complete_2009 <- rbind(ferc_year$`2009`, no_match_2009, still_no_match_2009)

##
complete_2009$DailyPeak <- do.call(pmax, complete_2009[6:29])
complete_2009$PeakHour <- apply(complete_2009[,6:29], 1, which.max)
complete_2009$DayWeek <- weekdays(as.Date(complete_2009$plan_date))
complete_2009$Season <- apply(complete_2009[5], 1, getSeason)

complete_2009 = left_join(complete_2009, file4_2009, by = "eia_code")
complete_2009 <- complete_2009[,c(-34, -35)]


complete_2009 <- complete_2009 %>% drop_na()

write.csv(complete_2009, file = "ferc_2009.csv",row.names = F)

#### 2010 ####
file4_2010 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file4_2010.csv", sep = ",", header = T)
file1_2010 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file1_cao_2010.csv", sep = ",", header = T)

colnames(file4_2010)[2] <- "eia_code"

file1_2010 <- file1_2010[, 2:3]
colnames(file1_2010) <- c("eia_utility","eia_BA")

no_match_2010 <- anti_join(ferc_year$`2010`, file4_2010, by="eia_code") # no_match 20862 rows

colnames(no_match_2010)[3] <- "eia_BA"

no_match_2010 <- left_join(no_match_2010, file1_2010, by = "eia_BA") 

still_no_match_2010 <- no_match_2010[is.na(no_match_2010$eia_utility),] 

no_match_2010 <- no_match_2010 %>% drop_na() # no_match 439035 rows

# checking with string match if theres anyone
table(still_no_match_2010$respondent_name)

# better analysis with excel for visual inspection
write.csv(still_no_match_2010, file = "snm_2010.csv",row.names = F)

still_no_match_2010 <- still_no_match_2010[,-30]
still_no_match_2010 = left_join(still_no_match_2010, file1_2010, by = "eia_BA")
still_no_match_2010 <- still_no_match_2010 %>% drop_na()

no_match_2010 <- no_match_2010[,-3]
no_match_2010 <- no_match_2010[,c(1,2,29,3:28)]
colnames(no_match_2010)[3] <- "eia_code"
still_no_match_2010 <- still_no_match_2010[,-3]
still_no_match_2010 <- still_no_match_2010[,c(1,2,29,3:28)]
colnames(still_no_match_2010)[3] <- "eia_code"

complete_2010 <- rbind(ferc_year$`2010`, no_match_2010, still_no_match_2010)

##
complete_2010$DailyPeak <- do.call(pmax, complete_2010[6:29])
complete_2010$PeakHour <- apply(complete_2010[,6:29], 1, which.max)
complete_2010$DayWeek <- weekdays(as.Date(complete_2010$plan_date))
complete_2010$Season <- apply(complete_2010[5], 1, getSeason)

complete_2010 = left_join(complete_2010, file4_2010, by = "eia_code")
complete_2010 <- complete_2010[,c(-34, -35)]


complete_2010 <- complete_2010 %>% drop_na()

write.csv(complete_2010, file = "ferc_2010.csv",row.names = F)

#### 2011 ####

file4_2011 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file4_2011.csv", sep = ",", header = T)
file1_2011 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file1_cao_2011.csv", sep = ",", header = T)

colnames(file4_2011)[2] <- "eia_code"

file1_2011 <- file1_2011[, 2:3]
colnames(file1_2011) <- c("eia_utility","eia_BA")

no_match_2011 <- anti_join(ferc_year$`2011`, file4_2011, by="eia_code") # no_match 20862 rows

colnames(no_match_2011)[3] <- "eia_BA"

no_match_2011 <- left_join(no_match_2011, file1_2011, by = "eia_BA") 

still_no_match_2011 <- no_match_2011[is.na(no_match_2011$eia_utility),] 

no_match_2011 <- no_match_2011 %>% drop_na() # no_match 439035 rows

# checking with string match if theres anyone
table(still_no_match_2011$respondent_name)

# better analysis with excel for visual inspection
write.csv(still_no_match_2011, file = "snm_2011.csv",row.names = F)

# 1: City of West Memphis == City of West Memphis from file1; changing BA code to 20382 to find a match in file 1
still_no_match_2011$eia_BA[still_no_match_2011$respondent_id == 292] <- 20382

still_no_match_2011 <- still_no_match_2011[,-30]
still_no_match_2011 = left_join(still_no_match_2011, file1_2011, by = "eia_BA")
still_no_match_2011 <- still_no_match_2011 %>% drop_na()

no_match_2011 <- no_match_2011[,-3]
no_match_2011 <- no_match_2011[,c(1,2,29,3:28)]
colnames(no_match_2011)[3] <- "eia_code"
still_no_match_2011 <- still_no_match_2011[,-3]
still_no_match_2011 <- still_no_match_2011[,c(1,2,29,3:28)]
colnames(still_no_match_2011)[3] <- "eia_code"

complete_2011 <- rbind(ferc_year$`2011`, no_match_2011, still_no_match_2011)

##
complete_2011$DailyPeak <- do.call(pmax, complete_2011[6:29])
complete_2011$PeakHour <- apply(complete_2011[,6:29], 1, which.max)
complete_2011$DayWeek <- weekdays(as.Date(complete_2011$plan_date))
complete_2011$Season <- apply(complete_2011[5], 1, getSeason)

complete_2011 = left_join(complete_2011, file4_2011, by = "eia_code")
complete_2011 <- complete_2011[,c(-34, -35)]


complete_2011 <- complete_2011 %>% drop_na()

write.csv(complete_2011, file = "ferc_2011.csv",row.names = F)

#### 2012 ####

file4_2012 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file4_2012.csv", sep = ",", header = T)
file1_2012 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file1_cao_2012.csv", sep = ",", header = T)

colnames(file4_2012)[2] <- "eia_code"

file1_2012 <- file1_2012[, c(2,4)]
colnames(file1_2012) <- c("eia_utility","eia_BA")

no_match_2012 <- anti_join(ferc_year$`2012`, file4_2012, by="eia_code") # no_match 17568 rows

colnames(no_match_2012)[3] <- "eia_BA"

no_match_2012 <- left_join(no_match_2012, file1_2012, by = "eia_BA") 

still_no_match_2012 <- no_match_2012[is.na(no_match_2012$eia_utility),] 

no_match_2012 <- no_match_2012 %>% drop_na() # no_match 439035 rows

# checking with string match if theres anyone
table(still_no_match_2012$respondent_name)

# better analysis with excel for visual inspection
write.csv(still_no_match_2012, file = "snm_2012.csv",row.names = F)

# 1: City of West Memphis == City of West Memphis from file1; changing BA code to 20382 to find a match in file 1
still_no_match_2012$eia_BA[still_no_match_2012$respondent_id == 292] <- 20382

still_no_match_2012 <- still_no_match_2012[,-30]
still_no_match_2012 = left_join(still_no_match_2012, file1_2012, by = "eia_BA")
still_no_match_2012 <- still_no_match_2012 %>% drop_na()

no_match_2012 <- no_match_2012[,-3]
no_match_2012 <- no_match_2012[,c(1,2,29,3:28)]
colnames(no_match_2012)[3] <- "eia_code"
still_no_match_2012 <- still_no_match_2012[,-3]
still_no_match_2012 <- still_no_match_2012[,c(1,2,29,3:28)]
colnames(still_no_match_2012)[3] <- "eia_code"

complete_2012 <- rbind(ferc_year$`2012`, no_match_2012, still_no_match_2012)

##
complete_2012$DailyPeak <- do.call(pmax, complete_2012[6:29])
complete_2012$PeakHour <- apply(complete_2012[,6:29], 1, which.max)
complete_2012$DayWeek <- weekdays(as.Date(complete_2012$plan_date))
complete_2012$Season <- apply(complete_2012[5], 1, getSeason)

complete_2012 = left_join(complete_2012, file4_2012, by = "eia_code")
complete_2012 <- complete_2012[,c(-34, -35)]


complete_2012 <- complete_2012 %>% drop_na()

write.csv(complete_2012, file = "ferc_2012.csv",row.names = F)

#### 2013 ####

file4_2013 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file4_2013.csv", sep = ",", header = T)
# file1 2013 changed formats, not having the code for the utility anymore. But there's still the code for the BA
# so I joined the 2012 and 2013 files (inner_join) to get the codes for utilities in 2013 that are in common with 2012
# this had already been done in the previous code and I pasted here the final CSV file
file1_2013 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file1_cao_2013.csv", sep = ",", header = T)

colnames(file4_2013)[2] <- "eia_code"

file1_2013 <- file1_2013[, c(2,4)]
colnames(file1_2013) <- c("eia_utility","eia_BA")

no_match_2013 <- anti_join(ferc_year$`2013`, file4_2013, by="eia_code") 

colnames(no_match_2013)[3] <- "eia_BA"

no_match_2013 <- left_join(no_match_2013, file1_2013, by = "eia_BA") 

still_no_match_2013 <- no_match_2013[is.na(no_match_2013$eia_utility),] 

no_match_2013 <- no_match_2013 %>% drop_na() # no_match 439035 rows

# checking with string match if theres anyone
table(still_no_match_2013$respondent_name)

# better analysis with excel for visual inspection
write.csv(still_no_match_2013, file = "snm_2013.csv",row.names = F)

# 1: Basin Electric Power Cooperative == Basin Electric Power Coop from file1; changing BA code to 28503 to find a match in file 1
still_no_match_2013$eia_BA[still_no_match_2013$respondent_id == 287] <- 28503

still_no_match_2013 <- still_no_match_2013[,-30]
still_no_match_2013 = left_join(still_no_match_2013, file1_2013, by = "eia_BA")
still_no_match_2013 <- still_no_match_2013 %>% drop_na()

no_match_2013 <- no_match_2013[,-3]
no_match_2013 <- no_match_2013[,c(1,2,29,3:28)]
colnames(no_match_2013)[3] <- "eia_code"
still_no_match_2013 <- still_no_match_2013[,-3]
still_no_match_2013 <- still_no_match_2013[,c(1,2,29,3:28)]
colnames(still_no_match_2013)[3] <- "eia_code"

complete_2013 <- rbind(ferc_year$`2013`, no_match_2013, still_no_match_2013)

##
complete_2013$DailyPeak <- do.call(pmax, complete_2013[6:29])
complete_2013$PeakHour <- apply(complete_2013[,6:29], 1, which.max)
complete_2013$DayWeek <- weekdays(as.Date(complete_2013$plan_date))
complete_2013$Season <- apply(complete_2013[5], 1, getSeason)

complete_2013 = left_join(complete_2013, file4_2013, by = "eia_code")
complete_2013 <- complete_2013[,c(-34, -35)]


complete_2013 <- complete_2013 %>% drop_na()

write.csv(complete_2013, file = "ferc_2013.csv",row.names = F)

#### 2014 ####
file4_2014 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file4_2014.csv", sep = ",", header = T)
# same pre-processing of the 2013 file
file1_2014 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file1_cao_2014.csv", sep = ",", header = T)

colnames(file4_2014)[2] <- "eia_code"

file1_2014 <- file1_2014[, c(2,4)]
colnames(file1_2014) <- c("eia_utility","eia_BA")

no_match_2014 <- anti_join(ferc_year$`2014`, file4_2014, by="eia_code") 

colnames(no_match_2014)[3] <- "eia_BA"

no_match_2014 <- left_join(no_match_2014, file1_2014, by = "eia_BA") 

still_no_match_2014 <- no_match_2014[is.na(no_match_2014$eia_utility),] 

no_match_2014 <- no_match_2014 %>% drop_na() # no_match 439035 rows

# checking with string match if theres anyone
table(still_no_match_2014$respondent_name)

# better analysis with excel for visual inspection
write.csv(still_no_match_2014, file = "snm_2014.csv",row.names = F)


still_no_match_2014 <- still_no_match_2014[,-30]
still_no_match_2014 = left_join(still_no_match_2014, file1_2014, by = "eia_BA")
still_no_match_2014 <- still_no_match_2014 %>% drop_na()

no_match_2014 <- no_match_2014[,-3]
no_match_2014 <- no_match_2014[,c(1,2,29,3:28)]
colnames(no_match_2014)[3] <- "eia_code"
still_no_match_2014 <- still_no_match_2014[,-3]
still_no_match_2014 <- still_no_match_2014[,c(1,2,29,3:28)]
colnames(still_no_match_2014)[3] <- "eia_code"

complete_2014 <- rbind(ferc_year$`2014`, no_match_2014, still_no_match_2014)

##
complete_2014$DailyPeak <- do.call(pmax, complete_2014[6:29])
complete_2014$PeakHour <- apply(complete_2014[,6:29], 1, which.max)
complete_2014$DayWeek <- weekdays(as.Date(complete_2014$plan_date))
complete_2014$Season <- apply(complete_2014[5], 1, getSeason)

complete_2014 = left_join(complete_2014, file4_2014, by = "eia_code")
complete_2014 <- complete_2014[,c(-34, -35)]


complete_2014 <- complete_2014 %>% drop_na()

write.csv(complete_2014, file = "ferc_2014.csv",row.names = F)

#### 2015 ####
file4_2015 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file4_2015.csv", sep = ",", header = T)
# same pre-processing of the 2013 file
file1_2015 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file1_cao_2015.csv", sep = ",", header = T)

colnames(file4_2015)[2] <- "eia_code"

file1_2015 <- file1_2015[, c(2,4)]
colnames(file1_2015) <- c("eia_utility","eia_BA")

no_match_2015 <- anti_join(ferc_year$`2015`, file4_2015, by="eia_code") 

colnames(no_match_2015)[3] <- "eia_BA"

no_match_2015 <- left_join(no_match_2015, file1_2015, by = "eia_BA") 

still_no_match_2015 <- no_match_2015[is.na(no_match_2015$eia_utility),] 

no_match_2015 <- no_match_2015 %>% drop_na() # no_match 439035 rows

# checking with string match if theres anyone
table(still_no_match_2015$respondent_name)

# better analysis with excel for visual inspection
write.csv(still_no_match_2015, file = "snm_2015.csv",row.names = F)


still_no_match_2015 <- still_no_match_2015[,-30]
still_no_match_2015 = left_join(still_no_match_2015, file1_2015, by = "eia_BA")
still_no_match_2015 <- still_no_match_2015 %>% drop_na()

no_match_2015 <- no_match_2015[,-3]
no_match_2015 <- no_match_2015[,c(1,2,29,3:28)]
colnames(no_match_2015)[3] <- "eia_code"
still_no_match_2015 <- still_no_match_2015[,-3]
still_no_match_2015 <- still_no_match_2015[,c(1,2,29,3:28)]
colnames(still_no_match_2015)[3] <- "eia_code"

complete_2015 <- rbind(ferc_year$`2015`, no_match_2015, still_no_match_2015)

##
complete_2015$DailyPeak <- do.call(pmax, complete_2015[6:29])
complete_2015$PeakHour <- apply(complete_2015[,6:29], 1, which.max)
complete_2015$DayWeek <- weekdays(as.Date(complete_2015$plan_date))
complete_2015$Season <- apply(complete_2015[5], 1, getSeason)

complete_2015 = left_join(complete_2015, file4_2015, by = "eia_code")
complete_2015 <- complete_2015[,c(-34, -35)]


complete_2015 <- complete_2015 %>% drop_na()

write.csv(complete_2015, file = "ferc_2015.csv",row.names = F)

#### 2016 ####
file4_2016 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file4_2016.csv", sep = ",", header = T)
# same pre-processing of the 2013 file
file1_2016 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file1_cao_2016.csv", sep = ",", header = T)

colnames(file4_2016)[2] <- "eia_code"

file1_2016 <- file1_2016[, c(2,4)]
colnames(file1_2016) <- c("eia_utility","eia_BA")

no_match_2016 <- anti_join(ferc_year$`2016`, file4_2016, by="eia_code") 

colnames(no_match_2016)[3] <- "eia_BA"

no_match_2016 <- left_join(no_match_2016, file1_2016, by = "eia_BA") 

still_no_match_2016 <- no_match_2016[is.na(no_match_2016$eia_utility),] 

no_match_2016 <- no_match_2016 %>% drop_na() # no_match 439035 rows

# checking with string match if theres anyone
table(still_no_match_2016$respondent_name)

# better analysis with excel for visual inspection
write.csv(still_no_match_2016, file = "snm_2016.csv",row.names = F)


still_no_match_2016 <- still_no_match_2016[,-30]
still_no_match_2016 = left_join(still_no_match_2016, file1_2016, by = "eia_BA")
still_no_match_2016 <- still_no_match_2016 %>% drop_na()

no_match_2016 <- no_match_2016[,-3]
no_match_2016 <- no_match_2016[,c(1,2,29,3:28)]
colnames(no_match_2016)[3] <- "eia_code"
still_no_match_2016 <- still_no_match_2016[,-3]
still_no_match_2016 <- still_no_match_2016[,c(1,2,29,3:28)]
colnames(still_no_match_2016)[3] <- "eia_code"

complete_2016 <- rbind(ferc_year$`2016`, no_match_2016, still_no_match_2016)

##
complete_2016$DailyPeak <- do.call(pmax, complete_2016[6:29])
complete_2016$PeakHour <- apply(complete_2016[,6:29], 1, which.max)
complete_2016$DayWeek <- weekdays(as.Date(complete_2016$plan_date))
complete_2016$Season <- apply(complete_2016[5], 1, getSeason)

complete_2016 = left_join(complete_2016, file4_2016, by = "eia_code")
complete_2016 <- complete_2016[,c(-34, -35)]


complete_2016 <- complete_2016 %>% drop_na()

write.csv(complete_2016, file = "ferc_2016.csv",row.names = F)

#### 2017 ####

file4_2017 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file4_2017.csv", sep = ",", header = T)

# pre=processing same as 2012 - present
file1_2017 <- read.table("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/Balancing_Authority_2017.csv", sep = ",", header = T, quote = "\"")
colnames(file1_2017)[2] <- "eia_BA"
file1_2017 <- inner_join(file1_2012, file1_2017, by="eia_BA")
file1_2017 <- file1_2017[order(file1_2017$eia_BA),]
file1_2017 <- file1_2017[,c(1,2,6)]

write.csv(file1_2017, file="file1_cao_2017.csv", row.names = F)
file1_2017 <- file1_2017[,c(1,2)]
file1_2017 <- unique(file1_2017)


colnames(file4_2017)[2] <- "eia_code"

no_match_2017 <- anti_join(ferc_year$`2017`, file4_2017, by="eia_code") 

colnames(no_match_2017)[3] <- "eia_BA"

no_match_2017 <- left_join(no_match_2017, file1_2017, by = "eia_BA") 

still_no_match_2017 <- no_match_2017[is.na(no_match_2017$eia_utility),] 

no_match_2017 <- no_match_2017 %>% drop_na() # no_match 439035 rows

# checking with string match if theres anyone
table(still_no_match_2017$respondent_name)

# better analysis with excel for visual inspection
write.csv(still_no_match_2017, file = "snm_2017.csv",row.names = F)


still_no_match_2017 <- still_no_match_2017[,-30]
still_no_match_2017 = left_join(still_no_match_2017, file1_2017, by = "eia_BA")
still_no_match_2017 <- still_no_match_2017 %>% drop_na()

no_match_2017 <- no_match_2017[,-3]
no_match_2017 <- no_match_2017[,c(1,2,29,3:28)]
colnames(no_match_2017)[3] <- "eia_code"
still_no_match_2017 <- still_no_match_2017[,-3]
still_no_match_2017 <- still_no_match_2017[,c(1,2,29,3:28)]
colnames(still_no_match_2017)[3] <- "eia_code"

complete_2017 <- rbind(ferc_year$`2017`, no_match_2017, still_no_match_2017)

##
complete_2017$DailyPeak <- do.call(pmax, complete_2017[6:29])
complete_2017$PeakHour <- apply(complete_2017[,6:29], 1, which.max)
complete_2017$DayWeek <- weekdays(as.Date(complete_2017$plan_date))
complete_2017$Season <- apply(complete_2017[5], 1, getSeason)

complete_2017 = left_join(complete_2017, file4_2017, by = "eia_code")
complete_2017 <- complete_2017[,c(-34, -35)]


complete_2017 <- complete_2017 %>% drop_na()

write.csv(complete_2017, file = "ferc_2017.csv",row.names = F)

#### 2018 ####

file4_2018 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file4_2018.csv", sep = ",", header = T)

# pre=processing same as 2012 - present
file1_2018 <- read.table("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/Balancing_Authority_2018.csv", sep = ",", header = T, quote = "\"")
colnames(file1_2018)[2] <- "eia_BA"
file1_2018 <- inner_join(file1_2012, file1_2018, by="eia_BA")
file1_2018 <- file1_2018[order(file1_2018$eia_BA),]
file1_2018 <- file1_2018[,c(1,2,6)]

write.csv(file1_2018, file="file1_cao_2018.csv", row.names = F)
file1_2018 <- file1_2018[,c(1,2)]
file1_2018 <- unique(file1_2018)


colnames(file4_2018)[2] <- "eia_code"

no_match_2018 <- anti_join(ferc_year$`2018`, file4_2018, by="eia_code") 

colnames(no_match_2018)[3] <- "eia_BA"

no_match_2018 <- left_join(no_match_2018, file1_2018, by = "eia_BA") 

still_no_match_2018 <- no_match_2018[is.na(no_match_2018$eia_utility),] 

no_match_2018 <- no_match_2018 %>% drop_na() # no_match 439035 rows

# checking with string match if theres anyone
table(still_no_match_2018$respondent_name)

# better analysis with excel for visual inspection
write.csv(still_no_match_2018, file = "snm_2018.csv",row.names = F)


still_no_match_2018 <- still_no_match_2018[,-30]
still_no_match_2018 = left_join(still_no_match_2018, file1_2018, by = "eia_BA")
still_no_match_2018 <- still_no_match_2018 %>% drop_na()

no_match_2018 <- no_match_2018[,-3]
no_match_2018 <- no_match_2018[,c(1,2,29,3:28)]
colnames(no_match_2018)[3] <- "eia_code"
still_no_match_2018 <- still_no_match_2018[,-3]
still_no_match_2018 <- still_no_match_2018[,c(1,2,29,3:28)]
colnames(still_no_match_2018)[3] <- "eia_code"

complete_2018 <- rbind(ferc_year$`2018`, no_match_2018, still_no_match_2018)

##
complete_2018$DailyPeak <- do.call(pmax, complete_2018[6:29])
complete_2018$PeakHour <- apply(complete_2018[,6:29], 1, which.max)
complete_2018$DayWeek <- weekdays(as.Date(complete_2018$plan_date))
complete_2018$Season <- apply(complete_2018[5], 1, getSeason)

complete_2018 = left_join(complete_2018, file4_2018, by = "eia_code")
complete_2018 <- complete_2018[,c(-34, -35)]


complete_2018 <- complete_2018 %>% drop_na()

write.csv(complete_2018, file = "ferc_2018.csv",row.names = F)

#### 2019 ####

file4_2019 <- read.csv("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/file4_2019.csv", sep = ",", header = T)

# pre-processing same as 2012 - present
file1_2019 <- read.table("C:/Users/dmaiasil/Documents/Purdue Projects/Data Paper - FERC/eia_territory_files/Balancing_Authority_2019.csv", sep = ",", header = T, quote = "\"")
colnames(file1_2019)[2] <- "eia_BA"
file1_2019 <- inner_join(file1_2012, file1_2019, by="eia_BA")
file1_2019 <- file1_2019[order(file1_2019$eia_BA),]
file1_2019 <- file1_2019[,c(1,2,6)]

write.csv(file1_2019, file="file1_cao_2019.csv", row.names = F)
file1_2019 <- file1_2019[,c(1,2)]
file1_2019 <- unique(file1_2019)


colnames(file4_2019)[2] <- "eia_code"

no_match_2019 <- anti_join(ferc_year$`2019`, file4_2019, by="eia_code") 

colnames(no_match_2019)[3] <- "eia_BA"

no_match_2019 <- left_join(no_match_2019, file1_2019, by = "eia_BA") 

still_no_match_2019 <- no_match_2019[is.na(no_match_2019$eia_utility),] 

no_match_2019 <- no_match_2019 %>% drop_na() # no_match 439035 rows

# checking with string match if theres anyone
table(still_no_match_2019$respondent_name)

# better analysis with excel for visual inspection
write.csv(still_no_match_2019, file = "snm_2019.csv",row.names = F)


still_no_match_2019 <- still_no_match_2019[,-30]
still_no_match_2019 = left_join(still_no_match_2019, file1_2019, by = "eia_BA")
still_no_match_2019 <- still_no_match_2019 %>% drop_na()

no_match_2019 <- no_match_2019[,-3]
no_match_2019 <- no_match_2019[,c(1,2,29,3:28)]
colnames(no_match_2019)[3] <- "eia_code"
still_no_match_2019 <- still_no_match_2019[,-3]
still_no_match_2019 <- still_no_match_2019[,c(1,2,29,3:28)]
colnames(still_no_match_2019)[3] <- "eia_code"

complete_2019 <- rbind(ferc_year$`2019`, no_match_2019, still_no_match_2019)

##
complete_2019$DailyPeak <- do.call(pmax, complete_2019[6:29])
complete_2019$PeakHour <- apply(complete_2019[,6:29], 1, which.max)
complete_2019$DayWeek <- weekdays(as.Date(complete_2019$plan_date))
complete_2019$Season <- apply(complete_2019[5], 1, getSeason)

complete_2019 = left_join(complete_2019, file4_2019, by = "eia_code")
complete_2019 <- complete_2019[,c(-34, -35)]


complete_2019 <- complete_2019 %>% drop_na()

write.csv(complete_2019, file = "ferc_2019.csv",row.names = F)