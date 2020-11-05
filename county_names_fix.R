# fixing county names to be recognized for mapping

county_fix <- function(dataset, old_name, new_name){
  
  dataset$County[dataset$County == old_name] <- new_name
  return(dataset)
  
}

county_fix2 <- function(dataset, old_name, new_name, state){
  
  dataset$County[dataset$County == old_name & dataset$State == state] <- new_name
  return(dataset)
  
}

county_multiple <- function(dataset, old_multi, new1, new2){
  
  data1 <- dataset %>%
    filter(County == old_multi)
  
  
  data2 <- data1
  
  data1$County <- new1
  data2$County <- new2
  
  data1 <- rbind(data1, data2)
  
  dataset <- dataset %>%
    filter(!County == old_multi)
  
  dataset <- rbind(dataset, data1)
  
  return(dataset)
  
}

county_multiple3 <- function(dataset, old_multi, new1, new2, new3){
  
  data1 <- dataset %>%
    filter(County == old_multi)
  
  data2 <- data1
  data3 <- data1
  
  data1$County <- new1
  data2$County <- new2
  data3$County <- new3
  
  data1 <- rbind(data1, data2, data3)
  
  dataset <- dataset %>%
    filter(!County == old_multi)
  
  dataset <- rbind(dataset, data1)
  
  return(dataset)
  
}

county_multiple4 <- function(dataset, old_multi, new1, new2, new3, new4){
  
  data1 <- dataset %>%
    filter(County == old_multi)
  
  
  data2 <- data1
  data3 <- data1
  data4 <- data1
  
  data1$County <- new1
  data2$County <- new2
  data3$County <- new3
  data4$County <- new4
  
  data1 <- rbind(data1, data2, data3, data4)
  
  dataset <- dataset %>%
    filter(!County == old_multi)
  
  dataset <- rbind(dataset, data1)
  
  return(dataset)
  
}

check_rows <- function(data, name){
  
  test <- data %>%
    filter(County == name)
  
  return(nrow(test))
  
}

fix_all <- function(data){
  
  # changes

  data <- county_fix(data, "Anaconda-Dee", "Deer Lodge")
  data <- county_fix(data, "Antim", "Antrim")
  data <- county_fix(data, "Butte-Silver", "Silver Bow")
  data <- county_fix2(data, "Dade", "Miami-Dade", "FL")
  data <- county_fix(data, "Portsmouth", "Newport")
  data <- county_fix(data, "Balto. City", "Baltimore City")
  data <- county_fix(data, "Carol", "Carroll")
  data <- county_fix(data, "Chattahooche", "Chattahoochee")
  data <- county_fix(data, "East Felicia", "East Feliciana")
  data <- county_fix(data, "Fairbault", "Faribault")
  data <- county_fix(data, "Gilford", "Guilford")
  data <- county_fix2(data, "Glasscock", "Glascock", "GA")
  data <- county_fix(data, "Grand Traver", "Grand Traverse")
  data <- county_fix(data, "Humbolt", "Humboldt")
  data <- county_fix(data, "Kossuh", "Kossuth")
  data <- county_fix(data, "Saint Franci", "St. Francis")
  data <- county_fix(data, "San Augustin", "San Augustine")
  data <- county_fix(data, "San Bernadino", "San Bernardino")
  data <- county_fix(data, "Stanley", "Stanly")
  data <- county_fix(data, "Wahkiakurn", "Wahkiakum")
  data <- county_fix(data, "Worchester", "Worcester")
  data <- county_fix(data, "Yellow Medic", "Yellow Medicine")
  data <- county_fix(data, "Cape Girarde", "Cape Girardeau")
  data <- county_fix(data, "Collingswort", "Collingsworth")
  data <- county_fix(data, "Golden Valle", "Golden Valley")
  data <- county_fix(data, "Jefferson Da", "Jefferson Davis")
  data <- county_fix(data, "Lewis and Cl", "Lewis and Clark")
  data <- county_fix(data, "Lousia", "Louisa")
  data <- county_fix(data, "Northumberla", "Northumberland")
  data <- county_fix(data, "Pointe Coupe", "Pointe Coupee")
  data <- county_fix(data, "Pottawattami", "Pottawattamie")
  data <- county_fix(data, "Poweshick", "Poweshiek")
  data <- county_fix(data, "Prince Georg", "Prince George's")
  data <- county_fix(data, "Spenser", "Spencer")
  data <- county_fix(data, "Winchester C", "Winchester")
  data <- county_fix(data, "San Bernardi", "San Bernardino")
  data <- county_fix(data, "Alexandria C", "Alexandria")
  data <- county_fix(data, "Charlottesvi", "Charlottesville")
  data <- county_fix(data, "Chesapeake C", "Chesapeake")
  data <- county_fix(data, "City of Manassas", "Manassas")
  data <- county_fix(data, "City of Suff", "Suffolk")
  data <- county_fix(data, "Colonial Hei", "Colonial Heights")
  data <- county_fix(data, "Covington Ci", "Covington")
  data <- county_fix(data, "Fredericksbu", "Fredericksburg")
  data <- county_fix2(data, "Green", "Greene", "IL")
  data <- county_fix(data, "Hopewell Cit", "Hopewell")
  data <- county_fix(data, "Isle of Wigh", "Isle of Wight")
  data <- county_fix(data, "JoDavies", "Jo Daviess")
  data <- county_fix(data, "King and Que", "King and Queen")
  data <- county_fix(data, "Lac Qui Parl", "Lac qui Parle")
  data <- county_fix(data, "Lake of The", "Lake of The Woods")
  data <- county_fix(data, "Lexington Ci", "Lexington")
  data <- county_fix(data, "Manassas Cit", "Manassas")
  data <- county_fix(data, "Manassas Par", "Manassas Park")
  data <- county_fix(data, "North Hampton", "Northampton")
  data <- county_fix(data, "Petersburg C", "Petersburg")
  data <- county_fix(data, "Portsmouth C", "Portsmouth")
  data <- county_fix(data, "Prince Edwar", "Prince Edward")
  data <- county_fix(data, "Prince George's", "Prince George")
  data <- county_fix(data, "Prince Willi", "Prince William")
  data <- county_fix(data, "Richmond Cit", "Richmond")
  data <- county_fix(data, "Staunton Cit", "Staunton")
  data <- county_fix(data, "Virginia Bea", "Virginia Beach")
  data <- county_fix(data, "Waynesboro C", "Waynesboro")
  data <- county_fix(data, "Poquoson Cit", "Poquoson")
  data <- county_fix2(data, "Stanly", "Stanley", "SD")
  data <- county_fix2(data, "Yellowstone", "Park", "WY")
  
  

  
  # expanding
  if(check_rows(data, " Kent & New Castle") != 0) {
    
    data <- county_multiple(data, " Kent & New Castle", "Kent", "New Castle")
  }
  
  if(check_rows(data, "Allegan, Antim, Barry, Benzie,") != 0) {
    
    data <- county_multiple4(data, "Allegan, Antim, Barry, Benzie,", "Allegan", "Antrim", "Barry", "Benzie")
  }
  
  if(check_rows(data, "Grand Traverse, Ingham, Ionia") != 0) {
    
    data <- county_multiple3(data, "Grand Traverse, Ingham, Ionia", "Grand Traverse", "Ingham", "Ionia")
  }
  
  if(check_rows(data, "Isabella, Lake, Leelanau") != 0) {
    
    data <- county_multiple3(data, "Isabella, Lake, Leelanau", "Isabella", "Lake", "Leelanau")
  }
  
  if(check_rows(data, "Manistee, Mason, Mecosta,") != 0) {
    
    data <- county_multiple3(data, "Manistee, Mason, Mecosta,", "Manistee", "Mason", "Mecosta")
  }
  
  if(check_rows(data, "Missaukee, Montcalm, Muskegon") != 0) {
    
    data <- county_multiple3(data, "Missaukee, Montcalm, Muskegon", "Missaukee", "Montcalm", "Muskegon")
  }

  if(check_rows(data, "Newaygo, Oceana, Osceola") != 0) {
    
    data <- county_multiple3(data, "Newaygo, Oceana, Osceola", "Newaygo", "Oceana", "Osceola")
  }  
 
  if(check_rows(data, "Ottawa, Alpena, Charlevoix") != 0) {
    
    data <- county_multiple3(data, "Ottawa, Alpena, Charlevoix", "Ottawa", "Alpena", "Charlevoix")
  }  
  
  if(check_rows(data, "McDonough, McCoupin") != 0) {
    
    data <- county_multiple(data, "McDonough, McCoupin", "McDonough", "Macoupin")
  } 
  
  if(check_rows(data, "Menard, Morgan,Montgomery") != 0) {
    
    data <- county_multiple3(data, "Menard, Morgan,Montgomery", "Menard", "Morgan", "Montgomery")
  } 
  
  if(check_rows(data, "Sangamon,Schuyler,Scott,Pike") != 0) {
    
    data <- county_multiple4(data, "Sangamon,Schuyler,Scott,Pike", "Sangamon", "Schuyler", "Scott", "Pike")
  } 
  
  
  #deleting
  data <- data[!(data$State== "ID" & data$County == "Marshall") ,]
  
  data <- data %>%
    filter(!County == "See footnote 3")
  
  data <- data %>%
    filter(!County == "NO DISTRIBUTION SYSTEM")
  
  data <- data %>%
    filter(!County == "Clifton Forg")
  
  
  
  return(data)
  
}




### 2006
complete_2006 <- fix_all(complete_2006)
write.csv(complete_2006, file = "ferc_2006.csv",row.names = F)



### 2007
complete_2007 <- fix_all(complete_2007)
write.csv(complete_2007, file = "ferc_2007.csv",row.names = F)

### 2008
complete_2008 <- fix_all(complete_2008)
write.csv(complete_2008, file = "ferc_2008.csv",row.names = F)

### 2009
complete_2009 <- fix_all(complete_2009)

complete_2009 <- county_fix(complete_2009, "San Bernardi", "San Bernardino")

write.csv(complete_2009, file = "ferc_2009.csv",row.names = F)

### 2010
complete_2010 <- fix_all(complete_2010)
write.csv(complete_2010, file = "ferc_2010.csv",row.names = F)

### 2011
complete_2011 <- fix_all(complete_2011)
write.csv(complete_2011, file = "ferc_2011.csv",row.names = F)

### 2012
complete_2012 <- fix_all(complete_2012)
write.csv(complete_2012, file = "ferc_2012.csv",row.names = F)

### 2013



complete_2013 <- fix_all(complete_2013)
write.csv(complete_2013, file = "ferc_2013.csv",row.names = F)

### 2014
test <- complete_2014 %>%
  filter(County == "Yellowstone" & State == "WY")

table(test$respondent_id)
table(test$State)
test

complete_2014 <- fix_all(complete_2014)
write.csv(complete_2014, file = "ferc_2014.csv",row.names = F)

### 2015
complete_2015 <- fix_all(complete_2015)
write.csv(complete_2015, file = "ferc_2015.csv",row.names = F)

### 2016
complete_2016 <- fix_all(complete_2016)
write.csv(complete_2016, file = "ferc_2016.csv",row.names = F)

### 2017
complete_2017 <- fix_all(complete_2017)
write.csv(complete_2017, file = "ferc_2017.csv",row.names = F)

### 2018
complete_2018 <- fix_all(complete_2018)
write.csv(complete_2018, file = "ferc_2018.csv",row.names = F)

### 2019
complete_2019 <- fix_all(complete_2019)
write.csv(complete_2019, file = "ferc_2019.csv",row.names = F)





