library(tidyverse)

# Load in the iDiv csv file
iDiv <- read_csv("./data/iDivdb/283_2_FoodWebDataBase_2018_12_10.csv")

glimpse(iDiv)

# clean up the data (only select variables needed)
# CleaniDiv <- iDiv %>% select(link.citation, interaction.type, study.site, foodweb.name, longitude, latitude, ecosystem.type,
                            # con.metabolic.type, res.metabolic.type, contains("taxonomy")) 

# glimpse(CleaniDiv)

# creates dataframes in a list categorised by foodwebs
list_foodwebs <- split(iDiv, iDiv$foodweb.name) 

# creates dataframes in a list categorised by authors
list_authors <- split(iDiv, iDiv$link.citation)

# probably more effiecent to call on dataframes from lists as needed, but in case we want to write .csv files look below

filenames_authors <- paste0("./data/iDivdb/clean/authors/",names(list_authors), ".csv")
Map(write.csv,list_authors,filenames_authors) # writes .csv files by author

filenames_foodwebs <-  paste0("./data/iDivdb/clean/foodwebs/",names(list_foodwebs), ".csv")
Map(write.csv,list_foodwebs,filenames_foodwebs) # writes .csv files by foodweb

####### Below is the same but using a for loop to create new dataframes (slow and innefficent - easier to call from list) --------


# Check for unique datasets in the large csv
# unique(iDiv$link.citation) # = 23 Studies

# unique(iDiv$foodweb.name)

#iDiv$link.citation[1] # 1st output = Cattin

#uni_names<-unique(iDiv$link.citation)
#uni_names[1:10] # store unique studies as vector 

#DF_A <- subset(iDiv, link.citation == uni_names[1])
#DF_B <- subset(iDiv, link.citation == uni_names[2]) # Too slow, better way? 
#DF_C <- subset(iDiv, link.citation == uni_names[3])

### 1 hours work :(
#DF = list() # need to create empty list for for loop to work so DF recognised in loop

#for (i in 1: length(uni_names)){ # for loop does x for 1 to i iterations (in this case the length if unique names= 23)
#  DF = subset(iDiv, link.citation == uni_names[i])
#  print(i) # print each iteration step to check loop is working
  
  #assign(paste(DF$link.citation[1], sep=""), DF) # prints whole unique name, not good - too many characters and spaces
  
  #assign(paste("X",i, sep=""), DF) # Names everything X "iteration step" e.g. X1, X2
  
  #assign(paste((strsplit(DF$link.citation[1]," ")[[1]][1]), i, sep=""), DF) # Doesnt remove numbers
  
  #assign(paste((word(DF$link.citation)), i, sep=""), DF) # Doesn't remove numbers
  
#  assign(paste((word(uni_names[i], 1)), i, sep=""), DF) # Again doesnt remove numbers
#}






