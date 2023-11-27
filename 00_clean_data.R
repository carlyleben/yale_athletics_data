# This script cleans and stacks the raw data files from Yale athletics.

#==================================#
#=== load in libraries and data ===#
#==================================#

library(readxl)
library(stringr)
library(plyr)
library(dplyr)
library(tidyverse)

# read in raw files
catapult_data <- read_xlsx("rawdata/Yale Football Catapult Raw Data 2023.xlsx")
jump_data <- read.csv("rawdata/Football_-01_20_23-11_05_23-_Countermovement_Jump.csv")

#===================#
#=== clean data  ===#
#===================#

jump_data <- jump_data %>% 
  mutate(Name = trimws(Name)) %>%
  #Having more than one space messes things up so we are combining his first name into one with no space
  mutate(Name = ifelse(Name == "Da Quan Gonzales", "DaQuan Gonzales", Name)) %>% 
  #islate last name
  mutate(last_name = gsub("^\\S* ", "", Name)) %>% 
  mutate(last_name = gsub("[.]$", "", last_name)) %>% 
  #isolate first initial and create last name first initial
  mutate(first_init = substr(Name, 1, 1)) %>% 
  mutate(last_first = paste(last_name, first_init)) %>% 
  #Disambiguating the Andersons
  mutate(last_first = ifelse(Name == "Bennie Anderson Jr.", "Anderson B", last_first)) %>% 
  mutate(last_first = ifelse(Name == "Damian Anderson Jr", "Anderson Jr", last_first)) %>% 
  mutate(Name = str_to_title(Name))

catapult_data <-  catapult_data %>% 
  #cutting out random periods at the end of names that are not useful
  mutate(`Player Name` = gsub("[.]$", "", `Player Name`)) %>% 
  #This one player had his whole first name which would mess up the merging if we don't change it
  mutate(`Player Name` = ifelse(`Player Name` == "Thomas Andre", "Thomas A", `Player Name`))

#Create a date variable from the date contained in the Session title variable
catapult_data <- catapult_data %>% 
  mutate(Date = gsub(".* ", "", `Session Title`)) %>% 
  mutate(Date = gsub("[.]", "/", Date)) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  #Add a tag to this so that when we stack the data we know where it comes from
  mutate(type = "catapult")


jump_data <- jump_data %>% 
  #put date column in date format
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  #Add a tag like we did above
  mutate(type = "jump")

#We need to create a variable where the format of the name in jump_data matches the format in catapult_data. First we check if there is a match for the first initial/last name and if there isn't we use just the last name. If there is neither we use the last name with first initial to preserve more information
jump_data <- jump_data %>% 
  mutate(Name = ifelse(last_first %in% catapult_data$`Player Name`, last_first, Name)) %>% 
  mutate(Name = ifelse(!(last_first %in% catapult_data$`Player Name`) & last_name %in% catapult_data$`Player Name`, last_name, Name)) %>% 
  mutate(Name = ifelse(!(last_first %in% catapult_data$`Player Name`) & !(last_name %in% catapult_data$`Player Name`), last_first, Name)) %>% 
  dplyr::select(-c(last_name, first_init, last_first))

#format the names of the catapult data nicely
names(catapult_data) <- gsub(" ", ".", names(catapult_data))
names(catapult_data) <- gsub("[(].*", "", names(catapult_data))
names(catapult_data) <- gsub("[.]$", "", names(catapult_data))
names(jump_data) <- gsub("[.][.]", ".", names(jump_data))

#rename catapult naming column so it matches jump naming column
catapult_data <- catapult_data %>% 
  dplyr::rename(Name = Player.Name)

#all of the numbers in jump_data are stored as characters so we change it to be numeric.
jump_data[,10:88] <- jump_data[,10:88] %>% 
  mutate_if(is.character, as.numeric)

#==========================#
#=== reshape/stack data ===#
#==========================#

#Pivot longer jump data while cutting out the variables that have no analog in the other data set or use
jump_data_longer <- jump_data %>%
  dplyr::select(-c(TestId, Excluded, Segment, Type)) %>% 
  pivot_longer(-c(Name, Date, Time, Position, Tags, type), names_to = "variable", values_to = "value") %>% 
  rename(Session.Id = Time)

#Pivot longer catapult data while cutting out the variables that have no analog in the other data set or use
catapult_data_longer <- catapult_data %>% 
  pivot_longer(-c(Name, Date, Session.Title, Split.Name, Tags, type), names_to = "variable", values_to = "value") %>% 
  select(-c(Split.Name)) %>% 
  rename(Session.ID = Session.Title)

#stack data
full_data <- rbind.fill(jump_data_longer, catapult_data_longer)

#=================================#
#=== Fill in Missing Positions ===#
#=================================#

#Only the jump data has positions, so we want to fill in the missing values in the catapult data with the values from the jump data

#Find which position each player plays
position_index <- full_data %>%
  filter(!is.na(Position)) %>% 
  group_by(Name) %>% 
  summarise(Position = toString(unique(Position)))

#For the NA values or the ones where it is just " ", use the index to fiill in the missing values
full_data <- full_data %>% 
  mutate(Position = ifelse((is.na(Position) | Position == " "), toString(unique(position_index$Position[position_index$Name == Name])), Position)) %>% 
  #There was one weird NA that stayed as part of the string so replace it with just OL
  mutate(Position = ifelse(Position == "OL, NA", "OL", "Position"))

#There are still a small number of rows without a position label but it is a negligible amount in a giant dataset (79 in a dataset with 2123680 rows)

#==============#
#=== export ===#
#==============#

# export raw files to app and to processed data folders
saveRDS(full_data, "processed_data/stacked_athletics_data2.rds")
saveRDS(full_data, "yale_athletics_app/data/stacked_athletics_data2.rds")
#=======================#
#=== deprecated code ===#
#=======================#



#We will now merge the data separately for the data where there is only the last name in catapult_data and where there is a space. We will then rbind these together
#St. Aubym is the one last name with a space in it so we have to single it out



#these are all things that we did to help clean above, they are no longer necessary to run
#mean(count_appearances(" ", jump_data$Name) > 1)

#unique(jump_data$Name[count_appearances(" ", jump_data$Name) != 1])

#length(unique(jump_data$Name)); length(unique(jump_data$last_name))

#names_comp <- jump_data %>% 
#  group_by(last_name) %>% 
#  summarise(Names = toString(unique(Name)), size = length(unique(Names)))

#View(names_comp[names_comp$size != 1,])

#space_names <- catapult_data$`Player Name`[str_count(space_names <- catapult_data$`Player Name`, " ") > 0]
#unique(space_names[nchar(gsub("^\\S* ", "", space_names)) > 1])

