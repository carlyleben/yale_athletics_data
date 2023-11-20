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

catapult_data <- catapult_data %>% 
  mutate(Date = gsub(".* ", "", `Session Title`)) %>% 
  mutate(Date = gsub("[.]", "/", Date)) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  mutate(type = "catapult")

jump_data <- jump_data %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  mutate(type = "jump")

jump_data <- jump_data %>% 
  mutate(Name = ifelse(last_first %in% catapult_data$`Player Name`, last_first, Name)) %>% 
  mutate(Name = ifelse(!(last_first %in% catapult_data$`Player Name`) & last_name %in% catapult_data$`Player Name`, last_name, Name)) %>% 
  mutate(Name = ifelse(!(last_first %in% catapult_data$`Player Name`) & !(last_name %in% catapult_data$`Player Name`), last_first, Name)) %>% 
  dplyr::select(-c(last_name, first_init, last_first))

names(catapult_data) <- gsub(" ", ".", names(catapult_data))
names(catapult_data) <- gsub("[(].*", "", names(catapult_data))
names(catapult_data) <- gsub("[.]$", "", names(catapult_data))
names(jump_data) <- gsub("[.][.]", ".", names(jump_data))

catapult_data <- catapult_data %>% 
  dplyr::rename(Name = Player.Name)

jump_data[,10:88] <- jump_data[,10:88] %>% 
  mutate_if(is.character, as.numeric)

#==========================#
#=== reshape/stack data ===#
#==========================#

jump_data_longer <- jump_data %>%
  dplyr::select(-c(TestId, Excluded, Segment, Type, Time)) %>% 
  pivot_longer(-c(Name, Date, Position, Tags, type), names_to = "variable", values_to = "value")

catapult_data_longer <- catapult_data %>% 
  pivot_longer(-c(Name, Date, Session.Title, Split.Name, Tags, type), names_to = "variable", values_to = "value") %>% 
  select(-c(Session.Title, Split.Name))

full_data <- rbind.fill(jump_data_longer, catapult_data_longer)

#==============#
#=== export ===#
#==============#

# export raw files to app and to processed data folders
saveRDS(full_data, "/processed_data/stacked_athletics_data.rds")
saveRDS(full_data, "/yale_athletics_app/data/stacked_athletics_data.rds")
#=======================#
#=== deprecated code ===#
#=======================#

# position_index <- full_data %>% 
#   group_by(Name) %>% 
#   summarise(Position = toString(unique(Position)))

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

