library(readxl)
library(tidyverse)
library(stringr)
catapult_data <- read_xlsx("rawdata/Yale Football Catapult Raw Data 2023.xlsx")
jump_data <- read.csv("rawdata/Football_-01_20_23-11_05_23-_Countermovement_Jump.csv")

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
  mutate(last_first = ifelse(Name == "Damian Anderson Jr", "Anderson Jr", last_first))

catapult_data <-  catapult_data %>% 
  #cutting out random periods at the end of names that are not useful
  mutate(`Player Name` = gsub("[.]$", "", `Player Name`)) %>% 
  #This one player had his whole first name which would mess up the merging if we don't change it
  mutate(`Player Name` = ifelse(`Player Name` == "Thomas Andre", "Thomas A", `Player Name`))

#We will now merge the data separately for the data where there is only the last name in catapult_data and where there is a space. We will then rbind these together
#St. Aubym is the one last name with a space in it so we have to single it out
catapult_space <- catapult_data[str_count(catapult_data$`Player Name`, " ") != 0 & catapult_data$`Player Name` != "St. Aubyn",]
catapult_no_space <- catapult_data[!(catapult_data$`Player Name` %in% catapult_space$`Player Name`),]



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

