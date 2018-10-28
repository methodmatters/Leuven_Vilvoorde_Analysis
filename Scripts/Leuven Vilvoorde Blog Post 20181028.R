# define path to the data
in_dir <- 'C:\\Folder\\Data\\' # make sure the data files are here!
plot_dir <- 'C:\\Folder\\Plots\\'


###### Read and Prepare Crime Data
###### Read and Prepare Crime Data
###### Read and Prepare Crime Data
###### Read and Prepare Crime Data
###### Read and Prepare Crime Data


# load the packages we'll need
library(openxlsx)
library(ggplot2)
library(plyr);library(dplyr)
library(tidyr)
# install devtool version of package
# devtools::install_github("jbryer/likert")
library(likert)


# function to read in crime data
read_crime_data <- function(file_name_f, in_dir_f){
  # read the Excel file
  crime_raw_f <- read.xlsx(paste0(in_dir_f, file_name_f))
  # remove weird rows with no data
  crime_raw_f <- crime_raw_f[1:12,]
  # make the column name city (translate from Dutch to English)
  names(crime_raw_f)[names(crime_raw_f) == 'Gebied'] <- 'City'
  # make the Flemish region code also in English
  crime_raw_f$City[crime_raw_f$City == "Vlaams Gewest"] <- "Vlaams Gewest (Flemish Region)"
  return(crime_raw_f)
}

# read in crime data
crime_clean <- read_crime_data('Leuven Vilvoorde Crime per 1000 inhabitants clean.xlsx', in_dir)

# make function to adjust for 
# "missing" students
# living but not registered in Leuven 
divide_it <- function(x){
            divided_figure <- x / 1.3
            return(divided_figure)
}

# create the data for the Leuven student assumptions
leuven_w_students <- crime_clean %>% filter(City == 'Leuven') %>%
                # mutate variables in the year columns (crime figures)
                # we apply our function to each cell (divide by 1.3)
                mutate_at(vars(`2000`:`2016`), divide_it)   %>%
                # change the name of the city to indicate it
                # reflects our assumptions, not the actual data
                mutate(City = dplyr::recode(City, 'Leuven' = 'Leuven Student Assumptions' ))

# stack our newly created data on top of original data
master_crime <- rbind(crime_clean, leuven_w_students)

# transform from wide to long format
# ggplot2 needs data in this structure
crime_long <- gather(master_crime, year, value, `2000`:`2016`)

# head pretty for blog
library(htmlTable)
crime_long %>% 
  mutate_if(is.numeric, round, digits = 2) %>% head(10) %>%
  htmlTable()


###### Read and Prepare Questionnaire Data
###### Read and Prepare Questionnaire Data
###### Read and Prepare Questionnaire Data
###### Read and Prepare Questionnaire Data
###### Read and Prepare Questionnaire Data

###### safety
read_safety_data <- function(file_name_f, in_dir_f){
  # read the file, specifying that there are dates
  safety_raw_f <- read.xlsx(paste0(in_dir, file_name_f), detectDates = TRUE)
  # remove info hanging around at bottom of file
  safety_raw_f <- safety_raw_f[!is.na(safety_raw_f$Indicator),]
  # name the columns
  names(safety_raw_f) <- c("Gemeente","Indicator","Item", 'Date', 'Empty',  'Never/Seldom', 'Sometimes', 'Often/Always')
  # select observations just from 2017 - we only have Vilvoorde for that date
  safety_raw_f <- safety_raw_f[safety_raw_f$Date == '2017-01-01',]
  # remove the empty column
  safety_raw_f$Empty <- NULL
  # replace missings in questionnaire data with zeros
  safety_raw_f[is.na(safety_raw_f)] <- 0
  # select only relevant columns for Leuven and Vilvoorde
  safety_raw_f <- safety_raw_f[safety_raw_f$Gemeente == 'Leuven' | safety_raw_f$Gemeente == 'Vilvoorde',c(1,3,5:7)]
  # make sure that the "Item" variable represents city vs. neighborhood
  # (for Likert plot)
  safety_raw_f$Item <- c('Neighborhood', 'City','Neighborhood', 'City')
  # make city and Item factors
  # (for Likert plot)
  safety_raw_f$Gemeente <- as.factor(safety_raw_f$Gemeente)
  safety_raw_f$Item <- as.factor(safety_raw_f$Item)
  # return the clean dataset
  return(safety_raw_f)
}

###### problems
read_problem_data <- function(file_name_f, in_dir_f){
  # read the file, specifying that there are dates
  probs_raw_f <- read.xlsx(paste0(in_dir, file_name_f), detectDates = TRUE)
  # remove info hanging around at bottom of file
  probs_raw_f <- probs_raw_f[!is.na(probs_raw_f$Indicator),]
  # name the columns
  names(probs_raw_f) <- c("Gemeente","Indicator","Item", 'Date', 'Empty',  'Never/Seldom', 'Sometimes', 'Often/Always')
  # select observations just from 2017 - we only have Vilvoorde for that date
  probs_raw_f <- probs_raw_f[probs_raw_f$Date == '2017-01-01',]
  # remove the empty column
  probs_raw_f$Empty <- NULL
  # replace missings in questionnaire data with zeros
  probs_raw_f[is.na(probs_raw_f)] <- 0
  # select only relevant columns for Leuven and Vilvoorde
  probs_raw_f <- probs_raw_f[probs_raw_f$Gemeente == 'Leuven' | probs_raw_f$Gemeente == 'Vilvoorde',c(1,3,5:7)]
  # delete the original item column (this is text on y axis)
  probs_raw_f$Item <- NULL
  # make city (Gemeente) the "Item"
  # (for Likert plot)
  names(probs_raw_f)[1] <- 'Item'
  # return the clean dataset
  return(probs_raw_f)
}

####### vandalism
read_vandalism_data <- function(file_name_f, in_dir_f){
  # read the file, specifying that there are dates
  vand_raw_f <- read.xlsx(paste0(in_dir, file_name_f), detectDates = TRUE)
  # remove info hanging around at bottom of file
  vand_raw_f <- vand_raw_f[!is.na(vand_raw_f$Indicator),]
  # name the columns
  names(vand_raw_f) <- c("Gemeente","Indicator","Item", 'Date', 'Empty',  'Never/Seldom', 'Sometimes', 'Often/Always')
  # select observations just from 2017 - we only have Vilvoorde for that date
  vand_raw_f <- vand_raw_f[vand_raw_f$Date == '2017' & vand_raw_f$Item == 'Vernieling straatmeubilair',]
  # remove the empty column
  vand_raw_f$Empty <- NULL
  # replace missings in questionnaire data with zeros
  vand_raw_f[is.na(vand_raw_f)] <- 0
  # select only relevant columns for Leuven and Vilvoorde
  vand_raw_f <- vand_raw_f[vand_raw_f$Gemeente == 'Leuven' | vand_raw_f$Gemeente == 'Vilvoorde',c(1,3,5:7)]
  # delete the original item column (this is text on y axis)
  vand_raw_f$Item <- NULL
  # make city (Gemeente) the "Item"
  # (for Likert plot)
  names(vand_raw_f)[1] <- 'Item'
  # return the clean dataset
  return(vand_raw_f)
}

######### pride in one's city
read_pride_data <- function(file_name_f, in_dir_f){
  # read the file, specifying that there are dates
  pride_raw_f <- read.xlsx(paste0(in_dir, file_name_f), detectDates = TRUE)
  # remove info hanging around at bottom of file
  pride_raw_f <- pride_raw_f[!is.na(pride_raw_f$Indicator),]
  # name the columns
  names(pride_raw_f) <- c("Gemeente","Indicator","Item", 'Date', 'Empty',  'Mostly/Completely disagree', 
                          'Neither agree nor disagree', 'Mostly/Completely agree')
  # select observations just from 2017 - we only have Vilvoorde for that date
  pride_raw_f <- pride_raw_f[pride_raw_f$Date == '2017-01-01',]
  # remove the empty column
  pride_raw_f$Empty <- NULL
  # replace missings in questionnaire data with zeros
  pride_raw_f[is.na(pride_raw_f)] <- 0
  #  select only relevant columns for Leuven and Vilvoorde
  pride_raw_f <- pride_raw_f[pride_raw_f$Gemeente == 'Leuven' | pride_raw_f$Gemeente == 'Vilvoorde',c(1,3,5:7)]
  # delete the original item column (this is text on y axis)
  pride_raw_f$Item <- NULL
  # make city (Gemeente) the "Item"
  # (for Likert plot)
  names(pride_raw_f)[1] <- 'Item'
  # return the clean dataset
  return(pride_raw_f)
}

# read in the data
safety_clean <- read_safety_data('SAMEN_onveiligheid.xlsx', in_dir)
probs_clean <- read_problem_data('SAMEN_problemen.xlsx', in_dir)
vandal_clean <- read_vandalism_data('SAMEN_vandalisme.xlsx', in_dir)
pride_clean <- read_pride_data('WON_fierheid.xlsx', in_dir)


# pretty head for blog
library(htmlTable)
safety_clean %>% 
  mutate_if(is.numeric, round, digits = 2) %>% head(5) %>%
  htmlTable()



###### Plot Crime Data
###### Plot Crime Data
###### Plot Crime Data
###### Plot Crime Data
###### Plot Crime Data
###### Plot Crime Data

# property damage
prop_damage_plot <- crime_long %>% filter(Categorie == 'Beschadigen van eigendom (aantal per 1.000 inw.)') %>% 
  ggplot(aes(x = year, y = value, color = City, group = City)) + 
  geom_line(aes(color = City), size = 1) + 
   geom_point() +   
  labs(x = 'Year', y = 'Property Damage (per 1000 inhabitants)', 
       title = 'Property Damage: 2000 - 2016') 

# theft
theft_plot <- crime_long %>% filter(Categorie == 'Diefstal en afpersing (aantal per 1.000 inw.)') %>% 
  ggplot(aes(x = year, y = value, color = City, group = City)) + 
  geom_line(aes(color = City), size = 1) + 
  geom_point() +   
  labs(x = 'Year', y = 'Theft (per 1000 inhabitants)', 
       title = 'Theft: 2000 - 2016') 

# physical violence
phys_violence_plot <- crime_long %>% filter(Categorie == 'Misdr. tegen de lichamelijke integriteit (aantal per 1.000 inw.)') %>% 
  ggplot(aes(x = year, y = value, color = City, group = City)) + 
  geom_line(aes(color = City), size = 1) + 
  geom_point() +   
  labs(x = 'Year', y = 'Physical Violence (per 1000 inhabitants)', 
       title = 'Physical Violence: 2000 - 2016') 

# overall crime
overall_crime_plot <- crime_long %>% filter(Categorie == 'Criminaliteitsgraad (aantal per 1.000 inw.)') %>% 
  ggplot(aes(x = year, y = value, color = City, group = City)) + 
  geom_line(aes(color = City), size = 1) + 
  geom_point() +   
  labs(x = 'Year', y = 'Overall Crime (per 1000 inhabitants)', 
       title = 'Overall Crime: 2000 - 2016') 


# plot multiple graphs with grid.arrange
# from gridExtra package
# we give a bit of extra space at the bottom
# of each graph
library(gridExtra)
margin = theme(plot.margin = unit(c(1,.5,.5,.5), "cm"))
# png(filename = paste0(plot_dir, 'crime_plot.png'), width=10,height=16, units='in', res=300)
grid.arrange(grobs = lapply(list(prop_damage_plot, theft_plot, phys_violence_plot, overall_crime_plot), "+", margin), nrow = 4)
# dev.off()


###### Plot the Questionnaire Data
###### Plot the Questionnaire Data
###### Plot the Questionnaire Data
###### Plot the Questionnaire Data
###### Plot the Questionnaire Data
###### Plot the Questionnaire Data


# make likert object
# for safety we use a grouping: neighborhood vs. city
# (questions were asked about both)
safety_likert <- likert(summary = safety_clean, grouping = safety_clean[,1]) 
# make the plots
# png(filename = paste0(plot_dir, 'subjective_safety.png'), width=12,height=8, units='in', res=300)
plot(safety_likert, group.order = c('Leuven', 'Vilvoorde')) + ggtitle('How often do you feel unsafe in your...')
# dev.off()

# make likert objects for the other questions
likert_problems <- likert(summary = probs_clean)
likert_vand <- likert(summary = vandal_clean)
likert_pride <- likert(summary = pride_clean)

# first make object for each graph
prob_plot <- plot(likert_problems, group.order = c('Leuven', 'Vilvoorde')) + ggtitle('How often are you hassled on the street?')
vand_plot <- plot(likert_vand, group.order = c('Leuven', 'Vilvoorde')) + ggtitle('How often do you see destruction of street furniture?')
pride_plot <- plot(likert_pride, group.order = c('Leuven', 'Vilvoorde')) + ggtitle('I am truly proud of my city')

# plot multiple graphs with grid.arrange
# from gridExtra package
# we give a bit of extra space at the bottom
# of each graph
margin = theme(plot.margin = unit(c(1,.5,.5,.5), "cm"))
# png(filename = paste0(plot_dir, 'multi_likert_plot.png'), width=10,height=12, units='in', res=300)
grid.arrange(grobs = lapply(list(prob_plot, vand_plot, pride_plot), "+", margin))
# dev.off()
