library(tidyverse)

replace_fun <- function(vec, cutoff){
  replace(vec, vec > cutoff, NA)
}

df <- read.csv(file="data/yllanon.csv", header=T, fileEncoding="UTF-8", na.strings = c("", " ", ".", "999")) %>% 
  type_convert() 

# Assign each participant a unique ID number ranging from 1 to 552
id_num <- seq(1:552)

id_strings <- df %>% distinct(id) # extract unique participant ID strings

cols <- cbind(id = id_strings, id_num) # match each ID strings with a number from 1-552

# merge the new ID numbers into the df using the shared ID string column
df <- df %>% left_join(cols)

# move the ID # and wave # columns to the front of the df
df <- df %>% relocate(id_num, .before = StartDate) %>%
  relocate(wave, .after = id_num)

# Count the number of missing values in each column
Num_NA<-sapply(df,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(df),Count=Num_NA)
NA_Count %>% arrange(desc(Count))


# Code each participant for who they voted for in 2016 - Clinton, Trump, or other
df <- df %>% mutate(voting_string = voting,
                    voting = case_when(voting == "clinton" ~ 1,
                                       voting == "trump" ~ 2,
                                       voting == "other" ~ 3,
                                       T ~ 999))
vote_col <- df %>% filter(wave==1) %>% select(id_num, voting_string) # votes were only recorded at W1
vote_col$voting_factor <- factor(vote_col$voting_string, levels=c("clinton", "trump", "other"), labels=c("Clinton", "Trump", "Other")) # convert to factor
vote_col <- vote_col %>% select(-voting_string) # get rid of the string variable that we don't need
df <- df %>% left_join(vote_col) # merge to main df using the shared id_num column


# Several items had additional options such as Don't Know or Haven't thought about it, coded 8 and 9. These values are not part of the 1-7 scale I am interested in so I want to replace them with NAs. Here I'm creating copies of these variables so that I can replace all values of 8 and 9 with NA.
vars_to_transform <- c("crime", "def", "terror", "poor", "health", "econ", "unemploy",
                       "blkaid", "adopt", "imm", "vaccines", "guns", "friends_1",
                       "friends_2", "friends_3", "friends_4", "friends_5", "climate", 
                       "ideo", "partyid", "check", "beh_identity", "quarantine", "sickleave")

df <- df %>% mutate(across(all_of(vars_to_transform), 
                           \(x) replace_fun(vec=x, cutoff=7))) 

# Repeat for variables where meaningful values range from 1-4
vars_to_transform <- c("djt")

df <- df %>% mutate(across(all_of(vars_to_transform),
                           \(x) replace_fun(vec=x, cutoff=4)))

##### Save out as a csv
write.csv(df, "data/brandt_clean.csv")
