library(tidyverse)
data(who)
head(who)

# County, Iso2, Iso3 are redundant
# What do the new_sp clumns mean? Seems like m3500 is some kind of value that we want to preserve.

# make new_sp_XXXX columns into VALUES in ONE column called newrel_f67
# the cells are counts of cases in each of these categories.


who1 <- who %>% 
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)
# this seems like now different variables are in the same column?? 


who1 %>% count(key)

# first 3 letters denote wether it's NEW or OLD cases of TB
# next 2 letters are type of TB: whether it's RELapse, ExtraPulmonary, SmearNegative, etc.
# next letter is the sex of TB patients
# remaining numbers are the age groups

# Next, fix an error where new_rel and newrel are inconsistent

who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2

# now let's separate the variables into different columns
who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")
who3

# get rid of redundant columsn
who4 <- who3 %>% 
  select(-new, -iso2, -iso3)

# split sex and age
who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1)
who5
