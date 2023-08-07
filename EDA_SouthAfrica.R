options(java.parameters = "-Xmx8000m")

setwd("C:/Users/musik/Downloads/Berry Postdoc/SciofSci")
library(tidyverse)
library(readxl)
library(janitor)
library(scales)
library(rvest)
library(plotly)
library(ggridges)
library(writexl)
library(stringi)
library(plotly)
library(data.table)
options(scipen=999)

df <- read_excel("SouthAfrica_pop_projections.xlsx",sheet = 1)

str(df)
names(df)[1] <- "Race"

df <- clean_names(df)

# manipulate table df to facilitate percentage calculation
dftot <- df %>%
  summarize_if(is.numeric, sum, na.rm=TRUE)
dftot <- dftot %>%
  pivot_longer(,cols=c(1:21),names_to = "Year",values_to = "totcount")
# merge in yearly totals to calculate pop percentages
sexraceage_pct <- df %>%
  pivot_longer(,cols=c(4:24),names_to = "Year",values_to = "count") %>%
  left_join(dftot,by="Year") %>%
  mutate(pct = count/totcount*100)

class(sexraceage_pct$age)
sexraceage_pct$age <- ifelse(sexraceage_pct$age=="80+","80-150",
                             sexraceage_pct$age)
sexraceage_pct$age_min <- as.numeric(sub("-.*","",sexraceage_pct$age))
sexraceage_pct$age_max <- as.numeric(sub(".*-","",sexraceage_pct$age))

age_range <- sexraceage_pct %>%
  select(age,age_min,age_max) %>%
  distinct(age,age_min,age_max) %>%
  mutate(range = paste0("[",age_min,",",age_max,"]"))

test <- subset(sexraceage_pct, race=="African"& sex=="Female"& age=="15-19" & 
                 Year=="x2019")

rm(dftot)



# obtain sums for each gender-race combination in each year
x <- df %>%
  group_by(sex,race) %>%
  summarize_if(is.numeric, sum, na.rm=TRUE)
x
x <- clean_names(x)

# manipulate table x to facilitate percentage calculation
tot <- x %>%
  summarize_if(is.numeric, sum, na.rm=TRUE)
tot <- tot%>%
  summarize_if(is.numeric, sum, na.rm=TRUE)
tot <- tot %>%
  pivot_longer(,cols=c(1:21),names_to = "Year",values_to = "totcount")

# merge in yearly totals to calculate pop percentages
sexrace_pct <- x %>%
  pivot_longer(,cols=c(3:23),names_to = "Year",values_to = "count") %>%
  left_join(tot,by="Year") %>%
  mutate(pct = count/totcount*100)



rm(df,tot,x)

#############################################
################### GET ARTICLE DATA 
#############################################
data <- read_excel("test.xlsx")
data1 <- data %>%
  distinct(author_id,document_id,publication_year,.keep_all = TRUE)

data1 <- subset(data1, !is.na(race)&!is.na(gender)&!is.na(age_at_pub))
data1 <- data1 %>%
  mutate(age_at_pub = as.numeric(age_at_pub))

data1$age_at_pub1 <- data1$age_at_pub

data1 <- as.data.table(data1)

age_range <- as.data.table(age_range)

setkey(data1,age_at_pub1,age_at_pub)
setkey(age_range,age_min,age_max)

data1$age <- age_range$age[foverlaps(data1,age_range, which = TRUE)$yid]

data1$international <- ifelse(!is.na(data1$wos_ut),"Yes","No")


# get author counts by race and gender for each year_subject combination
subj1 <- data1 %>%
  group_by(race,gender,publication_year,subject_lvl1,author_id) %>%
  summarize(lvl1count=n()) %>%
  group_by(race,gender,publication_year,subject_lvl1) %>%
  summarize(lvl1count=n())

# get author counts by race and gender for each year_subject combination
subj1_age <- data1 %>%
  group_by(race,gender,age,publication_year,subject_lvl1,author_id) %>%
  summarize(lvl1count=n())%>%
  group_by(race,gender,age,publication_year,subject_lvl1) %>%
  summarize(lvl1count=n())


# get overall counts by year_subject combination, rename total count column
ts1 <- subj1 %>%
  group_by(publication_year,subject_lvl1) %>%
  summarize_if(is.numeric, sum, na.rm=TRUE)
names(ts1)[3] <- "totcount"

# merge subj1 and ts1 to calculate proportions
subj1pct <- subj1 %>%
  left_join(ts1, by=c("publication_year","subject_lvl1")) %>%
  mutate(pct = lvl1count/totcount * 100,
         race_gender = paste(race,gender))

# merge subj1_age and ts1 to calculate proportions
subj1_agepct <- subj1_age %>%
  left_join(ts1, by=c("publication_year","subject_lvl1")) %>%
  mutate(pct = lvl1count/totcount * 100,
         race_gender_age = paste(race,gender,age))

# present race_gender proportions side-by-side in tabular form
prop <- subset(subj1pct, select=c(3:8))

prop_age <- subset(subj1_agepct, select=c(4:9))

#convert year variable to numeric
prop$year <- as.numeric(prop$publication_year)
prop_age$year <- as.numeric(prop_age$publication_year)

###################################################
############# OVER-/UNDER-REPRESENTATION 
#####################################################

############## BY SEX & RACE
# modify dates in sexrace_pct table to reflect actual years
sexrace_pct$Year <- stri_sub(sexrace_pct$Year,2,5)
# change "African" in sexrace table to "Black African"
sexrace_pct$race <- ifelse(sexrace_pct$race=="African","Black African",sexrace_pct$race)
#create race_gender column
sexrace_pct$race_gender <- paste(sexrace_pct$race,sexrace_pct$sex)
#rename pct column
names(sexrace_pct)[6] <- "pop_pct"
#remove totalcount column from sexrace
sexrace_pct <- subset(sexrace_pct,select=-c(totcount))

########### BY SEX, RACE, & AGE
# modify dates in sexraceage_pct table to reflect actual years
sexraceage_pct$Year <- stri_sub(sexraceage_pct$Year,2,5)
# change "African" in sexraceage table to "Black African"
sexraceage_pct$race <- ifelse(sexraceage_pct$race=="African","Black African",sexraceage_pct$race)
#create race_gender_age column
sexraceage_pct$race_gender_age <- paste(sexraceage_pct$race,sexraceage_pct$sex,sexraceage_pct$age)
#rename pct column
names(sexraceage_pct)[7] <- "pop_pct"
#remove totalcount column from sexraceage
sexraceage_pct <- subset(sexraceage_pct,select=-c(totcount))

#merge in population proportions for comparison
prop1 <- prop %>%
  left_join(sexrace_pct, by=c("publication_year"="Year","race_gender"="race_gender")) %>%
  mutate(repdiff = pct - pop_pct)

#merge in population proportions for comparison (table w/age)
prop1_age <- prop_age %>%
  left_join(sexraceage_pct, by=c("publication_year"="Year","race_gender_age"="race_gender_age")) %>%
  mutate(repdiff = pct - pop_pct) %>%
  mutate_at(c("sex","age","race","subject_lvl1"),as.factor)

write_xlsx(prop1_age, "prop1_age.xlsx")


#####################################
##### COUNT PLOTS
#########################################
# plot counts by year for each subject area
#example
natsci <- prop1_age %>%
  filter(subject_lvl1=="{Natural sciences}")
ggplotly(ggplot(natsci, aes(x = year, y=lvl1count, color=race,
                            text = paste("subgroup:", race_gender_age))) +
           geom_point(position="jitter", show.legend = TRUE, alpha=(1/3)) + 
           theme(legend.position='none')+
           ggtitle("SA Sample: # of Authors by Subject and Year (Subject: Natural Sciences)"))



# plot counts by year for each demographic (race, gender, age) group
# example
blkfem <- prop1_age %>%
  filter(race_gender_age=="Black African Female 45-49")

ggplotly(ggplot(blkfem, aes(x = year, y=totcount, color=subject_lvl1)) +
                          geom_point(position="jitter", show.legend = FALSE, alpha=(1/3)) + 
                          theme(legend.position='none')+
                          ggtitle("SA Sample: # of Authors by Year (Group: Black African Female 45-49)"))


######################################
##### PERCENTAGE PLOTS
######################################

# plot percentage by year for each demographic (race, gender, age) group
# example
ggplotly(ggplot(blkfem, aes(x = year, y=pct, color=subject_lvl1)) +
           geom_point(position="jitter", show.legend = FALSE, alpha=(1/3)) + 
           theme(legend.position='none')+
           ggtitle("SA Sample: % of Publications by Year (Group: Black African Female)"))


####### PLOTS: OVER-/UNDER-REPRESENTATION
# example
ggplotly(ggplot(blkfem, aes(x = year, y=repdiff, color=subject_lvl1, 
                            text = paste("subject count:", totcount))) +
           geom_point(position="jitter", show.legend = FALSE, alpha=(1/3)) + 
           theme(legend.position='none')+
           ggtitle("SA Sample: Over/underrepresentation by Year (Group: Black African Female)"))


