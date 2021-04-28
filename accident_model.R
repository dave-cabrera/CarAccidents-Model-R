library(dplyr)
library(readxl); library(GGally); library(MASS)
library(tidyverse); library(DescTools); library(gtools)
library(leaps); library(ResourceSelection)

crash_data = read.csv('Vermont_State_Police_Traffic_Fatalities_Heat_Map.csv')

### Data cleaning
crash_data <- crash_data %>%
  rename(ID = Crash.ID,
         Date = Crash.Date,
         Year = Crash.Year,
         Day = Crash.Day,
         Hour = Crash.Hour,
         Conditions = Road.Conditions,
         Circumstances = Crash.Circumstances,
         Role = Crash.Role,
         Injury_Type = Injury)



crash_data <- crash_data %>%
  select(-c("Street", "Count", "Injury_Type", "GeoLocation", "ID"))

crash_data$Hour[crash_data$Hour == ""] <- NA
crash_data$Circumstances[crash_data$Circumstances == ""] <- NA
crash_data$Conditions[crash_data$Conditions == "Unknown"] <- "Other"
crash_data$Conditions[crash_data$Conditions == "Rough"] <- "Other"
crash_data$Conditions[crash_data$Conditions == "Work Zone"] <- "Other"

crash_data$Seatbelt[crash_data$Seatbelt == "N/A"] <- NA


crash_data$Date <- as.Date(crash_data$Date, format = "%m/%d/%Y")
crash_data$Month <- format(crash_data$Date, "%m")
crash_data$Month[crash_data$Month == "01"] <- "January"
crash_data$Month[crash_data$Month == "02"] <- "February"
crash_data$Month[crash_data$Month == "03"] <- "March"
crash_data$Month[crash_data$Month == "04"] <- "April"
crash_data$Month[crash_data$Month == "05"] <- "May"
crash_data$Month[crash_data$Month == "06"] <- "June"
crash_data$Month[crash_data$Month == "07"] <- "July"
crash_data$Month[crash_data$Month == "08"] <- "August"
crash_data$Month[crash_data$Month == "09"] <- "September"
crash_data$Month[crash_data$Month == "10"] <- "October"
crash_data$Month[crash_data$Month == "11"] <- "November"
crash_data$Month[crash_data$Month == "12"] <- "December"


crash_data$Day_Type[crash_data$Day == 'Monday'] <- 'Weekday'
crash_data$Day_Type[crash_data$Day == 'Tuesday'] <- 'Weekday'
crash_data$Day_Type[crash_data$Day == 'Wednesday'] <- 'Weekday'
crash_data$Day_Type[crash_data$Day == 'Thursday'] <- 'Weekday'
crash_data$Day_Type[crash_data$Day == 'Friday'] <- 'Weekend'
crash_data$Day_Type[crash_data$Day == 'Saturday'] <- 'Weekend'
crash_data$Day_Type[crash_data$Day == 'Sunday'] <- 'Weekend'


crash_data$Month[crash_data$Month == "January"] <- "Winter"
crash_data$Month[crash_data$Month == "February"] <- "Winter"
crash_data$Month[crash_data$Month == "March"] <- "Winter"
crash_data$Month[crash_data$Month == "April"] <- "Spring"
crash_data$Month[crash_data$Month == "May"] <- "Spring"
crash_data$Month[crash_data$Month == "June"] <- "Spring"
crash_data$Month[crash_data$Month == "July"] <- "Summer"
crash_data$Month[crash_data$Month == "August"] <- "Summer"
crash_data$Month[crash_data$Month == "September"] <- "Summer"
crash_data$Month[crash_data$Month == "October"] <- "Fall"
crash_data$Month[crash_data$Month == "November"] <- "Fall"
crash_data$Month[crash_data$Month == "December"] <- "Fall"


crash_data <- crash_data %>%
  rename(Season = Month)

table(crash_data$Seatbelt)
#### Using stepwise selection to create a model predicting if someone was wearing a seatbelt
# or not if they were in a fatal car crash in VT

# Remove scenarios where seltbelt is n/a
data_cars = crash_data %>%
  filter(Role != "Bicycle") %>%
  filter(Role != "Pedestrian")


pairs(~Year+Age, 
      data= data_cars)

crash_data %>% ggpairs(columns=which(colnames(data_cars) %in% c("Year", "Age")),
                           aes(fill=Seatbelt, color=Seatbelt)) + scale_color_manual(values = c("blue", "orange"))
# Year and age do not appear to be strongly correlated with wearing a seatbelt

boxplot(data_cars$Age, main = "Boxplot of Age")
hist(data_cars$Age, main = "Histogram of Age")



# Make seatbelt 0/1 instead of no/yes
data_cars <- data_cars %>%
  mutate(SB = ifelse(Seatbelt == "No",0,1))


seatbelt_complete = glm(SB~ Age+Day_Type+Gender+Year+Conditions+Season+Season:Conditions+Age:Day_Type,
                        data = data_cars, family = binomial(link="logit"))

seatbelt_null = glm(SB~1, data = data_cars, family = binomial(link = "logit"))


sb_BS <- stepAIC(seatbelt_complete, direction="backward", trace = 0,
                  scope=list(lower=formula(seatbelt_null),upper=formula(seatbelt_complete)))


sb_FS <-stepAIC(seatbelt_null, direction="forward", trace = 0,
                 scope=list(lower=formula(seatbelt_null),upper=formula(seatbelt_complete)))


sb_SS_B <-stepAIC(seatbelt_complete, direction="both", trace = 0,
                   scope=list(lower=formula(seatbelt_null),upper=formula(seatbelt_complete)))

sb_SS_F <-stepAIC(seatbelt_null, direction="both", trace = 0,
                   scope=list(lower=formula(seatbelt_null),upper=formula(seatbelt_complete)))
summary(sb_BS)
summary(sb_FS)
summary(sb_SS_B)
summary(sb_SS_F)

# They all chose the same model, I'll choose sb_FS
summary(sb_FS)
seatbelt_prediction = if_else(sb_FS$fitted.values > .50, "Yes", "No")

confusion_matrix = table(data_cars$Seatbelt, seatbelt_prediction)
confusion_matrix

misclassification_rate = (52 + 14) / 268
misclassification_rate










# Does including log of Age improve the fit?
mod_sbFS_logage = glm(Seatbelt ~ log(Age)+Conditions+Day_Type+log(Age):Day_Type, data = data_cars, family = binomial(link = "logit"))
summary(mod_sbFS_logage)

sb_prediction3 = if_else(mod_sbFS_logage$fitted.values > .5, "Yes", "No")
table(sb_prediction3)

sb_prediction3[267] = "Yes"
sb_prediction3[268] = "No"

cm_2 = table(data_cars$Seatbelt, sb_prediction3)
cm_2
mislass_2 = (64 + 27) / n

# No, slightly worse misclassification rate


