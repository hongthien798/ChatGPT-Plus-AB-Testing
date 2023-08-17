library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Hmisc)
NUMERIC <- read_csv("Result_NUMERIC.csv")
TEXT <- read_csv("Result_TEXT.csv")
View(NUMERIC)
View(TEXT)

########################
numeric<-NUMERIC[, c("Timer_Page Submit","ba1_6_Q11_1", "ba1_6_Q19","ba1_6_Q18","ba1_6_Q15",
                     "ba1_6_Q16","age_1","sex_or","Income","Education","ba1_6_iv_DO")]
names(numeric) <- c("timer","upgrade_willines", "plan_con", "plan_decoy",
                    "primary","purpose","age","gender","income","education","type")
numeric <- numeric[-1,]


# numeric:
  # upgrade_willines: willingness to upgrade to ChatGPT Plus plan
  # plan_con/plan_decoy: Which plan will you go with 
  # primary: Current primary use of ChatGPT
  # purpose: Purpose of using ChatGPT Plus if you ever planned to upgrade it
  # type: control or decoy group
# text:
#   plan_con_txt/plan_decoy_txt: Which plan will you go with
#   primary_txt: Current primary use of ChatGPT
#   purpose_txt: Purpose of using ChatGPT Plus if you ever planned to upgrade it

as.vector(numeric)
numeric$plan_con[is.na(numeric$plan_con)] <- 0
numeric$plan_decoy[is.na(numeric$plan_decoy)] <- 0
numeric$upgrade_willines[is.na(numeric$upgrade_willines)] <- 0
numeric$plan <- as.numeric(numeric$plan_decoy)+as.numeric(numeric$plan_con)
numeric<-numeric[,-3:-4]
View(numeric)

new<-subset(numeric,numeric$upgrade_willines>0)
new$upgrade_willines<-as.numeric(new$upgrade_willines)-2
new2<-subset(new,as.numeric(new$timer)>8)
View(new2)

# new2 %>%
#   group_by(type) %>%
#   summarize(mean = mean(as.numeric(upgrade_willines),na.rm = TRUE),
#             sd = sd(as.numeric(upgrade_willines),na.rm = TRUE))

new2

decoy<-subset(new2,new2$type=="decoy")
mean(decoy$upgrade_willines)
sd(decoy$upgrade_willines)
con<-subset(new2,new2$type=="control")
mean(con$upgrade_willines)
sd(con$upgrade_willines)

ggplot(data= new2, aes(x= plan, y= education, fill= type)) + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position = 
                 position_dodge(width = 0.90), width = 0.2) +
  coord_cartesian(ylim=c(1,3))

new2$edu_lv <- ifelse(new2$education > 2, "Post Secondary", "Secondary or lower")

new2$pupose_txt <- ifelse(new2$purpose == 1, "Educational / Academic Purpose",
                          ifelse(new2$purpose == 2, "Career / Work Purpose",
                                 ifelse(new2$purpose == 3, "Social Purpose", "Entertainment Purpose")))

View(new2)

head(new2)

#write.csv(new2, "new2.csv", row.names = FALSE)

#Hypothesis 1
t.test(upgrade_willines ~ type, data = new2)

#Hypothesis 2
summary(aov(upgrade_willines ~ type*edu_lv, data = new2))
t.test(upgrade_willines ~ type, data = new2[new2$edu_lv=="Secondary or lower",])
t.test(upgrade_willines ~ type, data = new2[new2$edu_lv=="Post Secondary",])

#Hypothesis 3
summary(aov(upgrade_willines ~ type*purpose, data = new2))









