pkgs <- list("glmnet", "reticulate", "stringr", "rstudioapi", "data.table", "parallel", "minpack.lm", "doParallel",
             "foreach", "pROC", "gplots", "pwr", "dplyr", "caret", "sm", "ggplot2", "scales", "reshape2", "Hmisc",
             "bayesAB", "gridExtra", "plotly", "flux", "RColorBrewer", "plm", "xts", "pdp", "vip", "ranger", "vioplot",
             "randomForest", "haven")

install.packages("reshape2")
install.packages("reshape")
install.packages("caret")

library(reshape2)
library(reshape)
library(caret)

options(scipen=999)

# install packages in list
#lapply(pkgs, install.packages, character.only = T)

# load packages in list
lapply(pkgs, require, character.only = T)

# set wd
setwd("C:\\Users\\Vinu\\Desktop\\MISM 6214 Capstone\\Data\\")

##### Experimentation #####
### read in data
data <- read_dta("android_xsection_treated_cleaned.dta")
data <- data %>% rename(control_flag = c)


### store data as csv
# write.csv(data, file="android_xsection_treated_cleaned.csv")

# ===============================
# Adding new flags to the table
# ===============================
data  <- data %>% mutate(
  
  tc_segment = case_when(control_flag == 1 ~ "Control"
                         ,control_flag == 0 ~ "Treatment"),
  
  conversion_flag = if_else(is.na(days_first_purchase),0, 1),
  
  conversion_days_segment = case_when( (control_flag==1) ~ 'Control (No promotion)'
                                       ,is.na(days_first_purchase) ~ 'No Conversion'
                                       ,(days_first_purchase < days_first_promo_offer) ~ 'Converted before first promotion'
                                       ,(days_first_purchase >= days_first_promo_offer) ~ 'Converted after first promotion'),
  
  conversion_promo_segment = case_when((control_flag==1) ~ 'Control (No promotion)'
                                       ,is.na(days_first_purchase) ~ 'No Conversion'
                                       ,!(is.na(days_first_purchase)) & !(is.na(days_first_promo_purchase)) ~ 'Conversion w/ Promo'
                                       ,!(is.na(days_first_purchase)) & (is.na(days_first_promo_purchase)) ~ 'Conversion w/o Promo'
  ),
  
  fb_connect_flag = paste0(fb_connect, fb_ch1),
  
  fb_connect_segment = case_when(fb_connect==1 & fb_ch1==1 ~ 'FB connect bfr Ch1'
                                 ,fb_connect==1 & fb_ch1==0 ~ 'FB connect aftr Ch1'
                                 ,fb_connect==0 ~ 'No FB connect' ),
  
  treatment_group = case_when(treatment == 'after0days' ~ 'Immediate treatment'
                              ,treatment == 'after25days' ~ 'Treatment after 25 days'
                              ,treatment == 'after50days' ~ 'Treatment after 50 days'
                              ,treatment == 'no_promo' ~ 'Control (No promotion)'),
  days_btwn_promo_first_purchase = days_first_purchase - days_first_promo_offer,
  days_left_aft_fpur = 180 - days_first_purchase)

data  <- data %>% mutate( days_left_aft_fpur = if_else(days_left_aft_fpur == 0 ,1, days_left_aft_fpur),
                          revenue_per_days_left = revenue/days_left_aft_fpur)


### Some data formatting
data$treatment <- as.factor(data$treatment)
data$treatment <- relevel(data$treatment, ref='no_promo')
# data$treatment <- relevel(data$treatment, ref='after0days')

data$conversion_days_segment <- as.factor(data$conversion_days_segment)
data$conversion_days_segment <- factor(data$conversion_days_segment, levels = c("Control (No promotion)","Converted before first promotion","Converted after first promotion","No Conversion"))

colnames(data)




# ============================
# Revenue by treatment group
# ============================
df_rev <- data %>% group_by(treatment_group) %>% summarise(n = n(), m = mean(revenue), sd = sd(revenue)/sqrt(length(revenue)))


# Bar chart with error bars
ggplot(df_rev) +
  geom_bar(aes(x = treatment_group, y = m), stat = "identity", fill = "skyblue")+ 
  geom_point(aes(x = treatment_group, y = m), size = 3)+
  geom_errorbar(aes(x = treatment_group, ymin = m-sd, ymax = m+sd), width = 0.4)+ 
  theme_bw() + 
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
         axis.text=element_text(size=18, face ="bold"),
         axis.title=element_text(size=18,face="bold"))+
  # ggtitle("Plot of Purchases vs Treatment group") +
  xlab(" ") +
  ylab("Revenue")

c_revenue <- data %>% filter(c== 0) %>% select(revenue)
t_revenue <- data %>% filter(c== 1) %>% select(revenue)


res <- t.test(t_revenue, c_revenue, var.equal = TRUE)
res

# ============================
# Purchases by treatment group
# ============================
df_purchase <- data %>% group_by(treatment_group) %>% summarise(m = mean(purchases), sd = sd(purchases)/sqrt(length(purchases)))


df_purchase_ct <- data %>% group_by(c) %>% summarise(n = n(),m = mean(purchases), sd = sd(purchases)/sqrt(length(purchases)))


# Bar chart with error bars
ggplot(df_purchase) +
  geom_bar(aes(x = treatment_group, y = m), stat = "identity", fill = "skyblue")+ 
  geom_point(aes(x = treatment_group, y = m), size = 3)+
  geom_errorbar(aes(x = treatment_group, ymin = m-sd, ymax = m+sd), width = 0.4)+ 
  theme_bw() + 
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
         axis.text=element_text(size=18, face ="bold"),
         axis.title=element_text(size=18,face="bold"))+
  # ggtitle("Plot of Purchases vs Treatment group") +
  xlab(" ") +
  ylab("Purchases")

c_purchase <- data %>% filter(c== 0) %>% select(purchases)
t_purchase <- data %>% filter(c== 1) %>% select(purchases)


res <- t.test(t_purchase, c_purchase, var.equal = TRUE)
res

# ============================
# Revenue by conversion days segment
# ============================

df_trtmt_promodays_range <- data %>%
  group_by(treatment_group) %>%
  summarise(n=n(), n_converted = sum(conversion_flag), min_promodays = min(days_first_promo_offer, na.rm = T), max_promodays = max(days_first_promo_offer, na.rm = T))


df_conv_days_rev <- data %>% filter(conversion_flag == 1) %>% group_by(conversion_days_segment) %>% summarize(n = n(), m = mean(revenue_per_days_left), sd = sd(revenue_per_days_left)/sqrt(length(revenue_per_days_left)))

# Count of users across each conversion days segment
ggplot(df_conv_days_rev) +
  geom_bar(aes(x = conversion_days_segment, y = n), stat = "identity", fill = "orange")+
  geom_text(aes(x = conversion_days_segment, y = n,label = n), size=6,vjust = 1.5)+
  theme_bw() + 
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
         axis.text=element_text(size=18, face ="bold"),
         axis.title=element_text(size=18,face="bold"))+
  # ggtitle("Plot of Purchases vs Treatment group") +
  xlab("Conversion Before/After Promotions segment") +
  ylab("Count of users")


# Bar chart with error bars
ggplot(df_conv_days_rev) +
  geom_bar(aes(x = conversion_days_segment, y = m), stat = "identity", fill = "skyblue")+ 
  geom_point(aes(x = conversion_days_segment, y = m), size = 3)+
  geom_errorbar(aes(x = conversion_days_segment, ymin = m-sd, ymax = m+sd), width = 0.4)+ 
  theme_bw() + 
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
         axis.text=element_text(size=18, face ="bold"),
         axis.title=element_text(size=18,face="bold"))+
  # ggtitle("Plot of Purchases vs Treatment group") +
  xlab("Conversion Before/After Promotions segment") +
  ylab("Revenue per user per days left after 1st purchase")


cp_bfr_revenue <- data %>% filter(conversion_days_segment== 'Converted before first promotion') %>% select(revenue_per_days_left)
cp_aft_revenue <- data %>% filter(conversion_days_segment== 'Converted after first promotion') %>% select(revenue_per_days_left)

res <- t.test(cp_aft_revenue, cp_bfr_revenue, var.equal = TRUE)
res

diff_fpur_fpromo_mean <- data %>% group_by(treatment_group) %>% summarise(grp.median=median(days_btwn_promo_first_purchase, na.rm = T))


ggplot(data) +
  # geom_bar(aes(x = conversion_ch1, y = m), stat = "identity", fill = "skyblue")+ 
  geom_histogram(aes(x = days_btwn_promo_first_purchase, color=treatment_group, fill= treatment_group), alpha=0.5, position="identity",binwidth = 10)+
  geom_vline(data=diff_fpur_fpromo_mean, aes(xintercept=grp.median, color=treatment_group),linetype="longdash")+
  theme_bw() + 
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
         axis.text=element_text(size=18, face ="bold"),
         axis.title=element_text(size=18,face="bold"))+
  scale_x_continuous(breaks = seq(-200, 200, by = 50))+
  # ggtitle("Plot of Purchases vs Treatment group") +
  xlab("Days between first promo offer and first purchase") +
  ylab("Count")


# ============================
# Revenue by conversion promo segment
# ============================

df_promo_rev <- data %>% filter(conversion_flag == 1 & control_flag==0) %>% group_by(conversion_promo_segment) %>% summarize(n = n(), m = mean(revenue_per_days_left), sd = sd(revenue_per_days_left)/sqrt(length(revenue_per_days_left)))


# Bar chart with error bars
ggplot(df_promo_rev) +
  geom_bar(aes(x = conversion_promo_segment, y = m), stat = "identity", fill = "skyblue")+ 
  geom_point(aes(x = conversion_promo_segment, y = m), size = 3)+
  geom_errorbar(aes(x = conversion_promo_segment, ymin = m-sd, ymax = m+sd), width = 0.4)+ 
  theme_bw() + 
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
         axis.text=element_text(size=18, face ="bold"),
         axis.title=element_text(size=18,face="bold"))+
  # ggtitle("Plot of Purchases vs Treatment group") +
  xlab("Conversion With/Without Promotions segment") +
  ylab("Revenue")


cp_yes_revenue <- data %>% filter(conversion_promo_segment== 'Conversion w/ Promo') %>% select(revenue_per_days_left)
cp_no_revenue <- data %>% filter(conversion_promo_segment== 'Conversion w/o Promo') %>% select(revenue_per_days_left)

res <- t.test(cp_yes_revenue, cp_no_revenue, var.equal = TRUE)
res



# ============================
# Revenue by FB connect segment
# ============================

df_fb_rev <- data %>% group_by(fb_connect_segment) %>% summarise(m = mean(revenue), sd = sd(revenue)/sqrt(length(revenue)))

df_fb_rev_ct <- data %>% group_by(fb_connect_segment) %>% summarise(n = n(), m = mean(revenue), sd = sd(revenue)/sqrt(length(revenue)))


# Bar chart with error bars
ggplot(df_fb_rev) +
  geom_bar(aes(x = fb_connect_segment, y = m), stat = "identity", fill = "skyblue")+ 
  geom_point(aes(x = fb_connect_segment, y = m), size = 3)+
  geom_errorbar(aes(x = fb_connect_segment, ymin = m-sd, ymax = m+sd), width = 0.4)+ 
  theme_bw() + 
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
         axis.text=element_text(size=18, face ="bold"),
         axis.title=element_text(size=18,face="bold"))+
  # ggtitle("Plot of Purchases vs Treatment group") +
  xlab("Facebook connect segment") +
  ylab("Revenue")

summary(aov(revenue ~ fb_connect_segment,data=data))
oneway.test(revenue ~ fb_connect_segment, data = data, var.equal = FALSE)


# ============================
# Sessions by treatment group
# ============================
df_sessions <- data %>% group_by(treatment_group) %>% summarise(n = n(), m = mean(sessions), sd = sd(sessions)/sqrt(length(sessions)))


# Bar chart with error bars
ggplot(df_sessions) +
  geom_bar(aes(x = treatment_group, y = m), stat = "identity", fill = "skyblue")+
  geom_point(aes(x = treatment_group, y = m), size = 3)+
  geom_errorbar(aes(x = treatment_group, ymin = m-sd, ymax = m+sd), width = 0.4)+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
         axis.text=element_text(size=18, face ="bold"),
         axis.title=element_text(size=18,face="bold"))+
  # ggtitle("Plot of Purchases vs Treatment group") +
  xlab(" ") +
  ylab("Sessions")

c_sessions <- data %>% filter(control_flag== 1) %>% select(sessions)
t_sessions <- data %>% filter(control_flag== 0) %>% select(sessions)


res <- t.test(t_sessions, c_sessions, var.equal = TRUE)
res


# ============================
# Hours in game by treatment group
# ============================
df_hours_in_game <- data %>% group_by(treatment_group) %>% summarise(n = n(), m = mean(hours_in_game), sd = sd(hours_in_game)/sqrt(length(hours_in_game)))


# Bar chart with error bars
ggplot(df_hours_in_game) +
  geom_bar(aes(x = treatment_group, y = m), stat = "identity", fill = "skyblue")+
  geom_point(aes(x = treatment_group, y = m), size = 3)+
  geom_errorbar(aes(x = treatment_group, ymin = m-sd, ymax = m+sd), width = 0.4)+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
         axis.text=element_text(size=18, face ="bold"),
         axis.title=element_text(size=18,face="bold"))+
  # ggtitle("Plot of Purchases vs Treatment group") +
  xlab(" ") +
  ylab("Hours in game")

c_hours_in_game <- data %>% filter(control_flag== 1) %>% select(hours_in_game)
t_hours_in_game <- data %>% filter(control_flag== 0) %>% select(hours_in_game)


res <- t.test(t_hours_in_game, c_hours_in_game, var.equal = TRUE)
res


# ============================
# Rounds played by treatment group
# ============================
df_rounds_played <- data %>% group_by(treatment_group) %>% summarise(n = n(), m = mean(rounds_played), sd = sd(rounds_played)/sqrt(length(rounds_played)))


# Bar chart with error bars
ggplot(df_rounds_played) +
  geom_bar(aes(x = treatment_group, y = m), stat = "identity", fill = "skyblue")+
  geom_point(aes(x = treatment_group, y = m), size = 3)+
  geom_errorbar(aes(x = treatment_group, ymin = m-sd, ymax = m+sd), width = 0.4)+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
         axis.text=element_text(size=18, face ="bold"),
         axis.title=element_text(size=18,face="bold"))+
  # ggtitle("Plot of Purchases vs Treatment group") +
  xlab(" ") +
  ylab("Rounds Played")

c_rounds_played <- data %>% filter(control_flag== 1) %>% select(rounds_played)
t_rounds_played <- data %>% filter(control_flag== 0) %>% select(rounds_played)


res <- t.test(t_rounds_played, c_rounds_played, var.equal = TRUE)
res

# ============================
# Active hours by treatment group
# ============================
df_active_hours <- data %>% group_by(treatment_group) %>% summarise(n = n(), m = mean(active_hours), sd = sd(active_hours)/sqrt(length(active_hours)))


# Bar chart with error bars
ggplot(df_active_hours) +
  geom_bar(aes(x = treatment_group, y = m), stat = "identity", fill = "skyblue")+
  geom_point(aes(x = treatment_group, y = m), size = 3)+
  geom_errorbar(aes(x = treatment_group, ymin = m-sd, ymax = m+sd), width = 0.4)+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
         axis.text=element_text(size=18, face ="bold"),
         axis.title=element_text(size=18,face="bold"))+
  # ggtitle("Plot of Purchases vs Treatment group") +
  xlab(" ") +
  ylab("Active Hours")

c_active_hours <- data %>% filter(control_flag== 1) %>% select(active_hours)
t_active_hours <- data %>% filter(control_flag== 0) %>% select(active_hours)


res <- t.test(t_active_hours, c_active_hours, var.equal = TRUE)
res

# ============================
# Active Days by treatment group
# ============================
df_active_days <- data %>% group_by(treatment_group) %>% summarise(n = n(), m = mean(active_days), sd = sd(active_days)/sqrt(length(active_days)))


# Bar chart with error bars
ggplot(df_active_days) +
  geom_bar(aes(x = treatment_group, y = m), stat = "identity", fill = "skyblue")+
  geom_point(aes(x = treatment_group, y = m), size = 3)+
  geom_errorbar(aes(x = treatment_group, ymin = m-sd, ymax = m+sd), width = 0.4)+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
         axis.text=element_text(size=18, face ="bold"),
         axis.title=element_text(size=18,face="bold"))+
  # ggtitle("Plot of Purchases vs Treatment group") +
  xlab(" ") +
  ylab("Active Days")

c_active_days <- data %>% filter(control_flag== 1) %>% select(active_days)
t_active_days <- data %>% filter(control_flag== 0) %>% select(active_days)


res <- t.test(t_active_days, c_active_days, var.equal = TRUE)
res

# ============================
# Key outcomes by treatment
# ============================
by_treatment <- data %>%
  group_by(treatment) %>%
  summarise(n = n(),
            conversion_mean = mean(conversion),
            conversion_se = sd(conversion)/sqrt(length(conversion)),
            purchases_mean = mean(purchases),
            purchases_se = sd(purchases)/sqrt(length(purchases)),
            revenue_mean = mean(revenue),
            revenue_se = sd(revenue)/sqrt(length(revenue)),
            sessions_mean = mean(sessions),
            sessions_se = sd(sessions)/sqrt(length(sessions))
  )

btm <- melt(by_treatment[,c('treatment','conversion_mean','purchases_mean','revenue_mean','sessions_mean')],id.vars = 1)
btm$variable <- substr(btm$variable,1,7)

btm_se <- melt(by_treatment[,c('treatment','conversion_se','purchases_se','revenue_se','sessions_se')],id.vars = 1)
btm_se$variable <- substr(btm_se$variable,1,7)

btm_all <- merge(x = btm, y = btm_se, by = c("treatment", "variable"), all.x = TRUE)

revenue_ci_plot <- ggplot(data=filter(btm_all,btm_all$variable=="revenue"), aes(x=treatment, y = value.x)) +
  geom_bar(stat = "identity", position = "dodge", colour="grey", fill="azure1") +
  geom_errorbar(aes(ymin = value.x - 1.96*value.y, ymax = value.x + 1.96*value.y), width = .2, colour="coral4", position = position_dodge(.9)) +
  xlab("Treatment") +
  ylab("Revenue")

print(revenue_ci_plot)

conversion_ci_plot <- ggplot(data=filter(btm_all,btm_all$variable=="convers"), aes(x=treatment, y = value.x)) +
  geom_bar(stat = "identity", position = "dodge", colour="grey", fill="beige") +
  geom_errorbar(aes(ymin = value.x - 1.96*value.y, ymax = value.x + 1.96*value.y), width = .2, colour="coral4", position = position_dodge(.9)) +
  xlab("Treatment") +
  ylab("Conversion")
print(conversion_ci_plot)


purchases_ci_plot <- ggplot(data=filter(btm_all,btm_all$variable=="purchas"), aes(x=treatment, y = value.x)) +
  geom_bar(stat = "identity", position = "dodge", colour="grey", fill="darkolivegreen1") +
  geom_errorbar(aes(ymin = value.x - 1.96*value.y, ymax = value.x + 1.96*value.y), width = .2, colour="coral4", position = position_dodge(.9)) +
  xlab("Treatment") +
  ylab("Purchases")
print(purchases_ci_plot)

sessions_ci_plot <- ggplot(data=filter(btm_all,btm_all$variable=="session"), aes(x=treatment, y = value.x)) +
  geom_bar(stat = "identity", position = "dodge", colour="grey", fill="chocolate") +
  geom_errorbar(aes(ymin = value.x - 1.96*value.y, ymax = value.x + 1.96*value.y), width = .2, colour="coral4", position = position_dodge(.9)) +
  xlab("Treatment") +
  ylab("Sessions") +
  ylim(170,190)
print(sessions_ci_plot)



# ============================
# treatment effects via regression
# ============================

treatment_conversion <- train(conversion ~ treatment,
                              data =data,
                              method = "lm")
summary(treatment_converison)

treatment_purchase <- train(purchases ~ treatment
                        ,
                        data = data,
                        method="lm")
summary(treatment_purchase)

treatment_revenue <- train(revenue ~ treatment
                   ,
                   data = data,
                   method="lm")
summary(treatment_revenue)


treatment_sessions <- train(sessions ~ treatment
                    ,
                    data = data,
                    method="lm")
summary(treatment_sessions)

# ============================
### treatment effect heterogeneity
# ============================
# treatment effect heterogeneity For revenue in dpi
rev_dpi <- train(revenue ~ treatment * dpi
                     ,
                     data = data,
                     method="lm")
summary(rev_dpi)

# treatment effect heterogeneity for revenue in median split on high value
rev_hv <- train(revenue ~ treatment * high_value
                    ,
                    data = data,
                    method="lm")
summary(rev_hv)

# treatment effect heterogeneity for revenue in device ram
rev_ram <- train(revenue ~ treatment * device_ram
                     ,
                     data = data,
                     method="lm")
summary(rev_ram)


data$treatment <- as.factor(data$treatment)
data$treatment <- relevel(data$treatment, ref = "no_promo")
# treatment effect heterogeneity for revenue in active hours
rev_ah <- lm(
  formula = revenue ~treatment * active_hours,
  data = data
)
print(rev_ah)
summary(rev_ah)

# treatment effect heterogeneity for revenue in active days
rev_ad <- lm(
  formula = revenue ~treatment * active_days,
  data = data
)
summary(rev_ad)

# treatment effect heterogeneity for revenue in sessions
rev_ses <- train(revenue ~ treatment * sessions
                 ,
                 data = data,
                 method="lm")
summary(rev_ses)

# treatment effect heterogeneity for revenue in badges earned
rev_badge <- train(revenue ~ treatment * badges_earned,
                 data = data,
                 method="lm")
summary(rev_badge)


# get median of device ram
summary(data)

# create a new column device_ram_compare_with_1796
new_wb<-wb[,]
new_wb$device_ram_compare_with_1796<-ifelse(wb$device_ram>1796,"over_than_1796","lower_than_1796")

# bar plot of badges_earned and device_ram_compare_with_1796
ggplot(new_wb) +
  geom_bar(aes(x = badges_earned, y = device_ram_compare_with_1796), stat = "identity", fill = "skyblue")+ 
  geom_point(aes(x = badges_earned, y = device_ram_compare_with_1796), size = 3)+
  theme_bw() + 
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
         axis.text=element_text(size=18, face ="bold"),
         axis.title=element_text(size=18,face="bold"))
# regression model of badges_earned and device_ram_compare_with_1796
be_ram1796 <- lm(
  formula = badges_earned ~ device_ram_compare_with_1796,
  data = new_wb
)
summary(be_ram1796)

# regression model of revenue and device_ram_compare_with_1796
r_ram1796 <- lm(
  formula = revenue ~ device_ram_compare_with_1796,
  data = new_wb
)
summary(r_ram1796)






