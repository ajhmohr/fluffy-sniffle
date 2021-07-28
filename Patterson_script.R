## script
#adding something
data = read.csv(file="~/Desktop/My Stuff/Research Final 2/Data Stuff/Real Data Stuff/data_final2.csv", header=T)

## data processing

datalong <- data %>% 
  select(ResponseID, starts_with("DR_"), starts_with("IB_")) %>% 
  pivot_longer(cols = !ResponseID, 
               names_to = c(".value", "variable"), 
               names_pattern = "(IB|DR)_(.*)") %>% 
  filter(variable %in% c("title", "text", "text2", "text3")==FALSE)


datalong$IB_rev = 5 - datalong$IB


datalong$mvgroup = factor(gsub("_[[:digit:]]*", "", as.character(datalong$variable)))

#Create aggregate data - mean, sd, se by movement 
dataagg <- datalong %>% 
  group_by(variable) %>% 
  summarize(DR_mean = mean(DR, na.rm=T), 
            IB_rev_mean = mean(IB_rev, na.rm=T), 
            DR_sd = sd(DR, na.rm = T),
            IB_rev_sd = sd(IB_rev, na.rm = T),
            DR_se = DR_sd/sqrt(length(which(!is.na(DR)))),
            IB_rev_se = IB_rev_sd/sqrt(length(which(!is.na(IB)))))


dataagg$mvgroup = factor(gsub("_[[:digit:]]*", "", as.character(dataagg$variable)))

for (i in paste("IB_", dataagg$variable, sep="")) {
  data$a = 5 - data[,which(names(data)==i)]
  names(data)[which(names(data)=="a")] = paste(i, "rev", sep="_")
}

dataagg$MovmtNumber = as.numeric(gsub(".*_", "", dataagg$variable))

dataagg <- dataagg %>% 
  arrange(mvgroup, MovmtNumber) %>% 
  mutate(MovmtNumberContin = 1:30)


## Creating Scores
for (i in paste("DR_", dataagg$variable, sep="")) {
  data$a = ifelse(data[,which(names(data)==i)] > 2, 1, 0)
  names(data)[which(names(data)=="a")] = paste(i, "phigh", sep="_")
}

percentof1s = function(x) {round((sum(x, na.rm=T)/sum(complete.cases(x)))*100, 2)}

dataagg_p <- data %>% 
  select(contains("phigh")) %>% 
  summarize(across(.cols = everything(), .fns = percentof1s)) %>% 
  t()

dataagg$Percent_DR_High <-  dataagg_p
dataagg$checknames = names(dataagg_p)[1:30]

dataagg <- dataagg %>% 
  mutate(IB_rev_cat = case_when(IB_rev_mean > 3.01 ~ "2 to 7 days", 
                                between(IB_rev_mean, 2.51, 3.01) ~ "7 to 14 days", 
                                between(IB_rev_mean, 2.01, 2.51) ~ "14 to 21 days", 
                                between(IB_rev_mean, 1, 2.01) ~ "21 to 60 days")) 


## Figure 1
colors = c("#d7191c","#fdae61", "#ffffbf", "#abdda4", "#2b83ba")

dataagg$eb_min <- dataagg$IB_rev_mean - dataagg$IB_rev_se
dataagg$eb_max <- dataagg$IB_rev_mean + dataagg$IB_rev_se

dataagg$Percent_DR_high_nudge = dataagg$Percent_DR_High
dataagg$Percent_DR_high_nudge[1] = dataagg$Percent_DR_high_nudge[1]-3
dataagg$Percent_DR_high_nudge[21] = dataagg$Percent_DR_high_nudge[21]-3.3
dataagg$Percent_DR_high_nudge[6] = dataagg$Percent_DR_high_nudge[6]+3
dataagg$Percent_DR_high_nudge[10] = dataagg$Percent_DR_high_nudge[10]-3


ggplot(dataagg, aes(x=Percent_DR_High, y=IB_rev_mean)) + coord_cartesian(ylim=c(1,4), xlim=c(0,105)) +labs(title="Expert Consensus on Risk of Disease Spread and Average Time to Impact on Business",  x="Percent majority consensus of risk of disease spread", y="Average time until negative impact on business") + geom_vline(xintercept=c(25,75), color="gray") + geom_hline(yintercept=c(2, 2.5, 3), lty=2, color="gray") +  geom_point(aes(x=Percent_DR_High, y=IB_rev_mean, shape=mvgroup, fill=mvgroup), alpha=.75, size=8.5) + geom_errorbar(aes(ymin=eb_min, ymax=eb_max, color=mvgroup), width=1) + scale_shape_manual(values = c(21:25),name="Movement Type", labels=c("Equipment", "General", "Genetic", "Harvest", "Person"))  + scale_fill_manual(name="Movement Type", labels=c("Equipment", "General", "Genetic", "Harvest", "Person"), values=colors)  + scale_color_manual(name="Movement Type", labels=c("Equipment", "General", "Genetic", "Harvest", "Person"),     values=rep("darkgray", 5)) + scale_y_continuous(breaks = c(1, 2, 3, 4),labels=rev(c("< 48 hours",  "7 days",  "21 days", "> 60 days"))) +  geom_text(aes(x=Percent_DR_high_nudge,  y=IB_rev_mean, label=MovmtNumberContin), size=4) + annotate("text", x=12.5, y=1.2, label="Majority Low", size=4) + annotate("text", x=50, y=1.2, label="Unclear Consensus", size=4) + annotate("text", x=90, y=1.2, label="Majority High", size=4)  + theme_classic() 


## table

dataagg <- dataagg %>% 
  mutate(Category = case_when(mvgroup == "equip" ~ "Equipment", mvgroup == "general" ~ "General", mvgroup == "genetic" ~ "Genetic", mvgroup == "harvest" ~ "Harvest",         mvgroup == "person" ~ "Person"), NumCat = paste(Category, MovmtNumberContin, sep=" "))
dataagg <- dataagg %>% 
  mutate(Placement = case_when(between(Percent_DR_High, 25, 75) ~ "Unclear Consensus", 
                               Percent_DR_High <= 25 ~ "Low", 
                               Percent_DR_High >= 75 ~ "High"))

scores <- dataagg %>% 
  select(NumCat, Percent_DR_High, Placement, IB_rev_cat) %>% 
  rename(Movement = NumCat, 
         "Percent High RDS" = Percent_DR_High, 
         "Binned Risk of Disease Spread" = Placement, 
         "Time to Negative Business Impact" = IB_rev_cat)

scores <- scores %>% 
  mutate("Binned Risk of Disease Spread" = factor(`Binned Risk of Disease Spread`, levels=c("Low", "Unclear Consensus", "High")), 
         "Time to Negative Business Impact" = factor(`Time to Negative Business Impact`, levels = c("14 to 21 days", "7 to 14 days", "2 to 7 days")))

scores %>% 
  arrange(desc(`Binned Risk of Disease Spread`), desc(`Time to Negative Business Impact`), desc(`Percent High RDS`))

