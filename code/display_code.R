# Kodiak black rockfish catch data displays
# -----------------------------------------
#
# Author: William Gaeuman
# Contact: william.gaeuman@alaska.gov
# Last modified: 2017-11-18 
#
# Displays for exploring relationship between 2012-2016 BRF sportfish and commfish catch 
# age distributions and potential trend in 2007-2017 commercial catch weight distribution.
# Consider only Kodiak Afognak, Northeast and Eastside sections.
# ----

# Load libraries, read in basic data
library(tidyverse)

sf<-read_csv("data/sportfish.csv")
cf<-read_csv("data/commfish.csv")


# Construct needed data structures

# 2012-2016 sportfish/commfish age data
cf.age <- filter(cf, year > 2011 & year < 2017,
		         species == 142, 
	               sex == 1 | sex == 2, 
	               !(is.na(age)) & age > 0, 
	               section == "Northeast" | section == "Afognak" | section == "Eastside") %>%
	transmute(Year = as.factor(year), 
                Section = factor(section, levels = c("Afognak","Northeast","Eastside")), 
	          Sex = factor(recode(sex, "1" = "M", "2" = "F")),
                age = age,
	          Fishery = "Commfish")

sf.age <- filter(sf, year > 2011,
	             species == 142, 
	             sex == "M" | sex == "F", 
	             !(is.na(age)) & age > 0, 
	             section == "Northeast" | section == "Afognak" | section == "Eastside") %>%
	transmute(Year = as.factor(year), 
		    Section = factor(section, levels = c("Afognak","Northeast","Eastside")), 
	          Sex = as.factor(sex),
                age = age,
		    Fishery = "Sportfish")

age.data <- rbind(sf.age, cf.age) %>% mutate(Fishery = as.factor(Fishery))

# Section proportions data
tot.cnts <- group_by(age.data, Year, Fishery, Sex) %>% 
	summarize(tot.cnt = n())

sec.cnts <- group_by(age.data, Year, Fishery, Sex, Section) %>%  
	summarize(cnt = n())

sec.props <- left_join(sec.cnts, tot.cnts) %>% 
	mutate(prop = cnt / tot.cnt, se = sqrt(prop * (1 - prop) / cnt), up = prop + se, lo = prop - se)

# 2007-2017 comm age/weight data (NOTE: 2017 age data not yet available; default value = 1 for this year)
age.wt.data <- filter(cf, species == 142, 
	                    sex == 1 | sex == 2, 
	                    !(is.na(weight)) & weight > 0, 
		              !(is.na(age)) & age > 0,
	                    section == "Northeast" | section == "Afognak" | section == "Eastside") %>%
	transmute(year = as.numeric(year), Year = as.factor(year),
                       Section = factor(section, levels = c("Afognak","Northeast","Eastside")), 
	                 Sex = factor(recode(sex, "1" = "M", "2" = "F")),
                       weight = 2.20462 * weight,
		           age = age)

# Generate figures

# Display 1
ggplot(sec.props, aes(Year, prop, color = Section)) + 
geom_line(aes(group = Section), position = position_dodge(0.4), size = 1) + 
geom_point(position = position_dodge(0.4), size = 2) +
geom_errorbar(aes(ymin = lo , ymax = up), width = 0.2, position = position_dodge(0.4))+ 
facet_grid(Fishery ~ Sex) + theme_bw() +
labs(title = "Section Composition of Black Rockfish Age Data by Year, Sex and Fishery\n(error bars represent one standard error)", 
y = "proportion of sampled fish", x = "year") +
theme(legend.position = "bottom")

ggsave("figures/display1.png", width = 8, height = 8)

# Display 2a
my.fun <- function(x){
return(data.frame(ymin = quantile(x, 0.25), 
                  y = median(x), 
                  ymax = quantile(x, 0.75)))
}

ggplot(age.data, aes(Year, age, color = Fishery)) + 
stat_summary(fun.data = my.fun, size = 1, position = position_dodge(.5)) +
theme_bw() + facet_grid(Section ~ Sex, scales = "free") + 
scale_color_manual(values = c("darkgreen", "darkorange")) + 
theme(legend.position = "bottom") +
labs(title = "Quartiles of Sportfish and Commfish Sampled Black Rockfish Ages 
by Year, Sex and Section", 
y = "age (yr)", x = "year") + 
coord_flip()

ggsave("figures/display2a.png", width = 8, height = 8)

#Display 2b
ggplot(age.data, aes(Year, age)) +  
geom_boxplot(aes(fill = Section)) +
theme_bw() + stat_summary(fun.y = mean, geom = "point", color = "gold", size = 4) +
theme(legend.position = "bottom") + facet_grid(Sex*Fishery ~ .) +
labs(title = "Sampled Black Rockfish Ages by Year, Sex and Fishery 
with Combined Section Averages (large yellow points)", y = "age (yr)", x = "year")

ggsave("figures/display2b.png",width = 8, height = 10)

# Display 3
ggplot(age.data, aes(age, fill = Fishery)) + geom_histogram(aes(y = ..density..), binwidth = 1, color = "black") + 
facet_grid(Sex*Fishery ~ Year) + theme_bw() + theme(legend.position = "bottom") + 
scale_fill_manual(values = c("darkgreen","darkorange")) +
labs(title = "Combined Afognak, Northeast, Eastside Sampled Black Rockfish Ages 
by Year, Sex and Fishery", x = "age (yr)")

ggsave("figures/display3.png", width = 8, height = 7)

# Display 4
ggplot(age.wt.data, aes(x = Year, y = weight)) + geom_boxplot(aes(fill = Section)) +
theme_bw() + facet_grid(Sex ~ .) + #coord_flip() +
theme(legend.position = "bottom") +
labs(title = "Sampled Commericial Black Rockfish Weights by Sex with Combined Section Averages 
(large yellow points)", y = "weight (lb)", x = "year") + 
stat_summary(fun.y = mean, geom = "point", color = "gold", size = 4)

ggsave("figures/display4.png", width = 8, height = 7)

# Display 5
ggplot(age.wt.data, aes(year, weight, color = Section)) +  
geom_point(position = position_dodge(.5), alpha = 0.4)+
geom_smooth(method = "lm", se = FALSE) +
theme_bw() + scale_x_continuous(breaks = 2007:2017) +
theme(legend.position = "bottom") + 
labs(title = "Sampled Commercial Black Rockfish Weights by Sex 
with Section Specific Linear Fits", y = "weight (lb)") + 
facet_grid(Sex ~ .)

ggsave("figures/display5.png", width = 6, height = 8)

# Display 6
ggplot(filter(age.wt.data, year < 2017), aes(age, weight, color = Sex)) +
geom_point(alpha = 0.3) +
geom_smooth(se = F) +
facet_wrap(~Year, ncol = 2) +
scale_color_manual(values = c("darkgreen", "darkorange")) +
theme_bw() +
labs(title = "Loess Smooths of Combined Section Commercial Black Rockfish Age-to-Weight Data",
y = "weight (lb)", x = "age (yr)") +
theme(legend.position = "bottom")

ggsave("figures/display6.png", width = 8, height = 10)

