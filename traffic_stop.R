options(digits=10)
MT <- read.csv(file="MT_cleaned.csv", header=TRUE)
# Missing value is kept for this analysis
str(MT)
nrow(MT)
table(MT$driver_gender)/nrow(MT)

# prob of arrested if you have out of state plates
table(MT$is_arrested, useNA = "ifany")
table(MT$is_arrested, MT$out_of_state, useNA = "ifany")
4868/203641
# the prob of arrested if you do not have out of state plates
12190/616778
# how many more times likely
4868/203641/(12190/616778)
chi2 <- chisq.test(MT$is_arrested, MT$out_of_state)
chi2$statistic

sum(grepl("Speeding", MT$violation, ignore.case = FALSE))/nrow(MT)

VT <- read.csv(file="VT_cleaned.csv", header=TRUE)

# prop of DUI in MT
DUI_MT <- sum(grepl("DUI", MT$violation))/nrow(MT)
# prop of DUI in VT
DUI_VT <- sum(grepl("DUI", VT$violation))/nrow(VT)
DUI_MT/DUI_VT

# average manufacture year for each year
MT$stop_year <- as.integer(substr(MT$stop_date, 1,4))
MT$vehicle_year <- as.integer(as.character(df$vehicle_year))
# exlude NAs
MT_cp <- MT[complete.cases(MT$vehicle_year),]
model_year <- aggregate(MT_cp$vehicle_year, list(MT_cp$stop_year), mean)
colnames(model_year) <- c("Year_Stop", "Ave_Year_Make")
lm <- lm(Ave_Year_Make ~ Year_Stop, data = model_year)
summary(lm)
predict(lm, newdata = data.frame(Year_Stop=2020))
anova(lm)$'Pr(>F)'[1]

# combine two dataset
d <- c(substr(MT$stop_time, 1,2), substr(VT$stop_time, 1, 2))
table(d)
which.max(table(d)[2:25])
which.min(table(d)[2:25])

max(table(d)[2:25]) - min(table(d)[2:25])
