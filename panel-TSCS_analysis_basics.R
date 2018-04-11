
# -------------------------------------------------------------------
# -------------------------------------------------------------------
#####                      TSCS/CSTS                    -------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------


library(XLConnect)
Data <- readWorksheet(loadWorkbook("D:/Documents/DATA.xlsx"),sheet=1)



# -------------------------------------------------------------------
#                            Graphs 
# -------------------------------------------------------------------

colnames(Data)

coplot(unemp_eurostat ~ year|as.factor(cntr), type="l", data=Data)
coplot(unemp_eurostat ~ year|as.factor(cntr), type="b", data=Data)

# -----

library(car)
scatterplot(unemp_eurostat~year|as.factor(cntr), boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=Data)
scatterplot(unemp_eurostat~year|cntr_name, data=Data)

# -----

library(gplots)
plotmeans (unemp_eurostat ~ cntr_name,n.label=FALSE, 
           barcol="red", col="red", ccol="black", xaxt="n", # xaxt="n" to delete cases
           main="Heterogeineity across countries", 
           xlab= "County", ylab = "Unemployment",
           data=Data)
# -----

library(ggplot2)

p <- ggplot(data = Data, aes(x = year, y = unemp_eurostat, group = cntr_name))

p + geom_point()
p + geom_line()
p + geom_line() + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 3) 
p + geom_line() + stat_summary(aes(group = 1), geom = "point", fun.y = quantile, 
                               fun.args=(list(probs = c(0.25, 0.75))), shape = 17, size = 3)


p + geom_line() + stat_smooth(aes(group = 1), color="red") + 
  stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 2) +
  xlab("Year") +
  ylab("Unemployment") +
  ggtitle("Unemployment by Country, 1990-2017") +
  theme_light() + 
  theme(plot.title = element_text(hjust=0.5))

p + geom_line() + 
  stat_smooth(aes(group = 1), method = "lm", se = TRUE, color="red") +
  stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 2) +
  xlab("Year") +
  ylab("Unemployment") +
  ggtitle("Unemployment by Country, 1990-2017") +
  theme_light() + 
  theme(plot.title = element_text(hjust=0.5))

ggplot(Data, aes(x = year, y = unemp_eurostat, colour=cntr_name)) +
  geom_line() +
  scale_colour_discrete(name = "Country") +
  xlab("Year") +
  ylab("Unemployment") +
  ggtitle("Unemployment by Country, 1990-2017") +
  theme_light() + 
  theme(plot.title = element_text(hjust=0.5))

Unemployment <- aggregate(Data$unemp_eurostat,by=list(Data$year), mean, na.rm=TRUE)
colnames(Unemployment) <- c("year", "unemp_eurostat")

ggplot(Unemployment, aes(x = year, y = unemp_eurostat)) +
  geom_line() +
  xlab("Year") +
  ylab("Unemployment") +
  ggtitle("Unemployment, 1990-2017") +
  theme_light() + 
  theme(plot.title = element_text(hjust=0.5))

ggplot(Data, aes(x=year, y=unemp_eurostat)) +
  geom_line(aes(group=cntr)) +
  facet_wrap(~ cntr_name) +
  xlab("Year") +
  ylab("Unemployment") +
  ggtitle("Unemployment by Country, 1990-2017") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust=0.5))

# -----

library(xts)
library(PerformanceAnalytics)

# option 1 
Unemployment <- aggregate(Data$unemp_eurostat,by=list(Data$year), mean, na.rm=TRUE)

Unemployment <- as.xts(Unemployment, 
                       order.by=as.Date(as.character(Unemployment$Group.1), "%Y", 
                                        origin="1995"))
plot(Unemployment[,"x"])
lines(Return.calculate(Unemployment[,"x"]), type="h", on=NA)

# option 2
Unemployment <- aggregate(Data$unemp_eurostat,by=list(Data$year), mean, na.rm=TRUE)
Unemployment <- as.xts(ts(Unemployment$x, frequency = 1, start = c(1990)))

events <- xts(letters[1], as.Date(c("2007-01-01")))

plot(Unemployment[,1])
addEventLines(events, c("foo"), lwd=2, lty=2, col = "black")
addPanel(Return.calculate, method="discrete", type="h", on=NA, col = c("red"))


# -------------------------------------------------------------------
#                            Analsis 
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# OLS
# -------------------------------------------------------------------

ols<-lm(unemp_eurostat ~ trade + party_vote + crisis_2007_2008, data=Data)
summary(ols)

# -------------------------------------------------------------------
# Fixed effects
# -------------------------------------------------------------------

fixed.dum <-lm(unemp_eurostat ~ trade + party_vote + factor(cntr) -1, data=Data)
summary(fixed.dum)

fixed.dum <-lm(unemp_eurostat ~ trade + party_vote + crisis_2007_2008 + factor(cntr) -1, data=Data)
summary(fixed.dum)


library(car)
scatterplot(Data$unemp_eurostat~Data$party_vote|Data$cntr_name, boxplots=FALSE, 
            xlab="x1", ylab="yhat",smooth=FALSE)
abline(lm(Panel$y~Panel$x1),lwd=3, col="red")


library(apsrtable)
apsrtable(ols,fixed.dum, model.names= c("OLS", "OLS_DUM"), stars="default")


library(plm)
fixed <-plm(unemp_eurostat ~ trade + party_vote + crisis_2007_2008,
            data=Data, index=c("cntr", "year"), model="within")
summary(fixed)

# -------------------------------------------------------------------
# Tests - fixed effects
# -------------------------------------------------------------------

# ---------------
# Testing for fixed effects

fixef(fixed)
pFtest(fixed, ols)#  null: OLS better than fixed


# ---------------
# Testing time-fixed effects. 
fixed.time <-plm(unemp_eurostat ~ trade + party_vote + crisis_2007_2008 + factor(year) -1 ,
            data=Data, index=c("cntr", "year"), model="within")
summary(fixed.time)

pFtest(fixed.time, fixed)
plmtest(fixed, c("time"), type=("bp"))
# he null is that no time-fixed effects needed; if < 0.05 then use time-fixed effects.


# ---------------
# Testing for cross-sectional dependence/contemporaneous correlation

pcdtest(fixed, test = c("lm")) # Breusch-Pagan LM

pcdtest(fixed, test = c("cd")) # Pasaran CD test


# ---------------
# Testing for serial correlation
pbgtest(fixed) # 


# ---------------
# Testing for heteroskedasticity

library(lmtest)
bptest(unemp_eurostat ~ trade + party_vote + crisis_2007_2008 + factor(cntr), data = Data, studentize=F)
# null hypothesis of no heteroskedasticity (homoskedasticity)


# ---------------
# Testing for unit roots/stationarity

Panel.set<-pdata.frame(Data, index = c("cntr", "year"))
library(tseries)

adf.test(na.omit(Panel.set$unemp_eurostat), k=1)
adf.test(na.omit(Panel.set$unemp_eurostat), k=2)


# -------------------------------------------------------------------
# Random effects
# -------------------------------------------------------------------

random <-plm(unemp_eurostat ~ trade + party_vote + crisis_2007_2008, 
             data=Data, index=c("cntr", "year"), model="random")
summary(random)

# Setting as panel data (an alternative way to run the  model above
Panel.set<-pdata.frame(Data, index = c("cntr", "year"))

# Random effects using panel setting (same output as above)
random.set<-plm(unemp_eurostat ~ trade + party_vote + crisis_2007_2008, 
                data = Panel.set, model="random")

summary(random.set)

# -------------------------------------------------------------------
# Tests - random effects
# -------------------------------------------------------------------

# ---------------
# Hausman test

phtest(fixed, random) # If this number is < 0.05 then use fixed effects


# ---------------
# Breusch-Pagan Lagrange Multiplier for random effects.

# Regular OLS (pooling model) using plm
pool <-plm(unemp_eurostat ~ trade + party_vote + crisis_2007_2008, data=Data, index=c("cntr", "year"), model="pooling")
summary(pool)

# Null is no panel effect (i.e. OLS better).
plmtest(pool, type=c("bp")) 
# fail to reject the null and conclude that random effects is not appropriate.


# -------------------------------------------------------------------
#     Robust covariance matrix estimation (Sandwich estimator)
# -------------------------------------------------------------------


# ---------------
# Random effect

library(lmtest)

coeftest(random) # Original coefficients
coeftest(random, vcovHC) # Heteroskedasticity consistent coefficients
coeftest(random, vcovHC(random, type = "HC3")) 

# The following shows the HC standard errors of the coefficients
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(random, type = x)))))

# ---------------
# Fixed effect

coeftest(fixed) # Original coefficients
coeftest(fixed, vcovHC) # Heteroskedasticity consistent coefficients
coeftest(fixed, vcovHC(fixed, method = "arellano")) # Heteroskedasticity consistent coefficients (Arellano)
coeftest(fixed, vcovHC(fixed, type = "HC3")) 

# The following shows the HC standard errors of the coefficients
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fixed, type = x)))))

# -------------------------------------------------------------------
#     Clustered standard errors 
# -------------------------------------------------------------------

coeftest(model.plm, vcov=vcovHC(model.plm,type="HC0",cluster="group"))

# -------------------------------------------------------------------
#     Panel corrected standard errors 
# -------------------------------------------------------------------

install.packages("pcse")
library(pcse)

Data_pcse <- na.omit(Data[,c("unemp_eurostat","trade","crisis_2007_2008","party_vote","cntr","year")])
fixed_pcse <-lm(unemp_eurostat ~ trade + party_vote + crisis_2007_2008 + factor(cntr) -1, data=Data_pcse)

pcse(fixed, groupN = Data$cntr, groupT = Data$year)

# or

library(lmtest)
coeftest(fixed, vcov=vcovBK)


