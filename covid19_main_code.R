library(ggplot2)
library(scales)
library(ggpubr)
library(nlme)
library(gam)
library(splines)

# ----- UNITED STATE'S ANALYSIS -----

corona_nyt_us <- read.csv("corona_us.csv")

corona_nyt_us$day <- c(0:(length(corona_nyt_us$date)-1))

corona_nyt_us$date <- as.Date(corona_nyt_us$date, format='%Y-%m-%d')
attach(corona_nyt_us)

ggplot(corona_nyt_us, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))

wilcox.test(cases,day)


# ----- UNITED STATES MODEL FITTING -----

# ----- MODEL FITTING FOR US COVID-19 CASES ----- 

# ----- Generalized Linear Model -----

log.ss_us_cases <- nls(cases ~ SSlogis(day, theta1, theta2, theta3))
phi1 <- summary(log.ss_us_cases)$coef[1]
phi2 <- -summary(log.ss_us_cases)$coef[2]/summary(log.ss_us_cases)$coef[3]
phi3 <- 1/summary(log.ss_us_cases)$coef[3]

plot(day, cases)

xs <- c(0:(length(day)-1))

log_model <- predict(log.ss_us_cases, data.frame(day= 0:max(day)))
lines(xs,log_model,col="lightsteelblue",lwd=3)



# ----- SPLINES -----

# create spline breaks
breaks <- c(0,55,70,90,105)

# create a discrete/categorical/factor version of weight:
day.d <- cut(day,breaks)
class(day.d) # "factor" means categorical in R

# create a few linear models:
lm.lin <- lm(cases ~ day)
lm.quad <- lm(cases ~ poly(day,2,raw=TRUE))
lm.cubic <- lm(cases ~ poly(day,3,raw=TRUE))
lm.cat <- lm(cases ~ day.d)

# add cubic:
ys <- predict(lm.cubic,newdata=data.frame(day.d=cut(xs,breaks)))
lines(xs,ys,col="green",lwd=2)

# add a step function:
lm.cat.pred <- predict(lm.cat,newdata=data.frame(day.d=cut(xs,breaks)))
lines(xs,ys,col="darkgreen",lwd=2)

# now let's fit a bent line (i.e., linear spline) with knots at 2 and 4:

# first let's create a positive part function:
pp <- function(x) ifelse(x >= 0,x,0)
# alternatively:
pp <- function(x) pmax(x,0)

# fit the bent line model:
x <- day
xt1 <- x
xt2 <- pp(x-55)
xt3 <- pp(x-80)
lm.b <- lm(cases ~ xt1 + xt2 + xt3)
summary(lm.b)

# create y values for plotting:
lm.b.pred <- predict(lm.b,newdata=data.frame(xt1=xs,xt2=pp(xs-55),xt3=pp(xs-80)))
lines(xs,ys,col="blue",lwd=3)


# ----- GAM -----

gam_model <- gam(cases ~ s(day), data = corona_nyt_us)
summary(gam_model)

gam.model.pred <- predict(gam_model, data.frame(day= 0:max(day)))


# ----- NLR, SPLINES, AND GAM MODELS PLOTTED -----

us_case_models <- ggplot(corona_nyt_us, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log_model), size = 1, col = "dodgerblue") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))

us_case_models <- ggplot(corona_nyt_us, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log_model), size = 1, col = "dodgerblue") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))

us_case_models <- ggplot(corona_nyt_us, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log_model), size = 1, col = "dodgerblue") +
  geom_line(aes(y = lm.b.pred), size = 1, col = "lightsteelblue") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))



# ----- Generalized Linear Model -----

log.ss_us_cases <- nls(cases ~ SSlogis(day, theta1, theta2, theta3))
phi1 <- summary(log.ss_us_cases)$coef[1]
phi2 <- -summary(log.ss_us_cases)$coef[2]/summary(log.ss_us_cases)$coef[3]
phi3 <- 1/summary(log.ss_us_cases)$coef[3]

plot(day, cases)

xs <- c(0:(length(day)-1))

log_model <- predict(log.ss_us_cases, data.frame(day= 0:max(day)))
lines(xs,log_model,col="lightsteelblue",lwd=3)



# ----- SPLINES -----

# create spline breaks
breaks <- c(0,55,70,90,105)

# create a discrete/categorical/factor version of weight:
day.d <- cut(day,breaks)
class(day.d) # "factor" means categorical in R

# create a few linear models:
lm.lin <- lm(cases ~ day)
lm.quad <- lm(cases ~ poly(day,2,raw=TRUE))
lm.cubic <- lm(cases ~ poly(day,3,raw=TRUE))
lm.cat <- lm(cases ~ day.d)

# add cubic:
ys <- predict(lm.cubic,newdata=data.frame(day.d=cut(xs,breaks)))
lines(xs,ys,col="green",lwd=2)

# add a step function:
lm.cat.pred <- predict(lm.cat,newdata=data.frame(day.d=cut(xs,breaks)))
lines(xs,ys,col="darkgreen",lwd=2)

# now let's fit a bent line (i.e., linear spline) with knots at 2 and 4:

# first let's create a positive part function:
pp <- function(x) ifelse(x >= 0,x,0)
# alternatively:
pp <- function(x) pmax(x,0)

# fit the bent line model:
x <- day
xt1 <- x
xt2 <- pp(x-55)
xt3 <- pp(x-80)
lm.b <- lm(cases ~ xt1 + xt2 + xt3)
summary(lm.b)

# create y values for plotting:
lm.b.pred <- predict(lm.b,newdata=data.frame(xt1=xs,xt2=pp(xs-55),xt3=pp(xs-80)))
lines(xs,ys,col="blue",lwd=3)


# ----- GAM -----

gam_model <- gam(cases ~ s(day), data = corona_nyt_us)
summary(gam_model)

gam.model.pred <- predict(gam_model, data.frame(day= 0:max(day)))


# ----- NLR, SPLINES, AND GAM MODELS PLOTTED -----

us_case_models <- ggplot(corona_nyt_us, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log_model), size = 1, col = "dodgerblue") +
  geom_line(aes(y = lm.b.pred), size = 1, col = "lightsteelblue") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))



# ----- MODEL FITTING FOR US COVID-19 DEATHS -----

# ----- NONLINEAR REGRESSION -----
attach(corona_nyt_us)

log.ss_us_deaths <- nls(deaths ~ SSlogis(day, theta1, theta2, theta3))
phi1 <- summary(log.ss_us_deaths)$coef[1]
phi2 <- -summary(log.ss_us_deaths)$coef[2]/summary(log.ss_us_deaths)$coef[3]
phi3 <- 1/summary(log.ss_us_deaths)$coef[3]

plot(day, deaths)

xs <- c(0:(length(day)-1))

log.model.pred <- predict(log.ss_us_deaths, data.frame(day= 0:max(day)))


# ----- SPLINES -----

# now let's fit a bent line (i.e., linear spline) with knots at 2 and 4:

# first let's create a positive part function:
pp <- function(x) ifelse(x >= 0,x,0)
# alternatively:
pp <- function(x) pmax(x,0)

# fit the bent line model:
x <- day
xt1 <- x
xt2 <- pp(x-63)
xt3 <- pp(x-80)
lm.b <- lm(deaths ~ xt1 + xt2 + xt3)
summary(lm.b)

# create y values for plotting:
lm.b.pred <- predict(lm.b,newdata=data.frame(xt1=xs,xt2=pp(xs-63),xt3=pp(xs-80)))
lines(xs,ys,col="blue",lwd=3)


# ----- GAM -----

gam_model <- gam(deaths ~ s(day), data = corona_nyt_us)
summary(gam_model)

gam.model.pred <- predict(gam_model, data.frame(day= 0:max(day)))


# ----- NLR, SPLINES, AND GAM MODELS PLOTTED -----

us_death_model1 <- ggplot(corona_nyt_us, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  labs(x='Day',y='Deaths') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))

us_death_model2 <- ggplot(corona_nyt_us, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Deaths') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


us_death_model3 <- ggplot(corona_nyt_us, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  geom_line(aes(y = lm.b.pred), size = 1, col = "purple") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Deaths') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


# ----- STATE MODEL FITTING -----

corona <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")


# ----- CALIFORNIA MODEL FITTING -----

corona_california <- subset(corona, state == "California")

day <- c(0:(length(corona_california$date)-1))
corona_california$day <- day

corona_california$cum_cases <- corona_california$cases

corona_california$cases <- prepend(corona_california$cum_cases[-1] - corona_california$cum_cases[-length(corona_california$cum_cases)], 0)

corona_california$cum_deaths <- corona_california$deaths

corona_california$deaths <- prepend(corona_california$cum_deaths[-1] - corona_california$cum_deaths[-length(corona_california$cum_deaths)], 0)

attach(corona_california)

# ----- MODEL FITTING FOR CALIFORNIA COVID-19 CASES -----


# ----- NONLINEAR REGRESSION -----

log.ss_california_cases <- nls(cases ~ SSlogis(day, theta1, theta2, theta3))
phi1 <- summary(log.ss_california_cases)$coef[1]
phi2 <- -summary(log.ss_california_cases)$coef[2]/summary(log.ss_california_cases)$coef[3]
phi3 <- 1/summary(log.ss_california_cases)$coef[3]

plot(day, cases)

xs <- c(0:(length(day)-1))

log.model.pred <- predict(log.ss_california_cases, data.frame(day= 0:max(day)))


# ----- SPLINES -----

# now let's fit a bent line (i.e., linear spline) with knots at 2 and 4:

# first let's create a positive part function:
pp <- function(x) ifelse(x >= 0,x,0)
# alternatively:
pp <- function(x) pmax(x,0)

# fit the bent line model:
x <- day
xt1 <- x
xt2 <- pp(x-50)
xt3 <- pp(x-70)
lm.b <- lm(cases ~ xt1 + xt2 + xt3)
summary(lm.b)

# create y values for plotting:
lm.b.pred <- predict(lm.b,newdata=data.frame(xt1=xs,xt2=pp(xs-50),xt3=pp(xs-70)))
lines(xs,ys,col="blue",lwd=3)


fit <- lm(cases ~ bs(day, knots=c(63,90)), data = corona_california)
summary(fit)
pred.ns <- predict(fit, data.frame(day= 0:max(day)))


# ----- GAM -----
library(gam)

gam_model <- gam(cases ~ s(day), data = corona_california)
summary(gam_model)

gam.model.pred <- predict(gam_model, data.frame(day= 0:max(day)))


# ----- NLR, SPLINES, AND GAM MODELS PLOTTED -----

cal_case_model1 <- ggplot(corona_california, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))

cal_case_model2 <- ggplot(corona_california, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


cal_case_model3 <- ggplot(corona_california, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  geom_line(aes(y = lm.b.pred), size = 1, col = "purple") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


# ----- MODEL FITTING FOR CALIFORNIA COVID-19 DEATHS -----

# ----- NONLINEAR REGRESSION -----
attach(corona_california)

log.ss_california_deaths <- nls(deaths ~ SSlogis(day, theta1, theta2, theta3))
phi1 <- summary(log.ss_california_deaths)$coef[1]
phi2 <- -summary(log.ss_california_deaths)$coef[2]/summary(log.ss_california_deaths)$coef[3]
phi3 <- 1/summary(log.ss_california_deaths)$coef[3]

plot(day, deaths)

xs <- c(0:(length(day)-1))

log.model.pred <- predict(log.ss_california_deaths, data.frame(day= 0:max(day)))


# ----- SPLINES -----

# now let's fit a bent line (i.e., linear spline) with knots at 2 and 4:

# first let's create a positive part function:
pp <- function(x) ifelse(x >= 0,x,0)
# alternatively:
pp <- function(x) pmax(x,0)

# fit the bent line model:
x <- day
xt1 <- x
xt2 <- pp(x-55)
xt3 <- pp(x-85)
lm.b <- lm(deaths ~ xt1 + xt2 + xt3)
summary(lm.b)

# create y values for plotting:
lm.b.pred <- predict(lm.b,newdata=data.frame(xt1=xs,xt2=pp(xs-55),xt3=pp(xs-85)))
lines(xs,ys,col="blue",lwd=3)


fit <- lm(deaths ~ bs(day, knots=c(50,80)), data = corona_california)
summary(fit)
pred.ns <- predict(fit, data.frame(day= 0:max(day)))


# ----- GAM -----

gam_model <- gam(deaths ~ s(day), data = corona_california)
summary(gam_model)

gam.model.pred <- predict(gam_model, data.frame(day= 0:max(day)))


# ----- NLR, SPLINES, AND GAM MODELS PLOTTED -----

cal_case_model1 <- ggplot(corona_california, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  labs(x='Day',y='Deaths') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))

cal_case_model2 <- ggplot(corona_california, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Deaths') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


cal_case_model3 <- ggplot(corona_california, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  geom_line(aes(y = lm.b.pred), size = 1, col = "purple") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Deaths') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


# ----- OKLAHOMA MODEL FITTING -----

corona_oklahoma <- subset(corona, state == "Oklahoma")

day <- c(0:(length(corona_oklahoma$date)-1))
corona_oklahoma$day <- day

corona_oklahoma$cum_cases <- corona_oklahoma$cases

corona_oklahoma$cases <- prepend(corona_oklahoma$cum_cases[-1] - corona_oklahoma$cum_cases[-length(corona_oklahoma$cum_cases)], 0)

corona_oklahoma$cum_deaths <- corona_oklahoma$deaths

corona_oklahoma$deaths <- prepend(corona_oklahoma$cum_deaths[-1] - corona_oklahoma$cum_deaths[-length(corona_oklahoma$cum_deaths)], 0)

attach(corona_oklahoma)

# ----- MODEL FITTING FOR OKLAHOMA COVID-19 CASES -----

# ----- NONLINEAR REGRESSION -----

log.ss_oklahoma_cases <- nls(cases ~ SSlogis(day, theta1, theta2, theta3))
phi1 <- summary(log.ss_oklahoma_cases)$coef[1]
phi2 <- -summary(log.ss_oklahoma_cases)$coef[2]/summary(log.ss_oklahoma_cases)$coef[3]
phi3 <- 1/summary(log.ss_oklahoma_cases)$coef[3]
summary(log.ss_oklahoma_cases)

plot(day, cases)

xs <- c(0:(length(day)-1))

log.model.pred <- predict(log.ss_oklahoma_cases, data.frame(day= 0:max(day)))


# ----- SPLINES -----

# now let's fit a bent line (i.e., linear spline) with knots at 2 and 4:

# first let's create a positive part function:
pp <- function(x) ifelse(x >= 0,x,0)
# alternatively:
pp <- function(x) pmax(x,0)

# fit the bent line model:
x <- day
xt1 <- x
xt2 <- pp(x-15)
xt3 <- pp(x-25)
lm.b <- lm(cases ~ xt1 + xt2 + xt3)
summary(lm.b)

# create y values for plotting:
lm.b.pred <- predict(lm.b,newdata=data.frame(xt1=xs,xt2=pp(xs-15),xt3=pp(xs-25)))
lines(xs,ys,col="blue",lwd=3)


# ----- GAM -----

gam_model <- gam(cases ~ s(day), data = corona_oklahoma)
summary(gam_model)

gam.model.pred <- predict(gam_model, data.frame(day= 0:max(day)))


# ----- NLR, SPLINES, AND GAM MODELS PLOTTED -----

cal_case_model1 <- ggplot(corona_oklahoma, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))

cal_case_model2 <- ggplot(corona_oklahoma, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


cal_case_model3 <- ggplot(corona_oklahoma, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  geom_line(aes(y = lm.b.pred), size = 1, col = "purple") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


# ----- MODEL FITTING FOR OKLAHOMA COVID-19 DEATHS -----


# ----- NONLINEAR REGRESSION -----

log.ss_oklahoma_deaths <- nls(deaths ~ SSlogis(day, theta1, theta2, theta3))
phi1 <- summary(log.ss_oklahoma_deaths)$coef[1]
phi2 <- -summary(log.ss_oklahoma_deaths)$coef[2]/summary(log.ss_oklahoma_deaths)$coef[3]
phi3 <- 1/summary(log.ss_oklahoma_deaths)$coef[3]
summary(log.ss_oklahoma_deaths)

plot(day, deaths)

xs <- c(0:(length(day)-1))

log.model.pred <- predict(log.ss_oklahoma_deaths, data.frame(day= 0:max(day)))


# ----- SPLINES -----

# now let's fit a bent line (i.e., linear spline) with knots at 2 and 4:

# first let's create a positive part function:
pp <- function(x) ifelse(x >= 0,x,0)
# alternatively:
pp <- function(x) pmax(x,0)

# fit the bent line model:
x <- day
xt1 <- x
xt2 <- pp(x-15)
xt3 <- pp(x-30)
lm.b <- lm(deaths ~ xt1 + xt2 + xt3)
summary(lm.b)

# create y values for plotting:
lm.b.pred <- predict(lm.b,newdata=data.frame(xt1=xs,xt2=pp(xs-15),xt3=pp(xs-30)))
lines(xs,ys,col="blue",lwd=3)


fit <- lm(deaths ~ bs(day, knots=c(50,80)), data = corona_california)
summary(fit)
pred.ns <- predict(fit, data.frame(day= 0:max(day)))


# ----- GAM -----

gam_model <- gam(deaths ~ s(day), data = corona_oklahoma)
summary(gam_model)

gam.model.pred <- predict(gam_model, data.frame(day= 0:max(day)))


# ----- NLR, SPLINES, AND GAM MODELS PLOTTED -----

cal_case_model1 <- ggplot(corona_oklahoma, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  labs(x='Day',y='Deaths') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))

cal_case_model2 <- ggplot(corona_oklahoma, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  geom_line(aes(y = lm.b.pred), size = 1, col = "purple") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


cal_case_model3 <- ggplot(corona_oklahoma, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  geom_line(aes(y = lm.b.pred), size = 1, col = "purple") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))



# ----- NEW YORK MODEL FITTING -----

corona_ny <- subset(corona, state == "New York")

day <- c(0:(length(corona_ny$date)-1))
corona_ny$day <- day

corona_ny$cum_cases <- corona_ny$cases

corona_ny$cases <- prepend(corona_ny$cum_cases[-1] - corona_ny$cum_cases[-length(corona_ny$cum_cases)], 0)

corona_ny$cum_deaths <- corona_ny$deaths

corona_ny$deaths <- prepend(corona_ny$cum_deaths[-1] - corona_ny$cum_deaths[-length(corona_ny$cum_deaths)], 0)

attach(corona_ny)

# ----- MODEL FITTING FOR NEW YORK COVID-19 CASES -----


# ----- NONLINEAR REGRESSION -----

log.ss_ny_cases <- nls(cases ~ SSlogis(day, theta1, theta2, theta3))
phi1 <- summary(log.ss_ny_cases)$coef[1]
phi2 <- -summary(log.ss_ny_cases)$coef[2]/summary(log.ss_ny_cases)$coef[3]
phi3 <- 1/summary(log.ss_ny_cases)$coef[3]
summary(log.ss_ny_cases)

plot(day, cases)

xs <- c(0:(length(day)-1))

log.model.pred <- predict(log.ss_ny_cases, data.frame(day= 0:max(day)))


# ----- SPLINES -----

# now let's fit a bent line (i.e., linear spline) with knots at 2 and 4:

# first let's create a positive part function:
pp <- function(x) ifelse(x >= 0,x,0)
# alternatively:
pp <- function(x) pmax(x,0)

# fit the bent line model:
x <- day
xt1 <- x
xt2 <- pp(x-15)
xt3 <- pp(x-35)
lm.b <- lm(cases ~ xt1 + xt2 + xt3)
summary(lm.b)

# create y values for plotting:
lm.b.pred <- predict(lm.b,newdata=data.frame(xt1=xs,xt2=pp(xs-15),xt3=pp(xs-35)))
lines(xs,ys,col="blue",lwd=3)


# ----- GAM -----

gam_model <- gam(cases ~ s(day), data = corona_ny)
summary(gam_model)

gam.model.pred <- predict(gam_model, data.frame(day= 0:max(day)))


# ----- NLR, SPLINES, AND GAM MODELS PLOTTED -----

ny_case_model1 <- ggplot(corona_ny, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))

ny_case_model2 <- ggplot(corona_ny, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


ny_case_model3 <- ggplot(corona_ny, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  geom_line(aes(y = lm.b.pred), size = 1, col = "purple") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


# ----- MODEL FITTING FOR NEW YORK COVID-19 DEATHS -----


# ----- NONLINEAR REGRESSION -----

log.ss_ny_deaths <- nls(deaths ~ SSlogis(day, theta1, theta2, theta3))
phi1 <- summary(log.ss_ny_deaths)$coef[1]
phi2 <- -summary(log.ss_ny_deaths)$coef[2]/summary(log.ss_ny_deaths)$coef[3]
phi3 <- 1/summary(log.ss_ny_deaths)$coef[3]
summary(log.ss_ny_deaths)

plot(day, deaths)

xs <- c(0:(length(day)-1))

log.model.pred <- predict(log.ss_ny_deaths, data.frame(day= 0:max(day)))


# ----- SPLINES -----

# now let's fit a bent line (i.e., linear spline) with knots at 2 and 4:

# first let's create a positive part function:
pp <- function(x) ifelse(x >= 0,x,0)
# alternatively:
pp <- function(x) pmax(x,0)

# fit the bent line model:
x <- day
xt1 <- x
xt2 <- pp(x-20)
xt3 <- pp(x-40)
lm.b <- lm(deaths ~ xt1 + xt2 + xt3)
summary(lm.b)

# create y values for plotting:
lm.b.pred <- predict(lm.b,newdata=data.frame(xt1=xs,xt2=pp(xs-20),xt3=pp(xs-40)))
lines(xs,ys,col="blue",lwd=3)



# ----- GAM -----

gam_model <- gam(deaths ~ s(day), data = corona_ny)
summary(gam_model)

gam.model.pred <- predict(gam_model, data.frame(day= 0:max(day)))


# ----- NLR, SPLINES, AND GAM MODELS PLOTTED -----

ny_case_model1 <- ggplot(corona_ny, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  labs(x='Day',y='Deaths') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))

ny_case_model2 <- ggplot(corona_ny, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


ny_case_model3 <- ggplot(corona_ny, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = log.model.pred), size = 1, col = "dodgerblue") +
  geom_line(aes(y = lm.b.pred), size = 1, col = "purple") +
  geom_line(aes(y = gam.model.pred), size = 1, col = "green") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


# ----- TIME SERIES ANALYSIS -----

# This would only be used if I decide to use cumulative cases instead of new cases

 # time series plot
time_plot <- ggplot(corona_nyt_us, aes(x = date, y = cum_cases)) + geom_line() +
   scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
   theme_classic()

(decomp_2 <- ggplot(corona_nyt_us, aes(x = date, y = cum_cases)) +
    geom_line() +
    geom_smooth(method = "loess", se = FALSE, span = 0.6) +
    theme_classic())



# ----- STATE ANALYSIS -----

corona <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# ----- STATES WITH MOST CUMULATIVE CASES AND DEATHS -----

statelist <- c("California", "Connecticut", "Illinois", "Louisiana", "New Jersey", "New York", "Ohio", "Oregon", "Washington", "Colorado", "Delaware", "Hawaii", "Idaho", "Indiana", "Kentucky", "Massachusetts", "Michigan", "New Mexico", "Vermont", "West Virginia", "Wisconsin", "Alaska", "Kansas", "Maryland", "Minnesota", "Montana", "New Hampshire", "North Carolina", "Rhode Island", "Tennessee", "Virginia", "Wyoming", "Alabama", "Arizona", "District of Columbia","Florida", "Georgia", "Maine", "Mississippi", "Missouri", "Nevada", "Pennsylvania", "South Carolina", "Texas","Utah", "Iowa", "Arkansas", "Nebraska", "South Dakota", "North Dakota", "Oklahoma")

# ----- Top Cases and Death Numbers and Percentages as of 04/04/2020 -----

corona_040420 <- head(corona, 1829)

result <- c()
for(i in statelist) {
  statedata <- subset(corona_040420, corona_040420$state == i)
  statecases <- statedata$cases
  result[i] <- tail(statecases, 1)
}

result2 <- c()
for(i in statelist) {
  statedata <- subset(corona_040420, corona_040420$state == i)
  statedeaths <- statedata$deaths
  result2[i] <- tail(statedeaths, 1)
}

states_sorted_cases_040420 <- sort(result)
top10statescases_040420 <- tail(states_sorted_cases_040420, 10)

states_sorted_deaths_040420 <- sort(result2)
top10statesdeaths_040420 <- tail(states_sorted_deaths_040420, 10)

topstatecases_040420 <- c("Washington", "Pennsylvania", "Illinois", "Florida",  "Massachusetts", "Louisiana",  "California",  "Michigan", "New Jersey", "New York")

topstatedeaths_040420 <- c("Florida", "Georgia", "Massachusetts", "Illinois", "Washington", "California", "Louisiana", "Michigan", "New Jersey", "New York")

topstates_040420 <- data.frame(top10statescases_040420=top10statescases_040420, topstatecases_040420=topstatecases_040420, top10statesdeaths_040420 = top10statesdeaths_040420, topstatedeaths_040420 = topstatedeaths_040420 )

bxp_topstatecases_040420 <- ggplot(topstates_040420, aes(x = topstatecases_040420, y = top10statescases_040420)) + 
  geom_bar(stat="identity", fill="dodgerblue") +
  labs(x='State',y='Cases')+
  scale_y_continuous(labels = comma, limits = c(0, 300000)) +
  ggtitle("Cases as of 04/04/20") +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=90, hjust=1))

bxp_topstatedeaths_040420 <- ggplot(topstates_040420, aes(x = topstatedeaths_040420, y = top10statesdeaths_040420)) + 
  geom_bar(stat="identity", fill="dodgerblue") +
  labs(x='State',y='Deaths')+
  scale_y_continuous(labels = comma, limits = c(0, 17000)) +
  ggtitle("Deaths as of 04/04/20") +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=90, hjust=1))



# ----- Top Cases and Death Numbers and Percentages as of 04/11/2020 -----

corona_041120 <- head(corona, 2217)

result <- c()
for(i in statelist) {
  statedata <- subset(corona_041120, corona_041120$state == i)
  statecases <- statedata$cases
  result[i] <- tail(statecases, 1)
}

result2 <- c()
for(i in statelist) {
  statedata <- subset(corona_041120, corona_041120$state == i)
  statedeaths <- statedata$deaths
  result2[i] <- tail(statedeaths, 1)
}

states_sorted_cases_041120 <- sort(result)
top10statescases_041120 <- tail(states_sorted_cases_041120, 10)

states_sorted_deaths_041120 <- sort(result2)
top10statesdeaths_041120 <- tail(states_sorted_deaths_041120, 10)

topstatecases_041120 <- c("Texas","Florida", "Illinois", "Louisiana", "Pennsylvania", "California", "Massachusetts", "Michigan", "New Jersey", "New York")

topstatedeaths_041120 <- c("Connecticut", "Washington", "Pennsylvania", "California", "Illinois", "Massachusetts", "Louisiana", "Michigan", "New Jersey", "New York")

topstates_041120 <- data.frame(top10statescases_041120=top10statescases_041120, topstatecases_041120=topstatecases_041120, top10statesdeaths_041120 = top10statesdeaths_041120, topstatedeaths_041120 = topstatedeaths_041120 )

bxp_topstatecases_041120 <- ggplot(topstates_041120, aes(x = topstatecases_041120, y = top10statescases_041120)) + 
  geom_bar(stat="identity", fill="skyblue") +
  labs(x='State',y='Cases')+
  scale_y_continuous(labels = comma, limits = c(0, 300000)) +
  ggtitle("Cases as of 04/11/20") +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=90, hjust=1))

bxp_topstatedeaths_041120 <- ggplot(topstates_041120, aes(x = topstatedeaths_041120, y = top10statesdeaths_041120)) + 
  geom_bar(stat="identity", fill="skyblue") +
  labs(x='State',y='Deaths')+
  scale_y_continuous(labels = comma, limits = c(0, 17000)) +
  ggtitle("Deaths as of 04/11/20") +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=90, hjust=1))


# ----- Top Cases and Death Numbers and Percentages as of 04/18/2020 -----

corona_041820 <- head(corona, 2609)

result <- c()
for(i in statelist) {
  statedata <- subset(corona_041820, corona_041820$state == i)
  statecases <- statedata$cases
  result[i] <- tail(statecases, 1)
}

result2 <- c()
for(i in statelist) {
  statedata <- subset(corona_041820, corona_041820$state == i)
  statedeaths <- statedata$deaths
  result2[i] <- tail(statedeaths, 1)
}

states_sorted_cases_041820 <- sort(result)
top10statescases_041820 <- tail(states_sorted_cases_041820, 10)

states_sorted_deaths_041820 <- sort(result2)
top10statesdeaths_041820 <- tail(states_sorted_deaths_041820, 10)

topstatecases_041820 <- c("Texas", "Louisiana", "Florida", "Illinois", "Michigan", "California", "Pennsylvania", "Massachusetts", "New Jersey", "New York")

topstatedeaths_041820 <- c("Florida", "Connecticut", "California", "Pennsylvania", "Louisiana", "Illinois", "Massachusetts", "Michigan", "New Jersey", "New York")


topstates_041820 <- data.frame(top10statescases_041820=top10statescases_041820, topstatecases_041820=topstatecases_041820, top10statesdeaths_041820 = top10statesdeaths_041820, topstatedeaths_041820 = topstatedeaths_041820)

bxp_topstatecases_041820 <- ggplot(topstates_041820, aes(x = topstatecases_041820, y = top10statescases_041820)) + 
  geom_bar(stat="identity", fill="steelblue") +
  labs(x='State',y='Cases') +
  scale_y_continuous(labels = comma, limits = c(0, 300000)) +
  ggtitle("Cases as of 04/18/20") +
  theme(text = element_text(size=3.5),
        axis.text.x = element_text(angle=90, hjust=1))

bxp_topstatedeaths_041820 <- ggplot(topstates_041820, aes(x = topstatedeaths_041820, y = top10statesdeaths_041820)) + 
  geom_bar(stat="identity", fill="steelblue") +
  labs(x='State',y='Deaths') +
  scale_y_continuous(labels = comma, limits = c(0, 17000)) +
  ggtitle("Deaths as of 04/18/20") +
  theme(text = element_text(size=3.5),
        axis.text.x = element_text(angle=90, hjust=1))

# ----- Top Cases and Death Numbers and Percentages as of 04/18/2020 -----

corona_042520 <- head(corona, 2984)

result <- c()
for(i in statelist) {
  statedata <- subset(corona_042520, corona_042520$state == i)
  statecases <- statedata$cases
  result[i] <- tail(statecases, 1)
}

result2 <- c()
for(i in statelist) {
  statedata <- subset(corona_042520, corona_042520$state == i)
  statedeaths <- statedata$deaths
  result2[i] <- tail(statedeaths, 1)
}

states_sorted_cases_042520 <- sort(result)
top10statescases_042520 <- tail(states_sorted_cases_042520, 10)

states_sorted_deaths_042520 <- sort(result2)
top10statesdeaths_042520 <- tail(states_sorted_deaths_042520, 10)

topstatecases_042520 <- c("Connecticut", "Louisiana", "Florida", "Michigan", "Pennsylvania", "Illinois", "California","Massachusetts", "New Jersey", "New York")

topstatedeaths_042520 <- c("Florida", "Louisiana", "California", "Pennsylvania", "Connecticut", "Illinois",    "Massachusetts", "Michigan", "New Jersey", "New York")


topstates_042520 <- data.frame(top10statescases_042520=top10statescases_042520, topstatecases_042520=topstatecases_042520, top10statesdeaths_042520 = top10statesdeaths_042520, topstatedeaths_042520 = topstatedeaths_042520)

bxp_topstatecases_042520 <- ggplot(topstates_042520, aes(x = topstatecases_042520, y = top10statescases_042520)) + 
  geom_bar(stat="identity", fill="deepskyblue") +
  labs(x='State',y='Cases') +
  scale_y_continuous(labels = comma, limits = c(0, 300000)) +
  ggtitle("Cases as of 04/25/20") +
  theme(text = element_text(size=3.5),
        axis.text.x = element_text(angle=90, hjust=1))

bxp_topstatedeaths_042520 <- ggplot(topstates_042520, aes(x = topstatedeaths_042520, y = top10statesdeaths_042520)) + 
  geom_bar(stat="identity", fill="deepskyblue") +
  labs(x='State',y='Deaths') +
  scale_y_continuous(labels = comma, limits = c(0, 17000)) +
  ggtitle("Deaths as of 04/25/20") +
  theme(text = element_text(size=3.5),
        axis.text.x = element_text(angle=90, hjust=1))



ggarrange(bxp_topstatecases_040420, bxp_topstatecases_041120, bxp_topstatecases_041820, bxp_topstatecases_042520, ncol = 2, nrow = 2)

ggarrange(bxp_topstatedeaths_040420, bxp_topstatedeaths_041120, bxp_topstatedeaths_041820, bxp_topstatedeaths_042520, ncol = 2, nrow = 2)



# ----- STATES WITH HIGHEST PROPORTION OF CUMULATIVE CASES AND DEATHS -----

require(scales)

statelist <- c("California", "Connecticut", "Illinois", "Louisiana", "New Jersey", "New York", "Ohio", "Oregon", "Washington", "Colorado", "Delaware", "Hawaii", "Idaho", "Indiana", "Kentucky", "Massachusetts", "Michigan", "New Mexico", "Vermont", "West Virginia", "Wisconsin", "Alaska", "Kansas", "Maryland", "Minnesota", "Montana", "New Hampshire", "North Carolina", "Rhode Island", "Tennessee", "Virginia", "Wyoming", "Alabama", "Arizona", "District of Columbia","Florida", "Georgia", "Maine", "Mississippi", "Missouri", "Nevada", "Pennsylvania", "South Carolina", "Texas","Utah", "Iowa", "Arkansas", "Nebraska", "South Dakota", "North Dakota", "Oklahoma")

statespopulation <- c(39512223, 3565287, 12671821, 4648794, 8882190, 19453561, 11689100, 4217737, 7614893, 5758736, 973764, 1415872, 1787065, 6732219, 4467673, 6949503, 9986857, 2096829, 623989, 1792147, 5822434, 731545, 2913314, 6045680, 5639632, 1068778, 1359711, 10488084, 1059361, 6833174, 8535519, 578759, 4903185, 7278717, 705749, 21477737, 10617423, 1344212, 2976149, 6137428, 3080156, 12801989, 5148714, 28995881, 3205958, 3155070, 3017825, 1934408, 884659, 762062, 3956971)

# ----- Top Cases and Death Percentages as of 04/04/2020 -----

corona_040420 <- head(corona, 1829)

result <- c()
for(i in statelist) {
  statedata <- subset(corona_040420, corona_040420$state == i)
  statecases <- statedata$cases
  result[i] <- tail(statecases, 1)
}

statescases_040420 <- result

result2 <- c()
for(i in statelist) {
  statedata <- subset(corona_040420, corona_040420$state == i)
  statedeaths <- statedata$deaths
  result2[i] <- tail(statedeaths, 1)
}

statesdeaths_040420 <- result2

topstatepercentages_040420 <- data.frame(statelist, statespopulation, statescases_040420, statesdeaths_040420)

topstatepercentages_040420$casepercentages_040420 <- topstatepercentages_040420$statescases_040420/topstatepercentages_040420$statespopulation
topstatepercentages_040420$deathpercentages_040420 <- topstatepercentages_040420$statesdeaths_040420/topstatepercentages_040420$statespopulation

topcasespercentages_040420 <- tail(sort(topstatepercentages_040420$casepercentages_040420), 10)
topdeathspercentages_040420 <- tail(sort(topstatepercentages_040420$deathpercentages_040420), 10)

top10statecasepercentages_040420 <- c("Colorado", "Illinois", "Washington", "District of Columbia", "Michigan", "Connecticut", "Massachusetts", "Louisiana", "New Jersey", "New York")

top10statedeathpercentages_040420 <- c("Colorado", "District of Columbia", "Massachusetts", "Vermont", "Washington", "Connecticut", "Michigan", "Louisiana", "New Jersey", "New York")

top10statepercentages_040420 <- data.frame(topcasespercentages_040420, top10statecasepercentages_040420, topdeathspercentages_040420, top10statedeathpercentages_040420)

bxp_topstatecases_percentages_040420 <- ggplot(top10statepercentages_040420, aes(x = top10statecasepercentages_040420, y = topcasespercentages_040420)) + 
  geom_bar(stat="identity", fill="dodgerblue") +
  labs(x='State',y='Proportion of Cases') +
  scale_y_continuous(labels = comma, limits = c(0, 0.015)) +
  ggtitle("Proportion of Cases as of 04/04/20") +
  theme(text = element_text(size=3.5),
        axis.text.x = element_text(angle=90, hjust=1))

bxp_topstatedeaths_percentages_040420 <- ggplot(top10statepercentages_040420, aes(x = top10statedeathpercentages_040420, y = topdeathspercentages_040420)) + 
  geom_bar(stat="identity", fill="dodgerblue") +
  labs(x='State',y='Proportion of Deaths') +
  scale_y_continuous(labels = comma, limits = c(0, 0.0009)) +  
  ggtitle("Proportion of Deaths as of 04/04/20") +
  theme(text = element_text(size=3.5),
        axis.text.x = element_text(angle=90, hjust=1))


# ----- Top Cases and Death Percentages as of 04/11/2020 -----

corona_041120 <- head(corona, 2217)

result <- c()
for(i in statelist) {
  statedata <- subset(corona_041120, corona_041120$state == i)
  statecases <- statedata$cases
  result[i] <- tail(statecases, 1)
}

statescases_041120 <- result

result2 <- c()
for(i in statelist) {
  statedata <- subset(corona_041120, corona_041120$state == i)
  statedeaths <- statedata$deaths
  result2[i] <- tail(statedeaths, 1)
}

statesdeaths_041120 <- result2

topstatepercentages_041120 <- data.frame(statelist, statespopulation, statescases_041120, statesdeaths_041120)

topstatepercentages_041120$casepercentages_041120 <- topstatepercentages_041120$statescases_041120/topstatepercentages_041120$statespopulation
topstatepercentages_041120$deathpercentages_041120 <- topstatepercentages_041120$statesdeaths_041120/topstatepercentages_041120$statespopulation

topcasespercentages_041120 <- tail(sort(topstatepercentages_041120$casepercentages_041120), 10)
topdeathspercentages_041120 <- tail(sort(topstatepercentages_041120$deathpercentages_041120), 10)

top10statecasepercentages_041120 <- c("Delaware", "Pennsylvania","Rhode Island", "Michigan", "District of Columbia", "Connecticut",  "Massachusetts", "Louisiana", "New Jersey", "New York")

top10statedeathpercentages_041120 <- c("Rhode Island", "Illinois", "Washington", "District of Columbia", "Massachusetts", "Connecticut", "Michigan", "Louisiana", "New Jersey", "New York")

top10statepercentages_041120 <- data.frame(topcasespercentages_041120, top10statecasepercentages_041120, topdeathspercentages_041120, top10statedeathpercentages_041120)

bxp_topstatecases_percentages_041120 <- ggplot(top10statepercentages_041120, aes(x = top10statecasepercentages_041120, y = topcasespercentages_041120)) + 
  geom_bar(stat="identity", fill="skyblue") +
  labs(x='State',y='Proportion of Cases') +
  scale_y_continuous(labels = comma, limits = c(0, 0.015)) +
  ggtitle("Proportion of Cases as of 04/11/20") +
  theme(text = element_text(size=3.5),
        axis.text.x = element_text(angle=90, hjust=1))

bxp_topstatedeaths_percentages_041120 <- ggplot(top10statepercentages_041120, aes(x = top10statedeathpercentages_041120, y = topdeathspercentages_041120)) + 
  geom_bar(stat="identity", fill="skyblue") +
  labs(x='State',y='Proportion of Deaths') +
  scale_y_continuous(labels = comma, limits = c(0, 0.0009)) +  
  ggtitle("Proportion of Deaths as of 04/11/20") +
  theme(text = element_text(size=3.5),
        axis.text.x = element_text(angle=90, hjust=1))

# ----- Top Cases and Death Percentages as of 04/18/2020 -----

corona_041820 <- head(corona, 2609)

result <- c()
for(i in statelist) {
  statedata <- subset(corona_041820, corona_041820$state == i)
  statecases <- statedata$cases
  result[i] <- tail(statecases, 1)
}

statescases_041820 <- result

result2 <- c()
for(i in statelist) {
  statedata <- subset(corona_041820, corona_041820$state == i)
  statedeaths <- statedata$deaths
  result2[i] <- tail(statedeaths, 1)
}

statesdeaths_041820 <- result2

topstatepercentages_041820 <- data.frame(statelist, statespopulation, statescases_041820, statesdeaths_041820)

topstatepercentages_041820$casepercentages_041820 <- topstatepercentages_041820$statescases_041820/topstatepercentages_041820$statespopulation
topstatepercentages_041820$deathpercentages_041820 <- topstatepercentages_041820$statesdeaths_041820/topstatepercentages_041820$statespopulation

topcasespercentages_041820 <- tail(sort(topstatepercentages_041820$casepercentages_041820), 10)
topdeathspercentages_041820 <- tail(sort(topstatepercentages_041820$deathpercentages_041820), 10)

top10statecasepercentages_041820 <- c("Pennsylvania","Delaware", "Michigan", "District of Columbia", "Rhode Island", "Connecticut", "Louisiana", "Massachusetts", "New Jersey", "New York")

top10statedeathpercentages_041820 <- c("Pennsylvania", "Illinois", "District of Columbia", "Rhode Island", "Michigan", "Massachusetts", "Louisiana", "Connecticut", "New Jersey", "New York")


top10statepercentages_041820 <- data.frame(topcasespercentages_041820, top10statecasepercentages_041820, topdeathspercentages_041820, top10statedeathpercentages_041820)

bxp_topstatecases_percentages_041820 <- ggplot(top10statepercentages_041820, aes(x = top10statecasepercentages_041820, y = topcasespercentages_041820)) + 
  geom_bar(stat="identity", fill="steelblue") +
  labs(x='State',y='Proportion of Cases') +
  scale_y_continuous(labels = comma, limits = c(0, 0.015)) +  
  ggtitle("Proportion of Cases as of 04/18/20") +
  theme(text = element_text(size=3.5),
        axis.text.x = element_text(angle=90, hjust=1))

bxp_topstatedeaths_percentages_041820 <- ggplot(top10statepercentages_041820, aes(x = top10statedeathpercentages_041820, y = topdeathspercentages_041820)) + 
  geom_bar(stat="identity", fill="steelblue") +
  labs(x='State',y='Proportion of Deaths') +
  scale_y_continuous(labels = comma, limits = c(0, 0.0009)) +   
  ggtitle("Proportion of Deaths as of 04/18/20") +
  theme(text = element_text(size=3.5),
        axis.text.x = element_text(angle=90, hjust=1))

# ----- Top Cases and Death Percentages as of 04/25/2020 -----

corona_042520 <- head(corona, 2984)

result <- c()
for(i in statelist) {
  statedata <- subset(corona_042520, corona_042520$state == i)
  statecases <- statedata$cases
  result[i] <- tail(statecases, 1)
}

statescases_042520 <- result

result2 <- c()
for(i in statelist) {
  statedata <- subset(corona_042520, corona_042520$state == i)
  statedeaths <- statedata$deaths
  result2[i] <- tail(statedeaths, 1)
}

statesdeaths_042520 <- result2

topstatepercentages_042520 <- data.frame(statelist, statespopulation, statescases_042520, statesdeaths_042520)

topstatepercentages_042520$casepercentages_042520 <- topstatepercentages_042520$statescases_042520/topstatepercentages_042520$statespopulation
topstatepercentages_042520$deathpercentages_042520 <- topstatepercentages_042520$statesdeaths_042520/topstatepercentages_042520$statespopulation

topcasespercentages_042520 <- tail(sort(topstatepercentages_042520$casepercentages_042520), 10)
topdeathspercentages_042520 <- tail(sort(topstatepercentages_042520$deathpercentages_042520), 10)

top10statecasepercentages_042520 <- topstatepercentages_042520[topstatepercentages_042520$casepercentages_042520 %in% topcasespercentages_042520,]

top10statedeathpercentages_042520 <- topstatepercentages_042520[topstatepercentages_042520$deathpercentages_042520 %in% topdeathspercentages_042520,]


bxp_topstatecases_percentages_042520 <- ggplot(top10statecasepercentages_042520, aes(x = top10statecasepercentages_042520$statelist, y = top10statecasepercentages_042520$casepercentages_042520)) + 
  geom_bar(stat="identity", fill="deepskyblue") +
  labs(x='State',y='Proportion of Cases') +
  scale_y_continuous(labels = comma, limits = c(0, 0.015)) + 
  ggtitle("Proportion of Cases as of 04/25/20") +
  theme(text = element_text(size=3.5),
        axis.text.x = element_text(angle=90, hjust=1))

bxp_topstatedeaths_percentages_042520 <- ggplot(top10statedeathpercentages_042520, aes(x = top10statedeathpercentages_042520$statelist, y = top10statedeathpercentages_042520$deathpercentages_042520)) + 
  geom_bar(stat="identity", fill="deepskyblue") +
  labs(x='State',y='Proportion of Deaths') +
  scale_y_continuous(labels = comma, limits = c(0, 0.0009)) + 
  ggtitle("Proportion of Deaths as of 04/25/20") +
  theme(text = element_text(size=3.5),
        axis.text.x = element_text(angle=90, hjust=1))



ggarrange(bxp_topstatecases_percentages_040420, bxp_topstatecases_percentages_041120, bxp_topstatecases_percentages_041820, bxp_topstatecases_percentages_042520, ncol = 2, nrow = 2)

ggarrange(bxp_topstatedeaths_percentages_040420, bxp_topstatedeaths_percentages_041120, bxp_topstatedeaths_percentages_041820, bxp_topstatedeaths_percentages_042520, ncol = 2, nrow = 2)



# ----- HIGHEST STATE DEATH RATES -----

# Death rate is calculated as the number of deaths divided by the number of cases in each state

# ----- State's Death Rates as of 04/04/20 -----

corona_040420 <- head(corona, 1829)

result <- c()
for(i in statelist) {
  statedata <- subset(corona_040420, corona_040420$state == i)
  statecases <- statedata$cases
  result[i] <- tail(statecases, 1)
}

statescases_040420 <- result

result2 <- c()
for(i in statelist) {
  statedata <- subset(corona_040420, corona_040420$state == i)
  statedeaths <- statedata$deaths
  result2[i] <- tail(statedeaths, 1)
}

statesdeaths_040420 <- result2

deathrate_040420 <- data.frame(statelist, statescases_040420, statesdeaths_040420)

deathrate_040420$state_deathrate_040420 <- deathrate_040420$statesdeaths_040420/deathrate_040420$statescases_040420

top10_highest_deathrates_040420 <- tail(sort(deathrate_040420$state_deathrate_040420), 10)

top10statedeathrates_040420 <- deathrate_040420[deathrate_040420$state_deathrate_040420 %in% top10_highest_deathrates_040420,]

bxp_topstatedeathrates_040420 <- ggplot(top10statedeathrates_040420, aes(x = top10statedeathrates_040420$statelist, y = top10statedeathrates_040420$state_deathrate_040420)) + 
  geom_bar(stat="identity", fill="dodgerblue") +
  labs(x='State',y='Death Rate (as decimal)') +
  ylim(0, 0.09) +   
  ggtitle("Death Rates as of 04/04/20") +
  theme(text = element_text(size=5),
        axis.text.x = element_text(angle=90, hjust=1))


# ----- State's Death Rates as of 04/11/20 -----

corona_041120 <- head(corona, 2217)

result <- c()
for(i in statelist) {
  statedata <- subset(corona_041120, corona_041120$state == i)
  statecases <- statedata$cases
  result[i] <- tail(statecases, 1)
}

statescases_041120 <- result

result2 <- c()
for(i in statelist) {
  statedata <- subset(corona_041120, corona_041120$state == i)
  statedeaths <- statedata$deaths
  result2[i] <- tail(statedeaths, 1)
}

statesdeaths_041120 <- result2

deathrate_041120 <- data.frame(statelist, statescases_041120, statesdeaths_041120)

deathrate_041120$state_deathrate_041120 <- deathrate_041120$statesdeaths_041120/deathrate_041120$statescases_041120

top10_highest_deathrates_041120 <- tail(sort(deathrate_041120$state_deathrate_041120), 10)

top10statedeathrates_041120 <- deathrate_041120[deathrate_041120$state_deathrate_041120 %in% top10_highest_deathrates_041120,]

bxp_topstatedeathrates_041120 <- ggplot(top10statedeathrates_041120, aes(x = top10statedeathrates_041120$statelist, y = top10statedeathrates_041120$state_deathrate_041120)) + 
  geom_bar(stat="identity", fill="skyblue") +
  labs(x='State',y='Death Rate (as decimal)') +
  ylim(0, 0.09) +   
  ggtitle("Death Rates as of 04/11/20") +
  theme(text = element_text(size=5),
        axis.text.x = element_text(angle=90, hjust=1))


# ----- State's Death Rates as of 04/18/20 -----

corona_041820 <- head(corona, 2609)

result <- c()
for(i in statelist) {
  statedata <- subset(corona_041820, corona_041820$state == i)
  statecases <- statedata$cases
  result[i] <- tail(statecases, 1)
}

statescases_041820 <- result

result2 <- c()
for(i in statelist) {
  statedata <- subset(corona_041820, corona_041820$state == i)
  statedeaths <- statedata$deaths
  result2[i] <- tail(statedeaths, 1)
}

statesdeaths_041820 <- result2

deathrate_041820 <- data.frame(statelist, statescases_041820, statesdeaths_041820)

deathrate_041820$state_deathrate_041820 <- deathrate_041820$statesdeaths_041820/deathrate_041820$statescases_041820

top10_highest_deathrates_041820 <- tail(sort(deathrate_041820$state_deathrate_041820), 10)

top10statedeathrates_041820 <- deathrate_041820[deathrate_041820$state_deathrate_041820 %in% top10_highest_deathrates_041820,]

bxp_topstatedeathrates_041820 <- ggplot(top10statedeathrates_041820, aes(x = top10statedeathrates_041820$statelist, y = top10statedeathrates_041820$state_deathrate_041820)) + 
  geom_bar(stat="identity", fill="steelblue") +
  labs(x='State',y='Death Rate (as decimal)') +
  ylim(0, 0.09) +   
  ggtitle("Death Rates as of 04/18/20") +
  theme(text = element_text(size=5),
        axis.text.x = element_text(angle=90, hjust=1))

# ----- State's Death Rates as of 04/25/20 -----

corona_042520 <- head(corona, 2984)

result <- c()
for(i in statelist) {
  statedata <- subset(corona_042520, corona_042520$state == i)
  statecases <- statedata$cases
  result[i] <- tail(statecases, 1)
}

statescases_042520 <- result

result2 <- c()
for(i in statelist) {
  statedata <- subset(corona_042520, corona_042520$state == i)
  statedeaths <- statedata$deaths
  result2[i] <- tail(statedeaths, 1)
}

statesdeaths_042520 <- result2

deathrate_042520 <- data.frame(statelist, statescases_042520, statesdeaths_042520)

deathrate_042520$state_deathrate_042520 <- deathrate_042520$statesdeaths_042520/deathrate_042520$statescases_042520

top10_highest_deathrates_042520 <- tail(sort(deathrate_042520$state_deathrate_042520), 10)

top10statedeathrates_042520 <- deathrate_042520[deathrate_042520$state_deathrate_042520 %in% top10_highest_deathrates_042520,]

bxp_topstatedeathrates_042520 <- ggplot(top10statedeathrates_042520, aes(x = top10statedeathrates_042520$statelist, y = top10statedeathrates_042520$state_deathrate_042520)) + 
  geom_bar(stat="identity", fill="deepskyblue") +
  labs(x='State',y='Death Rate (as decimal)') +
  ylim(0, 0.09) +   
  ggtitle("Death Rates as of 04/25/20") +
  theme(text = element_text(size=5),
        axis.text.x = element_text(angle=90, hjust=1))


ggarrange(bxp_topstatedeathrates_040420, bxp_topstatedeathrates_041120, bxp_topstatedeathrates_041820, bxp_topstatedeathrates_042520, ncol = 2, nrow = 2)



# ----- STATE MODEL FITTING -----


# ----- California COVID-19 Case and Death Growth Rate Model Fitting -----

corona_california <- subset(corona, state == "California")

day <- c(0:(length(corona_california$date)-1))
corona_california$day <- day

attach(corona_california)

# fit logistic growth model for the cases of California
log.ss_california_cases <- nls(corona_california$cases ~ SSlogis(corona_california$day, theta1, theta2, theta3))
phi1 <- summary(log.ss_california_cases)$coef[1]
phi2 <- -summary(log.ss_california_cases)$coef[2]/summary(log.ss_california_cases)$coef[3]
phi3 <- 1/summary(log.ss_california_cases)$coef[3]

predlogistic_california_cases <- predict(log.ss_california_cases, data.frame(day= 0:max(corona_california$day)))

logistic_model_california_cases <- ggplot(corona_california, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = predlogistic_california_cases), size = 1, col = "steelblue") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


# fit exponential growth model for the cases in California
exponential_model_california <- lm(log(corona_california$cases) ~ corona_california$day)
summary(exponential_model_california) 

x_0 <- exp(exponential_model_california$coefficients[1])
b <- exp(exponential_model_california$coefficients[2])

dayvalues <- c(0:(length(date)-1))

exponential_cases_california <- exp(predict(exponential_model_california,list(day=dayvalues)))

exponential_model_california_cases <- ggplot(corona_california, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(x= dayvalues, y = exponential_cases_california), size = 1, col = "steelblue") +
  scale_y_continuous(labels = comma) +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


# fit logistic growth model for the deaths in California 
log.ss_california_deaths <- nls(deaths ~ SSlogis(day, theta1, theta2, theta3))
phi1 <- summary(log.ss_california_deaths)$coef[1]
phi2 <- -summary(log.ss_california_deaths)$coef[2]/summary(log.ss_california_deaths)$coef[3]
phi3 <- 1/summary(log.ss_california_deaths)$coef[3]


predlogistic_california_deaths <- predict(log.ss_california_deaths, data.frame(day= 0:max(corona_california$day)))

logistic_model_california_deaths <- ggplot(corona_california, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = predlogistic_california_deaths), size = 1, col = "steelblue") +
  labs(x='Day',y='Deaths') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


# fit exponential growth model for deaths in California
log_deaths_cal <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.6931472, 1.0986123, 1.3862944, 1.3862944, 1.6094379, 1.6094379, 1.7917595, 2.3978953, 2.6390573, 2.8332133, 2.9444390, 3.1780538, 3.3322045, 3.5553481, 3.6635616, 3.9512437, 4.2046926, 4.4067192, 4.6249728, 4.7957905, 4.8675345, 4.9836066, 5.2094862, 5.3565863, 5.5093883, 5.6419071, 5.7776523, 5.8550719, 5.9558374, 6.1025586, 6.2265367, 6.3062753, 6.3868793, 6.4488894, 6.5161931, 6.5861717, 6.6567265, 6.7855876, 6.8783265, 6.9565454, 7.0440329, 7.0698741, 7.1090621, 7.1823521, 7.2619271, 7.3479438, 7.3895640, 7.4336665, 7.4477513, 7.4955419, 7.5411525, 7.5812098)

california_deaths_exponential_model <- lm(log_deaths_cal ~ day)
summary(california_deaths_exponential_model)

dayvalues <- c(0:(length(date)-1))

california_deaths_exponential <- exp(predict(california_deaths_exponential_model,list(day=dayvalues)))

exponential_model_california_deaths <- ggplot(corona_california, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(x= dayvalues, y = california_deaths_exponential), size = 1, col = "steelblue") +
  labs(x='Day',y='Deaths') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


ggarrange(exponential_model_california_cases, exponential_model_california_deaths, nrow = 1, ncol=2)

ggarrange(logistic_model_california_cases, logistic_model_california_deaths, nrow = 1, ncol=2)


# ----- Oklahoma COVID-19 Case and Death Growth Rate Model Fitting -----

corona_oklahoma <- subset(corona, state == "Oklahoma")

day <- c(0:(length(corona_oklahoma$date)-1))
corona_oklahoma$day <- day

attach(corona_oklahoma)

# fit logistic growth model for the cases of Oklahoma
log.ss_oklahoma_cases <- nls(corona_oklahoma$cases ~ SSlogis(corona_oklahoma$day, theta1, theta2, theta3))
phi1 <- summary(log.ss_oklahoma_cases)$coef[1]
phi2 <- -summary(log.ss_oklahoma_cases)$coef[2]/summary(log.ss_oklahoma_cases)$coef[3]
phi3 <- 1/summary(log.ss_oklahoma_cases)$coef[3]

predlogistic_oklahoma_cases <- predict(log.ss_oklahoma_cases, data.frame(day= 0:max(corona_oklahoma$day)))

logistic_model_oklahoma_cases <- ggplot(corona_oklahoma, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = predlogistic_oklahoma_cases), size = 1, col = "steelblue") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


# fit exponential growth model for the cases in Oklahoma
oklahoma_cases_exponential_model <- lm(log(corona_oklahoma$cases) ~ corona_oklahoma$day)
summary(oklahoma_cases_exponential_model) 

x_0 <- exp(oklahoma_cases_exponential_model$coefficients[1])
b <- exp(oklahoma_cases_exponential_model$coefficients[2])

dayvalues <- c(0:(length(date)-1))

oklahoma_cases_exponential <- exp(predict(oklahoma_cases_exponential_model,list(day=dayvalues)))

exponential_model_oklahoma_cases <- ggplot(corona_oklahoma, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(x= dayvalues, y = oklahoma_cases_exponential), size = 1, col = "steelblue") +
  #ylim(0, 900000) +
  labs(x='Day',y='Cases') +
  #ggtitle("Exponential Growth Model of Kentucky as of 04/27/20") +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


# fit logistic growth model for the deaths of Kentucky 
log.ss_oklahoma_deaths <- nls(deaths ~ SSlogis(day, theta1, theta2, theta3))
phi1 <- summary(log.ss_oklahoma_deaths)$coef[1]
phi2 <- -summary(log.ss_oklahoma_deaths)$coef[2]/summary(log.ss_oklahoma_deaths)$coef[3]
phi3 <- 1/summary(log.ss_oklahoma_deaths)$coef[3]

predlogistic_oklahoma_deaths <- predict(log.ss_oklahoma_deaths, data.frame(day= 0:max(corona_oklahoma$day)))

logistic_model_oklahoma_deaths <- ggplot(corona_oklahoma, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = predlogistic_oklahoma_deaths), size = 1, col = "steelblue") +
  labs(x='Day',y='Deaths') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))

# fit exponential growth model for the cases in Kentucky
log_deaths_oklahoma <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0.0000000, 0.0000000, 0.6931472, 0.6931472, 1.0986123, 1.6094379, 1.9459101, 2.0794415, 2.7080502, 2.7725887, 2.8332133, 3.1354942, 3.4011974, 3.5263605, 3.6375862, 3.7376696, 3.8286414, 3.9318256, 4.2046926, 4.3694479, 4.3820266, 4.4773368, 4.5325995, 4.5643482, 4.5849675, 4.6913479, 4.8121844, 4.8751973, 4.9126549, 4.9344739, 4.9416424, 4.9628446, 5.0998664, 5.1298987, 5.1873858, 5.2364420, 5.2678582, 5.2729996, 5.2832037, 5.3327188, 5.3659760)

oklahoma_deaths_exponential_model <- lm(log_deaths_oklahoma ~ day)
summary(oklahoma_deaths_exponential_model) 

x_0 <- exp(oklahoma_deaths_exponential_model$coefficients[1])
b <- exp(oklahoma_deaths_exponential_model$coefficients[2])

dayvalues <- c(0:(length(day)-1))

oklahoma_deaths_exponential <- exp(predict(oklahoma_deaths_exponential_model,list(day=dayvalues)))

exponential_model_oklahoma_deaths <- ggplot(corona_oklahoma, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(x= dayvalues, y = oklahoma_deaths_exponential), size = 1, col = "steelblue") +
  #ylim(0, 55000) +
  labs(x='Day',y='Deaths') +
  #ggtitle("Exponential Growth Model of US Deaths") +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))

ggarrange(exponential_model_oklahoma_cases, exponential_model_oklahoma_deaths, nrow = 1, ncol=2)

ggarrange(logistic_model_oklahoma_cases, logistic_model_oklahoma_deaths, nrow = 1, ncol=2)


# ----- New York COVID-19 Case and Death Growth Rate Model Fitting -----

corona_ny <- subset(corona, state == "New York")
day <- c(0:(length(corona_ny$date)-1))
corona_ny$day <- day

attach(corona_ny)

# fit exponential growth model for the cases in New York
exponential_model_ny <- lm(log(corona_ny$cases) ~ corona_ny$day)
summary(exponential_model_ny) 

x_0 <- exp(exponential_model_ny$coefficients[1])
b <- exp(exponential_model_ny$coefficients[2])

dayvalues <- c(0:(length(date)-1))

predicted_exponential_cases_ny <- exp(predict(exponential_model_ny,list(day=dayvalues)))

exponential_model_ny_cases <- ggplot(corona_ny, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(x= dayvalues, y = predicted_exponential_cases_ny), size = 1, col = "steelblue") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


# fit logistic growth model for the cases in New York
log.ss_ny_cases <- nls(corona_ny$cases ~ SSlogis(corona_ny$day, theta1, theta2, theta3))
phi1 <- summary(log.ss_ny_cases)$coef[1]
phi2 <- -summary(log.ss_ny_cases)$coef[2]/summary(log.ss_ny_cases)$coef[3]
phi3 <- 1/summary(log.ss_ny_cases)$coef[3]

predlogistic_ny_cases <- predict(log.ss_ny_cases, data.frame(day= 0:max(corona_ny$day)))

logistic_model_ny_cases <- ggplot(corona_ny, aes(x = day, y = cases)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = predlogistic_ny_cases), size = 1, col = "steelblue") +
  labs(x='Day',y='Cases') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


# fit logistic growth model for the deaths of New York
log.ss_ny_deaths <- nls(deaths ~ SSlogis(day, theta1, theta2, theta3))
phi1 <- summary(log.ss_ny_deaths)$coef[1]
phi2 <- -summary(log.ss_ny_deaths)$coef[2]/summary(log.ss_ny_deaths)$coef[3]
phi3 <- 1/summary(log.ss_ny_deaths)$coef[3]

predlogistic_ny_deaths <- predict(log.ss_ny_deaths, data.frame(day= 0:max(corona_ny$day)))

logistic_model_ny_deaths <- ggplot(corona_ny, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = predlogistic_ny_deaths), size = 1, col = "steelblue") +
  labs(x='Day',y='Deaths') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))

# fit exponential model for the cases in New York
log_deaths_ny <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.6931472, 1.7917595, 2.3025851, 2.8332133, 3.2958369, 3.4011974, 4.0430513,4.3820266, 4.8040210, 5.0689042, 5.3844951, 5.7838252, 6.0684256, 6.2822667, 6.6618547, 6.8721281, 7.1098795, 7.3460102, 7.5709586, 7.7719103, 7.9844627, 8.1797605, 8.3335107, 8.4675827, 8.6238928, 8.7432126, 8.8631913, 8.9675042, 9.0626521, 9.1468679, 9.2159248, 9.2904446, 9.3575528, 9.4085353, 9.4589177, 9.5001701, 9.5374114, 9.5712961, 9.6042726, 9.6357388, 9.6639605, 9.6904181, 9.7170977, 9.7389666, 9.7586352, 9.7778109, 9.7989600)

ny_deaths_exponential_model <- lm(log_deaths_ny ~ day)
summary(ny_deaths_exponential_model) 

x_0 <- exp(ny_deaths_exponential_model$coefficients[1])
b <- exp(ny_deaths_exponential_model$coefficients[2])

dayvalues <- c(0:(length(day)-1))

ny_deaths_exponential <- exp(predict(ny_deaths_exponential_model,list(day=dayvalues)))

exponential_model_ny_deaths <- ggplot(corona_ny, aes(x = day, y = deaths)) +
  geom_point(size = 0.5) +
  geom_line(aes(x= dayvalues, y = ny_deaths_exponential), size = 1, col = "steelblue") +
  labs(x='Day',y='Deaths') +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=0, hjust=1))


ggarrange(exponential_model_ny_cases, exponential_model_ny_deaths, nrow = 1, ncol=2)

ggarrange(logistic_model_ny_cases, logistic_model_ny_deaths, nrow = 1, ncol=2)


