lib<-c('tidyverse','WDI','kableExtra','tidymodels','rlang','magrittr',
       'readxl','dplyr','purrr','knitr','survival','survminer')
lapply(lib, library, character.only = TRUE)


df <- data.frame(
y=replicate(10, rpois(12, lambda = 3),simplify = F) %>% map_df(~bind_cols(.x)),
x=replicate(10, 1:12,simplify = F) %>% map_df(~bind_cols(.x)),
z=rep(1:10, each = 12)
) %>% `colnames<-`(c('y','x','z'))

ggplot(df,aes(x=x,y=y,col=as.factor(z)))+geom_line()

df %>% 
  group_by(z)%>%
  mutate(csy=cumsum(y),
         csy_pct=cumsum(y)/sum(y)) %>%
  ggplot(aes(x=x,y=csy_pct,col=as.factor(z)))+geom_line()

set.seed(123)  # For reproducibility

# Total number of arrivals in a year
total_arrivals <- 1000

# Define months
months <- 1:12

# Define skewed probability distribution
# Assign higher probabilities to the last 6 months (July to December)
probabilities <- c(0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1, 0.1, 0.15, 0.15)

# Simulate monthly arrivals using the probability distribution
monthly_arrivals <- sample(months, total_arrivals, replace = TRUE, prob = probabilities)

# Summarize the number of arrivals per month
arrivals_per_month <- table(monthly_arrivals)

df <- data.frame(
  y=replicate(10, 
              table(sample(months, total_arrivals, replace = TRUE, prob = probabilities)),
              simplify = F) %>% unlist(),
  x=replicate(10, 1:12,simplify = F) %>% unlist(),
  z=rep(1:10, each = 12),
  div=rep(c('a','b','c','d'))
) %>% `colnames<-`(c('y','x','z','div'))

ggplot(df,aes(x=x,y=y,col=as.factor(z)))+geom_line()

df %>% 
  group_by(z)%>%
  mutate(R=sum(y)-cumsum(y),
         S_pct=1-y/R) %>%
  ggplot(aes(x=x,y=S_pct,col=as.factor(z)))+geom_line()+facet_wrap(.~div)

dat<-df %>% 
  mutate(event=1) %>% 
  select(time=y,event,years=z,div)
  #`colnames<-`(c('time','event'))
km<-survfit(Surv(time, event) ~ 1,
              data = dat)

ggsurvplot(km,
           conf.int = FALSE,
           surv.median.line = "hv",
           legend = "none"
)

summary(km)$table["median"]

km

# results
fit <- survfit(Surv(time, event) ~ 1,
  data = dat,
  conf.type = "log-log"
)

fit
# plot
ggsurvplot(fit,
  surv.median.line = "hv",
  legend = "none"
)

#Using the Ranger package for survival analysis
#Install.packages("ranger")
library(ranger)

#Drop rows with NA values
#pbc_nadrop=pbc[complete.cases(pbc), ]
#Fitting the random forest
ranger_model <- ranger(Surv(time, event) ~.,
                       data=dat,num.trees = 500,
                       importance = "permutation",
                       seed = 1)

#Plot the death times
plot(ranger_model$unique.death.times,ranger_model$survival[1,], type = "l", ylim = c(0,1),)

x<-4
t1=Sys.time()
sqrt(x)
(Sys.time()-t1)*1e3
t1=Sys.time()
x^.5
(Sys.time()-t1)*1e3
t1=Sys.time()
exp(log(x)/2)
(Sys.time()-t1)*1e3


bench::mark(
  sqrt(x),
  x^.5,
  exp(log(x)/2)
)


set.seed(123)  # Set seed for reproducibility

# Step 1: Generate a vector of random numbers
random_numbers <- runif(20, min = 1, max = 100)  # 20 random numbers between 1 and 100

# Step 2: Randomly assign NA values
na_positions <- sample(1:20, size = 5)  # Randomly select 5 positions for NA
random_numbers[na_positions] <- NA

# View the result

int<-function(x1,x2,reps){
  hits<-0
  for(i in 1:reps) {
    u1<- runif(1, min = 0, max = 1)
    u2<- runif(1, min = 0, max = 1)
    if (u1^2>u2) {hits=hits+1}
  }
  return(hits/reps)
}

int(0,1,1000000)




