lib<-c('tidyverse','WDI','kableExtra','tidymodels','DALExtra','rlang','magrittr',
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
  z=rep(1:10, each = 12)
) %>% `colnames<-`(c('y','x','z'))

ggplot(df,aes(x=x,y=y,col=as.factor(z)))+geom_line()

df %>% 
  group_by(z)%>%
  mutate(R=sum(y)-cumsum(y),
         S_pct=1-y/R) %>%
  ggplot(aes(x=x,y=S_pct,col=as.factor(z)))+geom_line()

dat<-df %>% 
  mutate(event=1) %>% 
  select(time=y,event)
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



