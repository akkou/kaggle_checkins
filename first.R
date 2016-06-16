setwd('~/kaggle_checkins/')
library(needs)
needs(dplyr, tidyr, stringr, lubridate, readr, ggplot2,
      MASS,
      pander, formattable, viridis)

list.files("data/") %>%
  as.list %>%
  pander

train <- read_csv("data/train.csv")
test  <- read_csv('data/test.csv') 
glimpse(train)
glimpse(test)



#attach(train)
#train$time %>% range

train <-
  train %>%
  mutate(place_id = as.character(place_id)) %>%
  mutate(time.POSIXct = as.POSIXct(time * 60, origin = '2014-01-01')) %>%
  arrange(time)

myratio <- nrow(test) / nrow(train)


# knn over x, y 
library(class)

test.knn  <- 
  train %>% 
  dplyr::select(row_id, x, y, place_id) %>%
  tail(n = round(nrow(train) * myratio / (1 + myratio)))

train.knn <- 
  train %>% 
  dplyr::select(row_id, x, y, place_id) %>%
  filter(!(row_id %in% test.knn$row_id))
#  slice(sample(1:n(), n()/10))





# slow version
# ptm <- proc.time()
# knn.pred <- 
#   knn(train.knn %>% dplyr::select(x, y), 
#       test.knn %>% dplyr::select(x, y), train.knn$place_id, k = 3)
# proc.time() - ptm

# install.packages('FNN')
library(FNN)


train <- rbind(train.knn, test.knn)

ptm <- proc.time()
knn.pred <- 
  attr(FNN::knn(train %>% dplyr::select(x, y), 
                test  %>% dplyr::select(x, y), train$place_id, k = 10,
                prob = T), 'nn.index')
proc.time() - ptm
save.image()
# 50 min for (k = 10 and full data), 27 min (k = 10) 20 min (k = 3)

myf <- 
  function(knnout){
    my.nn.index <- attr(knnout, 'nn.index')
    knn.id <- matrix(train.knn$place_id[my.nn.index], nrow = nrow(my.nn.index))
    
    temp <- 
      apply(knn.id, 1, 
            function(x) names(sort(table(x), decreasing = T))[1:3])
    
    return(t(temp))
  }

temp <- myf(knn.pred)

temp[is.na(temp[,2]),2] <- temp[is.na(temp[,2]),1]
temp[is.na(temp[,3]),3] <- temp[is.na(temp[,3]),2]
save.image()

dim(temp)

###




mean(temp[,1]== test.knn$place_id)
MAP <- function(out, ans){
  # out = temp; ans = test.knn$place_id
  ss = ((out[, 1] == ans)*(1 + 1/2 + 1/3) + 
          (out[, 2] == ans) * (1/2 + 1/3) +
          (out[, 3] == ans) * (1/3) )
  mean(ss)
}
#


ptm <- proc.time()
myout <- myf(knn.pred)
proc.time()- ptm



mean(knn.pred == test.knn$place_id)





train.knn <- 
  
  
  myknn <- 
  
  
  
  
  
  as.POSIXct(train$time * 60, origin = '2015-01-01') %>% range



table.place_id <- table(place_id) %>% sort(decreasing = T)
table.place_id %>% head

# 8772469670 1623394281 1308450003 

med_id <- names(table.place_id)[1:100] %>% as.numeric



temp <- train %>% filter(place_id %in% med_id)
dim(temp)

temp <- 
  temp %>% 
  mutate(min  = minute(time.POSIXct),
         hour = hour(time.POSIXct),
         minofday = min + hour * 60,
         minofweek = time %% (7 * 24 * 60))

#
# temp <- 
#   train %>% 
#   slice(sample(1:n(), n()/100)) %>%
#   mutate(minofweek = time %% (7 * 24 * 60))


temp %>% 
  ggplot(aes(x = minofweek)) + geom_histogram(binwidth = 60, fill ='white', color ='black') +
  geom_vline(xintercept = 24*60 *c(1:6), col = 'red')












train %>%
  sample_frac(.01) %>%
  ggplot(aes(x = accuracy)) +
  geom_density()
```

It looks like we have three peaks. Do we think there are underlying parameters in the simulation at these peaks?

We might also think that there are different measures of `accuracy` at different locations. 

Let's see how even the accuracy is over `x` and `y`.

Since we have two dimensions and quite a few buckets (50 x 50 = 2,500 bins).

```{r}
train %>%
sample_frac(0.01) %>%
ggplot(aes(x, y, z = accuracy)) +
stat_summary_2d(fun = mean, bins = 50) +
scale_fill_viridis()
```

Now, that looks pretty random to me. Doesn't look like there are either (large) hotspots, or a tendency across the whole square.

We can look at it a few other ways (median, max, min, sd, var, etc), but I don't think they add a great deal.

Even if the _general_ distribution is pretty even, are the high-accuracy values (which we think mean _low_ accuracy) anywhere in particular? 

```{r}
train %>%
filter(accuracy > 200) %>%
# Since we're dealing with the 