


setwd("/Users/jnlinao/iCloud/UCI_21-22/Winter'22/BANA_277/Final/")
options(scipen=999)
data <- read.csv('HighNote Data.csv')

library('ggplot2')
library('hrbrthemes')
library('paletteer')
library('dplyr')
library('MatchIt')

head(data)
str(data)

#distribution of target class
table(data$adopter)

#segmenting by target class
adopter.yes_df <- subset(data, adopter == 1)
adopter.no_df <- subset(data, adopter == 0)

#basic descriptive stats per segment
summary(adopter.yes_df)
summary(adopter.no_df)

data %>%
  group_by(adopter) %>%
  summarise_all(funs(mean(., na.rm = T)))

#t-test between the means of a variable for the two groups (adopter 1/0)
t.test_age <- t.test(data$age ~ data$adopter)
t.test_age

t.test_no.friends <- t.test(data$friend_cnt ~ data$adopter) 
t.test_no.friends

t.test_age.friends <- t.test(data$avg_friend_age ~ data$adopter) 
t.test_age.friends

t.test_age.friend.country <- t.test(data$friend_country_cnt ~ data$adopter) 
t.test_age.friend.country

t.test_gender <- t.test(data$male ~ data$adopter) 
t.test_gender

t.test_gender.friends <- t.test(data$avg_friend_male ~ data$adopter) 
t.test_gender.friends

t.test_.sub.friends<- t.test(data$subscriber_friend_cnt ~ data$adopter) 
t.test_.sub.friends

t.test_.sub.songs.list <- t.test(data$songsListened ~ data$adopter) 
t.test_.sub.songs.list

t.test_.loved.trk <- t.test(data$lovedTracks ~ data$adopter) 
t.test_.loved.trk

t.test_.posts <- t.test(data$posts ~ data$adopter) 
t.test_.posts

t.test_.shouts <- t.test(data$shouts ~ data$adopter) 
t.test_.shouts

t.test_.tenure <- t.test(data$tenure ~ data$adopter) 
t.test_.tenure

t.test_.country <- t.test(data$good_country ~ data$adopter) 
t.test_.country



data_viz <- data
data_viz$adopter <- as.factor(data_viz$adopter) #convert target to factor
data_viz$male <- as.factor(data_viz$male) 
data_viz$good_country <- as.factor(data_viz$good_country) 



#Demographics
##Age, 
ggplot(data_viz, aes(age, fill = adopter)) + 
   geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

##Friend Age
ggplot(data_viz, aes(avg_friend_age, fill = adopter)) + 
   geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

##Male
ggplot(as.data.frame(with(data_viz, table(adopter, male))), aes(x=adopter, y = Freq, fill=male)) + 
    geom_bar(stat="identity", position = "dodge")

##Country
ggplot(as.data.frame(with(data_viz, table(adopter, good_country))),
       aes(x=adopter, y = Freq, fill=good_country)) + 
    geom_bar(stat="identity", position = "dodge")


#peer influence
##friend_cnt
ggplot(data=data_viz, aes(x=friend_cnt, group=adopter, fill=adopter)) +
    geom_density(adjust=1.5, alpha=.4) +
    xlim(0,200) +
    theme_ipsum()

##friend_country_cnt
ggplot(data=data_viz, aes(x=friend_country_cnt, group=adopter, fill=adopter)) +
    geom_density(adjust=2.5, alpha=.4) +
    xlim(0,50) +
    theme_ipsum()

##subscriber_friend_cnt
ggplot(data=data_viz, aes(x=subscriber_friend_cnt, group=adopter, fill=adopter)) +
    geom_density(adjust=3, alpha=.4) +
    xlim(0,15) +
    theme_ipsum()

#user-engagement
##songsListened
ggplot(data=data_viz, aes(x=songsListened, group=adopter, fill=adopter)) +
    geom_density(adjust=3, alpha=.4) +
    xlim(0,275000) +
    theme_ipsum()

##posts
ggplot(data=data_viz, aes(x=posts, group=adopter, fill=adopter)) +
    geom_density(adjust=3, alpha=.4) +
    xlim(0,100) +
    theme_ipsum()

##lovedTracks
ggplot(data=data_viz, aes(x=songsListened, group=adopter, fill=adopter)) +
    geom_density(adjust=3, alpha=.4) +
    xlim(0,200000) +
    theme_ipsum()

##tenure
ggplot(data=data_viz, aes(x=tenure, group=adopter, fill=adopter)) +
    geom_density(adjust=3, alpha=.4) +
    xlim(0,150) +
    theme_ipsum()

##playlists
ggplot(data=data_viz, aes(x=playlists, group=adopter, fill=adopter)) +
    geom_density(adjust=10, alpha=.4) +
    xlim(0,10) +
    theme_ipsum()




#Create treatment column 
data2 <- data %>% 
  mutate(trmt = if_else(subscriber_friend_cnt >= 1, 1, 0))

#group by treatment -- see mean for each covariate
num_data <- data2 %>% dplyr::select(where(is.numeric))

num_data %>%
  group_by(trmt) %>%
  summarise_all(funs(mean(., na.rm = T)))

#select observed covariates
covs <- c('age','male','friend_cnt','avg_friend_age','avg_friend_male','friend_country_cnt','songsListened','lovedTracks','posts','playlists','shouts','tenure','good_country')

#Run logit model for propensity score estimation
pse <- glm(trmt ~ age + male + friend_cnt + avg_friend_age + avg_friend_male + 
          friend_country_cnt + songsListened + lovedTracks + posts + playlists + 
            shouts + tenure + good_country, family = binomial(), data = data2)

summary(pse)

#get propensity score for each user, which is the predicted probability of being treated given the observed covariates 
pps_df <- data.frame(pr_score = predict(pse, type = "response"),
                     sub.friends.cnt=pse$model$trmt)
head(pps_df)
head(pse)

#Conduct matched sampling
ps_matching <- data2 %>%  
  select(adopter, trmt, one_of(covs)) %>%
  na.omit() #remove NULL

#takes a bit to run
match <- matchit(trmt ~ age + male + friend_cnt + avg_friend_age + avg_friend_male + 
                   friend_country_cnt + songsListened + lovedTracks + posts + playlists + 
                   shouts + tenure + good_country, method = "nearest", data = ps_matching) 

#see success of matching
summary(match)

#Create dataframe of matched data points
match_df <- match.data(match)
dim(match_df)

#Examine the difference in means for balance among the covariates
match_df %>%
  group_by(trmt) %>%
  select(one_of(covs)) %>%
  summarise_all(funs(mean))

#Examine using t-tests: automated
lapply(covs, function(v) {
  t.test(match_df[, v] ~ match_df$trmt)
})

#Estimating the treatment effects on target variable using t-test (matched data)
with(match_df, t.test(adopter ~ trmt))

#Estimating the treatment effects on target variable using t-test (orginal data)
with(data2, t.test(adopter ~ trmt))

#Estimating treatment effects using logit without covariates
logit_treat1 <- glm(adopter ~ trmt, data = match_df, family = 'binomial')
summary(logit_treat1)
exp(coef(logit_treat1))

#treat categorical as factors
match_df_copy <- match_df
match_df_copy$adopter <- as.factor(match_df_copy$adopter)
match_df_copy$trmt <- as.factor(match_df_copy$trmt)
match_df_copy$male <- as.factor(match_df_copy$male)
match_df_copy$good_country <- as.factor(match_df_copy$good_country)

#Estimating treatment effects using logit with covariates
logit_treat2 <- glm(adopter ~ age + male + friend_cnt + avg_friend_age + avg_friend_male + 
                   friend_country_cnt + songsListened + lovedTracks + posts + playlists + 
                   shouts + tenure + good_country + trmt, 
               data = match_df, family = 'binomial')
summary(logit_treat2)
exp(coef(logit_treat2))

logit1 <- glm(adopter ~ age + male + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt
              + subscriber_friend_cnt + songsListened + lovedTracks + posts + playlists
              + shouts + tenure + good_country, data=data, family = "binomial")
summary(logit1)
exp(coef(logit1))

logit2 <- glm(adopter ~ age + male + friend_cnt + friend_country_cnt
              + subscriber_friend_cnt + songsListened + lovedTracks + playlists + 
                good_country, data=data, family = "binomial")
summary(logit2)
exp(coef(logit2))




