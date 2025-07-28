library(tidyverse)
a = read.csv("KBA.csv")

plot(log(a$abund)~log(a$ngrids))

library(ggthemes)

theme_set(theme_tufte())
require(extrafont)
library(cowplot)

ggp = ggplot(a, aes(x=ngrids, y=abund, label = Species)) + 
  #facet_wrap(. ~ region, scale="free", ncol = 4) +
  geom_abline(intercept = 0, slope = 1.17, col = "red", size = 1) + 
  geom_point(size = 2) +
  #geom_smooth(method = "loess") +
  xlab("number of filled 6.6 x 6.6km grid cells (log)") +
  ylab("number of observations (log)")

ggp1 = ggp +
  expand_limits(x =c(0, 2000), y = c(0, 15000)) +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 8)) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  scale_x_continuous(trans = 'log', breaks = c(2,10,50,250)) +
  scale_y_continuous(trans = 'log', breaks = c(2,10,50,250,1250,6250)) +
  geom_text(hjust = 0, check_overlap = TRUE, nudge_x = 0.05) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

ggp = ggplot(a, aes(x=ngrids, y=mean.abund, label = Species)) + 
  #facet_wrap(. ~ region, scale="free", ncol = 4) +
  #geom_abline(intercept = 0, slope = 1.17, col = "red", size = 1) + 
  geom_point(size = 2) +
  #geom_smooth(method = "loess") +
  xlab("number of filled 6.6 x 6.6km grid cells") +
  ylab("mean number of observations per grid cell (abundance)")

ggp2 = ggp +
  #expand_limits(x =c(0, 2000), y = c(0, 15000)) +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 8)) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  scale_x_continuous(breaks = c(2,10,50,250)) +
  #scale_y_continuous(breaks = c(2,10,50,250,1250,6250)) +
  geom_text(hjust = 0, check_overlap = TRUE, nudge_x = 0.05) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )



###################################################


library(tidyverse)
data = read.csv("KBA_temp_file.csv")


## first create a data frame with all the groupings that you want
## this is the dataframe you will populate with mean and CI values
## I am selecting distinct values of Season and Hour to initialize this data frame

df = data %>%
  distinct(Season,Hour)

df$cir = df$mean = df$cil = NA

## now I am starting a loop (repeated as many times as rows in the dataframe) selecting one row at a time 
## and subsetting base data according to that

for (i in 1:length(df$Season))
{
  data_sub = data %>%
    filter(Season == df$Season[i],Hour == df$Hour[i])
  
  ## now the repeated sampling for the bootstrap with the simplest logic
  
  a = numeric(10000) # initializing a vector to store the sample means
  
  for (j in 1:10000)
  {
    a[j] = mean(sample(data_sub$Sp.count, replace = T)) ## you must sample with replacement
  }
  
  ## storing means and CI values of the sample means in the appropriate row in the dataframe, df
  
  df$mean[i] = median(a)
  df$cil[i] = quantile(a,0.025)
  df$cir[i] = quantile(a,0.975)
}

##############################

## to check this for just wet + dry rather than by hour AND 
## to compare this with what you would get by just calculating SEs and CIs assuming normality

df = data %>%
  group_by(Season) %>% summarize(cil.normal = mean(Sp.count)-1.96*sd(Sp.count)/sqrt(n()), 
                                 mean.normal = mean(Sp.count), 
                                 cir.normal = mean(Sp.count)+1.96*sd(Sp.count)/sqrt(n()))

df$cir = df$mean = df$cil = NA

## now I am starting a loop (repeated as many times as rows in the dataframe) selecting one row at a time 
## and subsetting base data according to that

for (i in 1:length(df$Season))
{
  data_sub = data %>%
    filter(Season == df$Season[i])
  
  ## now the repeated sampling for the bootstrap with the simplest logic
  
  a = numeric(10000) # initializing a vector ato store the sample means
  
  for (j in 1:10000)
  {
    a[j] = mean(sample(data_sub$Sp.count, replace = T)) ## you must sample with replacement
  }
  
  ## storing means and CI values of the sample means in the appropriate row in the dataframe, df
  
  df$mean[i] = median(a)
  df$cil[i] = quantile(a,0.025)
  df$cir[i] = quantile(a,0.975)
}


