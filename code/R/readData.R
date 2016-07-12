# Load packages ------------------------------------------------------------------------------

library(R.matlab)
library(dplyr)
library(stringr)

rm(list = ls())

# Helper functions  ---------------------------------------------------------------------------

# rescaling function
fun_rescale = function(x,x.min,x.max,y.min,y.max){
  y =  (((x-x.min) * (y.max - y.min)) / (x.max - x.min)) + y.min
  return(y)
}

# Behavioral data -----------------------------------------------------------------------------

path.load = "../../data/raw/mat/"

file.list = list.files(path.load)

np = length(file.list)
ntrials = 20
column.names = c("participant", "name", "age", "sex", "trial", "clip", "outcome", "rating","time")

df.judgments = as.data.frame(matrix(ncol = length(column.names), nrow = 0))
colnames(df.judgments) = column.names

tmp = as.data.frame(matrix(ncol = length(column.names), nrow = ntrials))
colnames(tmp) = column.names

for (i in 1:length(file.list)){
  a = readMat(paste0(path.load,file.list[i]))
  tmp$participant = rep(a$data[,,1]$participant,nrow(tmp))
  tmp$name = rep(a$data[,,1]$name,nrow(tmp))
  tmp$age = rep(a$data[,,1]$age,nrow(tmp))
  tmp$sex = rep(a$data[,,1]$gender,nrow(tmp))
  tmp$trial = as.numeric(a$data[,,1]$trial)
  tmp$clip = as.numeric(a$data[,,1]$movie)
  tmp$outcome = as.numeric(a$data[,,1]$outcome)
  tmp$rating = as.numeric(a$data[,,1]$rating)
  tmp$time = as.numeric((a$t[,,1]$final.screen-a$t[,,1]$instr.1)/60)
  df.judgments = rbind(df.judgments,tmp)
}

# condition labels
df.participants = data.frame(
  participant = as.numeric(str_extract(file.list,"[0-9]+")),
  condition = str_extract(file.list,"causality|observations|counterfactuals")
)

df.judgments = df.judgments %>%
  mutate(participant = as.numeric(participant),
         age = as.numeric(age),
         sex = str_replace_all(sex,"Male","male"),
         sex = as.factor(sex)
         ) %>%
  mutate(rating = fun_rescale(rating,
                              x.min = 212,
                              x.max = 812,
                              y.min = 0,
                              y.max = 100))

df.judgments = merge(df.judgments,df.participants)

# Eye-tracking data (with HMM)  ---------------------------------------------------------------

path.load = "../../data/raw/RData/"

file.list = list.files(path.load)

transition.matrices = list()
load(paste(path.load,file.list[1],sep=""))
transition.matrices[[df.participants$participant]] = transition.matrix 
df.tmp = df.long

for(i in 2:length(file.list)){
  load(paste(path.load,file.list[i],sep=""))
  df.tmp = rbind(df.tmp,df.long)
  transition.matrices[[df.participants$participant]] = transition.matrix
}

df.eyes = df.tmp
df.eyes = merge(df.eyes,select(df.judgments,participant,clip,rating))

df.eyes = df.eyes %>%
  arrange(condition,participant,clip,instance,frame) %>%
  select(condition,participant,clip,instance,frame,everything())

# Data restructuring  -------------------------------------------------------------------------

df.clipinfo = data.frame(
  clip = 1:20,
  t.collision = c(090,118,103,105,096,139,098,098,131,126,091,091,110,070,139,133,152,132,100,090),
  t.outcome.counterfactual = c(197,196,200,203,201,201,196,196,201,197,201,202,196,196,202,197,201,251,197,201),
  t.outcome.actual = c(194,196,196,196,196,182,219,205,188,207,185,212,204,192,204,203,207,221,183,197),
  index = rep(1:2,10),
  outcome.actual = c(rep(c("miss","close","hit"),each=6),"hit","miss"),
  outcome.counterfactual = c(rep(rep(c("miss","close","hit"),each=2),3),"miss","hit"),
  outcome = c(rep(0,7),1,0,1,0,rep(1,7),1,0)) %>% 
  mutate(outcome.actual = factor(outcome.actual,levels = c('miss','close','hit')),
         outcome.counterfactual = factor(outcome.counterfactual,levels = c('miss','close','hit')))

df.judgments = df.judgments %>% 
  select(-name) %>% 
  mutate(condition = factor(condition,levels = c("counterfactuals", "causality", "observations"),
                            labels = c("counterfactual","causal","outcome"))) %>% 
  merge(df.clipinfo %>% select(clip,outcome.actual,outcome.counterfactual,index)) %>% 
  select(condition,participant,sex,age,time,trial,clip,index,outcome,outcome.actual,outcome.counterfactual,rating) %>% 
  arrange(condition,participant,clip)

df.eyes = df.eyes %>% 
  mutate(condition = factor(condition,levels = c("counterfactuals", "causality", "observations"),
                            labels = c("counterfactual","causal","outcome")),
         s = factor(s, levels = c(0,1), labels = c("other","fixation")),
         sindex = factor(sindex, levels = c(1,-1,2,0), labels = c("sacc.start", "sacc.end",
                                                                  "sacc.start_end", "other"))) %>% 
  select(condition:sindex,both.ballA.x:B.ballB.y,contains("post"),v.max,rating) %>% 
  rename(fixation = s,
         saccade = sindex,
         onlyA.x = A.ballA.x,
         onlyA.y = A.ballA.y,
         onlyB.x = B.ballB.x,
         onlyB.y = B.ballB.y,
         both.A.x = both.ballA.x,
         both.A.y = both.ballA.y,
         both.B.x = both.ballB.x,
         both.B.y = both.ballB.y,
         post.other = post.other.look,
         viterbi = v.max) %>% 
  merge(df.clipinfo) %>% 
  select(condition,participant,clip,everything())
  
# save data 
save(list = c("df.eyes","df.judgments"),file = "../../data/summary/trackingDataFrames.RData")
