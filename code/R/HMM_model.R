# Info ----------------------------------------------------------------------------------------

# Classifies fixations: 
# 1) Likelihood of each fixation 
# 2) Posterior over possible states (using HMM)
# 3) Series of most likely states (VITERBI algorithm)

# Paths ------------------------------------------------------------------------------------
rm(list=ls())

#local paths 
path.load = "../../data/raw/mat/"
path.save = "../../data/raw/RData/"

# Packages ------------------------------------------------------------------------------------
library(R.matlab)
library(stringr)
library(dplyr)
library(tidyr)

# ------------------- -------------------------------------------------------------------------

# Read in data file  --------------------------------------------------------------------------

file.list = list.files(path.load)

# extract the participant numbers 
participant.indices = as.numeric(str_extract(file.list,"[0-9]+"))

# only select one participant 
participant.index = 1

file.list = file.list[which(participant.indices == participant.index)]

np = length(file.list)
ntrials = 20

#eye-tracking data 

nframes = 300

columns = c("participant", "clip", "instance", "frame","x", "y", "t", "s")
df.long = as.data.frame(matrix(ncol = length(columns), nrow = 0))
colnames(df.long) = columns

b = as.data.frame(matrix(ncol = length(columns), nrow = nframes))
colnames(b) = columns

for (i in 1:length(file.list)){
  a = readMat(paste(path.load,file.list[i],sep=""))
  counter = 1
  for (instance in 1:2){
    for (clip in 1:ntrials){
      b$participant =  as.numeric(a$data[,,1]$participant)
      b$clip = clip 
      b$instance = instance
      b$frame = 1:nframes
      b$x = a[["a"]][[counter]][[1]][1:nframes,1]
      b$y = 768-a[["a"]][[counter]][[1]][1:nframes,2]
      b$t = a[["a"]][[counter]][[1]][1:nframes,3]
      b$s = a[["a"]][[counter]][[1]][1:nframes,4]
      b$sindex = a[["a"]][[counter]][[1]][1:nframes,5]
      df.long = rbind(df.long,b)
      counter = counter + 1  
    }
  }
}


# add condition label
df.participants = data.frame(
  participant = as.numeric(str_extract(file.list,"[0-9]+")),
  condition = str_extract(file.list,"causality|observations|counterfactuals")
)

df.long = merge(df.long,df.participants)

# add collision time
df.tmp = data.frame(clip = 1:20,
                    collision.time = c(090,118,103,105,096,139,098,098,131,126,091,091,110,070,139,133,152,132,100,090))

df.long = df.long %>% 
  merge(df.tmp)

df.long = df.long %>% 
  mutate(x = ifelse(x > 0 & x<1024, x, NA),
         y = ifelse(y > 0 & y<768, y, NA),
         x = ifelse(is.na(y),NA,x),
         y = ifelse(is.na(x),NA,y))

#remove variables
rm(list=setdiff(ls(), c("df.long","df.participants","np","ntrials","path.save")))

# Ball positions  ----------------------------------------------------------------

# function that reads in the data file much more quickly 
my.read.lines=function(fname) {
  s = file.info( fname )$size 
  buf = readChar( fname, s, useBytes=T)
  strsplit( buf,",",fixed=T,useBytes=T)[[1]]
}

df.ball_positions.wide = my.read.lines("ballPositions.txt")
df.ball_positions.long = as.data.frame(matrix(df.ball_positions.wide,ncol = 7,byrow=T),stringsAsFactors = F)

df.ball_positions.long = df.ball_positions.long %>% 
  mutate_each(funs(as.numeric(.))) %>% 
  select(condition = V1,
         clip = V2,
         frame = V3,
         ballA.x = V4,
         ballA.y = V5,
         ballB.x = V6,
         ballB.y = V7) %>% 
  mutate_each(funs(unlist(.))) %>% 
  mutate_each(funs(.+24),c(ballA.x,ballB.x)) %>% 
  mutate_each(funs(768-(.+18)),c(ballA.y,ballB.y)) %>% 
  mutate(frame = frame + 1) %>% 
  mutate(condition = factor(condition,levels=1:3,labels = c("both","B","A"))) 
# both = both balls are present, B = only ball B is present, A = only ball A is present 

tmp1 = df.ball_positions.long %>%
  filter(condition == "both") %>% 
  select(-condition) %>% 
  rename(both.ballA.x = ballA.x,
         both.ballA.y = ballA.y,
         both.ballB.x = ballB.x,
         both.ballB.y = ballB.y
  )

tmp2 = df.ball_positions.long %>%
  filter(condition == "A") %>% 
  select(-c(condition,clip,frame,ballB.x,ballB.y)) %>% 
  rename(A.ballA.x = ballA.x,
         A.ballA.y = ballA.y
  )

tmp3 = df.ball_positions.long %>%
  filter(condition == "B") %>% 
  select(-c(condition,clip,frame,ballA.x,ballA.y)) %>% 
  rename(B.ballB.x = ballB.x,
         B.ballB.y = ballB.y
  )

df.ball_positions = cbind(tmp1,tmp2,tmp3)

df.long = merge(df.long,df.ball_positions) %>% 
  arrange(participant,clip,instance,frame)

#remove variables
rm(list=setdiff(ls(), c("df.long","df.participants","df.ball_positions","path.save")))

# Likelihood of each fixation  ----------------------------------------------------------------

states = c("A.look","B.look","A.predict","B.predict","A.counterfactual","B.counterfactual","other.look")

# FREE PARAMETERS 
mu = 0 # mean of the Gaussiam
sigma = 30 # standard deviation of the Gaussian 
rate.prediction = 0.1 #exponential decay for prediction
rate.counterfactual =  0.05 #exponential decay for counterfactual 
step.ahead = 20 #steps from the present at which prediction/counterfactual starts 

# LIKELIHOOD FUNCTIONS
fun.likelihood.prediction = function(point,trajectory,frame,sigma,index){
  tmp = as.data.frame(trajectory) %>% 
    rowwise() %>% 
    mutate(distance = as.numeric(dist(rbind(point,c(V1,V2)))), #euclidian distance 
           probability = dnorm(distance,mean=0,sd=sigma)) #gaussian probability
  if (index == 'look'){
    out = sum(tmp$probability)/nrow(tmp) #uniform distribution  
  }else if (index == 'prediction'){
    exponential = dexp(1:(nrow(tmp)),rate=rate.prediction) #exponential distribution (1/rate = mean)
    out = sum(tmp$probability*exponential)
  }else if (index == 'counterfactual'){
    exponential = dexp(1:(nrow(tmp)),rate=rate.counterfactual) #exponential distribution
    out = sum(tmp$probability*exponential)
  }
  return(out)
}

# UNIFORM DISTRIBUTION OVER RECTANGLE
fun.likelihood.uniform = function(point,rectangle){
  out = dunif(point[1],min=rectangle[1],max=rectangle[2])*
    dunif(point[2],min=rectangle[3],max=rectangle[4])
  return(out)
}

# screen 
x.min = 0 
x.max = 1024
y.min = 0
y.max = 768
screen = c(x.min,x.max,y.min,y.max)

#uniform distributions 
like.other = fun.likelihood.uniform(point = c(1,1), rectangle = screen)

df.long = df.long %>% 
  rowwise %>% 
  mutate(
    A.look = fun.likelihood.prediction(point = c(x,y), 
                                       trajectory = cbind(both.ballA.x,both.ballA.y),
                                       frame = frame,
                                       sigma = sigma,
                                       index = "look"),
    B.look = fun.likelihood.prediction(point = c(x,y), 
                                       trajectory = cbind(both.ballB.x,both.ballB.y),
                                       frame = frame,
                                       sigma = sigma,
                                       index = "look"),
    A.predict = fun.likelihood.prediction(point = c(x,y), 
                                          trajectory = cbind(
                                            df.ball_positions$both.ballA.x[df.ball_positions$clip == clip &
                                                                             df.ball_positions$frame > frame+step.ahead],
                                            df.ball_positions$both.ballA.y[df.ball_positions$clip == clip &
                                                                             df.ball_positions$frame > frame+step.ahead]),
                                          frame = frame,
                                          sigma = sigma,
                                          index = "prediction"),
    B.predict = fun.likelihood.prediction(point = c(x,y), 
                                          trajectory = cbind(
                                            df.ball_positions$both.ballB.x[df.ball_positions$clip == clip &
                                                                             df.ball_positions$frame > frame+step.ahead],
                                            df.ball_positions$both.ballB.y[df.ball_positions$clip == clip &
                                                                             df.ball_positions$frame > frame+step.ahead]),
                                          frame = frame,
                                          sigma = sigma,
                                          index = "prediction"),
    A.counterfactual = fun.likelihood.prediction(point = c(x,y), 
                                                 trajectory = cbind(
                                                   df.ball_positions$A.ballA.x[df.ball_positions$clip == clip &
                                                                                 (df.ball_positions$frame > (collision.time+step.ahead) & 
                                                                                    df.ball_positions$frame > frame+step.ahead)],
                                                   df.ball_positions$A.ballA.y[df.ball_positions$clip == clip &
                                                                                 (df.ball_positions$frame > (collision.time+step.ahead) & 
                                                                                    df.ball_positions$frame > frame+step.ahead)]),
                                                 frame = frame,
                                                 sigma = sigma,
                                                 index = "counterfactual"),
    B.counterfactual = fun.likelihood.prediction(point = c(x,y), 
                                                 trajectory = cbind(
                                                   df.ball_positions$B.ballB.x[df.ball_positions$clip == clip &
                                                                                 (df.ball_positions$frame > (collision.time+step.ahead) & 
                                                                                    df.ball_positions$frame > frame+step.ahead)],
                                                   df.ball_positions$B.ballB.y[df.ball_positions$clip == clip &
                                                                                 (df.ball_positions$frame > (collision.time+step.ahead) & 
                                                                                    df.ball_positions$frame > frame+step.ahead)]),
                                                 frame = frame,
                                                 sigma = sigma,
                                                 index = "counterfactual"))%>% 
  ungroup %>% 
  mutate_each(funs(ifelse(both.ballA.x > 1024,0,.)),A.look, A.predict, A.counterfactual) %>%  
  mutate_each(funs(ifelse(both.ballB.x > 1024,0,.)),B.look, B.predict, B.counterfactual)

df.long$other.look = like.other
df.long$other.look[is.na(df.long$A.look)] = NA
df.long$l.max = NA 
df.long$l.max = states[as.numeric(apply(select(df.long,one_of(states)),1,which.max))]

rm(list=setdiff(ls(), c("df.long","df.participants","path.save")))

# HMM with EM ---------------------------------------------------------------------------------

# free parameters: 
em.steps = 5 #seems like a reasonable step (justification)? 

states = c("A.look","B.look","A.predict","B.predict","A.counterfactual","B.counterfactual","other.look")

# set up the model 
likelihoods = df.long %>% 
  na.omit() %>% 
  select(one_of(states))

nstates = length(states)
nsamples = nrow(likelihoods)

# prior 
m.prior = rep(1/nstates,nstates)

# transition model 
m.transition = matrix(rep(1,nstates*nstates),nrow=nstates) #uniform 
rownames(m.transition) = paste("to",states)
colnames(m.transition) = paste("from",states)
m.transition = t(apply(m.transition,1,function(x) x/sum(x)))

loss = as.data.frame(matrix(NA,ncol=2,nrow=em.steps)) 
colnames(loss) = c("step","y")
loss$step = 1:em.steps
l.transition = list()
l.posterior = list()

for (it in 1:em.steps){ # EM
  
  # FORWARD STEP 
  alpha = matrix(NA, nrow = nsamples,ncol = nstates)
  alpha[1,] = m.prior%*%diag(likelihoods[1,])
  alpha[1,] = alpha[1,]/sum(alpha[1,])
  
  for (t in 1:(nsamples-1)){
    alpha[t+1,] = diag(likelihoods[t+1,])%*%t(m.transition)%*%alpha[t,]
    alpha[t+1,] = alpha[t+1,]/sum(alpha[t+1,]) #normalizing
  }
  
  # BACKWARD STEP 
  beta = matrix(NA,ncol=nstates,nrow=nsamples)
  beta[nsamples,] = rep(1,nstates)
  
  for (t in (nsamples-1):1){
    beta[t,] = t(m.transition)%*%diag(likelihoods[t+1,])%*%beta[t+1,]
    beta[t,] = beta[t,]/sum(beta[t,]) #normalizing
  }
  
  # GAMMA 
  gamma = alpha*beta
  gamma = t(apply(gamma,1,function(x) x/sum(x))) #normalizing 
  
  # EPSILON: #DOUBLE CHECK THIS STEP 
  epsilon = list() 
  for (t in 1:(nsamples-1)){
    # y = 2-samples$outcome[t+1]
    m.obs = matrix(rep(unlist(likelihoods[t+1,]),nstates),nrow=nstates,byrow = T)
    epsilon[[t]] = alpha[t,]%*%t(beta[t+1,])*t(m.transition)*m.obs
    epsilon[[t]] = epsilon[[t]]/sum(epsilon[[t]]) #normalizing 
  }
  
  # PRIOR.STAR 
  prior.star = gamma[1,]
  
  # TRANSITION.STAR
  epsilon.sums = 0 
  for(t in 1:(nsamples-1)){
    tmp = epsilon[[t]]
    epsilon.sums = epsilon.sums+tmp
  }
  gamma.sums = matrix(rep(apply(gamma[-nsamples,],2,sum),nstates),nrow=nstates)
  transition.star = epsilon.sums/gamma.sums
  
  #some stuff to record
  l.transition[[it]] = m.transition
  
  m.prior = prior.star 
  m.transition = t(transition.star) 
}

gamma = as.data.frame(gamma)

complete_entries = which(apply(!is.na(df.long), 1, all))
posterior = as.data.frame(matrix(NA,ncol=nstates,nrow=nrow(df.long)))
colnames(posterior) = paste("post.",states,sep="")
posterior[complete_entries,] = gamma
df.long = cbind(df.long,posterior)

#add new states to dataframe 
df.long$p.max = NA
df.long$p.max[complete_entries] = states[apply(gamma,1,which.max)]

rm(list=setdiff(ls(), c("df.long","df.participants","l.transition","path.save")))

# VITERBI algorithm  --------------------------------------------------------------------------

transition.matrix = l.transition[[length(l.transition)]]

states = c("A.look","B.look","A.predict","B.predict","A.counterfactual","B.counterfactual","other.look")

# set up the model 
likelihoods = df.long %>%
  na.omit() %>% 
  select(one_of(states)) 

#log transform transition matrix and likelihoods 

transition.matrix = log2(transition.matrix)
likelihoods = log2(likelihoods)

v = matrix(NA,nrow=nrow(likelihoods),ncol=ncol(likelihoods))

v[1,] = as.numeric(-1+likelihoods[1,])

for (i in 2:nrow(likelihoods)){
  v[i,] = as.numeric(likelihoods[i,] + apply(v[i-1,]+transition.matrix,2,max))
}

label = apply(v,1,which.max)

df.long$v.max = NA
df.long$v.max[!is.na(df.long$p.max)] = states[label]

rm(list=setdiff(ls(), c("df.long","df.participants","transition.matrix", "path.save")))

# Save data file  -----------------------------------------------------------------------------

save(list = ls(),file = paste(path.save,
                              df.participants$condition,"_",df.participants$participant,".RData",sep=""))


