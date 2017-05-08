#' ---
#' title: Eye-tracking Causality -- Analysis File 
#' author: Tobias Gerstenberg
#' date: May 8, 2017
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 3
#'      toc_float: true
#'      theme: default
#'      highlight: tango
#' ---

#+ General settings, echo = FALSE, results = 'hide' ------------------------------------------------------------------------------

knitr::opts_chunk$set(fig.width=12, fig.height=8, warning=FALSE, message=FALSE)
figure.path = "../../figures/"

#+ PREPARATION -----------------------------------------------------------------------------
#' # PREPARATION

#+ Load Packages ------------------------------------------------------------------------------------
#' ## Load Packages
rm(list = ls())

library(pwr) # power analysis
library(afex) # package for running ANOVAs 
library(lsr) # useful functions 
library(MASS) # useful functions
library(lme4) # regressions 
library(xtable) # create latex tables 
library(png) # load pngs 
library(ggplot2) # plotting 
library(grid) # plotting
library(gridExtra) # plotting
library(stringr) # string handling  
library(Hmisc) # bootstrapping functions 
library(dplyr) # data frame transformation 
library(tidyr) # data frame transformation 

#+ Helper functions ----------------------------------------------------------------------------
#' ## Helper functions 

# function to make percentages sum up to 100%
roundAndSum = function(x){
  rounded = floor(x)
  difference = 100-sum(rounded)
  decimals = x-floor(x)
  if (difference != 0){
    for (i in 1:difference){
      rounded[which.max(decimals)] = rounded[which.max(decimals)]+1
      decimals[which.max(decimals)] = NA
    }
  }
  return(rounded)
}

# root mean squared error 
rmse = function(x,y){
  return(sqrt(mean((x-y)^2)))
}

# judgment bar graphs 

judgmentPlot = function(df.plot,question,name){
  p = ggplot(df.plot,aes(x=index,y=value,fill=outcome,group=model))+
    geom_bar(stat = "identity",position = position_dodge(0.9), width=0.9, color = "black")+
    geom_linerange(aes(ymin = rating.cl.low, ymax = rating.cl.high),position = position_dodge(0.9), size = 1)+
    geom_text(data = df.plot[1:18,], aes(y=-14,x=index,label = as.character(clip)),
              size = 6,position = position_dodge(width = .7),hjust=0.5)+
    facet_grid(outcome.actual ~ outcome.counterfactual)+
    scale_fill_manual(values=c("red","green","black"))+
    theme_bw()+
    ylab(question)+
    coord_cartesian(xlim = c(0.5, 2.5),ylim=c(0,100))+
    theme_bw()+
    theme(legend.position="none",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.spacing=unit(c(.8),"cm"),
          plot.margin=unit(c(0.2,0.2,.5,.2),"cm"),
          axis.title.x = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          strip.background = element_blank(),panel.border = element_rect(colour = "black"),
          text=element_text(size=20)
    )
  gt = ggplot_gtable(ggplot_build(p))
  gt$layout$clip = "off"
  grid.draw(gt)
  # print(p)
}

# print ANOVA results 

printAnova = function(fit,correction,row = 1){
  cat(
    "F(", fit$anova_table$`num Df`[row],",",fit$anova_table$`den Df`[row],") = ", fit$anova_table$F[row] %>% round(2),
    ", p = ", fit$anova_table$`Pr(>F)`[row] %>% round(3) * correction,
    ", \\eta_p^2 = ", fit$anova_table$pes[row] %>% round(2),
    sep = "")
}

#+ DATA PROCESSING -----------------------------------------------------------------------------
#' # DATA PROCESSING 

#+ Read in data file  --------------------------------------------------------------------------
#' ## Read in data file  

load("../../data/summary/trackingDataFrames.RData")
# load("trackingDataFrames.RData")

#+ Exclude participants based on trackloss  -----------------------------------------------------------------------
#' ## Exclude participants based on track loss  
#' - Removes practice clips, and excludes participants based on track loss 
#' - Displays average track loss  for each condition 

df.trackloss = df.eyes %>% 
  group_by(condition,participant) %>% 
  summarise(n = n(),
            nas = sum(is.na(x) | is.na(y))
  ) %>% 
  mutate(freq = nas/n) %>% 
  arrange(freq) %>%
  mutate(index = 1:length(condition)) %>%
  group_by(condition) %>% 
  mutate(exclude = ifelse(index > 10,1,0)) %>% 
  ungroup

df.eyes = df.eyes %>% 
  merge(.,df.trackloss %>% select(condition,participant,exclude)) %>% 
  filter(exclude == 0) %>% 
  select(-exclude)

df.judgments = df.judgments %>% 
  merge(.,df.trackloss %>% select(condition,participant,exclude)) %>% 
  filter(exclude == 0) %>% 
  select(-exclude)

# remove practice trials 
df.eyes = df.eyes %>% 
  filter(clip < 19)

df.judgments = df.judgments %>% 
  filter(clip < 19)

rm("df.trackloss")

# trackloss per condition
df.eyes %>% 
  group_by(condition,participant) %>% 
  summarise(trackloss = mean(is.na(x) | is.na(y))) %>% 
  group_by(condition) %>% 
  summarise(trackloss.mean = mean(trackloss),
            trackloss.sd = sd(trackloss)) %>% 
  mutate_each(funs(round(.*100,2)),contains("track"))
  

#+ Demographics --------------------------------------------------------------------------------
#' ## Demographics 
#' - Displays demographic information

df.demographics = df.judgments %>% 
  group_by(condition,participant) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  summarise(age.mean = round(mean(age),2),
            age.sd = round(sd(age),2),
            nfemale = sum(sex == "female"),
            time.mean = round(mean(time),2),
            time.sd = round(sd(time),2)) %>% 
  print()

# --------------------- -----------------------------------------------------------------------
#+ ANALYSES & PLOTS ------------------------------------------------------------------------------------
#' # ANALYSES & PLOTS 
# --------------------- -----------------------------------------------------------------------

#+ Behavioral judgments: Counterfactual condition ----------------------------------------------
#' ## Behavioral judgments: Counterfactual condition 
#' - ANOVA results and group means 

# ANOVA
fit = aov_ez(id = "participant",
             dv = "rating",
             data = df.judgments %>% filter(condition == "counterfactual"),
             between = NULL, 
             anova_table = list(es = "pes",correction="none"),
             within = c("outcome.actual","outcome.counterfactual"))
fit$anova_table

printAnova(fit,correction = 1, row = 1)
printAnova(fit,correction = 1, row = 2)

# Means and SDs 
df.judgments %>% filter(condition == "counterfactual") %>% 
  group_by(outcome.counterfactual) %>% 
  summarise(mean = mean(rating),
            sd = sd(rating))

#+ Plot: Counterfactual condition  -------------------------------------------------------------
#' ## Plot: Counterfactual condition  
#' - Plots the model predictions and results of the counterfactual condition 
#' - Shows r and RMSE of the model  

# read noisy simulation results
load("../../data/summary/simple_stepNoise_predictions.RData")

df.plot = df.judgments %>% 
  filter(condition == "counterfactual") %>% 
  group_by(condition,clip,outcome,outcome.actual,outcome.counterfactual,index) %>%
  summarise(rating.mean = smean.cl.boot(rating)[1],
            rating.cl.low = smean.cl.boot(rating)[2],
            rating.cl.high = smean.cl.boot(rating)[3]) %>%
  ungroup %>% 
  mutate(outcome.actual = factor(outcome.actual,labels = paste("actual",c("miss","close","hit"))),
         outcome.counterfactual = factor(outcome.counterfactual,labels = paste("counterfactual",c("miss","close","hit"))))

# find noise values that predicts participants' counterfactual judgments best
tmp = numeric(length=length(simple_noise_counterfactual.means))
for (i in 1:length(simple_noise_counterfactual.means)){
  tmp[i] = cor(simple_noise_counterfactual.means[[i]],df.plot$rating.mean)
}

#counterfactual outcomes
df.plot$outcome = c(0, 0, 0, 1, 1, 1,
                    0, 0, 1, 0, 1, 1,
                    0, 0, 1, 0, 1, 1)
df.plot$outcome = as.factor(df.plot$outcome)

df.plot$model = simple_noise_counterfactual.means[[which.max(tmp)]]
df.plot$model = as.numeric(lm(rating.mean~model,data = df.plot)$fitted.values)

df.plot = df.plot %>% 
  gather(model,value,c(rating.mean,model)) %>% 
  mutate(outcome = as.factor(ifelse(model == "model",3,outcome)),
         model = factor(model,levels = c("rating.mean","model"))) %>% 
  mutate_each(funs(ifelse(model == "model",NA,.)),contains("cl."))

#plot 
judgmentPlot(df.plot,expression(paste('Agreement with ', bold(counterfactual), ' statement')),'Figure 2a')


# r and RMSE 
cor(df.plot$value[df.plot$model == "rating.mean"],
    df.plot$value[df.plot$model == "model"]) %>% round(2)

rmse(df.plot$value[df.plot$model == "rating.mean"],
     df.plot$value[df.plot$model == "model"]) %>% round(2)

#+ Behavioral judgments: Causal condition ----------------------------------------------
#' ## Behavioral judgments: Causal condition 
#' - ANOVA results and group means (separate for cause and prevention judgments)

# ANOVA on prevention judgments
fit = aov_ez(id = "participant",
             dv = "rating",
             data = df.judgments %>% filter(condition == "causal", outcome == 0),
             between = NULL, 
             anova_table = list(es = "pes",correction="none"),
             within = c("outcome.counterfactual","outcome.actual"))
printAnova(fit,correction = 1, row = 1)
printAnova(fit,correction = 1, row = 2)

# Means and SDs 
df.judgments %>% 
  filter(condition == "causal",outcome == 0) %>% 
  group_by(outcome.counterfactual) %>% 
  summarise(mean = mean(rating),
            sd = sd(rating))

# ANOVA on causation judgments
fit = aov_ez(id = "participant",
             dv = "rating",
             data = df.judgments %>% filter(condition == "causal", outcome == 1),
             between = NULL, 
             anova_table = list(es = "pes",correction="none"),
             within = c("outcome.counterfactual","outcome.actual"))
fit$anova_table
printAnova(fit,correction = 1, row = 1)
printAnova(fit,correction = 1, row = 2)

# Means and SDs 
df.judgments %>% 
  filter(condition == "causal",outcome == 1) %>% 
  group_by(outcome.counterfactual) %>% 
  summarise(mean = mean(rating),
            sd = sd(rating))

#+ Plot: Causal condition  ---------------------------------------------------------------------
#' ## Plot: Causal condition  
#' - Plots the model predictions and results of the causal condition, shows r and RMSE of the model  

df.data = df.judgments %>% 
  filter(condition == "causal") %>% 
  group_by(clip,outcome,outcome.actual,outcome.counterfactual,index) %>%
  summarise(rating.mean = smean.cl.boot(rating)[1],
            rating.cl.low = smean.cl.boot(rating)[2],
            rating.cl.high = smean.cl.boot(rating)[3]) %>%
  ungroup

# use participants' counterfactual judgments as prediction
df.model = df.judgments %>% 
  filter(condition == "counterfactual") %>% 
  group_by(clip,outcome,outcome.actual,outcome.counterfactual,index) %>%
  summarise(model = mean(rating)) %>% 
  mutate(model = ifelse(outcome == 0,model,100-model)) %>% 
  ungroup

df.plot = merge(df.data,df.model)

regression = function(x){
  tmp = as.numeric(lm(rating.mean~model,data=x)$fitted.values)
  return(tmp)
}

df.plot = df.plot %>%
  mutate(model = regression(.),
         outcome.actual = factor(outcome.actual,labels = paste("actual",c("miss","close","hit"))),
         outcome.counterfactual = factor(outcome.counterfactual,labels = paste("counterfactual",c("miss","close","hit"))))

df.plot = df.plot %>% 
  gather(model,value,c(rating.mean,model)) %>% 
  mutate(outcome = as.factor(ifelse(model == "model",3,outcome)),
         model = factor(model,levels = c("rating.mean","model"))) %>% 
  mutate_each(funs(ifelse(model == "model",NA,.)),contains("cl."))

#plot 
judgmentPlot(df.plot,expression(paste('Agreement with ', bold(causal), ' statement')), 'Figure 2b')

# r and RMSE 
cor(df.plot$value[df.plot$model == "rating.mean"],
    df.plot$value[df.plot$model == "model"]) %>% round(2)

rmse(df.plot$value[df.plot$model == "rating.mean"],
     df.plot$value[df.plot$model == "model"]) %>% round(2)

#+ Behavioral judgments: Outcome condition ----------------------------------------------
#' ## Behavioral judgments: Outcome condition 
#' - ANOVA results and group means (separate for positive and negative outcomes)

# ANOVA 
fit = aov_ez(id = "participant",
             dv = "rating",
             data = df.judgments %>% filter(condition == "outcome", outcome == 0),
             between = NULL, 
             anova_table = list(es = "pes",correction="none"),
             within = c("outcome.actual","outcome.counterfactual"))
fit$anova_table
printAnova(fit,correction = 1, row = 1)
printAnova(fit,correction = 1, row = 2)


# Means and SDs 
df.judgments %>% 
  filter(condition == "outcome",outcome == 0) %>% 
  group_by(outcome.actual) %>% 
  summarise(mean = mean(rating),
            sd = sd(rating))

# ANOVA 
fit = aov_ez(id = "participant",
             dv = "rating",
             data = df.judgments %>% filter(condition == "outcome", outcome == 1),
             between = NULL, 
             anova_table = list(es = "pes",correction="none"),
             within = c("outcome.actual","outcome.counterfactual"))
fit$anova_table
printAnova(fit,correction = 1, row = 1)
printAnova(fit,correction = 1, row = 2)

df.judgments %>% 
  filter(condition == "outcome",outcome == 1) %>% 
  group_by(outcome.actual) %>% 
  summarise(mean = mean(rating),
            sd = sd(rating))

#+ Plot: Outcome condition  --------------------------------------------------------------------
#' ## Plot: Outcome condition  
#' - Plots the model predictions and results of the outcome condition, shows r and RMSE of the model  

# calculate distance from center of the goal 
df.model = df.eyes %>% 
  filter(participant == 1, instance == 1) %>% 
  group_by(clip) %>%
  filter(frame == t.outcome.actual) %>%
  mutate(distance = abs(both.B.y-384)) %>% 
  ungroup() %>% 
  select(clip,distance)

regression = function(x){
  tmp = as.numeric(lm(rating.mean~distance*outcome,data=x)$fitted.values)
  return(tmp)
}

df.plot = df.judgments %>% 
  filter(condition == "outcome") %>% 
  group_by(clip,outcome,outcome.actual,outcome.counterfactual,index) %>%
  summarise(rating.mean = smean.cl.boot(rating)[1],
            rating.cl.low = smean.cl.boot(rating)[2],
            rating.cl.high = smean.cl.boot(rating)[3]) %>%
  ungroup %>% 
  left_join(df.model)

lm(rating.mean~distance*outcome,data=df.plot) %>% summary()

df.plot = df.plot %>%
  mutate(model = regression(.),
         outcome.actual = factor(outcome.actual,labels = paste("actual",c("miss","close","hit"))),
         outcome.counterfactual = factor(outcome.counterfactual,labels = paste("counterfactual",c("miss","close","hit"))))

df.plot = df.plot %>% 
  gather(model,value,c(rating.mean,model)) %>% 
  mutate(outcome = as.factor(ifelse(model == "model",3,outcome)),
         model = factor(model,levels = c("rating.mean","model"))) %>% 
  mutate_each(funs(ifelse(model == "model",NA,.)),contains("cl."))

#plot 
judgmentPlot(df.plot,expression(paste('Agreement with ', bold(outcome), ' statement')), 'Figure 2c')

# r and RMSE 
cor(df.plot$value[df.plot$model == "rating.mean"],
    df.plot$value[df.plot$model == "model"]) %>% round(2)

rmse(df.plot$value[df.plot$model == "rating.mean"],
     df.plot$value[df.plot$model == "model"]) %>% round(2)

#+ Eye-tracking: Static analysis  ------------------------------------------------------------
#' ## Eye-tracking: Static analysis  
#' - Categorizes participants' looks prior to the two balls colliding into counterfactual looks and other looks
#' - Displays results table separated by condition, and separated by condition x clip 
#' - Reports statistical tests on whether looks differ between conditions 

threshold.distance = 100 #distance in pixels from the counterfactual path 
threshold.x = 50 #minimal distance from collision point

df.counterfactual.path = df.eyes %>% 
  filter(participant == 1, instance == 1, condition == "causal") %>% 
  group_by(clip) %>% 
  filter(frame == t.collision | frame == t.outcome.counterfactual) %>% 
  ungroup() %>% 
  arrange(clip,frame) %>% 
  mutate(path.marker = rep(c("start","end"),(nrow(.)/2))) %>% 
  select(clip,onlyB.x,onlyB.y,path.marker)

df.counterfactual.path = cbind(df.counterfactual.path %>% filter(path.marker == "start") %>% select(-path.marker),
                               df.counterfactual.path %>% filter(path.marker == "end") %>% select(-path.marker,-clip)) %>% 
  setNames(c("clip","path.start.x","path.start.y","path.end.x","path.end.y"))

df.eyes = df.eyes %>% 
  left_join(df.counterfactual.path)

# distance of each fixation to the counterfactual path 
fun.distance_to_line = function(x,y,path.start.x,path.start.y,path.end.x,path.end.y){
  distance = abs((path.end.y-path.start.y)*x-(path.end.x-path.start.x)*y+path.end.x*path.start.y-path.end.y*path.start.x)/
    sqrt((path.end.y-path.start.y)^2+(path.end.x-path.start.x)^2) #distance of point to line 
  return(distance)
}

df.eyes = df.eyes %>% 
  rowwise %>% 
  mutate(counterfactual.distance = fun.distance_to_line(x,y,path.start.x,path.start.y,path.end.x,path.end.y)) %>% 
  ungroup() %>% 
  mutate(counterfactual.look = ifelse(counterfactual.distance < threshold.distance & 
                                        !is.na(counterfactual.distance) & 
                                        x < (path.start.x-threshold.x), "counterfactual", "other")) %>% 
  mutate(counterfactual.look = factor(counterfactual.look,levels = c("counterfactual","other"))) %>% 
  arrange(condition,participant,clip,instance,frame)

# result table 
df.tmp = df.eyes %>% 
  na.omit() %>% 
  group_by(clip) %>% 
  filter(saccade == 'sacc.end') %>% 
  filter(frame > 15,frame < t.collision) %>% 
  group_by(condition,participant,counterfactual.look) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n),
         freq_rounded = roundAndSum(freq*100)) %>% 
  group_by(condition,counterfactual.look) %>% 
  summarise(mfreq = mean(freq),sdfreq = round(sd(freq)*100,2)) %>% 
  mutate(mfreq = roundAndSum(mfreq*100)) %>% 
  filter(counterfactual.look == "counterfactual") %>% 
  ungroup %>% 
  mutate(data = paste0(mfreq,"% (", sdfreq,")")) %>% 
  select(condition,data) %>%
  spread(condition,data) %>% 
  print()

# results per clip
df.tmp = df.eyes %>%
  na.omit() %>%
  filter(saccade == "sacc.end", frame < t.collision, frame > 15) %>%
  group_by(clip,condition,participant,counterfactual.look) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n),
         freq_rounded = roundAndSum(freq*100)) %>%
  complete(counterfactual.look,fill=list(n=0,freq=0,freq_rounded=0)) %>% 
  group_by(condition,clip,counterfactual.look) %>% 
  summarise(mfreq = mean(freq),sdfreq = round(sd(freq)*100,2)) %>% 
  ungroup() %>% 
  mutate(mfreq = roundAndSum(mfreq*100)) %>% 
  filter(counterfactual.look == "counterfactual") %>% 
  ungroup %>% 
  mutate(data = paste0(mfreq,"% (", sdfreq,")")) %>% 
  select(condition,clip,data) %>%
  spread(condition,data) %>% 
  print()

# Chi-square test of types of looks per condition 

df.chi = df.eyes %>% 
  filter(saccade == "sacc.end") %>% 
  filter(frame < t.collision, frame > 15)
  
df.chi = table(df.chi$counterfactual.look,df.chi$condition)
chisq.test(df.chi)

#+ Plot: Fixation map prior to collision ------------------------------------------------------------
#' ## Plot: Fixation map prior to collision 
#' - Plots fixation maps for clip 4 in the causal condition (code can be adapted to show plots for other conditions and clips)

# condition.name = "counterfactual"
condition.name = "causal"
# condition.name = "outcome"

# clips = 1:18
clips = 4

df.plot = df.eyes %>% 
  filter(saccade == "sacc.end") %>%
  filter(condition == condition.name) %>% 
  filter(frame < t.collision, frame > 15) %>% 
  na.omit()

df.legend_labels = df.plot %>% 
  group_by(clip,counterfactual.look) %>% 
  summarise (n = n()) %>%
  complete(counterfactual.look,fill = list(n=0)) %>% 
  group_by(clip) %>% 
  mutate(freq = n / sum(n),
         freq_rounded = roundAndSum(freq*100)) %>%
  ungroup

for (cli in clips){
  ins = c(1,2)
  df = df.plot %>% 
    filter(clip == cli,
           instance %in% ins)
  
  #HACK: add one of each look (behind the legend) to make sure the legend is correct
  tmp = df[1:2,]
  tmp = tmp %>% 
    mutate(x = 500,
           y = 50,
           counterfactual.look = c("counterfactual","other"))
  df = rbind(df,tmp)
  
  m = readPNG(paste0("../../figures/diagrams/png/clip_",cli,".png"), FALSE)
  w = matrix(rgb(m[,,1],m[,,2],m[,,3]), nrow=dim(m)[1])
  
p = ggplot(df,aes(x=x,y=y,color=counterfactual.look))+
    annotation_custom(xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf,
                      rasterGrob(w)) +
    geom_point(size=5, alpha = 0.5) +
    scale_color_manual(
      "",
      labels = paste0(as.vector(unlist(df.legend_labels %>% filter(clip == cli) %>% select(freq_rounded))),"%"),
      values = c("green","gray"))+
    expand_limits(x=c(0,1024),y=c(0,768))+
    scale_y_continuous(expand = c(0,0)) + 
    scale_x_continuous(expand = c(0,0)) +
    labs(x=NULL, y=NULL)+
    theme_void()+
    theme(
      text = element_text(size=40),
      axis.ticks.length = unit(0,"null"),
      legend.position = c(0.5,0), 
      legend.justification = c(0.5,-0.1),
      legend.key = element_rect(fill=alpha('white', 0)),
      legend.background = element_rect(fill='white',color="black"),
      legend.key.height = unit(1.2,"cm"),
      legend.direction = "horizontal"
    )

}
print(p)

#+ Eye-tracking: Analysis of HMM results  -----------------------------------------------
#' ## Eye-tracking: Analysis of HMM results  
#' - Runs ANOVA on the probability that participants made a counterfactual look to where ball B would have gone 
#' - Reports the ANOVA results, as well as the results of post-hoc comparisons between conditions 

# Mixed ANOVAs

# variable = "post.A.look"
# variable = "post.B.look"
# variable = "post.A.predict"
# variable = "post.B.predict"
# variable = "post.A.counterfactual"
variable = "post.B.counterfactual"
# variable = "post.other"

df.analysis = df.eyes %>% 
  filter(saccade == "sacc.end") %>% 
  filter(frame < t.outcome.actual, frame > 15) %>% 
  select_("participant","condition",variable) %>% 
  group_by(participant,condition) %>%
  summarize_(value = paste0("mean(",variable,",na.rm=T)")) %>% 
  ungroup()

fit = aov_ez(id = "participant",
             dv = "value",
             data = df.analysis,
             between = "condition", 
             anova_table = list(es = "pes",correction="none"),
             within = NULL)
summary(fit)

#post-hoc tests 
tmp = lsmeans(fit, "condition", contr = "pairwise") 
ttest = summary(tmp,adjust="none")$contrasts

# effect sizes 
cohen = c(cohensD(df.analysis %>% filter(condition == "counterfactual") %>% select(value) %>% unlist %>% as.numeric,
                  df.analysis %>% filter(condition == "causal") %>% select(value) %>% unlist %>% as.numeric),
            cohensD(df.analysis %>% filter(condition == "counterfactual") %>% select(value) %>% unlist %>% as.numeric,
                    df.analysis %>% filter(condition == "outcome") %>% select(value) %>% unlist %>% as.numeric),
            cohensD(df.analysis %>% filter(condition == "causal") %>% select(value) %>% unlist %>% as.numeric,
                    df.analysis %>% filter(condition == "outcome") %>% select(value) %>% unlist %>% as.numeric))


# print results 
correction = 7 # bonferroni correction for multiple comparisons 

cat(
  "F(", fit$anova_table$`num Df`,",",fit$anova_table$`den Df`,") = ", fit$anova_table$F %>% round(2),
  ", p = ", fit$anova_table$`Pr(>F)` %>% round(3),
  ", \\eta_p^2 = ", fit$anova_table$pes %>% round(2),
  sep = "")

for (i in 1:nrow(ttest)){
  cat("\n")
  cat(ttest$contrast[i] %>% as.character,": ",
    "t(",ttest$df[i] ,") = ", ttest$t.ratio[i] %>% round(2),
  ", p = ", ttest$p.value[i] %>% round(3),
  ", d = ", cohen[i] %>% round(2),
  sep = "")
}

#+ Plot: HMM results  --------------------------------------------------------------------------------
#' ## Plot: HMM results  
#' - Plots the average probability of participants being in different states separated by condition (only taking into account end points of participants' saccades)

df.plot = df.eyes %>% 
  filter(saccade == "sacc.end") %>%
  filter(frame < t.outcome.actual, frame > 15) %>%
  group_by(participant,condition) %>% 
  select(contains("post")) %>% 
  summarise_each(funs(mean(.,na.rm=T))) %>% 
  gather(look,percentage,-c(condition,participant)) %>% 
  ungroup %>%
  mutate(look = factor(look,levels = c("post.A.look", "post.B.look", "post.A.predict", "post.B.predict",
                                       "post.A.counterfactual", "post.B.counterfactual","post.other"),
                       labels=c("A look", "B look", "A predict ", "B predict ", "A counterfactual ",
                                "B counterfactual ", "other")),
         condition = factor(condition,levels = c("counterfactual","causal","outcome"),
                            labels=c(expression("counterfactual\ncondition"),
                                     expression("causal\ncondition"),
                                     expression("outcome\ncondition")
                            ))) %>% 
  arrange(condition,participant,look)

ggplot(df.plot,aes(x=look,y=percentage,fill = look))+
  stat_summary(fun.y = mean, geom = "bar", color="black", 
               position = position_dodge(0.8), width=0.8)+
  stat_summary(fun.data = mean_cl_boot, geom = "linerange",size = 1, 
               position = position_dodge(0.8))+
  facet_grid(~condition)+
  labs(y = 'probability of each type of look', fill = '')+
  scale_y_continuous(breaks = seq(0,0.4,0.1),labels = paste0(seq(0,40,10),"%"),
                     expand=c(0,0))+
  coord_cartesian(ylim=c(0,0.45))+
  theme_bw()+
  theme(text = element_text(size=30),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "gray60",linetype=2),
        strip.background = element_blank(),panel.border = element_rect(colour = "black"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size=36),
        legend.text = element_text(size=30),
        panel.spacing.y=unit(c(.8),"cm"),
        plot.margin=unit(c(0.2,0.2,.2,.2),"cm"),
        axis.title.y = element_text(margin = margin(0,0.5,0,0,unit="cm")),
        legend.key = element_blank(),
        legend.key.size = unit(1.2,"cm"),
        legend.key.height = unit(1.2,"cm")
  )+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

#+ Plot: HMM results (early vs. late trials)  --------------------------------------------------------------------------------
#' ## Plot: HMM results (early vs. later trials) 
#' - Plots the average probability of participants being in different states separated by condition (only taking into account end points of participants' saccades)

df.plot = df.eyes %>% 
  left_join(df.judgments %>% select(participant,clip,trial)) %>% 
  mutate(time = ifelse(trial <= 9, 'early','late')) %>% 
  filter(saccade == "sacc.end") %>%
  filter(frame < t.outcome.actual, frame > 15) %>%
  group_by(participant,condition,time) %>% 
  select(contains("post")) %>% 
  summarise_each(funs(mean(.,na.rm=T))) %>% 
  gather(look,percentage,-c(condition,participant,time)) %>% 
  ungroup %>%
  mutate(look = factor(look,levels = c("post.A.look", "post.B.look", "post.A.predict", "post.B.predict",
                                       "post.A.counterfactual", "post.B.counterfactual","post.other"),
                       labels=c("A look", "B look", "A predict ", "B predict ", "A counterfactual ",
                                "B counterfactual ", "other")),
         condition = factor(condition,levels = c("counterfactual","causal","outcome"),
                            labels=c(expression("counterfactual\ncondition"),
                                     expression("causal\ncondition"),
                                     expression("outcome\ncondition")
                            )))

ggplot(df.plot,aes(x=look,y=percentage,fill = look))+
  stat_summary(fun.y = mean, geom = "bar", color="black", 
               position = position_dodge(0.8), width=0.8)+
  stat_summary(fun.data = mean_cl_boot, geom = "linerange",size = 1, 
               position = position_dodge(0.8))+
  facet_grid(time~condition)+
  labs(y = 'probability of each type of look', fill = '')+
  scale_y_continuous(breaks = seq(0,0.4,0.1),labels = paste0(seq(0,40,10),"%"),
                     expand=c(0,0))+
  coord_cartesian(ylim=c(0,0.45))+
  theme_bw()+
  theme(text = element_text(size=30),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "gray60",linetype=2),
        strip.background = element_blank(),panel.border = element_rect(colour = "black"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size=36),
        legend.text = element_text(size=30),
        panel.spacing.y=unit(c(.8),"cm"),
        plot.margin=unit(c(0.2,0.2,.2,.2),"cm"),
        axis.title.y = element_text(margin = margin(0,0.5,0,0,unit="cm")),
        legend.key = element_blank(),
        legend.key.size = unit(1.2,"cm"),
        legend.key.height = unit(1.2,"cm")
  )+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

# ggsave('hmm_early_late.pdf',width=14,height=8)

#+ Eyes & Judgments: Relationship between counterfactual looks and outcome certainty  ----------------------------
#' ## Eyes & Judgments: Relationship between counterfactual looks and outcome certainty  
#' - Checks whether there is a relationship between participants' eye-movements and their uncertainty in the causal judgment

# check whether participants make more counterfactual looks for the clips in which the 
# counterfactual outcome was close 
df.tmp = df.eyes %>%
  na.omit() %>%
  filter(condition == "causal") %>% 
  filter(saccade == "sacc.end") %>%
  group_by(participant,outcome.counterfactual) %>%
  summarise_each(funs(mean),contains("post")) %>% 
  select(participant,outcome.counterfactual,post.B.counterfactual) %>% 
  ungroup()

fit = aov_ez(id = "participant",
             dv = "post.B.counterfactual",
             data = df.tmp,
             between = NULL, 
             anova_table = list(es = "pes",correction="none"),
             within = c("outcome.counterfactual"))
fit$anova_table

tmp = lsmeans(fit, "outcome.counterfactual", contr = "pairwise")
ttest = summary(tmp,adjust="none")$contrasts

cohen = c(cohensD(df.tmp %>% filter(outcome.counterfactual == "miss") %>% select(post.B.counterfactual) %>% unlist %>% as.numeric,
                  df.tmp %>% filter(outcome.counterfactual == "close") %>% select(post.B.counterfactual) %>% unlist %>% as.numeric),
          cohensD(df.tmp %>% filter(outcome.counterfactual == "miss") %>% select(post.B.counterfactual) %>% unlist %>% as.numeric,
                  df.tmp %>% filter(outcome.counterfactual == "hit") %>% select(post.B.counterfactual) %>% unlist %>% as.numeric),
          cohensD(df.tmp %>% filter(outcome.counterfactual == "close") %>% select(post.B.counterfactual) %>% unlist %>% as.numeric,
                  df.tmp %>% filter(outcome.counterfactual == "hit") %>% select(post.B.counterfactual) %>% unlist %>% as.numeric))

printAnova(fit,1)

for (i in 1:nrow(ttest)){
  cat("\n")
  cat(ttest$contrast[i] %>% as.character,": ",
      "t(",ttest$df[i] ,") = ", ttest$t.ratio[i] %>% round(2),
      ", p = ", ttest$p.value[i] %>% round(3),
      ", d = ", cohen[i] %>% round(2),
      sep = "")
}

# Plot 
df.plot = df.eyes %>% 
  filter(condition == "causal") %>%
  filter(saccade == "sacc.end") %>%
  filter(frame < t.outcome.actual, frame > 15) %>%
  group_by(participant,clip,outcome) %>%
  mutate(rating = abs(50-rating)) %>%
  summarize(counterfactual = mean(post.B.counterfactual,na.rm=T),rating = mean(rating))

ggplot(df.plot,aes(x=counterfactual,y=rating))+
  geom_smooth(method = lm,color = "black")+
  geom_point()+
  labs(y = "certainty in causal jugment", x = "p(look = B counterfactual)")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        text = element_text(size = 24),
        axis.text.y = element_text(angle = 0),
        legend.position = "none"
  )

cor.test(df.plot$counterfactual,df.plot$rating,test = T)

#+ Eyes & Judgments: Relationship between counterfactual looks and model predictions  ----------------------------
#' ## Eyes & Judgments: Relationship between counterfactual looks and model predictions  
#' - Checks whether there is a relationship between participants' eye-movements and how well the counterfactual simulation model explains their judgments

# model predictions based on participants' counterfactual judgments 
df.model = df.judgments %>% 
  filter(condition == "counterfactual") %>% 
  select(outcome,participant,clip,rating) %>% 
  group_by(outcome,clip) %>% 
  summarise(model = mean(rating)) %>% 
  mutate(model = ifelse(outcome == 0,model,100-model))

# merge counterfactual looks and model predictions, aggregate per participant  
df.plot = df.eyes %>% 
  filter(condition == "causal") %>%
  filter(saccade == "sacc.end") %>%
  filter(frame < t.outcome.actual, frame > 15) %>%
  group_by(participant,clip,outcome) %>%
  summarize(counterfactual = mean(post.B.counterfactual,na.rm=T),rating = mean(rating)) %>% 
  merge(.,df.model) %>% 
  group_by(participant) %>% 
  summarize(counterfactual = mean(counterfactual), fit = cor(rating,model)) %>% 
  arrange(counterfactual) %>% 
  print()

# plot 
ggplot(df.plot,aes(x=counterfactual,y=fit))+
  geom_smooth(method = lm,color = "black")+
  geom_point(size=4)+
  labs(y = "model fit", x = "p(look = B counterfactual)")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        text = element_text(size = 24),
        axis.text.y = element_text(angle = 0)
  )

cor.test(df.plot$counterfactual,df.plot$fit)
rmse(df.plot$counterfactual,df.plot$fit)

# --------------------- -----------------------------------------------------------------------
#+ SUPPLEMENTARY PLOTS ---------------------------------------------------------------------------------------
#' # SUPPLEMENTARY PLOTS 
# --------------------- -----------------------------------------------------------------------

#+ Change in r and RMSE for counterfactual judgments -------------------------------------------
#' ## Change in r and RMSE for counterfactual judgments 
#' - Plots the change in r and RMSE as a function of how much noise is added to ball B's motion vector in the counterfactual simulation

df.plot = df.judgments %>% 
  filter(condition == "counterfactual") %>% 
  select(condition,outcome,participant,clip,rating) %>% 
  group_by(clip,outcome) %>% 
  summarise(rating.mean = mean(rating)) %>% 
  arrange(clip) %>% 
  ungroup

# find noise values that predicts participants' counterfactual judgments best
tmp = as.data.frame(matrix(nrow= length(simple_noise_counterfactual.means),ncol = 3))
colnames(tmp) = c("noise", "r", "RMSE")

for (i in 1:length(simple_noise_counterfactual.means)){
  tmp$noise[i] = names(simple_noise_counterfactual.means)[i]
  df = data.frame(dv = df.plot$rating.mean, iv = simple_noise_counterfactual.means[[i]]*100)
  tmp$r[i] = cor(simple_noise_counterfactual.means[[i]],df.plot$rating.mean)
  tmp$RMSE[i] = sqrt(mean((lm(dv~iv,data=df)$fitted.values-df.plot$rating.mean)^2))
}

df.plot = tmp %>% 
  mutate(noise = as.numeric(str_replace(noise,"noise","")))

# Correlation 
ggplot(df.plot,aes(x = noise, y = r))+
  geom_line(size=2)+
  geom_vline(xintercept = 0.7,linetype = 2)+
  theme_bw()+
  labs(y = "Pearson's r", x = "degree of noise")+
  coord_cartesian(ylim = c(0.8,0.925))+
  scale_y_continuous(breaks = seq(0.8,0.925,0.025))+
  scale_x_continuous(breaks = seq(0,2,0.2))+
  theme(panel.grid = element_blank(),
        text = element_text(size=20))

# RMSE
ggplot(df.plot,aes(x = noise, y = RMSE))+
  geom_line(size=2)+
  geom_vline(xintercept = 0.7,linetype = 2)+
  theme_bw()+
  labs(y = "root mean squared error", x = "degree of noise")+
  coord_cartesian(ylim = c(12,18))+
  scale_y_continuous(breaks = seq(12,18,2))+
  scale_x_continuous(breaks = seq(0,2,0.2))+
  theme(panel.grid = element_blank(),
        text = element_text(size=20))

#+ Eye-tracking: Static analysis: Results for each individual participant --------------------------------------
#' ## Eye-tracking: Static analysis: Results for each individual participant 
#' - Classification of the different types of looks shown for each participant

df.plot = df.eyes %>% 
  na.omit() %>% 
  filter(frame > 15, frame < t.collision, saccade == "sacc.end") %>% 
  count(condition,participant,counterfactual.look) %>% 
  group_by(condition,participant) %>%
  mutate(freq = n/sum(n)) %>% 
  left_join(df.eyes %>% expand(participant,counterfactual.look) %>% na.omit(),.) %>% 
  mutate(condition = ifelse(is.na(condition),3,condition),
         condition = factor(condition,levels=1:3,labels = c("counterfactual","causal","outcome"))) %>% 
  mutate_each(funs(ifelse(is.na(.),0,.)),n,freq) %>% 
  arrange(condition) %>%
  mutate(index = rep(rep(1:10,each=2),3))

ggplot(df.plot,aes(x=index,y=freq,fill = counterfactual.look)) +
  geom_bar(stat = "identity",color = "black")+
  facet_wrap(~condition,scales = "free_x")+
  theme_bw()+
  scale_fill_manual(values= c("green","gray"))+
  labs(y = "percentage", x = "participant index", fill = "type of look")+
  scale_x_continuous(breaks = 1:10)+
  scale_y_continuous(breaks = seq(0,1,.25), labels = paste(seq(0,100,25),'%',sep=""))+
  theme(text = element_text(size=20),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),panel.border = element_rect(colour = "black"),
        strip.text = element_text(size=24),
        legend.text = element_text(size=16),
        panel.margin.y=unit(c(.8),"cm"),
        plot.margin=unit(c(0.2,0.2,.2,.2),"cm"),
        axis.title.y = element_text(vjust = 1.5)
  )

#+ Eye-tracking: Static analysis: Counterfacutal looks as a function of the distance parameter --------------------------------------
#' ## Eye-tracking: Static analysis: Counterfacutal looks as a function of the distance parameter 

#' - Classification of the different types of looks as a function of the distance parameter which determines how close the end point of a saccade needs to be to B's counterfactual path in order for it to qualify as a 'counterfactual saccade'

df.fixations = df.eyes %>%
  na.omit() %>%
  expand(condition,counterfactual.look)

threshold.x = 50

for (i in seq(0,300,10)){
# for (i in seq(0,300,50)){
  threshold.distance = i
  df.tmp = df.eyes %>% 
    na.omit() %>% 
    filter(saccade == "sacc.end") %>% 
    group_by(clip) %>% 
    filter(frame < t.collision, frame > 15) %>% 
    mutate(counterfactual.look = ifelse(counterfactual.distance < threshold.distance & 
                                          !is.na(counterfactual.distance) & 
                                          x < (path.start.x-threshold.x), "counterfactual", "other")) %>% 
    mutate(counterfactual.look = factor(counterfactual.look,levels = c("counterfactual","other"))) %>% 
    ungroup %>% 
    group_by(condition,counterfactual.look) %>% 
    summarise(n = n()) %>%
    complete(counterfactual.look,fill = list(n = 0)) %>% 
    mutate(freq = n/sum(n)) %>% 
    select(condition,counterfactual.look,freq) %>%
    left_join(df.fixations,.) %>%  #hack to make sure that all levels are being used
    mutate(freq = ifelse(is.na(freq),0,freq)) %>%
    ungroup
  df.fixations[[paste("threshold_",i,sep="")]] = df.tmp$freq
}

df.fixations = df.fixations %>% 
  gather(threshold,percentage,threshold_0:threshold_300) %>% 
  mutate(threshold = str_replace_all(threshold,"threshold_","") %>% as.numeric())

df.plot = df.fixations %>% 
  mutate(condition = factor(condition, levels = c("counterfactual","causal","outcome"),
                            labels=c(expression("counterfactual\ncondition"),
                                     expression("causal\ncondition"),
                                     expression("outcome\ncondition"))))

ggplot(df.plot,aes(x=threshold,y=percentage,color=counterfactual.look,group=counterfactual.look)) +
  geom_vline(xintercept = 100, linetype = 2)+
  geom_line(stat = "identity",size=2)+
  facet_wrap(~condition)+
  theme_bw()+
  scale_color_manual(values= c("green","gray"))+
  labs(y = "percentage", x = "distance parameter", color = "type of look")+
  scale_x_continuous(breaks = seq(0,300,50))+
  scale_y_continuous(breaks = seq(0,1,.25), labels = paste(seq(0,100,25),'%',sep=""))+
  coord_cartesian(ylim = c(0,1))+
  theme(text = element_text(size=20),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),panel.border = element_rect(colour = "black"),
        strip.text = element_text(size=24),
        legend.text = element_text(size=16),
        panel.margin.y=unit(c(.8),"cm"),
        plot.margin=unit(c(0.2,0.2,.2,.2),"cm"),
        axis.title.y = element_text(vjust = 1.5)
  )

#+ Eye-tracking: HMM posterior over time ---------------------------------
#' ## Eye-tracking: HMM posterior over time 
#' - Plots the average probability of participants being in different states separated by condition (taking into account all gaze positions, and not just the end points of saccades)

# condition.name = "counterfactual"
condition.name = "causal"
# condition.name = "outcome"
states = c("A look","B look","A predict","B predict","A counterfactual","B counterfactual","other")

smoothing_func = function(x,y){
  fit = loess(y~x,span=0.1)
  out = fit$fitted
  return(out)
}  

# cli = 1:18
cli = 4

for (i in cli){

df.plot = df.eyes %>% 
  group_by(clip) %>% 
  filter(frame > 15) %>% 
  # filter(saccade == "sacc.end") %>%
  ungroup %>% 
  filter(condition == condition.name, clip == i) %>% 
  select(participant,instance,frame,contains("post")) %>% 
  arrange(participant,instance,frame) %>% 
  na.omit() %>% 
  group_by(frame) %>% 
  summarise_each(funs(mean),contains("post")) %>% 
  gather(state,probability,contains("post")) %>%
  mutate(state = factor(state, levels = paste0("post.",c("A.look","B.look","A.predict","B.predict",
                                                         "A.counterfactual","B.counterfactual",
                                                         "other")),labels = states)) %>% 
  mutate(probability = ifelse(is.nan(probability),0,probability)) %>% 
  group_by(state) %>%
  mutate(smoothed = smoothing_func(frame,probability))

t.collision = unique(df.eyes$t.collision[df.eyes$clip == i])
t.outcome.actual = unique(df.eyes$t.outcome.actual[df.eyes$clip == i])
  
p = ggplot(df.plot,aes(x = frame,y = smoothed, color = state)) +
  geom_vline(xintercept = t.collision, linetype=2)+
  geom_vline(xintercept = t.outcome.actual, linetype=2)+
  geom_line(size=2)+
  geom_rect(colour="white",fill="white",aes(xmin=t.collision-25,xmax=t.collision+25 ,ymin=0.95,ymax=1.1))+
  geom_rect(colour="white",fill="white",aes(xmin=t.outcome.actual-25,xmax=t.outcome.actual+25 ,ymin=0.95,ymax=1.1))+
  annotate("text",x = t.collision,y = Inf, label = "collision", hjust = 0.5, vjust = 1.5,size=6)+
  annotate("text",x = t.outcome.actual,y = Inf, label = "outcome", hjust = 0.5, vjust = 1.5,size=6)+
  scale_x_continuous(limits=c(min(df.plot$frame)-1,max(df.plot$frame)+1),breaks=seq(0,max(df.plot$frame)+1,25))+
  scale_y_continuous(limits=c(-0.02,1.1),breaks=seq(0,1,0.25),labels = paste0(seq(0,100,25),"%"))+
  coord_cartesian(ylim = c(0,1))+
  labs(y = "probability of each look", color = "", x = "time frame")+
  theme_bw()+
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
    strip.background = element_blank(),panel.border = element_rect(colour = "black"),
    axis.title.x = element_text(margin = margin(0.5,0,0,0,"cm")),
    axis.title.y = element_text(margin = margin(0,0.5,0,0,"cm")),
    text=element_text(size=24),
    legend.text = element_text(size=16),
    legend.title = element_text(size=16)
  )+
  guides(color=guide_legend(nrow=2,byrow=TRUE))

print(p)
}

#+ Eye-tracking: Heatmaps ----------------------------------------------------------------------
#' ## Eye-tracking: Heatmaps 
#' - Displays a heat map of clip 4 in the causal condition (the code can be adapted to show heat maps for other clips and conditions)

# condition.name = "counterfactual"
condition.name = "causal"
# condition.name = "outcome"

clips = 4
ins = c(1,2)

for (cli in clips){
  
  df.tmp = df.eyes %>% 
    na.omit() %>% 
    # filter(saccade == "sacc.end") %>%
    # filter(frame < t.collision) %>% 
    filter(condition %in% condition.name,
           clip == cli,
           instance %in% ins) 
    
  fit = kde2d(df.tmp$x, df.tmp$y, h = c(100,100),n=500)

  df.plot = expand.grid(x = fit$x,y = fit$y)
  df.plot$z = matrix(unlist(fit$z),ncol=1)
  
  # adjust grid bounds (to avoid polygon errors ...)
  minValue<-sapply(df.plot,min)
  maxValue<-sapply(df.plot,max)
  arbitaryValue=min(df.plot$z)
  
  padding = 30
  bound1=data.frame(x=minValue[["x"]]-padding,y=unique(df.plot$y),z=arbitaryValue)
  bound2=data.frame(x=unique(df.plot$x),y=minValue[["y"]]-padding,z=arbitaryValue)
  bound3=data.frame(x=maxValue[["x"]]+padding,y=unique(df.plot$y),z=arbitaryValue)
  bound4=data.frame(x=unique(df.plot$x),y=maxValue[["y"]]+padding,z=arbitaryValue)
  
  bound=rbind(bound1,bound2,bound3,bound4)
  df.plot = rbind(df.plot,bound)
  
  m = readPNG(paste0("../../figures/diagrams/png/clip_",cli,".png"), FALSE)
  w = matrix(rgb(m[,,1],m[,,2],m[,,3]), nrow=dim(m)[1])
  p = ggplot(df.plot)+
    annotation_custom(xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf, 
                      rasterGrob(w)) +
    stat_contour(aes(x=x,y=y,z=z, color=z,fill=..level..), geom="polygon",alpha=0.05,bins=50)+
    scale_fill_continuous(low = "green", high = "red") +
    # geom_point(data = df.tmp,aes(x=x,y=y),size=3,color = "black", alpha = 0.1) +
    coord_cartesian(xlim=c(0,1024),
                    ylim=c(0,768))+
    labs(x=NULL, y=NULL)+
    theme_void()+
    theme(
      axis.ticks.length = unit(0,"null"),
      legend.position = "none"
    )
  print(p)
}
