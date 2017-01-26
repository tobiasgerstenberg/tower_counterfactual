# Packages ----------------------------------------------------------------
rm(list=ls())

library(lsr)
library(reshape2)
library(ggplot2)
library(stringr)
library(rjson)
library(RSQLite)
library(nlme)
library(xtable)
library(lazyeval)
library(clues) #non-parametric clustering
library(pdfCluster) #another non-parametric clustering
library(V8)
library(MASS)
library(mclust) #clustering
library(gtools) #used for mixed sorting
library(grid)
library(corrr)
library(Hmisc)
library(dplyr)
library(tidyr)

# Misc functions ------------------------------------------------------------------------------

#RMSE 
rmse = function(x,y){
  return(sqrt(mean((x-y)^2)))
}

#simple regression
func_regression = function(x,formula){
  return(lm(formula,data=x)$fitted.values)
}

# ================== --------------------------------------------------------------------------
# EXP1: Load data, trial info, and model predictions ----------------------------------------------------------------------------
load("exp1_prediction.RData")
load("exp1_responsibility.RData")
load("exp1_selection.RData")
load("exp1_info.RData")
load("exp1_models.RData")
# load("brick_selection_predictions2.RData")

df.exp1.info = df.exp1.info %>% 
  filter(trial != 5)

df.exp1.models = df.exp1.models %>% 
  filter(trial != 5)


# EXP1: Demographic information ----------------------------------------------------------------------------
load("exp1_wide.RData")

df.exp1.wide = df.exp1.wide %>% 
  mutate_each(funs(as.numeric(.)),age,time)
  
df.exp1.wide %>% 
  group_by(experiment,condition) %>% 
  summarise(time.mean = mean(time), 
            time.sd = sd(time),
            exclude = sum(exclude)) %>% 
  mutate_at(vars(contains("time")),funs(round(.,2)))
  
df.exp1.wide$age %>% mean() %>% round()
df.exp1.wide$age %>% sd() %>% round(2)

df.exp1.wide %>% nrow()
(df.exp1.wide$sex == "female") %>% sum()

# EXP1: Find best fitting models --------------------------------------------------------------
# MINIMIZE LOG-LIKELIHOOD

df.exp1.selection.long = df.exp1.info %>% 
  select(trial,index) %>% 
  filter(index != 0) %>% 
  left_join(df.exp1.selection %>% select(participant,trial)) %>% 
  mutate(response = 0) %>% 
  select(participant,trial,index,response) %>% 
  arrange(participant,trial,index)

for (i in 1:nrow(df.exp1.selection)){
  tmp = df.exp1.selection[i,]
  df.exp1.selection.long$response[
    df.exp1.selection.long$participant == tmp$participant & 
      df.exp1.selection.long$trial == tmp$trial & 
      df.exp1.selection.long$index %in% (tmp$response %>% unlist())] = 1
}

df.exp1.likelihood = df.exp1.selection.long %>% 
  left_join(df.exp1.models %>% select(-c(x,y,angle))) %>% 
  left_join(df.exp1.info %>% select(index,trial,fall)) %>%
  rename(truth = fall) %>%
  mutate_each(funs(./100),contains("impulse")) %>%
  mutate_each(funs(ifelse(response == 1,.,1-.)),contains("impulse"),truth) %>%
  mutate_each(funs(ifelse(. == 1,0.99,.)),contains("impulse"),truth) %>%
  mutate_each(funs(ifelse(. == 0,0.01,.)),contains("impulse"),truth) %>%
  mutate_each(funs(log(.)),contains("impulse"),truth) %>%
  summarise_at(vars(contains("impulse"),truth),sum) %>%
  rename(truth_0_0 = truth) %>%
  gather(model,likelihood)

df.exp1.likelihood = df.exp1.likelihood %>% 
  cbind(df.exp1.likelihood$model %>% 
          str_split("_") %>% 
          unlist() %>% 
          matrix(ncol=3,byrow=T) %>% 
          as.data.frame(stringsAsFactors = F) %>% 
          setNames(c("noise_type","noise_amt","fric"))) %>% 
  select(model,noise_type,noise_amt,fric,likelihood)

df.exp1.bestmodels = df.exp1.likelihood %>% 
  arrange(desc(likelihood)) %>%
  group_by(noise_type) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  arrange(model)

df.exp1.bestmodels$model[which(df.exp1.bestmodels$model == 'truth_0_0')] = 'ground_truth'

df.exp1.models = df.exp1.models %>%
  rename(ground_truth = fall) %>%
  mutate(ground_truth = ground_truth*100)

# EXP1: Regression model ----------------------------------------------------------------------

tmp = fromJSON(file = "../../data/features_world1.json")
df.features = tmp %>% 
  unlist() %>%
  matrix(nrow=42,byrow=T) %>% 
  as.data.frame() %>% 
  setNames(tmp[[1]] %>% names()) %>% 
  mutate(trial = 1:nrow(.)) %>% 
  filter(trial != 5) %>% 
  left_join(df.exp1.info %>% mutate(y = as.numeric(y)) %>% group_by(trial) %>% summarise(nbricks = n())) %>% 
  left_join(df.exp1.responsibility %>% group_by(trial) %>% summarise(responsibility = mean(response))) %>% 
  select(trial,everything())

fit = lm(responsibility~
           # above_naive+
           # above_contact_general+
           # above_contact_selective+
           # edge_distance+
           # avg_x+
           avg_y+
           # avg_angle+
           # tower_height+
           nbricks,
         data = df.features)

tmp = fit %>% summary()
sqrt(tmp$r.squared)



# EXP1: Correlation between selection noise models  -------------------------------------------

tmp = df.exp1.models %>% 
  select(one_of(df.exp1.bestmodels$model)) %>% 
  correlate()

# EXP1: Table with model results --------------------------------------------------------------

df.exp1.table = df.exp1.selection.long %>% 
  group_by(trial,index) %>% 
  summarise(selection = mean(response*100)) %>% 
  ungroup() %>% 
  left_join(df.exp1.models %>% select(trial,index,one_of(df.exp1.bestmodels$model))) %>% 
  summarise_(r_above = interp(~cor(var1,var2),
                              var1 = as.name("selection"), 
                              var2 = as.name(df.exp1.bestmodels$model[df.exp1.bestmodels$noise_type == "impulse-local-above-extended"])),
             r_local = interp(~cor(var1,var2),
                              var1 = as.name("selection"), 
                              var2 = as.name(df.exp1.bestmodels$model[df.exp1.bestmodels$noise_type == "impulse-local"])),
             r_global = interp(~cor(var1,var2),
                              var1 = as.name("selection"), 
                              var2 = as.name(df.exp1.bestmodels$model[df.exp1.bestmodels$noise_type == "impulse-global"])),
             r_truth = interp(~cor(var1,var2),
                               var1 = as.name("selection"), 
                              var2 = as.name(df.exp1.bestmodels$model[df.exp1.bestmodels$noise_type == "truth"])),
             rmse_above = interp(~rmse(var1,var2),
                               var1 = as.name("selection"), 
                               var2 = as.name(df.exp1.bestmodels$model[df.exp1.bestmodels$noise_type == "impulse-local-above-extended"])),
             rmse_local = interp(~rmse(var1,var2),
                                 var1 = as.name("selection"), 
                                 var2 = as.name(df.exp1.bestmodels$model[df.exp1.bestmodels$noise_type == "impulse-local"])),
             rmse_global = interp(~rmse(var1,var2),
                                 var1 = as.name("selection"), 
                                 var2 = as.name(df.exp1.bestmodels$model[df.exp1.bestmodels$noise_type == "impulse-global"])),
             rmse_truth = interp(~rmse(var1,var2),
                              var1 = as.name("selection"), 
                              var2 = as.name(df.exp1.bestmodels$model[df.exp1.bestmodels$noise_type == "truth"]))
             ) %>% 
  wideToLong(within = 'model') %>% 
  select(-id)

df.exp1.table$noise_type = c("impulse-local-above-extended","impulse-local","impulse-global","truth")

df.exp1.table = df.exp1.table %>% 
  left_join(df.exp1.bestmodels %>% select(noise_type,likelihood,noise_amt)) %>% 
  select(-noise_type) %>% 
  rename(sigma = noise_amt) %>% 
  mutate(model = factor(model,levels = c('truth', 'global', 'local', 'above')),
         likelihood = round(likelihood)) %>% 
  mutate_each(funs(round(.,2)),r,rmse) %>% 
  arrange(model) %>% 
  xtable() %>% 
  print(include.rownames=F)
  
  
# EXP1: PLOT - Relationship between prediction and responsibility ------------------------------------

#add labels
df.labels = matrix(NA,ncol=3,nrow=41) %>% 
  as.data.frame() %>% 
  setNames(c("trial","label","colorindex")) %>% 
  mutate(trial = unique(df.exp1.info$trial))
df.labels$label[c(2,10,14,24,28,22,41)] = letters[2:8]
df.labels$colorindex = as.factor((!is.na(df.labels$label))*1)

df.plot = df.exp1.prediction %>% 
  rbind(df.exp1.responsibility) %>% 
  left_join(df.exp1.info %>% group_by(trial) %>% summarise(nbricks = n())) %>% 
  mutate(response = ifelse(condition == 'prediction',response/nbricks,response)) %>% #transforms predictions to proportion of bricks 
  group_by(condition,trial) %>% 
  summarise(mean = mean(response),
            low = smean.cl.boot(response)[2],
            high = smean.cl.boot(response)[3]) %>% 
  ungroup() %>%
  gather(variable,value,-c(trial,condition)) %>% 
  unite(tmp,condition,variable) %>% 
  spread(tmp,value) %>% 
  mutate(prediction_fit = func_regression(.,formula(responsibility_mean~prediction_mean))) %>% 
  left_join(df.labels)

model = 'prediction_fit'

ggplot(df.plot,aes_string(x=model,y="responsibility_mean"))+
  geom_smooth(method='lm',color='black')+
  geom_point(size=2.5,aes(color = colorindex))+
  geom_errorbar(aes(ymin = responsibility_low, ymax = responsibility_high,color = colorindex),width=0.01)+
  geom_text(aes(label=label),hjust=1.5,vjust=0.2,size = 7)+
  annotate(geom = "text", x=30,y=Inf,label = paste0("r = ", cor(df.plot[[model]],df.plot$responsibility_mean) %>% round(2)),
           hjust = 0, vjust = 1.5, size = 10)+
  annotate(geom = "text", x=30,y=Inf,label = paste0("RMSE = ", rmse(df.plot[[model]],df.plot$responsibility_mean) %>% round(2)),
           hjust = 0, vjust = 3, size = 10)+
  theme_bw()+
  scale_colour_grey(start = 0.5, end = 0)+
  scale_fill_grey(start = 0.5, end = 0)+
  scale_x_continuous(breaks = seq(0,100,10))+
  # scale_x_continuous(breaks = seq(0,100,20),limits = c(17,80))+
  scale_y_continuous(breaks = seq(0,100,10))+
  # scale_y_continuous(breaks = seq(0,100,20),limits = c(17,80))+
  labs(x = "proportion of bricks predicted to fall", y = "responsibility judgment")+
  theme(text = element_text(size=24),
        panel.grid = element_blank(),
        axis.title.x = element_text(margin = margin(t=10)),
        axis.title.y = element_text(margin = margin(r=10)),
        axis.text = element_text(size=20),
        legend.position = "none"
  )
ggsave(paste0('../../figures/plots/exp1_prediction_responsibility_scatter.pdf'),width = 8,height = 6)

# EXP1: PLOT - Relationship between selection and responsibility ------------------------------------

#add labels
df.labels = matrix(NA,ncol=3,nrow=41) %>% 
  as.data.frame() %>% 
  setNames(c("trial","label","colorindex")) %>% 
  mutate(trial = unique(df.exp1.info$trial))
df.labels$label[c(2,10,14,24,28,22,41)] = letters[2:8]
df.labels$colorindex = as.factor((!is.na(df.labels$label))*1)

df.plot = df.exp1.selection %>% 
  select(participant,trial,number) %>% 
  rename(response = number) %>% 
  mutate(condition = 'selection') %>% 
  rbind(df.exp1.responsibility %>% select(participant,condition,trial,response)) %>% 
  left_join(df.exp1.info %>% group_by(trial) %>% summarise(nbricks = n())) %>% 
  mutate(response = ifelse(condition == 'selection',response/nbricks,response)) %>% #transforms predictions to proportion of bricks
  group_by(condition,trial) %>% 
  summarise(mean = mean(response),
            low = smean.cl.boot(response)[2],
            high = smean.cl.boot(response)[3]) %>% 
  ungroup() %>%
  gather(variable,value,-c(trial,condition)) %>% 
  unite(tmp,condition,variable) %>% 
  spread(tmp,value) %>% 
  mutate(selection_fit = func_regression(.,formula(responsibility_mean~selection_mean))) %>% 
  left_join(df.labels)

model = 'selection_fit'

ggplot(df.plot,aes_string(x=model,y="responsibility_mean"))+
  geom_smooth(method='lm',color='black')+
  geom_errorbar(aes(ymin = responsibility_low, ymax = responsibility_high,color = colorindex),width=0.01)+
  # geom_errorbarh(aes(xmin = selection_low, xmax = selection_high,color = colorindex),width=0.01)+
  geom_point(size=2.5,aes(color = colorindex))+
  geom_point(size=2.5,aes(color = colorindex),data = subset(df.plot,!is.na(label)))+
  geom_text(aes(label=label),hjust=1.5,vjust=0.2,size = 6)+
  annotate(geom = "text", x=30,y=Inf,label = paste0("r = ", cor(df.plot[[model]],df.plot$responsibility_mean) %>% round(2)),
           hjust = 0, vjust = 1.5, size = 10)+
  annotate(geom = "text", x=30,y=Inf,label = paste0("RMSE = ", rmse(df.plot[[model]],df.plot$responsibility_mean) %>% round(2)),
           hjust = 0, vjust = 3, size = 10)+
  theme_bw()+
  scale_colour_grey(start = 0.5, end = 0)+
  scale_fill_grey(start = 0.5, end = 0)+
  scale_x_continuous(breaks = seq(0,100,10))+
  # scale_x_continuous(breaks = seq(0,100,20),limits = c(17,80))+
  scale_y_continuous(breaks = seq(0,100,10))+
  # scale_y_continuous(breaks = seq(0,100,20),limits = c(17,80))+
  labs(x = "proportion of bricks (empirical)", y = "responsibility")+
  theme(text = element_text(size=24),
        panel.grid = element_blank(),
        axis.title.x = element_text(margin = margin(t=10)),
        axis.title.y = element_text(margin = margin(r=10)),
        axis.text = element_text(size=20),
        legend.position = "none"
  )

# ggsave(paste0('../../figures/plots/exp1_prediction_responsibility_scatter.pdf'),width = 8,height = 6)



# EXP1: PLOT - Relationship between selection and prediction ------------------------------------

#add labels
df.labels = matrix(NA,ncol=3,nrow=41) %>% 
  as.data.frame() %>% 
  setNames(c("trial","label","colorindex")) %>% 
  mutate(trial = unique(df.exp1.info$trial))
df.labels$label[c(2,10,14,24,28,22,41)] = letters[2:8]
df.labels$colorindex = as.factor((!is.na(df.labels$label))*1)

df.plot = df.exp1.selection %>% 
  select(participant,trial,number) %>% 
  rename(response = number) %>% 
  mutate(condition = 'selection') %>% 
  rbind(df.exp1.prediction %>% select(participant,condition,trial,response)) %>% 
  # left_join(df.exp1.info %>% group_by(trial) %>% summarise(nbricks = n())) %>% 
  # mutate(response = ifelse(condition == 'selection',response/nbricks,response)) %>% #transforms predictions to proportion of bricks
  group_by(condition,trial) %>% 
  summarise(mean = mean(response),
            low = smean.cl.boot(response)[2],
            high = smean.cl.boot(response)[3]) %>% 
  ungroup() %>%
  gather(variable,value,-c(trial,condition)) %>% 
  unite(tmp,condition,variable) %>% 
  spread(tmp,value) %>% 
  # mutate(selection_fit = func_regression(.,formula(responsibility_mean~selection_mean))) %>% 
  left_join(df.labels)

# model = 'selection_fit'

ggplot(df.plot,aes(x=prediction_mean,y=selection_mean))+
  geom_abline(intercept = 0,slope = 1, linetype = 2)+
  geom_smooth(method='lm',color='black')+
  geom_errorbar(aes(ymin = selection_low, ymax = selection_high,color = colorindex),width=0)+
  geom_errorbarh(aes(xmin = prediction_low, xmax = prediction_high,color = colorindex),height=0)+
  geom_point(size=2.5,aes(color = colorindex))+
  geom_point(size=2.5,aes(color = colorindex),data = subset(df.plot,!is.na(label)))+
  geom_text(aes(label=label),hjust=1.5,vjust=0,size = 8)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("r = ", cor(df.plot$prediction_mean,df.plot$selection_mean) %>% round(2)),
           hjust = 0, vjust = 1.5, size = 10)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("RMSE = ", rmse(df.plot$prediction_mean,df.plot$selection_mean) %>% round(2)),
           hjust = 0, vjust = 3, size = 10)+
  theme_bw()+
  scale_colour_grey(start = 0.5, end = 0)+
  scale_fill_grey(start = 0.5, end = 0)+
  scale_x_continuous(breaks = seq(0,10,1),limits = c(0,8))+
  # scale_x_continuous(breaks = seq(0,100,20),limits = c(17,80))+
  scale_y_continuous(breaks = seq(0,10,1),limits = c(0,8))+
  # scale_y_continuous(breaks = seq(0,100,20),limits = c(17,80))+
  labs(x = expression(paste(bold("predicted")," number of bricks")), 
       y = expression(paste(bold("selected")," number of bricks")))+
  theme(text = element_text(size=24),
        panel.grid = element_blank(),
        axis.title.x = element_text(margin = margin(t=10)),
        axis.title.y = element_text(margin = margin(r=10)),
        axis.text = element_text(size=20),
        legend.position = "none"
  )

# ggsave(paste0('../../figures/plots/exp1_prediction_selection_scatter.pdf'),width = 8,height = 6)



# EXP1: PLOT - Relationship between selection and noise models -----------------------------

modelindex = 4 #1: global, 2: local, 3: local-above, 4: ground truth

df.modelindex = df.exp1.bestmodels %>% 
  mutate(modelname = c("global noise","local noise","above noise","ground truth"),
         label = c("b)","c)","d)","a)")) %>% 
  select(model,modelname,label)

df.plot = df.exp1.selection.long %>% 
  group_by(trial,index) %>% 
  summarise(data = (sum(response)/n())*100) %>% 
  ungroup %>% 
  left_join(df.exp1.models %>% select(trial,index,model = one_of(df.modelindex$model[modelindex])))
  
ggplot(df.plot,aes(x=data,y = model))+
  geom_smooth(method='lm',color='black',fullrange=T)+
  geom_abline(intercept=0, slope=1, linetype='dashed') +
  geom_point(size=3,alpha=0.5)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("r = ", cor(df.plot$data,df.plot$model) %>% round(2)),
           hjust = 0, vjust = 1.5, size = 8)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("RMSE = ", rmse(df.plot$data,df.plot$model) %>% round(2)),
           hjust = 0, vjust =3, size = 8)+
  scale_y_continuous(breaks = seq(0,100,25),labels = seq(0,100,25),limits = c(0,115))+
  scale_x_continuous(breaks = seq(0,100,25),labels = seq(0,100,25),limits = c(0,100))+
  theme_bw()+
  labs(y=bquote(.(df.modelindex$label[modelindex])~bold(.(df.modelindex$modelname[modelindex]))),
       x = "probability that a brick was selected")+
  theme(text = element_text(size=24),
        panel.grid = element_blank(),
        axis.title.x = element_text(margin = margin(t=10))
  )
ggsave(paste0("../../figures/plots/exp1_selection_",df.modelindex$model[modelindex],"_scatter.pdf"),width = 8,height = 6)

# EXP1: PLOT - Relationship between prediction and noise models -----------------------------

modelindex = 1 #1: local-above, 2: global, 3: local, 4: ground truth

df.modelindex = df.exp1.bestmodels %>% 
  mutate(modelname = c("above noise","global noise","local noise", "ground truth"),
         label = c("d)","b)","c)","a)")) %>% 
  select(model,modelname,label)
# df.modelindex[4,] = c('ground_truth', 'ground truth', 'a)')

df.plot = df.exp1.prediction %>% 
  group_by(trial) %>% 
  summarise(data = mean(response),
            low = smean.cl.boot(response)[2],
            high = smean.cl.boot(response)[3]) %>% 
  left_join(df.exp1.models %>% 
              select(trial,index,model = one_of(df.modelindex$model[modelindex])) %>% 
                       group_by(trial) %>% 
                       summarise(model = sum((model/100))))

ggplot(df.plot,aes(x=data,y = model))+
  geom_smooth(method='lm',color='black')+
  geom_abline(intercept=0, slope=1, linetype='dashed') +
  geom_point(size=3,alpha=0.5)+
  # geom_text(label = paste0(df.plot$trial,"[",df.plot$index,"]"),hjust = 0, vjust = 0, parse = T)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("r = ", cor(df.plot$data,df.plot$model) %>% round(2)),
           hjust = 0, vjust = 1.5, size = 8)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("RMSE = ", rmse(df.plot$data,df.plot$model) %>% round(2)),
           hjust = 0, vjust =3, size = 8)+
  scale_y_continuous(breaks = seq(0,10,1),labels = seq(0,10,1),limits = c(0,7))+
  scale_x_continuous(breaks = seq(0,10,1),labels = seq(0,10,1),limits = c(0,7))+
  # coord_fixed(xlim = c(0,8), ylim = c(0,8))+
  # ggtitle(modelindex)+
  theme_bw()+
  # labs(y="model prediction", x = "empirical selections")+
  labs(y=bquote(.(df.modelindex$label[modelindex])~bold(.(df.modelindex$modelname[modelindex]))),
       x = "number of bricks predicted to fall")+
  theme(text = element_text(size=24),
        panel.grid = element_blank(),
        axis.title.x = element_text(margin = margin(t=10))
  )
# ggsave(paste0("../../figures/plots/exp1_selection_",df.modelindex$model[modelindex],"_scatter.pdf"),width = 8,height = 6)

# EXP1: PLOT - Relationship between prediction and noise models -----------------------------

modelindex = 1 #1: local-above, 2: global, 3: local, 4: ground truth

df.modelindex = df.exp1.bestmodels %>% 
  mutate(modelname = c("above noise","global noise","local noise", "ground truth"),
         label = c("d)","b)","c)","a)")) %>% 
  select(model,modelname,label)
# df.modelindex[4,] = c('ground_truth', 'ground truth', 'a)')

df.plot = df.exp1.responsibility %>% 
  group_by(trial) %>% 
  summarise(data = mean(response),
            low = smean.cl.boot(response)[2],
            high = smean.cl.boot(response)[3]) %>% 
  ungroup() %>% 
  left_join(df.exp1.models %>% 
              select(trial,index,model = one_of(df.modelindex$model[modelindex])) %>% 
              group_by(trial) %>% 
              summarise(model = sum((model/100)))) %>% 
  left_join(df.exp1.info %>% group_by(trial) %>% summarise(nbricks = n())) %>% 
  mutate(model = model/nbricks) %>% #transforms predictions to proportion of bricks 
  select(-nbricks) %>% 
  mutate(model = func_regression(.,formula(data~model))) %>% 
  left_join(df.labels)
  

ggplot(df.plot,aes(x=data,y = model))+
  geom_smooth(method='lm',color='black')+
  # geom_abline(intercept=0, slope=1, linetype='dashed') +
  geom_point(size=3,alpha=0.5)+
  # geom_text(label = paste0(df.plot$trial,"[",df.plot$index,"]"),hjust = 0, vjust = 0, parse = T)+
  annotate(geom = "text", x=-Inf,y=Inf,label = paste0("r = ", cor(df.plot$data,df.plot$model) %>% round(2)),
           hjust = 0, vjust = 1.5, size = 8)+
  annotate(geom = "text", x=-Inf,y=Inf,label = paste0("RMSE = ", rmse(df.plot$data,df.plot$model) %>% round(2)),
           hjust = 0, vjust =3, size = 8)+
  # scale_y_continuous(breaks = seq(0,10,1),labels = seq(0,10,1),limits = c(0,7))+
  # scale_x_continuous(breaks = seq(0,10,1),labels = seq(0,10,1),limits = c(0,7))+
  # coord_fixed(xlim = c(0,8), ylim = c(0,8))+
  # ggtitle(modelindex)+
  theme_bw()+
  # labs(y="model prediction", x = "empirical selections")+
  labs(y=bquote(.(df.modelindex$label[modelindex])~bold(.(df.modelindex$modelname[modelindex]))),
       x = "number of bricks predicted to fall")+
  theme(text = element_text(size=24),
        panel.grid = element_blank(),
        axis.title.x = element_text(margin = margin(t=10))
  )
# ggsave(paste0("../../figures/plots/exp1_selection_",df.modelindex$model[modelindex],"_scatter.pdf"),width = 8,height = 6)


# ================== --------------------------------------------------------------------------
# EXP2: Load data, trial info, and model predictions ------------------------------------------
load("exp2_prediction.RData")
load("exp2_responsibility.RData")
load("exp2_selection.RData")
load("exp2_info.RData")
load("exp2_models.RData")


# EXP2: Demographic information ----------------------------------------------------------------------------
load("exp2_wide.RData")

df.exp2.wide = df.exp2.wide %>% 
  mutate_each(funs(as.numeric(.)),age,time)

df.exp2.wide %>% 
  group_by(experiment,condition) %>% 
  summarise(time.mean = mean(time), 
            time.sd = sd(time),
            exclude = sum(exclude),
            n = n()) %>% 
  mutate_at(vars(contains("time")),funs(round(.,2)))

df.exp2.wide$age %>% mean() %>% round()
df.exp2.wide$age %>% sd() %>% round(2)

df.exp2.wide %>% nrow()
(df.exp2.wide$sex == "female") %>% sum()

# EXP2: Find best fitting models --------------------------------------------------------------
# MINIMIZE LOG-LIKELIHOOD

df.exp2.selection.long = df.exp2.info %>% 
  select(trial,index) %>% 
  filter(index != 0) %>% 
  left_join(df.exp2.selection %>% select(participant,trial)) %>% 
  mutate(response = 0) %>% 
  select(participant,trial,index,response) %>% 
  arrange(participant,trial,index)

for (i in 1:nrow(df.exp2.selection)){
  tmp = df.exp2.selection[i,]
  df.exp2.selection.long$response[
    df.exp2.selection.long$participant == tmp$participant & 
      df.exp2.selection.long$trial == tmp$trial & 
      df.exp2.selection.long$index %in% (tmp$response %>% unlist())] = 1
}

df.exp2.likelihood = df.exp2.selection.long %>% 
  left_join(df.exp2.models %>% select(-c(x,y,angle))) %>% 
  left_join(df.exp2.info %>% select(index,trial,fall)) %>%
  rename(truth = fall) %>%
  mutate_each(funs(./100),contains("impulse")) %>% 
  mutate_each(funs(ifelse(response == 1,.,1-.)),contains("impulse"),truth) %>%
  mutate_each(funs(ifelse(. == 1,0.99,.)),contains("impulse"),truth) %>%
  mutate_each(funs(ifelse(. == 0,0.01,.)),contains("impulse"),truth) %>%
  mutate_each(funs(log(.)),contains("impulse"),truth) %>%
  summarise_at(vars(contains("impulse"),truth),sum) %>%
  rename(truth_0_0 = truth) %>%
  gather(model,likelihood)

df.exp2.likelihood = df.exp2.likelihood %>% 
  cbind(df.exp2.likelihood$model %>%
          str_split("_") %>% 
          unlist() %>% 
          matrix(ncol=3,byrow=T) %>% 
          as.data.frame(stringsAsFactors = F) %>% 
          setNames(c("noise_type","noise_amt","fric"))) %>% 
  select(model,noise_type,noise_amt,fric,likelihood)

df.exp2.bestmodels = df.exp2.likelihood %>% 
  arrange(desc(likelihood)) %>%
  group_by(noise_type) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  arrange(model)
df.exp2.bestmodels$model[which(df.exp2.bestmodels$model == 'truth_0_0')] = 'ground_truth'

df.exp2.models = df.exp2.models %>%
  rename(ground_truth = fall) %>%
  mutate(ground_truth = ground_truth*100)


# EXP1: Regression model ----------------------------------------------------------------------

tmp = fromJSON(file = "../../data/features_world2.json")
df.features = tmp %>% 
  unlist() %>%
  matrix(nrow=42,byrow=T) %>% 
  as.data.frame() %>% 
  setNames(tmp[[1]] %>% names()) %>% 
  mutate(trial = 1:nrow(.)) %>% 
  left_join(df.exp2.info %>% mutate(y = as.numeric(y)) %>% group_by(trial) %>% summarise(nbricks = n())) %>% 
  left_join(df.exp2.responsibility %>% group_by(trial) %>% summarise(responsibility = mean(response))) %>% 
  select(trial,everything())

fit = lm(responsibility~
           # above_naive+
           # above_contact_general+
           above_contact_selective+
           edge_distance+
           # avg_x+
           # avg_y+
           # avg_angle+
           # tower_height+
           # nbricks,
         data = df.features)

tmp = fit %>% summary()
sqrt(tmp$r.squared)

# EXP2: Table with model results --------------------------------------------------------------

df.exp2.table = df.exp2.selection.long %>% 
  group_by(trial,index) %>% 
  summarise(selection = mean(response*100)) %>% 
  ungroup() %>% 
  left_join(df.exp2.models %>% select(trial,index,one_of(df.exp2.bestmodels$model))) %>% 
  summarise_(r_above = interp(~cor(var1,var2),
                              var1 = as.name("selection"), 
                              var2 = as.name(df.exp2.bestmodels$model[df.exp2.bestmodels$noise_type == "impulse-local-above-extended"])),
             r_local = interp(~cor(var1,var2),
                              var1 = as.name("selection"), 
                              var2 = as.name(df.exp2.bestmodels$model[df.exp2.bestmodels$noise_type == "impulse-local"])),
             r_global = interp(~cor(var1,var2),
                               var1 = as.name("selection"), 
                               var2 = as.name(df.exp2.bestmodels$model[df.exp2.bestmodels$noise_type == "impulse-global"])),
             r_truth = interp(~cor(var1,var2),
                              var1 = as.name("selection"), 
                              var2 = as.name(df.exp2.bestmodels$model[df.exp2.bestmodels$noise_type == "truth"])),
             rmse_above = interp(~rmse(var1,var2),
                                 var1 = as.name("selection"), 
                                 var2 = as.name(df.exp2.bestmodels$model[df.exp2.bestmodels$noise_type == "impulse-local-above-extended"])),
             rmse_local = interp(~rmse(var1,var2),
                                 var1 = as.name("selection"), 
                                 var2 = as.name(df.exp2.bestmodels$model[df.exp2.bestmodels$noise_type == "impulse-local"])),
             rmse_global = interp(~rmse(var1,var2),
                                  var1 = as.name("selection"), 
                                  var2 = as.name(df.exp2.bestmodels$model[df.exp2.bestmodels$noise_type == "impulse-global"])),
             rmse_truth = interp(~rmse(var1,var2),
                                 var1 = as.name("selection"), 
                                 var2 = as.name(df.exp2.bestmodels$model[df.exp2.bestmodels$noise_type == "truth"]))
  ) %>% 
  wideToLong(within = 'model') %>% 
  select(-id)

df.exp2.table$noise_type = c("impulse-local-above-extended","impulse-local","impulse-global","truth")

df.exp2.table = df.exp2.table %>% 
  left_join(df.exp2.bestmodels %>% select(noise_type,likelihood,noise_amt)) %>% 
  select(-noise_type) %>% 
  rename(sigma = noise_amt) %>% 
  mutate(model = factor(model,levels = c('truth', 'global', 'local', 'above')),
         likelihood = round(likelihood)) %>% 
  mutate_each(funs(round(.,2)),r,rmse) %>% 
  arrange(model) %>% 
  xtable() %>% 
  print(include.rownames=F)

# EXP2: Correlation between selection noise models  -------------------------------------------

tmp = df.exp2.models %>% 
  # filter(trial %in% 15:28) %>%
  # group_by(trial) %>% 
  # select(trial,one_of(c(df.exp2.bestmodels$model,"ground_truth"))) %>%
  select(one_of(c(df.exp2.bestmodels$model,"ground_truth"))) %>%
  # summarise(tmp = cor(`impulse-local-above-extended_10.4_0.5`,`impulse-global_3.6_0.5`)) %>% 
  correlate() %>% 
  print()

# EXP2: PLOT - Relationship between prediction and responsibility ------------------------------------

# here, taking the proportion of bricks actually works slightly less good 

#add labels
df.labels = matrix(NA,ncol=3,nrow=42) %>% 
  as.data.frame() %>% 
  setNames(c("trial","label","colorindex")) %>% 
  mutate(trial = unique(df.exp2.info$trial))
df.labels$label[c(8,2,17,18,26,33,42,21)] = letters[1:8]
df.labels$colorindex = as.factor((!is.na(df.labels$label))*1)

df.plot = df.exp2.prediction %>% 
  rbind(df.exp2.responsibility) %>% 
  left_join(df.exp2.info %>% group_by(trial) %>% summarise(nbricks = n())) %>% 
  mutate(response = ifelse(condition == 'prediction',response/nbricks,response)) %>% #transforms predictions to proportion of bricks
  group_by(condition,trial) %>% 
  summarise(mean = mean(response),
            low = smean.cl.boot(response)[2],
            high = smean.cl.boot(response)[3]) %>% 
  ungroup() %>%
  gather(variable,value,-c(trial,condition)) %>% 
  unite(tmp,condition,variable) %>% 
  spread(tmp,value) %>% 
  mutate(prediction_fit = func_regression(.,formula(responsibility_mean~prediction_mean))) %>% 
  left_join(df.labels)

model = 'prediction_fit'

ggplot(df.plot,aes_string(x=model,y="responsibility_mean"))+
  geom_smooth(method='lm',color='black')+
  geom_point(size=2.5,aes(color = colorindex))+
  geom_errorbar(aes(ymin = responsibility_low, ymax = responsibility_high,color = colorindex),width=0.01)+
  geom_text(aes(label=label),hjust=1.5,vjust=0.2,size = 7)+
  annotate(geom = "text", x=15,y=Inf,label = paste0("r = ", cor(df.plot[[model]],df.plot$responsibility_mean) %>% round(2)),
           hjust = 0, vjust = 1.5, size = 10)+
  annotate(geom = "text", x=15,y=Inf,label = paste0("RMSE = ", rmse(df.plot[[model]],df.plot$responsibility_mean) %>% round(2)),
           hjust = 0, vjust = 3, size = 10)+
  theme_bw()+
  scale_colour_grey(start = 0.5, end = 0)+
  scale_fill_grey(start = 0.5, end = 0)+
  scale_x_continuous(breaks = seq(0,100,10))+
  # scale_x_continuous(breaks = seq(0,100,20),limits = c(17,80))+
  scale_y_continuous(breaks = seq(0,100,10))+
  # scale_y_continuous(breaks = seq(0,100,20),limits = c(17,80))+
  labs(x = "proportion of bricks predicted to fall", y = "responsibility judgment")+
  theme(text = element_text(size=24),
        panel.grid = element_blank(),
        axis.title.x = element_text(margin = margin(t=10)),
        axis.title.y = element_text(margin = margin(r=10)),
        axis.text = element_text(size=20),
        legend.position = "none"
  )
ggsave(paste0('../../figures/plots/exp2_prediction_responsibility_scatter.pdf'),width = 8,height = 6)

# EXP2: PLOT - Relationship between selection and noise models -----------------------------

modelindex = 4 #1: global, 2: local, 3: local-above, 4: ground truth

df.modelindex = df.exp2.bestmodels %>% 
  mutate(modelname = c("global noise","local noise","above noise","ground truth"),
         label = c("b)","c)","d)","a)")) %>% 
  select(model,modelname,label)

df.plot = df.exp2.selection.long %>% 
  group_by(trial,index) %>% 
  summarise(data = (sum(response)/n())*100) %>% 
  ungroup %>% 
  left_join(df.exp2.models %>% select(trial,index,model = one_of(df.modelindex$model[modelindex])))

ggplot(df.plot,aes(x=data,y = model))+
  geom_smooth(method='lm',color='black')+
  geom_abline(intercept=0, slope=1, linetype='dashed') +
  geom_point(size=3,alpha=0.5)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("r = ", cor(df.plot$data,df.plot$model) %>% round(2)),
           hjust = 0, vjust = 1.5, size = 8)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("RMSE = ", rmse(df.plot$data,df.plot$model) %>% round(2)),
           hjust = 0, vjust =3, size = 8)+
  scale_y_continuous(breaks = seq(0,100,25),labels = seq(0,100,25),limits = c(0,115))+
  scale_x_continuous(breaks = seq(0,100,25),labels = seq(0,100,25),limits = c(0,100))+
  theme_bw()+
  labs(y=bquote(.(df.modelindex$label[modelindex])~bold(.(df.modelindex$modelname[modelindex]))),
       x = "probability that a brick was selected")+
  theme(text = element_text(size=24),
        panel.grid = element_blank(),
        axis.title.x = element_text(margin = margin(t=10))
  )
ggsave(paste0("../../figures/plots/exp2_selection_",df.modelindex$model[modelindex],"_scatter.pdf"),width = 8,height = 6)


# EXP2: PLOT - Relationship between selection and prediction ------------------------------------

#add labels
df.labels = matrix(NA,ncol=3,nrow=42) %>% 
  as.data.frame() %>% 
  setNames(c("trial","label","colorindex")) %>% 
  mutate(trial = unique(df.exp2.info$trial))
df.labels$label[c(8,2,17,18,26,33,42,21)] = letters[1:8]
df.labels$colorindex = as.factor((!is.na(df.labels$label))*1)

df.plot = df.exp2.selection %>% 
  select(participant,trial,number) %>% 
  rename(response = number) %>% 
  mutate(condition = 'selection') %>% 
  rbind(df.exp2.prediction %>% select(participant,condition,trial,response)) %>% 
  # left_join(df.exp2.info %>% group_by(trial) %>% summarise(nbricks = n())) %>% 
  # mutate(response = ifelse(condition == 'selection',response/nbricks,response)) %>% #transforms predictions to proportion of bricks
  group_by(condition,trial) %>% 
  summarise(mean = mean(response),
            low = smean.cl.boot(response)[2],
            high = smean.cl.boot(response)[3]) %>% 
  ungroup() %>%
  gather(variable,value,-c(trial,condition)) %>% 
  unite(tmp,condition,variable) %>% 
  spread(tmp,value) %>% 
  # mutate(selection_fit = func_regression(.,formula(responsibility_mean~selection_mean))) %>% 
  left_join(df.labels)

# model = 'selection_fit'

ggplot(df.plot,aes(x=prediction_mean,y=selection_mean))+
  geom_abline(intercept = 0,slope = 1, linetype = 2)+
  geom_smooth(method='lm',color='black')+
  geom_errorbar(aes(ymin = selection_low, ymax = selection_high,color = colorindex),width=0)+
  geom_errorbarh(aes(xmin = prediction_low, xmax = prediction_high,color = colorindex),height=0)+
  geom_point(size=2.5,aes(color = colorindex))+
  geom_point(size=2.5,aes(color = colorindex),data = subset(df.plot,!is.na(label)))+
  geom_text(aes(label=label),hjust=1.5,vjust=0,size = 8)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("r = ", cor(df.plot$prediction_mean,df.plot$selection_mean) %>% round(2)),
           hjust = 0, vjust = 1.5, size = 10)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("RMSE = ", rmse(df.plot$prediction_mean,df.plot$selection_mean) %>% round(2)),
           hjust = 0, vjust = 3, size = 10)+
  theme_bw()+
  scale_colour_grey(start = 0.5, end = 0)+
  scale_fill_grey(start = 0.5, end = 0)+
  scale_x_continuous(breaks = seq(0,10,1),limits = c(0,8))+
  # scale_x_continuous(breaks = seq(0,100,20),limits = c(17,80))+
  scale_y_continuous(breaks = seq(0,10,1),limits = c(0,8))+
  # scale_y_continuous(breaks = seq(0,100,20),limits = c(17,80))+
  labs(x = expression(paste(bold("predicted")," number of bricks")), 
       y = expression(paste(bold("selected")," number of bricks")))+
  theme(text = element_text(size=24),
        panel.grid = element_blank(),
        axis.title.x = element_text(margin = margin(t=10)),
        axis.title.y = element_text(margin = margin(r=10)),
        axis.text = element_text(size=20),
        legend.position = "none"
  )

ggsave(paste0('../../figures/plots/exp2_prediction_selection_scatter.pdf'),width = 8,height = 6)


