# To do list  ---------------------------------------------------------------------------------

# look at relationship between selections and responsibility judgments 


# Packages ----------------------------------------------------------------
rm(list=ls())

library(lsr)
library(reshape2)
library(ggplot2)
library(stringr)
library(rjson)
library(RSQLite)
library(nlme)
library(stargazer)
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

# get the ground truth
# tmp = fromJSON(file = "../../data/ground_truth_exp2_3.json")
# ground_truth = matrix(nrow = 0, ncol = 2) %>%
#   as.data.frame()
# for (i in 1:length(tmp)){
#   tmp2 = tmp[[i]] %>% unlist()
#   ground_truth  = rbind(ground_truth,matrix(c(rep(i,length(tmp2)),tmp2),ncol=2))
# }
# ground_truth = ground_truth %>% setNames(c("trial", "index")) %>% 
#   mutate(fall = 1)


# ========================= -------------------------------------------------------------------

# EXP1: Read data ---------------------------------------------------------------

con = dbConnect(SQLite(),dbname = "../javascript/experiment_1/participants.db");
# con = dbConnect(SQLite(),dbname = "../java/participants.db");
df.complete = dbReadTable(con,"towers")
dbDisconnect(con)

#filter out incompletes 
df.complete = subset(df.complete,status %in% c(3,4)) 
df.complete.experiment_1 = subset(df.complete,codeversion == "experiment_1")

# EXP1: Model  --------------------------------------------------------------------------------------

list.model = fromJSON(file = "../../data/model_exp1/ground_truth.json")
list.model.how = fromJSON(file = "../../data/model_exp1/ground_truth_how.json")

df.model = as.data.frame(matrix(nrow = 100, ncol = 4))
colnames(df.model) = c("id","before","after","how")

for (i in 1:length(list.model)){
  df.model[i,] = c(list.model[[i]],list.model.how[[i]][2])
}

df.model = df.model %>% 
  filter(id %in% c(1,2,4,5,6,7,8,13,14,15,16,17,18,19,21,22,23,25,26,27,28,29,30,31,32,33,34,35,36,37)) %>% 
  mutate(ratio = 1-(after/before),
         difference = before - after,
         image = id,
         id = as.factor(1:nrow(.)),
         ratio.how = how/before,
         ratioZ = as.numeric(scale(ratio)),
         differenceZ = as.numeric(scale(difference)),
         difference.howZ = as.numeric(scale(how)),
         ratio.howZ = as.numeric(scale(ratio.how))
         )

a = fromJSON(file = "../../data/model_exp1/ground_truth_wiggle.json")
df.model.wiggle = as.data.frame(matrix(nrow = length(a), ncol = 5))
colnames(df.model.wiggle) = c("id","simulation","before","after","how")
for (i in 1:nrow(df.model.wiggle)){
  df.model.wiggle[i,] = a[[i]]
}

df.model.wiggle = df.model.wiggle %>% 
  group_by(id) %>% 
  summarise(before = mean(before), 
            after = mean(after),
            how = mean(how)) %>% 
  ungroup(.) %>% 
  mutate(ratio = 1-(after/before),
         difference = before - after,
         ratio.how = how/before,
         ratioZ = as.numeric(scale(ratio)),
         differenceZ = as.numeric(scale(difference)),
         difference.howZ = as.numeric(scale(how)),
         ratio.howZ = as.numeric(scale(ratio.how)),
         image = id,
         id = 1:30)

# EXP1: Structure data ----------------------------------------------------------
df.experiment = df.complete.experiment_1
ntrials = 30
nvariables = 3
variables.unstructured = names(fromJSON(df.experiment$datastring[1])$questiondata)
variables.additional = c("experiment","participant","workerid","taskid","exclude","time")
variables.task = paste(c("id","image","rating"),"_",rep(1:ntrials,each=nvariables),sep="")
df.wide = data.frame(matrix(nrow=nrow(df.experiment),ncol=length(variables.unstructured)+
                              length(variables.additional)+length(variables.task)))
colnames(df.wide) = c(variables.additional,variables.task,variables.unstructured)

for (i in 1:nrow(df.wide)){
  a = fromJSON(df.experiment$datastring[i])
  # additional variables
  df.wide$experiment[i] = df.experiment$codeversion[i]
  df.wide$participant[i] = i
  df.wide$workerid[i] = a$workerId
  df.wide$taskid[i] = a$assignmentId
  df.wide$time[i] = as.numeric(as.POSIXct(df.experiment$endhit[i], format = "%Y-%m-%d %H:%M:%S")-
                                 as.POSIXct(df.experiment$beginhit[i], format = "%Y-%m-%d %H:%M:%S"))
  # unstructured variables 
  for (j in 1:length(variables.unstructured)){
    df.wide[[variables.unstructured[j]]][i] = a[["questiondata"]][[variables.unstructured[j]]]
  }
  
  # task variables 
  l = 1
  for (j in 1:ntrials){
    for (k in seq(2,by = 2, length.out = nvariables)){
      if (is.null(a$data[[j]]$trialdata[[k]])){
        b = NA;
      }else{
        b = a$data[[j]]$trialdata[[k]]
      }
      df.wide[[variables.task[l]]][i] = b
      l = l+1
    }
  }
}

df.wide = df.wide %>% 
  mutate(exclude = ifelse(color != "red and gray",1,0))

#wide to long 
df.long = wideToLong(df.wide,"trial")
df.long =  df.long %>% 
  select(participant,trial,id,image,rating) %>% 
  mutate(id = as.factor(id))

#merge with model predictions 
# df.long = merge(df.long,select(df.model.wiggle,id,difference,how,ratio,ratio.how,
#                                differenceZ,ratioZ,difference.howZ,ratio.howZ))

# ground truth model 
df.long = merge(df.long,select(df.model,id,difference,ratio,how,ratio.how,
                               differenceZ,ratioZ,difference.howZ,ratio.howZ))


df.long = df.long %>% 
  arrange(participant,id)

#z-score (on the level of individual participants)
df.long = df.long %>% 
  group_by(participant) %>%
  mutate(ratingZ = (rating - mean(rating))/sd(rating)) %>% 
  ungroup(.)

# EXP1: Regression models  --------------------------------------------------------------------

df.regression = df.long %>% 
  # filter(id %in% setdiff(1:30, c(21,30,11,9))) %>% 
  group_by(id) %>% 
  # summarise(data = mean(ratingZ),
  #           ratio = mean(ratioZ),
  #           difference = mean(differenceZ),
  #           difference.how = mean(difference.howZ),
  #           ratio.how = mean(ratio.howZ)) %>% 
  summarise(data = mean(rating),
            ratio = mean(ratio),
            difference = mean(difference),
            how = mean(how),
            # difference.how = mean(difference.how),
            ratio.how = mean(ratio.how)
            ) %>%
  ungroup(.)

fit1 = lm(data~difference,df.regression)
# summary(fit)

# fit = lm(data~ratio+ratio.how,df.regression)
# summary(fit)

fit2 = lm(data~difference+how,df.regression)
# summary(fit)

stargazer(fit)

# fit = lm(data~ratio,df.regression)
# summary(fit)

# fit = lm(data~difference.how,df.regression)
# summary(fit)
# 
# fit = lm(data~ratio.how+difference+ratio.how.truth,df.regression)
# summary(fit)

# fit = lm(data~ratio.how,df.regression)
# summary(fit)

# fit = lm(data~ratio.how+ratio,df.regression)
# summary(fit)

df.regression = df.regression %>% 
  mutate(fit1 = fit1$fitted.values %>% as.numeric(),
         fit2 = fit2$fitted.values %>% as.numeric())

df.fit = data.frame(id = df.regression$id, fit1 = fit1$fitted.values, fit2 = fit2$fitted.values)
df.long = merge(df.long,df.fit)

# fit = lm(data~difference+how,df.regression)
# summary(fit)
# 
# fit = lm(data~how,df.regression)
# summary(fit)

# EXP1: Bar graph -----------------------------------------------------------------------------

df.plot = df.long 

ggplot(df.plot,aes(x = id, y = rating))+
  stat_summary(fun.y = mean, geom = "bar",size=1, position = position_dodge(0.7), aes(width = 0.5), 
               color = "black", fill = "gray80")+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",width=0.3, position = position_dodge(0.7))+
  # facet_grid(balls~outcome)+
  labs(x="Trial",y = "Responsibility",
       fill ="Outcome")+
  scale_y_continuous(breaks=seq(0,100,25),labels=seq(0,100,25),limits=c(0,100))+
  theme_bw()+
  theme(text = element_text(size = 20),
        panel.grid = element_blank(),
        legend.position = "none")
# ggsave(paste("../../figures/plots/towers_bars.pdf",sep=""),width=10,height=6)

# EXP1: Bar graph (with model predictions) ---------------------------------------------------------

# modelfilter = c("ratingZ","ratioZ")
# modelfilter = c("ratingZ","differenceZ")
# modelfilter = c("ratingZ","differenceZ","ratioZ")
# modelfilter = c("ratingZ","ratioZ","difference.howZ", "ratio.howZ", "fit")
modelfilter = c("rating","fit1", "fit2")

df.plot = df.long %>% 
  select(participant,id,rating, ratingZ,ratioZ,differenceZ,difference.howZ,ratio.howZ, fit1, fit2) %>%
  gather(index,rating,-c(id,participant)) %>% 
  filter(index %in% modelfilter) %>% 
  mutate(index = factor(index,levels = modelfilter))

ggplot(df.plot,aes(x = id, y = rating, fill = index))+
  stat_summary(fun.y = mean, geom = "bar",size=1, position = position_dodge(0.7), aes(width = 0.7), 
               color = "black")+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",width=0.3, position = position_dodge(0.7))+
  # facet_grid(balls~outcome)+
  labs(x="Trial",y = "Responsibility",
       fill ="")+
  # scale_y_continuous(breaks=seq(0,100,25),labels=seq(0,100,25),limits=c(0,100))+
  scale_fill_manual(values = c("white","gray40","gray80"))+
  theme_bw()+
  theme(text = element_text(size = 20),
        panel.grid = element_blank(),
        legend.position = "bottom")
# ggsave(paste("../../figures/plots/towers_model_bars.pdf",sep=""),width=10,height=6)

# EXP1: Scatter Plot --------------------------------------------------------------------------

df.plot = df.regression
model = "fit1"
model = "fit2"

ggplot(df.plot,aes_string(x = "data", y = model))+
  geom_smooth(method=lm,color = "black")+
  geom_point(size = 3)+
  geom_text(aes(label = id),vjust=-0.5,hjust=1)+
  labs(x="Data",y = "Model")+
  annotate(geom="text",label = paste0("r = ", cor(df.regression[[model]],df.regression$data) %>% round(2)),
           x = -Inf, y = Inf, hjust = -1, vjust = 2, size = 10)+
  scale_x_continuous(breaks = seq(0,100,10),lim = c(10,90))+
  scale_y_continuous(breaks = seq(0,100,10),lim = c(10,90))+
  theme_bw()+
  theme(text = element_text(size = 20),
        panel.grid = element_blank(),
        legend.position = "bottom")
ggsave(paste("../../figures/plots/towers_model_how_scatter.pdf",sep=""),width=10,height=6)
# ggsave(paste("../../figures/plots/towers_model_whether_scatter.pdf",sep=""),width=10,height=6)
  

# EXP1: Analyses ------------------------------------------------------------------------------

df.summaryZ = df.long %>% 
  group_by(id) %>% 
  summarise(data = mean(ratingZ),
            model.ratio = mean(ratioZ),
            model.difference = mean(differenceZ))

df.summary = df.long %>% 
  group_by(id) %>% 
  summarise(data = mean(rating),
            sem = sd(rating/sqrt(length(rating))),
            sd = sd(rating))

fit = lm(data~model.ratio,df.summary)
summary(fit)

fit = lm(data~model.difference,df.summary)
summary(fit)

cor(df.model.wiggle$how,df.model$how)

# EXP1: Write results table  ------------------------------------------------------------------

df.tmp = df.long %>% 
  select(participant, image, rating)

write.csv(df.long, file = "../../data/data_exp1.csv" )

# ========================= -------------------------------------------------------------------

# EXP2: Process stimuli ----------------------------------------------------------------------

# Generate table with stimuli information 

files = list.files("../javascript/stimuli/data/")
stimuli = fromJSON(file=paste0("../javascript/stimuli/data/",files[str_detect(files,"table")]))

df.stimuli = matrix(NA,ncol = length(stimuli[[1]])+1,nrow = length(stimuli)) %>% 
  as.data.frame() %>% 
  setNames(c("name","initial","final_ground","final_mean","final_var","model","level")) %>% 
  mutate(name = names(stimuli))

for(i in 1:length(stimuli)){
  df.stimuli[i,-1] = stimuli[[i]] %>% unlist()
}

df.stimuli = df.stimuli %>% 
  mutate_each(funs(as.numeric(.)),initial,final_ground,final_mean,final_var) %>% 
  mutate(difference_ground = initial-final_ground,
         difference_noise = initial - final_mean
         )

# EXP2: Make selection (save json and create images) ----------------------------------------

df.selection = df.stimuli %>%
  group_by(difference_ground) %>%
  filter(row_number() <= 6) %>%
  filter(difference_ground <= 7) %>%
  arrange(difference_ground,initial) %>% 
  mutate(index = 1:n()) %>% 
  ungroup() %>% 
  mutate(trial_name = paste0('trial_',row_number(),'_diff_',difference_ground,'_index_',index))
# write.csv(df.selection, file = "stimuli.csv",row.names = F)

#write json 
# for (i in 1:nrow(df.selection)){
#   l.json = list(ID = i,
#                 image = df.selection$trial_name[i],
#                 bricks = df.selection$initial[i]-1)
#   write(toJSON(l.json),"stim.json",append=T)
# }

# # generate images from json files
# images = files[str_detect(files,"initial")] %>%  #based on json files 
#   str_extract("[[:digit:]]+") %>% 
#   paste(collapse=",") %>% 
#   write(file = 'images.txt')
# 

# generate images from table
# df.selection %>%
#   select(name) %>%
#   unlist() %>%
#   paste(collapse=",") %>%
#   write(file = 'selection.txt')

df.info = df.selection %>%
  select(name,initial,final_ground,difference_ground,index,trial_name) %>%
  rename(final = final_ground,
         difference = difference_ground) %>%
  mutate(trial = 1:nrow(.))

# save("df.info",file = "trialinfo.RData")

# ========================= -------------------------------------------------------------------
# Model predictions: EXP2 and EXP3  ---------------------------------------------------------------------------

# load("brick_selection_predictions.RData")

load("detailed_trial_info.RData")

location = "../../code/javascript/node/sim_archives/simulations12" #location of the simulations to be read in
filenames = list.files(location) %>% mixedsort()
nmodels = length(filenames)

df.percentages.prediction = df.info.detailed
nsims = 100 #number of simulations

for (f in 1:length(filenames)){
  #container data frame
  tmp = matrix(nrow = 0, ncol = 3) %>%
    as.data.frame() %>%
    setNames(c("index","trial",str_replace(filenames[f],".json","")))

  # extract data
  tmp1 = fromJSON(file = paste0(location,"/",filenames[f])) %>%
    lapply(str_c) %>%
    unlist() %>%
    matrix(ncol=42) %>%
    as.data.frame() %>%
    setNames(1:42) %>%
    gather(trial,prediction) %>%
    rowwise %>%
    mutate(prediction = prediction %>% str_extract_all("[0-9]+") %>% unlist() %>% as.numeric() %>% list()) %>%
    ungroup()

  for (i in 1:42){
    tmp2 = tmp1 %>%
      filter(trial == i) %>%
      select(prediction) %>%
      rbind(data.frame(prediction = 0)) %>% #add dummy to make sure data frame is not empty
      unlist() %>%
      table() %>%
      as.data.frame() %>%
      setNames(c("index",str_replace(filenames[f],".json",""))) %>%
      mutate(trial = i,
             index = index %>% as.character() %>% as.numeric())
    tmp = rbind(tmp,tmp2)
  }

  df.percentages.prediction = df.percentages.prediction %>%
    left_join(tmp)
}

df.percentages.prediction = df.percentages.prediction %>%
  na.replace(0)

df.exp1.models = df.percentages.prediction
# save("df.percentages.prediction", file = "brick_selection_predictions2.RData")
save("df.exp1.models", file = "exp1_models.RData")

# ========================= -------------------------------------------------------------------
# EXP2: Read in and structure data --------------------------------------------------------------------------
con = dbConnect(SQLite(),dbname = "../javascript/Experiment_2/participants.db");
# con = dbConnect(SQLite(),dbname = "../javascript/participants.db");
df.complete = dbReadTable(con,"towers")
dbDisconnect(con)

# read in ground truth 
load("detailed_trial_info.RData")

df.experiment = df.complete %>% 
  filter(codeversion == 'experiment_2') %>% 
  filter(status %in% c(3,4))

ntrials = 42
nvariables = 3
variables.unstructured = names(fromJSON(df.experiment$datastring[1])$questiondata)
variables.additional = c("experiment","participant","workerid","taskid","time")
variables.task = paste(c("id","image","response"),rep(1:ntrials,each=nvariables),sep="_")
  
df.wide = data.frame(matrix(nrow=nrow(df.experiment),ncol=length(variables.unstructured)+
                              length(variables.additional)+length(variables.task)))
colnames(df.wide) = c(variables.additional,variables.task,variables.unstructured)

for (i in 1:nrow(df.wide)){
  a = fromJSON(df.experiment$datastring[i])
  # additional variables
  df.wide$experiment[i] = df.experiment$codeversion[i]
  df.wide$participant[i] = i
  df.wide$workerid[i] = a$workerId
  df.wide$taskid[i] = a$assignmentId
  df.wide$time[i] = as.numeric(as.POSIXct(df.experiment$endhit[i], format = "%Y-%m-%d %H:%M:%S")-
                                 as.POSIXct(df.experiment$beginhit[i], format = "%Y-%m-%d %H:%M:%S"))
  # unstructured variables 
  for (j in 1:length(variables.unstructured)){
    df.wide[[variables.unstructured[j]]][i] = a[["questiondata"]][[variables.unstructured[j]]]
  }
  
  # task variables 
  l = 1
  for (j in 1:ntrials){
    for (k in seq(2,by = 2, length.out = nvariables)){
      if (is.null(a$data[[j]]$trialdata[[k]])){
        b = NA;
      }else{
        b = a$data[[j]]$trialdata[[k]]
      }
      df.wide[[variables.task[l]]][i] = b
      l = l+1
    }
  }
}

df.long = df.wide %>% 
  wideToLong(within='order') %>% 
  select(participant,condition,order,id,response) %>%
  mutate(condition = factor(condition,levels = 0:1,labels = c('prediction','responsibility'))) %>% 
  rename(trial = id) %>% 
  arrange(participant,trial)

#exclude based on catch trial 
exclude = df.long %>% 
  filter(trial == 5) %>% 
  filter((condition == 'responsibility' & response > 15) |
           (condition == 'prediction' & response >= 1)) %>%
  select(participant) %>% 
  unlist() %>% 
  as.numeric()

df.info = df.info.detailed %>% group_by(trial) %>% summarise(nbricks = n(), nfall = sum(fall), pfall = nfall/nbricks)

df.long = df.long %>%
  filter(!participant %in% exclude) %>%
  left_join(df.info)

# filter out catch trial 
df.long = df.long %>% 
  filter(trial != 5)

# write.csv(df.long,file = 'towers_data_exp2.csv',row.names = F)

# df.wide.exp2 = df.wide
# save("df.wide.exp2",file = 'towers_wide_exp2.RData')

# write summary of prediction data 
# df.prediction = df.long %>%
#   filter(condition == "prediction") %>%
#   group_by(trial) %>%
#   summarise(prediction_mean = smean.cl.boot(response)[1],
#             prediction_low = smean.cl.boot(response)[2],
#             prediction_high = smean.cl.boot(response)[3]
#             ) %>%
#   ungroup %>%
#   mutate(experiment = 'prediction')
# save("df.prediction",file = 'towers_prediction_exp2.RData')

# EXP2: Clustering  ---------------------------------------------------------------------------
df.tmp = df.long %>% 
  filter(condition == 'prediction') %>% 
  select(participant,trial,response) %>% 
  spread(trial,response)

# Gaussian mixture model classification 
set.seed(2)
fit = Mclust(df.tmp[,-1],modelNames = "EII")
df.cluster = data.frame(participant = df.tmp$participant, cluster = fit$classification)

df.long = df.long %>% 
  left_join(df.cluster)

# EXP2: Model fitting  ------------------------------------------------------------------------

# empirical prediction model
df.prediction.empirical = df.long %>% 
  filter(condition == "prediction") %>%
  group_by(trial) %>% 
  summarise(prediction_mean = mean(response),
            prediction_low = smean.cl.boot(response)[2],
            prediction_high = smean.cl.boot(response)[3]) %>% 
  ungroup()

# noisy prediction models 
df.prediction.model = df.percentages.prediction %>% 
  group_by(trial) %>% 
  summarise_at(vars(contains('impulse')),funs(sum(.)/100)) %>% 
  ungroup()



# #create data frame with judgments 
# df.model = df.long %>% 
#   filter(condition == "prediction") %>%
#   group_by(trial) %>% 
#   summarise(data = mean(response)) %>% 
#   left_join(df.info %>% select(trial,trial_name,name,difference,initial)) %>% 
#   ungroup() %>% 
#   select(trial,trial_name,name,initial,data,difference) %>% 
#   rename(ground_truth = difference) %>% 
#   mutate(ground_truth = ground_truth-1)
# 
# #read in json file and calculate correlation for each model 
# filenames = list.files("../../code/javascript/node/simulations")
# nmodels = length(filenames)
# 
# df.correlations = matrix(NA,nrow = nmodels, ncol = 3) %>% 
#   as.data.frame() %>% 
#   setNames(c("model","correlation","error"))
# 
# for (i in 1:nmodels){
#   tmp = fromJSON(file = paste0("../../code/javascript/node/simulations/",filenames[i]))
#   modelname = str_replace(filenames[i],".json","")
#   df.model[[modelname]] = df.model$initial - lapply(tmp,mean) %>% as.numeric() - 1 #get the means for each trial  
#   df.correlations[i,] = c(modelname,cor(df.model[[modelname]],df.model$data),rmse(df.model[[modelname]],df.model$data))
# }

df.correlations = df.correlations %>% 
  cbind(df.correlations$model %>% 
          str_split("_") %>% 
          unlist() %>% 
          matrix(ncol=3,byrow=T) %>% 
          as.data.frame(stringsAsFactors = F) %>% 
          setNames(c("noise_type","noise_amt","fric"))) %>% 
  mutate_each(funs(as.numeric(.)),noise_amt, fric, correlation, error)

# find best model of each type 
bestmodels = df.correlations %>% 
  # arrange(-correlation) %>%
  arrange(error) %>%
  group_by(noise_type) %>% 
  filter(row_number() == 1)

#find the model which correlates best 
bestmodel = bestmodels$model[1]

#boostrapped errors for best model 
tmp = fromJSON(file = paste0("../../code/javascript/node/simulations/",bestmodel,".json")) %>% 
  unlist() %>% 
  matrix(ncol=42) %>% 
  as.data.frame() %>% 
  setNames(1:42) %>% 
  gather(trial,prediction) %>% 
  mutate(trial = as.numeric(trial)) %>% 
  left_join(df.model %>% select(trial,initial)) %>% 
  mutate(prediction = initial - prediction - 1) %>% 
  select(-initial) %>% 
  group_by(trial) %>% 
  summarise(best = mean(prediction),
            model.low = smean.cl.boot(prediction)[2],
            model.high = smean.cl.boot(prediction)[3])

df.model = df.model %>% 
  left_join(tmp)



# EXP2: Feature-based predictions  ------------------------------------------------------------

df.features = fromJSON(file = "../../code/javascript/node/features.json") %>% 
  unlist() %>% 
  matrix(ncol=8,nrow=42,byrow=T) %>% 
  as.data.frame() %>% 
  setNames(c("above_naive","above_contact_general","above_contact_selective",
             "edge_distance","avg_x","avg_y","avg_angle","tower_height")) %>% 
  # mutate_each(funs(scale(.) %>% as.numeric())) %>% 
  mutate(trial = 1:nrow(.)) %>% 
  left_join(df.long %>% 
              filter(condition == "prediction") %>% 
              group_by(trial) %>% 
              summarise(prediction = mean(response))) %>% 
  left_join(df.info %>% select(trial,initial)) %>% 
  select(trial, prediction, everything())

# fit = lm("prediction~above_naive+above_contact_general+above_contact_selective+edge_distance+avg_x+avg_y+avg_angle+tower_height+initial",data=df.features)
# step = stepAIC(fit, direction="both")
# fit = lm("prediction ~ above_contact_general + avg_y + avg_angle + initial",data=df.features)
fit = lm("prediction ~ avg_y",data=df.features)
fit %>% summary()
df.model$features = fit$fitted.values



# EXP2: Scatter plot (prediction/responsibility vs. ground truth) -------------------------------------------------------------------------------

model = bestmodel
# model = "dxy-local_0.0_0.00" #ground truth
model = "impulse-global_3.95_0.2"
model = c("best","model.low","model.high")
# model = "impulse-local_4.85_0.1"
# model = "features"

# model = "ground_truth" #ground truth

condition.name = 'prediction'
# condition.name = 'responsibility'

df.plot = df.long %>% 
  filter(condition == condition.name) %>% 
  group_by(trial,index) %>% 
  summarise(mean = smean.cl.boot(response)[1],
            low = smean.cl.boot(response)[2],
            high = smean.cl.boot(response)[3],
            ground.truth = mean(difference)) %>% 
  ungroup() %>% 
  left_join(df.model %>% select(one_of("trial",model))) %>% 
  filter(trial != 5) #remove the exclusion trial ... 

names(df.plot)[7] = "model"
  
ggplot(df.plot,aes(x=model,y=mean))+
  geom_smooth(method='lm',color='black')+
  geom_errorbar(aes(ymin = low, ymax = high),width=0.05,alpha=0.5)+
  geom_errorbarh(aes(xmin = model.low, xmax = model.high),height=0.1,alpha=0.5)+
  geom_abline(intercept=0, slope=1, linetype='dashed') +
  geom_point(size=2)+
  geom_text(label = df.plot$trial,hjust = -0.5, vjust = -0.5)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("r = ", cor(df.plot$model,df.plot$mean) %>% round(2)),
           hjust = 0, vjust = 1.5, size = 8)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("RMSE = ", rmse(df.plot$model,df.plot$mean) %>% round(2)),
           hjust = 0, vjust =3, size = 8)+
  scale_x_continuous(breaks = 0:8,labels = 0:8)+
  scale_y_continuous(breaks = 0:8,labels = 0:8)+
  coord_fixed(xlim = c(0,8), ylim = c(0,8))+
  theme_bw()+
  labs(y="How many bricks would fall?", x = "model prediction")+
  # labs(y="How many bricks would fall?", x = "ground truth")+
  theme(text = element_text(size=24),
        panel.grid = element_blank()
  )
# ggsave(paste0("../../figures/plots/exp2_",condition.name,'_',model,'_scatter.pdf'),width = 8,height = 6)
ggsave(paste0("../../figures/plots/exp2_",condition.name,'_',bestmodel,'_scatter.pdf'),width = 8,height = 8)

# EXP2: Scatter plot (prediction vs responsibility) -------------------------------------------------------------------------------

#add labels
df.labels = matrix(NA,ncol=3,nrow=42) %>% 
  as.data.frame() %>% 
  setNames(c("trial","label","colorindex")) %>% 
  mutate(trial = 1:42)
df.labels$label[c(5,2,10,14,24,28,22,41)] = letters[1:8]
df.labels$colorindex = as.factor((!is.na(df.labels$label))*1)

df.plot = df.long %>% 
  left_join(df.info.detailed %>% group_by(trial) %>% summarise(nbricks = n())) %>% 
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

# ggsave(paste0('../../figures/plots/exp2_',model,'_responsibility_scatter.pdf'),width = 8,height = 6)
ggsave(paste0('../../figures/plots/exp2_prediction_responsibility_scatter.pdf'),width = 8,height = 6)
  

# EXP2: Analysis ------------------------------------------------------------------------------

# Regression: see whether average height predicts something over and above the best simulation model 

df.regression = df.long %>% 
  filter(condition == "prediction") %>% 
  group_by(trial) %>% 
  summarise(mean = smean.cl.boot(response)[1],
            low = smean.cl.boot(response)[2],
            high = smean.cl.boot(response)[3]) %>% 
  ungroup() %>% 
  left_join(df.model %>% select(trial,best,model.low,model.high)) %>% 
  left_join(df.features %>% select(trial,avg_y,initial))

fit = lm(mean~best+avg_y+initial,data=df.regression)
fit %>% summary()
df.regression$model = fit$fitted.values %>% as.numeric()

df.plot = df.regression 

ggplot(df.plot,aes(x=model,y=mean))+
  geom_smooth(method='lm',color='black')+
  geom_errorbar(aes(ymin = low, ymax = high),width=0.1,alpha=0.5)+
  geom_abline(intercept=0, slope=1, linetype='dashed') +
  geom_point(size=2)+
  geom_text(label = df.plot$trial,hjust = -0.5, vjust = -0.5)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("r = ", cor(df.plot$model,df.plot$mean) %>% round(2)),
           hjust = 0, vjust = 1.5, size = 8)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("RMSE = ", rmse(df.plot$model,df.plot$mean) %>% round(2)),
           hjust = 0, vjust =3, size = 8)+
  scale_x_continuous(breaks = 0:8,labels = 0:8)+
  scale_y_continuous(breaks = 0:8,labels = 0:8)+
  coord_fixed(xlim = c(0,8), ylim = c(0,8))+
  theme_bw()+
  labs(y="How many bricks would fall?", x = "model prediction")+
  # labs(y="How many bricks would fall?", x = "ground truth")+
  theme(text = element_text(size=24),
        panel.grid = element_blank()
  )



# there is still a significant partial correlation of the average tower height even when taking 
# into account the predictions of the best fitting simulation model 




# EXP2: Get features of each item  ------------------------------------------------------------------
load("trialinfo.RData")

nfeatures = 8 
df.features = matrix(NA,nrow=nrow(df.info),ncol=nfeatures+1)

for (i in 1:nrow(df.info)){
  ct = v8()
  ct$source("Box2dWeb-2.1.a.3.js")
  ct$source("../javascript/stimuli/js/counterfactual_brick_simulation.js")
  fileName = paste0("data/initial_", df.info$name[i], ".json")
  tmp = readChar(fileName, file.info(fileName)$size)
  ct$eval(paste0('var brick_init_positions = ',tmp)) #feed in json info
  ct$eval("getFeatures()") #run function 
  tmp = c("name" = df.info$name[i],ct$get("feature_list") %>% unlist())
  df.features[i,] = tmp
}

df.features = df.features %>% 
  as.data.frame(stringsAsFactors=F) %>% 
  setNames(names(tmp)) %>% 
  mutate_each(funs(as.numeric(.)),-name)

# EXP2: PLOT - Density plots of selections for each trial -----------------------------

df.plot = df.long %>% 
  filter(condition == 'prediction') %>% 
  select(participant,trial,response)

ggplot(df.plot,aes(x=response))+
  geom_density()+
  facet_wrap(~trial,ncol = 6, scales = "free_y")+
  # geom_text(data = df.text, aes(label = trial),vjust=1)+
  scale_x_continuous(breaks = seq(0,16,2), labels = seq(0,16,2))+
  theme_bw()+
  labs(x = 'number of bricks selected')+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold",vjust=1))

ggsave(paste0("../../figures/plots/exp2_bricks_selected_trial.pdf"),width = 8,height = 6)

# EXP2: PLOT - Individual participants -----------------------------

df.tmp = df.cluster %>% 
  count(cluster) %>% 
  mutate(label = paste0(cluster, ' (n = ',n,")")) %>% 
  select(cluster,label)

df.plot = df.long %>% 
  filter(condition == 'prediction') %>% 
  left_join(df.tmp) 

ggplot(df.plot,aes(x=trial,y=response, color = as.factor(label)))+
  geom_line(alpha = 0.2, size = 0.5,aes(group = participant))+
  stat_summary(fun.y = "mean", geom = "line", size = 1)+
  stat_summary(fun.y = "mean", geom = "point", size = 2)+
  scale_x_continuous(breaks = 1:42, labels = 1:42)+
  theme_bw()+
  labs(x = 'trial',y = 'number of bricks predicted', color = 'cluster')+
  theme(panel.grid = element_blank())

# ggsave(paste0("../../figures/plots/exp2_bricks_predicted_cluster.pdf"),width = 12,height = 6)


# ========================= -------------------------------------------------------------------

# EXP3: Generate info about the different trials  (no need to run) ---------------------------------------------

# Read in brick information 
filenames = list.files("../../data/stimuli_exp2/json") %>% mixedsort()

df.info.detailed = matrix(nrow=0,ncol=5) %>% 
  as.data.frame() %>% 
  setNames(c("index","x","y","angle","trial"))

for (i in 1:length(filenames)){
  tmp = fromJSON(file = paste0("../../data/stimuli_exp2/json/",filenames[i])) %>% 
    unlist() %>% 
    matrix(ncol=4,byrow=T) %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    setNames(c("index","x","y","angle")) %>% 
    mutate(index = str_replace(index,"brick_","") %>% as.numeric()) %>% 
    mutate(trial = i) %>% 
    filter(x != - 5)
  df.info.detailed = rbind(df.info.detailed,tmp)
}

# save("df.info.detailed", file = "detailed_trial_info.RData")

# EXP3: PLOT - Generate images with labeled bricks (no need to run) -----------------------------------------------------------------

# Read in brick information 
filenames = list.files("../../data/stimuli_exp2/json") %>% mixedsort()
imagenames = list.files("../../figures/stimuli/experiment2/initial")  %>% mixedsort()
imagenames = imagenames[str_detect(imagenames,"initial")] %>% mixedsort()

# for (i in 1:length(filenames)){
for (i in 1){

tmp = fromJSON(file = paste0("../../data/stimuli_exp2/json/",filenames[i])) %>% 
  unlist() %>% 
  matrix(ncol=4,byrow=T) %>% 
  as.data.frame(stringsAsFactors = F) %>% 
  setNames(c("index","x","y","angle")) %>% 
  mutate(index = str_replace(index,"brick_","")) %>% 
  mutate_all(funs(as.numeric(.))) %>% 
  mutate_each(funs(.*100),x,y) %>% 
  mutate(y = 600-y) %>% 
  filter(x != -500)

df.plot = tmp 

image = png::readPNG(paste0("../../figures/stimuli/experiment2/initial/",imagenames[i]))
ggplot(df.plot, aes(x = x, y = y)) +
  annotation_custom(rasterGrob(image, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    xmin = 0, xmax = 800, ymin = 0, ymax = 600) +
  geom_text(aes(label = index), size = 7, fontface = 2, 
            colour = 'black', hjust = 0.5, vjust = 0.5)+
  scale_x_continuous(expand = c(0,0), limits = c(0,800))+
  scale_y_continuous(expand = c(0,0), limits = c(0,600))+
  theme_void()+
  theme(axis.ticks.length = unit(0.001, "mm"))

# ggsave(paste0("../../figures/stimuli/experiment2/initial_labeled/",str_replace(imagenames[i],".png",""),".pdf"),width=8,height=6)
}

# EXP5: Generate images with marked bricks that will fall (no need to run) -----------------------------------------------------------------

# Read in brick information 
filenames = list.files("../../data/stimuli_exp2/json") %>% mixedsort()
imagenames = list.files("../../figures/stimuli/experiment2/initial")  %>% mixedsort()
# imagenames = imagenames[str_detect(imagenames,"initial")] %>% mixedsort()

for (i in 1:(length(filenames))){
  # for (i in 3){
  tmp = fromJSON(file = paste0("../../data/stimuli_exp2/json/",filenames[i])) %>% 
    unlist() %>% 
    matrix(ncol=4,byrow=T) %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    setNames(c("index","x","y","angle")) %>% 
    mutate(index = str_replace(index,"brick_","")) %>% 
    mutate_all(funs(as.numeric(.))) %>% 
    mutate_each(funs(.*100),x,y) %>% 
    mutate(y = 600-y) %>% 
    filter(x != -500) %>% 
    mutate(trial = i) %>% 
    left_join(df.info.detailed %>% select(trial,index,fall)) %>% 
    filter(fall == 1)
  
  df.plot = tmp 
  
  image = png::readPNG(paste0("../../figures/stimuli/experiment2/initial/",imagenames[i]))
  ggplot(df.plot, aes(x = x, y = y)) +
    annotation_custom(rasterGrob(image, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      xmin = 0, xmax = 800, ymin = 0, ymax = 600) +
    geom_point(color = 'white', size = 5)+
    scale_x_continuous(expand = c(0,0), limits = c(0,800))+
    scale_y_continuous(expand = c(0,0), limits = c(0,600))+
    theme_void()+
    theme(axis.ticks.length = unit(0.001, "mm"))
  
  ggsave(paste0("../../figures/stimuli/experiment3/initial_marked/","trial",i,"_marked.png"),width=8,height=6,dpi=100)
}

# EXP3: Read in and structure data --------------------------------------------------------------------------
# Reads in the data files and creates data frames
# rm(list = ls())
load("detailed_trial_info.RData")
load("towers_prediction_exp2.RData") #prediction data from EXP2

con = dbConnect(SQLite(),dbname = "../javascript/Experiment_3/participants.db");
# con = dbConnect(SQLite(),dbname = "../javascript/participants.db");
df.complete = dbReadTable(con,"towers")
dbDisconnect(con)

df.experiment = df.complete %>% 
  filter(codeversion == 'experiment_3') %>% 
  filter(status %in% c(3,4))

ntrials = 42
nvariables = 3
variables.unstructured = names(fromJSON(df.experiment$datastring[1])$questiondata)
variables.additional = c("experiment","participant","workerid","taskid","time")
variables.task = paste(c("id","image","response"),rep(1:ntrials,each=nvariables),sep="_")

df.wide = data.frame(matrix(nrow=nrow(df.experiment),ncol=length(variables.unstructured)+
                              length(variables.additional)+length(variables.task)))
colnames(df.wide) = c(variables.additional,variables.task,variables.unstructured)

for (i in 1:nrow(df.wide)){
  a = fromJSON(df.experiment$datastring[i])
  # additional variables
  df.wide$experiment[i] = df.experiment$codeversion[i]
  df.wide$participant[i] = i
  df.wide$workerid[i] = a$workerId
  df.wide$taskid[i] = a$assignmentId
  df.wide$time[i] = as.numeric(as.POSIXct(df.experiment$endhit[i], format = "%Y-%m-%d %H:%M:%S")-
                                 as.POSIXct(df.experiment$beginhit[i], format = "%Y-%m-%d %H:%M:%S"))
  # unstructured variables 
  for (j in 1:length(variables.unstructured)){
    df.wide[[variables.unstructured[j]]][i] = a[["questiondata"]][[variables.unstructured[j]]]
  }
  
  # task variables 
  l = 1
  for (j in 1:ntrials){
    for (k in seq(2,by = 2, length.out = nvariables)){
      if (length(a$data[[j]]$trialdata[[k]])==0){
        b = "";
      }else{
        b = a$data[[j]]$trialdata[[k]]
      }
      df.wide[[variables.task[l]]][i] = str_c(as.character(b),collapse=",")
      l = l+1
    }
  }
}

df.long = df.wide %>%
  wideToLong(within='order') %>%
  select(participant,order,id,response) %>%
  rename(trial = id) %>%
  mutate_each(funs(as.numeric(.)),trial,order) %>% 
  arrange(participant,trial) %>% 
  rowwise() %>% 
  mutate(response = response %>% str_split(",") %>% unlist() %>% as.numeric() %>% list()) %>% 
  mutate(number = length(response)) %>% 
  ungroup() %>% 
  mutate(number = ifelse(is.na(response),0,number))

#exclude based on catch trial
exclude = df.long %>%
  filter(trial == 5) %>%
  filter(number > 1) %>% 
  select(participant) %>%
  unlist() %>%
  as.numeric()

df.long = df.long %>%
  filter(!participant %in% exclude) %>% 
  filter(trial != 5) #remove the catch trial 
#   left_join(df.info %>% select(trial,initial,final,difference, index,trial_name, difference_proportion) %>% mutate_each(funs(.-1),difference) %>% rename(image = trial_name))

# merge prediction data about how many bricks will fall from EXP2 and EXP3 
df.prediction = df.prediction %>% 
  rbind(df.long %>% 
          group_by(trial) %>% 
          summarise(prediction_mean = smean.cl.boot(number)[1],
                    prediction_low = smean.cl.boot(number)[2],
                    prediction_high = smean.cl.boot(number)[3]) %>%
          ungroup %>%
          mutate(experiment = 'selection')
  )

# percentages of bricks falling 
df.percentages = matrix(ncol=3,nrow=0) %>% 
  as.data.frame() %>% 
  setNames(c("index","freq","trial"))

for (i in 1:ntrials){
  tmp = df.long %>% 
    filter(trial == i) %>% 
    select(response) %>% 
    rbind(data.frame(response = 0)) %>% #add dummy observation to deal with empty data frames 
    unlist() %>% 
    table() %>% 
    as.data.frame() %>% 
    setNames(c("index","freq")) %>% 
    mutate(trial = i)
  
  df.percentages = rbind(df.percentages,tmp)
}

df.percentages = df.percentages %>% 
  mutate(freq = ifelse(is.na(freq),0,freq),
         freq = ifelse(index == 0, 0, freq),
         freq = freq/nrow(df.wide)*100, #only works as long as we don't exclude any participants 
         index = index %>% as.character() %>% as.numeric())

#merge with information about the trials 
df.percentages = df.percentages %>%
  left_join(df.info.detailed,.) %>%
  select(trial,index,freq) %>%
  filter(index != 0) %>%
  na.replace(0)

#remove the special brick from the predictions 
df.percentages.prediction = df.percentages.prediction %>% 
  filter(index != 0)

# df.wide.exp3 = df.wide
# save("df.wide.exp3",file = 'towers_wide_exp3.RData')

# EXP3: Clustering  ---------------------------------------------------------------------------

df.tmp = df.long %>% 
  select(participant,trial,number) %>% 
  spread(trial,number)
  
# Gaussian mixture model classification 
set.seed(2)
fit = Mclust(df.tmp[,-1],modelNames = "EII")
df.cluster = data.frame(participant = 1:nrow(df.tmp), cluster = fit$classification)

df.long = df.long %>% 
  left_join(df.cluster)

# EXP3: Find best models  ---------------------------------------------------------------------

# MINIMIZE RMSE 
df.rmse = data.frame(matrix(ncol = 2, nrow = 0)) %>% setNames(c("model","rmse"))
for(i in 1:(ncol(df.percentages.prediction)-5)){
  df.rmse[i,] = c(names(df.percentages.prediction)[i+5],rmse(df.percentages$freq,df.percentages.prediction[[i+5]]))
}

df.rmse = df.rmse %>% 
  cbind(df.rmse$model %>% 
          str_split("_") %>% 
          unlist() %>% 
          matrix(ncol=3,byrow=T) %>% 
          as.data.frame(stringsAsFactors = F) %>% 
          setNames(c("noise_type","noise_amt","fric"))) %>% 
  mutate_each(funs(as.numeric(.)),noise_amt,fric) %>% 
  select(model,noise_type,noise_amt,fric,rmse)

bestmodels.rmse = df.rmse %>% 
  arrange(rmse) %>% 
  group_by(noise_type) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

# MINIMIZE LOG-LIKELIHOOD

df.selections = df.info.detailed %>% 
  select(trial,index) %>% 
  filter(index != 0) %>% 
  left_join(df.long %>% select(participant,trial)) %>% 
  mutate(response = 0) %>% 
  select(participant,trial,index,response) %>% 
  arrange(participant,trial,index)

for (i in 1:nrow(df.long)){
  tmp = df.long[i,]
  df.selections$response[
      df.selections$participant == tmp$participant & 
      df.selections$trial == tmp$trial & 
      df.selections$index %in% (tmp$response %>% unlist())] = 1
}

df.likelihood = df.selections %>% 
  left_join(df.percentages.prediction %>% select(-c(x,y,angle))) %>% 
  mutate_each(funs(./100),contains("impulse")) %>% 
  mutate_each(funs(ifelse(response == 1,.,1-.)),contains("impulse")) %>% 
  mutate_each(funs(ifelse(. == 1,0.99,.)),contains("impulse")) %>% 
  mutate_each(funs(ifelse(. == 0,0.01,.)),contains("impulse")) %>% 
  mutate_each(funs(log(.)),contains("impulse")) %>% 
  summarise_at(vars(contains("impulse")),sum) %>% 
  gather(model,likelihood)

df.likelihood = df.likelihood %>% 
  cbind(df.likelihood$model %>% 
        str_split("_") %>% 
        unlist() %>% 
        matrix(ncol=3,byrow=T) %>% 
        as.data.frame(stringsAsFactors = F) %>% 
        setNames(c("noise_type","noise_amt","fric"))) %>% 
  # select(-model) %>% 
  select(model,noise_type,noise_amt,fric,likelihood)

bestmodels.likelihood = df.likelihood %>% 
  arrange(desc(likelihood)) %>%
  group_by(noise_type) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

# EXP3: PLOT - 2D model fit landscape ---------------------------------------------------------

# df.plot = df.rmse %>%
#   mutate(value = as.numeric(rmse))

df.plot = df.likelihood %>%
  rename(value = likelihood) %>%
  mutate(noise_amt = as.numeric(noise_amt))

# simple line plot 
ggplot(df.plot,aes(x=noise_amt,y=value,group = noise_type, color =noise_type))+
  geom_line()
  
# 2D landscape 
# ggplot(df.plot,aes(x = fric, y = noise_amt, fill = rmse))+
#   geom_raster()+
#   scale_fill_gradient(low="red", high="green")


# EXP3: PLOT - Images that show percentage of selected bricks  -----------------------------------------------------------------

# Read in brick information 
filenames = list.files("../../data/stimuli_exp2/json") %>% mixedsort()

imagenames = list.files("../../figures/stimuli/experiment2/initial") %>% mixedsort
imagenames = imagenames[str_detect(imagenames,"initial")]

modelindex = "freq"
modelindex = df.rmse$model[which.min(df.rmse$rmse)]
# modelindex = "impulse-local_4.85_0.6"

# for (i in 1){
for (i in 1:length(imagenames)){
  
  tmp = fromJSON(file = paste0("../../data/stimuli_exp2/json/",filenames[i])) %>% 
    unlist() %>% 
    matrix(ncol=4,byrow=T) %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    setNames(c("index","x","y","angle")) %>% 
    mutate(index = str_replace(index,"brick_","")) %>% 
    mutate_all(funs(as.numeric(.))) %>% 
    mutate_each(funs(.*100),x,y) %>% 
    mutate(y = 600-y) %>% 
    filter(x != -500) %>% 
    # left_join(df.percentages %>% filter(trial == i) %>% select(index,one_of(modelindex))) %>% 
    left_join(df.percentages.prediction %>% filter(trial == i) %>% select(index,one_of(modelindex))) %>% 
    setNames(c("index","x","y","angle","freq")) %>% 
    mutate(freq = ifelse(index == 0, 0, freq),
           freq = freq %>% round(0),
           freq = paste0(freq,"%"))
  
  df.plot = tmp
  
  image = png::readPNG(paste0("../../figures/stimuli/experiment2/initial/",imagenames[i]))
  ggplot(df.plot, aes(x = x, y = y)) +
    annotation_custom(rasterGrob(image, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      xmin = 0, xmax = 800, ymin = 0, ymax = 600) +
    geom_text(aes(label = freq), size = 5, fontface = 2, 
              colour = 'black', hjust = 0.5, vjust = 0.5)+
    scale_x_continuous(expand = c(0,0), limits = c(0,800))+
    scale_y_continuous(expand = c(0,0), limits = c(0,600))+
    theme_void()+
    theme(axis.ticks.length = unit(0.001, "mm"))
  
  ggsave(paste0("../../figures/plots/selected_bricks/",modelindex,"_trial_",i,".pdf"),width=8,height=6)
  # ggsave(paste0("../../figures/plots/selected_bricks/selected_trial_",i,".pdf"),width=8,height=6)
}

# EXP3: PLOT - Model vs. selection -----------------------------

# modelindex = df.rmse$model[which.min(df.rmse$rmse)]
# modelindex = df.rmse %>% 
#   summarise(model[which.min(rmse)]) %>% 
#   as.character()

modelindex = bestmodels.likelihood %>%
  # filter(noise_type == 'impulse-local') %>%
  # filter(noise_type == 'impulse-global') %>%
  summarise(model[which.max(likelihood)]) %>%
  as.character()

# ylabelname = 'c) local noise'
# ylabelname = 'b) global noise'
ylabelname = 'd) local-above noise'
# ylabelname = 'a) ground truth'
# modelindex = 'ground_truth'

df.plot = df.percentages %>% 
  select(trial,index,freq) %>% 
  left_join(df.percentages.prediction %>% select(trial,index,one_of(modelindex))) %>%
  # left_join(df.info.detailed %>% select(trial,index,fall) %>% mutate(fall = 100*fall)) %>% #ground truth
  setNames(c("trial","index","data", "model"))
  
ggplot(df.plot,aes(x=data,y = model))+
  geom_smooth(method='lm',color='black')+
  geom_abline(intercept=0, slope=1, linetype='dashed') +
  geom_point(size=3,alpha=0.5)+
  # geom_text(label = paste0(df.plot$trial,"[",df.plot$index,"]"),hjust = 0, vjust = 0, parse = T)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("r = ", cor(df.plot$data,df.plot$model) %>% round(2)),
           hjust = 0, vjust = 1.5, size = 8)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("RMSE = ", rmse(df.plot$data,df.plot$model) %>% round(2)),
           hjust = 0, vjust =3, size = 8)+
  scale_y_continuous(breaks = seq(0,100,25),labels = seq(0,100,25),limits = c(0,100))+
  scale_x_continuous(breaks = seq(0,100,25),labels = seq(0,100,25),limits = c(0,100))+
  # coord_fixed(xlim = c(0,8), ylim = c(0,8))+
  # ggtitle(modelindex)+
  theme_bw()+
  # labs(y="model prediction", x = "empirical selections")+
  labs(y=ylabelname, x = "probability that a brick was selected")+
  theme(text = element_text(size=24),
        panel.grid = element_blank(),
        axis.title.x = element_text(margin = margin(t=10))
  )
ggsave(paste0("../../figures/plots/exp3_selection_",modelindex,"_scatter.pdf"),width = 8,height = 6)


# EXP3: Residuals plots to compare models  ----------------------------------------------------

df.plot = df.percentages %>% 
  select(trial,index,freq) %>% 
  left_join(df.percentages.prediction %>% select(one_of(bestmodels$model),index,trial)) %>% 
  mutate_each(funs(freq-.),one_of(bestmodels$model)) %>% 
  select(-freq) %>% 
  gather(modelindex,value,-c(index,trial)) %>% 
  mutate(label = paste0(trial,"_",index)) %>% 
  mutate(absdiff = abs(value)) %>% 
  arrange(-absdiff) %>% 
  group_by(trial,modelindex) %>%
  filter(row_number() == 1)

ggplot(df.plot,aes(x = label,y=value,color=modelindex))+
  geom_point()
  
# EXP3: PLOT - Relationship between prediction and selection data -----------------------------

df.plot = df.prediction %>% 
  rename(mean = prediction_mean,
         low = prediction_low,
         high = prediction_high) %>% 
  gather(variable,value,-c(trial,experiment)) %>% 
  unite(tmp,experiment,variable) %>% 
  spread(tmp,value)

ggplot(df.plot,aes(x=prediction_mean,y=selection_mean))+
  geom_smooth(method='lm',color='black')+
  geom_errorbar(aes(ymin = selection_low, ymax = selection_high),width=0.05,alpha=0.5)+
  geom_errorbarh(aes(xmin = prediction_low, xmax = prediction_high),height=0.1,alpha=0.5)+
  geom_abline(intercept=0, slope=1, linetype='dashed') +
  geom_point(size=2)+
  geom_text(label = df.plot$trial,hjust = -0.5, vjust = -0.5)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("r = ", cor(df.plot$prediction_mean,df.plot$selection_mean) %>% round(2)),
           hjust = 0, vjust = 1.5, size = 8)+
  annotate(geom = "text", x=0,y=Inf,label = paste0("RMSE = ", rmse(df.plot$prediction_mean,df.plot$selection_mean) %>% round(2)),
           hjust = 0, vjust =3, size = 8)+
  scale_x_continuous(breaks = 0:8,labels = 0:8)+
  scale_y_continuous(breaks = 0:8,labels = 0:8)+
  coord_fixed(xlim = c(0,8), ylim = c(0,8))+
  theme_bw()+
  labs(y="selection", x = "prediction")+
  # labs(y="How many bricks would fall?", x = "ground truth")+
  theme(text = element_text(size=24),
        panel.grid = element_blank()
  )
# ggsave(paste0("../../figures/plots/exp2_exp3_prediction_selection_scatter.pdf"),width = 8,height = 8)


# EXP3: PLOT - Density plots of selections for each trial -----------------------------

df.plot = df.long %>% 
  select(participant,trial,number)
ggplot(df.plot,aes(x=number))+
  geom_density()+
  facet_wrap(~trial,ncol = 6, scales = "free_y")+
  # geom_text(data = df.text, aes(label = trial),vjust=1)+
  scale_x_continuous(breaks = seq(0,16,2), labels = seq(0,16,2))+
  theme_bw()+
  labs(x = 'number of bricks selected')+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold",vjust=1))

ggsave(paste0("../../figures/plots/exp3_bricks_selected_trial.pdf"),width = 8,height = 6)


# EXP3: PLOT - Individual participants -----------------------------

df.tmp = df.cluster %>% 
  count(cluster) %>% 
  mutate(label = paste0(cluster, ' (n = ',n,")")) %>% 
  select(cluster,label)

df.plot = df.long %>% 
  left_join(df.tmp) 
  
ggplot(df.plot,aes(x=trial,y=number, color = as.factor(label)))+
  geom_line(alpha = 0.2, size = 0.5,aes(group = participant))+
  stat_summary(fun.y = "mean", geom = "line", size = 1)+
  stat_summary(fun.y = "mean", geom = "point", size = 2)+
  scale_x_continuous(breaks = 1:42, labels = 1:42)+
  theme_bw()+
  labs(x = 'trial',y = 'number of bricks selected', color = 'cluster')+
  theme(panel.grid = element_blank())

# ggsave(paste0("../../figures/plots/exp3_bricks_predicted_cluster.pdf"),width = 12,height = 6)


# ========================= -------------------------------------------------------------------
# EXP2 & 3: Demographics --------------------------------------------------------------------------------
rm(list=ls())
# Demographic data for EXP2 and EXP3 combined
load('towers_wide_exp2.RData')
load('towers_wide_exp3.RData')

df.wide = df.wide.exp2 %>% 
  rbind(df.wide.exp3)

nrow(df.wide) %>% print()
(df.wide$sex == 'female') %>% sum()
df.wide$age %>% as.numeric() %>% mean()
df.wide$age %>% as.numeric() %>% sd()
df.wide %>% group_by(experiment,condition) %>% summarise(mean.time = mean(time),sd.time = sd(time)) %>% 
  mutate_at(vars(contains("time")),funs(round(.,2)))





# ========================= -------------------------------------------------------------------
# EXP4 and 5: Trial info ---------------------------------------------

# Read in brick information 
filenames = list.files("../../data/stimuli_exp4/json") %>% mixedsort()

df.info.detailed = matrix(nrow=0,ncol=5) %>% 
  as.data.frame() %>% 
  setNames(c("y","x","angle","index","trial"))

for (i in 1:length(filenames)){
  tmp = fromJSON(file = paste0("../../data/stimuli_exp4/json/",filenames[i])) %>% 
    unlist() %>% 
    matrix(ncol=4,byrow=T) %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    setNames(c("y","x","angle","index")) %>% 
    mutate(index = str_replace(index,"brick_","") %>% as.numeric()) %>% 
    mutate(trial = i) %>% 
    filter(x != - 5)
  df.info.detailed = rbind(df.info.detailed,tmp)
}

# get the ground truth
tmp = fromJSON(file = "../../data/ground_truth_exp4_5.json")
ground_truth = matrix(nrow = 0, ncol = 2) %>%
  as.data.frame()
for (i in 1:length(tmp)){
  tmp2 = tmp[[i]] %>% unlist()
  ground_truth  = rbind(ground_truth,matrix(c(rep(i,length(tmp2)),tmp2),ncol=2))
}
ground_truth = ground_truth %>% setNames(c("trial", "index")) %>%
  mutate(fall = 1)

df.info.detailed = df.info.detailed %>% 
  select(trial,index,x,y,angle) %>% 
  filter(trial != 43) %>% 
  left_join(ground_truth) %>% 
  mutate(fall = na.replace(fall,0))

df.exp2.info = df.info.detailed
save("df.exp2.info", file = "exp2_info.RData")

# EXP4 and 5: Model predictions ---------------------------------------------------------------
location = "../../code/javascript/node/sim_archives/simulations13" #location of the simulations to be read in
filenames = list.files(location) %>% mixedsort()
nmodels = length(filenames)

df.percentages.prediction = df.info.detailed
nsims = 100 #number of simulations

for (f in 1:length(filenames)){
  #container data frame
  tmp = matrix(nrow = 0, ncol = 3) %>%
    as.data.frame() %>%
    setNames(c("index","trial",str_replace(filenames[f],".json","")))

  # extract data
  tmp1 = fromJSON(file = paste0(location,"/",filenames[f])) %>%
    lapply(str_c) %>%
    unlist() %>%
    matrix(ncol=42) %>%
    as.data.frame() %>%
    setNames(1:42) %>%
    gather(trial,prediction) %>%
    rowwise %>%
    mutate(prediction = prediction %>% str_extract_all("[0-9]+") %>% unlist() %>% as.numeric() %>% list()) %>%
    ungroup()

  for (i in 1:42){
    tmp2 = tmp1 %>%
      filter(trial == i) %>%
      select(prediction) %>%
      rbind(data.frame(prediction = 0)) %>% #add dummy to make sure data frame is not empty
      unlist() %>%
      table() %>%
      as.data.frame() %>%
      setNames(c("index",str_replace(filenames[f],".json",""))) %>%
      mutate(trial = i,
             index = index %>% as.character() %>% as.numeric())
    tmp = rbind(tmp,tmp2)
  }

  df.percentages.prediction = df.percentages.prediction %>%
    left_join(tmp)
}

df.percentages.prediction = df.percentages.prediction %>%
  na.replace(0)

df.exp2.models = df.percentages.prediction

# save("df.exp2.models", file = "exp2_models.RData")


# ========================= -------------------------------------------------------------------
# EXP4: Read in and structure data --------------------------------------------------------------------------
con = dbConnect(SQLite(),dbname = "../javascript/Experiment_4/participants.db");
# con = dbConnect(SQLite(),dbname = "../javascript/participants.db");
df.complete = dbReadTable(con,"towers")
dbDisconnect(con)

# read in ground truth 
# load("detailed_trial_info.RData")

df.experiment = df.complete %>% 
  filter(codeversion == 'experiment_4') %>% 
  filter(status %in% c(3,4,5))

ntrials = 43
nvariables = 3
variables.unstructured = names(fromJSON(df.experiment$datastring[1])$questiondata)
variables.additional = c("experiment","participant","workerid","taskid","time")
variables.task = paste(c("id","image","response"),rep(1:ntrials,each=nvariables),sep="_")

df.wide = data.frame(matrix(nrow=nrow(df.experiment),ncol=length(variables.unstructured)+
                              length(variables.additional)+length(variables.task)))
colnames(df.wide) = c(variables.additional,variables.task,variables.unstructured)

for (i in 1:nrow(df.wide)){
  a = fromJSON(df.experiment$datastring[i])
  # additional variables
  df.wide$experiment[i] = df.experiment$codeversion[i]
  df.wide$participant[i] = i
  df.wide$workerid[i] = a$workerId
  df.wide$taskid[i] = a$assignmentId
  df.wide$time[i] = as.numeric(as.POSIXct(df.experiment$endhit[i], format = "%Y-%m-%d %H:%M:%S")-
                                 as.POSIXct(df.experiment$beginhit[i], format = "%Y-%m-%d %H:%M:%S"))
  # unstructured variables 
  for (j in 1:length(variables.unstructured)){
    df.wide[[variables.unstructured[j]]][i] = a[["questiondata"]][[variables.unstructured[j]]]
  }
  
  # task variables 
  l = 1
  for (j in 1:ntrials){
    for (k in seq(2,by = 2, length.out = nvariables)){
      if (is.null(a$data[[j]]$trialdata[[k]])){
        b = NA;
      }else{
        b = a$data[[j]]$trialdata[[k]]
      }
      df.wide[[variables.task[l]]][i] = b
      l = l+1
    }
  }
}

df.long = df.wide %>% 
  wideToLong(within='order') %>% 
  select(participant,condition,order,id,response) %>%
  mutate(condition = factor(condition,levels = 0:1,labels = c('prediction','responsibility'))) %>% 
  rename(trial = id) %>% 
  arrange(participant,trial)

#exclude based on catch trial 
exclude = df.long %>% 
  filter(trial == 43) %>% 
  filter((condition == 'responsibility' & response > 15) |
           (condition == 'prediction' & response >= 1)) %>%
  select(participant) %>% 
  unlist() %>% 
  as.numeric()

# df.info = df.info.detailed %>% group_by(trial) %>% summarise(nbricks = n(), nfall = sum(fall), pfall = nfall/nbricks)

#update the trial info ... 

df.long = df.long %>%
  filter(!participant %in% exclude)

# filter out catch trial 
df.long = df.long %>% 
  filter(trial != 43)

df.exp2.prediction = df.long %>% filter(condition == 'prediction')
df.exp2.responsibility = df.long %>% filter(condition == 'responsibility')

save("df.exp2.prediction",file = "exp2_prediction.RData")
save("df.exp2.responsibility",file = "exp2_responsibility.RData")

df.wide = df.wide %>% 
  mutate(exclude = participant %in% exclude)

df.exp2.wide = df.wide
# ========================= -------------------------------------------------------------------
# EXP5: Read in and structure data --------------------------------------------------------------------------
# Reads in the data files and creates data frames
# rm(list = ls())
# load("detailed_trial_info.RData")
# load("towers_prediction_exp2.RData") #prediction data from EXP2

con = dbConnect(SQLite(),dbname = "../javascript/Experiment_5/participants.db");
# con = dbConnect(SQLite(),dbname = "../javascript/participants.db");
df.complete = dbReadTable(con,"towers")
dbDisconnect(con)

df.experiment = df.complete %>% 
  filter(codeversion == 'experiment_5') %>% 
  filter(status %in% c(3,4,5))

ntrials = 43
nvariables = 3
variables.unstructured = names(fromJSON(df.experiment$datastring[1])$questiondata)
variables.additional = c("experiment","participant","workerid","taskid","time")
variables.task = paste(c("id","image","response"),rep(1:ntrials,each=nvariables),sep="_")

df.wide = data.frame(matrix(nrow=nrow(df.experiment),ncol=length(variables.unstructured)+
                              length(variables.additional)+length(variables.task)))
colnames(df.wide) = c(variables.additional,variables.task,variables.unstructured)

for (i in 1:nrow(df.wide)){
  a = fromJSON(df.experiment$datastring[i])
  # additional variables
  df.wide$experiment[i] = df.experiment$codeversion[i]
  df.wide$participant[i] = i
  df.wide$workerid[i] = a$workerId
  df.wide$taskid[i] = a$assignmentId
  df.wide$time[i] = as.numeric(as.POSIXct(df.experiment$endhit[i], format = "%Y-%m-%d %H:%M:%S")-
                                 as.POSIXct(df.experiment$beginhit[i], format = "%Y-%m-%d %H:%M:%S"))
  # unstructured variables 
  for (j in 1:length(variables.unstructured)){
    df.wide[[variables.unstructured[j]]][i] = a[["questiondata"]][[variables.unstructured[j]]]
  }
  
  # task variables 
  l = 1
  for (j in 1:ntrials){
    for (k in seq(2,by = 2, length.out = nvariables)){
      if (length(a$data[[j]]$trialdata[[k]])==0){
        b = "";
      }else{
        b = a$data[[j]]$trialdata[[k]]
      }
      df.wide[[variables.task[l]]][i] = str_c(as.character(b),collapse=",")
      l = l+1
    }
  }
}

df.long = df.wide %>%
  wideToLong(within='order') %>%
  select(participant,order,id,response) %>%
  rename(trial = id) %>%
  mutate_each(funs(as.numeric(.)),trial,order) %>% 
  arrange(participant,trial) %>% 
  rowwise() %>% 
  mutate(response = response %>% str_split(",") %>% unlist() %>% as.numeric() %>% list()) %>% 
  mutate(number = length(response)) %>% 
  ungroup() %>% 
  mutate(number = ifelse(is.na(response),0,number))

#exclude based on catch trial
exclude = df.long %>%
  filter(trial == 43) %>%
  filter(number > 1) %>% 
  select(participant) %>%
  unlist() %>%
  as.numeric()

df.long = df.long %>%
  filter(!participant %in% exclude) %>% 
  filter(trial != 43) #remove the catch trial 
#   left_join(df.info %>% select(trial,initial,final,difference, index,trial_name, difference_proportion) %>% mutate_each(funs(.-1),difference) %>% rename(image = trial_name))

# df.exp2.selection = df.long 
# save("df.exp2.selection",file = "exp2_selection.RData")

# df.wide = df.wide %>% 
#   mutate(exclude = participant %in% exclude)
# 
# df.exp2.wide = rbind(df.exp2.wide,df.wide)
# save("df.exp2.wide",file = "exp2_wide.RData")


# # merge prediction data about how many bricks will fall from EXP2 and EXP3 
# df.prediction = df.prediction %>% 
#   rbind(df.long %>% 
#           group_by(trial) %>% 
#           summarise(prediction_mean = smean.cl.boot(number)[1],
#                     prediction_low = smean.cl.boot(number)[2],
#                     prediction_high = smean.cl.boot(number)[3]) %>%
#           ungroup %>%
#           mutate(experiment = 'selection')
#   )
# 
# percentages of bricks falling
df.percentages = matrix(ncol=3,nrow=0) %>%
  as.data.frame() %>%
  setNames(c("index","freq","trial"))

for (i in 1:ntrials){
  tmp = df.long %>%
    filter(trial == i) %>%
    select(response) %>%
    rbind(data.frame(response = 0)) %>% #add dummy observation to deal with empty data frames
    unlist() %>%
    table() %>%
    as.data.frame() %>%
    setNames(c("index","freq")) %>%
    mutate(trial = i)

  df.percentages = rbind(df.percentages,tmp)
}

df.percentages = df.percentages %>%
  mutate(freq = ifelse(is.na(freq),0,freq),
         freq = ifelse(index == 0, 0, freq),
         freq = freq/nrow(df.wide)*100, #only works as long as we don't exclude any participants
         index = index %>% as.character() %>% as.numeric())

#merge with information about the trials
df.percentages = df.percentages %>%
  left_join(df.info.detailed,.) %>%
  select(trial,index,freq) %>%
  filter(index != 0) %>%
  na.replace(0)

#remove the special brick from the predictions
df.percentages.prediction = df.percentages.prediction %>%
  filter(index != 0) %>% 
  left_join(df.percentages)

# df.wide.exp3 = df.wide
# save("df.wide.exp3",file = 'towers_wide_exp3.RData')



# EXP5: Generate images with labeled bricks (no need to run) -----------------------------------------------------------------

# Read in brick information 
filenames = list.files("../../data/stimuli_exp4/json") %>% mixedsort()
imagenames = list.files("../../figures/stimuli/experiment4/initial_small")  %>% mixedsort()
# imagenames = imagenames[str_detect(imagenames,"initial")] %>% mixedsort()

for (i in 1:length(filenames)){

  tmp = fromJSON(file = paste0("../../data/stimuli_exp4/json/",filenames[i])) %>% 
    unlist() %>% 
    matrix(ncol=4,byrow=T) %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    setNames(c("y","x","angle","index")) %>% 
    mutate(index = str_replace(index,"brick_","")) %>% 
    mutate_all(funs(as.numeric(.))) %>% 
    mutate_each(funs(.*100),x,y) %>% 
    mutate(y = 600-y) %>% 
    filter(x != -500)
  
  df.plot = tmp 
  
  image = png::readPNG(paste0("../../figures/stimuli/experiment4/initial_small/",imagenames[i]))
  ggplot(df.plot, aes(x = x, y = y)) +
    annotation_custom(rasterGrob(image, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      xmin = 0, xmax = 800, ymin = 0, ymax = 600) +
    geom_text(aes(label = index), size = 7, fontface = 2, 
              colour = 'black', hjust = 0.5, vjust = 0.5)+
    scale_x_continuous(expand = c(0,0), limits = c(0,800))+
    scale_y_continuous(expand = c(0,0), limits = c(0,600))+
    theme_void()+
    theme(axis.ticks.length = unit(0.001, "mm"))
  
  ggsave(paste0("../../figures/stimuli/experiment5/initial_labeled/",str_replace(imagenames[i],".png",""),".pdf"),width=8,height=6)
}

# EXP5: Generate images with marked bricks that will fall (no need to run) -----------------------------------------------------------------

# Read in brick information 
filenames = list.files("../../data/stimuli_exp4/json") %>% mixedsort()
imagenames = list.files("../../figures/stimuli/experiment4/initial_small")  %>% mixedsort()
# imagenames = imagenames[str_detect(imagenames,"initial")] %>% mixedsort()

for (i in 1:(length(filenames)-1)){
# for (i in 3){
  tmp = fromJSON(file = paste0("../../data/stimuli_exp4/json/",filenames[i])) %>% 
    unlist() %>% 
    matrix(ncol=4,byrow=T) %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    setNames(c("y","x","angle","index")) %>% 
    mutate(index = str_replace(index,"brick_","")) %>% 
    mutate_all(funs(as.numeric(.))) %>% 
    mutate_each(funs(.*100),x,y) %>% 
    mutate(y = 600-y) %>% 
    filter(x != -500) %>% 
    mutate(trial = i) %>% 
    left_join(df.info.detailed %>% select(trial,index,fall)) %>% 
    filter(fall == 1)
  
  df.plot = tmp 
  
  image = png::readPNG(paste0("../../figures/stimuli/experiment4/initial_small/",imagenames[i]))
  ggplot(df.plot, aes(x = x, y = y)) +
    annotation_custom(rasterGrob(image, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      xmin = 0, xmax = 800, ymin = 0, ymax = 600) +
    # geom_text(aes(label = index), size = 7, fontface = 2, 
    #           colour = 'black', hjust = 0.5, vjust = 0.5)+
    geom_point(color = 'white', size = 5)+
    scale_x_continuous(expand = c(0,0), limits = c(0,800))+
    scale_y_continuous(expand = c(0,0), limits = c(0,600))+
    theme_void()+
    theme(axis.ticks.length = unit(0.001, "mm"))
  
  ggsave(paste0("../../figures/stimuli/experiment5/initial_marked/","trial",i,"_marked.png"),width=8,height=6,dpi=100)
}

# EXP5: PLOT - Images that show percentage of selected bricks  -----------------------------------------------------------------

# Read in brick information 
filenames = list.files("../../data/stimuli_exp4/json") %>% mixedsort()
imagenames = list.files("../../figures/stimuli/experiment4/initial") %>% mixedsort

load("exp2_models.RData") #load model data 

# modelindex = "freq"
# modelindex = "impulse-local-above-extended_12.5_0.5"
# modelindex = "impulse-local_7.2_0.5"
# modelindex = "impulse-global_2.5_0.5"

# for (i in 1){
for (i in 1:length(imagenames)){
  
  tmp = fromJSON(file = paste0("../../data/stimuli_exp4/json/",filenames[i])) %>% 
    unlist() %>% 
    matrix(ncol=4,byrow=T) %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    setNames(c("y","x","angle","index")) %>% 
    mutate(index = str_replace(index,"brick_","")) %>% 
    mutate_all(funs(as.numeric(.))) %>% 
    mutate_each(funs(.*100),x,y) %>% 
    mutate(y = 600-y) %>% 
    filter(x != -500) %>% 
    # left_join(df.percentages.prediction %>% filter(trial == i) %>% select(index,one_of(modelindex))) %>% 
    left_join(df.exp2.models %>% filter(trial == i) %>% select(index,one_of(modelindex))) %>%
    setNames(c("y","x","angle","index","freq")) %>% 
    mutate(freq = ifelse(index == 0, 0, freq),
           freq = freq %>% round(0),
           freq = paste0(freq,"%")) %>% 
    filter(index != 0)
  
  df.plot = tmp
  
  image = png::readPNG(paste0("../../figures/stimuli/experiment4/initial/",imagenames[i]))
  ggplot(df.plot, aes(x = x, y = y)) +
    annotation_custom(rasterGrob(image, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      xmin = 0, xmax = 800, ymin = 0, ymax = 600) +
    geom_text(aes(label = freq), size = 5, fontface = 2, 
              colour = 'black', hjust = 0.5, vjust = 0.5)+
    scale_x_continuous(expand = c(0,0), limits = c(0,800))+
    scale_y_continuous(expand = c(0,0), limits = c(0,600))+
    theme_void()+
    theme(axis.ticks.length = unit(0.001, "mm"))
  
  ggsave(paste0("../../figures/plots/selected_bricks/exp5_",modelindex,"_trial_",i,".png"),width=8,height=6,dpi=100)
}
