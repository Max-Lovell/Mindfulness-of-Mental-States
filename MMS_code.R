#SETUP#######################
# Load packages and data ---------------------------
#NOTE: if there are issue with code, makes sure dplyr functions haven't been masked  
#invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
list.of.packages <- c("car", "pastecs", "stats", "data.table", "papaja", "emmeans", "Rmisc", "tidyverse", "dplyr") 
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, require, character.only = TRUE)

mindful_data <- read.csv("mindful_data.csv") %>% 
  mutate_if(is.character, factor) %>% 
  dplyr::mutate(condition = fct_relevel(condition, "Waitlist", "World", "Mental"))

# Missing data ---------------------------

#survey website allowed people to continue without answering every question
  pre_range <- c(which(colnames(mindful_data) == "AWH_pre"):which(colnames(mindful_data) == "PWB_18_pre"))
    missing_pre <- sum(!complete.cases(mindful_data[pre_range])) #save count for later
  mindful_data <- mindful_data[complete.cases(mindful_data[pre_range]), ] #delete pre-test missing
#delete post-test missing questions, but not those who missed the whole second session
  post_range <- c(which(colnames(mindful_data) == "AWH_post"):which(colnames(mindful_data) == "PWB_18_post"))
      missing_post <- sum(!complete.cases(mindful_data[!is.na(mindful_data$URN_post),post_range])) #save count for later
    index_missing <- is.na(mindful_data$URN_post) #index of those that missed second session
    mindful_data_index <- mindful_data #create duplicate dataset
    mindful_data_index[index_missing,post_range] <- mindful_data_index[index_missing,pre_range] #Dummy ITT data
    mindful_data <- mindful_data[complete.cases(mindful_data_index[post_range]), ] #those missing some but not all data

# Scale Re-coding ---------------------------
PSS_recode <- c(1:4,9) %>% as.character() %>%
    {c(paste0("PSS_",.,"_pre"), paste0("PSS_",.,"_post"))}
  mindful_data[PSS_recode] <- mindful_data[PSS_recode] - 1
PHQ_recode <- c(1:4) %>% as.character() %>%
    {c(paste0("PHQ_",.,"_pre"), paste0("PHQ_",.,"_post"))}
  mindful_data[PHQ_recode] <- mindful_data[PHQ_recode] - 1

#Reverse Coding
FFMQ_reverse <- c(4,5,7,8,11,12,14,17,19,22:24) %>% as.character() %>%
    {c(paste0("FFMQ_",.,"_pre"), paste0("FFMQ_",.,"_post"))}
  mindful_data[FFMQ_reverse] <- 6 - mindful_data[FFMQ_reverse]
PSS_reverse <- c(4,5,7,8) %>% as.character() %>%
    {c(paste0("PSS_",.,"_pre"), paste0("PSS_",.,"_post"))}
  mindful_data[PSS_reverse] <- 5 - mindful_data[PSS_reverse]
PWB_reverse <- c(2,4,6,10,11,14,16) %>% as.character() %>%
    {c(paste0("PWB_",.,"_pre"), paste0("PWB_",.,"_post"))}
  mindful_data[PWB_reverse] <- 7 - mindful_data[PWB_reverse]

# Scale Objects ---------------------------      
DS <- c(1,2,5,11,16) %>% as.character()
  DS_pre <- {paste0("FFMQ_",DS,"_pre")}
  DS_post <- {paste0("FFMQ_",DS,"_post")}
NR <- c(3,9,13,18,21) %>% as.character()
  NR_pre <- {paste0("FFMQ_",NR,"_pre")}
  NR_post <- {paste0("FFMQ_",NR,"_post")}
NJ <- c(4,7,14,19,24) %>% as.character()
  NJ_pre <- {paste0("FFMQ_",NJ,"_pre")}
  NJ_post <- {paste0("FFMQ_",NJ,"_post")}
OB <- c(6,10,15,20) %>% as.character()
  OB_pre <- {paste0("FFMQ_",OB,"_pre")}
  OB_post <- {paste0("FFMQ_",OB,"_post")}
AA <- c(8,12,17,22,23) %>% as.character()
  AA_pre <- {paste0("FFMQ_",AA,"_pre")}
  AA_post <- {paste0("FFMQ_",AA,"_post")}
#FFMQ effective scales
  OBAA_pre <- c(OB_pre,AA_pre)
  OBAA_post <- c(OB_post,AA_post)
PSS <- c(1:10) %>% as.character()
  PSS_pre <- {paste0("PSS_",PSS,"_pre")}
  PSS_post <- {paste0("PSS_",PSS,"_post")}
PHQ <- c(1:4) %>% as.character()
  PHQ_pre <- {paste0("PHQ_",PHQ,"_pre")}
  PHQ_post <- {paste0("PHQ_",PHQ,"_post")}
RRS <- c(1:22) %>% as.character()
  RRS_pre <- {paste0("RRS_",RRS,"_pre")}
  RRS_post <- {paste0("RRS_",RRS,"_post")}
WBSI <- c(1:15) %>% as.character()
  WBSI_pre <- {paste0("WBSI_",WBSI,"_pre")}
  WBSI_post <- {paste0("WBSI_",WBSI,"_post")}
PWB <- c(1:18) %>% as.character()
  PWB_pre <- {paste0("PWB_",PWB,"_pre")}
  PWB_post <- {paste0("PWB_",PWB,"_post")}
#Awareness last hour
  AH_pre <- c("AMH_pre","AWH_pre")
  AH_post <- c("AMH_post","AWH_post")

#list of all scales, and pre and post separately
  scales <- c("AMH","AWH","OB","NR","DS","NJ","AA","OBAA","PSS","PHQ","RRS","WBSI","PWB")

    for(scale in scales){
      if(scale == scales[1]){
        scales_pre <- paste0(scale,"_pre")
        scales_post <- paste0(scale,"_post")
        scales_time <- c(scales_pre,scales_post)
      }
      else{
        scales_pre <- c(scales_pre,paste0(scale,"_pre"))
        scales_post <- c(scales_post,paste0(scale,"_post"))
        scales_time <- c(scales_time,paste0(scale,"_pre"),paste0(scale,"_post"))
      }
    }
# Mean Scores --------------------------- 
  for(scale in scales_time){
    if(grepl("^A.H",scale)){next}
    measure <- mindful_data[get(scale)]
    varname <- paste0(scale)
    mindful_data <- dplyr::mutate(mindful_data,!!varname := rowMeans(measure), na.rm=TRUE)
  } 
  
mindful_data <- mindful_data[,!(names(mindful_data) %in% c("na.rm"))] %>% 
  dplyr::mutate(expect_mean = rowMeans(mindful_data[c("expect_anx","expect_dep")], na.rm=TRUE))

#Intention-to-treat
  mindful_data[is.na(mindful_data$URN_post),scales_post] <- mindful_data[is.na(mindful_data$URN_post),scales_pre]
  
#Difference scores
  for(scale in scales){
    mindful_data[paste0(scale,"_diff")] <- mindful_data[paste0(scale,"_post")] - mindful_data[paste0(scale,"_pre")]
  }
  
# Filtered Datasets ---------------------------   
  waitlist <- mindful_data[mindful_data$condition == "Waitlist",]
  world <- mindful_data[mindful_data$condition == "World",]
  mental <- mindful_data[mindful_data$condition == "Mental",]
#DATA EXPLORATION#######################
# Descriptives ---------------------------

#Demographics
mindful_data %>% {tibble(Variable = c(levels(.$condition),levels(.$gender),"Age","Range"),
                         Description = c(sapply(Variable[1:3], function(x) sum(.$condition == x)),
                                       sapply(Variable[4:5], function(x) sum(.$gender == x)),
                                       printnum(mean(.$age)),paste0(min(.$age),"-",max(.$age))))} %>% 
    write.csv("demographics.csv")

# Missing data
  missing_pre_cond <- 69 #missing all pre-test and condition
tibble(Reason = c("Missing-at-Random", "Dropouts", "Intention-to-Treat"),
       Count  = c(missing_pre_cond + missing_pre + missing_post, sum(is.na(mindful_data$URN_post)), 
                  NROW(mindful_data))) %>% 
  write.csv("missing.csv")

# Descriptives
lapply(levels(mindful_data$condition), function(x){
  tibble("{x}_Pre" := sapply(scales_pre, function(y){paste0(printnum(mean(mindful_data[which(mindful_data$condition==x),y])),
                                                      " (",printnum(sd(mindful_data[which(mindful_data$condition==x),y])),")")}),
         "{x}_Post" := sapply(scales_post, function(z){paste0(printnum(mean(mindful_data[which(mindful_data$condition==x),z])),
                                                        " (",printnum(sd(mindful_data[which(mindful_data$condition==x),z])),")")})
         )}) %>% 
  bind_cols %>% dplyr::mutate(Scale = scales, .before = Waitlist_Pre) %>% 
  write.csv("descriptives.csv")

# Cronbach's Alpha
lapply(scales, function(x){
  if(grepl("^A.H",x)){return()}
  tibble(Scale = x,
         Pre = psych::alpha(mindful_data[get(paste0(x,"_pre"))])$total$raw_alpha,
         Post = psych::alpha(mindful_data[get(paste0(x,"_post"))])$total$raw_alpha
  )}) %>% bind_rows %>% printnum %>%
  write.csv("cronbachs.csv")

# Bargraphs ---------------------------   

melt_se <- function(dataset){
  scale_melt <- gather(dplyr::select(dataset, scales_time, condition, URN), scale, score, -URN, -condition)
  scale_melt$time <- gl(2, NROW(dataset), labels=c("Pre-test","Post-test"))
  scale_melt$measure <- gl(NROW(scales), NROW(dataset)*2, labels=scales)
  scale_SE <- summarySE(scale_melt, measurevar="score", groupvars=c("measure","time","condition"))
  return(scale_SE)
}
mindful_melt <- melt_se(mindful_data)

  ggplot2::ggplot(mindful_melt[mindful_melt$measure == "AWH"|
                               mindful_melt$measure == "OB"| 
                               mindful_melt$measure == "PHQ",], 
    aes(condition, score, fill=time))+
    geom_bar(position=position_dodge(),width=.7,stat="identity")+
    geom_errorbar(aes(ymin=score-ci, ymax=score+ci), width=.2, position=position_dodge(.7))+
    facet_wrap(~measure, scales = "free")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    guides(fill=guide_legend(title="Time"))+ 
    labs(x = "Condition", y = "Score (+/- C.I. 95%)")
  
  ggsave("Bargraphs_free.png",  width = 20, height = 10, units = "cm")

#INFERENTIAL#######################
# Bayes Factor function---------------------------
Bf<-function(sd, obtained, dfdata = 1, likelihood = c("normal", "t"), 
             modeloftheory= c("normal","t","cauchy", "uniform") ,lower =0, 
             upper=1, modeoftheory = 0, scaleoftheory = 1, dftheory = 1, tail = 2)
{
  if(likelihood=="normal"){
    dfdata=10^10
  }
  if(modeloftheory=="normal"){
    dftheory = 10^10
  } else if(modeloftheory=="cauchy"){
    dftheory = 1
  }
  area <- 0
  normarea <- 0
  if(modeloftheory=="uniform"){
    theta <- lower
    range <- upper - lower
    incr <- range / 2000
    for (A in -1000:1000){
      theta <- theta + incr
      dist_theta <- 1 / range
      height <- dist_theta * dt((obtained-theta)/sd, df=dfdata)
      area <- area + height * incr
    }
    LikelihoodTheory <- area
  }else{
    theta <- modeoftheory - 10 * scaleoftheory
    incr <- scaleoftheory/200
    for (A in -2000:2000){
      theta <- theta + incr
      dist_theta <- dt((theta-modeoftheory)/scaleoftheory, df=dftheory)
      if(identical(tail, 1)){
        if (theta <= modeoftheory){
          dist_theta <- 0
        } else {
          dist_theta <- dist_theta * 2
        }
      }
      height <- dist_theta * dt((obtained-theta)/sd, df = dfdata)
      area <- area + height * incr
      normarea <- normarea + dist_theta*incr
    }
    LikelihoodTheory <- area/normarea
  }
  Likelihoodnull <- dt(obtained/sd, df = dfdata)
  BayesFactor <- LikelihoodTheory/Likelihoodnull
  BayesFactor
}

# Expectations ---------------------------
#Note: data loss: these were collected with emails
#Datasets
exp_data <- mindful_data %$%
  {!(is.na(expect_anx) | is.na(expect_dep)) & (condition == "World" | condition == "Mental")} %>% 
  {mindful_data[.,grepl("^expect_|^condition$", names(mindful_data))]} %>% droplevels

#T-test
exp_t <- lapply(exp_data[2:4], function(x){
    t.test(x ~ condition, exp_data, var.equal=TRUE) %$% 
      c(printnum(statistic[[1]]), printp(p.value), printnum(conf.int)) %>%
      set_names(c("t","p","CI-","CI+"))
  }) %>% bind_rows %>% unite("CI", "CI-","CI+", sep=", ")

#H1 calculation
    #observed difference = .27 (we use .2, see paper):
      #{mean(mindful_data[mindful_data$condition == "Mental","PHQ_diff"]) -
      #mean(mindful_data[mindful_data$condition == "World","PHQ_diff"])} %>% abs %>% round(digits = 2)
exp_prior <- lm(expect_mean ~ PHQ_pre + PHQ_post, data=mindful_data) %>% summary() %$%
  {coefficients[[3,1]] + (coefficients[[3,2]]*1.96)} %>% round(digits = 1) %>% 
  {.*0.2/2} %>% round(digits = 2) #minimally interesting difference = observed diff * 1/2 max predicted change 

exp_data %>%
  group_by(condition) %>%
  dplyr::summarise(across(starts_with("expect"), 
                   list(mean = mean, n = length, se = ~sd(.x)/sqrt(length(.x))
                   ))) %>% printnum %>%
  {tibble(Outcome = c("Anxiety", "Depression", "Combined"),
          Diff = printnum(abs(as.numeric(unlist(.[2,grepl("_mean$",names(.))])) - 
                                as.numeric(unlist(.[1,grepl("_mean$",names(.))])))),
          SE = printnum(sqrt((as.numeric(unlist(.[2,grepl("_se",names(.))]))^2) +
                               (as.numeric(unlist(.[1,grepl("_se",names(.))]))^2))),
          Diff_SE = paste0(Diff," (",SE,")"),
          Bf = printnum(Bf(as.numeric(SE), as.numeric(Diff), 39, "normal", "normal", 
                  modeoftheory = 0, scaleoftheory = exp_prior, dftheory = 100, tail = 1))
  )} %>% select(-Diff,-SE) %>% add_column(exp_t) %>% 
  write.csv("expectations.csv")

# Analysis ---------------------------
contrasts = list(Main    = c(0, -1, 1),
                 Control = c(-1, 1, 0))

lapply(paste0(scales,"_diff"), function(x){
  contrast_lm <- lm(get(x) ~ condition, mindful_data) %>%
        emmeans(., "condition", contr = contrasts) %$% contrasts %>% 
        c(summary(.), apa_print(.))
  
  ifelse(grepl("^A.H",x), H1<-.25, H1<-.2)
  
  tibble(Scales          = sub("_diff", "", x),
         Active_mean     = mean(mental[,x]) - mean(world[,x]),
         Active_SE       = sqrt((sd(mental[,x])/sqrt(NROW(mental))) ^2+
                                (sd(world[,x])/sqrt(NROW(world))) ^2),
         Active          = paste0(printnum(Active_mean)," (",printnum(Active_SE),")"),
         Active_BF       = Bf(round(Active_SE,2), round(abs(Active_mean),2), NROW(mental) + NROW(world) - 2, 
                              "normal", "normal", modeoftheory = 0, scaleoftheory = H1, dftheory = 102, tail = 1),
        "Active_t(119)" := contrast_lm$t.ratio[1],
         Active_p        = contrast_lm$p.value[1],

         Control_mean    = mean(world[,x]) - mean(waitlist[,x]),
         Control_SE      = sqrt((sd(world[,x])/sqrt(NROW(world))) ^2+
                                (sd(waitlist[,x])/sqrt(NROW(waitlist))) ^2),
         Control          = paste0(printnum(Control_mean)," (",printnum(Control_SE),")"),
         Control_BF      = Bf(round(Control_SE,2), round(abs(Control_mean),2), NROW(world) + NROW(waitlist) - 2, 
                              "normal", "normal", modeoftheory = 0, scaleoftheory = H1, dftheory = 102, tail = 1),
        "Control_t(119)" = contrast_lm$t.ratio[2],
         Control_p       = contrast_lm$p.value[2]
  )}) %>% bind_rows %>% select(-ends_with("_mean"), -ends_with("_SE")) %>%
  dplyr::mutate(across(ends_with("_p"), printp), across(!ends_with("_p"), printnum)) %>%
  write.csv("analysis.csv")
  
  
#Extra#######################
# Sample Size Estimation ----
n_size <- function(var, N, H = .2){
  N = N/3
  diff <- paste0(var,"_diff") #variable
  #from this data: H1 = round(mean(mental[,diff]) - mean(world[,diff]), digits = 2)
  df_world <- NROW(mental) + NROW(world) - 2 ; harmonic_m <- 43
  #raw effects
  SE_world <- sd(world[,diff])/sqrt(NROW(world))                                        
  SE_mental <- sd(mental[,diff])/sqrt(NROW(mental)) 
  SE_diff <- round(sqrt(SE_mental^2+SE_world^2), digits = 2)
  SE_estimate <- SE_diff*sqrt(harmonic_m/N)    
  #estimate (change H1 to 0 if needed)
  Bf(SE_estimate, H, 100,"normal", "normal", modeoftheory = 0, 
     scaleoftheory = .2, dftheory = df_world, tail = 1) %>% 
    round(digits = 2) %>% paste("BF =",.)
  #print(SE_diff)
}

n_size("PHQ",438, H=0)


# Robustness Regions ----
       #Active       #Control
#AMH	 .45,.14       .55,.66
#AWH	 .45,.62       .57,1.74
#OB    .13,.23       .13,.01
#NR    .15,.1        .14,0
#DS    .12,.07       .13,.08
#NJ	   .16,.05       .15,.12
#AA    .16,.25       .15,.04
#OBAA  .12,.24       .11,.01
#PSS	 .1,0          .11,.05
#PHQ	 .13,.27       .14,.04
#RRS	 .09,.12       .09,.06
#WBSI	 .13,.15       .11,.05
#PWB	 .1,.17        .08,.07

#Expectations  
#Anxiety .35,.14
#Depression	.32,.28
#Combined .30,.21 

Bf(.13,.23, scaleoftheory = .2, 39, "normal", "normal", modeoftheory = 0, 
   dftheory = 100, tail = 1) %>% printnum

# OB question means -----
lapply(levels(mindful_data$condition), function(x){
  tibble("{x}" := sapply(OB, function(y){mean(mindful_data[which(mindful_data$condition==x),paste0("FFMQ_",y,"_post")] - 
                           mindful_data[which(mindful_data$condition==x),paste0("FFMQ_",y,"_pre")], na.rm=T)}))}) %>%
  bind_cols %>% printnum %>% dplyr::mutate(Q = paste0("#", OB), .before = Waitlist) %>%
  write.csv("abc.csv")




# PHQ separate anx and dep -----
mindful_data_PHQ <- mindful_data

PHQ_anx <- c(1,2) %>% as.character()
PHQ_anx_pre <- {paste0("PHQ_",PHQ_anx,"_pre")}
PHQ_anx_post <- {paste0("PHQ_",PHQ_anx,"_post")}
PHQ_dep <- c(3,4) %>% as.character()
PHQ_dep_pre <- {paste0("PHQ_",PHQ_dep,"_pre")}
PHQ_dep_post <- {paste0("PHQ_",PHQ_dep,"_post")}

PHQ_scales <- c("PHQ_anx_pre", "PHQ_anx_post", "PHQ_dep_pre", "PHQ_dep_post")
#means pre and post
for(scale in PHQ_scales){
  measure <- mindful_data_PHQ[get(scale)]
  varname <- paste0(scale)
  mindful_data_PHQ <- dplyr::mutate(mindful_data_PHQ,!!varname := rowMeans(measure), na.rm=TRUE)
} 

#Intention-to-treat
mindful_data_PHQ[is.na(mindful_data_PHQ$URN_post),"PHQ_anx_post"] <- mindful_data_PHQ[is.na(mindful_data_PHQ$URN_post),"PHQ_anx_pre"]
mindful_data_PHQ[is.na(mindful_data_PHQ$URN_post),"PHQ_dep_post"] <- mindful_data_PHQ[is.na(mindful_data_PHQ$URN_post),"PHQ_dep_pre"]

#Difference scores
mindful_data_PHQ$PHQ_anx_diff <- mindful_data_PHQ$PHQ_anx_post - mindful_data_PHQ$PHQ_anx_pre
mindful_data_PHQ$PHQ_dep_diff <- mindful_data_PHQ$PHQ_dep_post - mindful_data_PHQ$PHQ_dep_pre


waitlist_PHQ <- mindful_data_PHQ[mindful_data_PHQ$condition == "Waitlist",]
world_PHQ <- mindful_data_PHQ[mindful_data_PHQ$condition == "World",]
mental_PHQ <- mindful_data_PHQ[mindful_data_PHQ$condition == "Mental",]


contrasts = list(Main    = c(0, -1, 1),
                 Control = c(-1, 1, 0))

lapply(c("PHQ_anx_diff","PHQ_dep_diff"), function(x){
  print(x)
  contrast_lm <- lm(get(x) ~ condition, mindful_data_PHQ) %>%
    emmeans(., "condition", contr = contrasts) %$% contrasts %>% 
    c(summary(.), apa_print(.))
H1<-.2
tibble(Scales          = sub("_diff", "", x),
         Active_mean     = mean(mental_PHQ[,x]) - mean(world_PHQ[,x]),
         Active_SE       = sqrt((sd(mental_PHQ[,x])/sqrt(NROW(mental_PHQ))) ^2+
                                  (sd(world_PHQ[,x])/sqrt(NROW(world_PHQ))) ^2),
         Active          = paste0(printnum(Active_mean)," (",printnum(Active_SE),")"),
         Active_BF       = Bf(round(Active_SE,2), round(abs(Active_mean),2), NROW(mental_PHQ) + NROW(world_PHQ) - 2, 
                              "normal", "normal", modeoftheory = 0, scaleoftheory = H1, dftheory = 102, tail = 1),
         "Active_t(119)" := contrast_lm$t.ratio[1],
         Active_p        = contrast_lm$p.value[1],
         
         Control_mean    = mean(world_PHQ[,x]) - mean(waitlist_PHQ[,x]),
         Control_SE      = sqrt((sd(world_PHQ[,x])/sqrt(NROW(world_PHQ))) ^2+
                                  (sd(waitlist_PHQ[,x])/sqrt(NROW(waitlist_PHQ))) ^2),
         Control          = paste0(printnum(Control_mean)," (",printnum(Control_SE),")"),
         Control_BF      = Bf(round(Control_SE,2), round(abs(Control_mean),2), NROW(world_PHQ) + NROW(waitlist_PHQ) - 2, 
                              "normal", "normal", modeoftheory = 0, scaleoftheory = H1, dftheory = 102, tail = 1),
         "Control_t(119)" = contrast_lm$t.ratio[2],
         Control_p       = contrast_lm$p.value[2]
  )}) %>% bind_rows %>% select(-ends_with("_mean"), -ends_with("_SE")) %>%
  dplyr::mutate(across(ends_with("_p"), printp), across(!ends_with("_p"), printnum))
