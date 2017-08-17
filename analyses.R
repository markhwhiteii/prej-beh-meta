# packages
purrr::walk(c("dplyr", "robumeta", "clubSandwich", "psych", "ggplot2", "tidyr", "lme4", "metafor"), library, character.only=TRUE)

# loading and formatting data
meta <- readr::read_csv("data.csv") %>% 
  mutate(effectid=as.factor(effectid), studyid=as.factor(studyid), 
         attitude=as.factor(attitude), paradigm=as.factor(paradigm),
         transformed=as.factor(transformed))

# data without other and unsure attitudes
meta_noatt3 <- meta %>% 
  filter(attitude != 3) %>% 
  droplevels.data.frame()

# no self-reports
meta_nosr <- meta %>% 
  filter(!(paradigm %in% c("p1", "p2", "p3", "p4")))

# how many transformed?
table(meta$transformed)

# how many effect sizes of each attitude type?
table(meta$attitude)

# how many studies?
length(levels(meta$studyid))

# intercept only
mod0.rve <- robu(formula=es ~ 1, 
                 data=meta, studynum=studyid, var.eff.size=var, 
                 modelweights="CORR")

mod0.rve$reg_table[1,c(2,7,8)] %>% 
  unname() %>% 
  unlist() %>% 
  sapply(transf.ztor) # converting estimate and CI to r

# distribution of effect sizes
ggplot(meta, aes(r))+
  geom_vline(xintercept=transf.ztor(mod0.rve$reg_table[1,2]), linetype=2)+
  geom_histogram(binwidth=.03, alpha=.8)+
  theme_light()+
  labs(x="Effect Size", y="Count") +
  annotate("text", x=.33, y=17, label="r = .21")

# pub year
mod1.rve <- robu(formula=es ~ pubyear, 
                 data=meta, studynum=studyid, var.eff.size=var, 
                 modelweights="CORR")

transf.ztor(mod1.rve$reg_table[1,2]) # predicted value at oldest

round(mod1.rve$reg_table[1,2] + 
        (mod1.rve$reg_table[2,2] * max(meta$pubyear)),2) %>% 
  transf.ztor() # predicted value at most recent

# early year
mod2.rve <- robu(formula=es ~ earlyyear, 
                 data=meta, studynum=studyid, var.eff.size=var, 
                 modelweights="CORR")

transf.ztor(mod2.rve$reg_table[1,2]) # predicted value at oldest

round(mod2.rve$reg_table[1,2] + 
        (mod2.rve$reg_table[2,2] * max(meta$earlyyear)),2) %>% 
  transf.ztor() # predicted value at most recent

# pub year, all dat
mod3.rve <- robu(formula=es ~ pubyear + subtle + attitude + transformed, 
                 data=meta, studynum=studyid, var.eff.size=var, 
                 modelweights="CORR")
Wald_test(mod3.rve, 4:6, "CR2") # omnibus for attitudes
Wald_test(mod3.rve, 2:6, "CR2") # omnibus for all mods

# early year, all dat
mod4.rve <- robu(formula=es ~ earlyyear + subtle + attitude + transformed, 
                 data=meta, studynum=studyid, var.eff.size=var, 
                 modelweights="CORR")
Wald_test(mod4.rve, 4:6, "CR2") # omnibus for attitudes
Wald_test(mod4.rve, 2:6, "CR2") # omnibus for all mods

# pub year, no att 3
mod5.rve <- robu(formula=es ~ pubyear + subtle + attitude + transformed, 
                 data=meta_noatt3, studynum=studyid, var.eff.size=var, 
                 modelweights="CORR")
Wald_test(mod5.rve, 4:5, "CR2") # omnibus for attitudes
Wald_test(mod5.rve, 2:5, "CR2") # omnibus for all mods

# early year, no att 3
mod6.rve <- robu(formula=es ~ earlyyear + subtle + attitude + transformed, 
                 data=meta_noatt3, studynum=studyid, var.eff.size=var, 
                 modelweights="CORR")
Wald_test(mod6.rve, 4:5, "CR2") # omnibus for attitudes
Wald_test(mod6.rve, 2:5, "CR2") # omnibus for all mods

# just subtlety
mod7.rve <- robu(formula=es ~ subtle, 
                 data=meta, studynum=studyid, var.eff.size=var, 
                 modelweights="CORR")

# just attitude, all dat
mod8.rve <- robu(formula=es ~ attitude, 
                 data=meta, studynum=studyid, var.eff.size=var, 
                 modelweights="CORR")
Wald_test(mod8.rve, 2:4, "CR2") # omnibus for attitudes

# just attitude, no att 2
mod9.rve <- robu(formula=es ~ attitude, 
                 data=meta_noatt3, studynum=studyid, var.eff.size=var, 
                 modelweights="CORR")
Wald_test(mod9.rve, 2:3, "CR2") # omnibus for attitudes

# sensitivity
c("mod0.rve", "mod1.rve", "mod2.rve", "mod3.rve", "mod4.rve", "mod5.rve", "mod6.rve") %>% 
  lapply(function(x) sensitivity(get(x)))

# research trends
# subtlety
subtle0 <- lmer(subtle ~ (1|studyid), data=meta, REML=FALSE)
subtle1 <- lmer(subtle ~ pubyear + (1|studyid), data=meta, REML=FALSE)
summary(subtle1)$coef
anova(subtle0, subtle1)
subtle2 <- lmer(subtle ~ earlyyear + (1|studyid), data=meta, REML=FALSE)
summary(subtle2)$coef
anova(subtle0, subtle2)

# subtlety, after taking out self-reports
subtle3 <- lmer(subtle ~ (1|studyid), data=meta_nosr, REML=FALSE)
subtle4 <- lmer(subtle ~ pubyear + (1|studyid), data=meta_nosr, REML=FALSE)
summary(subtle4)$coef
anova(subtle3, subtle4)
subtle5 <- lmer(subtle ~ earlyyear + (1|studyid), data=meta_nosr, REML=FALSE)
summary(subtle5)$coef
anova(subtle3, subtle5)

# how many were NOT self-reported behaviors?
nrow(meta_nosr)
# how many WERE self-reported?
nrow(meta) - nrow(meta_nosr)
# percentage self-reported?
round(nrow(meta_nosr) / nrow(meta)*100, 2)

# attitude
meta_noatt3 <- meta_noatt3 %>% 
  mutate(aff=factor(ifelse(attitude=="1", 1, 0)),
         cog=factor(ifelse(attitude=="0", 1, 0)),
         both=factor(ifelse(attitude=="2", 1, 0)))

# aff
att0.aff <- glmer(aff ~ (1|studyid), family=binomial, data=meta_noatt3)
att1.aff <- glmer(aff ~ earlyyear + (1|studyid), family=binomial, data=meta_noatt3)
att2.aff <- glmer(aff ~ pubyear + (1|studyid), family=binomial, data=meta_noatt3)
anova(att0.aff, att1.aff)
anova(att0.aff, att2.aff)
summary(att1.aff)
summary(att2.aff)

# cog
att0.cog <- glmer(cog ~ (1|studyid), family=binomial, data=meta_noatt3)
att1.cog <- glmer(cog ~ earlyyear + (1|studyid), family=binomial, data=meta_noatt3)
att2.cog <- glmer(cog ~ pubyear + (1|studyid), family=binomial, data=meta_noatt3)
anova(att0.cog, att1.cog)
anova(att0.cog, att2.cog)
summary(att1.cog)
summary(att2.cog)

# both
att0.both <- glmer(both ~ (1|studyid), family=binomial, data=meta_noatt3)
att1.both <- glmer(both ~ earlyyear + (1|studyid), family=binomial, data=meta_noatt3)
att2.both <- glmer(both ~ pubyear + (1|studyid), family=binomial, data=meta_noatt3)
anova(att0.both, att1.both)
anova(att0.both, att2.both)
summary(att1.both)
summary(att2.both)

# funnel plot, grouping by study, as did Uttal et al. (2013)
funneldata <- meta %>%
  group_by(studyid) %>% 
  summarise(m_es=mean(es), m_var=mean(var))

funnel(rma(m_es, m_var, data=funneldata))

# fail safe N
# converting r to d from Borenstein et al. 2009, Chapter 7
rtod <- function(r){
  (2*r)/sqrt(1-r^2)
}

failSafeN <- function(k, observed, trivial){
  round(k*((observed-trivial)/trivial),0)
}

failSafeN(90, rtod(transf.ztor(mod0.rve$reg_table[1,2])), rtod(.10)) # fail safe N
