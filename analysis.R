load('C:/Users/Zen/Desktop/Consulting/Moises/dat.Rdata')

dat = database.tb

names(dat)

####Testing

#summary stuff
# summary(dat$sex)
# summary(dat$age)
# summary(as.factor(dat$nationality))
# summary(dat$education)
# summary(as.factor(dat$first_language))
# summary(dat$re_imprisonment)
# summary(dat$hta)


# summary(dat$dm)
# summary(dat$std)
# summary(dat$hiv_aids)


# summary by group
# tb.yes = dat[dat$tb=='Yes',]
# tb.no = dat[dat$tb=='No',]
# 
# tapply(dat$sex, dat$tb, summary)
# prop.table(table(tb.yes$sex))
# prop.table(table(tb.no$sex))
# 
# tapply(dat$age, dat$tb, summary)
# sd(tb.yes$age)
# sd(tb.no$age,na.rm=TRUE)
# 
# tapply(as.factor(dat$nationality), dat$tb, summary)
# prop.table(table(tb.yes$nationality))
# prop.table(table(tb.no$nationality))
# 
# tapply(dat$education, dat$tb, summary)
# prop.table(table(tb.yes$education))
# prop.table(table(tb.no$education))
# 
# tapply(as.factor(dat$first_language), dat$tb, summary)
# tapply(dat$re_imprisonment, dat$tb, summary)
# tapply(dat$hta, dat$tb, summary)
# tapply(dat$dm, dat$tb, summary)
# tapply(dat$std, dat$tb, summary)
# tapply(dat$hiv_aids, dat$tb, summary)


# response
yes = dat$tb=='Yes'
no  = dat$tb=='No'

sex = dat$sex
tapply(sex, dat$tb,summary)
prop.table(table(sex[yes]))
prop.table(table(sex[no]))
prop.table(table(sex))

age = dat$age
tapply(age, dat$tb, summary)
sd(age[yes])
sd(age[no],na.rm=TRUE)

nationality.tmp1 = dat$nationality
nationality = as.factor(ifelse(nationality.tmp1=='Missing',NA,nationality.tmp1))
tapply(nationality, dat$tb, summary)
prop.table(table(nationality[yes]))
prop.table(table(nationality[no]))

# redefine education basic as no or elementary; medium as high school; high as higher
# edu.tmp1 = gsub('No education|Elementary school','basic',dat$education)
# edu.tmp2 = gsub('High school','medium',edu.tmp1)
# edu = as.factor(gsub('Higher education','high',edu.tmp2))

edu.tmp1 = gsub('No education|Elementary school|High school','secondary',dat$education)
edu = as.factor(gsub('Higher education','post-secondary',edu.tmp1))


tapply(edu, dat$tb, summary)
prop.table(table(edu[yes]))
prop.table(table(edu[no]))

table(edu)
prop.table(table(edu))


language.tmp1 = dat$first_language
language = as.factor(ifelse(language.tmp1=='Missing',NA,language.tmp1))
tapply(language, dat$tb, summary)
prop.table(table(language[yes]))
prop.table(table(language[no]))

table(language)
prop.table(table(language))



reimprison = as.factor(gsub('I don\'t', NA, dat$re_imprisonment))
tapply(reimprison, dat$tb, summary)
prop.table(table(reimprison[yes]))
prop.table(table(reimprison[no]))

table(reimprison)
prop.table(table(reimprison))


hypertension = dat$hta
tapply(hypertension, dat$tb, summary)
prop.table(table(hypertension[yes]))
prop.table(table(hypertension[no]))

table(hypertension)
prop.table(table(hypertension))

dm = dat$dm
tapply(dm, dat$tb, summary)
prop.table(table(dm[yes]))
prop.table(table(dm[no]))

table(dm)
prop.table(table(dm))

std = dat$std
tapply(std, dat$tb, summary)
prop.table(table(std[yes]))
prop.table(table(std[no]))

table(std)
prop.table(table(std))

hiv = dat$hiv_aids
tapply(hiv, dat$tb, summary)
prop.table(table(hiv[yes]))
prop.table(table(hiv[no]))

table(hiv)
prop.table(table(hiv))

tb = dat$tb

m.dat = data.frame(tb,sex,age,language,edu,reimprison,hypertension,dm,hiv)

# fit = glm(tb ~ sex, data=dat, family='binomial')
# summary(fit)



my.result=list()
my.result=c()


for (i in 2:dim(m.dat)[2]){
  print(i)
  # predictors.var = predictors[,i]
  # dat = na.omit(data.frame(tb, predictors.var))
  # print(paste('there are',dim(dat)[1],'complete data'))
  my.formula = as.formula(paste("tb~", paste(names(m.dat)[i], collapse="+")))
  print(my.formula)
  print(names(m.dat)[i])
  f1 <- try(glm(my.formula, data=m.dat, family='binomial'))
  if ('try-error' %in% class(f1)) next
  print(summary(f1)$coefficients)
  coef = cbind(names(m.dat)[i],summary(f1)$coefficients,exp(confint(f1)))
  my.result = rbind(my.result,coef)
}







dim(my.result)
rownames(my.result)
intercept.index = grepl('Intercept',rownames(my.result))

result = my.result[!intercept.index,-c(3,4)]

or = exp(as.numeric(result[,colnames(result)=='Estimate']))

final = cbind(result,or)

# Adjusted
my.formula = as.formula(paste("tb~", paste(names(m.dat)[-1], collapse="+")))


f1 = glm(tb ~ sex + age + language + edu + reimprison + hypertension + 
           dm + hiv + hypertension*dm , data = m.dat, family='binomial')
coef = cbind(summary(f1)$coefficients,exp(confint(f1)))
intercept.index = grepl('Intercept',rownames(coef))

adj.result = coef[!intercept.index,-c(2,3)]

OR = exp(as.numeric(adj.result[,colnames(adj.result)=='Estimate']))

adj.final = cbind(adj.result,OR)


# interaction not significant so look at main effect model
my.formula = as.formula(paste("tb~", paste(names(m.dat)[-1], collapse="+")))


f1 = glm(my.formula, data = m.dat, family='binomial')
coef = cbind(summary(f1)$coefficients,exp(confint(f1)))
intercept.index = grepl('Intercept',rownames(coef))

adj.result = coef[!intercept.index,-c(2,3)]

OR = exp(as.numeric(adj.result[,colnames(adj.result)=='Estimate']))

adj.final = cbind(adj.result,OR)

##############  Goodness of fit
library(BaylorEdPsych)
PseudoR2(f1)


###############################
dim(m.dat)[1]
sum(complete.cases(m.dat))
gof.dat=m.dat[complete.cases(m.dat),]

library(MKmisc)
HLgof.test(fit = fitted(f1), obs = gof.dat$tb)
# small values indicates a good fit
# source https://www.r-bloggers.com/evaluating-logistic-regression-models/


################## sub group analysis on HIV pop
hiv.yes = m.dat[m.dat$hiv=='Yes',-10]
hiv.no = m.dat[m.dat$hiv=='No',-10]


my.formula = as.formula(paste("tb~", paste(names(hiv.yes)[-c(1,9)], collapse="+")))


f1 = glm(my.formula, data = hiv.yes, family='binomial')
coef = cbind(summary(f1)$coefficients,exp(confint(f1)))
intercept.index = grepl('Intercept',rownames(coef))

adj.result = coef[!intercept.index,-c(2,3)]

OR = exp(as.numeric(adj.result[,colnames(adj.result)=='Estimate']))

hiv.yes.adj.final = cbind(adj.result,OR)

#######################
my.formula = as.formula(paste("tb~", paste(names(hiv.no)[-c(1,9)], collapse="+")))


f1 = glm(my.formula, data = hiv.no, family='binomial')
coef = cbind(summary(f1)$coefficients,exp(confint(f1)))
intercept.index = grepl('Intercept',rownames(coef))

adj.result = coef[!intercept.index,-c(2,3)]

OR = exp(as.numeric(adj.result[,colnames(adj.result)=='Estimate']))

hiv.no.adj.final = cbind(adj.result,OR)










