########################################################################################
## spript: Germany code
## by: Olga Churkina (contact e-mail: ochurkina@gatech.edu)
## date: September 17th, 2021
## article: A quasi-experiment in international student mobility: Germany’s fee re-introductions
## authors: Matteo Zullo & Olga Churkina


if (!require(c("readxl","plm","lmtest","dplyr","ggplot2","Synth","SCtools","stringr",
               "stats","cvTools","stargazer","furrr","purrr","magrittr"))) 
  install.packages(c("readxl","plm","lmtest","dplyr","ggplot2","Synth","SCtools","stringr",
                     "stats","cvTools","stargazer","furrr","purrr","magrittr")) 

library(readxl) # Excel files
library(plm) # panel data
library(lmtest) # Breusch-Pagan and Breusch-Godfrey tests
library(dplyr) # data manipulations
library(ggplot2) # visualizations
library(Synth) # synthetic control analysis
library(SCtools) # extensions for synthetic controls analysis
library(stringr) # string manipulations
library(stats) # statistical functions
library(cvTools) # cross-validation tools for regression models
library(stargazer) # exporting to LaTeX
library(furrr) # speed the calculations
library(purrr) # speed the calculations
library(magrittr) # speed the calculations

############################################ extended difference-in-differences ############################################

## full data

data <- read_excel("Data submission.xls") # accessing full data

data <- pdata.frame(data, index=c("land","Y")) # creating panel data

## two-way fixed effects DD estimator (Goodman-Bacon, 2018)
uniform <- as.formula("log(foreign_1) ~ log(GDP_2017_CPI)+log(pops)+log(unemp)+log(foreign)
                      +log(export_2017_CPI)+log(import_2017_CPI)+log(HE_exp_2017_CPI)+log(German_1)+fees")

## uniform fixed-effects model for full data
uniform.fe <- plm(uniform, data=data, model = "within", effect = "twoways")
summary(uniform.fe)

## random effects
uniform.re <- plm(uniform, data=data, model = "random", effect = "twoways")
summary(uniform.re)

## Hausman test
phtest(uniform.fe,uniform.re)

## Breusch-Godfrey Test for serial correlation - reject Ho
pbgtest(uniform.fe)

## Breusch-Pagan Test for heteroskedasticity - reject Ho
bptest(uniform.fe, studentize = F)

## Cluster-Robust Standard Errors - Arellano (1987)
coeftest(uniform.fe, vcov=vcovHC(uniform.fe, method = "arellano", type = "HC1", cluster = c("group", "time")))
#coeftest(uniform.fe, vcov=vcovHC) #White (1980)

## Robustness check - no diff
uniform.rc <- as.formula("log(foreign_1) ~ log(GDP_2017_CPI)+log(pops)+log(unemp)+log(foreign)
                         +log(export_2017_CPI)+log(import_2017_CPI)+log(HE_exp_2017_CPI)+log(German_1)+fees+size+university_rank")
uniform.re.rc <- plm(uniform.rc, data=data, model = "random", effect = "twoways")
summary(uniform.re.rc)

## state-specific fixed-effects model for full data
state <- as.formula("log(foreign_1) ~ log(GDP_2017_CPI)+log(pops)+log(unemp)+log(foreign)
                    +log(export_2017_CPI)+log(import_2017_CPI)+log(HE_exp_2017_CPI)+log(German_1)
                    +fees_Baden+fees_Bavaria+fees_Hamburg+fees_Hesse+fees_LS+fees_West+fees_Saarland")
state.fe <- plm(state, data=data, model = "within", effect = "twoways")
summary(state.fe)

## Breusch-Godfrey Test for serial correlation - reject Ho
pbgtest(state.fe)

## Breusch-Pagan Test for heteroskedasticity - reject Ho
bptest(state.fe, studentize = F)

## Cluster-Robust Standard Errors - Arellano (1987)
coeftest(state.fe, vcov=vcovHC(state.fe, method = "arellano", type = "HC1", cluster = c("group", "time")))

#Correlations <- read_excel("Data.xls", sheet = "Correlations")
#cor(Correlations) #all positive

## Hamburg-Saarland

HS <- read_excel("Data submission.xls", 
                 sheet = "Hamburg Saarland") # accessing Hamburg-Saarland data

HS <- pdata.frame(HS, index=c("land","Y")) # creating panel data

## uniform model for Hamburg-Saarland data
uniform_HS.fe <- plm(uniform, data=HS, model = "within", effect = "twoways")
summary(uniform_HS.fe)

## random effect
uniform_HS.re <- plm(uniform, data=HS, model = "random", effect = "twoways")
summary(uniform_HS.re)

## Hausman test
phtest(uniform_HS.fe,uniform_HS.re)

## Breusch-Godfrey Test for serial correlation - reject Ho
pbgtest(uniform_HS.fe)

## Breusch-Pagan Test for heteroskedasticity - reject Ho
bptest(uniform_HS.fe, studentize = F)

## Cluster-Robust Standard Errors - Arellano (1987)
coeftest(uniform_HS.fe, vcov=vcovHC(uniform_HS.fe, method = "arellano", type = "HC1", cluster = c("group", "time")))

## state-specific model for Hamburg-Saarland data
state_HS <- as.formula("log(foreign_1) ~ log(GDP_2017_CPI)+log(pops)+log(unemp)+log(foreign)
                    +log(export_2017_CPI)+log(import_2017_CPI)+log(HE_exp_2017_CPI)+log(German_1)
                    +fees_Hamburg+fees_Saarland")
state_HS.fe <- plm(state_HS, data=HS, model = "within", effect = "twoways")
summary(state_HS.fe)

## Breusch-Godfrey Test for serial correlation - reject Ho
pbgtest(state_HS.fe)

## Breusch-Pagan Test for heteroskedasticity - reject Ho
bptest(state_HS.fe, studentize = F)

## Cluster-Robust Standard Errors - Arellano (1987)
coeftest(state_HS.fe, vcov=vcovHC(state_HS.fe, method = "arellano", type = "HC1", cluster = c("group", "time")))

## Baden-Bavaria-Lower Saxony

BBLS <- read_excel("Data submission.xls", 
                   sheet = "Bavaria Baden Lower Saxony") # accessing Baden-Bavaria-Lower Saxony data

BBLS <- pdata.frame(BBLS, index=c("land","Y")) # creating panel data

## uniform model for Baden-Bavaria-Lower Saxony data
uniform_BBLS.fe <- plm(uniform, data=BBLS, model = "within", effect = "twoways")
summary(uniform_BBLS.fe)

## random effects
uniform_BBLS.re <- plm(uniform, data=BBLS, model = "random", effect = "twoways")
summary(uniform_BBLS.re)

## Hausman test
phtest(uniform_BBLS.fe,uniform_BBLS.re)

## Breusch-Godfrey Test for serial correlation - reject Ho
pbgtest(uniform_BBLS.fe)

## Breusch-Pagan Test for heteroskedasticity - reject Ho
bptest(uniform_BBLS.fe, studentize = F)

## Cluster-Robust Standard Errors - Arellano (1987)
coeftest(uniform_BBLS.fe, vcov=vcovHC(uniform_BBLS.fe, method = "arellano", type = "HC1", cluster = c("group", "time")))

## state-specific model for Baden-Bavaria-Lower Saxony data
state_BBLS <- as.formula("log(foreign_1) ~ log(GDP_2017_CPI)+log(pops)+log(unemp)+log(foreign)
                         +log(export_2017_CPI)+log(import_2017_CPI)+log(HE_exp_2017_CPI)+log(German_1)
                         +fees_Baden+fees_Bavaria+fees_LS")
state_BBLS.fe <- plm(state_BBLS, data=BBLS, model = "within", effect = "twoways")
summary(state_BBLS.fe)

## Breusch-Godfrey Test for serial correlation - reject Ho
pbgtest(state_BBLS.fe)

## Breusch-Pagan Test for heteroskedasticity - reject Ho
bptest(state_BBLS.fe, studentize = F)

## Cluster-Robust Standard Errors - Arellano (1987)
coeftest(state_BBLS.fe, vcov=vcovHC(state_BBLS.fe, method = "arellano", type = "HC1", cluster = c("group", "time")))

## LATEX export - uniform model 
stargazer(uniform.fe, uniform_HS.fe, uniform_BBLS.fe, title="Results", align=TRUE,
          dep.var.labels="First-year foreign students", column.labels=c("data","HS","BBLS"),
          covariate.labels=c("GDP","Population", "Unemployment","Foreign population","Export","Import",
                             "HE expenditures", "First-year German students", "Fees in place", no.space=TRUE))

## LATEX export - state-specific model
stargazer(state.fe, state_HS.fe, state_BBLS.fe, title="Results", align=TRUE,
          dep.var.labels="First-year foreign students", column.labels=c("data","HS","BBLS"),
          covariate.labels=c("GDP","Population", "Unemployment","Foreign population","Export","Import",
                             "HE expenditures", "First-year German students", "Fees in place (Baden)",
                             "Fees in place (Bavaria)", "Fees in place (Hamburg)", "Fees in place (Hesse)", "Fees in place (Lower Saxony)",
                             "Fees in place (Westphalia)", "Fees in place (Saarland)", no.space=TRUE))

############################################ synthetic coltrol ############################################

## synthetic states
Baden <- read_excel("Data submission.xls", 
                    sheet = "Baden SC") # accessing Baden data
Bavaria <- read_excel("Data submission.xls", 
                      sheet = "Bavaria SC") # accessing Bavaria data
Hamburg <- read_excel("Data submission.xls", 
                      sheet = "Hamburg SC") # accessing Hamburg data
Hesse <- read_excel("Data submission.xls", 
                    sheet = "Hesse SC") # accessing Hesse data
Lower_Saxony <- read_excel("Data submission.xls", 
                           sheet = "LS SC") # accessing Lower Saxony data
Westphalia <- read_excel("Data submission.xls", 
                         sheet = "West SC") # accessing Westphalia data
Saarland <- read_excel("Data submission.xls", 
                       sheet = "Saarland SC") # accessing Saarland data

## placebo states

Berlin <- read_excel("Data submission.xls", 
                     sheet = "Berlin placebo") # accessing Berlin data
Brandenburg <- read_excel("Data submission.xls", 
                          sheet = "Brandenburg placebo") # accessing Brandenburg data
Bremen <- read_excel("Data submission.xls", 
                     sheet = "Bremen placebo") # accessing Bremen data
Pomerania <- read_excel("Data submission.xls", 
                        sheet = "Pomerania placebo") # accessing Pomerania data
Rhineland <- read_excel("Data submission.xls", 
                        sheet = "Rhineland placebo") # accessing Rhineland data
Saxony <- read_excel("Data submission.xls", 
                     sheet = "Saxony placebo") # accessing Saxony data
Anhalt <- read_excel("Data submission.xls", 
                     sheet = "Anhalt placebo") # accessing Saxony-Anhalt data
Holstein <- read_excel("Data submission.xls", 
                       sheet = "Holstein placebo") # accessing Holstein data
Thuringia <- read_excel("Data submission.xls", 
                        sheet = "Thuringia placebo") # accessing Thuringia data


## synthetic control output
synth_land <- function(land, end){
  land <- as.data.frame(land)
  
  dataprep.out.land<-
    dataprep(
      foo = land,
      predictors = c("size", "pops", "unemp", "GDP_2017_CPI", "foreign", "export_2017_CPI",
                     "import_2017_CPI", "HE_exp_2017_CPI", "German_1", "foreign_1"),
      predictors.op = "mean",
      dependent = "foreign_1",
      unit.variable = "unit",
      time.variable = "year",
      treatment.identifier = 1,
      controls.identifier = c(2:10),
      time.predictors.prior = c(1997:end),
      time.optimize.ssr = c(1997:end),
      unit.names.variable = "land",
      time.plot = 1997:2017
    )
  
  synth.out.land <- synth(dataprep.out.land)
  
  round(synth.out.land$solution.w,2)
  synth.out.land$solution.v # factor weights
  
  synth.tables.land <- synth.tab(
    dataprep.res = dataprep.out.land,
    synth.res = synth.out.land)
  print(synth.tables.land) # state weights
  
  ## modifications to Hainmueller's code
  path.plot <- function(
    synth.res = NA,
    dataprep.res = NA,
    tr.intake = NA,
    Ylab = c("Y Axis"),
    Xlab = c("Time"),
    Ylim = NA,
    Legend=c("Actual","Synthetic"),
    Legend.position=c("bottom"),
    Main = NA,
    Z.plot = FALSE
  )
  { 
    if(Z.plot==FALSE)
    { 
      if(sum(is.na(dataprep.res$Y1plot)) > 0)
      {
        stop(
          "\n\n#####################################################",
          "\nYou have missing Y data for the treated!\n\n"
        )  
      }
      if(sum(is.na(dataprep.res$Y0plot)) > 0)
      {
        stop(
          "\n\n#####################################################",
          "\nYou have missing Y data for the controls!\n\n"
        )  
      } 
      y0plot1 <- dataprep.res$Y0plot %*% synth.res$solution.w                
      # Get Ylim right 
      if(sum(is.na(Ylim))>0)
      {
        Y.max <-  max(c(y0plot1,dataprep.res$Y1plot))
        Y.min <-  min(c(y0plot1,dataprep.res$Y1plot))
        Ylim <- c(
          (Y.min - .3*Y.min ),
          (.3*Y.max + Y.max)
        )
      }
      plot(
        dataprep.res$tag$time.plot,dataprep.res$Y1plot,
        t="l",col="black",tck=-.025,
        lwd=2,cex.main=1,cex.lab=0.8,cex.axis=0.7,
        main=Main,bty="L",
        ylab=Ylab,las=1,
        xlab=Xlab,xaxs="i",yaxs="i",ylim=Ylim)
      lines(
        dataprep.res$tag$time.plot,
        y0plot1,
        col="gray80",
        lty=1,lwd=2,
        cex=4/5
      )
    } else {
      z0plot <- dataprep.res$Z0 %*% synth.res$solution.w                
      # Get Ylim right 
      if(sum(is.na(Ylim))>0)
      {
        Y.max <-  max(c(z0plot,dataprep.res$Z1))
        Y.min <-  min(c(z0plot,dataprep.res$Z1))
        Ylim <- c(
          (Y.min - .3*Y.min),
          (.3*Y.max + Y.max)
        )
      }
      plot(
        dataprep.res$tag$time.optimize.ssr,z0plot,
        t="l",col="black",tck=-.025,
        lwd=2,cex.main=1,cex.lab=0.8,cex.axis=0.7,
        main=Main,bty="L",
        ylab=Ylab,las=1,
        xlab=Xlab,xaxs="i",yaxs="i",ylim=Ylim)
      lines(
        dataprep.res$tag$time.optimize.ssr,
        dataprep.res$Z1,
        col="gray80",
        lty=1,lwd=2,
        cex=4/5
      ) 
    }
    abline(v=end,lty='dotted')
    if(sum(is.na(Legend))==0)
    {
      legend(Legend.position,legend=Legend,inset=c(0,-0.5),
             xpd=TRUE,horiz=TRUE, bty="n",
             col=c("black","gray80"),lwd=c(2,2),cex=5/7)
    }
  }
  
  ## for graph divergence 
  path.plot(dataprep.res = dataprep.out.land, synth.res = synth.out.land,
            Xlab = c(" "), Ylab = c("First-year Foreign Students"))
  
  gaps<-dataprep.out.land$Y1plot-(
    dataprep.out.land$Y0plot%*%synth.out.land$solution.w
  ) # calculating differences between treated and synthetic values for every year
  
  b <- as.integer(end)-1997+1  # bound of first loop
  d <- as.integer(end)-1997+2  # bound of second loop
  sq.gaps.pre <- vector("numeric")  # result vector - pre
  sq.gaps.post <- vector("numeric")  # result vector - post
  
  for (i in 1:b) {  # calculating RMSPE pre-treatment
    sq.gaps.pre[i] <- gaps[i,1]^2  # storing sum of squares gap
  }
  RMSPE.pre <- sqrt(sum(sq.gaps.pre)/length(sq.gaps.pre))  # RMSPE pre
  
  for (i in d:21) {  # calculating RMSPE post-treatment
    sq.gaps.post[i-b] <- gaps[i,1]^2  # storing sum of squares gap
  }
  RMSPE.post <- sqrt(sum(sq.gaps.post)/length(sq.gaps.post))  # RMSPE post
  
  RMSPE.ratio <- RMSPE.post/RMSPE.pre
  print(RMSPE.ratio)
  
}

## synthetic control - spaghetti graph
spaghetti_land <- function(land, end){
  land <- as.data.frame(land)
  
  dataprep.out.land<-
    dataprep(
      foo = land,
      predictors = c("size", "pops", "unemp", "GDP_2017_CPI", "foreign", "export_2017_CPI",
                     "import_2017_CPI", "HE_exp_2017_CPI", "German_1", "foreign_1"),
      predictors.op = "mean",
      dependent = "foreign_1",
      unit.variable = "unit",
      time.variable = "year",
      treatment.identifier = 1,
      controls.identifier = c(2:10),
      time.predictors.prior = c(1997:end),
      time.optimize.ssr = c(1997:end),
      unit.names.variable = "land",
      time.plot = 1997:2017
    )
  
  synth.out.land <- synth(dataprep.out.land)
  
  ## proportion of units (placebos  and treated) that have a RMSPE ratio equal or higher that of the treated unit
  tdf.land <- generate.placebos(dataprep.out.land,synth.out.land)
  ratio <- mspe.test(tdf.land)
  print(ratio$p.val)
  
  plot_placebos(tdf.land, discard.extreme=TRUE, mspe.limit=10, ylab = "Treated-Synthetic Gap")
  
}

## main analysis visualization
synth_land(Baden, 2006)
spaghetti_land(Baden, 2006)

synth_land(Bavaria, 2006)
spaghetti_land(Bavaria, 2006)

synth_land(Hamburg, 2006)
spaghetti_land(Hamburg, 2006)

synth_land(Hesse, 2006)
spaghetti_land(Hesse, 2006)

synth_land(Lower_Saxony, 2005)
spaghetti_land(Lower_Saxony, 2005)

synth_land(Westphalia, 2005)
spaghetti_land(Westphalia, 2005)

synth_land(Saarland, 2006)
spaghetti_land(Saarland, 2006)

## in-time placebo visualization
synth_land(Baden, 2001)
spaghetti_land(Baden, 2001)

synth_land(Bavaria, 2001)
spaghetti_land(Bavaria, 2001)

synth_land(Hamburg, 2001)
spaghetti_land(Hamburg, 2001)

synth_land(Hesse, 2002)
spaghetti_land(Hesse, 2002)

synth_land(Lower_Saxony, 2001)
spaghetti_land(Lower_Saxony, 2001)

synth_land(Westphalia, 2001)
spaghetti_land(Westphalia, 2001)

synth_land(Saarland, 2002)
spaghetti_land(Saarland, 2002)

## generating placebo states
synth_land_placebo <- function(land, end){
  land <- as.data.frame(land)
  
  dataprep.out.land<-
    dataprep(
      foo = land,
      predictors = c("size", "pops", "unemp", "GDP_2017", "foreign", "export_2017",
                     "import_2017", "HE_exp_2017", "German_1", "foreign_1"),
      predictors.op = "mean",
      dependent = "foreign_1",
      unit.variable = "unit",
      time.variable = "year",
      treatment.identifier = 1,
      controls.identifier = c(2:9),
      time.predictors.prior = c(1997:end),
      time.optimize.ssr = c(1997:end),
      unit.names.variable = "land",
      time.plot = 1997:2017
    )
  
  synth.out.land <- synth(dataprep.out.land)
  
  round(synth.out.land$solution.w,2)
  synth.out.land$solution.v # factor weights
  
  synth.tables.land <- synth.tab(
    dataprep.res = dataprep.out.land,
    synth.res = synth.out.land)
  print(synth.tables.land) # state weights
  
  ## modifications to Hainmueller's code
  path.plot <- function(
    synth.res = NA,
    dataprep.res = NA,
    tr.intake = NA,
    Ylab = c("Y Axis"),
    Xlab = c("Time"),
    Ylim = NA,
    Legend=c("Actual","Synthetic"),
    Legend.position=c("bottom"),
    Main = NA,
    Z.plot = FALSE
  )
  { 
    if(Z.plot==FALSE)
    { 
      if(sum(is.na(dataprep.res$Y1plot)) > 0)
      {
        stop(
          "\n\n#####################################################",
          "\nYou have missing Y data for the treated!\n\n"
        )  
      }
      if(sum(is.na(dataprep.res$Y0plot)) > 0)
      {
        stop(
          "\n\n#####################################################",
          "\nYou have missing Y data for the controls!\n\n"
        )  
      } 
      y0plot1 <- dataprep.res$Y0plot %*% synth.res$solution.w                
      # Get Ylim right 
      if(sum(is.na(Ylim))>0)
      {
        Y.max <-  max(c(y0plot1,dataprep.res$Y1plot))
        Y.min <-  min(c(y0plot1,dataprep.res$Y1plot))
        Ylim <- c(
          (Y.min - .3*Y.min ),
          (.3*Y.max + Y.max)
        )
      }
      plot(
        dataprep.res$tag$time.plot,dataprep.res$Y1plot,
        t="l",col="black",tck=-.025,
        lwd=2,cex.main=1,cex.lab=0.8,cex.axis=0.7,
        main=Main,bty="L",
        ylab=Ylab,las=1,
        xlab=Xlab,xaxs="i",yaxs="i",ylim=Ylim)
      lines(
        dataprep.res$tag$time.plot,
        y0plot1,
        col="gray80",
        lty=1,lwd=2,
        cex=4/5
      )
    } else {
      z0plot <- dataprep.res$Z0 %*% synth.res$solution.w                
      # Get Ylim right 
      if(sum(is.na(Ylim))>0)
      {
        Y.max <-  max(c(z0plot,dataprep.res$Z1))
        Y.min <-  min(c(z0plot,dataprep.res$Z1))
        Ylim <- c(
          (Y.min - .3*Y.min),
          (.3*Y.max + Y.max)
        )
      }
      plot(
        dataprep.res$tag$time.optimize.ssr,z0plot,
        t="l",col="black",tck=-.025,
        lwd=2,cex.main=1,cex.lab=0.8,cex.axis=0.7,
        main=Main,bty="L",
        ylab=Ylab,las=1,
        xlab=Xlab,xaxs="i",yaxs="i",ylim=Ylim)
      lines(
        dataprep.res$tag$time.optimize.ssr,
        dataprep.res$Z1,
        col="gray80",
        lty=1,lwd=2,
        cex=4/5
      ) 
    }
    abline(v=end,lty='dotted')
    if(sum(is.na(Legend))==0)
    {
      legend(Legend.position,legend=Legend,inset=c(0,-0.5),
             xpd=TRUE,horiz=TRUE, bty="n",
             col=c("black","gray80"),lwd=c(2,2),cex=5/7)
    }
  }
  
  ## for graph divergence 
  path.plot(dataprep.res = dataprep.out.land, synth.res = synth.out.land,
            Xlab = c(" "), Ylab = c("First-year Foreign Students"))
  
  gaps<-dataprep.out.land$Y1plot-(
    dataprep.out.land$Y0plot%*%synth.out.land$solution.w
  ) # calculating differences between treated and synthetic values for every year
  
  b <- as.integer(end)-1997+1  # bound of first loop
  d <- as.integer(end)-1997+2  # bound of second loop
  sq.gaps.pre <- vector("numeric")  # result vector - pre
  sq.gaps.post <- vector("numeric")  # result vector - post
  
  for (i in 1:b) {  # calculating RMSPE pre-treatment
    sq.gaps.pre[i] <- gaps[i,1]^2  # storing sum of squares gap
  }
  RMSPE.pre <- sqrt(sum(sq.gaps.pre)/length(sq.gaps.pre))  # RMSPE pre
  
  for (i in d:21) {  # calculating RMSPE post-treatment
    sq.gaps.post[i-b] <- gaps[i,1]^2  # storing sum of squares gap
  }
  RMSPE.post <- sqrt(sum(sq.gaps.post)/length(sq.gaps.post))  # RMSPE post
  
  RMSPE.ratio <- RMSPE.post/RMSPE.pre
  print(RMSPE.ratio)
  
}

## placebo spaghetti graph
spaghetti_land_placebo <- function(land, end){
  land <- as.data.frame(land)
  
  dataprep.out.land<-
    dataprep(
      foo = land,
      predictors = c("size", "pops", "unemp", "GDP_2017", "foreign", "export_2017",
                     "import_2017", "HE_exp_2017", "German_1", "foreign_1"),
      predictors.op = "mean",
      dependent = "foreign_1",
      unit.variable = "unit",
      time.variable = "year",
      treatment.identifier = 1,
      controls.identifier = c(2:9),
      time.predictors.prior = c(1997:end),
      time.optimize.ssr = c(1997:end),
      unit.names.variable = "land",
      time.plot = 1997:2017
    )
  
  synth.out.land <- synth(dataprep.out.land)
  
  ## proportion of units (placebos  and treated) that have a RMSPE ratio equal or higher that of the treated unit
  tdf.land <- generate.placebos(dataprep.out.land,synth.out.land)
  ratio <- mspe.test(tdf.land)
  print(ratio$p.val)
  
  plot_placebos(tdf.land, discard.extreme=TRUE, mspe.limit=10, ylab = "Treated-Synthetic Gap")
  
}

## synthetic states visualization
synth_land_placebo(Berlin, 2006)
spaghetti_land_placebo(Berlin, 2006)

synth_land_placebo(Brandenburg, 2006)
spaghetti_land_placebo(Brandenburg, 2006)

synth_land_placebo(Bremen, 2006)
spaghetti_land_placebo(Bremen, 2006)

synth_land_placebo(Pomerania, 2006)
spaghetti_land_placebo(Pomerania, 2006)

synth_land_placebo(Rhineland, 2006)
spaghetti_land_placebo(Rhineland, 2006)

synth_land_placebo(Saxony, 2006)
spaghetti_land_placebo(Saxony, 2006)

synth_land_placebo(Anhalt, 2006)
spaghetti_land_placebo(Anhalt, 2006)

synth_land_placebo(Holstein, 2006)
spaghetti_land_placebo(Holstein, 2006)

synth_land_placebo(Thuringia, 2006)
spaghetti_land_placebo(Thuringia, 2006)
