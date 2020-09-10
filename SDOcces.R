library(sjPlot)
library(car)
library(psy)
library(psych)
library(ggplot2)

#SDO
CCES14$UMS318<-recode(CCES14$UMS318, "9=NA")
CCES14$UMS318<-recode(CCES14$UMS318, "8=NA")
CCES14$UMS318<-6-CCES14$UMS318
CCES14$UMS319<-recode(CCES14$UMS319, "9=NA")
CCES14$UMS319<-recode(CCES14$UMS319, "8=NA")
CCES14$UMS319<-6-CCES14$UMS319
CCES14$UMS320<-recode(CCES14$UMS320, "9=NA")
CCES14$UMS320<-recode(CCES14$UMS320, "8=NA")
CCES14$UMS320<-6-CCES14$UMS320
CCES14$UMS321<-recode(CCES14$UMS321, "9=NA")
CCES14$UMS321<-recode(CCES14$UMS321, "8=NA")
CCES14$UMS322<-recode(CCES14$UMS322, "9=NA")
CCES14$UMS322<-recode(CCES14$UMS322, "8=NA")
CCES14$UMS323<-recode(CCES14$UMS323, "9=NA")
CCES14$UMS323<-recode(CCES14$UMS323, "8=NA")

#immigrant resentment
CCES14$UMS324<-recode(CCES14$UMS324, "9=NA")
CCES14$UMS324<-recode(CCES14$UMS324, "8=NA")
CCES14$UMS325<-recode(CCES14$UMS325, "9=NA")
CCES14$UMS325<-recode(CCES14$UMS325, "8=NA")
CCES14$UMS325<-6-CCES14$UMS325
CCES14$UMS326<-recode(CCES14$UMS326, "9=NA")
CCES14$UMS326<-recode(CCES14$UMS326, "8=NA")
CCES14$UMS327<-recode(CCES14$UMS327, "9=NA")
CCES14$UMS327<-recode(CCES14$UMS327, "8=NA")
CCES14$UMS328<-recode(CCES14$UMS328, "9=NA")
CCES14$UMS328<-recode(CCES14$UMS328, "8=NA")
CCES14$UMS329<-recode(CCES14$UMS329, "9=NA")
CCES14$UMS329<-recode(CCES14$UMS329, "8=NA")

#RWA
CCES14$UMS330<-recode(CCES14$UMS330, "9=NA")
CCES14$UMS330<-recode(CCES14$UMS330, "8=NA")
CCES14$UMS331<-recode(CCES14$UMS331, "9=NA")
CCES14$UMS331<-recode(CCES14$UMS331, "8=NA")
CCES14$UMS332<-recode(CCES14$UMS332, "9=NA")
CCES14$UMS332<-recode(CCES14$UMS332, "8=NA")
CCES14$UMS332<-3-CCES14$UMS332
CCES14$UMS333<-recode(CCES14$UMS333, "9=NA")
CCES14$UMS333<-recode(CCES14$UMS333, "8=NA")

#gender
CCES14$male<-CCES14$gender
CCES14$male<-recode(CCES14$male, "2=0")
CCES14$male<-recode(CCES14$male, "8=NA")
CCES14$male<-recode(CCES14$male, "9=NA")
table(CCES14$male)

#party id
CCES14$partyid<-CCES14$pid7
CCES14$partyid<-recode(CCES14$partyid, "98=NA")
CCES14$partyid<-recode(CCES14$partyid, "99=NA")

#race
CCES14$white<-CCES14$race
CCES14$white<-recode(CCES14$white, "98=NA")
CCES14$white<-recode(CCES14$white, "99=NA")
CCES14$white<-recode(CCES14$white, "2=0")
CCES14$white<-recode(CCES14$white, "3=0")
CCES14$white<-recode(CCES14$white, "4=0")
CCES14$white<-recode(CCES14$white, "5=0")
CCES14$white<-recode(CCES14$white, "6=0")
CCES14$white<-recode(CCES14$white, "7=0")
CCES14$white<-recode(CCES14$white, "8=0")

CCES14$black<-CCES14$race
CCES14$black<-recode(CCES14$black, "98=NA")
CCES14$black<-recode(CCES14$black, "99=NA")
CCES14$black<-recode(CCES14$black, "1=0")
CCES14$black<-recode(CCES14$black, "2=1")
CCES14$black<-recode(CCES14$black, "3=0")
CCES14$black<-recode(CCES14$black, "4=0")
CCES14$black<-recode(CCES14$black, "5=0")
CCES14$black<-recode(CCES14$black, "6=0")
CCES14$black<-recode(CCES14$black, "7=0")
CCES14$black<-recode(CCES14$black, "8=0")

CCES14$hisp<-CCES14$race
CCES14$hisp<-recode(CCES14$hisp, "98=NA")
CCES14$hisp<-recode(CCES14$hisp, "99=NA")
CCES14$hisp<-recode(CCES14$hisp, "1=0")
CCES14$hisp<-recode(CCES14$hisp, "2=0")
CCES14$hisp<-recode(CCES14$hisp, "3=1")
CCES14$hisp<-recode(CCES14$hisp, "4=0")
CCES14$hisp<-recode(CCES14$hisp, "5=0")
CCES14$hisp<-recode(CCES14$hisp, "6=0")
CCES14$hisp<-recode(CCES14$hisp, "7=0")
CCES14$hisp<-recode(CCES14$hisp, "8=0")

attach(CCES14)
#factor analysis: SDO
Y<-cbind(UMS318,UMS319,UMS320,UMS321,UMS322,UMS323)
SDOfactor<-factanal(na.omit(Y),factors=1,scores="regression")
SDOfactor
cronbach(Y)
fs<-factor.scores(Y,SDOfactor)
fs<-fs$scores
CCES14<-cbind(CCES14,fs)
CCES14$SDO<-CCES14$Factor1
CCES14$Factor1<-NULL


#factor analysis: Immigrant Resentment
Y1<-cbind(UMS324,UMS325,UMS326,UMS327,UMS328,UMS329)
immfactor<-factanal(na.omit(Y1),factors=2,scores="regression")
immfactor
cronbach(Y1)
fs1<-factor.scores(Y1,immfactor)
fs1<-fs1$scores
CCES14<-cbind(CCES14,fs1)
CCES14$immass<-CCES14$Factor1
CCES14$imminc<-CCES14$Factor2
CCES14$Factor1<-NULL
CCES14$Factor2<-NULL

#factor analysis: authoritarianism
Y2<-cbind(UMS330,UMS331,UMS332,UMS333)
authfactor<-factanal(na.omit(Y2),factors=1,scores="regression")
authfactor
cronbach(Y2)
fs2<-factor.scores(Y2,authfactor)
fs2<-fs2$scores
CCES14<-cbind(CCES14,fs2)
CCES14$auth1<-CCES14$Factor1
CCES14$Factor1<-NULL


#models
model<-lm(immass~SDO+auth1+SDO*auth1+male+partyid+black+hisp, data=CCES14)
summary(model)
p1 <- plot_model(model,type="int") +
  ggtitle("Assimilation") +
  scale_y_continuous(limits = c(-2, 2))

model1<-lm(imminc~SDO+auth1+SDO*auth1+male+partyid+black+hisp, data=CCES14)
summary(model1)
p2 <- plot_model(model1,type="int") +
  ggtitle("Incorporation") +
  scale_y_continuous(limits = c(-2, 2))

#alternate DP
CCES14$CC14_322_1<-recode(CCES14$CC14_322_1, "8=NA")
CCES14$CC14_322_1<-recode(CCES14$CC14_322_1, "9=NA")
CCES14$CC14_322_1<-recode(CCES14$CC14_322_1, "2=0")
model1<-glm(CC14_322_1~SDO+RWA+SDO*RWA+male+partyid+white+black+hisp+UMS336+UMS340, data=CCES14, family=binomial)
summary(model1)
plot_model(model1,type="int")

#alternate DP - increase border patrol
CCES14$CC14_322_2<-recode(CCES14$CC14_322_2, "8=NA")
CCES14$CC14_322_2<-recode(CCES14$CC14_322_2, "9=NA")
CCES14$CC14_322_2<-recode(CCES14$CC14_322_2, "2=0")
model2<-glm(CC14_322_2~SDO+RWA+SDO*RWA+male+partyid+white+black+hisp, data=CCES14, family=binomial)
summary(model2)
plot_model(model2,type="int")

#alternate DP - allow police to question anyone they think is here illegally
CCES14$CC14_322_3<-recode(CCES14$CC14_322_3, "8=NA")
CCES14$CC14_322_3<-recode(CCES14$CC14_322_3, "9=NA")
CCES14$CC14_322_3<-recode(CCES14$CC14_322_3, "2=0")
model3<-glm(CC14_322_3~SDO+RWA+SDO*RWA+male+partyid+white+black+hisp, data=CCES14, family=binomial)
summary(model3)
plot_model(model3,type="int")

#alternate DP - fine businesses that hire illegal immigrants
CCES14$CC14_322_4<-recode(CCES14$CC14_322_4, "8=NA")
CCES14$CC14_322_4<-recode(CCES14$CC14_322_4, "9=NA")
CCES14$CC14_322_4<-recode(CCES14$CC14_322_4, "2=0")
model4<-glm(CC14_322_4~SDO+RWA+SDO*RWA+male+partyid+white+black+hisp, data=CCES14, family=binomial)
summary(model4)
plot_model(model4,type="int")

#alternate DP - deport
CCES14$CC14_322_5<-recode(CCES14$CC14_322_5, "8=NA")
CCES14$CC14_322_5<-recode(CCES14$CC14_322_5, "9=NA")
CCES14$CC14_322_5<-recode(CCES14$CC14_322_5, "2=0")
model5<-glm(CC14_322_5~SDO+RWA+SDO*RWA+male+partyid+white+black+hisp, data=CCES14, family=binomial)
summary(model5)
plot_model(model5,type="int")