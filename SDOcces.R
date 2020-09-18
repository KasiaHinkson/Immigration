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

yimm<-cbind(UMS324,UMS326,UMS327,UMS328,UMS329)
yimmfactor<-factanal(na.omit(yimm),factors=1,scores="regression")
yimmfactor
cronbach(yimm)
fsy<-factor.scores(yimm,yimmfactor)
fsy<-fsy$scores
CCES14<-cbind(CCES14,fsy)
CCES14$resent<-CCES14$Factor1
CCES14$Factor1<-NULL

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
model<-lm(resent~SDO+auth1+SDO*auth1+male+partyid+black+hisp, data=CCES14)
summary(model)
p1 <- plot_model(model,type="int") +
  ggtitle("Immigrant Resentment") +
  scale_y_continuous(limits = c(-2, 2))
p1

model1<-lm(imminc~SDO+auth1+SDO*auth1+male+partyid+black+hisp, data=CCES14)
summary(model1)
p2 <- plot_model(model1,type="int") +
  ggtitle("Incorporation") +
  scale_y_continuous(limits = c(-2, 2))
p2


coplot(immass~SDO|auth1, panel=panel.car, col="red", rows=1, data=CCES14)
coplot(immass~auth1|SDO, panel=panel.car, col="red", rows=1, data=CCES14)
coplot(imminc~auth1|SDO, panel=panel.car, col="red", rows=1, data=CCES14)
coplot(imminc~SDO|auth1, panel=panel.car, col="red", rows=1, data=CCES14)

#alternate DP - immigrants policies
CCES14$UMS372<-recode(CCES14$UMS372, "998=NA")
CCES14$UMS372<-recode(CCES14$UMS372, "997=NA")
CCES14$UMS372<-recode(CCES14$UMS372, "999=NA")
CCES14$UMS373<-recode(CCES14$UMS373, "998=NA")
CCES14$UMS373<-recode(CCES14$UMS373, "997=NA")
CCES14$UMS373<-recode(CCES14$UMS373, "999=NA")
CCES14$UMS374<-recode(CCES14$UMS374, "998=NA")
CCES14$UMS374<-recode(CCES14$UMS374, "997=NA")
CCES14$UMS374<-recode(CCES14$UMS374, "999=NA")
CCES14$UMS374<-100-CCES14$UMS374
CCES14$UMS375<-recode(CCES14$UMS375, "998=NA")
CCES14$UMS375<-recode(CCES14$UMS375, "997=NA")
CCES14$UMS375<-recode(CCES14$UMS375, "999=NA")

Y3<-cbind(UMS372,UMS373,UMS375)
statusfactor<-factanal(na.omit(Y3),factors=1,scores="regression")
statusfactor
cronbach(Y3)
fs3<-factor.scores(Y3,statusfactor)
fs3<-fs3$scores
CCES14<-cbind(CCES14,fs3)
CCES14$status<-CCES14$Factor1
CCES14$Factor1<-NULL

model3<-lm(status~SDO+auth1+SDO*auth1+male+partyid+black+hisp+UMS312, data=CCES14)
summary(model3)
plot_model(model3,type="int")


#alternate DP - feelings towards immigrants
CCES14$UMS311 #irish immigrants
model2<-lm(UMS311~SDO+auth1+SDO*auth1+male+partyid+black+hisp, data=CCES14)
summary(model2)
plot_model(model2,type="int")

CCES14$UMS312 #illegal immigrants
model3<-lm(UMS312~SDO+auth1+auth1*SDO+male+partyid+black+hisp, data=CCES14)
summary(model3)
plot_model(model3,type="int")

CCES14$UMS313 #african immigrants
model4<-lm(UMS313~SDO+auth1+SDO*auth1+male+partyid+black+hisp, data=CCES14)
summary(model4)
plot_model(model4,type="int")

CCES14$UMS314 #chinese immigrants
model5<-lm(UMS314~SDO+auth1+SDO*auth1+male+partyid+black+hisp, data=CCES14)
summary(model5)
plot_model(model5,type="int")

CCES14$UMS315 #mexican immigrants
model6<-lm(UMS315~SDO+auth1+SDO*auth1+male+partyid+black+hisp, data=CCES14)
summary(model6)
plot_model(model6,type="int")
