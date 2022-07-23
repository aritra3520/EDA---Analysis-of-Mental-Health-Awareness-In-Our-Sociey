Analysis_data <- read.csv("F:/Aritra MSc/PDS- Sudeep Sir/New folder/ANALYSIS OF MENTAL HEALTH AWARENESS IN OUR SOCIETY.csv",header=T)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

Analysis_data1<- Analysis_data
#1:
Analysis_data1$Age<-cut(as.numeric(Analysis_data$Age),6)
Analysis_data1%>%
  group_by( Do.you.know.about.the.term..MENTAL.HEALTH...,Age)%>%
  summarise(freq = n())%>%
  ggplot()+
  geom_bar(mapping = aes(x = Age,y= freq,fill = Do.you.know.about.the.term..MENTAL.HEALTH...),stat = "identity",position = "fill")+
  labs(y = "no of persons", title = "Age-wise awareness of the term Mental Health")+
  theme_dark()


#2:
Analysis_data1$Gender <- replace(Analysis_data1$Gender,c(1,2),"Others")
Analysis_data1%>%
  group_by(Do.you.know.about.the.term..MENTAL.HEALTH...,Gender)%>%
  summarise(Frequency=n())%>%
  ggplot()+
  geom_bar(aes(x=`Gender`,y=`Frequency`,fill=`Do.you.know.about.the.term..MENTAL.HEALTH...`),stat="identity",position="dodge")+
  labs(title = "Awareness of the Term MENTAL HEALTH",
       subtitle = "(Gender Wise)")+
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(color = "red"))+
  theme_dark()+
  scale_fill_brewer(palette = "Set2")


#3:
Analysis_data1%>%
  group_by( Do.you.know.about.the.term..MENTAL.HEALTH...,What.is.your.profession.,Where.do.you.live.)%>%
  summarise(freq = n())%>%
  ggplot()+
  geom_bar(mapping = aes(x = What.is.your.profession.,y= freq,fill = Do.you.know.about.the.term..MENTAL.HEALTH...),stat = "identity",position = "fill")+
  labs(y = "no of persons", title = "Profession-wise awareness of the term Mental Health",subtitle = "(According to place of residence)")+
  theme_dark()+
  facet_wrap(~Where.do.you.live.,nrow = 3)+
  scale_fill_brewer(palette = "Spectral")


#4:
Analysis_data1$Gender <- replace(Analysis_data1$Gender,c(1,2),"Others")
Analysis_data1%>%
  group_by(Do.you.know.about.the.term..MENTAL.HEALTH...,Have.you.ever.been.into.a.course.or.a.training.program.involving.mental.health.awarness.,Gender)%>%
  summarise(Frequency = n())%>%
  ggplot()+
  geom_count(aes(x= Have.you.ever.been.into.a.course.or.a.training.program.involving.mental.health.awarness.,y = Frequency,color = Do.you.know.about.the.term..MENTAL.HEALTH...))+
  labs(title = "People attending training program involving mental health awareness")+
  facet_wrap(~Gender)+
  theme_dark()


#5:
Analysis_data1%>%
  group_by(Do.you.know.about.the.term..MENTAL.HEALTH...,Have.you.ever.been.into.a.course.or.a.training.program.involving.mental.health.awarness.,What.is.your.profession.)%>%
  summarise(Frequency = n())%>%
  ggplot()+
  geom_count(aes(x= Have.you.ever.been.into.a.course.or.a.training.program.involving.mental.health.awarness.,y = Frequency,color = Do.you.know.about.the.term..MENTAL.HEALTH...))+
  labs(title = "People attending training program involving mental health awareness")+
  facet_wrap(~What.is.your.profession.)+
  theme_dark()


#6:
Analysis_data1%>%
  group_by(Do.you.know.about.the.term..MENTAL.HEALTH...,Have.you.ever.been.into.a.course.or.a.training.program.involving.mental.health.awarness.,Where.do.you.live.)%>%
  summarise(Frequency = n())%>%
  ggplot()+
  geom_count(aes(x= Have.you.ever.been.into.a.course.or.a.training.program.involving.mental.health.awarness.,y = Frequency,color = Do.you.know.about.the.term..MENTAL.HEALTH...))+
  labs(title = "People attending training program involving mental health awareness")+
  facet_wrap(~Where.do.you.live.)+
  theme_dark()


#7:
Analysis_data1$Gender <- replace(Analysis_data1$Gender,c(1,2),"Others")
Analysis_data1%>%
  group_by(Do.you.know.anyone.who.has.experienced.any.mental.health.issue.,Have.you.ever.recommended.any.of.your.friends.or.family.members.to.any.professional.regarding.mental.health.,Gender)%>%
  summarise(freq = n())%>%
  ggplot()+
  geom_bar(aes(x = Have.you.ever.recommended.any.of.your.friends.or.family.members.to.any.professional.regarding.mental.health.,y = freq, 
               fill = Do.you.know.anyone.who.has.experienced.any.mental.health.issue.),stat = "identity",position = "dodge")+
  labs(y = "no of persons recommended",title = "Recommending anyone to any mental health professional",
       subtitle = "(Gender wise)")+
  theme_dark()+
  facet_wrap(~Gender,ncol = 5)




#8:
Analysis_data1%>%
  group_by(Do.you.know.anyone.who.has.experienced.any.mental.health.issue.,Have.you.ever.recommended.any.of.your.friends.or.family.members.to.any.professional.regarding.mental.health.,Where.do.you.live.)%>%
  summarise(freq = n())%>%
  ggplot()+
  geom_bar(aes(x = Have.you.ever.recommended.any.of.your.friends.or.family.members.to.any.professional.regarding.mental.health.,y = freq, 
               fill = Do.you.know.anyone.who.has.experienced.any.mental.health.issue.),stat = "identity",position = "dodge")+
  labs(y = "no of persons recommended",title = "Recommending anyone to any mental health professional",
       subtitle = "(Location wise)")+
  theme_dark()+
  facet_wrap(~Where.do.you.live.,ncol = 5)



#9:
Analysis_data1$Age<-cut(as.numeric(Analysis_data$Age),6)
Analysis_data1$Age
Analysis_data1%>%
  group_by(Do.you.know.anyone.who.has.experienced.any.mental.health.issue.,Have.you.ever.recommended.any.of.your.friends.or.family.members.to.any.professional.regarding.mental.health.,Age)%>%
  summarise(freq = n())%>%
  ggplot()+
  geom_bar(aes(x = Have.you.ever.recommended.any.of.your.friends.or.family.members.to.any.professional.regarding.mental.health.,y = freq, 
               fill = Do.you.know.anyone.who.has.experienced.any.mental.health.issue.),stat = "identity",position = "dodge")+
  labs(y = "no of persons recommended",title = "Recommending anyone to any mental health professional",
       subtitle = "(Age wise)")+
  theme_dark()+
  facet_wrap(~Age,ncol = 5)




#10:
Analysis_data1%>%
  group_by(Do.you.know.anyone.who.has.experienced.any.mental.health.issue.,Have.you.ever.recommended.any.of.your.friends.or.family.members.to.any.professional.regarding.mental.health.,What.is.your.profession.)%>%
  summarise(freq = n())%>%
  ggplot()+
  geom_bar(aes(x = Have.you.ever.recommended.any.of.your.friends.or.family.members.to.any.professional.regarding.mental.health.,y = freq, 
               fill = Do.you.know.anyone.who.has.experienced.any.mental.health.issue.),stat = "identity",position = "dodge")+
  labs(y = "no of persons recommended",title = "Recommending anyone to any mental health professional",
       subtitle = "(Profession wise)")+
  theme_dark()+
  facet_wrap(~What.is.your.profession.,ncol = 5)


#11:
Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your.family.members.<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your.family.members.,
                                                                                                                    labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))
Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your..friends.<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your..friends.,
                                                                                                              labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))
Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with.your.family.members..<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with.your.family.members..,
                                                                                                                   labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))
Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with..your.friends.<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with..your.friends.,
                                                                                                            labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))

Analysis_data2 <- Analysis_data1%>%
  select(Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your.family.members.,
         Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your..friends.,
         Are.you.comfortable.with.sharing.your.mental.health..conditions.with.your.family.members..,
         Are.you.comfortable.with.sharing.your.mental.health..conditions.with..your.friends.,
         Where.do.you.live.)
colnames(Analysis_data2)<-c("sharing physical health conditions with family members",
                            "sharing mental health conditions with family members",
                            "sharing physical health conditions with friends",
                            "sharing mental health conditions with friends","Where.do.you.live.")
Analysis_data3<-Analysis_data2%>% pivot_longer(., cols = c(`sharing physical health conditions with family members`,                                                           `sharing mental health conditions with family members`,
                                                           `sharing physical health conditions with friends`,
                                                           `sharing mental health conditions with friends`),
                                               names_to = "Sharing with Friends and Family", values_to = "response")
Analysis_data3$`Sharing with Friends and Family`<-factor(Analysis_data3$`Sharing with Friends and Family`)

Analysis_data3%>%
  group_by(`Sharing with Friends and Family`,response,Where.do.you.live.)%>%
  summarise(no_of_respondants = n())%>%
  ggplot(aes(x = `Sharing with Friends and Family` , y = no_of_respondants, fill = response)) +
  geom_bar(stat="identity",position = "dodge")+
  facet_wrap(~Where.do.you.live.,ncol = 3)+
  labs(title = "Persons comfortable to share physical and mental health to their family & friends",
       subtitle = "(Location wise)")+
  theme_dark()+
  coord_flip()
  


  




#12:
Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your.family.members.<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your.family.members.,
                                                                                                                    labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))
Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your..friends.<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your..friends.,
                                                                                                              labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))
Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with.your.family.members..<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with.your.family.members..,
                                                                                                                   labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))
Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with..your.friends.<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with..your.friends.,
                                                                                                            labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))                       
Analysis_data2 <- Analysis_data1%>%
  select(Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your.family.members.,
         Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your..friends.,
         Are.you.comfortable.with.sharing.your.mental.health..conditions.with.your.family.members..,
         Are.you.comfortable.with.sharing.your.mental.health..conditions.with..your.friends.,
         What.is.your.profession.)

colnames(Analysis_data2)<-c("sharing physical health conditions with family members",
                            "sharing mental health conditions with family members",
                            "sharing physical health conditions with friends",
                            "sharing mental health conditions with friends","What.is.your.profession.")
Analysis_data3<-Analysis_data2%>% pivot_longer(., cols = c(`sharing physical health conditions with family members`,
                                                           `sharing mental health conditions with family members`,
                                                           `sharing physical health conditions with friends`,
                                                           `sharing mental health conditions with friends`),
                                               names_to = "Sharing with Friends and Family", values_to = "response")
Analysis_data3$`Sharing with Friends and Family`<-factor(Analysis_data3$`Sharing with Friends and Family`)

Analysis_data3%>%
  group_by(`Sharing with Friends and Family`,response,What.is.your.profession.)%>%
  summarise(no_of_respondants = n())%>%
  ggplot(aes(x = `Sharing with Friends and Family` , y = no_of_respondants, fill = response)) +
  geom_bar(stat="identity",position = "dodge")+
  facet_wrap(~What.is.your.profession.,ncol = 4)+
  labs(title = "Persons comfortable to share physical and mental health to their family & friends",
       subtitle = "(Profession wise)")+
  theme_dark()+
  coord_flip()







#13:
Analysis_data1$Age<-cut(as.numeric(Analysis_data$Age),6)
Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your.family.members.<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your.family.members.,
                                                                                                                    labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))
Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your..friends.<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your..friends.,
                                                                                                              labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))
Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with.your.family.members..<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with.your.family.members..,
                                                                                                                   labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))
Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with..your.friends.<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with..your.friends.,
                                                                                                            labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))                       
Analysis_data2 <- Analysis_data1%>%
  select(Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your.family.members.,
         Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your..friends.,
         Are.you.comfortable.with.sharing.your.mental.health..conditions.with.your.family.members..,
         Are.you.comfortable.with.sharing.your.mental.health..conditions.with..your.friends.,
         Age)

colnames(Analysis_data2)<-c("sharing physical health conditions with family members",
                            "sharing mental health conditions with family members",
                            "sharing physical health conditions with friends",
                            "sharing mental health conditions with friends","Age")
Analysis_data3<-Analysis_data2%>% pivot_longer(., cols = c(`sharing physical health conditions with family members`,
                                                           `sharing mental health conditions with family members`,
                                                           `sharing physical health conditions with friends`,
                                                           `sharing mental health conditions with friends`),
                                               names_to = "Sharing with Friends and Family", values_to = "response")
Analysis_data3$`Sharing with Friends and Family`<-factor(Analysis_data3$`Sharing with Friends and Family`)

Analysis_data3%>%
  group_by(`Sharing with Friends and Family`,response,Age)%>%
  summarise(no_of_respondants = n())%>%
  ggplot(aes(x = `Sharing with Friends and Family` , y = no_of_respondants, fill = response)) +
  geom_bar(stat="identity",position = "dodge")+
  facet_wrap(~Age,ncol = 5)+
  labs(title = "Persons comfortable to share physical and mental health to their family & friends",
       subtitle = "(Age wise)")+
  theme_dark()+
  coord_flip()







#14:
Analysis_data1$Gender <- replace(Analysis_data1$Gender,c(1,2),"Others")
Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your.family.members.<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your.family.members.,
                                                                                                                    labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))
Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your..friends.<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your..friends.,
                                                                                                              labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))
Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with.your.family.members..<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with.your.family.members..,
                                                                                                                   labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))
Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with..your.friends.<- factor(Analysis_data1$Are.you.comfortable.with.sharing.your.mental.health..conditions.with..your.friends.,
                                                                                                            labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))                       
Analysis_data2 <- Analysis_data1%>%
  select(Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your.family.members.,
         Are.you.comfortable.with.sharing.your.physical.health..conditions.with.your..friends.,
         Are.you.comfortable.with.sharing.your.mental.health..conditions.with.your.family.members..,
         Are.you.comfortable.with.sharing.your.mental.health..conditions.with..your.friends.,
         Gender)

colnames(Analysis_data2)<-c("sharing physical health conditions with family members",
                            "sharing mental health conditions with family members",
                            "sharing physical health conditions with friends",
                            "sharing mental health conditions with friends","Gender")
Analysis_data3<-Analysis_data2%>% pivot_longer(., cols = c(`sharing physical health conditions with family members`,
                                                           `sharing mental health conditions with family members`,
                                                           `sharing physical health conditions with friends`,
                                                           `sharing mental health conditions with friends`),
                                               names_to = "Sharing with Friends and Family", values_to = "response")
Analysis_data3$`Sharing with Friends and Family`<-factor(Analysis_data3$`Sharing with Friends and Family`)

Analysis_data3%>%
  group_by(`Sharing with Friends and Family`,response,Gender)%>%
  summarise(no_of_respondants = n())%>%
  ggplot(aes(x = `Sharing with Friends and Family` , y = no_of_respondants, fill = response)) +
  geom_bar(stat="identity",position = "dodge")+
  facet_wrap(~Gender,ncol = 3)+
  labs(title = "Persons comfortable to share physical and mental health to their family & friends",
       subtitle = "(Gender wise)")+
  theme_dark()+
  coord_flip()







#15:
Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Social.media.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Social.media.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Advertisements.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Advertisements.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Parents.taking.an.active.role.to.make.their.children.more.aware.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Parents.taking.an.active.role.to.make.their.children.more.aware.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Sharing.with.friends.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Sharing.with.friends.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Different.non.beneficial.promotional.clubs.or.movements.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Different.non.beneficial.promotional.clubs.or.movements.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Educational.institutes.arranging.regular.interactive.sessions.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Educational.institutes.arranging.regular.interactive.sessions.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Offices.and.companies.arranging.regular.discussion.sessions.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Offices.and.companies.arranging.regular.discussion.sessions.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Government.programs.and.initiatives.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Government.programs.and.initiatives.)

df <- Analysis_data1%>%
  select(Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Social.media.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Advertisements.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Parents.taking.an.active.role.to.make.their.children.more.aware.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Sharing.with.friends.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Different.non.beneficial.promotional.clubs.or.movements.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Educational.institutes.arranging.regular.interactive.sessions.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Offices.and.companies.arranging.regular.discussion.sessions.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Government.programs.and.initiatives.,
         Where.do.you.live.)

colnames(df) <- c("Social Media","Advertisements","Role of the parents","Sharing with friends",
                  "Non Beneficial Promotional Clubs or Movements","Educational institutes",
                  "Offices and Companies","Government Programs and Initiatives","Location")

df1 <- df%>%
  pivot_longer(.,cols = c("Social Media","Advertisements","Role of the parents","Sharing with friends",
                          "Non Beneficial Promotional Clubs or Movements","Educational institutes",
                          "Offices and Companies","Government Programs and Initiatives"),
               names_to = "Different platforms to spread Mental Health Awareness",values_to = "Ratings")

df1$`Different platforms to spread Mental Health Awareness`<-factor(df1$`Different platforms to spread Mental Health Awareness`)

df1%>%
  group_by(`Different platforms to spread Mental Health Awareness`,Ratings,Location)%>%
  summarise(no_of_ratings = n())%>%
  ggplot(aes(x = `Different platforms to spread Mental Health Awareness`,y = no_of_ratings,fill = Ratings))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title = "Ratings to different platforms to spread Mental Health Awareness",
       subtitle = "(Location wise)")+
  facet_wrap(~Location,ncol = 3)+
  coord_flip()+
  theme_dark()







#16:
Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Social.media.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Social.media.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Advertisements.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Advertisements.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Parents.taking.an.active.role.to.make.their.children.more.aware.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Parents.taking.an.active.role.to.make.their.children.more.aware.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Sharing.with.friends.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Sharing.with.friends.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Different.non.beneficial.promotional.clubs.or.movements.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Different.non.beneficial.promotional.clubs.or.movements.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Educational.institutes.arranging.regular.interactive.sessions.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Educational.institutes.arranging.regular.interactive.sessions.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Offices.and.companies.arranging.regular.discussion.sessions.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Offices.and.companies.arranging.regular.discussion.sessions.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Government.programs.and.initiatives.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Government.programs.and.initiatives.)

df2 <- Analysis_data1%>%
  select(Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Social.media.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Advertisements.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Parents.taking.an.active.role.to.make.their.children.more.aware.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Sharing.with.friends.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Different.non.beneficial.promotional.clubs.or.movements.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Educational.institutes.arranging.regular.interactive.sessions.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Offices.and.companies.arranging.regular.discussion.sessions.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Government.programs.and.initiatives.,
         What.is.your.profession.)

colnames(df2) <- c("Social Media","Advertisements","Role of the parents","Sharing with friends",
                   "Non Beneficial Promotional Clubs or Movements","Educational institutes",
                   "Offices and Companies","Government Programs and Initiatives","Profession")

df3 <- df2%>%
  pivot_longer(.,cols = c("Social Media","Advertisements","Role of the parents","Sharing with friends",
                          "Non Beneficial Promotional Clubs or Movements","Educational institutes",
                          "Offices and Companies","Government Programs and Initiatives"),
               names_to = "Different platforms to spread Mental Health Awareness",values_to = "Ratings")
df3$`Different platforms to spread Mental Health Awareness`<- factor(df3$`Different platforms to spread Mental Health Awareness`)
df3%>%
  group_by(`Different platforms to spread Mental Health Awareness`,Ratings,Profession)%>%
  summarise(freq_ratings = n())%>%
  ggplot(aes(x = `Different platforms to spread Mental Health Awareness`,y = freq_ratings,fill = Ratings))+
  geom_bar(stat = "identity",position = "dodge")+
  facet_wrap(~Profession,ncol = 4)+
  labs(title = "Ratings to different platforms to spread Mental Health Awareness",
       subtitle = "(Profession Wise)")+
  coord_flip()+
  theme_dark()







#17:
Analysis_data1$Age<-cut(as.numeric(Analysis_data$Age),6);Analysis_data1$Age
Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Social.media.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Social.media.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Advertisements.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Advertisements.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Parents.taking.an.active.role.to.make.their.children.more.aware.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Parents.taking.an.active.role.to.make.their.children.more.aware.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Sharing.with.friends.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Sharing.with.friends.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Different.non.beneficial.promotional.clubs.or.movements.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Different.non.beneficial.promotional.clubs.or.movements.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Educational.institutes.arranging.regular.interactive.sessions.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Educational.institutes.arranging.regular.interactive.sessions.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Offices.and.companies.arranging.regular.discussion.sessions.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Offices.and.companies.arranging.regular.discussion.sessions.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Government.programs.and.initiatives.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Government.programs.and.initiatives.)

df4 <- Analysis_data1%>%
  select(Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Social.media.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Advertisements.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Parents.taking.an.active.role.to.make.their.children.more.aware.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Sharing.with.friends.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Different.non.beneficial.promotional.clubs.or.movements.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Educational.institutes.arranging.regular.interactive.sessions.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Offices.and.companies.arranging.regular.discussion.sessions.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Government.programs.and.initiatives.,
         Age)
colnames(df4) <- c("Social Media","Advertisements","Role of the parents","Sharing with friends",
                   "Non Beneficial Promotional Clubs or Movements","Educational institutes",
                   "Offices and Companies","Government Programs and Initiatives","Age")

df5 <- df4%>%
  pivot_longer(.,cols = c("Social Media","Advertisements","Role of the parents","Sharing with friends",
                          "Non Beneficial Promotional Clubs or Movements","Educational institutes",
                          "Offices and Companies","Government Programs and Initiatives"),
               names_to = "Different platforms to spread Mental Health Awareness",values_to = "Ratings")

df5$`Different platforms to spread Mental Health Awareness`<- factor(df5$`Different platforms to spread Mental Health Awareness`)  

df5%>%
  group_by(`Different platforms to spread Mental Health Awareness`,Ratings,Age)%>%
  summarise(freq_Ratings = n())%>%
  ggplot(aes(x = `Different platforms to spread Mental Health Awareness`,y = freq_Ratings ,fill = Ratings))+
  geom_bar(stat = "identity", position = "fill")+
  facet_wrap(~Age,ncol = 5)+
  labs(title = "Ratings to different platforms to spread Mental Health Awareness",
       subtitle = "(Age Wise)")+
  coord_flip()+
  theme_dark()






#18:
Analysis_data1$Gender <- replace(Analysis_data1$Gender,c(1,2),"Others")
Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Advertisements.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Advertisements.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Parents.taking.an.active.role.to.make.their.children.more.aware.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Parents.taking.an.active.role.to.make.their.children.more.aware.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Sharing.with.friends.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Sharing.with.friends.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Different.non.beneficial.promotional.clubs.or.movements.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Different.non.beneficial.promotional.clubs.or.movements.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Educational.institutes.arranging.regular.interactive.sessions.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Educational.institutes.arranging.regular.interactive.sessions.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Offices.and.companies.arranging.regular.discussion.sessions.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Offices.and.companies.arranging.regular.discussion.sessions.)

Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Government.programs.and.initiatives.<- factor(Analysis_data1$Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Government.programs.and.initiatives.)

df6 <- Analysis_data1%>%
  select(Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Social.media.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Advertisements.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Parents.taking.an.active.role.to.make.their.children.more.aware.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Sharing.with.friends.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Different.non.beneficial.promotional.clubs.or.movements.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Educational.institutes.arranging.regular.interactive.sessions.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Offices.and.companies.arranging.regular.discussion.sessions.,
         Please.rate.out.of.10.according.to.the.impact.the.following.departments.will.have.in.playing.a.crucial.role.in.spreading.awareness.regarding.mental.health..the.options.are.upto.10.in.each.row.if.all.the.options.are.not.coming.at.a.glance.please.try.swiping.right...Government.programs.and.initiatives.,
         Gender)

colnames(df6) <- c("Social Media","Advertisements","Role of the parents","Sharing with friends",
                   "Non Beneficial Promotional Clubs or Movements","Educational institutes",
                   "Offices and Companies","Government Programs and Initiatives","Gender")

df7 <- df6%>%
  pivot_longer(.,cols = c("Social Media","Advertisements","Role of the parents","Sharing with friends",
                          "Non Beneficial Promotional Clubs or Movements","Educational institutes",
                          "Offices and Companies","Government Programs and Initiatives"),
               names_to = "Different platforms to spread Mental Health Awareness",values_to = "Ratings")

df7$`Different platforms to spread Mental Health Awareness`<- factor(df7$`Different platforms to spread Mental Health Awareness`)  

df7%>%
  group_by(`Different platforms to spread Mental Health Awareness`,Ratings,Gender)%>%
  summarise(freq_Ratings = n())%>%
  ggplot(aes(x = `Different platforms to spread Mental Health Awareness`,y = freq_Ratings ,fill = Ratings))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~Gender,ncol = 3)+
  labs(title = "Ratings to different platforms to spread Mental Health Awareness",subtitle = "(Gender Wise)")+
  coord_flip()+
  theme_dark()



#19:
Analysis_data1%>%
  group_by( Do.you.feel.Covid..19.situation.has.caused.more.mental.stress.than.usual.time.,What.is.your.profession.)%>%
  summarise(frequency=n())%>%
  ggplot()+
  geom_bar(aes(x=What.is.your.profession.,y= frequency,fill= Do.you.feel.Covid..19.situation.has.caused.more.mental.stress.than.usual.time.),stat="identity",position="fill")+
  labs(title= "COVID 19 effect of on mental stress",
       subtitle= "(Profession wise)")+
  theme_dark()+
  coord_flip()+
  scale_fill_brewer(palette = "Set2")



#20:
Analysis_data1%>%
  group_by( Do.you.feel.Covid..19.situation.has.caused.more.mental.stress.than.usual.time.,Where.do.you.live.)%>%
  summarise(frequency=n())%>%
  ggplot()+
  geom_bar(aes(x=Where.do.you.live.,y= frequency,fill= Do.you.feel.Covid..19.situation.has.caused.more.mental.stress.than.usual.time.),stat="identity",position="dodge")+
  labs(title= "COVID 19 effect of on mental stress",
       subtitle= "(Location wise)")+
  theme_dark()+
  scale_fill_brewer(palette = "Set1")



#21:
Analysis_data1$Age<-cut(as.numeric(Analysis_data$Age),6)
Analysis_data1%>%
  group_by( Do.you.feel.Covid..19.situation.has.caused.more.mental.stress.than.usual.time.,Age)%>%
  summarise(frequency=n())%>%
  ggplot()+
  geom_bar(aes(x=Age,y= frequency,fill= Do.you.feel.Covid..19.situation.has.caused.more.mental.stress.than.usual.time.),stat="identity",position="fill")+
  labs(title= "COVID 19 effect of on mental stress",
       subtitle= "(Age wise)")+
  theme_dark()+
  scale_fill_brewer(palette = "Dark2")




#22:
Analysis_data1$Do.you.feel.people.surrounding.you.give.enough.importance.to.mental.health.<- factor(Analysis_data1$Do.you.feel.people.surrounding.you.give.enough.importance.to.mental.health.,
                                                                                                    labels = c("Not at all","Not Much","Somewhat","Yes","Very Much"))

Analysis_data1%>%
  group_by( Do.you.feel.people.surrounding.you.give.enough.importance.to.mental.health.,What.is.your.profession.)%>%
  summarise(frequency=n())%>%
  ggplot()+
  geom_bar(aes(x=What.is.your.profession.,y=frequency,fill=Do.you.feel.people.surrounding.you.give.enough.importance.to.mental.health.),stat="identity",position="fill")+
  coord_polar("y",start = 2)+
  labs(title= "Importance of mental health",
       subtitle="(Profession wise)")+
  theme_dark()
   



#23:
Analysis_data1%>%
  group_by( Do.you.feel.people.surrounding.you.give.enough.importance.to.mental.health.,Where.do.you.live. )%>%
  summarise(frequency=n())%>%
  ggplot()+
  geom_bar(aes(x=Where.do.you.live.,y=frequency,fill=Do.you.feel.people.surrounding.you.give.enough.importance.to.mental.health.),stat="identity",position="fill")+
  coord_polar("y")+
  labs(title= "Importance of mental health",
       subtitle="(Location wise)")+
  theme_dark()




#24:
Analysis_data1$Age<-cut(as.numeric(Analysis_data$Age),6)
Analysis_data1%>%
  group_by( Do.you.feel.people.surrounding.you.give.enough.importance.to.mental.health.,Age)%>%
  summarise(frequency=n())%>%
  ggplot()+
  geom_bar(aes(x=Age,y=frequency,fill=Do.you.feel.people.surrounding.you.give.enough.importance.to.mental.health.),stat="identity",position="fill")+
  coord_polar("y")+
  labs(title= "Importance of mental health",
       subtitle="(Age wise)")+
  theme_dark()






  
