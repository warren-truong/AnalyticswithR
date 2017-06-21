poll = read.csv("AnonymityPoll.csv")
str(poll)
summary(poll)
table(poll$Smartphone)
summary(poll$Smartphone)

table(poll$Internet.Use, poll$Smartphone)

limited = subset(poll, Internet.Use == 1|Smartphone == 1)

hist(limited$Age)

plot(limited$Age, limited$Info.On.Internet)

max(table(limited$Age, limited$Info.On.Internet))

jitter(c(1,2,3))

plot(jitter(limited$Age),jitter(limited$Info.On.Internet))

tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)