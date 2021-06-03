data(ToothGrowth)
str(ToothGrowth)
summary(ToothGrowth)


t.test(len~supp, data = ToothGrowth)


g <- ggplot(data = ToothGrowth, aes(x=factor(dose),y=len))
label <- c("Orange Juice", "Vitamin C")
names(label) <- c("OJ", "VC")
g + geom_boxplot(aes(fill=supp),show.legend=FALSE) + 
    facet_grid(.~supp, labeller = labeller(supp=label)) + 
    labs(title = 'Tooth length by dose', x='Dose (mg/day)', y='Tooth length')



dose1 <- subset(ToothGrowth,dose==.5)
dose2 <- subset(ToothGrowth,dose==1)
dose3 <- subset(ToothGrowth,dose==2)


t.test(len~supp, data = dose1)

t.test(len~supp, data = dose2)

t.test(len~supp, data = dose3)