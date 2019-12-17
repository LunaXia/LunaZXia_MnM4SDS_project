#####################################################
# Variable importance and partial dependence plot
######################################################


varImpPlot(rf.Heroin, n.var = 20, class = "1", type = 1,
           main="Variable importancen for predicting Heroin hotspots")

partialPlot(rf.Heroin, Heroin.train[,c(5:187)], nb_t_2, which.class = "1", main="Partial Dependence on nb_t_1", ylab="Logits, log og fraction of votes")
randomForest::partialPlot(rf.Heroin, Heroin.train[,c(5:187)], HS_t_1, which.class = "1",main="Partial Dependence on HS_t_1", ylab="Logits, log og fraction of votes")


###################################
# ggplot variable importance plots
###################################
Heroin_imp <- as.data.frame(ranger::importance(ranger.Heroin5))
write.csv(Heroin_imp, file = "imp_heroin.csv")

Synthetic_imp <- as.data.frame(ranger::importance(ranger.Synthetic4))
write.csv(Synthetic_imp, file = "imp_synthetic.csv")

library(ggplot2)
imp <- read.csv(file="imp_heroin.csv")
colnames(imp)[1]="varnames"
p <- ggplot(imp, aes(x=reorder(varnames, Gini), y=Gini,colour = as.factor(var_categ))) +
  geom_point() +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=Gini)) +
  ylim(0,100)+
  ylab("Variable importance score (Gini index)") +
  xlab("Variable Name")+
  coord_flip()+
  theme_classic()+
  theme(axis.text.y = element_text(colour ='black', size = '12'))

p + scale_colour_manual(values = c("#F8766D" ,"#7CAE00", "#C77CFF","#00BFC4")) 


imp <- read.csv(file="imp_synthetic.csv")
colnames(imp)[1]="varnames"
p <- ggplot(imp, aes(x=reorder(varnames, Gini), y=Gini,colour = as.factor(var_categ))) +
  geom_point() +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=Gini)) +
  ylim(0,100)+
  ylab("Variable importance score (Gini index)") +
  xlab("Variable Name")+
  coord_flip()+
  theme_classic()+
  theme(axis.text.y = element_text(colour ='black',size = '12'))

p + scale_colour_manual(values = c("#F8766D" ,"#7CAE00", "#00BFC4"))  + scale_color_discrete(name="Variable Type")
scales::hue_pal()(4)

###########
# box plot
##########
# boxplot
Heroin2019 <- Heroin[Heroin$Year==191,]
Synthetic2019 <- Synthetic[Synthetic$Year==191,]

merge2019 <- merge(Heroin2019, Synthetic2019, by.x="GEOID_Data", by.y="GEOID_Data", suffixes = c(".a",".b"))
Other <- merge2019[merge2019$HS.a=="0" & merge2019$HS.b=='0',]
Other$group <- 'Other'                   
  
Heroin2019HS <- merge2019[merge2019$HS.a=='1',]
Heroin2019HS$group <- 'Heroin'

Synthetic2019HS <-merge2019[merge2019$HS.b=='1',]
Synthetic2019HS$group <- 'Synthetic'

HeroinSynthetic19 <- rbind(Other,Heroin2019HS,Synthetic2019HS)


HeroinSynthetic19$group  = factor(HeroinSynthetic19$group, levels=c("Heroin", 'Synthetic', 'Other'))


# plot 8 variables
library(ggplot2)
par(mfrow=c(2,4),mai = c(0.1, 0.1, 0.1, 0.1))

HeroinSynthetic19$title <- "Transit"
e <- ggplot(HeroinSynthetic19, aes(x = group, y = D5ae.a, fill=group))+ geom_boxplot(outlier.shape = NA) 
e1<- e + ylab("Working age population within 45 minutes auto travel time") + xlab("")+ facet_grid(. ~ title) + theme(legend.position = "none")

HeroinSynthetic19$title <- "Transit"
e <- ggplot(HeroinSynthetic19, aes(x = group, y = Pct_MeWgWr.a, fill=group))+ geom_boxplot(outlier.shape = NA) 
e2<- e + ylab("% of all medium-wage workers in region accessible by transit") + xlab("")+ facet_grid(. ~ title)+ theme(legend.position = "none")

HeroinSynthetic19$title <- "Built environment"
e <- ggplot(HeroinSynthetic19, aes(x = group, y = D5cri.a, fill=group))+ geom_boxplot(outlier.shape = NA) + theme(legend.position = "none")
e3 <- e + ylab("Regional Centrality Index") + xlab("")+ facet_grid(. ~ title)

HeroinSynthetic19$title <- "Built environment"
e <- ggplot(HeroinSynthetic19, aes(x = group, y = VacLot.a, fill=group))+ geom_boxplot(outlier.shape = NA) + theme(legend.position = "none")
e4 <- e + ylab("Vacant lots") + xlab("")+ facet_grid(. ~ title)

HeroinSynthetic19$title <- "Built environmentn"
e <- ggplot(HeroinSynthetic19, aes(x = group, y = VacBldg.a, fill=group))+ geom_boxplot(outlier.shape = NA) + theme(legend.position = "none")
e5 <- e + ylab("Vacant buildings") + xlab("")+ facet_grid(. ~ title)

HeroinSynthetic19$title <- "Built environmentn"
e <- ggplot(HeroinSynthetic19, aes(x = group, y = AlleyCount.a, fill=group))+ geom_boxplot(outlier.shape = NA) + theme(legend.position = "none")
e6 <- e + ylab("Total number of alleys") + xlab("")+ facet_grid(. ~ title)

HeroinSynthetic19$title <- "Sociodemographic"
e <- ggplot(HeroinSynthetic19, aes(x = group, y = MedianHouseIncome.a, fill=group))+ geom_boxplot(outlier.shape = NA) + theme(legend.position = "none")
e7 <- e + ylab("Median household income") + xlab("")+ facet_grid(. ~ title)

HeroinSynthetic19$title <- "Sociodemographic"
e <- ggplot(HeroinSynthetic19, aes(x = group, y = PBachelorHigher.a, fill=group))+ geom_boxplot(outlier.shape = NA) + theme(legend.position = "none")
e8 <- e + ylab("% of population with college degree or higher") + xlab("")+ facet_grid(. ~ title)


gridExtra::grid.arrange(e1, e2, e3, e4, e5, e6, e7, e8,  ncol=4, nrow = 2)

###################################
#
# Plot partial dependence function
#
###################################
install.packages('pdp')
library('pdp')

Heroin.train <- Heroin[(Heroin$Year >= 172 & Heroin$Year <= 182 ),]
Heroin.train[is.na(Heroin.train)] <- 0


#ranger random forest, with probability
set.seed(123)
ranger.Heroin5.prob <- ranger(HS ~ ., data = Heroin.train[,c(EPAsel_Heroin,ACSfeaV,KEYLOCfeaV,timelag123V,HS_V)], importance = 'impurity', mtry = 9, num.trees=300, split.select.weights = varWeight_Heroin5,probability = TRUE)
ranger::importance(ranger.Heroin5.prob)

set.seed(123)
ranger.synthetic4.prob <- ranger(HS ~ ., data = Synthetic.train[,c(EPAsel_Heroin,ACSfeaV,KEYLOCfeaV,timelag12V,HS_V)], importance = 'impurity', mtry = 14, num.trees=250, sample.fraction = c(0.25*0.632,0.632), split.select.weights = varWeight_Synthetic4,probability = TRUE)
ranger::importance(ranger.synthetic4.prob)

Synthetic_imp_all <- as.data.frame(ranger::importance(ranger.synthetic4.prob))
write.csv(Synthetic_imp_all, file = "imp_synthetic_all.csv")

# ggplot2-based PDP
p1 <- ranger.Heroin5.prob %>%  # the %>% operator is read as "and then"
  partial(pred.var = "nb_t_1", which.class = 2) %>%
  autoplot(smooth = TRUE, ylab = expression(f(nb_t_1)), xlab='# of neighboring BGs belonged to a hotspot at t-1 period') +
  theme_light() +
  ggtitle("Heroin: nb_t_1")

p2 <- ranger.Heroin5.prob %>%  # the %>% operator is read as "and then"
  partial(pred.var = "trend_nb_12", which.class = 2) %>%
  autoplot(smooth = TRUE, ylab = expression(f(trend_nb_12)), xlab = 'Changing trend in neighborhood between t-2 and t-1 period') +
  theme_light() +
  ggtitle("Heroin: trend_nb_12")

pd.h <- partial(ranger.Heroin5.prob, pred.var = c("nb_t_1", "trend_nb_12"), which.class = 2)

p3 <- plotPartial(pd.h, levelplot = FALSE,lab = "hotspot", colorkey = FALSE, 
                  screen = list(z = -20, x = -60), main="Joint effect of nb_t_1 and trend_nb_12")

p4 <- ranger.synthetic4.prob %>%  # the %>% operator is read as "and then"
  partial(pred.var = "nb_t_1", which.class = 2) %>%
  autoplot(smooth = TRUE, ylab = expression(f(nb_t_1)), xlab='# of neighboring BGs belonged to a hotspot at t-1 period') +
  theme_light() +
  ggtitle("Synthetic drug: nb_t_1")

p5 <- ranger.synthetic4.prob %>%  # the %>% operator is read as "and then"
  partial(pred.var = "trend_nb_12", which.class = 2) %>%
  autoplot(smooth = TRUE, ylab = expression(f(trend_nb_12)), xlab = 'Changing trend in neighborhood between t-2 and t-1 period') +
  theme_light() +
  ggtitle("Synthetic drug: trend_nb_12")

pd.h2 <- partial(ranger.synthetic4.prob, pred.var = c("nb_t_1", "trend_nb_12"), which.class = 2)

p6 <- plotPartial(pd.h2, levelplot = FALSE, zlab = "hotspot", colorkey = FALSE, 
                  screen = list(z = -20, x = -60), main="Joint effect of nb_t_1 and trend_nb_12")

gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow = 2)
