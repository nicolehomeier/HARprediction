# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
        ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
            layout.pos.col = matchidx$col))
        }
    }
}

library(ggplot2)

ds = read.csv("pml-training.csv")
test = read.csv("pml-testing.csv")
#do some visualizations

ds = ds[,-(1:7)]
test = test[,-(1:7)]
nds = ds
for (i in 1:(ncol(ds)-1)){ #skip the classe var
    if (class(ds[,i])=="factor"){nds[,i] = as.numeric(as.character(ds[,i]))}
}
ctna = sapply(ds,function(x) sum(is.na(x)))
nds = ds[,-which(ctna>5000)]
namesnds = names(ds)[which(ctna>5000)]
#check out the test set, no use in predicting on variable that aren't available in the test set
ctnatest = sapply(test, function(x) sum(is.na(x)))
namestest = names(test)[which(ctnatest==0)]

#now have a better set of variables for fitting

keepvars = namestest
cnds = ds[,which(names(ds) %in% namestest)]
cnds$classe = ds$classe

for (i in 1:(ncol(cnds)-1)){
    if (class(cnds[,i])=="factor"){cnds[,i] = as.numeric(as.character(cnds[,i]))}
}
#ctna = sapply(cnds,function(x) sum(is.na(x)))
#ncnds = cnds[,-which(ctna>5000)]
#summary(ncnds)

M = cor(cnds[,-ncol(cnds)])
highcorr=which(M>.8 & M <1,arr.ind=T)
names(cnds)[highcorr[,2]]
names(highcorr[,1])
dropvars = c("roll_belt","accel_belt_y","magnet_belt_x","gyros_forearm_z","magnet_arm_x","accel_dumbbell_x","magnet_arm_y","accel_dumbbell_z")
dpts = which(names(cnds) %in% dropvars)
cnds = cnds[,-dpts]
#do a glm fit to get rid of the remaining non-important "predictors"
glmfit = glm(classe~.,data=cnds,family=binomial)
summary(glmfit)
dropvars = c("gyros_belt_y","gyros_arm_x","gyros_arm_z","pitch_dumbbell","accel_dumbbell_y","magnet_forearm_z")
dpts = which(names(cnds) %in% dropvars)
cnds = cnds[,-dpts]

write.csv(cnds,"reduced-pml-training.csv",row.names=F)
#example of variable we want to drop
qplot(max_yaw_forearm,classe,data=cnds,geom=c("boxplot","jitter"),alpha=.1)

loopit = c(1,9,17,25)
i=25
p1 = qplot(ncnds[,i],classe,data=cnds,geom=c("boxplot","jitter"),alpha=.1,xlab=names(ncnds[i]))
p2 = qplot(ncnds[,i+1],classe,data=cnds,geom=c("boxplot","jitter"),alpha=.1,xlab=names(ncnds[i+1]))
p3 = qplot(ncnds[,i+2],classe,data=cnds,geom=c("boxplot","jitter"),alpha=.1,xlab=names(ncnds[i+2]))
p4 = qplot(ncnds[,i+3],classe,data=cnds,geom=c("boxplot","jitter"),alpha=.1,xlab=names(ncnds[i+3]))
p5 = qplot(ncnds[,i+4],classe,data=cnds,geom=c("boxplot","jitter"),alpha=.1,xlab=names(ncnds[i+4]))
p6 = qplot(ncnds[,i+5],classe,data=cnds,geom=c("boxplot","jitter"),alpha=.1,xlab=names(ncnds[i+5]))
p7 = qplot(ncnds[,i+6],classe,data=cnds,geom=c("boxplot","jitter"),alpha=.1,xlab=names(ncnds[i+6]))
p8 = qplot(ncnds[,i+7],classe,data=cnds,geom=c("boxplot","jitter"),alpha=.1,xlab=names(ncnds[i+7]))
multiplot(p1,p2,p3,p4,p5,p6,p7,p8,cols=2)
#also get rid of gyros_dumbbell_z,magnet_dumbbell_y,gyros_forearm_x

#now create some training and testing data and train some models
set.seed(3233)
inTrain = createDataPartition(cnds$classe,p=.6,list=TRUE)[[1]]
training = cnds[inTrain,]
testing = cnds[-inTrain,]
rpart.grid = expand.grid(cp=seq(0,0.01,0.0005))
rpartfit = train(classe~.,data=training,method="rpart",tuneGrid=rpart.grid,trControl=trainControl(method="cv"))
predrpart = predict(rpartfit,newdata=testing)
confusionMatrix(predrpart,testing$classe)

ldafit = train(classe~.,data=training,method="lda",preProcess=c("center","scale"),trControl=trainControl(method="cv"))
predlda = predict(ldafit,newdata=testing)
confusionMatrix(predlda,testing$classe)

knn.grid = expand.grid(k=seq(3,12,2))
knnfit = train(classe~.,data=training,method="knn",preProcess=c("center","scale"),tuneGrid=knn.grid,trControl=trainControl(method="cv"))
predknn = predict(knnfit,newdata=testing)
confusionMatrix(predknn,testing$classe)

rffit = randomForest(classe~.,data=training)
predrf = predict(rffit,newdata=testing)
confusionMatrix(predrf,testing$classe)
