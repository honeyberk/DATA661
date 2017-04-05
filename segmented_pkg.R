
# https://www.r-bloggers.com/r-for-ecologists-putting-together-a-piecewise-regression/

####

library(segmented)
options(scipen = 999)

gold <- read.csv("gold.csv", header = T)
head(gold)
str(gold)
df0 <- gold[c("OAT", "Elec")]
plot(df0)

#### Clusters
df0 <- gold[c("OAT", "Elec")]
plot(df0)

library(ggplot2)
ggplot(df0, aes(OAT, Elec, color=OAT)) + geom_point() + theme_bw()

# Divide OATs into clusters
(kc <- kmeans(df0, 2)) 

table(df0$OAT, kc$cluster)

# Perform K-Means with 2 clusters
set.seed(7)
km1 = kmeans(df0, 2, nstart=100)

# Plot results
plot(df0, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)

# Check for the optimal number of clusters given the data

mydata <- df0
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

# Perform K-Means with the optimal number of clusters identified from the Elbow method
set.seed(7)
km2 = kmeans(df0, 3, nstart=100)

# Examine the result of the clustering algorithm
km2




df0.stand <- scale(df0[-1]) 

# K-Means
k.means.fit <- kmeans(df0.stand, 3) # k = 3

attributes(k.means.fit)
k.means.fit$centers

# Clusters:
k.means.fit$cluster

# Cluster size:
k.means.fit$size

library(cluster)
clusplot(df0.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


fit.glm <- glm(Elec ~ OAT, data = df0)
fit.seg <- segmented(fit.glm, seg.Z = ~OAT, psi = 50)
print(fit.seg)
summary(fit.seg)

davies.test(fit.glm, ~OAT, k=24)

slope(fit.seg)

fit.glm <- update(fit.glm, .~.-OAT)
fit.seg1 <- update(fit.seg)

fit.seg1$psi

x <- df0$OAT
y <- df0$Elec
plot(x,y, pch=16)


####

#http://www.statmethods.net/advstats/cluster.html

# Determine number of clusters
wss <- (nrow(df0)-1)*sum(apply(df0,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df0, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(df0, 5) # 5 cluster solution
# get cluster means 
aggregate(df0,by=list(fit$cluster),FUN=mean)
# append cluster assignment
df0 <- data.frame(df0, fit$cluster)

# Ward Hierarchical Clustering
d <- dist(df0, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")



# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(df0, method.hclust="ward.D2",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)




# Model Based Clustering
library(mclust)
fit <- Mclust(df0)
plot(fit) # plot results 
summary(fit) # display the best model



# K-Means Clustering with 5 clusters
fit <- kmeans(df0, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(df0, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)



###

library(graphics)
heatmap(elec)
