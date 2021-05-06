# Motivating Example: Cheese Data
# Load Data
library(bayesm)
data(cheese)

# Remove high volume outlier sales (Top 1%)
cutoff = ceiling(length(cheese$VOLUME)*99/100)
vol_cutoff = sort(cheese$VOLUME)[cutoff]

cheese_new = cheese[which(cheese$VOLUME < vol_cutoff),]
colors= as.numeric(cheese_new$RETAILER)

# Nonlinearity
# Plot total sales volume vs price
par(mar=c(5,4,4,4))
plot(cheese_new$PRICE,cheese_new$VOLUME,xlab="price",ylab="volume",
     pch=16,col=colors,main="",cex=0.3)
plot(cheese_new$DISP,cheese_new$VOLUME,xlab="display",ylab="volume",
     pch=16,col=colors,main="",cex=0.3)

# Interaction
library(scatterplot3d)
scatterplot3d(cheese_new$DISP,cheese_new$PRICE,cheese_new$VOLUME,
              ylab="price",xlab="display",zlab="volume",
              main="",pch=20,color=colors,type="p",
              cex.symbols = 0.3,label.tick.marks = FALSE,scale.y=0.7)
median_display = sort(cheese_new$DISP)[length(cheese_new$DISP)/2]
cheese_highDisp =cheese_new[which(cheese_new$DISP > median_display),]
cheese_lowDisp =cheese_new[which(cheese_new$DISP <= median_display),]
par(mfrow=c(1,2))
plot(cheese_highDisp$PRICE,log(cheese_highDisp$VOLUME),xlab="price",ylab="log volume",
     pch=16,col="blue",main="",cex=0.3)
plot(cheese_lowDisp$PRICE,log(cheese_lowDisp$VOLUME),xlab="price",ylab="log volume",
       pch=16,col="blue",main="",cex=0.3)
median_price = sort(cheese_new$PRICE)[length(cheese_new$PRICE)/2]
cheese_highPrice =cheese_new[which(cheese_new$PRICE > median_price),]
cheese_lowPrice =cheese_new[which(cheese_new$PRICE <= median_price),]
plot(log(cheese_highPrice$DISP),log(cheese_highPrice$VOLUME),xlab="log display",ylab="log volume",
     pch=16,col="blue",main="",cex=0.3)
plot(log(cheese_lowPrice$DISP),log(cheese_lowPrice$VOLUME),xlab="log display",ylab="log volume",
     pch=16,col="blue",main="",cex=0.3)

# Customer Heterogeneity
stores = levels(cheese_new$RETAILER)
birmingham = cheese_new[which(cheese_new$RETAILER==stores[8]),]
# n = 68
charlotte = cheese_new[which(cheese_new$RETAILER==stores[16]),]
# n= 61
chicago = cheese_new[which(cheese_new$RETAILER==stores[21]),]
# n =68
houston = cheese_new[which(cheese_new$RETAILER==stores[35]),]
# n = 61
jacksonville = cheese_new[which(cheese_new$RETAILER==stores[38]),]
# n = 61
losangeles = cheese_new[which(cheese_new$RETAILER==stores[43]),]
# n = 68
newyork = cheese_new[which(cheese_new$RETAILER==stores[55]),]
# n = 61
oklahoma = cheese_new[which(cheese_new$RETAILER==stores[56]),]
# n = 68
syracuse = cheese_new[which(cheese_new$RETAILER==stores[82]),]
# n = 61

plot(log(birmingham$PRICE),(log(birmingham$VOLUME)-mean(log(birmingham$VOLUME)))/sd(log(birmingham$VOLUME)),xlab="log price",ylab="log sales",
     pch=16,col="blue",main="Birmingham",cex=0.5)
plot(log(chicago$PRICE),(log(chicago$VOLUME)-mean(log(chicago$VOLUME)))/sd(log(chicago$VOLUME)),xlab="log price",ylab="log sales",
     pch=16,col="blue",main="Chicago",cex=0.5)
plot(log(jacksonville$PRICE),(log(jacksonville$VOLUME)-mean(log(jacksonville$VOLUME)))/sd(log(jacksonville$VOLUME)),xlab="log price",ylab="log sales",
     pch=16,col="blue",main="Jacksonville",cex=0.5)
plot(log(newyork$PRICE),(log(newyork$VOLUME)-mean(log(newyork$VOLUME)))/sd(log(newyork$VOLUME)),xlab="log price",ylab="log sales",
     pch=16,col="blue",main="NewYork",cex=0.5)
plot(log(oklahoma$PRICE),(log(oklahoma$VOLUME)-mean(log(oklahoma$VOLUME)))/sd(log(oklahoma$VOLUME)),xlab="log price",ylab="log sales",
     pch=16,col="blue",main="Oklahoma",cex=0.5)
plot(log(syracuse$PRICE),(log(syracuse$VOLUME)-mean(log(syracuse$VOLUME)))/sd(log(syracuse$VOLUME)),xlab="log price",ylab="log sales",
     pch=16,col="blue",main="Syracuse",cex=0.5)

plot(charlotte$PRICE,charlotte$VOLUME,xlab="price",ylab="volume",
     pch=16,col="blue",main="Charlotte",cex=0.5)
plot(chicago$PRICE,chicago$VOLUME/max(chicago$VOLUME),xlab="price",ylab="volume",
     pch=16,col="blue",main="Chicago",cex=0.5)
plot(houston$PRICE,houston$VOLUME,xlab="price",ylab="volume",
     pch=16,col="blue",main="Houston",cex=0.5)
plot(jacksonville$PRICE,jacksonville$VOLUME,xlab="price",ylab="volume",
     pch=16,col="blue",main="Jacksonville",cex=0.5)
plot(losangeles$PRICE,losangeles$VOLUME,xlab="price",ylab="volume",
     pch=16,col="blue",main="Los Angeles",cex=0.5)
plot(newyork$PRICE,newyork$VOLUME,xlab="price",ylab="volume",
     pch=16,col="blue",main="New York",cex=0.5)
plot(oklahoma$PRICE,oklahoma$VOLUME,xlab="price",ylab="volume",
     pch=16,col="blue",main="Oklahoma",cex=0.5)
plot(syracuse$PRICE,syracuse$VOLUME,xlab="price",ylab="volume",
     pch=16,col="blue",main="Syracuse",cex=0.5)

########################################

# Motivating Example: Tuna Data
# Load Data
library(bayesm)
data(tuna)

# Nonlinearity
# Plot total sales volume vs log price 

par(mar=c(5,4,4,4))

all_price = c(tuna$LPRICE1,tuna$LPRICE2,tuna$LPRICE3,
              tuna$LPRICE4,tuna$LPRICE5,tuna$LPRICE5,tuna$LPRICE7)
all_sales = c(tuna$MOVE1,tuna$MOVE2,tuna$MOVE3,
              tuna$MOVE4,tuna$MOVE5,tuna$MOVE5,tuna$MOVE6)
all_display = c(tuna$NSALE1,tuna$NSALE2,tuna$NSALE3,tuna$NSALE4,tuna$NSALE5,
                tuna$NSALE6,tuna$NSALE7)
brand = c(rep(1,length(tuna$MOVE1)),rep(2,length(tuna$MOVE2)),rep(3,length(tuna$MOVE3)),
          rep(4,length(tuna$MOVE4)),rep(5,length(tuna$MOVE5)),rep(6,length(tuna$MOVE6)),
                rep(7,length(tuna$MOVE7)))
colors = brand
ylim1 = min(log(all_sales))*0.95
ylim2 = max(log(all_sales))*1.05
plot(all_price,log(all_sales),xlab="log price",ylab="log sales",
     pch=16,col=colors,main="",cex=0.3, ylim=c(ylim1,ylim2))
plot(log(all_display),log(all_sales),xlab="log display",ylab="log sales",
     pch=16,col=colors,main="",cex=0.3,ylim=c(7,12))

# Measure Interaction Between Price & Display

price.no_display = all_price[which(all_display==0)]
price.display = all_price[which(all_display!=0)]
sales.no_display = all_sales[which(all_display==0)]
sales.display = all_sales[which(all_display!=0)]
colors.no_display = brand[which(all_display==0)]
colors.display = brand[which(all_display!=0)]

plot(price.no_display,log(sales.no_display),xlab="log price",ylab="log sales",
     pch=16,col=colors.no_display,main="Not Displayed",cex=0.3,ylim=c(ylim1,ylim2))
plot(price.display,log(sales.display),xlab="log price",ylab="log sales",
     pch=16,col=colors.display,main="Displayed",cex=0.3,ylim=c(ylim1,ylim2))

# Demographic Heterogeneity: 
# Bumblebee 6 oz (#4) vs Large Can (#6)
plot(tuna$NSALE4,log(tuna$MOVE4),xlab="log price",ylab="log sales",
     pch=16,col=4,main="6 oz",cex=0.3)
plot(tuna$NSALE6,log(tuna$MOVE6),xlab="log price",ylab="log sales",
     pch=16,col=6,main="Large Can",cex=0.3)





