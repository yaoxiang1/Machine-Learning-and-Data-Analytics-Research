rest_all<-read.csv(file="/Users/alan/Desktop/data_reference/menu_all.csv",head=TRUE,sep=",")	
rest_all_sample<-read.csv(file="/Users/alan/Desktop/data_reference/rest_all_sample.csv",head=TRUE,sep=",")	

for(locationid in rest_all_sample$locationid){
	price_cut_01_9(locationid)			
}
	
price_cut_01_9<-function(locationid){
		range <- c(seq(4,9,0.1))
	
		Fn <- ecdf(rest_all["price"][rest_all["locationid"]==locationid])
		cdf_frame <- as.data.frame(Fn(range)-Fn(range-0.1))	
		cdf_frame["price"] <- range
		names(cdf_frame)[1] <- "probability"
		cdf_frame<-subset(cdf_frame, select=c("price", "probability"))
		#print(cdf_frame)
		max_value <- (max(cdf_frame$probability))
		max_index <- (cdf_frame[cdf_frame$probability == max_value,1])
		#get the min value of the locationid izn case there are duplicate probability
		max_index <- min(max_index)
		print(c(locationid,max_index-0.1))

		plot(cdf_frame)	
		abline(v=seq(4,11,0.1), col="grey", lty=3)
		abline(v=seq(4,11,0.5), col="blue", lty=3)
	}



# write algo, compare does to the 40 I have looked at and find if it performs well
# if it perform well, I can apply to the whole thing	
# 0.1 cents can be too small, can change to 0.5 in the future
#########################################################################################	
# (CDF)
# Fn(t) = (# of sample values =< t) / n
# Fn is a function, Fn(a), return the probablity with the value <= a
# The price plot is a certain price related to a percentage, ex. Fn(10) = 0.45
# Give a certain price, show a percentage, there are price jump
#	
# 	|            .--- 
# 	|       .----
# 	|    .--
# 	| .--
# 	----------------- 
#
#
	 		
rest_398<-read.csv(file="/Users/alan/Desktop/menu_398.csv",head=TRUE,sep=",")

Fn <- ecdf(rest_398$price)
Fn(3)
Fn(rest_338$price)
Fn(c(seq(1,10,0.1)))
Fn(c(seq(1,10,0.1)))-Fn(c(seq(1.1,10.1,0.1)))
Fn(c(seq(4,12,0.1)))-Fn(c(seq(4.1,12.1,0.1)))

####################################################################################
# Calculate the probablity difference from 4.0 - 3.9, 4.1 - 4.0, 4.2 - 4.1 ......
#  
# 0.1
rest_398<-read.csv(file="/Users/alan/Desktop/menu_398.csv",head=TRUE,sep=",")	
Fn <- ecdf(rest_398$price)
		
cdf_frame <- as.data.frame(Fn(c(seq(4,11,0.1)))-Fn(c(seq(3.9,10.9,0.1))))	
cdf_frame["price"] <- c(seq(4,11,0.1))
names(cdf_frame)[1] <- "probability"
cdf_frame<-subset(cdf_frame, select=c("price", "probability"))
cdf_frame
plot(cdf_frame)	
abline(v=0:50, col="gray", lty=3)


		
plot(Fn)
abline(v=0:50, col="gray", lty=3)
	
####################################################################################
# Calculate the probablity difference from 4.0 - 3.9, 4.1 - 4.0, 4.2 - 4.1 ......  
# 0.1	
	rest_all<-read.csv(file="/Users/alan/Desktop/data_reference/menu_all.csv",head=TRUE,sep=",")	
	rest_train<-read.csv(file="/Users/alan/Desktop/data_reference/rest_train.csv",head=TRUE,sep=",")	
	rest_all_sample<-read.csv(file="/Users/alan/Desktop/data_reference/rest_all_sample.csv",head=TRUE,sep=",")	

	rest_train$locationid
	rest_all_sample$locationid

rest_all<-read.csv(file="/Users/alan/Desktop/data_reference/menu_all.csv",head=TRUE,sep=",")	
rest_train<-read.csv(file="/Users/alan/Desktop/data_reference/rest_train.csv",head=TRUE,sep=",")	
rest_all_sample<-read.csv(file="/Users/alan/Desktop/data_reference/rest_all_sample.csv",head=TRUE,sep=",")	
			
locationid <- 276
price_cut(locationid)			

locationid <- 276
price_cut_graph(locationid)			
	
				
for(locationid in rest_train$locationid){
	price_cut(locationid)			
}

################################################################################
### CDF graphing
rest_all<-read.csv(file="/Users/alan/Desktop/data_reference/menu_all.csv",head=TRUE,sep=",")			
Fn <- ecdf(rest_all["price"][rest_all["locationid"]==130])
plot(Fn)
abline(v=seq(0,50,1), col="blue", lty=3)

################################################################################	
				
price_cut_graph<-function(locationid){
		range <- c(seq(5,10,0.1))
			
		Fn <- ecdf(rest_all["price"][rest_all["locationid"]==locationid])
		cdf_frame <- as.data.frame(Fn(range)-Fn(range-0.1))	
		cdf_frame["price"] <- range
		names(cdf_frame)[1] <- "probability"
		cdf_frame<-subset(cdf_frame, select=c("price", "probability"))
		#print(cdf_frame)
		max_value <- (max(cdf_frame$probability))
		max_index <- (cdf_frame[cdf_frame$probability == max_value,1])
		#get the min value of the locationid in case there are duplicate probability
		max_index <- min(max_index)
		print(c(locationid,max_index))
		
		plot(cdf_frame)	
		abline(v=seq(4,11,0.1), col="grey", lty=3)
		abline(v=seq(4,11,0.5), col="blue", lty=3)
	}


price_cut<-function(locationid){
		range <- c(seq(5,10,0.1))
			
		Fn <- ecdf(rest_all["price"][rest_all["locationid"]==locationid])
		cdf_frame <- as.data.frame(Fn(range)-Fn(range-0.1))	
		cdf_frame["price"] <- range
		names(cdf_frame)[1] <- "probability"
		cdf_frame<-subset(cdf_frame, select=c("price", "probability"))
		#print(cdf_frame)
		max_value <- (max(cdf_frame$probability))
		max_index <- (cdf_frame[cdf_frame$probability == max_value,1])
		#get the min value of the locationid in case there are duplicate probability
		max_index <- min(max_index)
		print(c(locationid,max_index))
		
		#plot(cdf_frame)	
		#abline(v=seq(4,11,0.1), col="grey", lty=3)
		#abline(v=seq(4,11,0.5), col="blue", lty=3)
	}
	
for(locationid in rest_train$locationid){
	price_cut_05(locationid)			
}	

price_cut_05<-function(locationid){
		range <- c(seq(5,10,0.5))
		
		Fn <- ecdf(rest_all["price"][rest_all["locationid"]==locationid])
		cdf_frame <- as.data.frame(Fn(range)-Fn(range-0.1))	
		cdf_frame["price"] <- range
		names(cdf_frame)[1] <- "probability"
		cdf_frame<-subset(cdf_frame, select=c("price", "probability"))
		#print(cdf_frame)
		max_value <- (max(cdf_frame$probability))
		max_index <- (cdf_frame[cdf_frame$probability == max_value,1])
		#get the min value of the locationid in case there are duplicate probability
		max_index <- min(max_index)
		print(c(locationid,max_index))
	
		#plot(cdf_frame)	
		#abline(v=seq(4,11,0.1), col="grey", lty=3)
		#abline(v=seq(4,11,0.5), col="blue", lty=3)
	}

####################################################################
# so far performing the best
####################################################################
	 
for(locationid in rest_train$locationid){
	price_cut_01_9(locationid)			
}

####################################################################
####################################################################



