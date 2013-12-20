	// upload csv file
	->  read.csv(file="/Users/alan/Desktop/K-mean.csv",head=TRUE,sep=",")
	
	//  only pick couples of fields after inserting the data
	// add avg dollar, performance ratio decrease
	cluster_menu_1<-cluster_menu_csv[c("Trimmed","IncomeScore","deli_rate","credit_rate","avgdollar","discount_percent")]
	
	cluster_menu_2<-cluster_menu_csv[c("deli_rate","credit_rate","avgdollar")]
	
	
	//////////////////////////////////////////////////////////////////////////////////////////////
	// run this block of code together	
	cluster_menu_3<-cluster_menu_csv[c("deli_rate","credit_rate","IncomeScore")];
		
	# calculate the average for 10 output for a certain number
	cal_average<-function(numb_cluster){
		total <- 0;
		for(i in 1:10){
			total<-total+(round(1-(kmeans(cluster_menu_3,numb_cluster,iter.max=50)$tot.withinss)/(kmeans(cluster_menu_3,numb_cluster,iter.max=50)$totss),4))
		};
		print(total/10);
	}
	
	#// use this to call the function after it is created
	# cal_average(9)
	
	#// use this to have the output from 1 to 50	
	#// can do this directly since I have write other function in it already
	for(i in 1:50){
		cal_average(i);
			
	}
	
	
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////rs	
	// run the most basic kmeans method
	// get the kmean value for the $size of clusters
	
	// 1. the size for each clusters
	
	1. Place K points into the space represented by the objects that are being 
	clustered. These points represent initial group centroids.
	2. Assign each object to the group that has the closest centroid.
	When all objects have been assigned, recalculate the positions of the 
	K centroids.
	3. Repeat Steps 2 and 3 until the centroids no longer move. 
	4. This produces a separation of the objects into groups from which the metric to be minimized 
	can be calculated.
	
	
	kmeans(cluster_menu,6,iter.max=50)$size
	// run this 10 times to see whether the size is stable	
	for(i in 1:10){
		print(sort(kmeans(cluster_menu,6,iter.max=50)$size))
	}
	
	// 2. between_SS/total_SS Analysis	
	round(1-(kmeans(cluster_menu,6,iter.max=50)$tot.withinss)/(kmeans(cluster_menu,6,iter.max=50)$totss),4)
	
	for(i in 1:10){
		print(round(1-(kmeans(cluster_menu,6,iter.max=50)$tot.withinss)/(kmeans(cluster_menu,6,iter.max=50)$totss),4))
	};
	// 2. the centers for each clusters
	kmeans(cluster_menu,6,iter.max=50)$centers
		
	for(i in 1:10){
		kmeans(cluster_menu,6,iter.max=50)$centers;
	};
	
	// 3. assign the the center of different variale to a matrix_sort matrix, 
	matrix_sort<-kmeans(cluster_menu,6,iter.max=50)$centers
		
        Trimmed IncomeScore  deli_rate credit_rate    avgdollar
  1 -0.09664970   0.3649036 -1.4228170  1.33238911 -0.082785505
  2 -0.04875201   1.4724330  0.1283837  0.36867493  0.262549816
  3 -0.16750730   0.1582254  0.6212726 -2.05726578 -0.007339126
  4 -0.29879454  -0.4086578  0.5087013 -0.06502821 -0.257972749
  5  1.23774765  -0.3035953  0.7629427  0.44364938  0.540684387
  6 -0.83124081  -0.4580246 -1.4484386 -0.95090874 -0.365902945
			
	// need to sort by Trimmed, to line up the mean for one variable (all other variables follow ) in order to line up the clusters		
	
	matrix_sort[order(matrix_sort[,1]),] 
	// have a for loop to do it 10 times to see if it has persistant behavior
	for(i in 1:10){
		matrix_sort<-kmeans(cluster_menu,6,iter.max=50)$centers;
		print(matrix_sort[order(matrix_sort[,3]),]); 		
		print(matrix_sort[order(matrix_sort[,3]),][,1]); 
	};
	
	// 4. cluster, list what each points belong to
	matrix_sort<-kmeans(cluster_menu,6,iter.max=50)$cluster
		
		
	// 5. Rand Index: 
	ex. A: 1 2 1 2 2   for each index pair in each group R=(a+d)/(n) 
		B: 1 2 2 1 2											  2
						sa cluster A    dif cluster A
			sa cluster A     a               b			a,d numb agree
		   dif cluster B     c				 d			c,b numb disaggree
				 
	The Adjusted Rand Index can yield a value between -1 and +1
	
	adjustedRandIndex(x, y)
	x	 A numeric or character vector of class labels.
	y	 A numeric or character vector of class labels. 
		 The length of y should be the same as that of x.
	
	// use adjustedRandIndex to compare the different clusters 
	cluster_menu_csv<-read.csv(file="/Users/alan/Desktop/K-mean.csv",head=TRUE,sep=",")
	
	cluster_menu_1<-cluster_menu_csv[c("Trimmed","IncomeScore","deli_rate","credit_rate","avgdollar","discount_percent")];
	cluster_menu_2<-cluster_menu_csv[c("deli_rate","credit_rate","avgdollar")];
	cluster_menu_3<-cluster_menu_csv[c("deli_rate","credit_rate","Trimmed")];
	
	
	cluster_cat_1<-kmeans(cluster_menu_1,6,iter.max=50)$cluster;
	cluster_cat_2<-kmeans(cluster_menu_2,6,iter.max=50)$cluster;
	cluster_cat_3<-kmeans(cluster_menu_2,6,iter.max=50)$cluster;
	
	//as.data.frame(cluster_rest);
	adjustedRandIndex(cluster_cat_1, cluster_cat_2)
	adjustedRandIndex(cluster_cat_2, cluster_cat_3)
	
	// do the ranking for size first to see if the size ranking behave stablely	
	sort(kmeans(cluster_menu_3,6,iter.max=50)$size)
	
	for(i in 1:10){
		print(sort(kmeans(cluster_menu_1,6,iter.max=50)$size))
	}	
			
	/////////////////////////////////////////////
	// algorithm
	15 49 58 25 72 48
  -> 6  3  2  5  1  4 (rank)
  -> 1  2  3  4  5  6 (output)
  
	rank(kmeans(cluster_menu_1,6,iter.max=50)$size);
	1 4 2 6 3 5
			
	1. first I will cal the rank vector of the size vector
	2. need to use the rank vector to update the output value
		rank_vector[output_value]
	3. need to update to each previous Clustering vector 
	 	test[1]<-3
	
//////////////////////////////////////////////////////////////////////////////////
// Implimentation 
	size_trans<-function(before_cluster){	
		#current_kmean <- kmeans(before_cluster,before_cluster[c(10,21,56,70,83,123),],iter.max=50);
			
			current_kmean <- kmeans(before_cluster,6,iter.max=50);
			current_cluster <- current_kmean[[1]];	
			#rank_vector <- rank(current_kmean$size);	
			#print(rank_vector);
			#print("Transform Before:...................................................");
			#print(current_cluster);
		
			#for(i in 1:length(current_cluster)){
			#	current_cluster[i] <- rank_vector[current_cluster[i]]
			#}
	
			#print("Transform After:....................................................");
			#after_cluster <- current_cluster;
		
			return (current_cluster);
		}	
		
		size_trans(cluster_menu_1);
		
		cluster_menu_1<-cluster_menu_csv[c("deli_rate","credit_rate","avgdollar")];
		cluster_menu_2<-cluster_menu_csv[c("deli_rate","credit_rate","Trimmed_10_average_menu")];
		cluster_menu_3<-cluster_menu_csv[c("deli_rate","credit_rate","IncomeScore")];
		cluster_menu_4<-cluster_menu_csv[c("deli_rate","credit_rate","total_discount")];
		cluster_menu_5<-cluster_menu_csv[c("Trimmed_10_average_menu","IncomeScore","deli_rate","credit_rate","avgdollar")];	
		cluster_menu_6<-cluster_menu_csv[c("Trimmed_10_average_menu","IncomeScore","deli_rate","credit_rate","avgdollar","total_discount")];
		
		cluster_menu_7<-cluster_menu_csv[c("Trimmed_10_average_menu","deli_rate","credit_rate","avgdollar")];
		cluster_menu_8<-cluster_menu_csv[c("Trimmed_10_average_menu","deli_rate","credit_rate","avgdollar","total_discount")];
		
		cluster_menu_9<-cluster_menu_csv[c("trimmed_income","deli_rate","credit_rate","avgdollar","total_discount")];
		cluster_menu_10<-cluster_menu_csv[c("trimmed_income","deli_rate","credit_rate","avgdollar")];
		
		
		
		for(i in 1:500){
			print(adjustedRandIndex(size_trans(cluster_menu_9), size_trans(cluster_menu_9)));	
		}
		
		> plot(hclust(dist(cluster_menu_10[,c(1:3)])))
		> plot(hclust(dist(cluster_menu_10[,c(1:3)]),method="ward"))
		> cluster_menu_10[72,]
		
		
		
		
		
		
		
	////////////////////////////////////////////////////////////////////////////////////
	// plot n by n graph in two demision, the parameter is a frame
	plot(cluster_menu)	
			 Trimmed  IncomeScore    deli_rate  credit_rate    avgdollar
	1    1.860803243 -0.187126645  0.957423733  0.335881823 -0.151548628
	2    1.846585762  0.193128333  1.125057483  0.338974836 -0.120731757
	
	// clusplot.default()	
	// bivariate and multivariate graphing for the cluster analysis		 
	clusplot(cluster_menu_5[c("deli_rate","credit_rate")],kmeans(cluster_menu_1,6,iter.max=50)$cluster)
	 
	plot(cluster_menu_5,col=c("red","blue","green","black","purple","brown")[kmeans(cluster_menu_5,6,iter.max=50)$cluster])
	
	/////////////////////////////////////////////////////////////////////////// save the data and plot the data with the matching between color and // data clusters	
	save_color<-c("red","blue","green","black","purple","brown")[kmeans(cluster_menu_5,6,iter.max=50)$cluster]
	save the color of the kmeans with color put it into save_color
	
	as.data.frame(save_color)
	put into data frame and export and clean the data in excel
		
	plot(cluster_menu_5,col=save_color) 
		
	////////////////////////////////////////////////////////////////////////
	// PCA in R
	princomp(x, ...)
	
	
	/////////////////////////////////////////////////////////////////////////////
	// hierarchical clustering: hclust
	/////////////////////////////////////////////////////////////////////////////
	
	1. trimmed/income
	2. delivery rate
	3. credit rate
	4. percent orders, 5,6,7
	5. percent before 3
	6. percent discount
		
		
	method: the agglomeration method to be used.
			‘"ward"’, ‘"single"’,‘"complete"’, ‘"average"’, ‘"mcquitty"’, ‘"median"’ or ‘"centroid"’.
	
	1. hclust(dist(cluster_menu_10[-72,c(1:3)],method="manhattan"),method="single")
	
	2. // directly plot the result
		plot(hclust(dist(cluster_menu_10[-72,c(1:3)],method="manhattan"),method="ward"))
	
	3. // need to input a distance matrix computed by
	   // input frame, method -> distance matrix 
		dist(cluster_menu_10[-72,c(1:3)],method="manhattan")
		method: ‘"euclidean"’, ‘"maximum"’, ‘"manhattan"’, ‘"canberra"’,‘"binary"’ or ‘"minkowski"’
	
	4. // if wanting to remove a certain data tuple in the dataset, can use -72 to remove a certain data 
		cluster_menu_10[-72,c(1:3)]
	/////////////////////////////////////////////////////////////////////////////////////////////
	// inport data into new_data
	new_data: the original data inport from csv
	1: locationid 
	2: trimmed.income    
	3: deli_rate  
	4: credit_rate 
	5: total.discount
	6: total_before_3    
	7: total_567
	
	new_data[c(2:7)]
	plot(hclust(dist(new_data[-72,c(2:7)],method="euclidean"),method="ward"))
	plot(hclust(dist(new_data[-72,c(2:7)],method="manhattan"),method="ward"))
	plot(hclust(dist(new_data[-72,c(2:7)],method="maximum"),method="ward"))
		
	// BAD: complete, centroid, average, single
	// constraint the number of cut
	hcluster<-cutree(hclust(dist(new_data[-72,c(2:7)],method="euclidean"),method="ward"),6)
	// constraint the number of height of the tree
	// At least one of k: ‘number of clusters’ or h: ‘distance’ must be specified, ‘k’ overrides ‘h’ if
	// both are given.
	cutree(hclust(dist(new_data[-72,c(2:7)],method="euclidean"),method="ward"),,20)
	
	hcluster<-as.vector(cutree(hclust(dist(new_data[-72,c(2:7)],method="euclidean"),method="ward"),5));
	// used to plot the graph
	plot(new_data[-72,c(2:7)],col=c("red","blue","green","black","purple")[hcluster])
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Validating cluster solutions: The function cluster.stats() 	
	/////////////////////////////////////////////////////////////////////////////////////////////////////////
		
	# comparing 2 cluster solutions
	ex.
	library(fpc)
	//////////////////////////////////////////////////////////
	// usage 1: can be used to compare two matrix
	cluster.stats(d, fit1$cluster, fit2$cluster)
	
	//////////////////////////////////////////////////////////
	// usage 1: can be used to compare two matrix	   
	
	cluster.stats(dist(input_data,method="euclidean"), hcluster)
	
	
	cluster_menu_csv<-read.csv(file="/Users/alan/data_reference/New_Data_Analysis.csv",head=TRUE,sep=",")
	
	//whole data	
	for (i in 2:20 ) {
		input_data <- new_data[-72,c(2:7)];
		total_ss<-kmeans(input_data,6,iter.max=50)[[3]]
		hcluster<-cutree(hclust(dist(input_data,method="euclidean"),method="ward"),i)	
		print(1- cluster.stats(dist(input_data,method="euclidean"), hcluster)$within.cluster.ss/total_ss)
	}
	
	//no weekend
	for (i in 2:20 ) {
		input_data <- new_data[-72,c(2:6)];
		total_ss<-kmeans(input_data,6,iter.max=50)[[3]]
		hcluster<-cutree(hclust(dist(input_data,method="euclidean"),method="ward"),i)		
		print (1- cluster.stats(dist(input_data,method="euclidean"), hcluster)$within.cluster.ss/total_ss)
	}	
	
	//no discount
	for (i in 2:20 ) {
		input_data <- new_data[-72,c(2,3,4,6,7)];
		total_ss<-kmeans(input_data,6,iter.max=50)[[3]]
		hcluster<-cutree(hclust(dist(input_data,method="euclidean"),method="ward"),i)	
		print (1- cluster.stats(dist(input_data,method="euclidean"), hcluster)$within.cluster.ss/total_ss)
	}
	
	//no discount_weekend
	for (i in 2:20 ) {
		input_data <- new_data[-72,c(2,3,4,6)];
		total_ss<-kmeans(input_data,6,iter.max=50)[[3]]
		hcluster<-cutree(hclust(dist(input_data,method="euclidean"),method="ward"),i)	
		print (1- cluster.stats(dist(input_data,method="euclidean"), hcluster)$within.cluster.ss/total_ss)
	}
	// whole data	0.6846685
	// no weekend	0.6550371
	// no discount	0.6660963
	// no discount_weekend	0.621989
	
	// expord the data to the excel
	 as.data.frame(cutree(hclust(dist(new_data[-72,c(2:7)],method="euclidean"),method="ward"),5))
		
	 cluster_data <- as.data.frame(cutree(hclust(dist(new_data[-72,c(2:7)],method="euclidean"),method="ward"),5))
	 new_data[-72,c(1)]
	 cbind ( cluster_data[1] , as.data.frame(new_data[-72,c(1)]))
		 
	///////////////////////////////////////////////////////////////////////////////////
 	///////////////////////////////////////////////////////////////////////////////////
	//whole data
	input_data1 <- new_data[-72,c(2:7)];
	//no weekend	 
	input_data2 <- new_data[-72,c(2:6)];
	//no discount
	input_data3 <- new_data[-72,c(2,3,4,6,7)];
	//no discount_weekend
	input_data4 <- new_data[-72,c(2,3,4,6)];
		 
	data1<-cutree(hclust(dist(input_data1,method="euclidean"),method="ward"),5)	 
	data2<-cutree(hclust(dist(input_data2,method="euclidean"),method="ward"),5)	 
	data3<-cutree(hclust(dist(input_data3,method="euclidean"),method="ward"),5)	 
	data4<-cutree(hclust(dist(input_data4,method="euclidean"),method="ward"),5)	 
	
	require(mclust)
		> adjustedRandIndex(data1, data2) 
		[1] 0.4836663
		> adjustedRandIndex(data1, data3) 
		[1] 0.4518242
		> adjustedRandIndex(data1, data4) 
		[1] 0.6922352
		> 
		> adjustedRandIndex(data2, data3) 
		[1] 0.5600262
		> adjustedRandIndex(data2, data4) 
		[1] 0.5656652
		> 
		> adjustedRandIndex(data3, data4) 
		[1] 0.4036362
	
	
	
	//////////////////////////////////////////////////////////////////////////////
	// for cluster 1: plot the histogram for a certain field - total
		orderid	                    1
		locationid					2				
		customerid_0_visitor		3
		firstname					4
		subtotal					5
		discount					6
		deli_charge					7
		gratuity					8
		tax							9
		total						10
		zip							11	
		dayofweek					12
		dayofmonth					13
		dayofyear					14
		hourofday					15
		monthofyear					16
		deli_1_takeout_0			17
		credit_1_cash_0	clusters	18
		cred_cash_deli_take			19	
		is_discount					20
		total_floor					21
		subtotal_floor				22
			
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster3.csv",head=TRUE,sep=",")
	
	////////////////////////////////////////////////////////
	// filter data and plot from 0 to 50
		
	kernel function, 0.5 bw, work with total value	
	individual orders > 5
	
	##################################################################################################
	# histogram: hist() 	
	# total 		
	# hist(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 ] , breaks=seq(0,50,0.5)) 
	# text(locator(1), "total", 1)		
	# grid(50, NA, lwd = 2)
	##################################################################################################
	
	
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster2.csv",head=TRUE,sep=",")
	bandwith<-0.5
	graph_individual_novisitor_before3_weekday1234(bandwith)
	
	##################################################################################################
	
	bandwith<-0.5
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster1.csv",head=TRUE,sep=",")
	graph_individual_novisitor_before3_weekday1234_cash_takeout_deli_credit(bandwith)
	
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster2.csv",head=TRUE,sep=",")
	graph_individual_novisitor_before3_weekday1234_cash_takeout_deli_credit(bandwith)
	
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster3.csv",head=TRUE,sep=",")	
	graph_individual_novisitor_before3_weekday1234_cash_takeout_deli_credit(bandwith)
	
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster4.csv",head=TRUE,sep=",")	
	graph_individual_novisitor_before3_weekday1234_cash_takeout_deli_credit(bandwith)
	
	bandwith<-0.8
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster5.csv",head=TRUE,sep=",")	
	graph_individual_novisitor_before3_weekday1234_cash_takeout_deli_credit(bandwith)
	
	graph_individual_novisitor_before3_weekday1234_cash_takeout_deli_credit<-function(bandwith){
		###################################################################
		# delivery_credit			
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
					         cluster1["deli_1_takeout_0"]==1 & 
		   					 cluster1["credit_1_cash_0"]==1 & 
							 cluster1["numb_item_7"] == 0 & 
							 cluster1["customerid_0_visitor"]!=0 &
			  				 cluster1["dayofweek"] < 5 &  
							 cluster1["hourofday"] < 15]							 
		# bandwith <- bw.nrd0(data) 
		# print(bandwith)
		plot(density(data,bw=bandwith,kernel="epanechnikov"),col='blue',ylim=c(0,0.2))
	
		###################################################################
		abline(v=0:50, col="gray", lty=3)		
		legend("topright", c("delivery_credit","takeout_cash","discount"), 
					  fill=c("blue",             "green",      "brown"))
		###################################################################
		# takeout_cash	
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
						  cluster1["deli_1_takeout_0"]==0 & 
						  cluster1["numb_item_7"] == 0 & 
					  	  cluster1["credit_1_cash_0"]==0 & 
						  cluster1["customerid_0_visitor"]!=0 & 
						  cluster1["dayofweek"] < 5 &  
						  cluster1["hourofday"] < 15]				  
		# bandwith <- bw.nrd0(data)				  	
		# print(bandwith)
		lines(density(data,bw=bandwith,kernel="epanechnikov"),col='green')  

		##############################################################################																
		# with discount
  		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
					  cluster1["is_discount"]==1 & 
					  cluster1["numb_item_7"] == 0 & 
					  cluster1["customerid_0_visitor"]!=0 & 
			  		  cluster1["dayofweek"]  < 5 & 
					  cluster1["hourofday"] < 15]
		# bandwith <- bw.nrd0(data)
  		# print(bandwith)	
		lines(density(data, bw=bandwith,kernel="epanechnikov"),col='brown')  	 
			
			
	}	
	
	
	bandwith<-0.5
	
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster1.csv",head=TRUE,sep=",")
	graph_individual_novisitor_after3_weekday567(bandwith)
	
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster2.csv",head=TRUE,sep=",")
	graph_individual_novisitor_after3_weekday567(bandwith)
	
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster3.csv",head=TRUE,sep=",")
	graph_individual_novisitor_after3_weekday567(bandwith)
	
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster4.csv",head=TRUE,sep=",")
	graph_individual_novisitor_after3_weekday567(bandwith)
	
	#########################################################
	# no data returned from this one 
	bandwith<-0.8	
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster5.csv",head=TRUE,sep=",")
	graph_individual_novisitor_after3_weekday567(bandwith)

	
	graph_individual_novisitor_after3_weekday567<-function(bandwith){
		###################################################################
		# delivery			
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
					         cluster1["deli_1_takeout_0"]==1 & 
							 cluster1["numb_item_7"] == 0 & 
							 cluster1["customerid_0_visitor"]!=0 &
			  				 cluster1["dayofweek"] > 4 &  
							 cluster1["hourofday"] > 15]							 
		 
		plot(density(data,bw=bandwith,kernel="epanechnikov"),col='blue',ylim=c(0,0.2))
	
		###################################################################
		abline(v=0:50, col="gray", lty=3)		
		legend("topright", c("delivery","takeout","credit","cash","discount"), 
					  fill=c("blue",    "green", "orange","purple","brown"))
		###################################################################
		# takeout	
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
						  cluster1["deli_1_takeout_0"]==0 & 
						  cluster1["numb_item_7"] == 0 & 
						  cluster1["customerid_0_visitor"]!=0 & 
						  cluster1["dayofweek"] > 4 &  
						  cluster1["hourofday"] > 15]				  
						  	
		lines(density(data,bw=bandwith,kernel="epanechnikov"),col='green')  

		##############################################################################											
		# credit
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
					  cluster1["credit_1_cash_0"]==1 & 
					  cluster1["numb_item_7"] == 0 & 
					  cluster1["customerid_0_visitor"]!=0 & 						  
					  cluster1["dayofweek"]  > 4 & 
					  cluster1["hourofday"]>15]	
		
		lines(density(data, bw=bandwith,kernel="epanechnikov"),col='orange')  
		
												  	    
		##############################################################################																
		# cash	
  		data <- cluster1["total"][cluster1["total"]<50 & 
					  cluster1["total"]>0 & 
					  cluster1["credit_1_cash_0"]==0 & 
					  cluster1["numb_item_7"] == 0 & 
					  cluster1["customerid_0_visitor"]!=0 & 						  
					  cluster1["dayofweek"]  > 4 &  
					  cluster1["hourofday"]>15]
  		
		lines(density(data,bw=bandwith,kernel="epanechnikov"),col='purple')  

		##############################################################################																
		# with discount
  		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
					  cluster1["is_discount"]==1 & 
					  cluster1["numb_item_7"] == 0 & 
					  cluster1["customerid_0_visitor"]!=0 & 
			  		  cluster1["dayofweek"]  > 4 & 
					  cluster1["hourofday"]>15]
		
		lines(density(data, bw=bandwith,kernel="epanechnikov"),col='brown')  	 
			
			
	}	
	
	
	
		
		
		bandwith<-0.5
		
		cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster1.csv",head=TRUE,sep=",")
		graph_individual_novisitor_before3_weekday1234(bandwith)
		
		cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster2.csv",head=TRUE,sep=",")
		graph_individual_novisitor_before3_weekday1234(bandwith)
		
		cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster3.csv",head=TRUE,sep=",")
		graph_individual_novisitor_before3_weekday1234(bandwith)
		
		cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster4.csv",head=TRUE,sep=",")
		graph_individual_novisitor_before3_weekday1234(bandwith)
		
		bandwith<-0.8	
		cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster5.csv",head=TRUE,sep=",")
		graph_individual_novisitor_before3_weekday1234(bandwith)
	
		
	graph_individual_novisitor_before3_weekday1234<-function(bandwith){
		###################################################################
		# delivery			
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
					         cluster1["deli_1_takeout_0"]==1 & 
							 cluster1["numb_item_7"] == 0 & 
							 cluster1["customerid_0_visitor"]!=0 &
			  				 cluster1["dayofweek"] < 5 &  
							 cluster1["hourofday"] < 15]							 
		# bandwith <- bw.nrd0(data) 
		# print(bandwith)
		plot(density(data,bw=bandwith,kernel="epanechnikov"),col='blue',ylim=c(0,0.2))
	
		###################################################################
		abline(v=0:50, col="gray", lty=3)		
		legend("topright", c("delivery","takeout","credit","cash","discount"), 
					  fill=c("blue",    "green", "orange","purple","brown"))
		###################################################################
		# takeout	
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
						  cluster1["deli_1_takeout_0"]==0 & 
						  cluster1["numb_item_7"] == 0 & 
						  cluster1["customerid_0_visitor"]!=0 & 
						  cluster1["dayofweek"] < 5 &  
						  cluster1["hourofday"] < 15]				  
		# bandwith <- bw.nrd0(data)				  	
		# print(bandwith)  
		lines(density(data,bw=bandwith,kernel="epanechnikov"),col='green')  

		##############################################################################											
		# credit
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
					  cluster1["credit_1_cash_0"]==1 & 
					  cluster1["numb_item_7"] == 0 & 
					  cluster1["customerid_0_visitor"]!=0 & 						  
					  cluster1["dayofweek"]  < 5 & 
					  cluster1["hourofday"] < 15]	
		# bandwith <- bw.nrd0(data)
 		# print(bandwith)		
		lines(density(data, bw=bandwith,kernel="epanechnikov"),col='orange')  
		
												  	    
		##############################################################################																
		# cash	
  		data <- cluster1["total"][cluster1["total"]<50 & 
					  cluster1["total"]>0 & 
					  cluster1["credit_1_cash_0"]==0 & 
					  cluster1["numb_item_7"] == 0 & 
					  cluster1["customerid_0_visitor"]!=0 & 						  
					  cluster1["dayofweek"]  < 5 &  
					  cluster1["hourofday"] < 15]
  		# bandwith <- bw.nrd0(data)
	    # print(bandwith)
		lines(density(data,bw=bandwith,kernel="epanechnikov"),col='purple')  

		##############################################################################																
		# with discount
  		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
					  cluster1["is_discount"]==1 & 
					  cluster1["numb_item_7"] == 0 & 
					  cluster1["customerid_0_visitor"]!=0 & 
			  		  cluster1["dayofweek"]  < 5 & 
					  cluster1["hourofday"] < 15]
		# bandwith <- bw.nrd0(data)
  		# print(bandwith)	
		lines(density(data, bw=bandwith,kernel="epanechnikov"),col='brown')  	 
			
			
	}	
	
	
	
	bandwith<-0.5
		
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster1.csv",head=TRUE,sep=",")
	graph_individual_novisitor(bandwith)
		
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster2.csv",head=TRUE,sep=",")
	graph_individual_novisitor(bandwith)
		
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster3.csv",head=TRUE,sep=",")
	graph_individual_novisitor(bandwith)
		
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster4.csv",head=TRUE,sep=",")
	graph_individual_novisitor(bandwith)
		
	bandwith<-0.8	
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster5.csv",head=TRUE,sep=",")
	graph_individual_novisitor(bandwith)
	
	
	graph_individual_novisitor<-function(bandwith){
			# delivery 
			plot(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["deli_1_takeout_0"]==1 & cluster1["numb_item_7"] == 0 & cluster1["customerid_0_visitor"]!=0], 
			bw=bandwith,kernel="epanechnikov"),col='blue',ylim=c(0,0.2));  
		
			###################################################################
			abline(v=0:50, col="gray", lty=3)		
			legend("topright", c("delivery","takeout","credit","cash","discount","after_3","before_3", "weekday",    "weekend"), 
						  fill=c("blue",    "green", "orange","purple","brown", "grey" ,  "cyan",    "darkorchid4", "burlywood4"))
			###################################################################
			# takeout		
			lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["deli_1_takeout_0"]==0 & cluster1["numb_item_7"] == 0 & cluster1["customerid_0_visitor"]!=0], 
						  bw=bandwith,kernel="epanechnikov"),col='green')  
	
			# credit	
			lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["credit_1_cash_0"]==1 & cluster1["numb_item_7"] == 0 & cluster1["customerid_0_visitor"]!=0], 
						  bw=bandwith,kernel="epanechnikov"),col='orange')  
			# cash
			lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["credit_1_cash_0"]==0 & cluster1["numb_item_7"] == 0 & cluster1["customerid_0_visitor"]!=0], 
						  bw=bandwith,kernel="epanechnikov"),col='purple')  
	
			# with discount
			lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["is_discount"]==1 & cluster1["numb_item_7"] == 0 & cluster1["customerid_0_visitor"]!=0], 
						  bw=bandwith,kernel="epanechnikov"),col='brown')  
	
			# after_3
			lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["hourofday"]>15 & cluster1["numb_item_7"] == 0 & cluster1["customerid_0_visitor"]!=0], 
						  bw=bandwith,kernel="epanechnikov"),col='grey')  	
			# before_3
			lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["hourofday"]<15 & cluster1["numb_item_7"] == 0 & cluster1["customerid_0_visitor"]!=0], 
						  bw=bandwith,kernel="epanechnikov"),col='cyan')  

			# weekday (4,5,6)
			lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["dayofweek"] > 4 & cluster1["numb_item_7"] == 0 & cluster1["customerid_0_visitor"]!=0], 
						  bw=bandwith,kernel="epanechnikov"),col='darkorchid4')  
			# weekend (1,2,3,4)
			lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["dayofweek"] < 5 & cluster1["numb_item_7"] == 0 & cluster1["customerid_0_visitor"]!=0], 
						  bw=bandwith,kernel="epanechnikov"),col='burlywood4')  
	
	}	
	
	
	
	
	
	
	################################################################################################################################
	bandwith<-0.5
		
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster1.csv",head=TRUE,sep=",")
	graph_transaction(bandwith)
		
	luster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster2.csv",head=TRUE,sep=",")
	graph_transaction(bandwith)
		
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster3.csv",head=TRUE,sep=",")
	graph_transaction(bandwith)
		
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster4.csv",head=TRUE,sep=",")
	graph_transaction(bandwith)
		
	bandwith<-0.8	
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster5.csv",head=TRUE,sep=",")
	graph_transaction(bandwith)
		
	################################################################################################################################
		
	
	# kernel density estimate	
	# total
	graph_transaction<-function(bandwith){
		
	plot(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]> 0 ], bw=bandwith,kernel="epanechnikov"),col='red',ylim=c(0,0.2))  
	###################################################################
	abline(v=0:50, col="gray", lty=3)		
	legend("topright", c("total","delivery","takeout","credit","cash","discount","after_3","before_3","is_visitor", "weekend",    "weekday",   "grouporder",  "individual_order"), 
				  fill=c("red",   "blue",    "green", "orange","purple","brown", "grey" ,  "cyan",    "chocolate", "darkorchid4", "burlywood4","darkorchid4", "darkolivegreen"))
	###################################################################
	# delivery
	lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["deli_1_takeout_0"]==1 ], bw=bandwith,kernel="epanechnikov"),col='blue')  
	# takeout		
	lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["deli_1_takeout_0"]==0 ], bw=bandwith,kernel="epanechnikov"),col='green')  
	
	# credit	
	lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["credit_1_cash_0"]==1 ], bw=bandwith,kernel="epanechnikov"),col='orange')  
	# cash
	lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["credit_1_cash_0"]==0 ], bw=bandwith,kernel="epanechnikov"),col='purple')  
	
	# with discount
	lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["is_discount"]==1 ], bw=bandwith,kernel="epanechnikov"),col='brown')  
	
	# after_3
	lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["hourofday"]>15 ], bw=bandwith,kernel="epanechnikov"),col='grey')  	
	# before_3
	lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["hourofday"]<15 ], bw=bandwith,kernel="epanechnikov"),col='cyan')  
	
	# is_visitor
	lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0  ], bw=bandwith,kernel="epanechnikov"),col='chocolate')  
	
	# weekday (4,5,6)
	lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["dayofweek"] > 4 ], bw=bandwith,kernel="epanechnikov"),col='darkorchid4')  
	# weekend (1,2,3,4)
	lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["dayofweek"] < 5 ], bw=bandwith,kernel="epanechnikov"),col='burlywood4')  
	
	# group
	lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["numb_item_7"] > 0 ], bw=bandwith,kernel="epanechnikov"),col='darkorchid4')  
	# individual
	lines(density(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & cluster1["numb_item_7"] == 0 ], bw=bandwith,kernel="epanechnikov"),col='darkolivegreen')  
	
	}	
	

	##############################################################################
	# using stats package, 
	# implements a rule-of-thumb for choosing the bandwidth of
	#      a Gaussian kernel density estimator.  It defaults to 0.9 times the
	#      minimum of the standard deviation and the interquartile range
	#      divided by 1.34 times the sample size to the negative one-fifth
	#      power (= Silverman's ‘rule of thumb’, Silverman (1986, page 48,
	#      eqn (3.31)) _unless_ the quartiles coincide when a positive result
	#      will be guaranteed.
	
	
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// kernel estimate: dependence on end points of the bins, we centre each block at each data point rather than fixing the end points of blocks.
	// 1. (every point - all other points) / bandwidth 
	// -> imagine that a rectangle (height 1/2h and width 2h) is placed over each observed point on the x–axis. The estimate of the pdf at a given point
	//	  is 1/n times the sum of the heights of all the rectangles that cover the point
	// -> f(x) ﬂuctuate less as value of h is increased. Increasing h one increases the width of each rectangle, thereby increases degree of “smoothing”.
	//    A kernel is a standardized weighting function, namely the weighting function with h = 1.
	//    The kernel determines the shape of the weighting function. 
	// -> Kernel estimation of pdfs is charactized by the kernel, 
	//    1. K, which determines the shape of the weighting function, and 
	//    2. the bandwidth, h, which determines the “width” of the weighting function and hence the amount of smoothing. 
	// -> Epanechnikov is optimalized
	// -> if pdf were normal, 0.9*sd/n^5 sd = min(s, R/1.34), R is the interquartile range of the data.
	
	
	
	
	
	
	
	
	
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	cluster1<-read.csv(file="/Users/alan/Desktop/present_graphics/cluster2.csv",head=TRUE,sep=",")
	order_multi<-read.csv(file="/Users/alan/Desktop/order_multi.csv",head=TRUE,sep=",")	
	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
	#################################################################
	# change the data frame value from NA to 0
	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0
	
	#cluster1$orderitem
	
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	// Start of PDF automatic process !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
		
		setwd("/Users/alan/Desktop/data_graphics/")
		
		order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
		bandwith<-0.5
		
		pdf('lunch_weekday_c2.pdf', bg = "white")		
		c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c2_menu.csv",head=TRUE,sep=",")			
		cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster2.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0
		###########################################################################################	
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
		cluster1$total[is.na(cluster1$total)] <- 0	
		
		###########################################################################################
		graph_individual_novisitor_before3_weekday12345(bandwith)
		dev.off()
		
		pdf('lunch_weekday_c3.pdf', bg = "white")		
		c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c3_menu.csv",head=TRUE,sep=",")			
		cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster3.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
		###########################################################################################	
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
		###########################################################################################
		graph_individual_novisitor_before3_weekday12345(bandwith)
		dev.off()
		
		pdf('lunch_weekday_c4.pdf', bg = "white")		
		c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c4_menu.csv",head=TRUE,sep=",")			
		cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster4.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
		###########################################################################################	
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
		###########################################################################################
		graph_individual_novisitor_before3_weekday12345(bandwith)
		dev.off()
		
		pdf('lunch_weekday_c5.pdf', bg = "white")		
		c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c5_menu.csv",head=TRUE,sep=",")			
		cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster5.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
		###########################################################################################	
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
		###########################################################################################
		graph_individual_novisitor_before3_weekday12345(bandwith)
		dev.off()
	
			#144
			#193
			#194
			#302
			#377
			#481
	
	# lunch_weekday, for lunch we include 1 - 5				
	graph_individual_novisitor_before3_weekday12345<-function(bandwith){
		###################################################################
		# graph menu information	
		data_menu <- c1_menu["price"][c1_menu["price"]<50 & c1_menu["price"]>0]
		data_menu[is.na(data_menu)] <- 0	
		numb_menu <- length(data_menu)
		plot(density(data_menu,bw=bandwith,kernel="gaussian"),col='grey',ylim=c(0,0.2))
		###################################################################
		# delivery			
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 				 cluster1["large_pizza"]==0 & 		
					         cluster1["deli_1_takeout_0"]==1 & 
							 cluster1["orderitem"] == 0 & 
							 cluster1["customerid_0_visitor"]!=0 &
			  				 cluster1["dayofweek"] < 6 &  
							 cluster1["hourofday"] < 15
							 & cluster1["locationid"] != 144	
							 & cluster1["locationid"] != 193
							 & cluster1["locationid"] != 194
							 & cluster1["locationid"] != 302
							 & cluster1["locationid"] != 377 
							 & cluster1["locationid"] != 481 						  
							 ]	
												 
		# bandwith <- bw.nrd0(data) 
		# print(bandwith)
		numb_delivery <- length(data)
		if(numb_delivery>500){
			lines(density(data,bw=bandwith,kernel="gaussian"),col='blue')
		}
	
			
	    ###################################################################
	    # takeout	
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 			  cluster1["large_pizza"]==0 & 			
						  cluster1["deli_1_takeout_0"]==0 & 
						  cluster1["orderitem"] == 0 & 
						  cluster1["customerid_0_visitor"]!=0 & 
						  cluster1["dayofweek"] < 6 &  
						  cluster1["hourofday"] < 15
 							 & cluster1["locationid"] != 144	
 							 & cluster1["locationid"] != 193
 							 & cluster1["locationid"] != 194
 							 & cluster1["locationid"] != 302
 							 & cluster1["locationid"] != 377 
 							 & cluster1["locationid"] != 481 
							  ]				  
		# bandwith <- bw.nrd0(data)				  	
		# print(bandwith)  
		numb_takeout <- length(data)
		if(numb_takeout>500){
		lines(density(data,bw=bandwith,kernel="gaussian"),col='green')  
		}	
		##############################################################################											
		# credit
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 		cluster1["large_pizza"]==0 & 		
					  cluster1["credit_1_cash_0"]==1 & 
					  cluster1["orderitem"] == 0 & 
					  cluster1["customerid_0_visitor"]!=0 & 						  
					  cluster1["dayofweek"]  < 6 & 
					  cluster1["hourofday"] < 15
						 & cluster1["locationid"] != 144	
						 & cluster1["locationid"] != 193
						 & cluster1["locationid"] != 194
						 & cluster1["locationid"] != 302
						 & cluster1["locationid"] != 377 
						 & cluster1["locationid"] != 481 ]	
		# bandwith <- bw.nrd0(data)
 		# print(bandwith)
		numb_credit <- length(data)	
		if(numb_credit > 500){
			lines(density(data, bw=bandwith,kernel="gaussian"),col='orange')  
		}
			
												  	    
		##############################################################################																
		# cash	
  		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 		cluster1["large_pizza"]==0 & 			
					  cluster1["credit_1_cash_0"]==0 & 
					  cluster1["orderitem"] == 0 & 
					  cluster1["customerid_0_visitor"]!=0 & 						  
					  cluster1["dayofweek"]  < 6 &  
					  cluster1["hourofday"] < 15
						 & cluster1["locationid"] != 144	
						 & cluster1["locationid"] != 193
						 & cluster1["locationid"] != 194
						 & cluster1["locationid"] != 302
						 & cluster1["locationid"] != 377 
						 & cluster1["locationid"] != 481 ]
  		# bandwith <- bw.nrd0(data)
	    # print(bandwith)
		numb_cash <- length(data)				  
		if(numb_cash > 500){
			lines(density(data,bw=bandwith,kernel="gaussian"),col='purple')  
		}

		##############################################################################																
		# with discount
  		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 		cluster1["large_pizza"]==0 & 		
					  cluster1["is_discount"]==1 & 
					  cluster1["orderitem"] == 0 & 																  
					  cluster1["customerid_0_visitor"]!=0 & 
			  		  cluster1["dayofweek"]  < 6 & 
					  cluster1["hourofday"] < 15
						 & cluster1["locationid"] != 144	
						 & cluster1["locationid"] != 193
						 & cluster1["locationid"] != 194
						 & cluster1["locationid"] != 302
						 & cluster1["locationid"] != 377 
						 & cluster1["locationid"] != 481 ]
		# bandwith <- bw.nrd0(data)
  		# print(bandwith)	
		numb_discount <- length(data)
		if(numb_discount > 500){
			lines(density(data, bw=bandwith,kernel="gaussian"),col='brown')  	 
		}	
																					 	###################################################################
		abline(v=0:50, col="gray", lty=3)		
		legend("topright", c(paste("delivery",numb_delivery),
							 paste("takeout",numb_takeout),
							 paste("credit",numb_credit),
							 paste("cash",numb_cash),
							 paste("discount",numb_discount), 
							 paste("menu_price",numb_menu)), 
		fill=c("blue",    "green", "orange","purple","brown","grey"))
		
			
	}	
	
	
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	
	setwd("/Users/alan/Desktop/data_graphics/")
	
	order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
	bandwith<-0.5
	
	pdf('dinner_weekday_c2.pdf', bg = "white")		
	c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c2_menu.csv",head=TRUE,sep=",")			
	cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster2.csv",head=TRUE,sep=",")
	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
	###########################################################################################	
	large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
	cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
	cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
	cluster1$total[is.na(cluster1$total)] <- 0		
	###########################################################################################	
	graph_individual_novisitor_after3_weekday1234(bandwith)
	dev.off()
	
	pdf('dinner_weekday_c3.pdf', bg = "white")		
	c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c3_menu.csv",head=TRUE,sep=",")			
	cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster3.csv",head=TRUE,sep=",")
	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
	###########################################################################################	
	large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
	cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
	cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
	###########################################################################################	
	graph_individual_novisitor_after3_weekday1234(bandwith)
	dev.off()
	
	pdf('dinner_weekday_c4.pdf', bg = "white")		
	c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c4_menu.csv",head=TRUE,sep=",")			
	cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster4.csv",head=TRUE,sep=",")
	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
	###########################################################################################	
	large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
	cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
	cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
	###########################################################################################	
	graph_individual_novisitor_after3_weekday1234(bandwith)
	dev.off()
	
	pdf('dinner_weekday_c5.pdf', bg = "white")		
	c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c5_menu.csv",head=TRUE,sep=",")			
	cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster5.csv",head=TRUE,sep=",")
	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
	###########################################################################################	
	large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
	cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
	cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
	###########################################################################################	
	graph_individual_novisitor_after3_weekday1234(bandwith)
	dev.off()
	
	graph_individual_novisitor_after3_weekday1234<-function(bandwith){
		###################################################################
		# graph menu information	
		data_menu <- c1_menu["price"][c1_menu["price"]<50 & c1_menu["price"]>0]
		data_menu[is.na(data_menu)] <- 0	
		numb_menu <- length(data_menu)
		plot(density(data_menu,bw=bandwith,kernel="gaussian"),col='grey',ylim=c(0,0.2))
		###################################################################
		# delivery			
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 				cluster1["large_pizza"]==0 & 		
					         cluster1["deli_1_takeout_0"]==1 & 
							 cluster1["orderitem"] == 0 & 
							 cluster1["customerid_0_visitor"]!=0 &
			  				 cluster1["dayofweek"] < 5 &  
							 cluster1["hourofday"] > 15
								 & cluster1["locationid"] != 144	
								 & cluster1["locationid"] != 193
								 & cluster1["locationid"] != 194
								 & cluster1["locationid"] != 302
								 & cluster1["locationid"] != 377 
								 & cluster1["locationid"] != 481 ]							 
		# bandwith <- bw.nrd0(data) 
		# print(bandwith)
		numb_delivery <- length(data)
		if(numb_delivery>500){
			lines(density(data,bw=bandwith,kernel="gaussian"),col='blue')
		}
	
			
	    ###################################################################
	    # takeout	
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 			cluster1["large_pizza"]==0 & 		
						  cluster1["deli_1_takeout_0"]==0 & 
						  cluster1["orderitem"] == 0 & 
						  cluster1["customerid_0_visitor"]!=0 & 
						  cluster1["dayofweek"] < 5 &  
						  cluster1["hourofday"] > 15
 							 & cluster1["locationid"] != 144	
 							 & cluster1["locationid"] != 193
 							 & cluster1["locationid"] != 194
 							 & cluster1["locationid"] != 302
 							 & cluster1["locationid"] != 377 
 							 & cluster1["locationid"] != 481 ]				  
		# bandwith <- bw.nrd0(data)				  	
		# print(bandwith)  
		numb_takeout <- length(data)
		if(numb_takeout>500){
		lines(density(data,bw=bandwith,kernel="gaussian"),col='green')  
		}	
		##############################################################################											
		# credit
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 		cluster1["large_pizza"]==0 & 			
					  cluster1["credit_1_cash_0"]==1 & 
					  cluster1["orderitem"] == 0 & 
					  cluster1["customerid_0_visitor"]!=0 & 						  
					  cluster1["dayofweek"]  < 5 & 
					  cluster1["hourofday"] > 15
						 & cluster1["locationid"] != 144	
						 & cluster1["locationid"] != 193
						 & cluster1["locationid"] != 194
						 & cluster1["locationid"] != 302
						 & cluster1["locationid"] != 377 
						 & cluster1["locationid"] != 481 ]	
		# bandwith <- bw.nrd0(data)
 		# print(bandwith)
		numb_credit <- length(data)	
		if(numb_credit > 500){
			lines(density(data, bw=bandwith,kernel="gaussian"),col='orange')  
		}
			
												  	    
		##############################################################################																
		# cash	
  		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 		cluster1["large_pizza"]==0 & 		
					  cluster1["credit_1_cash_0"]==0 & 
					  cluster1["orderitem"] == 0 & 
					  cluster1["customerid_0_visitor"]!=0 & 						  
					  cluster1["dayofweek"]  < 5 &  
					  cluster1["hourofday"] > 15
						 & cluster1["locationid"] != 144	
						 & cluster1["locationid"] != 193
						 & cluster1["locationid"] != 194
						 & cluster1["locationid"] != 302
						 & cluster1["locationid"] != 377 
						 & cluster1["locationid"] != 481 ]
  		# bandwith <- bw.nrd0(data)
	    # print(bandwith)
		numb_cash <- length(data)				  
		if(numb_cash > 500){
			lines(density(data,bw=bandwith,kernel="gaussian"),col='purple')  
		}

		##############################################################################																
		# with discount
  		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 &
			 		cluster1["large_pizza"]==0 & 		 
					  cluster1["is_discount"]==1 & 
					  cluster1["orderitem"] == 0 & 																  
					  cluster1["customerid_0_visitor"]!=0 & 
			  		  cluster1["dayofweek"]  < 5 & 
					  cluster1["hourofday"] > 15
						 & cluster1["locationid"] != 144	
						 & cluster1["locationid"] != 193
						 & cluster1["locationid"] != 194
						 & cluster1["locationid"] != 302
						 & cluster1["locationid"] != 377 
						 & cluster1["locationid"] != 481 ]
		# bandwith <- bw.nrd0(data)
  		# print(bandwith)	
		numb_discount <- length(data)
		if(numb_discount > 500){
			lines(density(data, bw=bandwith,kernel="gaussian"),col='brown')  	 
		}	
																					 	###################################################################
		abline(v=0:50, col="gray", lty=3)		
		legend("topright", c(paste("delivery",numb_delivery),
							 paste("takeout",numb_takeout),
							 paste("credit",numb_credit),
							 paste("cash",numb_cash),
							 paste("discount",numb_discount), 
							 paste("menu_price",numb_menu)), 
		fill=c("blue",    "green", "orange","purple","brown","grey"))
		
			
	}	
	
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
		
		setwd("/Users/alan/Desktop/data_graphics/")
		
		order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
		bandwith<-0.5
		
		pdf('lunch_weekend_c2.pdf', bg = "white")		
		c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c2_menu.csv",head=TRUE,sep=",")			
		cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster2.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
		###########################################################################################	
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
		cluster1$total[is.na(cluster1$total)] <- 0		
		###########################################################################################	
		graph_individual_novisitor_before3_weekend67(bandwith)
		dev.off()
		
		pdf('lunch_weekend_c3.pdf', bg = "white")		
		c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c3_menu.csv",head=TRUE,sep=",")			
		cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster3.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
		###########################################################################################	
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
		###########################################################################################	
		graph_individual_novisitor_before3_weekend67(bandwith)
		dev.off()
		
		pdf('lunch_weekend_c4.pdf', bg = "white")		
		c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c4_menu.csv",head=TRUE,sep=",")			
		cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster4.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
		###########################################################################################	
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
		###########################################################################################	
		graph_individual_novisitor_before3_weekend67(bandwith)
		dev.off()
		
		pdf('lunch_weekend_c5.pdf', bg = "white")		
		c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c5_menu.csv",head=TRUE,sep=",")			
		cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster5.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
		###########################################################################################	
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
		###########################################################################################	
		graph_individual_novisitor_before3_weekend67(bandwith)
		dev.off()
	
		
	graph_individual_novisitor_before3_weekend67<-function(bandwith){
		###################################################################
		# graph menu information	
		data_menu <- c1_menu["price"][c1_menu["price"]<50 & c1_menu["price"]>0]
		data_menu[is.na(data_menu)] <- 0	
		numb_menu <- length(data_menu)
		plot(density(data_menu,bw=bandwith,kernel="gaussian"),col='grey',ylim=c(0,0.2))
		###################################################################
		# delivery			
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 				cluster1["large_pizza"]==0 & 			
					         cluster1["deli_1_takeout_0"]==1 & 
							 cluster1["orderitem"] == 0 & 
							 cluster1["customerid_0_visitor"]!=0 &
			  				 cluster1["dayofweek"] > 5 &  
							 cluster1["hourofday"] < 15
								 & cluster1["locationid"] != 144	
								 & cluster1["locationid"] != 193
								 & cluster1["locationid"] != 194
								 & cluster1["locationid"] != 302
								 & cluster1["locationid"] != 377 
								 & cluster1["locationid"] != 481 ]							 
		# bandwith <- bw.nrd0(data) 
		# print(bandwith)
		numb_delivery <- length(data)
		if(numb_delivery>500){
			lines(density(data,bw=bandwith,kernel="gaussian"),col='blue')
		}
	
			
	    ###################################################################
	    # takeout	
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 			cluster1["large_pizza"]==0 & 			
						  cluster1["deli_1_takeout_0"]==0 & 
						  cluster1["orderitem"] == 0 & 
						  cluster1["customerid_0_visitor"]!=0 & 
						  cluster1["dayofweek"] > 5 &  
						  cluster1["hourofday"] < 15
 							 & cluster1["locationid"] != 144	
 							 & cluster1["locationid"] != 193
 							 & cluster1["locationid"] != 194
 							 & cluster1["locationid"] != 302
 							 & cluster1["locationid"] != 377 
 							 & cluster1["locationid"] != 481 ]				  
		# bandwith <- bw.nrd0(data)				  	
		# print(bandwith)  
		numb_takeout <- length(data)
		if(numb_takeout>500){
		lines(density(data,bw=bandwith,kernel="gaussian"),col='green')  
		}	
		##############################################################################											
		# credit
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 		cluster1["large_pizza"]==0 & 		
					  cluster1["credit_1_cash_0"]==1 & 
					  cluster1["orderitem"] == 0 & 
					  cluster1["customerid_0_visitor"]!=0 & 						  
					  cluster1["dayofweek"]  > 5 & 
					  cluster1["hourofday"] < 15
						 & cluster1["locationid"] != 144	
						 & cluster1["locationid"] != 193
						 & cluster1["locationid"] != 194
						 & cluster1["locationid"] != 302
						 & cluster1["locationid"] != 377 
						 & cluster1["locationid"] != 481 ]	
		# bandwith <- bw.nrd0(data)
 		# print(bandwith)
		numb_credit <- length(data)	
		if(numb_credit > 500){
			lines(density(data, bw=bandwith,kernel="gaussian"),col='orange')  
		}
			
												  	    
		##############################################################################																
		# cash	
  		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 		cluster1["large_pizza"]==0 & 			
					  cluster1["credit_1_cash_0"]==0 & 
					  cluster1["orderitem"] == 0 & 
					  cluster1["customerid_0_visitor"]!=0 & 						  
					  cluster1["dayofweek"]  > 5 &  
					  cluster1["hourofday"] < 15
						 & cluster1["locationid"] != 144	
						 & cluster1["locationid"] != 193
						 & cluster1["locationid"] != 194
						 & cluster1["locationid"] != 302
						 & cluster1["locationid"] != 377 
						 & cluster1["locationid"] != 481 ]
  		# bandwith <- bw.nrd0(data)
	    # print(bandwith)
		numb_cash <- length(data)				  
		if(numb_cash > 500){
			lines(density(data,bw=bandwith,kernel="gaussian"),col='purple')  
		}

		##############################################################################																
		# with discount
  		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 		cluster1["large_pizza"]==0 & 			
					  cluster1["is_discount"]==1 & 
					  cluster1["orderitem"] == 0 & 																  
					  cluster1["customerid_0_visitor"]!=0 & 
			  		  cluster1["dayofweek"]  > 5 & 
					  cluster1["hourofday"] < 15
						 & cluster1["locationid"] != 144	
						 & cluster1["locationid"] != 193
						 & cluster1["locationid"] != 194
						 & cluster1["locationid"] != 302
						 & cluster1["locationid"] != 377 
						 & cluster1["locationid"] != 481 ]
		# bandwith <- bw.nrd0(data)
  		# print(bandwith)	
		numb_discount <- length(data)
		if(numb_discount > 500){
			lines(density(data, bw=bandwith,kernel="gaussian"),col='brown')  	 
		}	
																					 	###################################################################
		abline(v=0:50, col="gray", lty=3)		
		legend("topright", c(paste("delivery",numb_delivery),
							 paste("takeout",numb_takeout),
							 paste("credit",numb_credit),
							 paste("cash",numb_cash),
							 paste("discount",numb_discount), 
							 paste("menu_price",numb_menu)), 
		fill=c("blue",    "green", "orange","purple","brown","grey"))
		
			
	}	
	
	
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	
	
	setwd("/Users/alan/Desktop/data_graphics/")
	
	order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
	bandwith<-0.5
	
	pdf('dinner_weekend_c2.pdf', bg = "white")	
	#######################################################################
	# NOTE!!!!!!!!!!!!!!!!: menu table need to be updated with removing catering	
	c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c2_menu.csv",head=TRUE,sep=",")			
	cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster2.csv",head=TRUE,sep=",")
	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
	###########################################################################################	
	large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
	cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
	cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
	cluster1$total[is.na(cluster1$total)] <- 0	
	###########################################################################################	
	graph_individual_novisitor_after3_weekend567(bandwith)
	dev.off()
	
	pdf('dinner_weekend_c3.pdf', bg = "white")		
	c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c3_menu.csv",head=TRUE,sep=",")			
	cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster3.csv",head=TRUE,sep=",")
	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
	###########################################################################################	
	large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
	cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
	cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
	###########################################################################################	
	graph_individual_novisitor_after3_weekend567(bandwith)
	dev.off()
	
	pdf('dinner_weekend_c4.pdf', bg = "white")		
	c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c4_menu.csv",head=TRUE,sep=",")			
	cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster4.csv",head=TRUE,sep=",")
	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
	###########################################################################################	
	large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
	cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
	cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
	###########################################################################################	
	graph_individual_novisitor_after3_weekend567(bandwith)
	dev.off()
	
	pdf('dinner_weekend_c5.pdf', bg = "white")		
	c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c5_menu.csv",head=TRUE,sep=",")			
	cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster5.csv",head=TRUE,sep=",")
	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
	###########################################################################################	
	large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
	cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
	cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
	###########################################################################################	
	graph_individual_novisitor_after3_weekend567(bandwith)
	dev.off()

	
graph_individual_novisitor_after3_weekend567<-function(bandwith){
	###################################################################
	# graph menu information	
	data_menu <- c1_menu["price"][c1_menu["price"]<50 & c1_menu["price"]>0]
	data_menu[is.na(data_menu)] <- 0	
	
	numb_menu <- length(data_menu)
	plot(density(data_menu,bw=bandwith,kernel="gaussian"),col='grey',ylim=c(0,0.2))
	###################################################################
	# delivery			
	data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
		 				cluster1["large_pizza"]==0 & 	
						 cluster1["large_pizza"]==0 & 
				         cluster1["deli_1_takeout_0"]==1 & 
						 cluster1["orderitem"] == 0 & 
						 cluster1["customerid_0_visitor"]!=0 &
		  				 cluster1["dayofweek"] > 4 &  
						 cluster1["hourofday"] > 15
							 & cluster1["locationid"] != 144	
							 & cluster1["locationid"] != 193
							 & cluster1["locationid"] != 194
							 & cluster1["locationid"] != 302
							 & cluster1["locationid"] != 377 
							 & cluster1["locationid"] != 481 ]							 
	# bandwith <- bw.nrd0(data) 
	# print(bandwith)
	numb_delivery <- length(data)
	if(numb_delivery>500){
		lines(density(data,bw=bandwith,kernel="gaussian"),col='blue')
	}

		
    ###################################################################
    # takeout	
	data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
		 		cluster1["large_pizza"]==0 & 		
					  cluster1["deli_1_takeout_0"]==0 & 
					  cluster1["orderitem"] == 0 & 
					  cluster1["customerid_0_visitor"]!=0 & 
					  cluster1["dayofweek"] > 4 &  
					  cluster1["hourofday"] > 15
						 & cluster1["locationid"] != 144	
						 & cluster1["locationid"] != 193
						 & cluster1["locationid"] != 194
						 & cluster1["locationid"] != 302
						 & cluster1["locationid"] != 377 
						 & cluster1["locationid"] != 481 ]				  
	# bandwith <- bw.nrd0(data)				  	
	# print(bandwith)  
	numb_takeout <- length(data)
	if(numb_takeout>500){
	lines(density(data,bw=bandwith,kernel="gaussian"),col='green')  
	}	
	##############################################################################											
	# credit
	data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
		 		cluster1["large_pizza"]==0 & 		
				  cluster1["credit_1_cash_0"]==1 & 
				  cluster1["orderitem"] == 0 & 
				  cluster1["customerid_0_visitor"]!=0 & 						  
				  cluster1["dayofweek"]  > 4 & 
				  cluster1["hourofday"] > 15
					 & cluster1["locationid"] != 144	
					 & cluster1["locationid"] != 193
					 & cluster1["locationid"] != 194
					 & cluster1["locationid"] != 302
					 & cluster1["locationid"] != 377 
					 & cluster1["locationid"] != 481 ]	
	# bandwith <- bw.nrd0(data)
	# print(bandwith)
	numb_credit <- length(data)	
	if(numb_credit > 500){
		lines(density(data, bw=bandwith,kernel="gaussian"),col='orange')  
	}
		
											  	    
	##############################################################################																
	# cash	
	data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
		 		cluster1["large_pizza"]==0 & 	
				  cluster1["credit_1_cash_0"]==0 & 
				  cluster1["orderitem"] == 0 & 
				  cluster1["customerid_0_visitor"]!=0 & 						  
				  cluster1["dayofweek"]  > 4 &  
				  cluster1["hourofday"] > 15
					 & cluster1["locationid"] != 144	
					 & cluster1["locationid"] != 193
					 & cluster1["locationid"] != 194
					 & cluster1["locationid"] != 302
					 & cluster1["locationid"] != 377 
					 & cluster1["locationid"] != 481 ]
	# bandwith <- bw.nrd0(data)
    # print(bandwith)
	numb_cash <- length(data)				  
	if(numb_cash > 500){
		lines(density(data,bw=bandwith,kernel="gaussian"),col='purple')  
	}

	##############################################################################																
	# with discount
	data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
		 		cluster1["large_pizza"]==0 & 
				  cluster1["is_discount"]==1 & 
				  cluster1["orderitem"] == 0 & 																  
				  cluster1["customerid_0_visitor"]!=0 & 
		  		  cluster1["dayofweek"]  > 4 & 
				  cluster1["hourofday"] > 15
					 & cluster1["locationid"] != 144	
					 & cluster1["locationid"] != 193
					 & cluster1["locationid"] != 194
					 & cluster1["locationid"] != 302
					 & cluster1["locationid"] != 377 
					 & cluster1["locationid"] != 481 ]
	# bandwith <- bw.nrd0(data)
	# print(bandwith)	
	numb_discount <- length(data)
	if(numb_discount > 500){
		lines(density(data, bw=bandwith,kernel="gaussian"),col='brown')  	 
	}	
																				 	###################################################################
	abline(v=0:50, col="gray", lty=3)		
	legend("topright", c(paste("delivery",numb_delivery),
						 paste("takeout",numb_takeout),
						 paste("credit",numb_credit),
						 paste("cash",numb_cash),
						 paste("discount",numb_discount), 
						 paste("menu_price",numb_menu)), 
	fill=c("blue",    "green", "orange","purple","brown","grey"))
	
		
}	


	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	
	
	
	
	cut_regression <- read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")
	
	incomescore <- cut_regression$IncomeScore
	incomescore_sq <- (cut_regression$IncomeScore) ^2
	trimmed <- cut_regression$Trimmed
	trimmed_sq <- (cut_regression$Trimmed)^2
	oneperson <- cut_regression$OnePerson
	enteractive <- cut_regression$Trimmed * cut_regression$OnePerson
	trimmed_root <- (cut_regression$Trimmed)^0.5
	
	###################################### un successful
	before_3 <- cut_regression$factor_3
	is_pizza <- cut_regression$is_pizza
	incomescore_tri <- (cut_regression$IncomeScore) ^3	
	trimmed_tri <- (cut_regression$Trimmed)^3	
		
	
	model_cut <- lm(oneperson~incomescore+trimmed)	
	model_cut <- lm(oneperson~log(incomescore)+log(trimmed))	
	model_cut <- lm( oneperson ~ trimmed + incomescore + trimmed_sq + incomescore_sq+ trimmed_tri + incomescore_tri )
	# R^2 = 0.16	
	###################################################################
	# highest adjusted R square  R^2 0.19
	###################################################################
	model_cut <- lm( oneperson ~ trimmed + incomescore + trimmed_sq + incomescore_sq )
	# Adjusted R-squared:  0.1905 
	model_cut <- lm( oneperson ~ trimmed + trimmed_sq + incomescore_sq )
	# Adjusted R-squared:  0.2216 
	model_cut <- lm( oneperson ~ trimmed + trimmed_sq  )
	# Adjusted R-squared:  0.1603  
	summary(model_cut)
		
	# Therefore, we should use 
	model_cut <- lm( oneperson ~ trimmed + trimmed_sq + incomescore_sq )
	summary(model_cut)
		
	model_cut <- lm( oneperson ~ trimmed + incomescore + trimmed_sq + incomescore_sq )
		
	model_cut <- lm( oneperson ~ trimmed + incomescore + trimmed : incomescore + trimmed_sq + incomescore_sq) 
	
	model_cut <- lm( oneperson ~ trimmed + incomescore + trimmed : incomescore + trimmed_sq ) 
		
	model_cut <- lm( oneperson ~ trimmed + trimmed : incomescore + trimmed_sq + incomescore_sq) 

	model_cut <- lm( oneperson ~ trimmed + trimmed : incomescore + trimmed_sq + incomescore_sq) 

	model_cut <- lm( oneperson ~ trimmed + trimmed : incomescore + trimmed_sq + log(incomescore) +incomescore) 

	model_cut <- lm( oneperson ~ trimmed + trimmed : incomescore + trimmed_sq + incomescore + is_pizza) 

	# The best model so far 
	model_cut <- lm( oneperson ~ trimmed + trimmed : incomescore + trimmed_sq + incomescore) 
		
		model_cut <- lm( oneperson ~ trimmed_root + incomescore) 
		
	
		Coefficients:
		                    Estimate Std. Error t value Pr(>|t|)   
		(Intercept)         -1.73737    2.65008  -0.656  0.51832   
		trimmed              1.23277    0.38456   3.206  0.00379 **
		trimmed_sq          -0.02979    0.01391  -2.142  0.04257 * 
		incomescore          5.34804    2.18574   2.447  0.02211 * 
		trimmed:incomescore -0.55780    0.25514  -2.186  0.03878 * 
		
		Multiple R-squared:  0.3788,	Adjusted R-squared:  0.2753 
	
	###########################################################################################
	# after I suppress the constant term, the result change dramatically, increase R^2 to 98%
	model_cut <- lm( oneperson ~ 0 + trimmed + trimmed : incomescore + trimmed_sq + incomescore) 
	###########################################################################################

			Coefficients:
			                    Estimate Std. Error t value Pr(>|t|)    
			trimmed              1.00336    0.15765   6.364 1.16e-06 ***
			trimmed_sq          -0.02638    0.01275  -2.069 0.049056 *  
			incomescore          4.06731    0.96911   4.197 0.000298 ***
			trimmed:incomescore -0.41005    0.11824  -3.468 0.001913 ** 			
			Adjusted R-squared:  0.29
	# I take 10 more result and get this answer, it is pretty closed, 
	# then I add these 10 data back to original  
			
			Test on 10 data set :
	
			Test Set								My   Model          percent difference
			170	0.804781635	4.549305556	0.614483493	5	 5.790648298	15.81%
			715	1.756393771	10.00661765	0.25170068	9	 7.335686456	-18.49%
			361	1.928204029	10.2377551	0.083844581	6.5	 7.255242617	11.62%
			618	1.349573749	4.621698113	0.593949045	6.75 7.005268488	3.78%
			282	2.107539172	10.06666667	0.257918552	7	 7.299637767	4.28%
			151	1.88238796	6.078333333	0.676		7	 8.088670339	15.55%
			431	0.91648807	6.745492958	0.155096012	7	 6.760468438	-3.42%
			570	1.086155165	9.7996		0.231511254	8    7.352402517	-8.09%
			571	1.559603753	4.760149254	0.416184971	4.5	 7.477601004	66.17%
			537	1.594965948	9.085777778	0.247474747	8	 7.483580018	-6.46%
			593	1.325117874	5.435		0.751037344	9  	 7.110495509	-20.99%
	
	
				
			Coefficients:
			                    Estimate Std. Error t value Pr(>|t|)    
			trimmed              1.17355    0.16016   7.327 1.23e-08 ***
			trimmed_sq          -0.04050    0.01275  -3.175  0.00306 ** 
			incomescore          2.84827    0.83807   3.399  0.00167 ** 
			trimmed:incomescore -0.29626    0.09521  -3.112  0.00363 ** 
			
			Multiple R-squared:  0.29
				
			LocationID	IncomeScore	Trimmed	total_before_3_rate	model_30 model_40	model_30_40_dif
			381			1.325117874	15.22432432	0.337803667	6.27842683	6.276950795	0%
			212			0.773110444	16.84926606	0.174065714	7.219684168	6.618439371	8%
			401			1.325117874	14.28967517	0.359568733	6.576190993	6.664219306	-1%
			411			1.770133829	8.109130435	0			7.715405576	7.642501297	1%
			205			0.984783541	13.77619048	0.190211732	7.258441454	7.266518339	0%
			..........
			the average difference is only 0.036995827
			which means that the original data show really good prediction, 
			since we have more data coming up, I will go ahead use the new 40 dataset with	
			
				Coefficients:
				                    Estimate Std. Error t value Pr(>|t|)    
				trimmed              1.17355    0.16016   7.327 1.23e-08 ***
				trimmed_sq          -0.04050    0.01275  -3.175  0.00306 ** 
				incomescore          2.84827    0.83807   3.399  0.00167 ** 
				trimmed:incomescore -0.29626    0.09521  -3.112  0.00363 ** 
		
		Then I will run the filtering with this new dataset	


	
	###########################################################################
	###########################################################################
	###########################################################################
	# silverman dip test
	###########################################################################
	# numb of mode keep going up until we have significant value	
	require(silvermantest)
	plot(density(x, bw=0.5,kernel="gaussian"))
	silverman.test(x,1)
	silverman.test(x,2)
	silverman.test(x,3)
	silverman.test(x,4)
	silverman.test(x,5)
	
	###############################################################################
	# Conduct silvermantest
	###############################################################################
	# Note: Can run the silvermantest on certain range instead of the whole thing
	require(silvermantest)
	order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
	bandwith<-0.5
	
	cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster2.csv",head=TRUE,sep=",")
	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
			
	data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
				  cluster1["credit_1_cash_0"]==0 & 
				  cluster1["orderitem"] == 0 & 
				  cluster1["customerid_0_visitor"]!=0 & 						  
				  cluster1["dayofweek"]  < 5 &  
				  cluster1["hourofday"] > 15]
					  
  	data_range1 <- cluster1["total"][cluster1["total"]<10 & cluster1["total"]>0 & 
  				  cluster1["credit_1_cash_0"]==0 & 
  				  cluster1["orderitem"] == 0 & 
  				  cluster1["customerid_0_visitor"]!=0 & 						  
  				  cluster1["dayofweek"]  < 5 &  
  				  cluster1["hourofday"] > 15]
	  
	bandwith <- 0.5
    # print(bandwith)
	plot(density(data_range1,bw=bandwith,kernel="epanechnikov"),col='purple')  
	numb_cash <- length(data_range1)	
				  
  	silverman.test(data_range1,1)
  	silverman.test(data_range1,2)
  	silverman.test(data_range1,3)
  	silverman.test(data_range1,4)
  	silverman.test(data_range1,5)	
					  
	##################################################################################	
	
					  
	  >   silverman.test(data,2)
	  Silvermantest: Testing the hypothesis if the number of modes is <=  2 
	  The P-Value is  0.01101101 
	  >   silverman.test(data,3)
	  Silvermantest: Testing the hypothesis if the number of modes is <=  3 
	  The P-Value is  0.002002002 
	  >   silverman.test(data,4)
	  Silvermantest: Testing the hypothesis if the number of modes is <=  4 
	  The P-Value is  0.006006006 
	  >   silverman.test(data,5)
	  Silvermantest: Testing the hypothesis if the number of modes is <=  5 
	  The P-Value is  0.7437437 
		
	for(i in 1:5){
	  	print(silverman.test(data,3))
	}	
	
	////////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////
	// menu distribution
	# hist(cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 ] , breaks=seq(0,50,0.5)) 
	
	c1_menu<-read.csv(file="/Users/alan/Desktop/data_reference/c1_menu.csv",head=TRUE,sep=",")	
	hist(c1_menu["price"][c1_menu["price"]<50 & c1_menu["price"]>0],probability=TRUE, breaks=seq(0,50),col="grey")
		plot(density(data,bw=bandwith,kernel="gaussian"),col='blue',ylim=c(0,0.2))
	
	c1_menu<-read.csv(file="/Users/alan/Desktop/data_reference/c2_menu.csv",head=TRUE,sep=",")	
	hist(c1_menu["price"][c1_menu["price"]<50 & c1_menu["price"]>0],probability=TRUE, breaks=seq(0,50,1),col="grey")

	c1_menu<-read.csv(file="/Users/alan/Desktop/data_reference/c3_menu.csv",head=TRUE,sep=",")	
	hist(c1_menu["price"][c1_menu["price"]<50 & c1_menu["price"]>0],probability=TRUE, breaks=seq(0,50,1),col="grey")
	
	c1_menu<-read.csv(file="/Users/alan/Desktop/data_reference/c4_menu.csv",head=TRUE,sep=",")	
	hist(c1_menu["price"][c1_menu["price"]<50 & c1_menu["price"]>0],probability=TRUE, breaks=seq(0,50,1),col="grey")
	
	c1_menu<-read.csv(file="/Users/alan/Desktop/data_reference/c5_menu.csv",head=TRUE,sep=",")	
	hist(c1_menu["price"][c1_menu["price"]<50 & c1_menu["price"]>0],probability=TRUE, breaks=seq(0,50,1),col="grey")
				
	###################################################################								
	 abline(v=0:50, col="gray", lty=3)		
	 // legend("topright", c(paste("delivery",numb_delivery),
 // 	 paste("takeout",numb_takeout),
 // 	 paste("credit",numb_credit),
 // 	 paste("cash",numb_cash),
 // 	 paste("discount",numb_discount)), 
 // 	 fill=c("blue",    "green", "orange","purple","brown"))
 // 	
	
	
 # if people put their price of menu to be really low, the price a person spend for a meal will 
 # likely to be really low. Extreme case, when price of menu is 0, the price a person spend for a meal 
 # will be 0 
	
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
	
	
	rest_all<-read.csv(file="/Users/alan/Desktop/data_reference/menu_all.csv",head=TRUE,sep=",")	
	rest_all_sample<-read.csv(file="/Users/alan/Desktop/data_reference/rest_all_sample.csv",head=TRUE,sep=",")	
	
	for(locationid in rest_all_sample$locationid){
		price_cut_01_9(locationid)			
	}
		
	price_cut_01_9<-function(locationid){
			range <- c(seq(5,9,0.1))
		
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

	
		1. copy the output from the cut algorithm to excel
		2. empty the ml_cut database and import the data into the database
		3. run the query_cut script to get the export excel file
		4. run the algorithm of the new cut price table (automatic get the graphes) 
	
	
	
	################################################################################
	################################################################################
	################################################################################
	################################################################################
			
	
	
	
	order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
	cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster2.csv",head=TRUE,sep=",")
	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)			
	numb_item_no_restrict_table <- read.csv(file="/Users/alan/Desktop/data_reference/numb_item_no_restrict_table.csv",head=TRUE,sep=",")
	cluster1<-merge(cluster1, numb_item_no_restrict_table, by="orderid")
	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
	#####################################################################
	large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
	cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
	cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
	#####################################################################
	#####################################################################
	#####################################################################	
	
	algorithm_numb_item<-function(price){
		
		data_location_lowerprice <- subset(data_location,total < price)
		total_number_row_lowerprice <- nrow(data_location_lowerprice)
		# print(total_number_row_lowerprice)
		data_location_lowerprice_moreitem <- subset(data_location_lowerprice,numb_item_no_restrict>1)
		total_number_row_lowerprice_moreitem <- nrow(data_location_lowerprice_moreitem)
		# print(total_number_row_lowerprice_moreitem)
		prob_moreitem <- total_number_row_lowerprice_moreitem/total_number_row_lowerprice
		#vec_price <- c(vec_price,price)
		#vec_ratio <- c(vec_ratio,prob_moreitem)
		print(c(price,total_number_row_lowerprice_moreitem/total_number_row_lowerprice))
			
		return (c(price,total_number_row_lowerprice_moreitem/total_number_row_lowerprice))
			
	}
	
	#####################################################################
	#####################################################################	
	# graph_multi_item_before3_weekday1234
	setwd("/Users/alan/Desktop/item_graphics/")
	pdf('before3_weekday1234_c5.pdf', bg = "white")	
				
		locationlist <- unique(cluster1$locationid)			
		vec_color_exist <- rgb(runif(300),runif(300),runif(300))	
		vec_color <- c()
		count_color <- 1		
		vec_location <- c()	
		
		for(i in locationlist){
			LocationID = i
			data_location<-subset(cluster1,locationid==LocationID)		
			data_location<-subset(data_location,credit_1_cash_0 == 0)
			data_location<-subset(data_location,orderitem == 0)
			data_location<-subset(data_location,customerid_0_visitor!=0)
			data_location<-subset(data_location,dayofweek  < 5 )
			data_location<-subset(data_location,hourofday < 15)
			data_length <- length(data_location$orderid)
				 
			if(data_length > 100){		
				vec_price <- c()	
				vec_ratio <- c()	
				for(i in seq(4,20,0.1)){
					price <- (algorithm_numb_item(i)[1])	
					prob_more_item <- (algorithm_numb_item(i)[2])				
					vec_price <- c(vec_price,price)
					vec_ratio <- c(vec_ratio,prob_more_item)
				}
					
				data <- data.frame(vec_price,vec_ratio)
				current_color = vec_color_exist[count_color]			
				vec_color <- c(vec_color,current_color)	
				vec_location <- c(vec_location,paste(LocationID,data_length, sep=":"))
				
				if(count_color==1){
					plot(data, type="l",ylim=c(0,1),col=current_color,pch=2)
				}
				else{
					lines(data, type="l",ylim=c(0,1),col=current_color,pch=2)
				}
				abline(v=seq(0,18,1), col="grey", lty=1)
				count_color = count_color+1
				
			}else{
				print(c(LocationID,"no data"))
			}
		
		}

		legend("topleft", as.character(vec_location), fill=vec_color)
	
	dev.off()
	
	
	
	
	# graph_multi_item_after3_weekday1234
			setwd("/Users/alan/Desktop/item_graphics/")
			pdf('after3_weekday1234_c5.pdf', bg = "white")	
			
		locationlist <- unique(cluster1$locationid)			
		vec_color_exist <- rgb(runif(300),runif(300),runif(300))	
		vec_color <- c()
		count_color <- 1		
		vec_location <- c()	
		
		for(i in locationlist){
			LocationID = i
			data_location<-subset(cluster1,locationid==LocationID)		
			data_location<-subset(data_location,credit_1_cash_0 == 0)
			data_location<-subset(data_location,orderitem == 0)
			data_location<-subset(data_location,customerid_0_visitor!=0)
			data_location<-subset(data_location,dayofweek  < 5 )
			data_location<-subset(data_location,hourofday > 15)
			data_length <- length(data_location$orderid)
				 
			if(data_length > 100){		
				vec_price <- c()	
				vec_ratio <- c()	
				for(i in seq(4,20,0.1)){
					price <- (algorithm_numb_item(i)[1])	
					prob_more_item <- (algorithm_numb_item(i)[2])				
					vec_price <- c(vec_price,price)
					vec_ratio <- c(vec_ratio,prob_more_item)
				}
					
				data <- data.frame(vec_price,vec_ratio)
				current_color = vec_color_exist[count_color]			
				vec_color <- c(vec_color,current_color)	
				vec_location <- c(vec_location,paste(LocationID,data_length, sep=":"))
				
				if(count_color==1){
					plot(data, type="l",ylim=c(0,1),col=current_color,pch=2)
				}
				else{
					lines(data, type="l",ylim=c(0,1),col=current_color,pch=2)
				}
				abline(v=seq(0,18,1), col="grey", lty=1)
				count_color = count_color+1
				
			}else{
				print(c(LocationID,"no data"))
			}
		
		}

		legend("topleft", as.character(vec_location), fill=vec_color)
	
	dev.off()
	
	
	
	#graph_multi_item_before3_weekend567
			setwd("/Users/alan/Desktop/item_graphics/")
			pdf('before3_weekend567_c5.pdf', bg = "white")	
			
		locationlist <- unique(cluster1$locationid)			
		vec_color_exist <- rgb(runif(300),runif(300),runif(300))	
		vec_color <- c()
		count_color <- 1		
		vec_location <- c()	
		
		for(i in locationlist){
			LocationID = i
			data_location<-subset(cluster1,locationid==LocationID)		
			data_location<-subset(data_location,credit_1_cash_0 == 0)
			data_location<-subset(data_location,orderitem == 0)
			data_location<-subset(data_location,customerid_0_visitor!=0)
			data_location<-subset(data_location,dayofweek  > 4 )
			data_location<-subset(data_location,hourofday < 15)
			data_length <- length(data_location$orderid)
				 
			if(data_length > 100){		
				vec_price <- c()	
				vec_ratio <- c()	
				for(i in seq(4,20,0.1)){
					price <- (algorithm_numb_item(i)[1])	
					prob_more_item <- (algorithm_numb_item(i)[2])				
					vec_price <- c(vec_price,price)
					vec_ratio <- c(vec_ratio,prob_more_item)
				}
					
				data <- data.frame(vec_price,vec_ratio)
				current_color = vec_color_exist[count_color]			
				vec_color <- c(vec_color,current_color)	
				vec_location <- c(vec_location,paste(LocationID,data_length, sep=":"))
				
				if(count_color==1){
					plot(data, type="l",ylim=c(0,1),col=current_color,pch=2)
				}
				else{
					lines(data, type="l",ylim=c(0,1),col=current_color,pch=2)
				}
				abline(v=seq(0,18,1), col="grey", lty=1)
				count_color = count_color+1
				
			}else{
				print(c(LocationID,"no data"))
			}
		
		}

		legend("topleft", as.character(vec_location), fill=vec_color)
	
	dev.off()
	
	
	
	# graph_multi_item_after3_weekend567
			setwd("/Users/alan/Desktop/item_graphics/")
			pdf('after3_weekend567_c5.pdf', bg = "white")	
	
		locationlist <- unique(cluster1$locationid)			
		vec_color_exist <- rgb(runif(300),runif(300),runif(300))	
		vec_color <- c()
		count_color <- 1		
		vec_location <- c()	
		
		for(i in locationlist){
			LocationID = i
			data_location<-subset(cluster1,locationid==LocationID)		
			data_location<-subset(data_location,credit_1_cash_0 == 0)
			data_location<-subset(data_location,orderitem == 0)
			data_location<-subset(data_location,customerid_0_visitor!=0)
			data_location<-subset(data_location,dayofweek  > 4 )
			data_location<-subset(data_location,hourofday > 15)
			data_length <- length(data_location$orderid)
				 
			if(data_length > 100){		
				vec_price <- c()	
				vec_ratio <- c()	
				for(i in seq(4,20,0.1)){
					price <- (algorithm_numb_item(i)[1])	
					prob_more_item <- (algorithm_numb_item(i)[2])				
					vec_price <- c(vec_price,price)
					vec_ratio <- c(vec_ratio,prob_more_item)
				}
					
				data <- data.frame(vec_price,vec_ratio)
				current_color = vec_color_exist[count_color]			
				vec_color <- c(vec_color,current_color)	
				vec_location <- c(vec_location,paste(LocationID,data_length, sep=":"))
				
				if(count_color==1){
					plot(data, type="l",ylim=c(0,1),col=current_color,pch=2)
				}
				else{
					lines(data, type="l",ylim=c(0,1),col=current_color,pch=2)
				}
				abline(v=seq(0,18,1), col="grey", lty=1)
				count_color = count_color+1
				
			}else{
				print(c(LocationID,"no data"))
			}
		
		}

		legend("topleft", as.character(vec_location), fill=vec_color)
			
	dev.off()
		
	
	######################################################################################################
	######################################################################################################
	######################################################################################################
	######################################################################################################
	######################################################################################################
	
			
		algorithm_numb_item<-function(price){
		
			data_location_lowerprice <- subset(data_location,total < price)
			total_number_row_lowerprice <- nrow(data_location_lowerprice)
			# print(total_number_row_lowerprice)
			data_location_lowerprice_moreitem <- subset(data_location_lowerprice,numb_item_no_restrict>1)
			total_number_row_lowerprice_moreitem <- nrow(data_location_lowerprice_moreitem)
			# print(total_number_row_lowerprice_moreitem)
			prob_moreitem <- total_number_row_lowerprice_moreitem/total_number_row_lowerprice
			#vec_price <- c(vec_price,price)
			#vec_ratio <- c(vec_ratio,prob_moreitem)
			print(c(price,total_number_row_lowerprice_moreitem/total_number_row_lowerprice))
			
			return (c(price,total_number_row_lowerprice_moreitem/total_number_row_lowerprice))
			
		}
		
	######################################################################################################
	######################################################################################################
	######################################################################################################
	######################################################################################################
		
	#regression_quad(vec_ratio,vec_price)
			
			
			
		regression_quad<-function(vec_ratio,vec_price){	
	
			vec_price_sq = vec_price^2		
			vec_price_tri = vec_price^3
			vec_price_qual = vec_price^4
			vec_price_five = vec_price^5
			vec_price_six = vec_price^6
			vec_price_seven = vec_price^7
			
			
			model_cut <- lm( vec_ratio ~ vec_price + vec_price_sq + vec_price_tri + vec_price_qual + vec_price_five + vec_price_six + vec_price_seven) 
			r_square <- round( summary(model_cut)$r.squared ,3 ) 
		
			model_cut$coefficients[1] # Intercept
			model_cut$coefficients[2] # vec_price
			model_cut$coefficients[3] # vec_price^2
			model_cut$coefficients[4] # vec_price^3
			model_cut$coefficients[5] # vec_price^4
			model_cut$coefficients[6] # vec_price^5
			model_cut$coefficients[7] # vec_price^6
			model_cut$coefficients[8] # vec_price^7
			
			
			
			value_vector <- c()	
			min_i <- 100
			min_value <- 100
			for(i in seq(0,18,0.1)){
			
				Intercept      <- model_cut$coefficients[1]
				vec_price      <- model_cut$coefficients[2]
				vec_price_sq   <- model_cut$coefficients[3]
				vec_price_tri  <- model_cut$coefficients[4]
				vec_price_qual <- model_cut$coefficients[5]
				vec_price_five <- model_cut$coefficients[6]
				vec_price_six  <- model_cut$coefficients[7]
				vec_price_seven<- model_cut$coefficients[8]
			
			
				value <- Intercept + vec_price * i + vec_price_sq * i^2 + vec_price_tri * i^3 + vec_price_qual * i^4 + vec_price_five * i^5 + vec_price_six * i^6 + vec_price_seven * i^7;
				value_vector <- c(value_vector,value)
			
				if(i>6){
					if(value<min_value){
						min_value <- value
						min_i <-i
					}		
				}
			}
		
			lines(seq(0,18,0.1),value_vector)
			return(paste(min_i,r_square,"  "))	
				
		}	
			
	
	######################################################################################################
	######################################################################################################
	# This is to graph the cluster trend, NOT on restaurant level anymore
	######################################################################################################
    ######################################################################################################
	# change the fitting point to the lowess
	# need to find a way to plug in a value to output another value, we shift it and use the point minus the previous point
	####################################################################################################################################
	# Totally, I have 200 points, by using 0.025, I am using 5 points to do each regression
	# 0.015, 3 points, local sliding window, similar to professor erfle talking about
		
	#	144
    #	193
    #	194
    #	302
    #	377
    # 	481
		
	######################################################################################################
	######################################################################################################
	######################################################################################################	
	##########################################################################################################
	cluster_numb = 5
	setwd("/Users/alan/Desktop/item_graphics/")
	pdf(paste('C',cluster_numb,'_cash.pdf',sep=""), bg = "white")	
		
	order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
	cluster1<-read.csv(file=paste("/Users/alan/Desktop/data_reference/cluster",cluster_numb,'.csv',sep=""),head=TRUE,sep=",")
	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)			
	numb_item_no_restrict_table <- read.csv(file="/Users/alan/Desktop/data_reference/numb_item_no_restrict_table.csv",head=TRUE,sep=",")
	cluster1<-merge(cluster1, numb_item_no_restrict_table, by="orderid")
	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
	#####################################################################
	large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
	cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
	cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	

	#################################################################################################
	# c2, weekday1234_lunch
	credit_1_cash_0_set = 1
	# 5 point (totally 200 points) , 0.1 for one point
	# 0.1 * 200 = 20 point, 2 is the window = (+-1)
	window_width = 0.1
			
			data_location<-cluster1
			data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
			data_location<-subset(data_location,orderitem == 0)
			data_location<-subset(data_location,large_pizza == 0)	
			data_location<-subset(data_location,customerid_0_visitor!=0)
			data_location<-subset(data_location,dayofweek  < 5 )
			data_location<-subset(data_location,hourofday < 15)			
					data_location<-subset(data_location,locationid != 144)
					data_location<-subset(data_location,locationid != 193)
					data_location<-subset(data_location,locationid != 194)
					data_location<-subset(data_location,locationid != 302)
					data_location<-subset(data_location,locationid != 377)
					data_location<-subset(data_location,locationid != 481)
				

			data_length_weekday1234_lunch <- length(data_location$orderid)

			if(data_length_weekday1234_lunch > 100){	
				vec_price <- c()	
				vec_ratio <- c()	
				for(i in seq(2,20,0.1)){
					price <- (algorithm_numb_item(i)[1])	
					prob_more_item <- (algorithm_numb_item(i)[2])
					vec_price <- c(vec_price,price)
					vec_ratio <- c(vec_ratio,prob_more_item)
						
				}

				data <- data.frame(vec_price,vec_ratio)

				plot(lowess(data$vec_price,data$vec_ratio,f=window_width), type="l",ylim=c(0,1),col="red",pch=2)
				# plot(data, type="l",ylim=c(0,1),col="red",pch=2)
				abline(v=seq(0,18,1), col="grey", lty=1)

			}else{
				print(c(LocationID,"no data"))
			}
			
			#rq_1<-regression_quad(vec_ratio,vec_price)
			
	#################################################################################################
	# c2, weekday1234_dinner
	
	# lowess			
				data_location<-cluster1
				data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
				data_location<-subset(data_location,orderitem == 0)
				data_location<-subset(data_location,large_pizza == 0)
				data_location<-subset(data_location,customerid_0_visitor!=0)
				data_location<-subset(data_location,dayofweek  < 5 )
				data_location<-subset(data_location,hourofday > 15)
						data_location<-subset(data_location,locationid != 144)
						data_location<-subset(data_location,locationid != 193)
						data_location<-subset(data_location,locationid != 194)
						data_location<-subset(data_location,locationid != 302)
						data_location<-subset(data_location,locationid != 377)
						data_location<-subset(data_location,locationid != 481)
				
				data_length_weekday1234_dinner <- length(data_location$orderid)

				if(data_length_weekday1234_dinner > 100){	
					vec_price <- c()	
					vec_ratio <- c()	
					for(i in seq(2,20,0.1)){
						price <- (algorithm_numb_item(i)[1])	
						prob_more_item <- (algorithm_numb_item(i)[2])
						vec_price <- c(vec_price,price)
						vec_ratio <- c(vec_ratio,prob_more_item)
						
					}

					data <- data.frame(vec_price,vec_ratio)

					lines(lowess(data$vec_price,data$vec_ratio,f=window_width), type="l",ylim=c(0,1),col="blue",pch=2)
					abline(v=seq(0,18,1), col="grey", lty=1)

				}else{
					print(c(LocationID,"no data"))
				}
			#rq_2<-regression_quad(vec_ratio,vec_price)

	#################################################################################################
	# c2, weekday567_lunch
				
					data_location<-cluster1
					data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
					data_location<-subset(data_location,orderitem == 0)
					data_location<-subset(data_location,large_pizza == 0)
					data_location<-subset(data_location,customerid_0_visitor!=0)
					data_location<-subset(data_location,dayofweek  > 4 )
					data_location<-subset(data_location,hourofday < 15)
					data_length_weekday567_lunch <- length(data_location$orderid)
					data_length_weekday1234_dinner <- length(data_location$orderid)
								data_location<-subset(data_location,locationid != 144)
								data_location<-subset(data_location,locationid != 193)
								data_location<-subset(data_location,locationid != 194)
								data_location<-subset(data_location,locationid != 302)
								data_location<-subset(data_location,locationid != 377)
								data_location<-subset(data_location,locationid != 481)
			
	
					if(data_length_weekday567_lunch > 100){	
						vec_price <- c()	
						vec_ratio <- c()	
						for(i in seq(2,20,0.1)){
							price <- (algorithm_numb_item(i)[1])	
							prob_more_item <- (algorithm_numb_item(i)[2])
							vec_price <- c(vec_price,price)
							vec_ratio <- c(vec_ratio,prob_more_item)
						
						}

						data <- data.frame(vec_price,vec_ratio)

						lines(lowess(data$vec_price,data$vec_ratio,f=window_width), type="l",ylim=c(0,1),col="green",pch=2)
						abline(v=seq(0,18,1), col="grey", lty=1)

					}else{
						print(c(LocationID,"no data"))
					}
			
			#rq_3<-regression_quad(vec_ratio,vec_price)
				
	
	#################################################################################################
	# c2, weekend567_dinner
				
					data_location<-cluster1
					data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
					data_location<-subset(data_location,orderitem == 0)
					data_location<-subset(data_location,large_pizza == 0)
					data_location<-subset(data_location,customerid_0_visitor!=0)
					data_location<-subset(data_location,dayofweek  > 4 )
					data_location<-subset(data_location,hourofday > 15)
					data_length_weekend567_dinner <- length(data_location$orderid)
								data_location<-subset(data_location,locationid != 144)
								data_location<-subset(data_location,locationid != 193)
								data_location<-subset(data_location,locationid != 194)
								data_location<-subset(data_location,locationid != 302)
								data_location<-subset(data_location,locationid != 377)
								data_location<-subset(data_location,locationid != 481)
			
					if(data_length_weekend567_dinner > 100){	
						vec_price <- c()	
						vec_ratio <- c()	
						for(i in seq(2,20,0.1)){
							price <- (algorithm_numb_item(i)[1])	
							prob_more_item <- (algorithm_numb_item(i)[2])
							vec_price <- c(vec_price,price)
							vec_ratio <- c(vec_ratio,prob_more_item)
					
						}

						data <- data.frame(vec_price,vec_ratio)

						lines(lowess(data$vec_price,data$vec_ratio,f=window_width), type="l",ylim=c(0,1),col="brown",pch=2)
						abline(v=seq(0,18,1), col="grey", lty=1)

					}else{
						print(c(LocationID,"no data"))
					}
			#rq_4<-regression_quad(vec_ratio,vec_price)

legend("topleft", c(paste("weekday1234_lunch",data_length_weekday1234_lunch,sep="  "),
					paste("weekday1234_dinner",data_length_weekday1234_dinner,sep="  "),
					paste("weekday567_lunch",data_length_weekday567_lunch,sep="  "),
					paste("weekend567_dinner",data_length_weekend567_dinner,sep="  ")), fill=c("red","blue","green","brown"))
	dev.off()
	
	######################################################################################
	######################################################################################
	######################################################################################
	######################################################################################
	# This is to graph the difference, not the graph itself
	######################################################################################
	setwd("/Users/alan/Desktop/item_graphics/")
	pdf(paste('C',cluster_numb,'_cash_dif.pdf',sep=""), bg = "white")	
			
	#	order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
	#	cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster2.csv",head=TRUE,sep=",")
	#	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)			
	#	numb_item_no_restrict_table <- read.csv(file="/Users/alan/Desktop/data_reference/numb_item_no_restrict_table.csv",head=TRUE,sep=",")
	#	cluster1<-merge(cluster1, numb_item_no_restrict_table, by="orderid")
	#	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
	#	#####################################################################
	#	large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
	#	cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
	#	cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	

		#################################################################################################
		# c2, weekday1234_lunch
		#credit_1_cash_0_set = 0
		# 5 point (totally 200 points) , 0.1 for one point
		# 0.1 * 200 = 20 point, 2 is the window = (+-1)
		window_width = 0.1

				data_location<-cluster1
				data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
				data_location<-subset(data_location,orderitem == 0)
				data_location<-subset(data_location,large_pizza == 0)	
				data_location<-subset(data_location,customerid_0_visitor!=0)
				data_location<-subset(data_location,dayofweek  < 5 )
				data_location<-subset(data_location,hourofday < 15)			
						data_location<-subset(data_location,locationid != 144)
						data_location<-subset(data_location,locationid != 193)
						data_location<-subset(data_location,locationid != 194)
						data_location<-subset(data_location,locationid != 302)
						data_location<-subset(data_location,locationid != 377)
						data_location<-subset(data_location,locationid != 481)


				data_length_weekday1234_lunch <- length(data_location$orderid)

				if(data_length_weekday1234_lunch > 100){	
					vec_price <- c()	
					vec_ratio <- c()	
					for(i in seq(2,20,0.1)){
						price <- (algorithm_numb_item(i)[1])	
						prob_more_item <- (algorithm_numb_item(i)[2])
						vec_price <- c(vec_price,price)
						vec_ratio <- c(vec_ratio,prob_more_item)
		
					}

					data <- data.frame(vec_price,vec_ratio)
					local_reg <- lowess(data$vec_price,data$vec_ratio,f=window_width)	
						
					before_point <- local_reg$y[-length(local_reg$y)]
				    after_point <- local_reg$y[-1]						
						
					dif_point = after_point - before_point
						#print(length(dif_point))
					dif_price = vec_price[!vec_price == vec_price[1]]
						#print(length(dif_price))
						
					data <- data.frame(dif_price,dif_point)
					plot(data, type="l",ylim=c(-0.05,0.05),col="red",pch=2,yaxs="i")
					abline(h =0, untf = FALSE)
																			
					# plot(data, type="l",ylim=c(0,1),col="red",pch=2)
					abline(v=seq(0,18,1), col="grey", lty=1)

				}else{
					print(c(LocationID,"no data"))
				}

				#rq_1<-regression_quad(vec_ratio,vec_price)

		#################################################################################################
		# c2, weekday1234_dinner

		# lowess			
					data_location<-cluster1
					data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
					data_location<-subset(data_location,orderitem == 0)
					data_location<-subset(data_location,large_pizza == 0)
					data_location<-subset(data_location,customerid_0_visitor!=0)
					data_location<-subset(data_location,dayofweek  < 5 )
					data_location<-subset(data_location,hourofday > 15)
							data_location<-subset(data_location,locationid != 144)
							data_location<-subset(data_location,locationid != 193)
							data_location<-subset(data_location,locationid != 194)
							data_location<-subset(data_location,locationid != 302)
							data_location<-subset(data_location,locationid != 377)
							data_location<-subset(data_location,locationid != 481)

					data_length_weekday1234_dinner <- length(data_location$orderid)

					if(data_length_weekday1234_dinner > 100){	
						vec_price <- c()	
						vec_ratio <- c()	
						for(i in seq(2,20,0.1)){
							price <- (algorithm_numb_item(i)[1])	
							prob_more_item <- (algorithm_numb_item(i)[2])
							vec_price <- c(vec_price,price)
							vec_ratio <- c(vec_ratio,prob_more_item)
		
						}

						data <- data.frame(vec_price,vec_ratio)
						local_reg <- lowess(data$vec_price,data$vec_ratio,f=window_width)	
						
						before_point <- local_reg$y[-length(local_reg$y)]
						after_point <- local_reg$y[-1]						
						
						dif_point = after_point - before_point
							#print(length(dif_point))
						dif_price = vec_price[!vec_price == vec_price[1]]
							#print(length(dif_price))
						
						data <- data.frame(dif_price,dif_point)
						lines(data, type="l",ylim=c(-0.05,0.05),col="blue",pch=2)
						abline(h =0, untf = FALSE)
						# plot(data, type="l",ylim=c(0,1),col="red",pch=2)
						abline(v=seq(0,18,1), col="grey", lty=1)
									
					}else{
						print(c(LocationID,"no data"))
					}
				#rq_2<-regression_quad(vec_ratio,vec_price)

		#################################################################################################
		# c2, weekday567_lunch

						data_location<-cluster1
						data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
						data_location<-subset(data_location,orderitem == 0)
						data_location<-subset(data_location,large_pizza == 0)
						data_location<-subset(data_location,customerid_0_visitor!=0)
						data_location<-subset(data_location,dayofweek  > 4 )
						data_location<-subset(data_location,hourofday < 15)
						data_length_weekday567_lunch <- length(data_location$orderid)
						data_length_weekday1234_dinner <- length(data_location$orderid)
									data_location<-subset(data_location,locationid != 144)
									data_location<-subset(data_location,locationid != 193)
									data_location<-subset(data_location,locationid != 194)
									data_location<-subset(data_location,locationid != 302)
									data_location<-subset(data_location,locationid != 377)
									data_location<-subset(data_location,locationid != 481)


						if(data_length_weekday567_lunch > 100){	
							vec_price <- c()	
							vec_ratio <- c()	
							for(i in seq(2,20,0.1)){
								price <- (algorithm_numb_item(i)[1])	
								prob_more_item <- (algorithm_numb_item(i)[2])
								vec_price <- c(vec_price,price)
								vec_ratio <- c(vec_ratio,prob_more_item)
		
							}

							data <- data.frame(vec_price,vec_ratio)
							local_reg <- lowess(data$vec_price,data$vec_ratio,f=window_width)	
							
							before_point <- local_reg$y[-length(local_reg$y)]
							after_point <- local_reg$y[-1]						
								
							dif_point = after_point - before_point
								#print(length(dif_point))
							dif_price = vec_price[!vec_price == vec_price[1]]
								#print(length(dif_price))
						
							data <- data.frame(dif_price,dif_point)
							lines(data, type="l",ylim=c(-0.05,0.05),col="green",pch=2)
							abline(h =0, untf = FALSE)
							# plot(data, type="l",ylim=c(0,1),col="red",pch=2)
							abline(v=seq(0,18,1), col="grey", lty=1)
							
						}else{
							print(c(LocationID,"no data"))
						}

				#rq_3<-regression_quad(vec_ratio,vec_price)


		#################################################################################################
		# c2, weekend567_dinner

						data_location<-cluster1
						data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
						data_location<-subset(data_location,orderitem == 0)
						data_location<-subset(data_location,large_pizza == 0)
						data_location<-subset(data_location,customerid_0_visitor!=0)
						data_location<-subset(data_location,dayofweek  > 4 )
						data_location<-subset(data_location,hourofday > 15)
						data_length_weekend567_dinner <- length(data_location$orderid)
									data_location<-subset(data_location,locationid != 144)
									data_location<-subset(data_location,locationid != 193)
									data_location<-subset(data_location,locationid != 194)
									data_location<-subset(data_location,locationid != 302)
									data_location<-subset(data_location,locationid != 377)
									data_location<-subset(data_location,locationid != 481)

						if(data_length_weekend567_dinner > 100){	
							vec_price <- c()	
							vec_ratio <- c()	
							for(i in seq(2,20,0.1)){
								price <- (algorithm_numb_item(i)[1])	
								prob_more_item <- (algorithm_numb_item(i)[2])
								vec_price <- c(vec_price,price)
								vec_ratio <- c(vec_ratio,prob_more_item)
	
							}

							data <- data.frame(vec_price,vec_ratio)
							local_reg <- lowess(data$vec_price,data$vec_ratio,f=window_width)	
						
							before_point <- local_reg$y[-length(local_reg$y)]
							after_point <- local_reg$y[-1]						
						
							dif_point = after_point - before_point
								#print(length(dif_point))
							dif_price = vec_price[!vec_price == vec_price[1]]
								#print(length(dif_price))
						
							data <- data.frame(dif_price,dif_point)
							lines(data, type="l",ylim=c(-0.05,0.05),col="brown",pch=2)
							abline(h =0, untf = FALSE)
							# plot(data, type="l",ylim=c(0,1),col="red",pch=2)
							abline(v=seq(0,18,1), col="grey", lty=1)
							
						}else{
							print(c(LocationID,"no data"))
						}
				#rq_4<-regression_quad(vec_ratio,vec_price)

	legend("topleft", c(paste("weekday1234_lunch",data_length_weekday1234_lunch,sep="  "),
						paste("weekday1234_dinner",data_length_weekday1234_dinner,sep="  "),
						paste("weekday567_lunch",data_length_weekday567_lunch,sep="  "),
						paste("weekend567_dinner",data_length_weekend567_dinner,sep="  ")), fill=c("red","blue","green","brown"))					
	dev.off()
	
	
	######################################################################
	######################################################################
	######################################################################							
	######################################################################
	######################################################################
	######################################################################
	######################################################################
	######################################################################
	######################################################################							
	######################################################################
	######################################################################
	######################################################################
							
	# 302
																
		order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
		cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster5.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)			
		numb_item_no_restrict_table<-read.csv(file="/Users/alan/Desktop/data_reference/numb_item_no_restrict_table.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, numb_item_no_restrict_table, by="orderid")
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
		
		cluster1<-subset(cluster1,total < 31)	
		cluster1<-subset(cluster1,orderitem == 0)
		cluster1<-subset(cluster1,large_pizza == 0)
		cluster1<-subset(cluster1,customerid_0_visitor!=0)
		
		cluster1<-subset(cluster1,locationid!=144)	
		cluster1<-subset(cluster1,locationid!=193)	
		cluster1<-subset(cluster1,locationid!=194)	
		cluster1<-subset(cluster1,locationid!=302)	
		cluster1<-subset(cluster1,locationid!=377)	
		cluster1<-subset(cluster1,locationid!=481)	
			
		cluster1$is_weekend <- floor(cluster1$dayofweek/4)
		cluster1$is_dinner <- floor(cluster1$hourofday/15)
		cluster1$numb_item_no_restrict <- cluster1$numb_item_no_restrict - 1
		cluster1$numb_item_no_restrict[which(cluster1$numb_item_no_restrict>=1)]<-1
		
		cluster1["numb_item_1"] <- NA
		cluster1$numb_item_1[which(floor(cluster1$total)==1 | floor(cluster1$total)==11 | floor(cluster1$total)==21 | floor(cluster1$total)==31)]<-1
		cluster1$numb_item_1[is.na(cluster1$numb_item_1)] <- 0		
		cluster1$numb_item_1
		
		cluster1["numb_item_2"] <- NA
		cluster1$numb_item_2[which(floor(cluster1$total)==2 | floor(cluster1$total)==12 | floor(cluster1$total)==22 | floor(cluster1$total)==32)]<-1
		cluster1$numb_item_2[is.na(cluster1$numb_item_2)] <- 0		
		cluster1$numb_item_2
		
		cluster1["numb_item_3"] <- NA
		cluster1$numb_item_3[which(floor(cluster1$total)==3 | floor(cluster1$total)==13 | floor(cluster1$total)==23 | floor(cluster1$total)==33)]<-1
		cluster1$numb_item_3[is.na(cluster1$numb_item_3)] <- 0		
		cluster1$numb_item_3
		
		cluster1["numb_item_4"] <- NA
		cluster1$numb_item_4[which(floor(cluster1$total)==4 | floor(cluster1$total)==14 | floor(cluster1$total)==24 | floor(cluster1$total)==34)]<-1
		cluster1$numb_item_4[is.na(cluster1$numb_item_4)] <- 0	
		cluster1$numb_item_4
			
		cluster1["numb_item_5"] <- NA
		cluster1$numb_item_5[which(floor(cluster1$total)==5 | floor(cluster1$total)==15 | floor(cluster1$total)==25 | floor(cluster1$total)==35)]<-1
		cluster1$numb_item_5[is.na(cluster1$numb_item_5)] <- 0		
		cluster1$numb_item_5
		
		cluster1["numb_item_6"] <- NA
		cluster1$numb_item_6[which(floor(cluster1$total)==6 | floor(cluster1$total)==16 | floor(cluster1$total)==26 | floor(cluster1$total)==36)]<-1
		cluster1$numb_item_6[is.na(cluster1$numb_item_6)] <- 0		
		cluster1$numb_item_6
			
		cluster1["numb_item_7"] <- NA
		cluster1$numb_item_7[which(floor(cluster1$total)==7 | floor(cluster1$total)==17 | floor(cluster1$total)==27 | floor(cluster1$total)==37)]<-1
		cluster1$numb_item_7[is.na(cluster1$numb_item_7)] <- 0		
		cluster1$numb_item_7
			
		cluster1["numb_item_8"] <- NA
		cluster1$numb_item_8[which(floor(cluster1$total)==8 | floor(cluster1$total)==18 | floor(cluster1$total)==28 | floor(cluster1$total)==38)]<-1
		cluster1$numb_item_8[is.na(cluster1$numb_item_8)] <- 0		
		cluster1$numb_item_8
			
		cluster1["numb_item_9"] <- NA
		cluster1$numb_item_9[which(floor(cluster1$total)==9 | floor(cluster1$total)==19 | floor(cluster1$total)==29 | floor(cluster1$total)==39)]<-1
		cluster1$numb_item_9[is.na(cluster1$numb_item_9)] <- 0	
		cluster1$numb_item_9
				
		cluster1["numb_item_10"] <- NA
		cluster1$numb_item_10[which(floor(cluster1$total)==10 | floor(cluster1$total)==20 | floor(cluster1$total)==30 | floor(cluster1$total)==40)]<-1
		cluster1$numb_item_10[is.na(cluster1$numb_item_10)] <- 0		
		cluster1$numb_item_10
		
		focus_flag     <- cluster1$focus_flag
		focus_flag_sq  <- (cluster1$focus_flag)^2	
		number_item    <- cluster1$numb_item_no_restrict
		number_item_sq <- number_item^2
		total          <- (cluster1$total)
		total_sq       <- (total)^2
		total_tri      <- (total)^3
		credit         <- cluster1$credit_1_cash_0
		delivery       <- cluster1$deli_1_takeout_0
		dinner         <- cluster1$is_dinner
		weekend        <- cluster1$is_weekend	
		
		focus_flag_1   <- cluster1$numb_item_1
		focus_flag_2   <- cluster1$numb_item_2
		focus_flag_3   <- cluster1$numb_item_3
		focus_flag_4   <- cluster1$numb_item_4
		focus_flag_5   <- cluster1$numb_item_5
				
		focus_flag_6   <- cluster1$numb_item_6
		focus_flag_7   <- cluster1$numb_item_7
		focus_flag_8   <- cluster1$numb_item_8
		focus_flag_9   <- cluster1$numb_item_9
		focus_flag_10  <- cluster1$numb_item_10
					
		#model_cut <- lm(number_item ~ focus_flag  + credit + delivery + total + total_sq)
			
		########################################################################################	
		#logistic regression
				
		model_glm<-glm(number_item ~ focus_flag  + delivery + total + total_sq + total_tri + credit + total * credit, family=binomial) 
		model_glm<-glm(number_item ~ focus_flag  + delivery + total + credit , family=binomial) 	
			
		summary(model_glm)
			
		model_glm<-glm(number_item ~ focus_flag_6 + focus_flag_7 + focus_flag_8 + focus_flag_9 + focus_flag_10 + delivery + total + credit + total * credit, family=binomial) 	
		summary(model_glm)
		
		model_glm<-glm(number_item ~ focus_flag_1 + focus_flag_2 + focus_flag_3 + focus_flag_4 + focus_flag_5  + focus_flag_6 + focus_flag_7 + focus_flag_8 + focus_flag_9 + focus_flag_10 + delivery + total + credit + total * credit, family=binomial) 	
		summary(model_glm)
		
		model_glm<-glm(number_item ~  focus_flag_1 + focus_flag_3 + focus_flag_4+ focus_flag_5  + focus_flag_6 + focus_flag_7 + focus_flag_8 + focus_flag_9 + focus_flag_10 + delivery + total + credit, family=binomial) 	
		
		summary(model_glm)
		///////////////////////////////////////////////////////////////////////////////
		// Cluster 2
		
		glm(formula = number_item ~ focus_flag + credit + delivery + 
		    total, family = binomial)

		Deviance Residuals: 
		    Min       1Q   Median       3Q      Max  
		-2.4851  -0.8425   0.5319   0.7307   1.9728  

		Coefficients:
		             Estimate Std. Error z value Pr(>|z|)    
		(Intercept) -1.520933   0.032559 -46.713  < 2e-16 ***
		focus_flag  -0.198321   0.027123  -7.312 2.63e-13 ***
		credit      -0.270961   0.025857 -10.479  < 2e-16 ***
		delivery     0.942852   0.026787  35.198  < 2e-16 ***
		total        0.123903   0.001981  62.536  < 2e-16 ***
		
		p = 1/ (1+ exp(-( -1.52 + 0.19 * focus_flag + -0.27 * credit + 0.94 * delivery + 0.12 * total ) )  )
		////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		// When the chance of focus_flag increase by 1,  the change of having more than two item will decrease with a factor of 0.82
		> exp(1*-0.198321)
		[1] 0.8201066
		
		///////////////////////////////////////////////////////////////////////////////
		// Cluster 3
		Coefficients:
		             Estimate Std. Error z value Pr(>|z|)    
		(Intercept) -2.188008   0.030485  -71.77   <2e-16 ***
		focus_flag  -0.470889   0.018717  -25.16   <2e-16 ***
		delivery     0.559303   0.020556   27.21   <2e-16 ***
		total        0.148855   0.001693   87.95   <2e-16 ***							
									
		///////////////////////////////////////////////////////////////////////////////
		// Cluster 4				
		Coefficients:
		            Estimate Std. Error z value Pr(>|z|)    
		(Intercept) -3.44758    0.06517 -52.900  < 2e-16 ***
		focus_flag  -0.50167    0.04801 -10.450  < 2e-16 ***
		delivery     0.24739    0.07584   3.262  0.00111 ** 
		total        0.28281    0.00509  55.560  < 2e-16 ***
		credit      -0.21269    0.04879  -4.359  1.3e-05 ***					
		
		///////////////////////////////////////////////////////////////////////////////
		// Cluster 5
									
		Coefficients:
		             Estimate Std. Error z value Pr(>|z|)    
		(Intercept) -2.618718   0.173268 -15.114  < 2e-16 ***
		focus_flag  -0.294452   0.082108  -3.586 0.000336 ***
		total        0.147774   0.005984  24.696  < 2e-16 ***
		credit      -0.444124   0.151573  -2.930 0.003388 ** 
		
		
		
		
		
		###########################################################################################
		###########################################################################################
		###########################################################################################
		###########################################################################################
			
			
		setwd("/Users/alan/Desktop/data_date/")
	
		order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
		bandwith<-0.5
	
		pdf('dinner_weekend_c2_3.pdf', bg = "white")		
		c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c2_menu.csv",head=TRUE,sep=",")			
		cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster2.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
		###########################################################################################	
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
		###########################################################################################	
		graph_individual_novisitor_after3_weekend567_date(bandwith)
		dev.off()
	
		pdf('dinner_weekend_c3_3.pdf', bg = "white")		
		c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c3_menu.csv",head=TRUE,sep=",")			
		cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster3.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
		###########################################################################################	
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
		###########################################################################################	
		graph_individual_novisitor_after3_weekend567_date(bandwith)
		dev.off()
	
		pdf('dinner_weekend_c4_3.pdf', bg = "white")		
		c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c4_menu.csv",head=TRUE,sep=",")			
		cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster4.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
		###########################################################################################	
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
		###########################################################################################	
		graph_individual_novisitor_after3_weekend567_date(bandwith)
		dev.off()
	
		pdf('dinner_weekend_c5_3.pdf', bg = "white")		
		c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c5_menu.csv",head=TRUE,sep=",")			
		cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster5.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
		###########################################################################################	
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
		###########################################################################################	
		graph_individual_novisitor_after3_weekend567_date(bandwith)
		dev.off()

	
	graph_individual_novisitor_after3_weekend567_date<-function(bandwith){
		###################################################################
		# graph menu information	
		data_menu <- c1_menu["price"][c1_menu["price"]<50 & c1_menu["price"]>0]
		numb_menu <- length(data_menu)
		plot(density(data_menu,bw=bandwith,kernel="gaussian"),col='grey',ylim=c(0,0.2))
		###################################################################
		# delivery			
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 				cluster1["large_pizza"]==0 & 	
							 cluster1["large_pizza"]==0 & 
					         cluster1["deli_1_takeout_0"]==1 & 
							 cluster1["orderitem"] == 3 & 
							 cluster1["customerid_0_visitor"]!=0 &
			  				 cluster1["dayofweek"] > 4 &  
							 cluster1["hourofday"] > 15]							 
		# bandwith <- bw.nrd0(data) 
		# print(bandwith)
		numb_delivery <- length(data)
		if(numb_delivery>500){
			lines(density(data,bw=bandwith,kernel="gaussian"),col='blue')
		}

		
	    ###################################################################
	    # takeout	
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 		cluster1["large_pizza"]==0 & 		
						  cluster1["deli_1_takeout_0"]==0 & 
						  cluster1["orderitem"] == 3 & 
						  cluster1["customerid_0_visitor"]!=0 & 
						  cluster1["dayofweek"] > 4 &  
						  cluster1["hourofday"] > 15]				  
		# bandwith <- bw.nrd0(data)				  	
		# print(bandwith)  
		numb_takeout <- length(data)
		if(numb_takeout>500){
		lines(density(data,bw=bandwith,kernel="gaussian"),col='green')  
		}	
		##############################################################################											
		# credit
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 		cluster1["large_pizza"]==0 & 		
					  cluster1["credit_1_cash_0"]==1 & 
					  cluster1["orderitem"] == 3 & 
					  cluster1["customerid_0_visitor"]!=0 & 						  
					  cluster1["dayofweek"]  > 4 & 
					  cluster1["hourofday"] > 15]	
		# bandwith <- bw.nrd0(data)
		# print(bandwith)
		numb_credit <- length(data)	
		if(numb_credit > 500){
			lines(density(data, bw=bandwith,kernel="gaussian"),col='orange')  
		}
		
											  	    
		##############################################################################																
		# cash	
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 		cluster1["large_pizza"]==0 & 	
					  cluster1["credit_1_cash_0"]==0 & 
					  cluster1["orderitem"] == 3 & 
					  cluster1["customerid_0_visitor"]!=0 & 						  
					  cluster1["dayofweek"]  > 4 &  
					  cluster1["hourofday"] > 15]
		# bandwith <- bw.nrd0(data)
	    # print(bandwith)
		numb_cash <- length(data)				  
		if(numb_cash > 500){
			lines(density(data,bw=bandwith,kernel="gaussian"),col='purple')  
		}

		##############################################################################																
		# with discount
		data <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
			 		cluster1["large_pizza"]==0 & 
					  cluster1["is_discount"]==1 & 
					  cluster1["orderitem"] == 3 & 																  
					  cluster1["customerid_0_visitor"]!=0 & 
			  		  cluster1["dayofweek"]  > 4 & 
					  cluster1["hourofday"] > 15]
		# bandwith <- bw.nrd0(data)
		# print(bandwith)	
		numb_discount <- length(data)
		if(numb_discount > 500){
			lines(density(data, bw=bandwith,kernel="gaussian"),col='brown')  	 
		}	
																					 	###################################################################
		abline(v=0:50, col="gray", lty=3)		
		legend("topright", c(paste("delivery",numb_delivery),
							 paste("takeout",numb_takeout),
							 paste("credit",numb_credit),
							 paste("cash",numb_cash),
							 paste("discount",numb_discount), 
							 paste("menu_price",numb_menu)), 
		fill=c("blue",    "green", "orange","purple","brown","grey"))
	
		
	}		
		






		#####################################################################
		#####################################################################
		#####################################################################
							
		order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
		cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster2.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)			
		numb_item_no_restrict_table <- read.csv(file="/Users/alan/Desktop/data_reference/numb_item_no_restrict_table.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, numb_item_no_restrict_table, by="orderid")
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0			
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	

		if(data_length_weekday1234_lunch > 100){	
			vec_price <- c()	
			vec_ratio <- c()	
			for(i in seq(4,20,0.1)){
				price <- (algorithm_numb_item(i)[1])	
				prob_more_item <- (algorithm_numb_item(i)[2])
				vec_price <- c(vec_price,price)
				vec_ratio <- c(vec_ratio,prob_more_item)
				
			}

			data <- data.frame(vec_price,vec_ratio)

			# plot(data, type="l",ylim=c(0,1),col="red",pch=2)
			abline(v=seq(0,18,1), col="grey", lty=1)

		}else{
			print(c(LocationID,"no data"))
		}
		
		regression_quad(vec_ratio,vec_price)
		
		
		
		
		
		regression_quad<-function(vec_ratio,vec_price){	
	
			vec_price_sq = vec_price^2		
			vec_price_tri = vec_price^3
			model_cut <- lm( vec_ratio ~ vec_price + vec_price_sq + vec_price_tri) 
			summary(model_cut)$r.squared 
		
			model_cut$coefficients[1] # Intercept
			model_cut$coefficients[2] # vec_price
			model_cut$coefficients[3] # vec_price^2
			model_cut$coefficients[4] # vec_price^3
			
			value_vector <- c()	
			min_i <- 100
			min_value <- 100
			for(i in seq(0,18,0.1)){
			
				Intercept     <- model_cut$coefficients[1]
				vec_price     <- model_cut$coefficients[2]
				vec_price_sq  <- model_cut$coefficients[3]
				vec_price_tri <- model_cut$coefficients[4]
			
				value <- Intercept + vec_price * i + vec_price_sq * i^2 + vec_price_tri * i^3;
				value_vector <- c(value_vector,value)
			
				if(value<min_value){
					min_value <- value
					min_i <-i
				}		
			
			}
		
			plot(seq(0,18,0.1),value_vector)
			min_i
				
		
				
		}	
			
			
			
	#####################################################################################		
	#	The diffence between two density graph
	#####################################################################################
	# Here I am able to put graph together, however, I do not know how to use statistics to compare the bump between two graph, since both are density 
	#
			
	
			
			setwd("/Users/alan/Desktop/data_graphics/")
	
			order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
			bandwith<-0.5
	
			pdf('dinner_weekend_c2.pdf', bg = "white")		
			c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c2_menu.csv",head=TRUE,sep=",")			
			cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster2.csv",head=TRUE,sep=",")
			cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
			cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
			###########################################################################################	
			large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
			cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
			cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
			###########################################################################################	
			cluster1$total[is.na(cluster1$total)] <- 0		
			graph_individual_novisitor_after3_weekend567(bandwith)
			dev.off()
	
			pdf('dinner_weekend_c3.pdf', bg = "white")		
			c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c3_menu.csv",head=TRUE,sep=",")			
			cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster3.csv",head=TRUE,sep=",")
			cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
			cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
			###########################################################################################	
			large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
			cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
			cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
			###########################################################################################	
			graph_individual_novisitor_after3_weekend567(bandwith)
			dev.off()
	
			pdf('dinner_weekend_c4.pdf', bg = "white")		
			c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c4_menu.csv",head=TRUE,sep=",")			
			cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster4.csv",head=TRUE,sep=",")
			cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
			cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
			###########################################################################################	
			large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
			cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
			cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
			###########################################################################################	
			graph_individual_novisitor_after3_weekend567(bandwith)
			dev.off()
	
			pdf('dinner_weekend_c5.pdf', bg = "white")		
			c1_menu <- read.csv(file="/Users/alan/Desktop/data_reference/c5_menu.csv",head=TRUE,sep=",")			
			cluster1<-read.csv(file="/Users/alan/Desktop/data_reference/cluster5.csv",head=TRUE,sep=",")
			cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)
			cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
			###########################################################################################	
			large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
			cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
			cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
			###########################################################################################	
			graph_individual_novisitor_after3_weekend567(bandwith)
			dev.off()

	
		graph_individual_novisitor_after3_weekend567<-function(bandwith){
			###################################################################
			# graph menu information	
			data_menu <- c1_menu["price"][c1_menu["price"]<50 & c1_menu["price"]>0]
			numb_menu <- length(data_menu)
			plot(density(data_menu,bw=bandwith,kernel="gaussian"),col='grey',ylim=c(0,0.2))
			###################################################################
			# delivery			
			data_567 <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 	
								 cluster1["large_pizza"]==0 & 
						         cluster1["deli_1_takeout_0"]==1 & 
								 cluster1["orderitem"] == 0 & 
								 cluster1["customerid_0_visitor"]!=0 &
				  				 cluster1["dayofweek"] > 4 &  
								 cluster1["hourofday"] > 15]	
									 	
 			data_1234 <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
 				 				cluster1["large_pizza"]==0 & 	
 								 cluster1["large_pizza"]==0 & 
 						         cluster1["deli_1_takeout_0"]==1 & 
 								 cluster1["orderitem"] == 0 & 
 								 cluster1["customerid_0_visitor"]!=0 &
 				  				 cluster1["dayofweek"] < 5 &  
 								 cluster1["hourofday"] > 15]	
														  					 
			# bandwith <- bw.nrd0(data) 
			# print(bandwith)
			#lines(density(data_567,bw=0.5,kernel="gaussian"),col='blue')
			#lines(density(data_1234,bw=0.5,kernel="gaussian"),col='blue')
														  
			#density_difference <- density(data_567,bw=0.5,kernel="gaussian")$y-density(data_1234,bw=0.5,kernel="gaussian")$y
														  								  
			numb_delivery <- length(data)
			if(numb_delivery>500){
				lines(density(data_567,bw=0.5,kernel="gaussian"),col='blue')
				lines(density(data_1234,bw=0.5,kernel="gaussian"),col='blue')
				
				#lines(density_difference ,col='blue')
			}

		
		    ###################################################################
		    # takeout	
			data_567 <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
				 		cluster1["large_pizza"]==0 & 		
							  cluster1["deli_1_takeout_0"]==0 & 
							  cluster1["orderitem"] == 0 & 
							  cluster1["customerid_0_visitor"]!=0 & 
							  cluster1["dayofweek"] > 4 &  
							  cluster1["hourofday"] > 15]	
								  
  			data_1234 <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
  				 		cluster1["large_pizza"]==0 & 		
  							  cluster1["deli_1_takeout_0"]==0 & 
  							  cluster1["orderitem"] == 0 & 
  							  cluster1["customerid_0_visitor"]!=0 & 
  							  cluster1["dayofweek"] < 5 &  
  							  cluster1["hourofday"] > 15]				  
			# bandwith <- bw.nrd0(data)				  	
			# print(bandwith)  
			density_difference <- density(data_567,bw=0.2,kernel="gaussian")$y-density(data_1234,bw=0.2,kernel="gaussian")$y
																							
			numb_takeout <- length(data)
			if(numb_takeout>500){
				lines(density(data_567,bw=0.5,kernel="gaussian"),col='green')
				lines(density(data_1234,bw=0.5,kernel="gaussian"),col='green')
			}	
			##############################################################################											
			# credit
			data_567 <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
				 		cluster1["large_pizza"]==0 & 		
						  cluster1["credit_1_cash_0"]==1 & 
						  cluster1["orderitem"] == 0 & 
						  cluster1["customerid_0_visitor"]!=0 & 						  
						  cluster1["dayofweek"]  > 4 & 
						  cluster1["hourofday"] > 15]	
							  
  			data_1234 <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
  				 		cluster1["large_pizza"]==0 & 		
  						  cluster1["credit_1_cash_0"]==1 & 
  						  cluster1["orderitem"] == 0 & 
  						  cluster1["customerid_0_visitor"]!=0 & 						  
  						  cluster1["dayofweek"]  < 5 & 
  						  cluster1["hourofday"] > 15]	
			# bandwith <- bw.nrd0(data)
			# print(bandwith)
			numb_credit <- length(data)
			density_difference <- density(data_567,bw=0.2,kernel="gaussian")$y-density(data_1234,bw=0.2,kernel="gaussian")$y
			
			if(numb_credit > 500){
				lines(density(data_567,bw=0.5,kernel="gaussian"),col='orange')
				lines(density(data_567,bw=0.5,kernel="gaussian"),col='orange')
			}
		
											  	    
			##############################################################################																
			# cash	
			data_567 <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
				 		cluster1["large_pizza"]==0 & 	
						  cluster1["credit_1_cash_0"]==0 & 
						  cluster1["orderitem"] == 0 & 
						  cluster1["customerid_0_visitor"]!=0 & 						  
						  cluster1["dayofweek"]  > 4 &  
						  cluster1["hourofday"] > 15]
							  
  			data_1234 <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
  				 		cluster1["large_pizza"]==0 & 	
  						  cluster1["credit_1_cash_0"]==0 & 
  						  cluster1["orderitem"] == 0 & 
  						  cluster1["customerid_0_visitor"]!=0 & 						  
  						  cluster1["dayofweek"]  < 5 &  
  						  cluster1["hourofday"] > 15]
			# bandwith <- bw.nrd0(data)
		    # print(bandwith)
			numb_cash <- length(data)
			#density_difference <- density(data_567,bw=0.2,kernel="gaussian")$y-density(data_1234,bw=0.2,kernel="gaussian")$y
															  
			if(numb_cash > 500){
				lines(density(data_567,bw=0.5,kernel="gaussian"),col='purple')
				lines(density(data_567,bw=0.5,kernel="gaussian"),col='purple')
					
			}

			##############################################################################																
			# with discount
			data_567 <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
				 		cluster1["large_pizza"]==0 & 
						  cluster1["is_discount"]==1 & 
						  cluster1["orderitem"] == 0 & 																  
						  cluster1["customerid_0_visitor"]!=0 & 
				  		  cluster1["dayofweek"]  > 4 & 
						  cluster1["hourofday"] > 15]
							  
  			data_1234 <- cluster1["total"][cluster1["total"]<50 & cluster1["total"]>0 & 
  				 		cluster1["large_pizza"]==0 & 
  						  cluster1["is_discount"]==1 & 
  						  cluster1["orderitem"] == 0 & 																  
  						  cluster1["customerid_0_visitor"]!=0 & 
  				  		  cluster1["dayofweek"]  < 5 & 
  						  cluster1["hourofday"] > 15]
			# bandwith <- bw.nrd0(data)
			# print(bandwith)	
			numb_discount <- length(data)
			density_difference <- density(data_567,bw=0.2,kernel="gaussian")$y-density(data_1234,bw=0.2,kernel="gaussian")$y
											
			if(numb_discount > 500){
				lines(density(data_567,bw=0.5,kernel="gaussian"),col='brown')
				lines(density(data_567,bw=0.5,kernel="gaussian"),col='brown')
				
			}	
			###################################################################
			abline(v=0:50, col="gray", lty=3)		
			legend("topright", c(paste("delivery",numb_delivery),
								 paste("takeout",numb_takeout),
								 paste("credit",numb_credit),
								 paste("cash",numb_cash),
								 paste("discount",numb_discount), 
								 paste("menu_price",numb_menu)), 
			fill=c("blue",    "green", "orange","purple","brown","grey"))
	
		
		}	
		
		
		
		
		
		lty = 3 type of line
		lwd =  line witdth
		
		
		
	###################################################################################################
	###################################################################################################	
	# pizza algorithm
	###################################################################################################
	###################################################################################################
			
			SELECT 
			    history_cs_orders.orderid,
			    item_name,
			    item_size,
			    subtotal
			FROM
			    history_cs_orders,
			    (
				    SELECT 
				        *
				    FROM 
				        history_items
				    WHERE
				    ((item_name like "%medium%"
				    or item_name like "%large%" 
				    or item_name like "%lg%"
				    or item_name like "%xl%"                                                            
				    or item_name like "%md%" 
				    )
				    OR
				    (item_size like "%medium%" 
				    or item_size like "%large%" 
				    or item_size like "%xl%"
				    or item_size like "%med%"
				    or item_size like "%md%" ))
				    and (item_name like "%pizza%"
				    or
				    item_name like "%Sicilian%")
			    ) as history_items_price
			WHERE
			    history_items_price.orderid = history_cs_orders.orderid
			GROUP BY
				history_cs_orders.orderid
		
		
		
		
			# GLMfunction
			GLMfunction function (M = 500)
			{
				# 500 data, have uniform distribution between 0 and 4
			    xpts <- runif(M, 0, 4)
			    xpts2 <- xpts^2
			    xpts3 <- xpts^3
				# print out the uniform distribution data
			    print(xpts)
				# use fun1, that quatratic function to plug every data point to the f(x) function	
			    reg1 <- fun1(xpts)
				# print the after plug into the function data
			    print(reg1)
				
			    probs <- round(exp(reg1)/(1 + exp(reg1)), 4)
			    data1 <- cbind(xpts, probs)
			    ## data1 <- SortM(data1, 1)
			    ## plot(data1[, 1], data1[, 2], pch = "")
				plot(data1)
			    ## lines(data1[, 1], data1[, 2])
				
				# print the probability 
			    print(probs)
					
				# random generation for the binomial distribution with parameters ‘size’ and ‘prob’
			    ypts <- 0
			    for (i in 1:M) {
			        ypts <- c(ypts, rbinom(1, 1, probs[i]))
			    }
				# remove the first variable
			    ypts <- ypts[-1]	
			    print(ypts)
					
					
			    model1 <- glm(ypts ~ xpts + xpts2 + xpts3, family = "binomial")
			    fit1 <- model1[[1]][1] + model1[[1]][2] * seq(0, 4, 0.01) +
			        model1[[1]][3] * seq(0, 4, 0.01)^2 + model1[[1]][4] *
			        seq(0, 4, 0.01)^3
			    fit2<-exp(fit1)/(1+exp(fit1))
			    lines(seq(0, 4, 0.01), fit2, lty = 3)
			    print(summary(model1))
			    model1
			}
			
