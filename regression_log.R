divisor <- 5
# c2, weekday1234_lunch
cluster_numb = 3
credit_1_cash_0_set = 1

# start <- 6
# end   <- 11
	
# start <- 10
# end   <- 15
	
start <- 11
end   <- 21

cluster_total<-read.csv(file=paste("/Users/alan/Desktop/data_reference/cluster_data_in.csv",sep=""),head=TRUE,sep=",")


#########################################################################################################

	order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
	# cluster1<-read.csv(file=paste("/Users/alan/Desktop/data_reference/cluster",cluster_numb,'.csv',sep=""),head=TRUE,sep=",")
	cluster1<-cluster_total
	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)			
	numb_item_no_restrict_table <- read.csv(file="/Users/alan/Desktop/data_reference/numb_item_no_restrict_table_withmin.csv",head=TRUE,sep=",")
	cluster1<-merge(cluster1, numb_item_no_restrict_table, by="orderid")
	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
	#################################################################################################
	cluster1$min_price_item[cluster1$min_price_item > 3 | cluster1$numb_item_no_restrict == 1] <- 0	
	#################################################################################################
	large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
	cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
	cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	

	
	data_location<-cluster1
	data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
	data_location<-subset(data_location,orderitem == 0)
	data_location<-subset(data_location,large_pizza == 0)	
	data_location<-subset(data_location,customerid_0_visitor!=0)				
	data_location<-subset(data_location,discount == 0)	


	#################################################################################################
	# red < <
	# blue < >
	# green >= <
	# brown >= >
	data_location<-subset(data_location,dayofweek  < 5 )
	data_location<-subset(data_location,hourofday  < 15)	
	#################################################################################################
	data_location<-subset(data_location,cluster == cluster_numb)	
		
	cluster1	  <- data_location
	
	
	cluster1$total_min_item <- cluster1$total - cluster1$min_price_item
	cluster1$is_weekend <- floor(cluster1$dayofweek/4)
	cluster1$is_dinner <- floor(cluster1$hourofday/15)
	
	###################################################################################################
	###################################################################################################
	# The location to decide price interval

	
	cluster1<-subset(cluster1,total_min_item <= end)	
	cluster1<-subset(cluster1,total_min_item >= start)	
	###################################################################################################
	###################################################################################################
	
	cluster1["is_addon"] <- NA
	cluster1$is_addon[which(cluster1$min_price_item > 0 )]<-1
	cluster1$is_addon[is.na(cluster1$is_addon)] <- 0		
	
	cluster1["numb_item_1"] <- NA
	cluster1$numb_item_1[which(floor(cluster1$total_min_item)%%divisor==0 | floor(cluster1$total_min_item)%%divisor==1 | floor(cluster1$total_min_item)%%divisor==2 )]<-1
	cluster1$numb_item_1[is.na(cluster1$numb_item_1)] <- 0		
	# cluster1$numb_item_1

	focus_flag_1   <- cluster1$numb_item_1
	
	focus_total  <- cluster1$total
	
	delivery       <- cluster1$deli_1_takeout_0
	dinner         <- cluster1$is_dinner
	weekend        <- cluster1$is_weekend
	is_addon	   <- cluster1$is_addon
					
	model1<-glm(is_addon ~  focus_total + focus_flag_1, family=binomial) 		
	
	print (summary(model1))
	
	
	
	
	
	#########################################################################################################

		order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
		cluster1<-cluster_total
		cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)			
		numb_item_no_restrict_table <- read.csv(file="/Users/alan/Desktop/data_reference/numb_item_no_restrict_table_withmin.csv",head=TRUE,sep=",")
		cluster1<-merge(cluster1, numb_item_no_restrict_table, by="orderid")
		cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
		#################################################################################################
		cluster1$min_price_item[cluster1$min_price_item > 3 | cluster1$numb_item_no_restrict == 1] <- 0	
		#################################################################################################
		large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
		cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
		cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	

	
		data_location<-cluster1
		data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
		data_location<-subset(data_location,orderitem == 0)
		data_location<-subset(data_location,large_pizza == 0)	
		data_location<-subset(data_location,customerid_0_visitor!=0)
		data_location<-subset(data_location,discount == 0)	
		

		#################################################################################################
		# red < <
		# blue < >
		# green >= <
		# brown >= >
		data_location<-subset(data_location,dayofweek  < 5 )
		data_location<-subset(data_location,hourofday  > 15)	
		#################################################################################################
		data_location<-subset(data_location,cluster == cluster_numb)	
		
		cluster1	  <- data_location
		
	
		cluster1$total_min_item <- cluster1$total - cluster1$min_price_item
		cluster1$is_weekend <- floor(cluster1$dayofweek/4)
		cluster1$is_dinner <- floor(cluster1$hourofday/15)
	
		###################################################################################################
		###################################################################################################
		# The location to decide price interval

	
		cluster1<-subset(cluster1,total_min_item <= end)	
		cluster1<-subset(cluster1,total_min_item >= start)	
		###################################################################################################
		###################################################################################################
	
		cluster1["is_addon"] <- NA
		cluster1$is_addon[which(cluster1$min_price_item > 0 )]<-1
		cluster1$is_addon[is.na(cluster1$is_addon)] <- 0		
	
		cluster1["numb_item_1"] <- NA
		cluster1$numb_item_1[which(floor(cluster1$total_min_item)%%divisor==0 | floor(cluster1$total_min_item)%%divisor==1 | floor(cluster1$total_min_item)%%divisor==2 )]<-1
		cluster1$numb_item_1[is.na(cluster1$numb_item_1)] <- 0		
		# cluster1$numb_item_1

		focus_flag_1   <- cluster1$numb_item_1
	
		focus_total  <- cluster1$total
	
		delivery       <- cluster1$deli_1_takeout_0
		dinner         <- cluster1$is_dinner
		weekend        <- cluster1$is_weekend
		is_addon	   <- cluster1$is_addon
					
		model1<-glm(is_addon ~  focus_total + focus_flag_1, family=binomial) 		
	
		print (summary(model1))
		
		
		
		
		#########################################################################################################

			order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
			cluster1<-cluster_total
			cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)			
			numb_item_no_restrict_table <- read.csv(file="/Users/alan/Desktop/data_reference/numb_item_no_restrict_table_withmin.csv",head=TRUE,sep=",")
			cluster1<-merge(cluster1, numb_item_no_restrict_table, by="orderid")
			cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
			#################################################################################################
			cluster1$min_price_item[cluster1$min_price_item > 3 | cluster1$numb_item_no_restrict == 1] <- 0	
			#################################################################################################
			large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
			cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
			cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	

	
			data_location<-cluster1
			data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
			data_location<-subset(data_location,orderitem == 0)
			data_location<-subset(data_location,large_pizza == 0)	
			data_location<-subset(data_location,customerid_0_visitor!=0)
			data_location<-subset(data_location,discount == 0)	
			

			#################################################################################################
			# red < <
			# blue < >
			# green >= <
			# brown >= >
			data_location<-subset(data_location,dayofweek  >= 5 )
			data_location<-subset(data_location,hourofday  < 15)	
			#################################################################################################
			data_location<-subset(data_location,cluster == cluster_numb)	
		
			cluster1	  <- data_location
			
	
			cluster1$total_min_item <- cluster1$total - cluster1$min_price_item
			cluster1$is_weekend <- floor(cluster1$dayofweek/4)
			cluster1$is_dinner <- floor(cluster1$hourofday/15)
	
			###################################################################################################
			###################################################################################################
			# The location to decide price interval

	
			cluster1<-subset(cluster1,total_min_item <= end)	
			cluster1<-subset(cluster1,total_min_item >= start)	
			###################################################################################################
			###################################################################################################
	
			cluster1["is_addon"] <- NA
			cluster1$is_addon[which(cluster1$min_price_item > 0 )]<-1
			cluster1$is_addon[is.na(cluster1$is_addon)] <- 0		
	
			cluster1["numb_item_1"] <- NA
			cluster1$numb_item_1[which(floor(cluster1$total_min_item)%%divisor==0 | floor(cluster1$total_min_item)%%divisor==1 | floor(cluster1$total_min_item)%%divisor==2 )]<-1
			cluster1$numb_item_1[is.na(cluster1$numb_item_1)] <- 0		
			# cluster1$numb_item_1

			focus_flag_1   <- cluster1$numb_item_1
	
			focus_total  <- cluster1$total
	
			delivery       <- cluster1$deli_1_takeout_0
			dinner         <- cluster1$is_dinner
			weekend        <- cluster1$is_weekend
			is_addon	   <- cluster1$is_addon
					
			model1<-glm(is_addon ~  focus_total + focus_flag_1, family=binomial) 		
	
			print (summary(model1))
			
			
			
			
			#########################################################################################################

				order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
				cluster1<-cluster_total
				cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)			
				numb_item_no_restrict_table <- read.csv(file="/Users/alan/Desktop/data_reference/numb_item_no_restrict_table_withmin.csv",head=TRUE,sep=",")
				cluster1<-merge(cluster1, numb_item_no_restrict_table, by="orderid")
				cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
				#################################################################################################
				cluster1$min_price_item[cluster1$min_price_item > 3 | cluster1$numb_item_no_restrict == 1] <- 0	
				#################################################################################################
				large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
				cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
				cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	

	
				data_location<-cluster1
				data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
				data_location<-subset(data_location,orderitem == 0)
				data_location<-subset(data_location,large_pizza == 0)	
				data_location<-subset(data_location,customerid_0_visitor!=0)
				data_location<-subset(data_location,discount == 0)	
				

				#################################################################################################
				# red < <
				# blue < >
				# green >= <
				# brown >= >
				data_location<-subset(data_location,dayofweek  >= 5 )
				data_location<-subset(data_location,hourofday  > 15)	
				#################################################################################################
				data_location<-subset(data_location,cluster == cluster_numb)	
		
				cluster1	  <- data_location
				
	
				cluster1$total_min_item <- cluster1$total - cluster1$min_price_item
				cluster1$is_weekend <- floor(cluster1$dayofweek/4)
				cluster1$is_dinner <- floor(cluster1$hourofday/15)
	
				###################################################################################################
				###################################################################################################
				# The location to decide price interval

	
				cluster1<-subset(cluster1,total_min_item <= end)	
				cluster1<-subset(cluster1,total_min_item >= start)	
				###################################################################################################
				###################################################################################################
	
				cluster1["is_addon"] <- NA
				cluster1$is_addon[which(cluster1$min_price_item > 0 )]<-1
				cluster1$is_addon[is.na(cluster1$is_addon)] <- 0		
	
				cluster1["numb_item_1"] <- NA
				cluster1$numb_item_1[which(floor(cluster1$total_min_item)%%divisor==0 | floor(cluster1$total_min_item)%%divisor==1 | floor(cluster1$total_min_item)%%divisor==2 )]<-1
				cluster1$numb_item_1[is.na(cluster1$numb_item_1)] <- 0		
				# cluster1$numb_item_1

				focus_flag_1   <- cluster1$numb_item_1
	
				focus_total  <- cluster1$total
	
				delivery       <- cluster1$deli_1_takeout_0
				dinner         <- cluster1$is_dinner
				weekend        <- cluster1$is_weekend
				is_addon	   <- cluster1$is_addon
					
				model1<-glm(is_addon ~  focus_total + focus_flag_1, family=binomial) 		
	
				print (summary(model1))
	
