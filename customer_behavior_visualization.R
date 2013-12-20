algorithm_numb_item_change <-function(price){
		# calculate the number of row lower than a cetain price, need to use (total - one_item)	
		data_location_lowerprice    <- subset(data_location,(total-min_price_item) < price + 1 & (total-min_price_item) > price )
		if(price == 9){
			# print(data_location_lowerprice$orderid)
		}
		total_number_row_lowerprice <- nrow(data_location_lowerprice)
		# calculate the number of row with more than one item
		data_location_lowerprice_moreitem <- subset(data_location_lowerprice,min_price_item != 0)
		if(price == 9){
			# print("--------------------------------------------------------------->")

			# print(data_location_lowerprice_moreitem$orderid)
		}
		total_number_row_lowerprice_moreitem <- nrow(data_location_lowerprice_moreitem)
		# print(total_number_row_lowerprice_moreitem)
		if(total_number_row_lowerprice <= 10){
			total_number_row_lowerprice_moreitem <- 0
			prob_moreitem <- total_number_row_lowerprice_moreitem/total_number_row_lowerprice
		}else{
			prob_moreitem <- total_number_row_lowerprice_moreitem/total_number_row_lowerprice
		}
		
		print(paste(price,total_number_row_lowerprice,total_number_row_lowerprice_moreitem,prob_moreitem,sep=" , "))
		return (c(price,total_number_row_lowerprice_moreitem/total_number_row_lowerprice))
	
	}
	
	algorithm_numb_item_add <-function(price){
		# calculate the number of row lower than a cetain price, need to use (total - one_item)	
		data_location_lowerprice    <- subset(data_location,(total-min_price_item) < price + 1 & (total-min_price_item) > price )
		total_number_row_lowerprice <- nrow(data_location_lowerprice)
		# calculate the number of row with more than one item
		data_location_lowerprice_moreitem <- subset(data_location_lowerprice,min_price_item != 0)
		total_number_row_lowerprice_moreitem <- nrow(data_location_lowerprice_moreitem)
		# print(total_number_row_lowerprice_moreitem)
		prob_moreitem <- total_number_row_lowerprice_moreitem/total_number_row_lowerprice
		return (c(price,total_number_row_lowerprice_moreitem/total_number_row_lowerprice))
	
	}
	
	sequ <- seq(5,20,1)
	
	cluster_total<-read.csv(file=paste("/Users/alan/Desktop/data_reference/cluster_data_in.csv",sep=""),head=TRUE,sep=",")
	
	
	price_cut_addon <- 3
	cluster_numb = 1
	setwd("/Users/alan/Desktop/data_graphics")
	pdf(paste('C',cluster_numb,'_credit.pdf',sep=""), bg = "white")			
	order_multi<-read.csv(file="/Users/alan/Desktop/data_reference/cut_cdf.csv",head=TRUE,sep=",")	
	
	cluster1<-cluster_total
	
	cluster1<-merge(cluster1, order_multi, by="orderid" , all.x=TRUE)			
	numb_item_no_restrict_table <- read.csv(file="/Users/alan/Desktop/data_reference/numb_item_no_restrict_table_withmin.csv",head=TRUE,sep=",")
	cluster1<-merge(cluster1, numb_item_no_restrict_table, by="orderid")
	cluster1$orderitem[is.na(cluster1$orderitem)] <- 0	
	#################################################################################################
	#################################################################################################
	cluster1$min_price_item[cluster1$min_price_item > price_cut_addon | cluster1$numb_item_no_restrict == 1] <- 0	

	large_pizza_orders<-read.csv(file="/Users/alan/Desktop/data_reference/large_pizza_orders.csv",head=TRUE,sep=",")		
	cluster1<-merge(cluster1, large_pizza_orders, by="orderid" , all.x=TRUE)			
	cluster1$large_pizza[is.na(cluster1$large_pizza)] <- 0	
	order_remove_01<-read.csv(file="/Users/alan/Desktop/data_reference/order_remove_01.csv",head=TRUE,sep=",")		
	cluster1<-merge(cluster1, order_remove_01, by="orderid" , all.x=TRUE)			
	cluster1$remove_index[is.na(cluster1$remove_index)] <- 0	
	# c2, weekday1234_lunch
	credit_1_cash_0_set = 1
	window_width = 0.1
			
			data_location<-cluster1
			data_location<-subset(data_location,cluster == 1)	
			length(data_location$cluster)
			data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
			data_location<-subset(data_location,orderitem == 0)
			data_location<-subset(data_location,large_pizza == 0)	
			data_location<-subset(data_location,customerid_0_visitor!=0)
			data_location<-subset(data_location,dayofweek  < 5 )
			data_location<-subset(data_location,hourofday < 15)		
			#################################################################################################
			data_location<-subset(data_location,discount == 0)	
			#################################################################################################
			#################################################################################################
			data_location<-subset(data_location,remove_index == 0)	
			#################################################################################################
			
			#################################################################################################
			data_location<-subset(data_location,locationid != 398)
			data_location<-subset(data_location,locationid != 381)
			#################################################################################################
			data_length_weekday1234_lunch <- length(data_location$orderid)

			if(data_length_weekday1234_lunch > 100){	
				vec_price <- c()	
				vec_ratio <- c()
				
				###############################################################
				vec_sum <- 0
				vec_average <- 0 	
				for(i in sequ){
					pair_sum <- algorithm_numb_item_add(i)	
					prob_more_item_sum <- pair_sum[2]	
					vec_sum <- vec_sum + prob_more_item_sum
					vec_average <- vec_sum / 26
				}
				###############################################################
				
				for(i in sequ){
					pair <- algorithm_numb_item_change(i)	
					price <- pair[1]	
					prob_more_item <-pair[2]	
				
					vec_price <- c(vec_price,price)
					vec_ratio <- c(vec_ratio,prob_more_item)
					
				}
				
				data <- data.frame(vec_price,vec_ratio)
				# plot(lowess(data$vec_price,data$vec_ratio,f=window_width), type="l",ylim=c(0,1),col="red",pch=2)
				plot(data, type="l",ylim=c(0,1),col="red",pch=2)
				abline(v=seq(0,18,1), col="grey", lty=1)

			}else{
				print(c(LocationID,"no data"))
			}
			
			print("-------------------------------------")
			#### rq_1<-regression_quad(vec_ratio,vec_price)
			
	#################################################################################################
	# c2, weekday1234_dinner
	
	# lowess			
				data_location<-cluster1
				data_location<-subset(data_location,cluster == 2)	
				
				data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
				data_location<-subset(data_location,orderitem == 0)
				data_location<-subset(data_location,large_pizza == 0)
				data_location<-subset(data_location,customerid_0_visitor!=0)
				data_location<-subset(data_location,dayofweek  < 5 )
				data_location<-subset(data_location,hourofday > 15)
				#################################################################################################
				data_location<-subset(data_location,discount == 0)	
				#################################################################################################
				#################################################################################################
				data_location<-subset(data_location,remove_index == 0)	
				#################################################################################################
				data_location<-subset(data_location,locationid != 398)
				data_location<-subset(data_location,locationid != 381)
				#################################################################################################
				data_length_weekday1234_dinner <- length(data_location$orderid)

				if(data_length_weekday1234_dinner > 100){	
					vec_price <- c()	
					vec_ratio <- c()	
					###############################################################
					vec_sum <- 0
					vec_average <- 0 	
					for(i in sequ){
						pair_sum <- algorithm_numb_item_add(i)	
						prob_more_item_sum <- pair_sum[2]	
						vec_sum <- vec_sum + prob_more_item_sum
						vec_average <- vec_sum / 26
					}
					###############################################################
			
					for(i in sequ){
						pair <- algorithm_numb_item_change(i)	
						price <- pair[1]	
						prob_more_item <-pair[2]	
					
						vec_price <- c(vec_price,price)
						vec_ratio <- c(vec_ratio,prob_more_item)
						
					}

					data <- data.frame(vec_price,vec_ratio)

					# lines(lowess(data$vec_price,data$vec_ratio,f=window_width), type="l",ylim=c(0,1),col="blue",pch=2)
					lines(data, type="l",ylim=c(0,1),col="blue",pch=2)
					abline(v=seq(0,18,1), col="grey", lty=1)

				}else{
					print(c(LocationID,"no data"))
				}
			#rq_2<-regression_quad(vec_ratio,vec_price)
			print("-------------------------------------")

	#################################################################################################
	# c2, weekday567_lunch
				
					data_location<-cluster1
					data_location<-subset(data_location,cluster == 2)	
					
					data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
					data_location<-subset(data_location,orderitem == 0)
					data_location<-subset(data_location,large_pizza == 0)
					data_location<-subset(data_location,customerid_0_visitor!=0)
					data_location<-subset(data_location,dayofweek  > 4 )
					data_location<-subset(data_location,hourofday < 15)
					#################################################################################################
					data_location<-subset(data_location,discount == 0)	
					#################################################################################################
					#################################################################################################
					data_location<-subset(data_location,remove_index == 0)	
					#################################################################################################
			
					data_length_weekday567_lunch <- length(data_location$orderid)
					
					#################################################################################################
					data_location<-subset(data_location,locationid != 398)
					data_location<-subset(data_location,locationid != 381)
					#################################################################################################

					if(data_length_weekday567_lunch > 100){	
						vec_price <- c()	
						vec_ratio <- c()	
						###############################################################
						vec_sum <- 0
						vec_average <- 0 	
						for(i in sequ){
							pair_sum <- algorithm_numb_item_add(i)	
							prob_more_item_sum <- pair_sum[2]	
							vec_sum <- vec_sum + prob_more_item_sum
							vec_average <- vec_sum / 26
						}
						###############################################################
			
						for(i in sequ){
							pair <- algorithm_numb_item_change(i)	
							price <- pair[1]	
							prob_more_item <-pair[2]	
					
							vec_price <- c(vec_price,price)
							vec_ratio <- c(vec_ratio,prob_more_item)
						
						}

						data <- data.frame(vec_price,vec_ratio)

						# lines(lowess(data$vec_price,data$vec_ratio,f=window_width), type="l",ylim=c(0,1),col="green",pch=2)
						lines(data, type="l",ylim=c(0,1),col="green",pch=2)
						abline(v=seq(0,18,1), col="grey", lty=1)

					}else{
						print(c(LocationID,"no data"))
					}
			
			#rq_3<-regression_quad(vec_ratio,vec_price)
			print("-------------------------------------")
				
	
	#################################################################################################
	# c2, weekend567_dinner
				
					data_location<-cluster1
					data_location<-subset(data_location,cluster == 2)	
					
					data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
					data_location<-subset(data_location,orderitem == 0)
					data_location<-subset(data_location,large_pizza == 0)
					data_location<-subset(data_location,customerid_0_visitor!=0)
					data_location<-subset(data_location,dayofweek  > 4 )
					data_location<-subset(data_location,hourofday > 15)
					#################################################################################################
					data_location<-subset(data_location,discount == 0)	
					#################################################################################################
					#################################################################################################
					data_location<-subset(data_location,remove_index == 0)	
					#################################################################################################
			
					data_length_weekend567_dinner <- length(data_location$orderid)
				
					#################################################################################################
					data_location<-subset(data_location,locationid != 398)
					data_location<-subset(data_location,locationid != 381)
					#################################################################################################
					if(data_length_weekend567_dinner > 100){	
						vec_price <- c()	
						vec_ratio <- c()	
						###############################################################
						vec_sum <- 0
						vec_average <- 0 	
						for(i in sequ){
							pair_sum <- algorithm_numb_item_add(i)	
							prob_more_item_sum <- pair_sum[2]	
							vec_sum <- vec_sum + prob_more_item_sum
							vec_average <- vec_sum / 26
						}
						###############################################################
			
						for(i in sequ){
							pair <- algorithm_numb_item_change(i)	
							price <- pair[1]	
							prob_more_item <-pair[2]	
					
							vec_price <- c(vec_price,price)
							vec_ratio <- c(vec_ratio,prob_more_item)
					
						}

						data <- data.frame(vec_price,vec_ratio)

						# lines(lowess(data$vec_price,data$vec_ratio,f=window_width), type="l",ylim=c(0,1),col="brown",pch=2)
						lines(data, type="l",ylim=c(0,1),col="brown",pch=2)
						abline(v=seq(0,18,1), col="grey", lty=1)

					}else{
						print(c(LocationID,"no data"))
					}
			#rq_4<-regression_quad(vec_ratio,vec_price)

			#################################################################################################
			# c2, all
			print("c5, all")
	
				
							data_location<-cluster1
							data_location<-subset(data_location,cluster == 2)	
							
							data_location<-subset(data_location,credit_1_cash_0 == credit_1_cash_0_set)
							
							data_location<-subset(data_location,orderitem == 0)
							data_location<-subset(data_location,large_pizza == 0)
							data_location<-subset(data_location,customerid_0_visitor!=0)
							#################################################################################################
							data_location<-subset(data_location,discount == 0)	
							#################################################################################################
							#################################################################################################
							data_location<-subset(data_location,remove_index == 0)	
							#################################################################################################
			
							data_length_weekend567_dinner <- length(data_location$orderid)
						
							#################################################################################################
							data_location<-subset(data_location,locationid != 398)
							data_location<-subset(data_location,locationid != 381)
							#################################################################################################
							if(data_length_weekend567_dinner > 100){	
								vec_price <- c()	
								vec_ratio <- c()	
								###############################################################
								vec_sum <- 0
								vec_average <- 0 	
								for(i in sequ){
									pair_sum <- algorithm_numb_item_add(i)	
									prob_more_item_sum <- pair_sum[2]	
									vec_sum <- vec_sum + prob_more_item_sum
									vec_average <- vec_sum / 26
								}
								###############################################################
			
								for(i in sequ){
									pair <- algorithm_numb_item_change(i)	
									price <- pair[1]	
									prob_more_item <-pair[2]	
					
									vec_price <- c(vec_price,price)
									vec_ratio <- c(vec_ratio,prob_more_item)
					
								}

								data <- data.frame(vec_price,vec_ratio)

								# lines(lowess(data$vec_price,data$vec_ratio,f=window_width), type="l",ylim=c(0,1),col="brown",pch=2)
								lines(data, type="b",ylim=c(0,1),col="black",pch=2)
								
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
	

	
