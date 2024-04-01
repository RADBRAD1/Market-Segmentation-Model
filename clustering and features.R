
# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 1 - STATISTICAL SEGMENTATION
# __________________________________________________________
# //////////////////////////////////////////////////////////


# --- COMPUTING RECENCY, FREQUENCY, MONETARY VALUE ---------


# Load text file into local variable called 'data'
data = read.delim(file = 'purchases1.txt', header = FALSE, sep = '\t', dec = '.')

# Add headers and interpret the last column as a date, extract year of purchase
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")
data$days_since       = as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))

# Display the data after transformation
head(data)
summary(data)

# Compute key marketing indicators using SQL language
library(sqldf)

# Compute recency, frequency, and average purchase amount
customers = sqldf("SELECT customer_id,
                          MIN(days_since) AS 'recency',
                          COUNT(*) AS 'frequency',
                          AVG(purchase_amount) AS 'amount'
                   FROM data GROUP BY 1")

# Explore the data
head(customers)
summary(customers)
hist(customers$recency)
hist(customers$frequency)
hist(customers$amount)
hist(customers$amount, breaks = 100)


# --- PREPARING AND TRANSFORMING DATA ----------------------


# Copy customer data into new data frame
new_data = customers

# Remove customer id as a variable, store it as row names
head(new_data)
row.names(new_data) = new_data$customer_id
new_data$customer_id = NULL
head(new_data)

# Take the log-transform of the amount, and plot
 new_data$amount = log(new_data$amount)
 hist(new_data$amount)

# Take the log-transform of the frequency, and plot
new_data$frequency = log(new_data$frequency)
hist(new_data$frequency)

# Standardize variables
new_data = scale(new_data)
head(new_data)


# --- RUNNING A HIERARCHICAL SEGMENTATION ------------------


# Compute distance metrics on standardized data
# This will likely generate an error on most machines
# d = dist(new_data)

# Take a 10% sample
sample = seq(1, 18417, by = 10)
head(sample)
customers_sample = customers[sample, ]
new_data_sample  = new_data[sample, ]

# Compute distance metrics on standardized data
d = dist(new_data_sample)

# Perform hierarchical clustering on distance metrics
c = hclust(d, method="ward.D2")

#colnames(customers_sample)

# Plot de dendogram
plot(c)

# Cut at 5 segments
members = cutree(c, k = 5)

# Show 30 first customers, then a frequency table
#members contains all customer data and segments they belong to
#when we did rownames = customerid it makes it so we can view customerid in the members array. 
members[1:30]
table(members)

# We compute the average profile of each segment, and show profile of each segment
#we aggregate the original customers sample, with just the first 3 variables (freq, rec, monetary val)
#we group them by cluster membership, in by = list(members), and we use the mean function 
#to group.
aggregate(customers_sample[, 2:4], by = list(members), mean)

#Compare customer 260 to customer 5920. 
members[1:30] # can see customer 260 and 5920 in the members array where we performed the segmentation.
#members[]

########
######Cosine similarity between new customers and all old customers. This will help us determine which existing customer will help us best model the behavior of the new customers.
###
# 
# sum(customer_data[x,2:6]*customer_data[y,2:6])/(norm(as.matrix(customer_data[x,2:6]),'F')*norm(as.matrix(customer_data[y,2:6]),'F'))
# # create an empty matrix to hold similarity values
# sims
# # compare each new customer (rows 7:8) to each existing customer (rows 1:6)
# 
# for (i in 7:8){
#   
#   #create empty vector to hold similarity values
#   
#   temp_sims
#   
#   for (j in 1:6){
#     
#     temp_sims = append(temp_sims,sim(j,i))
#     
#   }
#   
#   #add values in temp_sims to sims matrix
#   
#   sims = rbind(sims[(i-7),],temp_sims)
#   
# }
# 
# 
