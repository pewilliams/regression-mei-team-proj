######################################################################################### 
# Prepare the data for modeling:
######################################################################################### 
data <- readData()
#head(data)

# check for null or na values
# any(is.na(data))

# set a seed for splitting the data randomly
set.seed(1)

# split the data into a Training set and Testing set
sample <- sample.split(data$smf, SplitRatio = 0.7) # split along first Y column 

# 70% of the data for training
trainingData <- subset(data, sample == TRUE)

# 30% of the data for testing
testData <- subset(data, sample == FALSE)
