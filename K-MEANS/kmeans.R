
#initialize centroid
initial.center = function (data, k) {
	return (data[sample(nrow(data), k),])
}

#fill in Distance Matrix
fillDist = function (data, centroid, distMatrix) {
	for(r in 1:nrow(centroid)) {
		for(c in 1:nrow(data)) {
			value = 0
			for(feat in 1:ncol(data)) {
				value = value + (data[c,feat]-centroid[r,feat])**2
			}
			value = sqrt(value)
			distMatrix[r,c] = value
		}
	}
	return (distMatrix)
}

#fill in Group Matrix
fillGroup = function (distMatrix, group) {
	for(r in 1:nrow(distMatrix)) {
		for(c in 1:ncol(distMatrix)) {
			group[r,c] = 0
		}
	}
	for(c in 1:ncol(distMatrix)) {
		min = min(distMatrix[,c])
		index = match(min, distMatrix[,c])
		group[index,c] = 1
	}
	return(group)
}

# Change centroid
newCentroid = function (data, group, centroid) {
	for(k in 1:nrow(group)) {
	v = c()
		for(i in 1:ncol(group)) {
			if(group[k,i] == 1) {
				v = c(v, i)
			}
		}
		for(j in 1:ncol(data)) {
		sum = 0
			for(r in v) {
				sum = data[r,j] + sum
			}
			sum = sum/length(v)
			centroid[k,j] = sum
		}
	}
	return(centroid)
}
# Preprocessing 
preprocess <- function() {
  
  args<-commandArgs(TRUE)
  x <- args[1]
  dataset = read.delim(x, header=FALSE, sep = ",")
  print(dataset)
  #dataset1 = data.frame(dataset)
  data_matrix = data.matrix(dataset)
  typeof(data_matrix)
  data = data_matrix[,-1]
  cat("Enter cluster number :")
  k = readLines(con="stdin", 1)
  k = as.double(k)
  print(data)
  indexes = sample(1:nrow(data), size = 0.2*nrow(data))
  test = data[indexes,]
  data = data[-indexes,]
  centroid = kmeans(data, k)
  test(test, centroid, k)
  
}

# K-means Algorithm
kmeans = function (data, k) {
	centroid = initial.center(data, k)
	distMatrix = matrix(, nrow = k, ncol = nrow(data))
	distMatrix = fillDist(data, centroid, distMatrix)
	group = matrix(, nrow = k, ncol = nrow(data))

	for(i in seq(1:30)) {
		group = fillGroup(distMatrix, group)
		centroid = newCentroid(data, group, centroid)
		cat(".")
		distMatrix = fillDist(data, centroid, distMatrix)
		}
	print("\n")
	print("Centroid")
	print(centroid)
	return(centroid)

		
}

# Test 
test = function(test, centroid, k) {
	distMatrix = matrix(, nrow = k, ncol = nrow(test))
	distMatrix = fillDist(test, centroid, distMatrix)
	group = matrix (, nrow = k, ncol = nrow(test))
	group = fillGroup(distMatrix, group)
	print("After Testing")
	print("Cluster groups")
	print(group)
}

preprocess()



