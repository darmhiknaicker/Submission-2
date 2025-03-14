library(foreach)
library(doParallel)

#question 1
# Register parallel backend (optional, but can speed up execution)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Set seed for reproducibility
set.seed(123)

# Use foreach to generate samples, calculate mean and variance, and row-bind results
results <- foreach(i = 1:100, .combine = rbind) %dopar% {
  sample_data <- rexp(100, rate = 1)  # Exponential distribution with mean 1 (rate = 1)
  sample_mean <- mean(sample_data)
  sample_var <- var(sample_data)
  c(sample_mean, sample_var)
}

# Convert to a data frame and add column names
results <- as.data.frame(results)
colnames(results) <- c("Mean", "Variance")

# Stop parallel cluster
stopCluster(cl)

# Display the first few rows of the results
head(results)


#question 2
library(MASS)

set.seed(123)
B <- 10000  # Number of bootstrap samples


serial_time <- system.time({
  serial_results <- replicate(B, {
    sample_data <- sample(galaxies, length(galaxies), replace = TRUE)
    median(sample_data)
  })
})


library(doParallel)
library(foreach)

# Set up parallel backend
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

set.seed(123)

# Parallel bootstrap
parallel_time <- system.time({
  
  parallel_results <- foreach(i = 1:B, .combine = c, .packages = 'MASS') %dopar% {
    sample_data <- sample(galaxies, length(galaxies), replace = TRUE)
    median(sample_data)
  }
})

stopCluster(cl)

# Reinitialize parallel backend
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

set.seed(123)
B_batch <- 1000  # Number of bootstrap samples per batch
num_batches <- B / B_batch  # Total batches

batch_time <- system.time({
  batch_results <- foreach(i = 1:num_batches, .combine = c, .packages = 'MASS') %dopar% {
    replicate(B_batch, {
      sample_data <- sample(galaxies, length(galaxies), replace = TRUE)
      median(sample_data)
    })
  }
})



# Stop parallel cluster
stopCluster(cl)

# Display execution time

cat("Serial processing time:", serial_time, "\n")
cat("Parallel processing time (single bootstrap per iteration):", parallel_time, "\n")
cat("Parallel processing time (batch of 1000 bootstraps per iteration):", batch_time, "\n")



#question 3
library(boot)

# Function to compute the median from a resample
median_boot <- function(data, indices) {
  return(median(data[indices]))
}

# Parameters
set.seed(123)
B <- 1000    # Number of bootstrap resamples per sample
MC_reps <- 1000  # Number of Monte Carlo replications
n <- 50      # Sample size
true_median <- log(2)  # True median of Exp(1) distribution
coverage_count <- 0    # Counter for coverage



sys_time1 <- system.time({
  for (i in 1:MC_reps) {
    # Generate a new sample
    sample_data <- rexp(n, rate = 1)
    
    # Perform bootstrap resampling
    boot_result <- boot(sample_data, statistic = median_boot, R = B)
    
    # Compute 95% percentile bootstrap CI
    ci <- boot.ci(boot_result, type = "perc")$percent[4:5]  # Extract lower & upper bounds
    
    # Check if the true median is inside the CI
    if (!is.null(ci) && true_median >= ci[1] && true_median <= ci[2]) {
      coverage_count <- coverage_count + 1
    }
  }
})

coverage_prob <- coverage_count / MC_reps  # Estimated coverage probability

# Display results
cat("Estimated Coverage Probability:", coverage_prob, "\n")
cat("Execution Time:", sys_time1, "\n")

#question 4
# Load required packages
library(iterators)
library(foreach)

# Create an iterator for 3 vectors, each containing 5 normal random numbers
rand_iter <- irnorm(n = 5, mean = 0, sd = 1)

# Use foreach to find the maximum in each vector (3 iterations)
max_values <- foreach(i = 1:3, .combine = c) %do% {
  max(nextElem(rand_iter))
}


# Print the results
print(max_values)

#question 5
# Load required packages
library(iterators)
library(foreach)
library(parallel)
library(doParallel)

# Set seed for reproducibility
set.seed(1234)

# Number of cores for parallel execution
numCores <- detectCores() - 1  
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Define function to generate max values from 5 random normals
find_max <- function() {
  max(rnorm(5, mean = 0, sd = 1))
}

# 1. Using foreach (sequential)
system.time({
  rand_iter <- irnorm(n = 5, mean = 0, sd = 1)
  max_values_foreach_seq <- foreach(i = 1:3, .combine = c) %do% {
    max(nextElem(rand_iter))
  }
})

# 2. Using foreach (parallel)
system.time({
  max_values_foreach_par <- foreach(i = 1:3, .combine = c, .packages = "iterators") %dopar% {
    max(rnorm(5, mean = 0, sd = 1))
  }
})

# 3. Using parLapply (parallel)
system.time({
  max_values_parLapply <- parLapply(cl, 1:3, function(x) max(rnorm(5, mean = 0, sd = 1)))
})

# 4. Using replicate (sequential)
system.time({
  max_values_replicate <- replicate(3, max(rnorm(5, mean = 0, sd = 1)))
})

# Stop parallel cluster
stopCluster(cl)









