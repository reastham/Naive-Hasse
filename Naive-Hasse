library(tidyverse)
library(numbers)

#Inputs for coefficients and prime
a <- -1
b <- 4
p <- 100

curve <- function(x) {
  return(x^3 + (a*x) + b)
}

prime_list <- Primes(2,p) #Primes to p
p <- prime_list[length(prime_list)] #In case p is not prime, this chooses the largest prime <= p
to_prime <- c(1:p) #Integers to p

#y^2 of integers
output <- curve(to_prime)

df <- data.frame(to_prime, output)

#Define the main function
process_primes <- function(df, prime_list) {
  results <- numeric(length(prime_list))  #Initialize the result list
  
  for (i in seq_along(prime_list)) {
    p <- prime_list[i]
    #Filter df rows where df$to_prime <= p
    filtered_df <- df[df$to_prime <= (p-1), ]
    
    #Legendre on each
    scores <- sapply(filtered_df$output, function(x) {
      #Check if x is a multiple of p
      if (x %% p == 0) {
        return(0)
      }
      
      #Compute x modulo p
      x <- x %% p
      
      #Check if x is a quadratic residue modulo p
      if (any((0:(p - 1))^2 %% p == x)) {
        return(1)
      } else {
        return(-1)
      }
    })
    
    #Sum up the scores for each prime
    results[i] <- sum(scores)
  }
  return(results)
}

a_p <- process_primes(df, prime_list)
a_p <- a_p*(-1)

number_points <- function(p, a_p) {
  points <- (p - a_p + 1)
  return(points)
}

N_p <- number_points(prime_list, a_p)

prime_list <- as.integer(prime_list)
a_p <- as.integer(a_p)
N_p <- as.integer(N_p)

#Print table
table <- data.frame(Primes = prime_list, Defect = a_p, Points = N_p)
print(table)

plot <- ggplot(table, aes(x = Primes, y = Defect)) +
  geom_point() +  #Add points
  stat_function(fun = function(x) 2 * sqrt(x), color = "red") +  #Add 2sqrt(x) line
  stat_function(fun = function(x) -2 * sqrt(x), color = "red") +  #Add -2sqrt(x) line
  labs(title = "Hasse's Theorem", subtitle="Primes Compared to Defects", x = "Prime", y = "P-Defect") +
  theme_minimal()

print(plot)
