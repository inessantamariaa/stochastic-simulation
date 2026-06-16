#--------------------------------------
# Erlang B formula
#--------------------------------------

erlangB <- function(m, A) {
  num <- A^m / factorial(m)
  den <- sum(sapply(0:m, function(k) A^k / factorial(k)))
  return(num / den)
}

#--------------------------------------
# Generate service times
#--------------------------------------

generate_service <- function(type, mean_service, n) {
  
  if(type == "exp"){
    return(rexp(n, rate = 1/mean_service))
  }
  
  if(type == "constant"){
    return(rep(mean_service, n))
  }
  
  if(type == "pareto105"){
    k <- 1.05
    xm <- mean_service * (k - 1) / k
    u <- runif(n)
    return(xm / (1-u)^(1/k))
  }
  
  if(type == "pareto205"){
    k <- 2.05
    xm <- mean_service * (k - 1) / k
    u <- runif(n)
    return(xm / (1-u)^(1/k))
  }
  if(type == "gamma"){
    shape <- 4
    scale <- mean_service/shape
    return(rgamma(n, shape=shape, scale=scale))
  }
}

#--------------------------------------
# Generate interarrival times
#--------------------------------------

generate_arrivals <- function(type, n){
  
  if(type == "poisson"){
    return(rexp(n, rate = 1))
  }
  
  if(type == "erlang"){
    return(rgamma(n, shape = 2, rate = 2))
  }
  
  if(type == "hyperexp"){
    
    p <- runif(n)
    
    x <- numeric(n)
    
    x[p <= 0.8] <- rexp(sum(p<=0.8), rate = 0.8333)
    x[p > 0.8] <- rexp(sum(p>0.8), rate = 5.0)
    
    return(x)
  }
  if(type == "pareto105"){
    # Pareto for interarrival times with mean=1, k=1.05
    k <- 1.05
    xm <- 1 * (k - 1) / k
    u <- runif(n)
    return(xm / (1-u)^(1/k))
  }
  
  if(type == "pareto205"){
    # Pareto for interarrival times with mean=1, k=2.05
    k <- 2.05
    xm <- 1 * (k - 1) / k
    u <- runif(n)
    return(xm / (1-u)^(1/k))
  }
  
  if(type == "chisq"){
    # Chi-squared with 1 degree of freedom
    # Mean = 1, Variance = 2
    return(rchisq(n, df = 1))
  }
}

#--------------------------------------
# Blocking simulation
#--------------------------------------

simulate_blocking <- function(
    m = 10,
    mean_service = 8,
    n_customers = 10000,
    arrival_type = "poisson",
    service_type = "exp"
){
  
  inter <- generate_arrivals(arrival_type, n_customers)
  arrivals <- cumsum(inter)
  
  services <- generate_service(
    service_type,
    mean_service,
    n_customers
  )
  
  departures <- numeric(0)
  
  blocked <- 0
  
  for(i in 1:n_customers){
    
    departures <- departures[departures > arrivals[i]]
    
    if(length(departures) < m){
      
      departures <- c(
        departures,
        arrivals[i] + services[i]
      )
      
    } else {
      
      blocked <- blocked + 1
      
    }
  }
  
  return(blocked / n_customers)
}

#--------------------------------------
# Run replications
#--------------------------------------

run_experiment <- function(
    arrival_type,
    service_type,
    nrep = 10
){
  
  results <- numeric(nrep)
  
  for(i in 1:nrep){
    
    results[i] <- simulate_blocking(
      arrival_type = arrival_type,
      service_type = service_type
    )
    
  }
  
  mean_est <- mean(results)
  se <- sd(results)/sqrt(nrep)
  
  t_critical <- qt(0.975, df = nrep - 1)
  
  ci <- c(
    mean_est - t_critical * se,
    mean_est + t_critical * se
  )
  
  return(
    list(
      mean = mean_est,
      CI = ci,
      results = results
    )
  )
}

set.seed(123)

#Question 1
ans1 <- run_experiment(
  "poisson",
  "exp"
)

print(ans1)

cat(
  "Analytical Erlang B = ",
  erlangB(10,8),
  "\n"
)
#Question 2a
run_experiment(
  "erlang",
  "exp"
)
#Question 2b
run_experiment(
  "hyperexp",
  "exp"
)
#Question 3a
run_experiment(
  "poisson",
  "constant"
)
#Question 3b
run_experiment(
  "poisson",
  "pareto105"
)

run_experiment(
  "poisson",
  "pareto205"
)
#Question 3c
#You could add another distribution, for example Gamma:
  
  simulate_blocking(
    arrival_type = "poisson",
    service_type = "gamma"
  )

# and run

run_experiment(
  "poisson",
  "gamma"
)