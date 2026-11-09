# MSA102 project: Hamiltonian Monte Carlo for election night predictions.
# Run from this folder: Rscript HamiltonianMonteCarlo.R
# All plots are saved as png files in this folder.

# Fixed seed so the MCMC results can be reproduced
set.seed(2025)

##########################################################################################
# Read the data and plot it
data <- read.table("election25.txt", header=TRUE)

png("overview_data.png", width = 900, height = 400)
par(mfrow=c(1,3), cex=1)  

hist(data$ResultPreviousElection, main="Previous election log-odds (qi)", xlab="Log-odds")
hist(data$finalResults, main="Final log-odds (pi)", xlab="Log-odds")

exit_poll_data <- data[!is.na(data$exitPollResults), ]
exit_poll_data$exitPollResults_log_odds <- log((exit_poll_data$exitPollResults/100) / (1 - (exit_poll_data$exitPollResults/100)))

hist(exit_poll_data$exitPollResults_log_odds, main="Log-odds (converted) from exit poll", xlab="Log-odds")

exit_poll_data$exitPollResults_log_odds_diff <- (exit_poll_data$exitPollResults_log_odds - exit_poll_data$finalResults)
data$finalResults_diff <- (data$finalResults - data$ResultPreviousElection)
invisible(dev.off())

png("scatterplot.png")
plot(data$finalResults, data$ResultPreviousElection,
     main="Scatter plot",
     xlab="Final result",
     ylab="Previous result",
     pch=19,  
     col="darkblue", 
     cex=1)  
abline(a=0, b=1, col="red")
invisible(dev.off())



##########################################################################################
#Library for gradient verification
library(numDeriv)

# Define variables
q <- data$ResultPreviousElection  # log-odds from previous election
y <- data$exitPollResults  # exit poll results
y[is.na(y)] <- 0 #remove NA
t_data <- data$timeReceived # time when final results are received
p <- data$finalResults  # log-odds of final results


##########################################################################################
# Function to compute U_t(mu, gamma, psi) at time t
create_U_function <- function(t) {

  # Determine the constituencies with final results by time t
  final_results <- !is.na(p) & t_data <= t
  s <- sum(final_results)
  #final results where we have results by time t minus previous election results.
  d_s <- p[final_results] - q[final_results]

  # Determine the indices of constituencies with only poll data by time t
  # since t is the time of final election we look att poll data where we have election results coming in after, boolean indexes
  poll_data <- !is.na(y) & is.na(p) | t_data > t
  r <- sum(poll_data)

  #exit poll results where we have not received final score
  y_r <- y[poll_data]

  #previous odds where we have not received final score
  q_r <- q[poll_data]

  # Define the function U
  U <- function(q) {
    mu <- q[1]
    gamma <- q[2]
    psi <- q[3]

    #normalizing constant

    normalizing_c <- gamma*(s+r)/2

    # Compute the sum for constituencies with final results
    sum_final <- sum((d_s - mu)^2)

    # Compute the sum for constituencies with only poll data
    sum_poll <- sum(-y_r * (psi + q_r) + 100 * log(1 + exp(psi + q_r)))

    # Compute U
    U_value <- normalizing_c + (exp(gamma) / 2) * sum_final + sum_poll + (exp(gamma) / 2) * sum((psi - mu)^2)

    return(U_value)
  }

  return(U)
}

##########################################################################################

create_U_grad <- function(t) {
  # same as for U
  final_results <- !is.na(p) & t_data <= t
  s <- sum(final_results)

  d_s <- p[final_results] - q[final_results]

  poll_data <- !is.na(y) & (is.na(p) | t_data > t)
  r <- sum(poll_data)

  y_r <- y[poll_data]
  q_r <- q[poll_data]

  U_grad <- function(q) {
    mu <- q[1]
    gamma <- q[2]
    psi <- q[3]

    # Individual gradients
    grad_mu <- -exp(gamma) * (sum(d_s - mu) + sum(psi - mu))
    grad_gamma <- (s+r)/2 + 0.5 * exp(gamma) * (sum((d_s - mu)^2) + sum((psi - mu)^2))  
    grad_psi <- sum(100*exp(psi + q_r) / (1 + exp(psi + q_r)) - y_r) + (exp(gamma)*sum(psi-mu))

    grad <- c(grad_mu, grad_gamma, grad_psi)
    return(grad)
  }

  return(U_grad)
}

##########################################################################################

# Test point
test_point <- c(1, 2, 2)

# Compute the gradient at the test point using gradU
u_grad2 = create_U_grad(3.4)
grad_test <- u_grad2(test_point)
print("Gradient from gradU:")
print(grad_test)

# Compute the numerical gradient using numDeriv
U_func <- create_U_function(3.4)
numerical_grad <- grad(U_func, test_point)
print("Numerical gradient:")
print(numerical_grad)


##########################################################################################
# First program the leapfrog algorithm: 
# Input: 
# currentp  the current value pf p
# currentq  the current value of q
# gradU     the gradient function for the U function 
# epsilon   the size of the steps of the leapfrog
# L         the number of the steps for the leapfrog
# Output: 
# newp      the new value for p
# newq      the new value for q

leapfrog <- function(currentp, currentq, gradU, epsilon, L, plotpoints=TRUE) {
  q <- currentq 
  p <- currentp - epsilon*gradU(q)/2 # Start with a step of half length
  plot_points <- NULL  # Initialize to NULL

  for (i in 1:L) {
    if (plotpoints)  {
      # Store the current q (first two dimensions for plotting)
      plot_points <- rbind(plot_points, q[1:2])
    }
    q <- q + epsilon*p
    if (i!=L) p <- p - epsilon*gradU(q)
  }
  p <- p - epsilon*gradU(q)/2 # End with half step
  list(newp=p, newq=q, plot_points=plot_points)
}
##########################################################################################

hmc <- function(t,N,epsilon,L,trace) {
  leapfrog_points_2 <- NULL
  for (i in 2:N) {
    U_func <- create_U_function(t)
    gradU <- create_U_grad(t)
    currentp <- rnorm(3, 0, 1)
    # To compute the leapfrog result, plotting only when i==2:  
    res <- leapfrog(currentp, trace[i-1,], gradU, epsilon, L, i==2) 
    if (i == 2) {
      leapfrog_points_2 <- res$plot_points
      }
    if (runif(1)<exp(- U_func(res$newq) - sum(res$newp^2)/2 +
                    U_func(trace[i-1,]) + sum(currentp^2)/2))
      trace[i,] <- res$newq
    else
      trace[i,] <- trace[i-1,]
  }
  return(list(trace=trace, leapfrog_points = leapfrog_points_2))
}

##########################################################################################
N     <- 50
epsilon <- 0.05
L     <- 20
trace <- matrix(0, N, 3)
# Set q0: 
trace[1,] <- c(-1, -2, 1)
fixed_t = 3.4



test <- hmc(fixed_t,N,epsilon,L, trace)

# Compute empirical mean and covariance matrix from the trace
mu_emp <- colMeans(test$trace[,1:2])
Sigma <- cov(test$trace[,1:2])
P     <- solve(Sigma) # Precision matrix


# Plotting an ellipse: 
ellplot <- function(Sigma, mu=mu_emp, r=1, N=1001) {
  ang  <- seq(0, 2*pi, length.out=N) 
  pts  <- r*cbind(cos(ang), sin(ang))
  lines(pts%*%chol(Sigma) + matrix(mu, N, 2, byrow=T))
}

png("hmc_fixed_t.png", width = 1000, height = 1000)
par(mar = c(5, 5, 5, 5), cex.axis = 1.5, cex.lab = 1.5, cex.main = 2)
plot(c(-17, 10), c(-17, 10), type="n", xlab = "X-axis", ylab = "Y-axis", main="HMC, t=3.4")
ellplot(Sigma,r=1); ellplot(Sigma,r=2); ellplot(Sigma, r=3)
points(test$leapfrog_points[,1], test$leapfrog_points[,2], col="red")
lines(test$trace, col="blue")
invisible(dev.off())

##########################################################################################
## Function to compute the probability that party A wins a majority

compute_win_probability <- function(trace, data, t) {
    win_indicator <- numeric(nrow(trace))

  for (i in 1:nrow(trace)) {
  
  # Count seats already won by A (where p_i > 0 and t_i <= t)
    known_seats <- sum(data$finalResults[data$timeReceived <= t & !is.na(data$finalResults)] > 0, na.rm = TRUE)

  # Predict outcomes for constituencies with only poll data
   poll_indices <- which(!is.na(data$exitPollResults) & (is.na(data$finalResults) | data$timeReceived> t))

    q_prev <- data$ResultPreviousElection[poll_indices]

    mu <- trace[i, 1]
    gamma <- trace[i, 2]
    psi <- trace[i, 3]
    # Reverse log-prob
    prob_prev <- plogis(psi + q_prev)

    #Simulate the number of seats using the prob
    seats_poll <- rbinom(length(poll_indices), 1, prob_prev)
    known_seats <- known_seats + sum(seats_poll)
    # Predict outcomes for constituencies with no data
    no_data_indices <- which(is.na(data$exitPollResults) & (is.na(data$finalResults) | data$timeReceived > t))
    q_no_data <- data$ResultPreviousElection[no_data_indices]
  
    d_no_data <- rnorm(length(no_data_indices), mu, exp(-gamma/2))

    #reverse log prob
    prob_no_data <- plogis(d_no_data + q_no_data)

    #approx # of seats
    seats_no_data <- rbinom(length(no_data_indices), 1, prob_no_data)
    known_seats <- known_seats + sum(seats_no_data)

    # Determine if A wins a majority
    win_indicator[i] <- known_seats >= 116
    
  }

  # Compute the probability that A wins a majority
  win_probability <- mean(win_indicator)
  return(win_probability)
}

##########################################################################################

# Get unique time points where new data arrives
time_points <- sort((data$timeReceived[!is.na(data$timeReceived)]))

# Initialize vectors to store the values
actual_seats <- numeric(length(time_points))
probabilities <- numeric(length(time_points))
total_seats <- 231 

N     <- 100
epsilon <- 0.05
L     <- 20
trace_full <- matrix(0, N, 3)
trace_full[1,] <- c(-.4, 6, 0)

# Compute values at each time point
for (i in 1:length(time_points)) {
  t <- time_points[i]
  trace_iter <- hmc(t, N, epsilon, L, trace_full)

  # Count actual seats won by time t
  seats_won <- sum(data$finalResults[data$timeReceived <= t & !is.na(data$finalResults)] > 0, na.rm = TRUE)
  actual_seats[i] <- seats_won / total_seats

  # Calculate probability of winning majority
  probabilities[i] <- compute_win_probability(trace_iter$trace, data, t)
}

# Create the plot
png("probabilitiesandproportion.png", width = 700, height = 700)
par(mar = c(5, 5, 5, 5), cex.axis = 1.5, cex.lab = 1.5, cex.main = 2)
plot(time_points, actual_seats,
     type = "l",
     col = "blue",
     ylim = c(0, 1),
     xlab = "Time",
     ylab = "",
     main = "Election progress and win probability")
# Add the probability line
lines(time_points, probabilities, col = "red")
# Add legend
legend("topleft",
       legend = c("Proportion of seats won", "Win probability"),
       col = c("blue", "red"),
       lty = 1)
# Add horizontal line at 0.5 (majority)
abline(h = 0.5, lty = 2, col = "black")
invisible(dev.off())
