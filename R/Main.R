
#' @import LaplacesDemon qpdf
#' @importFrom stats integrate pnorm qnorm rnorm
NULL

#' Simulating a 2-Stages SMART with Binary Final Outcome
#' @description simulates a 2-stages SMART with binary final outcome in the
#' standard design where responders to the first treatment are not
#' re-randomized.
#' @param n sample size of the simulated trial.
#' @param p_A,p_B response rates to initial treatments.
#' @param p_AC,p_AD,p_AA,p_BE,p_BF,p_BB response rates to stage-2 treatments for each sequence of treatments.
#' @return a data frame with \code{n} rows and 4 columns. For each observation the
#' matrix provides the first treatment label, sequence of treatments label,
#' final outcome, and response indicator to the 1-stage intervention
#' (1 for responders and 0 for non-responders).
#' @export
#'
#' @examples SMART.binary(60, 0.5, 0.3, 0.3, 0.2, 0.5, 0.2, 0.2, 0.2 )
SMART.binary <-  function(n, p_A, p_AC, p_AD, p_AA, p_B, p_BE, p_BF, p_BB ){

  finalout <- c()
  R <- c()
  path <- matrix(NA, ncol=2, nrow=n)


  for (i in 1:n) {


    r1        <- sample(c("A","B"), 1)
    path[i,1] <- r1
    if (r1=="A") {

      outcome <- as.numeric(rnorm(1, mean = 1, sd = 1)< qnorm(p_A, mean = 1, sd = 1))
      R[i] <- outcome
    }else{
      outcome <- as.numeric(rnorm(1, mean = 1, sd = 1)< qnorm(p_B, mean = 1, sd = 1))
      R[i] <- outcome
    }

    if (outcome==0){

      if (r1=="A"){

        r2 <- sample(c("AC","AD"), 1)


        if (r2=="AC") {

          outcome <- as.numeric(rnorm(1, mean = 1, sd = 1)< qnorm(p_AC, mean = 1, sd = 1))

        }else{
          outcome <- as.numeric(rnorm(1, mean = 1, sd = 1)< qnorm(p_AD, mean = 1, sd = 1))
        }

      }else{

        r2 <- sample(c("BE","BF"), 1)
        if (r2=="BE") {

          outcome <- as.numeric(rnorm(1, mean = 1, sd = 1)< qnorm(p_BE, mean = 1, sd = 1))

        }else{
          outcome <- as.numeric(rnorm(1, mean = 1, sd = 1)< qnorm(p_BF, mean = 1, sd = 1))
        }
      }

    }else {

      if (r1=="A"){
        r2 <- "AA"
        outcome <- as.numeric(rnorm(1, mean = 1, sd = 1)< qnorm(p_AA, mean = 1, sd = 1))

      }else{
        r2 <- "BB"
        outcome <- as.numeric(rnorm(1, mean = 1, sd = 1)< qnorm(p_BB, mean = 1, sd = 1))
      }

    }
    path[i,2] <- r2
    finalout[i] <- outcome
  }
  return(data.frame(tr1 = path[,1], tr2 = path[,2], outcome = finalout, R = R))
}




#' Simulating a 2-Stages SMART With Continuous Final Outcome
#' @description simulates a 2-stages SMART with binary final outcome in the
#' standard design where responders to the first treatment are not
#' re-randomized.
#' @param n sample size of the simulated trial.
#' @param p_A,p_B response rates to initial treatments.
#' @param phi_1,phi_2,phi_3,phi_4,phi_5,phi_6 parameters of the outcome conditional mean model.
#' @param sd_aa,sd_ac,sd_ad,sd_bb,sd_be,sd_bf standard deviation values of the conditional
#' outcome distribution.
#'
#' @return A data frame with \code{n} rows and 4 columns. For each observation, the
#' matrix provides the first treatment label, sequence of treatments label,
#' final outcome, and response indicator to the 1-stage intervention
#' (1 for responders and 0 for non-responders).
#' @export
#'
#' @examples SMART.continuous(n = 100, p_A = 0.5, p_B = 0.5, phi_1 = 10, phi_2 = 5, phi_3 = -15,
#' phi_4 = -3, phi_5 = 10, phi_6 = -3, sd_aa = 2, sd_ac = 2, sd_ad = 2,
#' sd_bb = 2, sd_be = 3, sd_bf = 2)
#'
#'
SMART.continuous <-  function(n, p_A, p_B,  phi_1, phi_2, phi_3, phi_4,phi_5, phi_6,
                              sd_aa, sd_ac, sd_ad, sd_bb, sd_be, sd_bf){

  finalout <- c()
  R <- c()
  path <- matrix(NA, ncol=2, nrow=n)


  for (i in 1:n) {


    r1        <- sample(c("A","B"), 1)
    path[i,1] <- r1
    if (r1=="A") {

      outcome <- as.numeric(rnorm(1, mean = 1, sd = 1)< qnorm(p_A, mean = 1, sd = 1))
      R[i] <- outcome
    }else{
      outcome <- as.numeric(rnorm(1, mean = 1, sd = 1)< qnorm(p_B, mean = 1, sd = 1))
      R[i] <- outcome
    }

    if (outcome==0){

      if (r1=="A"){

        r2 <- sample(c("AC","AD"), 1)


        if (r2=="AC") {

          outcome <- rnorm(1, mean = phi_1 +phi_2 +phi_3 +phi_4 +phi_5 +phi_6 ,  sd = sd_ac )

        }else{
          outcome <- rnorm(1, mean = phi_1 +phi_2 +phi_3 +phi_4,  sd = sd_ad )
        }

      }else{

        r2 <- sample(c("BE","BF"), 1)
        if (r2=="BE") {

          outcome <- rnorm(1, mean = phi_1  +phi_3  +phi_5 ,  sd = sd_be )

        }else{
          outcome <- rnorm(1, mean = phi_1  +phi_3   ,  sd = sd_bf )
        }
      }

    }else{
      if (r1=="A"){
        r2 <- "AA"
        outcome <- rnorm(1, mean = phi_1 +phi_2 ,  sd = sd_aa )

      }else{
        r2 <- "BB"
        outcome <- rnorm(1, mean = phi_1,  sd = sd_bb )
      }

    }



    path[i,2] <- r2
    finalout[i] <- outcome
  }
  return(data.frame(tr1 = path[,1], tr2 = path[,2], outcome = finalout, R = R))
}





#' Computing the Power for a 2-Stages SMART
#' @description Computes the Bayesian marginal power for the comparison of two strategies
#' that start with a different initial treatment in a 2-stages SMART in the
#' standard design where responders to the first treatment are not re-randomized.
#' @param n sample size of the trial.
#' @param vn degrees of freedom of the inverse chi-squared distribution of tau_1+tau_2.
#' @param sigma_n non-centrality parameter of the inverse chi-squared distribution of tau_1+tau_2.
#' @param theta_0 mean of the analysis prior Normal density.
#' @param sigma_0 standard deviation of the analysis prior Normal density.
#' @param theta_d mean of the design prior Normal density.
#' @param sigma_d standard deviation of the design prior Normal density.
#' @param epsilon bayesian equivalent of type I error. 1-epsilon represents the
#' Bayesian significance level.
#' @param upper_int upper limit of the integral that marginalizes the Bayesian power function.
#' The default is \code{Inf}.
#'
#' @return Bayesian marginal power value.
#' @export
#'
SMART.power <- function(n,vn, sigma_n, theta_0, sigma_0, theta_d, sigma_d,
                        epsilon, upper_int = Inf){


  power <- function(mar,n){
    pnorm(
      (1/sqrt( (mar/n) + sigma_d^2) ) * (
        qnorm(epsilon)*sqrt(mar)*sqrt(mar + n*sigma_0^2)*(1/(n*sigma_0)) +
          mar*(theta_0)*(1/(n*sigma_0^2)) +
          theta_d
      ),
    )
  }

  integrand <- function(mar, n){dinvchisq(mar, df = vn, scale = sigma_n)*power(mar, n)}

  int_func <- function(n){integrate(integrand, lower = 0, upper = upper_int, n=n)$value}

  v_int_func <- Vectorize(int_func)

  out <- v_int_func(n)

  return(out)
}



#' Estimating the Strategy Mean and its Variance for a 2-Stages SMART
#' @description Computes the strategy mean and the estimator's variance (tau^2)
#' in a 2-stages SMART in the
#' standard design where responders to the first treatment are not re-randomized.
#' @param tr1 vector of length \code{n} indicating the stage-1 interventions.
#' @param tr2 vector of length \code{n} indicating the stage-2 interventions.
#' @param outcome vector of final outcomes.
#' @param R vector of responses to the stage-1 intervention
#' (1 for responders and 0 for non-responders).
#' @param id1 label of the stage-1 intervention used in \code{tr1} of the strategy in question.
#' @param id2 label of the stage-2 intervention used in \code{tr2} for non-responders of the strategy in question.
#'
#' @return A list of two values. \code{mu} is the estimated strategy mean and
#' \code{tau} the estimator's variance.
#' @export
#'
#' @examples sim.data <- SMART.continuous(n = 100, p_A = 0.5, p_B = 0.5, phi_1 = 10, phi_2 = 5, phi_3 = -15,
#' phi_4 = -3, phi_5 = 10, phi_6 = -3, sd_aa = 2, sd_ac = 2, sd_ad = 2,
#' sd_bb = 2, sd_be = 3, sd_bf = 2)
#'
#' SMART.est(tr1 = sim.data$tr1, tr2 = sim.data$tr2, outcome = sim.data$outcome,
#'           R = sim.data$R, id1 = "A", id2 = "AC")
#'
#'
#'
SMART.est <- function(tr1 , tr2, outcome, R, id1, id2){

  weights <- (tr1==id1)*2*( (1-R)*(tr2==id2)*2 + R)

  mu <- as.numeric((outcome%*%weights)/sum(weights)  )

  tau <- mean((weights^2)*((outcome-mu)^2))

  return(list(mu = mu, tau = tau))
}




#' Estimating the Sample Size of a 2-Stages SMART
#' @description Computes the required sample size
#' in a 2-stages SMART to achieve the desired power level in the
#' standard design where responders to the first treatment are not re-randomized. To estimate
#' the variance components, the function needs either data from the pilot study through
#' the parameters \code{(tr1, tr2, outcome, R, id1_study, id2_study, id1_ref, id2_ref)} or
#' the direct specification of \code{vn} and \code{sigma_n}.
#' @param n_grid vector of sample size values used to search for the optimal sample size.
#' @param theta_0 mean of the analysis prior Normal density. If \code{theta_0="pilot"}, this density
#' is centered at the difference between mean strategies estimated from the pilot study.
#' @param sigma_0 standard deviation of the analysis prior Normal density.
#' @param theta_d mean of the design prior Normal density.
#' @param sigma_d standard deviation of the design prior Normal density.
#' @param power target power level.
#' @param epsilon bayesian equivalent of type I error. 1-epsilon represents the
#' Bayesian significance level.
#' @param tr1 vector of length \code{n} indicating the stage-1 interventions.
#' @param tr2 vector of length \code{n} indicating the stage-2 interventions.
#' @param outcome vector of final outcomes.
#' @param R vector of responses to the stage-1 intervention
#' (1 for responders and 0 for non-responders).
#' @param id1_study label of the stage-1 intervention used in \code{tr1} of the strategy 1.
#' @param id2_study label of the stage-2 intervention used in \code{tr2} of the strategy 1.
#' @param id1_ref label of the stage-1 intervention used in \code{tr1} of the strategy 2.
#' @param id2_ref label of the stage-2 intervention used in \code{tr2} of the strategy 2.
#' @param v_p,k_p,u_p,sigma_p parameters of the Normal-inverse-chi-squared (NIX)
#' prior distribution used to estimate the posterior distribution of the variance
#' components. The default choice is \code{v_p=0.5,k_p=1,u_p=0,sigma_p=4}.
#' @param vn degrees of freedom of the inverse chi-squared distribution of tau_1+tau_2.
#' @param sigma_n non-centrality parameter of the inverse chi-squared distribution of tau_1+tau_2.
#' @param upper_int upper limit of the integral that marginalizes the Bayesian power function.
#' The default is \code{Inf}. If an error occurs regarding the integral being divergent
#' or the upper bound of n_grid being high enough despite increasing it, try selecting
#' a large finite value for \code{upper_int}.
#' @param save_grid logical; if TRUE, a the estimated power for each value of \code{n_grid}
#' is stored. The default is FALSE.
#'
#' @return By default, the function returns two values: the
#' estimated sample size and corresponding power. If \code{save_grid = F},
#' an additional data frame containing the estimated power for each value of
#' \code{n_grid} is returned.
#' @export
#'
#' @examples set.seed(123)
#' p_A <- 0.5 ; p_B <- 0.5
#' phi_1 <- 10 ; phi_2 <- 5 ; phi_3 <- -15; phi_4 <- -3; phi_5 <- 10; phi_6 <- -3
#' sd_aa = 2 ; sd_ac = 2 ; sd_ad = 2 ; sd_bb = 2 ; sd_be = 3; sd_bf = 2
#' sim <- SMART.continuous(n = 100, p_A = p_A, p_B = p_B, phi_1 = phi_1, phi_2 =phi_2,
#'                           phi_3=phi_3, phi_4=phi_4,phi_5=phi_5,  phi_6=phi_6,
#'                           sd_aa = sd_aa, sd_ac = sd_ac,
#'                           sd_ad = sd_ad,sd_bb = sd_bb,
#'                           sd_be = sd_be,sd_bf = sd_bf)
#'
#'  SMART.ss(n_grid = seq(1,7000), theta_0=0, sigma_0 = 0.2,
#'           theta_d=2, sigma_d = 0.1, power = 0.9, epsilon = 0.05,
#'           tr1 = sim$tr1, tr2 = sim$tr2, outcome = sim$outcome, R = sim$R,
#'           id1_study = "A", id2_study = "AB", id1_ref = "B", id2_ref = "BE")
#'
SMART.ss <- function(n_grid, theta_0, sigma_0, theta_d, sigma_d, power, epsilon,
                    tr1 = NULL , tr2 = NULL, outcome = NULL, R = NULL,
                    id1_study = NULL, id2_study = NULL, id1_ref = NULL, id2_ref = NULL,
                    v_p = 0.5, k_p = 1, u_p = 0, sigma_p = 4,
                    vn = NULL, sigma_n = NULL,
                    upper_int = Inf, save_grid = F){

  op1 <- list(tr1 , tr2, outcome, R, id1_study, id2_study, id1_ref, id2_ref, v_p, k_p)

  op2 <- list(vn, sigma_n)

  if(sum(sapply(c(op1,op2), is.null))==0|
     (sum(sapply(op1, is.null))>0 &  sum(sapply(op2, is.null))>0)){

    stop("Please specify either the parameters of the pilot study or vn & sigma_n")

  }else if (sum(sapply(op1, is.null))==0){

    par_study <- SMART.est(tr1 = tr1, tr2 = tr2, outcome = outcome,
                            R = R, id1 = id1_study, id2 = id2_study)
    par_ref   <- SMART.est(tr1 = tr1, tr2 = tr2, outcome = outcome,
                              R = R, id1 = id1_ref, id2 = id2_ref)

    mu_1 <- par_study$mu
    mu_2 <- par_ref$mu

    tau_1 <- par_study$tau
    tau_2 <- par_ref$tau

    n_pilot <- length(outcome)

    vn      <- v_p+n_pilot
    sigma_n <- (1/vn)*(v_p*sigma_p+(tau_1+tau_2)*n_pilot+n_pilot*k_p*(u_p-(mu_1-mu_2))/(k_p+n_pilot))

  }

  if(theta_0=="pilot"){
    theta_0 <- mu_1 - mu_2
  }
  power.grid <- SMART.power(n = n_grid, vn = vn, sigma_n = sigma_n, theta_0 = theta_0,
                            sigma_0 = sigma_0, theta_d = theta_d, sigma_d = sigma_d, epsilon = epsilon,
                            upper_int = upper_int)

  ss  <- n_grid[power.grid>=power][1]
  pow <- power.grid[power.grid>=power][1]

  if(is.na(ss)){
    stop("The upper bound of n_grid might be not high enough. Please try increasing it")
  }

  if(save_grid){
    return(list(data.frame(Sample_size=ss, Power = pow), data.frame(n=n_grid, Power = power.grid)))
  }else{
    return(data.frame(Sample_size=ss, Power = pow))
    }


}







