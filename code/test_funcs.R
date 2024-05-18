ackley <- function(xx, a=20, b=0.2, c=2*pi)
{
  ##########################################################################
  #
  # ACKLEY FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUTS:
  #
  # xx = c(x1, x2, ..., xd)
  # a = constant (optional), with default value 20
  # b = constant (optional), with default value 0.2
  # c = constant (optional), with default value 2*pi
  #
  ##########################################################################
  
  d <- length(xx)
  
  sum1 <- sum(xx^2)
  sum2 <- sum(cos(c*xx))
  
  term1 <- -a * exp(-b*sqrt(sum1/d))
  term2 <- -exp(sum2/d)
  
  y <- term1 + term2 + a + exp(1)
  return(y)
}


goldpr <- function(xx)
{
  ##########################################################################
  #
  # GOLDSTEIN-PRICE FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  fact1a <- (x1 + x2 + 1)^2
  fact1b <- 19 - 14*x1 + 3*x1^2 - 14*x2 + 6*x1*x2 + 3*x2^2
  fact1 <- 1 + fact1a*fact1b
  
  fact2a <- (2*x1 - 3*x2)^2
  fact2b <- 18 - 32*x1 + 12*x1^2 + 48*x2 - 36*x1*x2 + 27*x2^2
  fact2 <- 30 + fact2a*fact2b
  
  y <- fact1*fact2
  return(y)
}

goldprsc <- function(xx)
{
  ##########################################################################
  #
  # GOLDSTEIN-PRICE FUNCTION, SCALED
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1bar <- 4*xx[1] - 2
  x2bar <- 4*xx[2] - 2
  
  fact1a <- (x1bar + x2bar + 1)^2
  fact1b <- 19 - 14*x1bar + 3*x1bar^2 - 14*x2bar + 6*x1bar*x2bar + 3*x2bar^2
  fact1 <- 1 + fact1a*fact1b
  
  fact2a <- (2*x1bar - 3*x2bar)^2
  fact2b <- 18 - 32*x1bar + 12*x1bar^2 + 48*x2bar - 36*x1bar*x2bar + 27*x2bar^2
  fact2 <- 30 + fact2a*fact2b
  
  prod <- fact1*fact2
  
  y <- (log(prod) - 8.693) / 2.427
  return(y)
}

dim = 2
ackley_lbound_scalar = -32.768
ackley_ubound_scalar = 32.768
ackley_lbound = rep(ackley_lbound_scalar, dim)
ackley_ubound = rep(ackley_ubound_scalar, dim)
ackley_argmin = rep(0, dim)