ackley <- function(xx, a=20, b=0.2, c=2*pi)
{
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
 
# cit <- function(xx)
# {
#   ##########################################################################
#   #
#   # INPUT:
#   #
#   # xx = c(x1, x2)
#   #
#   ##########################################################################
#   
#   x1 <- xx[1]
#   x2 <- xx[2]
#   
#   fact1 <- sin(x1)*sin(x2)
#   fact2 <- exp(abs(100 - sqrt(x1^2+x2^2)/pi))
#   
#   y <- -0.0001 * (abs(fact1*fact2)+1)^0.1
#   return(y)
# }
# 
# neg_copeak <- function(xx, u=rep(0.5, 1, length(xx)), a=rep(5, 1, length(xx)), theta = 0)
# {
#   ##########################################################################
#   #
#   # INPUTS:
#   #
#   # xx = c(x1, x2, ..., xd)
#   # u = c(u1, u2, ..., ud) (optional), with default value
#   #     c(0.5, 0.5, ..., 0.5)
#   # a = c(a1, a2, ..., ad) (optional), with default value c(5, 5, ..., 5)
#   #
#   #########################################################################
#   
#   d <- length(xx)
#   rot_mat = rbind(c(cos(theta), -sin(theta)), c(sin(theta), cos(theta)))
#   disp = rep(0.5, d)
#   rotated_coords = disp + rot_mat %*% (xx - disp)
#   xx = rotated_coords
#   
#   sum <- sum(a*xx)
#   
#   y <- (1 + sum)^(-d-1)
#   return(-y)
# }
# 
# neg_fourpeak <- function(xx){
#   return(neg_copeak(xx) + 0.4*neg_copeak(xx, theta = pi/2)
#          + 0.5*neg_copeak(xx, theta = pi) + 0.3*neg_copeak(xx, theta = 3*pi/2))
# }
# 
# goldpr <- function(xx)
# {
#   ##########################################################################
#   #
#   # INPUT:
#   #
#   # xx = c(x1, x2)
#   #
#   ##########################################################################
#   
#   x1 <- xx[1]
#   x2 <- xx[2]
#   
#   fact1a <- (x1 + x2 + 1)^2
#   fact1b <- 19 - 14*x1 + 3*x1^2 - 14*x2 + 6*x1*x2 + 3*x2^2
#   fact1 <- 1 + fact1a*fact1b
#   
#   fact2a <- (2*x1 - 3*x2)^2
#   fact2b <- 18 - 32*x1 + 12*x1^2 + 48*x2 - 36*x1*x2 + 27*x2^2
#   fact2 <- 30 + fact2a*fact2b
#   
#   y <- fact1*fact2
#   return(y)
# }
# 
# goldprsc <- function(xx)
# {
#   ##########################################################################
#   #
#   # INPUT:
#   #
#   # xx = c(x1, x2)
#   #
#   ##########################################################################
#   
#   x1bar <- 4*xx[1] - 2
#   x2bar <- 4*xx[2] - 2
#   
#   fact1a <- (x1bar + x2bar + 1)^2
#   fact1b <- 19 - 14*x1bar + 3*x1bar^2 - 14*x2bar + 6*x1bar*x2bar + 3*x2bar^2
#   fact1 <- 1 + fact1a*fact1b
#   
#   fact2a <- (2*x1bar - 3*x2bar)^2
#   fact2b <- 18 - 32*x1bar + 12*x1bar^2 + 48*x2bar - 36*x1bar*x2bar + 27*x2bar^2
#   fact2 <- 30 + fact2a*fact2b
#   
#   prod <- fact1*fact2
#   
#   y <- (log(prod) - 8.693) / 2.427
#   return(y)
# }

grie <- function(xx)
{
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  ii <- c(1:length(xx))
  sum <- sum(xx^2/4000)
  prod <- prod(cos(xx/sqrt(ii)))
  
  y <- sum - prod + 1
  return(y)
}

langer <- function(xx, m=5, cvec, A)
{
  ##########################################################################
  #
  # INPUTS:
  #
  # xx   = c(x1, x2, ..., xd)
  # m    = constant (optional), with default value 5
  # cvec = m-dimensional vector (optional), with default value c(1, 2, 5, 2, 3)
  #        (when m=5)
  # A    = (mxd)-dimensional matrix (optional), with default value:
  #        [3  5]
  #        [5  2]
  #        [2  1]
  #        [1  4]
  #        [7  9]
  #        (when m=5 and d=2)
  #
  ##########################################################################
  
  d <- length(xx)
  
  if (missing(cvec)) {
    if (m == 5){
      cvec <- c(1,2,5,2,3)
    }
    else {
      stop('Value of the m-dimensional vector cvec is required.')
    }
  }
  
  # if (missing(A)) {
  #   if (m==5 && d==2) {
  #     A <- matrix(c(3,5,5,2,2,1,1,4,7,9),5,2,byrow=TRUE)
  #   }
  #   else {
  #       stop('Value of the (mxd)-dimensional matrix A is required.')
  #   }
  # }

  if(missing(A)) {
    A <- matrix(c(3,5,5,2,2,1,1,4,7,9),5,2,byrow=TRUE)

    if (d %% 2 == 0) {
      A <- matrix(rep(A, d/2), ncol = d)
    } else {
      A <- matrix(rep(A, ceiling(d/2)), ncol = d + 1)
      A <- A[, -ncol(A)]
    }
  }

  xxmat <- matrix(rep(xx,times=m), m, d, byrow=TRUE)
  inner <- rowSums((xxmat-A[,1:d])^2)	
  outer <- sum(cvec * exp(-inner/pi) * cos(pi*inner))
	
  y <- outer
  return(y)
}

levy <- function(xx)
{
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
  w <- 1 + (xx - 1)/4
	
  term1 <- (sin(pi*w[1]))^2 
  term3 <- (w[d]-1)^2 * (1+1*(sin(2*pi*w[d]))^2)
	
  wi <- w[1:(d-1)]
  sum <- sum((wi-1)^2 * (1+10*(sin(pi*wi+1))^2))
	
  y <- term1 + sum + term3
  return(y)
}

michal <- function(xx, m=10)
{
  ##########################################################################
  #
  # INPUTS:
  #
  # xx = c(x1, x2)
  # m = constant (optional), with default value 10
  #
  ##########################################################################
  
  ii <- c(1:length(xx))
  sum <- sum(sin(xx) * (sin(ii*xx^2/pi))^(2*m))
	
  y <- -sum
  return(y)
}

rastr <- function(xx)
{
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
  
  sum <- sum(xx^2 - 10*cos(2*pi*xx))
  
  y <- 10*d + sum
  return(y)
}

schwef <- function(xx)
{
##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
	
  sum <- sum(xx*sin(sqrt(abs(xx))))

  y <- 418.9829*d - sum
  return(y)
}

stybt <- function(xx)
{
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  sum <- sum(xx^4 - 16*xx^2 + 5*xx)
	
  y <- sum/2
  return(y)
}

# dim = 2

# ackley_lbound_scalar = -32.768
# ackley_ubound_scalar = 32.768
# ackley_lbound = rep(ackley_lbound_scalar, dim)
# ackley_ubound = rep(ackley_ubound_scalar, dim)
# 
# ackley_x1_split_point = mean(ackley_ubound[1] + ackley_lbound[1])
# ackley_reg_1_lbound = ackley_lbound
# ackley_reg_1_ubound = ackley_ubound
# ackley_reg_1_ubound[1] = ackley_x1_split_point
# # ackley_reg_1_ubound = c(ackley_x1_split_point, ackley_ubound_scalar)
# # ackley_reg_2_lbound = c(ackley_x1_split_point, ackley_lbound_scalar)
# ackley_reg_2_lbound = ackley_lbound
# ackley_reg_2_lbound[1] = ackley_x1_split_point
# ackley_reg_2_ubound = ackley_ubound
# ackley_reg_1 = cbind(ackley_reg_1_lbound, ackley_reg_1_ubound)
# ackley_reg_2 = cbind(ackley_reg_2_lbound, ackley_reg_2_ubound)
# 
# ackley_argmin = rep(0, dim)
# 
# cit_lbound_scalar = -10
# cit_ubound_scalar = 10
# cit_lbound = rep(cit_lbound_scalar, dim)
# cit_ubound = rep(cit_ubound_scalar, dim)
# 
# cit_reg_1_lbound = cit_lbound
# cit_reg_1_ubound = rep(0, dim)
# cit_reg_1 = cbind(cit_reg_1_lbound, cit_reg_1_ubound)
# 
# cit_argmin = rbind(c(1.3491, -1.3491),
#                    c(1.3491, 1.3491),
#                    c(-1.3491, 1.3491),
#                    c(-1.3491, -1.3491)
# )
# 
# neg_copeak_lbound_scalar = 0
# neg_copeak_ubound_scalar = 1
# neg_copeak_lbound = rep(neg_copeak_lbound_scalar, dim)
# neg_copeak_ubound = rep(neg_copeak_ubound_scalar, dim)
# 
# neg_copeak_argmin = rep(0, dim)
# 
# grie_lbound_scalar = -600
# grie_ubound_scalar = 600
# grie_lbound = rep(grie_lbound_scalar, dim)
# grie_ubound = rep(grie_ubound_scalar, dim)
# 
# grie_x1_split_point = mean(grie_ubound[1] + grie_lbound[1])
# grie_reg_1_lbound = grie_lbound
# grie_reg_1_ubound = grie_ubound
# grie_reg_1_ubound[1] = grie_x1_split_point
# # grie_reg_1_ubound = c(grie_x1_split_point, grie_ubound_scalar)
# # grie_reg_2_lbound = c(grie_x1_split_point, grie_lbound_scalar)
# grie_reg_2_lbound = grie_lbound
# grie_reg_2_lbound[1] = grie_x1_split_point
# grie_reg_2_ubound = grie_ubound
# grie_reg_1 = cbind(grie_reg_1_lbound, grie_reg_1_ubound)
# grie_reg_2 = cbind(grie_reg_2_lbound, grie_reg_2_ubound)
# 
# grie_argmin = rep(0, dim)
# 
# rastr_lbound_scalar = -5.12
# rastr_ubound_scalar = 5.12
# rastr_lbound = rep(rastr_lbound_scalar, dim)
# rastr_ubound = rep(rastr_ubound_scalar, dim)
# 
# rastr_x1_split_point = mean(rastr_ubound[1] + rastr_lbound[1])
# rastr_reg_1_lbound = rastr_lbound
# rastr_reg_1_ubound = rastr_ubound
# rastr_reg_1_ubound[1] = rastr_x1_split_point
# # rastr_reg_1_ubound = c(rastr_x1_split_point, rastr_ubound_scalar)
# # rastr_reg_2_lbound = c(rastr_x1_split_point, rastr_lbound_scalar)
# rastr_reg_2_lbound = rastr_lbound
# rastr_reg_2_lbound[1] = rastr_x1_split_point
# rastr_reg_2_ubound = rastr_ubound
# rastr_reg_1 = cbind(rastr_reg_1_lbound, rastr_reg_1_ubound)
# rastr_reg_2 = cbind(rastr_reg_2_lbound, rastr_reg_2_ubound)
# 
# rastr_argmin = rep(0, dim)

# test_func_dict = c(
#   "rastr" = c(
#     "lbound_scalar" = -5.12,
#     "ubound_scalar" = 5.12,
#     "lbound" = rep(-5.12, dim),
#     "ubound" = rep(5.12, dim),
#     "argmin" = rep(0, dim)
#   )
# )

test_func_list = list(
  ackley = list(
    func = ackley,
    lbound_scalar = -32.768,
    ubound_scalar = 32.768,
    lbound = rep(-32.768, dim),
    ubound = rep(32.768, dim),
    argmin = rep(0, dim)
  ),
  grie = list(
    func = grie,
    lbound_scalar = -600,
    ubound_scalar = 600,
    lbound = rep(-600, dim),
    ubound = rep(600, dim),
    argmin = rep(0, dim)
  ),
  langer = list(
    func = langer,
    lbound_scalar = 0,
    ubound_scalar = 10,
    lbound = rep(0, dim),
    ubound = rep(10, dim),
    argmin = rep(0, dim) # I don't know the argmin.
  ),
  levy = list(
    func = levy,
    lbound_scalar = -10,
    ubound_scalar = 10,
    lbound = rep(-10, dim),
    ubound = rep(10, dim),
    argmin = rep(1, dim)
  ),
  michal = list(
    func = michal,
    lbound_scalar = 0,
    ubound_scalar = pi,
    lbound = rep(0, dim),
    ubound = rep(pi, dim),
    argmin = rep(0, dim) # I don't know the argmin in arbitrary dimension.
  ),
  rastr = list(
    func = rastr,
    lbound_scalar = -5.12,
    ubound_scalar = 5.12,
    lbound = rep(-5.12, dim),
    ubound = rep(5.12, dim),
    argmin = rep(0, dim)
  ),
  schwef = list(
    func = schwef,
    lbound_scalar = -500,
    ubound_scalar = 500,
    lbound = rep(-500, dim),
    ubound = rep(500, dim),
    argmin = rep(420.9687, dim)
  ),
  stybt = list(
    func = stybt,
    lbound_scalar = -5,
    ubound_scalar = 5,
    lbound = rep(-5, dim),
    ubound = rep(5, dim),
    argmin = rep(-2.903534, dim)
  )
)
