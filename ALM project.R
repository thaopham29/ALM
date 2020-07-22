#MonteCarlo Simulation for rt, St, Pt, and Vt 

N_Mont <- 10000 # Number of simulations
#rho: correlation between W_1(t) and W_2(t)
#sigma_r: volatility of r(t)
#a_r= mean reversion speed

# lambda1 
#  lambda2 
#Time  
#T_0: time to maturity
dt = 1/100  #discritize in 50 points for 1 year 
t0 <- seq(0,T_0,by = dt)
Npoint = length(t0)
x <- T_0 - t0
# Btx
Btx = 1/a_r*(1-exp(-a_r*x));


  #Generate random normal distribution matrices
dw1<- matrix(0:0, nrow = N_Mont, ncol = Npoint)
dz1 <-  matrix(0:0, nrow = N_Mont, ncol = Npoint)
dw2 <-  matrix(0:0, nrow = N_Mont, ncol = Npoint)

for (i in 1:N_Mont){
  for (j in 2:Npoint){
    dw1[i,j] <- rnorm(1,0,1)
    dz1[i,j] <- rnorm(1,0,1)
    dw2[i,j] <- rnorm(1,0,1)
    }
    
}
  
#Simulate r_t
#r_0: initial value for r(t), which could be obtained using Nelson-Siegel (NS) model.
r_t<- matrix(1:1, nrow = N_Mont, ncol = Npoint)*r_0 

for (i in 1:N_Mont){
  for (j in 2:Npoint){
    r_t[i,j] <- r_t[i,j - 1]+ a_r*(theta_t(j-1)-rt[i,j-1])*dt + sigma_r * sqrt(dt) * dw1[i,j-1]
  }
}
  
#Simulate mutual fund S_t
#rho: correlation between W_1(t) and W_2(t)
#S_0: initial value for mutual fund
S_t<- matrix(1:1, nrow = N_Mont, ncol = Npoint)*S_0 

for (i in 1:N_Mont){
  for (j in 2:Npoint){
    dSt <- rt[i,j - 1]*dt + sigma_s*dw2[i,j-1]*sqrt(dt)
    S_t[i,j] <- S_t[i,j - 1]+ St[i,i-1].*dSt
  }
}

#Simulate short term deposit B(t)
B_t<- matrix(1:1, nrow = N_Mont, ncol = Npoint)*B_0 
for (i in 1:N_Mont){
  for (j in 2:Npoint){
    B_t(i,j) = B_t(i,j-1).*exp(rt(i,j-1)*dt)
  }
}

#Simulate zero-coupon bond with maturity T_0
#P_0: initial value for P(t)
P_t<- matrix(1:1, nrow = N_Mont, ncol = Npoint)*P_0 
for (i in 1:N_Mont){
  for (j in 2:Npoint){
    dPt = rt[ni,i-1]*dt -sigma_r*Btx[j-1].*dw1[i,j-1]*sqrt(dt)
    P_t[i,j] = P_t[i,j-1] + Pt[i,j-1].*dPt
  }
}

#Simulate value of a portfolio with the combination of mutual fund and zero-coupon bond
# x_mf: Share of mutual fund S in V
#x_b <- # Share of zero-coupon bond P in V
#lambda_1 and lambda_2: market prices of risk
V_t<- matrix(1:1, nrow = N_Mont, ncol = Npoint)*V_0 
for (i in 1:N_Mont){
  for (j in 2:Npoint){
    dVt = (rt[i,j-1]+ lambda1*(0.2*sigma_s*rho-0.8*sigma_r*Btx[j-1])+
0.2*sigma_s*sqrt(1-rho^2)*lambda2)*dt -0.8*sigma_r*Btx[j-1].*dw1[i,j-1]*sqrt(dt) + 0.2*sigma_s*dw2[i,j-1]*sqrt(dt)
    V_t[i,j] = V_t[i,j-1] + V_t[i,j-1].*dVt2
  }
}