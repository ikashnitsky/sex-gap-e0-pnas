#===============================================================================
# 2021-03-12-- sex gap e0
# Own functions
# Virgina Zarulli, vzarulli@sdu.dk
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================


# kannisto smooth ---------------------------------------------------------

# Kannisto optimization
kannisto_loglk <- function(theta, D, E, x, pl = 1){
    a <- theta[1]
    b <- theta[2]
    mu <- (a*exp(b*(x-80)))/(1+(a/pl)*exp(b*(x-80)))
    loglk <- (D*log(mu)-(E*mu))+1
    return(-sum(loglk))
}

# a function that performs the smooth
kannisto_smooth <- function(mx, Dx, Ex, plateau = 1) {
    
    oldage = 80:110
    
    optim <-
        optim(
            par = c(.044, .044),
            fn = kannisto_loglk,
            method = "L-BFGS-B",
            lower = 0,
            upper = 5,
            x = oldage,
            D = Dx,
            E = Ex,
            pl = plateau
        )
    
    mx_smooth <- 
        ((optim$par[1]*exp(optim$par[2]*(oldage-80)))/
             (1+(optim$par[1]/plateau)*exp(optim$par[2]*(oldage-80))))
    
    return(c(mx[1:15], mx_smooth[16:31]))
}


# life table --------------------------------------------------------------

# a(0) from HMD protocol
a0sex <- function(m0,sex){ #sex: m=male f=female
    if (sex=="m"){
        if(m0<0.02300){
            a0 <- 0.14929-1.99545*m0 }
        else if (m0>=0.0230 & m0<0.08307){
            a0 <- 0.02832+3.26021*m0}
        else if (m0>=0.08307){
            a0 <- 0.29915}
    }
    else {
        if(m0<0.01724){
            a0 <- 0.14903-2.05527*m0 }
        else if (m0>=0.01724 & m0<0.06891){
            a0 <- 0.04667+3.88089*m0}
        else if (m0>=0.06891) {
            a0 <- 0.31411}
    }
    return(a0)
}

# life table for single age groups
lt <- function(mx, sex = c("f", "m"), a0 = NULL) {
    x <-  seq_along(mx) - 1
    ax <- rep(.5, length(x))
    if(!is.null(a0)){ax[1] <- a0}
    else {ax[1] <- a0sex(mx[1], sex)} # own func defined above
    nx = c(diff(x),NA)
    qx = ifelse(nx*mx/(1+(nx-ax)*mx)>1, 1, nx*mx/(1+(nx-ax)*mx))
    qx[length(qx)] = 1
    px = 1-qx
    lx = head(cumprod(c(1, px)), -1)
    dx = c(-diff(lx), tail(lx, 1))
    Lx = ifelse(mx==0, lx*nx, dx/mx)
    Tx = rev(cumsum(rev(Lx)))
    ex = Tx/lx
    ax.update <- c(ax[-length(ax)],ex[length(ex)])
    return(
        tibble(
            age = x,
            mx = mx,
            qx = qx,
            ax = ax.update,
            lx = lx,
            dx = dx,
            lx_2 = Lx,
            tx = Tx,
            ex = ex
        )
    )
}

# function for only e0
e0 <- function(mx) {
    lifet <- lt(mx, a0 = .3) 
    return(lifet$ex[1])
}
# for a0=.3 see: https://twitter.com/ikashnitsky/status/1370531536320266243

# function for only e0gap
e0gap <- function(mx_f_m) {
    mx_f = mx_f_m %>% head(length(mx_f_m)/2)
    mx_m = mx_f_m %>% tail(length(mx_f_m)/2)
    lifet_f <- lt(mx_f, a0 = .3) 
    lifet_m <- lt(mx_m, a0 = .3) 
    return(lifet_f$ex[1] - lifet_m$ex[1])
}


# 
# # decompose changes in e0 gap ---------------------------------------------
# 
# decompose_gap <- function (mx1, mx2, mx3, n, periods){
#     #create output data frame
#     output <- data.frame(age=n,
#                          age.groups=age_grouping(n),
#                          changegap1=NA,
#                          contribution1=NA,
#                          changegap2=NA,
#                          contribution2=NA,
#                          years=toString(periods))
#     
#     #decompose the gap between time 1 and time 2
#     decomposegap1 <- stepwise_replacement(func = e0gap,
#                                           pars1 = mx1, pars2 = mx2,
#                                           symmetrical = TRUE, direction = "up") 
#     #decompose the gap between time 2 and time 3
#     decomposegap2 <- stepwise_replacement(func = e0gap,
#                                           pars1 = mx2, pars2 = mx3,
#                                           symmetrical = TRUE, direction = "up") 
#     
#     #add up the age contributions 1+112, 2+113, 3+114...
#     for (i in 1:length(n)) {
#         output$contribution1[i] <- decomposegap1[i]+decomposegap1[i+length(n)]
#         output$contribution2[i] <- decomposegap2[i]+decomposegap2[i+length(n)]
#     }
#     output$changegap1 <- sum(decomposegap1)
#     output$changegap2 <- sum(decomposegap2)
#     return(output)
# }
