#Distribuição normal

p <- function(q, dist = "normal", lower.tail = TRUE, rounding = 4, 
              porcentage = FALSE, gui = "plot", ...) {
  
  argaddit <- list(...)
  argdef <- formals(p)
  if (dist == "normal") {
    
    if (!any(names(argaddit) == "mean")) stop("Insira o argumento 'mean'!", call. = FALSE)
    if (!any(names(argaddit) == "sd")) stop("Insira o argumento 'sd'!", call. = FALSE)
    
    if (lower.tail) {
      plotcurve <- function(q, mu, sigma) {
        curve(dnorm(x, mean = mu, sd = sigma), -6, 6, ylab = expression(f[T](t)), xlab="T")
        x <- seq(-6, q, by = 0.01)
        y <- seq(q, 6, by = 0.01)
        fx <- dnorm(x, mean = mu, sd = sigma)
        fy <- dnorm(y, mean = mu, sd = sigma)
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col="red")
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        abline(v=0, lty=2)
        qq <- round(q, digits=2)
        qqaux <-round(q, digits=2)
        Pr <- round(pnorm(qq,  mean = mu, sd=sigma, lower.tail = T), digits=rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, labels=qqaux,
             col="red", font = 2)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(P(T<=q)==Pr~"\n\n"~mean==mu, list(q=qq, Pr=Pr, mu = mu)))
      }
      if (gui == "plot" ) {
        # Probability
        mu <- argaddit$mean
        sigma <- argaddit$sd
        prob <- pnorm(q = q, mean = mu, sd = sigma)
        # Plot
        plotcurve(q, mu,sigma)
      }
      if (gui == "rstudio") {
        manipulate::manipulate(plotcurve(qaux, muaux),
                               qaux = manipulate::slider(-6, 6, q),
                               muaux = manipulate::slider(mu, mu + 200, mu))
      }
      
    } else{
      plotcurve <- function(q, mu, sigma) {
        curve(dnorm(x, mean = mu, sd=sigma), -6, 6, ylab = expression(f[T](t)), xlab="T")
        x <- seq(q, 6, by=0.01)
        y <- seq(-6, q, by=0.01)
        fx <- dnorm(x, mean = mu, sd = sigma)
        fy <- dnorm(y, mean = mu, sd = sigma)
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col="red")
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        abline(v=0, lty=2)
        qq <- round(q, digits=2)
        qqaux <-round(q, digits=2)
        Pr <- round(pnorm(qq, mean = mu, sd=sigma, lower.tail = F), digits=rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, labels=qqaux,
             col="red", font = 2)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(P(T~`>`~q)==Pr~"\n\n"~mean==mu, list(q=qq, Pr=Pr, mu = mu)))
      }
      if (gui == "plot") {
        # Probability
        mu <- argaddit$mean
        sigma <- argaddit$sd
        prob <- pnorm(q = q, mean = mu, sd=sigma)
        # Plot
        plotcurve(q, mu, sigma)
      }
      if (gui == "rstudio") {
        manipulate::manipulate(plotcurve(qaux, muaux),
                               qaux = manipulate::slider(-6, 6, q),
                               muaux = manipulate::slider(mu, mu + 200, mu))
      }
      
    }
  }
  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)
}