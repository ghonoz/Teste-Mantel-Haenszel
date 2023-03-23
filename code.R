maentel_haeszer_test <- function(x, correct = FALSE) { 
  dimensao <- as.list(dim(x))[[3]]
  
  lista <- list()
  
  for(i in 1:dimensao) {
    lista[[i]] <- x[, , i]
  }
  
  ratio <- function(x) {
    (x[1, 1]*x[2, 2])/sum(x)
  }
  
 
  ratio2 <- function(x) {
    (x[1, 2]*x[2, 1])/sum(x)
  }
  

  soma1 <- Reduce('+', lapply(lista, ratio)) # Certo
  soma2 <- Reduce('+', lapply(lista, ratio2)) # Certo
  
  odds_ratio_estimator <- soma1/soma2 # OK 
  
  
  
  
  
  
  f_statistic <- function(x)
  {
    (x[1,1]*x[2,2])*((x[1,1]+x[2,2])/(x[1,1]+x[2,1]+x[1,2]+x[2,2])^2)
  }
  
 
  
  soma_f <- Reduce('+', lapply(lista, f_statistic)) # Certo
  
  # R = (ai*di)/ni
  
  r_statistic_squared <- function(x) {
    ((x[1, 1]*x[2, 2])/sum(x))^2
  }
  
  r_statistic <- function(x) {
    (x[1,1]*x[2,2])/sum(x)
  }
  
  soma_r_squared <- Reduce('+', lapply(lista, r_statistic_squared)) # Certo
  soma_r         <- Reduce('+', lapply(lista, r_statistic)) # Certo
  
  # G = ((ai*bi(bi+ci)) + (bi*ci(aidi))/ni^2
  
  g_statistic <- function(x) {
    (x[1,1]*x[2,2]*(x[1,2]+x[2,1])+x[1,2]*x[2,1]*(x[1,1]+x[2,2]))/(x[1,1]+x[2,2]+x[1,2]+x[2,1])^2
    
  }
  
  soma_g <- Reduce('+', lapply(lista, g_statistic)) # Certo
  
  # H = (bici(bi+ci))/ni^2
  
  h_statistic <- function(x) {
    ((x[1, 2]*x[2, 1])*(x[1, 2]+x[2, 1]))/(sum(x)^2)
  }
  #fantoche
  
  #((bi*ci)*(bi+ci))/(sum(fantoche)^2)
  
  soma_h <- Reduce('+', lapply(lista, h_statistic)) # Certo
  
  s_statistic <- function(x) {
    (x[1, 2]*x[2, 1])/sum(x)
  }
  
  s_statistic_squared <- function(x) {
    ((x[1, 2]*x[2, 1])/sum(x))^2
  }
  
  soma_s <- Reduce('+', lapply(lista, s_statistic)) # Certo
  soma_s_squared <- Reduce('+', lapply(lista, s_statistic_squared)) # Certo
  
  # Todas estão certas..
  
  
  var_odds.ratio <- (soma_f/(2*soma_r_squared)) + (soma_g/(2*soma_r*soma_s)) + (soma_h/(2*soma_s_squared))
  print(var_odds.ratio)
  
  
  var_ln.odds.ratio <- var_odds.ratio/(odds_ratio_estimator^2)
  
  ic_superior <- exp(log(odds_ratio_estimator) + 1.96*sqrt(var_ln.odds.ratio))
  ic_inferior <- exp(log(odds_ratio_estimator) - 1.96*sqrt(var_ln.odds.ratio))
  
  
  estimator_variance <- function(x) {
    ((x[1, 1]+x[1, 2])*(x[2, 1]+x[2, 2])*(x[1, 1]*x[2, 1])*(x[1, 2]+x[2, 2]))/((sum(x)^2)*(sum(x)-1))
  }
  
  soma_estimador_variance <- Reduce('+', lapply(lista, estimator_variance))
  
  expectation_estimator <- function(x) {
    ((x[1, 1]+x[1, 2])*(x[1, 1]+x[2, 1]))/sum(x)
  }
  
  soma_expectation_estimator <- Reduce('+', lapply(lista, expectation_estimator))
  
  soma_a <- function(x) {
    x[1, 1]
  }
  
  sum_a <- Reduce('+', lapply(lista, soma_a))
  paste0(ic_inferior, ic_superior)
  
  if (correct == TRUE) {
    mantel_haenszer_statistic <- ((abs(sum_a - soma_expectation_estimator) - 0.5)^2)/soma_estimador_variance
    
  } else {
    mantel_haenszer_statistic <- ((sum_a - soma_expectation_estimator)^2)/soma_estimador_variance
  }
  
  # Rejeitamos a hipótese nula se o valor de da estatística for igual ou maior que o valor crítico da estatística do teste,
  # Que é o valor do qui-quadrado tabelado para 1 grau de liberdade e o nível de significância escolhido (retirado do seu material)
  
  if (correct == TRUE) {
    print('Mantel-Haenszel chi-squared test with continuity correction')
  } else {
    print('Mantel-Haenszel chi-squared test without continuity correction')
  }
  
  p_value <- pchisq(mantel_haenszer_statistic, 1, lower.tail = FALSE)
  
  paste0('data: ', x)
  paste0('Mantel-Haenszel X-squared = ', mantel_haenszer_statistic, 'df = 1, p-value: ', p_value)
  paste0('Alternative hypothesis: true common odds ratio is not equal to 1')
  print('95 percent confidence interval: ')
  paste(ic_inferior, ic_superior, sep = " ")
  paste0('Common odds ratio: ', odds_ratio_estimator)
  
  if (p_value  < 0.05) {
    print('Como o p valor é menor do que 0.05, rejeitamos a hipótese nula')
  } else {
    print('Como o p valor é maior do que 0.05, falhamos em rejeitar a hipótese nula')
  }
  
}

data(Titanic, package = 'datasets')
partial_tables <- margin.table(Titanic, c(2,4,1))
mantelhaen.test(partial_tables)
maentel_haenszer_test(partial_tables)

teste31 <- array(c(1011, 390, 81, 77, 383, 365, 66, 123), dim = c(2, 2, 2))
teste31
maentel_haeszer_test(teste31)
