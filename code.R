mantel_haenszel_test <- function(x, correct = c(TRUE, FALSE), confidence_level) { 
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
  
  r_statistic <- function(x) {
    (x[1,1]*x[2,2])/(x[1,1]+x[2,2]+x[2,1]+x[1,2])
  }
  
  soma_r         <- Reduce('+', lapply(lista, r_statistic)) # Certo
  
  g_statistic <- function(x) {
    (x[1,1]*x[2,2]*(x[1,2]+x[2,1])+x[1,2]*x[2,1]*(x[1,1]+x[2,2]))/(x[1,1]+x[2,2]+x[1,2]+x[2,1])^2
    
  }
  
  soma_g <- Reduce('+', lapply(lista, g_statistic)) # Certo
  
  
  h_statistic <- function(x) {
    ((x[1, 2]*x[2, 1])*(x[1, 2]+x[2, 1]))/(x[1,1]+x[2,2]+x[2,1]+x[1,2])^2
  }
  
  
  soma_h <- Reduce('+', lapply(lista, h_statistic)) # Certo
  
  s_statistic <- function(x) {
    (x[1, 2]*x[2, 1])/sum(x)
  }
  
  
  soma_s <- Reduce('+', lapply(lista, s_statistic)) # Certo
  
  # Todas est�o certas..
  
  
  var_odds.ratio <- round(soma_f/(2*(soma_r^2)), digits = 4) + 
    round(soma_g/(2*soma_r*soma_s), digits = 4) + 
    round(soma_h/(2*(soma_s^2)), digits = 4)
  
  
  var_ln.odds.ratio <- var_odds.ratio/(odds_ratio_estimator^2) # Certo
  
  significance_level <- 1-confidence_level
  z <- 1 - significance_level/2
  z_calculator <- 1 - z
  
  
  ic_superior <- exp(log(odds_ratio_estimator) + qnorm(z_calculator, lower.tail = FALSE)*sqrt(var_ln.odds.ratio)) # Certo
  ic_inferior <- exp(log(odds_ratio_estimator) - qnorm(z_calculator, lower.tail = FALSE)*sqrt(var_ln.odds.ratio)) # Certo
  
  # Tudo pra cima t� certo..
  estimator_variance <- function(x) {
    ((x[1,1]+x[1,2])*(x[1,1]+x[2,1])*(x[1,2]+x[2,2])*(x[2,1]+x[2,2]))/(sum(x)^2*(sum(x)-1))
  }
  
  soma_estimador_variance <- Reduce('+', lapply(lista, estimator_variance)) # Certo
  
  expectation_estimator <- function(x) {
    ((x[1, 1]+x[1, 2])*(x[1, 1]+x[2, 1]))/sum(x)
  }
  
  soma_expectation_estimator <- Reduce('+', lapply(lista, expectation_estimator)) # Certo
  
  soma_linha_1coluna_1 <- function(x) {
    x[1, 1]
  }
  
  sum_a <- Reduce('+', lapply(lista, soma_a)) # Certo
  
  
  if (correct == TRUE) {
    mantel_haenszer_statistic <- ((abs(sum_a - soma_expectation_estimator) - 0.5)^2)/soma_estimador_variance
    
  } else {
    mantel_haenszer_statistic <- ((sum_a - soma_expectation_estimator)^2)/soma_estimador_variance
  }
  
  # Rejeitamos a hip�tese nula se o valor de da estat�stica for igual ou maior que o valor cr�tico da estat�stica do teste,
  # Que � o valor do qui-quadrado tabelado para 1 grau de liberdade e o n�vel de signific�ncia escolhido (retirado do seu material)
  
  if (correct == TRUE) {
    print('Teste de Mantel-Haenszel com corre��o de continuidade')
  } else {
    print('Teste de Mantel-Haenszel sem corre��o de continuidade')
  }
  
  p_value <- 1 - pchisq(mantel_haenszer_statistic, 1)
  
  print(paste0("Estat�stica do teste: ", round(mantel_haenszer_statistic,4), 
               ', gl = 1, valor-p: ', round(p_value,4)))
  
  print('Hip�tese alternativa: true common odds ratio n�o � 1')
  
  print(paste0('Intervalo de confian�a para ', confidence_level*100, '%'))
  print(paste0(round(ic_inferior, digits = 4), '    ', round(ic_superior, digits = 4)))
  
  print(paste0('Valor cr�tico: � direita de ', round(qchisq(confidence_level, 1), 2)))
  
  print(paste0('Odds Ratio: ', round(odds_ratio_estimator, digits = 2)))
  
  
  if (p_value  < 0.05) {
    print('Como o p valor � menor do que 0.05, rejeitamos a hip�tese nula')
  } else {
    print('Como o p valor � maior do que 0.05, falhamos em rejeitar a hip�tese nula')
  }
  
}


