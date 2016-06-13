#' Creates the full saturated negative log-likelihood function
#' \code{create_nll} Creates the full saturated negative log-likelihood function
#' @keywords internal
#' @export
create_nllsaturated <- function(d, x, k, n, psyfunguesslapses){
  function(p) {
    phi <- d[[k]] / d[[n]]
    phi[phi < .Machine$double.eps] <- .Machine$double.eps
    phi[phi > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
    f <- data.frame(k = d[[k]], n = d[[n]]) %>% mutate(coef = lchoose(n,k))
    return(-sum(f$coef + d[[k]] * log(phi) + (d[[n]] - d[[k]]) * log(1 - phi)))
  }
}


