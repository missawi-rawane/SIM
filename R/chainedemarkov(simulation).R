# 'chaine de markov a temps discret
# '@export
# '@param z vecteur numerique representant les ?tapes
# '@param mu vecteur numerique la distribution initiale
# '@param p matrice de transition
# '@param n nombre numerique representant le nombre de pas

Simr  <- function ( z , mu , P , n )
{
  x  <- c(rep( 0 , n  +  1 ))
  t  <- c(seq(0:n))
  x [ 1 ] <- rdist( z , mu )

  for ( i   in   1  :  n ){
    x [ i  +  1 ] <- rdist( z , P [ x [ i ],])
  }

  plot(t,x)
  return ( x )
}




