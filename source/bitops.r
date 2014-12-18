#bitops.r
#Andrew Campbell
#Marine Institite, Galway
#andrew.campbell@marine.ie
#Sys.putenv("V:bitops.r"="2.0")
#Sys.setenv("V:bitops.r"="2.0")

#assign the environment var V:bitops.r
set.env.var("V:bitops.r","2.0")

bitwiseAnd <- function(A,B) 
{

	#if a bitwise AND of A and B returns true for any digit, A is returned
    #up to 20 significant digits are possible

	orig.A <- A

	base2 <- c()

	for (i in 0:20) {
		base2 <- c(base2,2^i)
    }

	#convert A and B to bit streams

    Abits<-c()
	Bbits<-c()

	for (i in 1:(length(base2))) {

		if (A >= base2[length(base2)-i+1]) {
			Abits <- c(Abits,1)
            A <- A - base2[length(base2)-i+1]
		} else {
			Abits <- c(Abits,0)
		}

		if (B >= base2[length(base2)-i+1]) {
			Bbits <- c(Bbits,1)
            B <- B - base2[length(base2)-i+1]
		} else {
			Bbits <- c(Bbits,0)
		}
	}

    Cbits <- Abits & Bbits

	if (sum(Cbits)>0) {
		ret<-orig.A
	} else {
		ret<-0
	}

}
