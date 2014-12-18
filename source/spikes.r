# generate.spikes<-function(mean_interval,s,projyr)
# {
#   # generate a vector of time intervals between spikes
#   x <- runif(100,0,1)   # 1)
#   y <-round( mean_interval * ( x*s + 1 - s/2),0)       #2,3,4)
#   
#   #y[1]<-round(runif(1,1,11),0)      # subtract the time since the last spike to the first time interval
#   y[1]<-y[1]-8
#   
#   # convert the vector of time intervals in a time series of 0s (for normal years) or 1 (for spikes)
#   # first deduce 8 years to the first time interval
#   spikes<-c()
#   for (i in 1:length(y)) spikes<-c(spikes,rep(0,(y[i]-1)),1)
#   spikes<-spikes[1:projyr]
#   
#   
#   # define the amplitude of the spikes
#   # possible values
#   spikeR<-c(72201900,23945400)
#   # probabilities of each value to be drawn
#   p.spikeR<-c(0.5,0.5)
#   # sample between the two observed spikes :  sample(c(6,19),1)
#   spikes[spikes==1]<-sample(x=spikeR,size=sum(spikes),replace=T,prob=p.spikeR)
#   
#   return (spikes)
# }

fgenerate_spikes<-function(mean_interval, spike_var, projyr, offset = 0, spike_years = c(1982,2001),
                           spike_probs = c(0.5,0.5))
{
  # generate a vector of time intervals between spikes
  # mean_interval is the mean time in years between spike events
  # spike_var is a variability parameter
  # projyr - how many years of data are required
  # offset - an initial offset for the first event
  # spike_years - vector of years of spikes
  # spike_probs - vector of spike probabilities
  
  #returns a vector with 0 (no spike) or a year on which the size of the spike is determined
  
  #100 draws from a uniform distribution with min = 0 and max = 1
  x <- runif(100,0,1)
  
  #multiply by variability factor (s) so that distribution varies from 0 to s, with a mean = s/2
  x <- x*spike_var
  
  #multiply by mean_interval
  y <-mean_interval*(x+1-spike_var/2)
  
  #round to the nearest integer, this becomes the time to the next spike
  y <- round(y,0)
  
  #y[1]<-round(runif(1,1,11),0)      
  #subtract the time since the last spike from the first interval
  y[1] <- y[1] - offset
  
  # convert the vector of time intervals in a time series of 0s (for normal years) or 1 (for spikes)
  spikes<-c()
  
  for (i in 1:length(y)) spikes<-c(spikes,rep(0,(y[i]-1)),1)
  
  #trim vector to necessary length
  spikes<-spikes[1:projyr]
  
  # sample between the two observed spikes :  sample(c(6,19),1)
  spikes[spikes==1] <- sample(x = spike_years,
                              size = sum(spikes),
                              replace = T,
                              prob = spike_probs)
  
  return (spikes)
}
