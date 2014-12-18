#Sys.putenv("V:nodeval.r"="2.0")
#Sys.setenv("V:nodeval.r"="2.0")

#assign the environment var V:nodeval.r
set.env.var("V:nodeval.r","2.0")

nodeval<-function(root,nodename,default) {

   if (xmlSize(root$children[[nodename]])>0) {
      return (xmlValue(xmlElementsByTagName(root,nodename)[[1]]))
   } else {
      return (default)
   }

}
