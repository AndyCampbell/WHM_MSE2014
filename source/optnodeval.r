#assign the environment var V:nodeval.r
fset_env_var("V:nodeval.r","2.0")

fnodeval<-function(root,nodename,default) {

   if (xmlSize(root$children[[nodename]])>0) {
      return (xmlValue(xmlElementsByTagName(root,nodename)[[1]]))
   } else {
      return (default)
   }

}
