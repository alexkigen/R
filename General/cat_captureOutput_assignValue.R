#cat function in r

prod = c('FDA', 'VDA')
caln = c(2014:2018)


#these two are different
cat(prod[1], caln[1], sep = "_")
paste(prod[1], caln[1], sep = "_")

##saves the name
capture.output(cat(prod[1], caln[1], sep = "_"))
##assigns a concatenated name to an object
assign(capture.output(cat(prod[1], caln[1], sep = "_")), data.frame(integers = c(1:10, 11:34))) 
