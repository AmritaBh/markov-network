
filter_vector <- function(input_list)
{
  n <- length(input_list)
  print(n)
  out_list <- list()
  for (i in c(1:n)) {
    v <- input_list[i]
    print(i)
    v <- unlist(strsplit(as.vector(v), ", "))
    c <- match("chromium-reviews@googlegroups.com", v, nomatch = 0)
    if(c != 0) v <- v[-c]
    else v <- v[!c]
    out_list <- append(out_list, list(v))
  }
  return(out_list)
}

l <- filter_vector(recipient_lists[,1])
fil_rec <- character(0)
for(i in c(1:826398))
{
  t <- paste0(unlist(l[[i]]), collapse = ",")
  fil_rec <- append(fil_rec, t)
}

comment_data <- cbind(comment_data, "filtered_recipients" = fil_rec)
#write.csv(comment_data, "filtered_comment1.csv", row.names = FALSE)
write.csv2(comment_data, "filtered_comment2.csv", row.names = FALSE, sep = ";")
