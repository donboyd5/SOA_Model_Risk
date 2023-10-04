
round_nearest <- function(x, nearest) {round(x / nearest) * nearest}

list_from_df <- function(df, name, value){
  # create a named list from two columns of a data frame, one that has
  # names and the other that has values
  # name and value are strings giving the relevant columns to use
  
  # example:
  #   list_from_df(df, "range_name", "default")
  
  as.list(setNames(df[[value]], df[[name]]))
}

mbsize <- function(object){
  object.size(object) |> format(units="Mb", digits=2)
}


mem <- function (maxnobjs = 5) 
{
  objs <- ls(envir = globalenv())
  nobjs <- min(length(objs), maxnobjs)
  
  getobjs <- function() {
    f <- function(x) pryr::object_size(get(x)) / (1024^2)
    sizeMB <- sapply(objs, f)
    tmp <- enframe(sizeMB, name="object", value="sizeMB") |> 
      dplyr::arrange(dplyr::desc(sizeMB)) |>
      dplyr::mutate(sizeMB=num(sizeMB, digits=2))
    return(tmp)
  }
  
  if (nobjs > 0) {
    cat("Memory for selected objects: ")
    cat("\n")
    print(utils::head(getobjs(), nobjs))
  }
  # don't need to do garbage collection because mem_used wraps around it
  used <- pryr::mem_used() |> as.numeric()
  cat("\n")
  cat(paste0("Total memory usage: ",
             f_comma(used, scale=1e-6, accuracy=.1),
             " MB"))
}
