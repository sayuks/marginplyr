# Investigation evidence, 2026-10-26. Run from the repository root.
pkgload::load_all('.', quiet=TRUE)
set.seed(20261026)
vars <- c('a','b','c','d')
data <- data.frame(a=1,b='b',c=TRUE,d=as.Date('2020-01-01'),fixed='f')
construct <- list(set=grouping_set, sets=grouping_sets, rollup=rollup, cube=cube, product=grouping_spec)
canon <- function(z) {
  dims <- as.character(unique(unlist(z,use.names=FALSE)))
  lapply(z,function(s) dims[dims %in% s])
}
make <- function(depth=0L) {
  kind <- if(depth >= 3L) 'set' else sample(names(construct),1)
  k <- sample(if(kind=='sets') 1:3 else 0:3,1)
  if(kind %in% c('rollup','cube') && k==0) k <- 1L
  args <- list(); exp <- list(); units <- list()
  for(i in seq_len(k)) {
    if(kind %in% c('sets','product') && runif(1)<0.6) {
      child <- make(depth+1L)
      args[[i]] <- child$spec; exp[[i]] <- child$sets
    } else {
      selected <- sample(vars, sample(if(kind %in% c('rollup','cube')) 1:3 else 0:3,1))
      if(kind %in% c('rollup','cube') && runif(1)<0.5) {
        child <- rlang::inject(grouping_set(!!!lapply(selected,rlang::sym)))
        args[[i]] <- child; units <- c(units,list(selected))
      } else {
        args[[i]] <- rlang::expr(dplyr::all_of(!!selected))
        if(kind %in% c('rollup','cube')) units <- c(units,as.list(selected))
      }
      exp[[i]] <- list(selected)
    }
  }
  fn <- construct[[kind]]
  spec <- rlang::inject(fn(!!!args))
  if(kind=='set') sets <- list(unique(unlist(lapply(exp,`[[`,1),use.names=FALSE)))
  if(kind=='sets') sets <- unlist(exp,recursive=FALSE)
  if(kind=='product') {
    grids <- if(length(exp)) do.call(expand.grid,c(lapply(exp,seq_along),KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)) else data.frame(identity=1L)
    if(length(exp)) {
      # expand.grid varies its first axis fastest, while specification products vary the last.
      grids <- grids[do.call(order,as.list(grids)),,drop=FALSE]
      sets <- lapply(seq_len(nrow(grids)),function(r)unique(unlist(lapply(seq_along(exp),function(j)exp[[j]][[grids[r,j]]]),use.names=FALSE)))
    } else sets <- list(character())
  }
  if(kind=='rollup') sets <- lapply(seq(length(units),0L),function(n)if(n)unique(unlist(units[seq_len(n)],use.names=FALSE))else character())
  if(kind=='cube') {
    grids <- expand.grid(rep(list(c(TRUE,FALSE)),length(units)), KEEP.OUT.ATTRS=FALSE)
    grids <- grids[order(-rowSums(grids)),,drop=FALSE]
    # combinations in cube use lexicographic order of included positions.
    pos <- lapply(seq_len(nrow(grids)),function(i)which(unlist(grids[i,])))
    keys <- vapply(pos,function(z)paste(sprintf('%02d',z),collapse=':'),character(1))
    pos <- pos[order(-lengths(pos),keys)]
    sets <- lapply(pos,function(z)if(length(z))unique(unlist(units[z],use.names=FALSE))else character())
  }
  list(spec=spec,sets=sets,kind=kind)
}
checked <- 0L
for(i in 1:500) {
  z <- make(); expected <- canon(z$sets)
  for(policy in c('keep','drop','error')) {
    duplicates <- duplicated(vapply(expected,function(s)paste(s,collapse='|'),character(1)))
    if(policy=='error'&&any(duplicates)) {
      out <- tryCatch(inspect_grouping(data,.by=fixed,.grouping=z$spec,.duplicates=policy,.format='list'),error=identity)
      stopifnot(inherits(out,'marginplyr_error'),grepl('Duplicate grouping sets',conditionMessage(out),fixed=TRUE))
    } else {
      want <- if(policy=='drop') expected[!duplicates] else expected
      out <- inspect_grouping(data,.by=fixed,.grouping=z$spec,.duplicates=policy,.format='list')
      if(!identical(out$included,want)) {
        saveRDS(z,'/tmp/marginplyr-grouping-algebra-failure.rds');print(list(i=i,kind=z$kind,actual=out$included,want=want));stop('algebra mismatch')
      }
      stopifnot(identical(out$set_id,seq_along(want)))
      stopifnot(identical(out$fixed,rep(list('fixed'),length(want))))
      dims <- unique(unlist(expected,use.names=FALSE))
      bits <- lapply(want,function(s)if(length(dims))stats::setNames(as.integer(!dims%in%s),dims)else integer())
      stopifnot(identical(out$grouping_bits,bits))
      ids <- vapply(bits,function(b)if(length(b))as.integer(sum(b*2^rev(seq.int(0,length(b)-1))))else 0L,integer(1))
      stopifnot(identical(out$grouping_id,ids))
    }
    checked <- checked+1L
  }
}
cat('PASS: ',checked,' public inspections across 500 recursive specifications; keep/drop/error, membership/order/IDs/fixed keys.\n',sep='')
