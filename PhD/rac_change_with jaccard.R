RAC_change_beta <- function(df,
                       time.var,
                       species.var,
                       abundance.var,
                       replicate.var = NULL,
                       reference.time = NULL) {
  
  # validate function call and purge extraneous columns
  args <- as.list(match.call()[-1])
  df <- do.call(check_args, args, envir = parent.frame())
  
  # add zeros for species absent from a time period within a replicate
  by <- c(replicate.var)
  allsp <- split_apply_combine(df, by, FUN = fill_species, species.var, abundance.var)
  
  # rank species in each time and optionally replicate
  by <- c(time.var, replicate.var)
  rankdf <- split_apply_combine(allsp, by, FUN = add_ranks, abundance.var)
  
  # merge subsets on time difference of one time step
  cross.var <- time.var
  cross.var2 <- paste(cross.var, 2, sep = '')
  split_by <- c(replicate.var)
  merge_to <- !(names(rankdf) %in% split_by)
  if (is.null(reference.time)) {
    ranktog <- split_apply_combine(rankdf, split_by, FUN = function(x) {
      y <- x[merge_to]
      cross <- merge(x, y, by = species.var, suffixes = c('', '2'))
      f <- factor(cross[[cross.var]])
      f2 <- factor(cross[[cross.var2]], levels = levels(f))
      idx <- (as.integer(f2) - as.integer(f)) == 1
      cross[idx, ]
    })
  } else {
    ranktog <- split_apply_combine(rankdf, split_by, FUN = function(x) {
      y <- x[x[[time.var]] != reference.time, merge_to]
      x <- x[x[[time.var]] == reference.time, ]
      merge(x, y, by = species.var, suffixes = c('', '2'))
    })
  }
  # remove rows with NA for both abundances (preferably only when introduced
  # by fill_species)
  idx <- is.na(ranktog[[abundance.var]])
  abundance.var2 <- paste(abundance.var, 2, sep = '')
  idx2 <- is.na(ranktog[[abundance.var2]])
  ranktog[idx, abundance.var] <- 0
  ranktog[idx2, abundance.var2] <- 0
  idx <- ranktog[[abundance.var]] != 0 | ranktog[[abundance.var2]] != 0
  ranktog <- ranktog[idx, ]
  
  # apply turnover calculation to all replicates for each time point
  by <- c(replicate.var, time.var)
  output <- split_apply_combine(ranktog, by, FUN = SERGL,
                                species.var, abundance.var, abundance.var2)
  
  if(any(is.na(output$evenness_change)))
    warning(paste0("evenness_change values contain NAs because there are plots",
                   " with only one species"))
  
  output_order <- c(
    time.var, paste(time.var, 2, sep = ''),
    replicate.var,
    'richness_change', 'evenness_change', 'rank_change', 'gains', 'losses', 'beta3')
  
  return(output[intersect(output_order, names(output))])
}

############################################################################
#
# Private functions: these are internal functions not intended for reuse.
# Future package releases may change these without notice. External callers
# should not use them.
#
############################################################################

# A function to calculate RAC changes for a replicate between two consecutive time points 
# @param df a dataframe
# @param time.var the name of the time column
# @param rank.var1 the name of the speices rank column for the first time peroid
# @param rank.var2 the name of the species rank column for the second time period
# @param abundance.var1 the name of the abundance column for the first time peroid
# @param abundance.var2 the name of the abundance column for the second time period
SERGL <- function(df, species.var, abundance.var, abundance.var2) {
  
  out <- c(species.var, 'rank', 'rank2', abundance.var, abundance.var2)
  out <- unique(df[!(names(df) %in% out)])
  
  # ricness and evenness differences
  s_t1 <- S(df[[abundance.var]])
  e_t1 <- Evar(as.numeric(df[[abundance.var]]))
  s_t2 <- S(df[[abundance.var2]])
  e_t2 <- Evar(as.numeric(df[[abundance.var2]]))
  
  delta_s <- (s_t2-s_t1) / nrow(df)
  delta_e <- (e_t2-e_t1) 
  
  # gains and losses
  df$gain <- ifelse(df[[abundance.var]] == 0, 1, 0)
  df$loss <- ifelse(df[[abundance.var2]] == 0, 1, 0)
  
  gain <- sum(df$gain) / nrow(df)
  loss <- sum(df$loss) / nrow(df)
  
  #Species diff beta -2 based on Carvalho (2012; 10.1111/j.1466-8238.2011.00694.x)
  idx <- df[[abundance.var]] != 0
  idx2 <- df[[abundance.var2]] != 0
  a <- sum(idx & idx2)
  b <- sum(idx & !idx2)
  c <- sum(!idx & idx2)
  spdiff <- 2*(min(b, c) / (a+b+c))
  
  delta_r <- mean(abs(df[['rank']] - df[['rank2']])) / nrow(df)
  
  measures <- data.frame(
    richness_change = delta_s, evenness_change = delta_e,
    rank_change = delta_r, gains = gain, losses = loss, beta3=spdiff)
  
  return(cbind(out, measures))
}





















# Convert from a long form abundance dataframe to a time by species dataframe.
#
# @param df A dataframe containing time.var, species.var and abundance.var columns
# @param time.var The name of the time column from df
# @param species.var The name of the species column from df
# @param abundance.var The name of the abundance column from df
# @return A dataframe of species abundances x time
transpose_community <- function(df, time.var,
                                species.var,
                                abundance.var) {
  df <- as.data.frame(df)
  
  # remove unused levels if species is a factor
  df[species.var] <- if(is.factor(df[[species.var]]) == TRUE){factor(df[[species.var]])} else {df[species.var]}
  
  # sort by time and species
  df <- df[order(df[[time.var]], df[[species.var]]),]
  
  # cast as a species x time dataframe; NAs to 0s
  comdat <- tapply(df[[abundance.var]], list(df[[time.var]], as.vector(df[[species.var]])), sum)
  comdat[is.na(comdat)] <- 0
  comdat <- as.data.frame(comdat)
  
  # results
  return(comdat)
}

# check names of data frames
#
# @param given Vector of variable names as supplied by user
# @param data Data frame containing variables
check_names <- function(given, data) {
  for (i in given) {
    assertthat::assert_that(assertthat::has_name(data, i))
  }
}

# Utility function to warn users that either multiple records exist within
# replicates, or that data may be spanning multiple replicates but no
# replicate.var has been specified
# @param df A dataframe containing time.var, species.var and abundance.var columns
# @param time.var The name of the time column from df
# @param species.var The name of the species column from df
check_single_onerep <- function(df, time.var, species.var) {
  counts <- table(df[[time.var]], df[[species.var]])
  if (any(counts > 1))
    stop(paste("Either data span multiple replicates with no replicate.var",
               "specified or multiple records within years for some species",
               sep = ' '))
}

# Utility function to ensure only a single record exists for a given species
# within one replicate if replicate.var given, and for one time point if
# time.var given.
# @param df A dataframe containing a species.var column, and optionally a
#   time.var and replicate.var columns
# @param species.var The name of the species column from df
# @param time.var The name of the time column from df
# @param replicate.var The name of the replicate column from df
#
#' @importFrom utils capture.output
check_single <- function(df,
                         species.var,
                         time.var = NULL,
                         replicate.var = NULL) {
  
  if (is.null(time.var) & is.null(replicate.var)) {
    checksingle <- table(df[species.var]) > 1
    if (any(checksingle)) {
      stop(paste(
        'Multiple records for one or more species found.',
        'Did you mean to specify a "replicate.var" or "time.var"?'))
    }
  } else {
    by <- c(species.var, time.var, replicate.var)
    checksingle <- apply(table(df[by]) > 1, 2:length(by), any)
    if(any(checksingle)) {
      idx <- which(checksingle, arr.ind = TRUE, useNames = FALSE)
      if (is.matrix(idx)) {
        tab <- apply(idx, 1, function(x) mapply(`[[`, dimnames(checksingle), x))
        tab <- capture.output(prmatrix(t(tab), rowlab = rep('', ncol(tab))))
        tab <- paste(tab, collapse = '\n')
      } else {
        tab <- paste(c(by[2], names(idx)), collapse = '\n')
        nullvar <- c('replicate.var', 'time.var')[
          c(is.null(replicate.var), is.null(time.var))]
        tab <- paste(tab,
                     paste0('Did you mean to specify a "', nullvar, '"?'), sep = '\n')
      }
      stop(paste(
        'Multiple records for one or more species found at:',
        tab, sep = '\n'))
    }
  }
}

# Utility to check for numeric abundance and time variables
#
# @param df A dataframe containing time.var, species.var, and replicate.var columns
# @param time.var The name of the time column from df
# @param abundance.var The name of the replicate column from df
check_numeric <- function(df, time.var, abundance.var) {
  if(!is.numeric(df[[time.var]])) { stop("Time variable is not numeric") }
  if(!is.numeric(df[[abundance.var]])) { stop("Abundance variable is not numeric") }
}

# Utility function to stop calculations if only one species occurs in at least one replicate
# @param df A dataframe containing time.var, species.var and abundance.var columns
# @param species.var The name of the species column from df
# @param replicate.var The name of the replicate column from df
check_multispp <- function(df, species.var, replicate.var){
  
  df <- droplevels(df)
  
  spptable <- table(df[[species.var]], df[[replicate.var]])
  if (min(colSums(spptable > 1)) < 2) {
    stop("One or more replicates consists of only a single species;
       please remove these replicates prior to calculations ")
  }
}

# Utility function to stop calculations if the species never change in a replicate
# @param comdat A community dataframe
#' @importFrom stats var
check_sppvar <- function(comdat){
  sppvar <- sum(apply(comdat, 2, var))
  if(sppvar == 0)
    stop("One or more replicates consist of species that never vary;
         please remove these replicates before calculation")}

# Utility function to calculate richness
# @param x Vector of abundance of each species
S <- function(x){
  x <- x[x > 0 & !is.na(x)]
  stopifnot(x == as.numeric(x))
  length(x)
}

# Add zero abundances for missing species, on the assumption that any species
# in the \code{species.var} column should be included for every group defined
# by all the remaining columns save \code{abundance.var}.
#
# @param df A dataframe with species, abundances, and at least one other column
#   to group by
# @param species.var The name of the species column from df
# @param abundance.var The name of the abundance column from df
# @return A dataframe with the same columns as df, but with zeros added for
#   species that are present in df, but not in a particular group.
fill_zeros <- function(df, species.var, abundance.var) {
  
  mergevars <- !(names(df) %in% c(species.var, abundance.var))
  full <- merge(unique(df[mergevars]), unique(df[species.var]))
  df <- merge(df, full, all = TRUE)
  df[is.na(df[abundance.var]), abundance.var] <- 0
  
  return(df)
}

# Add missing abundances for species absent from a replicate, on the assumption
# that any species in the \code{species.var} column should be included for
# every group defined by all the remaining columns except \code{abundance.var}.
#
# @param df A dataframe with species, abundances, and at least one other column
#   to group by
# @param species.var The name of the species column from df
# @param abundance.var The name of the abundance column from df
# @return A dataframe with the same columns as df, but with \code{NA} added for
#   species that are present in df, but not in each group.
fill_species <- function(df, species.var, abundance.var) {
  
  mergevars <- !(names(df) %in% c(species.var, abundance.var))
  complete <- merge(unique(df[mergevars]), unique(df[species.var]))
  df <- merge(df, complete, all = TRUE)
  
  return(df)
}

# @title Add abundance ranks
# @description Rank species by abundance, by specified grouping. Species with
#   zero abundance receive rank S+1, where S is the total number of species in
#   the group.
# @param df A data frame containing a single record per species with its abundance
# @param abundance.var The name of the abundance column
#
# @return The add_ranks function returns a data frame with the following
#   additional column:
#   \itemize{
#     \item{rank: }{A numeric column with the species rank; a rank of 1
#     indicates the species was most abundant in that time period. All species
#     that are not present in that time period have the rank value S+1 where S
#     is the number of species in the sample.
#     }
#   }
add_ranks <- function(df, abundance.var) {
  
  # get species richness, note that zero abunance does not contribute to S
  S <- S(df[[abundance.var]])
  # rank from high to low abundance
  df[['rank']] <- rank(-df[[abundance.var]], ties.method = 'average')
  # adjust ranks for species with zero abundance to S + 1
  df[df[['rank']] > S, 'rank'] <- S + 1
  
  return(df)
}

# @title Add relative abundance ranks and cumulative abundance
# @description Rank species by abundance within the specified grouping. The
#   rank of the lead abundant species is 1 and the most abundant species has
#   rank 1/S, where S is the number of species in the group.
# @param df A data frame containing a single record per species with its abundance
# @param abundance.var The name of the abundance column
# @param species.var The name of the species column
#
# @return The add_rank_abundance function returns a data frame with the following
#   additional column:
#   \itemize{
#     \item{rank: }{}
#     \item{relrank: }{A numeric column with the species rank divided
#   by the maximum rank in the group; a rank of 1 indicates the species was the
#   least abundant.}
#     \item{cumabund: }{}
#   }
#' @importFrom stats stepfun
add_rank_abundance <- function(df, species.var, abundance.var) {
  
  df <- add_ranks(df, abundance.var)
  out <- unique(df[!(names(df) %in% c('rank', species.var, abundance.var))])
  
  df <- df[order(df[['rank']]), ]
  relrank <- df[['rank']] / max(df[['rank']])
  cumabund <- cumsum(df[[abundance.var]])
  
  out[['rankabund']] <- list(stepfun(relrank, c(0, cumabund)))
  
  return(out)
}

# @title A Split-Apply-Combine implementation
# @description Faster split-apply-combine for data frames, when the results of FUN
# are homogeneous with respect to the number, order, data type and (if
# applicable) levels of columns in the returned data frame.
#
# @param df A data frame
# @param by The name of column(s) in the data frame that define groups to split
# @param FUN The function applied to each grouped data frame after splitting
# @param ... Additional parameters to FUN
#
# @source \url{https://stackoverflow.com/a/9730292/687112}
split_apply_combine <- function(df, by, FUN, ...) {
  if (is.null(by)) {
    # just apply
    out <- FUN(df, ...)
  } else {
    # split (names get in the way)
    dfs <- split(df, df[by], drop = TRUE)
    dfs <- unname(dfs)
    # apply
    dfs <- lapply(dfs, FUN = FUN, ...)
    # combine (flatten outer list, then flatten again across vectors from same column)
    hdr <- names(dfs[[1]])
    dfs <- unlist(dfs, recursive = FALSE, use.names = FALSE)
    idx <- seq_along(hdr)
    out <- lapply(idx, function(i) unlist(dfs[i == idx], FALSE, FALSE))
    names(out) <- hdr
  }
  idx <- sapply(out, is.list)
  out[idx] <- lapply(out[idx], I)
  as.data.frame(out)
}

# @title Rank-Abundance Curve Dissimilarity
# @description A function to calculate the curve difference between two communities, given the empirical relative rank by abundance curve as a \code{stepfun}.
# @param sf The curve for the first community.
# @param sf2 The curve for the second community.
curve_dissim <- function(sf, sf2) {
  
  relrank <- get('x', envir = environment(sf))
  relrank2 <- get('x', envir = environment(sf2))
  r <- sort(unique(c(0, relrank, relrank2)))
  h <- abs(sf(r) - sf2(r))
  w <- c(diff(r), 0)
  
  return(sum(w*h))
}

# @title Evar
# @description A utility function to calculate Evar from Smith and Wilson 1996
# @param S the number of species in the sample
# @param x the vector of abundances of each species
Evar <- function(x, S = length(x)) {
  x1 <- x[x!=0]
  lnx <- log(x1)
  theta <- (S - 1) / S * var(lnx)
  return(1 - 2 / pi * atan(theta))
}

# @title Checking for errors in arguments
# @description Check for errors in the application of arguments and input data
#   for all \code{*_change} and \code{*_difference} functions.
# @param df The name of the dataframe
# @param time.var The name of the optional time column
# @param species.var The name of the species column
# @param abundance.var The name of the abundance column
# @param replicate.var The name of the replicate column
# @param treatment.var The name of the optional treatment column
# @param block.var The name of the optional block column
# @param pool The name the optional pooling approach
# @param reference.treatment The name of the optional reference treatment
# @param reference.time The name of the optional reference time
check_args <- function(df,
                       time.var = NULL,
                       species.var,
                       abundance.var,
                       replicate.var = NULL,
                       treatment.var = NULL,
                       pool = FALSE,
                       block.var = NULL,
                       reference.time = NULL,
                       reference.treatment = NULL) {
  
  # drop extraneous columns
  args <- as.list(match.call()[-1])
  args <- args[!sapply(args, is.null)]
  df <- as.data.frame(df[as.character(args[grep('\\.var$', names(args))])])
  
  # validate argument combinations
  if (pool && (is.null(treatment.var) || !is.null(block.var)))
    stop("Not providing a treatment.var or providing a block.var is incompatible with pooling.")
  if (is.null(treatment.var) && (pool || !is.null(block.var)))
    stop("Not providing a treatment.var is incompatible with pooling or providing block.var.")
  if (grepl('_difference$', sys.call(-2)[1]) && is.null(replicate.var)) {
    stop("Providing a replicate.var is required, even if uniquely determined by treatment and block.")
  }
  if (!is.null(block.var)) {
    reps_exp <- length(unique(df[[block.var]])) * length(unique(df[[treatment.var]]))
    reps_obs <- length(unique(df[[replicate.var]]))
    if (reps_exp != reps_obs)
      stop("There is not one replicate per treatment in a block")
  }
  
  # check no NAs in abundance column
  if (any(is.na(df[[abundance.var]])))
    stop("Abundance column contains missing values")
  
  # check no NAs in species column
  if (any(is.na(df[[species.var]])))
    stop("Species names are missing")
  
  # check no species are repeated
  check_single(df, species.var,
               time.var = time.var, replicate.var = replicate.var)
  
  # check reference exists
  if (!is.null(reference.time) && !is.element(reference.time, df[[time.var]]))
    stop('The reference time could not be found in the time.var column')
  if (!is.null(reference.treatment) && !is.element(reference.treatment, df[[treatment.var]]))
    stop('The reference treatment could not be found in the treatment.var column')
  
  return(df)
}

