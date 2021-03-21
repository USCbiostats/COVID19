
# Create the list for week_par_all:
week_par_all <- vector("list", nrow(ABC.out.mat))

# Once we have ABC.par.out_week_1 ready, put it into the list week_par_all:
week_par_all[[1]] <- ABC.par.out_week_1

# And once we have ABC.par.out_week_i ready, put it into the list week_par_all at position i:
week_par_all[[i]] <- ABC.par.out_week_i

# Then once we're ready we can combine all dataframes ABC.par.out_week_i from the week_par_all list into 1 long dataframe doing:
week_par_all_long <- do.call(rbind, week_par_all)
