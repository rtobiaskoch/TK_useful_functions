n = 1:10000

t = paste0("hello world ", n, "!")


n_iter <- 10000 # Number of iterations of the loop

# Initializes the progress bar
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

for(i in 1:n_iter) {
  
  #---------------------
  # Code to be executed
  #---------------------
  
  print(paste0(t[i], " Go foo yourself!"))
  
  #---------------------
  
  # Sets the progress bar to the current state
  setTxtProgressBar(pb, i)
}

close(pb) # Close the connection