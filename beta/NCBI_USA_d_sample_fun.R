#DOWN SAMPLING USA GENOME FUNCTION
#Description: this function downsamples USA samples by the Country Variable 
#from NCBI Virus database downloads: https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/

d_sample_fun = function(data, n = NULL) {
  if(is.null(n)) {
    n = .15
  }
  sample_intl =  data %>% 
    filter(Country != "USA")
  
  set.seed(1)
  sample_usa = data %>%
    filter(Country == "USA") %>%
    sample_frac(n)
  
  bind_rows(sample_usa, sample_intl) 
  
}