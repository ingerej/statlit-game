questions <- list(
  
  list(
    id = "q1",
    type = "multiple_choice",
    
    title = "Tolk sannsynlighet i kontekst",
    description = "En lærer undersøkte søvn hos elever.",
    
    has_plot = TRUE,
    
    plot_fun = function() {
      hours <- seq(3, 10, by = 0.5)
      counts <- c(1,0,0,0,2,4,8,11,17,11,8,6,4,3,1)
      
      x <- rep(hours, counts)
      y <- unlist(lapply(counts, function(n) seq_len(n)))
      
      plot(x, y, pch = 19)
    },
    
    choices = c(
      "Alternativ A",
      "Alternativ B",
      "Alternativ C"
    ),
    
    correct = 3,
    
    explanation = "Alternativ C er riktig fordi..."
  ),
  
  
  list(
    id = "q2",
    type = "text_only",
    
    title = "Enklere spørsmål",
    description = "Dette er et spørsmål uten plot.",
    
    has_plot = FALSE,
    
    choices = c(
      "Svar 1",
      "Svar 2",
      "Svar 3"
    ),
    
    correct = 2,
    
    explanation = "Her er svar 2 riktig."
  )
)