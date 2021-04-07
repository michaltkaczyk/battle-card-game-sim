library(ggplot2)
library(progress)

N_SIMULATIONS <- 100000

play_game <- function(game) {
    if (game$winner == 0) {
        if (game$cards_1[1] > game$cards_2[1]) {
            if (length(game$cards_2) == 1) {
                game$winner <- 1
            } else {
                game$counter <- game$counter + 1
                game$cards_1 <- c(game$cards_1[-1], sample(c(game$stash, game$cards_1[1], game$cards_2[1])))
                game$cards_2 <- game$cards_2[-1]
                game$stash <- c()
            }
        } else {
            if (game$cards_1[1] < game$cards_2[1]) {
                if (length(game$cards_1) == 1) {
                    game$winner <- 2
                } else {
                    game$counter <- game$counter + 1
                    game$cards_2 <- c(game$cards_2[-1], sample(c(game$stash, game$cards_2[1], game$cards_1[1])))
                    game$cards_1 <- game$cards_1[-1]
                    game$stash <- c()
                }
            } else {
                if (length(game$cards_2) == 1) {
                    game$winner <- 1
                } else {
                    if (length(game$cards_1) == 1) {
                        game$winner <- 2
                    } else {
                        game$stash <- c(game$stash, game$cards_1[1:2], game$cards_2[1:2])
                        game$cards_1 <- game$cards_1[-(1:2)]
                        game$cards_2 <- game$cards_2[-(1:2)]
                        
                        if (length(game$cards_1) == 0) {
                            game$winner <- 2
                        } else {
                            if (length(game$cards_2) == 0) {
                                game$winner <- 1
                            } else {
                                play_game(game)
                            }
                        }
                    }
                }
            }
        }
    }
    return(game)
}

run_simulation <- function(sim_iterations) {
    
    sim_rounds_elapsed <- rep(0, sim_iterations)
    
    pb <- progress_bar$new(total = sim_iterations)
    
    for (i in 1:sim_iterations) {
        
        deck <- sample(rep(1:13, 4))
        
        game <- list(
            cards_1 = deck[1:26],
            cards_2 = deck[27:52],
            stash = c(),
            winner = 0,
            counter = 0)
        
        while(game$winner == 0) {
            game <- play_game(game)
        }
        
        sim_rounds_elapsed[i] <- game$counter
        pb$tick()
    }
    
    data.frame(sim_rounds_elapsed = sim_rounds_elapsed)
}


simulation_results <- run_simulation(N_SIMULATIONS)

ggplot(simulation_results, aes(x = sim_rounds_elapsed)) + geom_histogram(binwidth = 25)

paste("Mean number of rounds:", round(mean(simulation_results$sim_rounds_elapsed)))
paste("Median number of rounds:", round(median(simulation_results$sim_rounds_elapsed)))
