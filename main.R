library(ggplot2)

play_game <- function(game) {
    if (game$winner == 0) {
        if (game$cards_1[1] > game$cards_2[1]) {
            if (length(game$cards_2) == 1) {
                game$winner <- 1
            } else {
                game$counter <- game$counter + 1
                game$cards_1 <- c(game$cards_1[-1], game$stash_1, game$stash_2, game$cards_1[1], game$cards_2[1])
                game$cards_2 <- game$cards_2[-1]
                game$stash_1 <- c()
                game$stash_2 <- c()
            }
        } else {
            if (game$cards_1[1] < game$cards_2[1]) {
                if (length(game$cards_1) == 1) {
                    game$winner <- 2
                } else {
                    game$counter <- game$counter + 1
                    game$cards_2 <- c(game$cards_2[-1], game$stash_2, game$stash_1, game$cards_2[1], game$cards_1[1])
                    game$cards_1 <- game$cards_1[-1]
                    game$stash_1 <- c()
                    game$stash_2 <- c()
                }
            } else {
                if (length(game$cards_2) == 1) {
                    game$winner <- 1
                } else {
                    if (length(game$cards_1) == 1) {
                        game$winner <- 2
                    } else {
                        game$stash_1 <- c(game$stash_1, game$cards_1[1:2])
                        game$stash_2 <- c(game$stash_2, game$cards_2[1:2])
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
    sim_winner <- rep(0, sim_iterations)
    
    for (i in 1:sim_iterations) {
        
        deck <- sample(rep(1:13, 4))
        
        game <- list(
            cards_1 = deck[1:26],
            cards_2 = deck[27:52],
            stash_1 = c(),
            stash_2 = c(),
            winner = 0,
            counter = 0)
        
        for (rounds in 1:10000) {
            game <- play_game(game)
        }
        
        sim_rounds_elapsed[i] <- game$counter
        sim_winner[i] <- game$winner
    }
    
    data.frame(
        sim_rounds_elapsed = sim_rounds_elapsed,
        sim_winner = sim_winner)
}

simulation_results <- run_simulation(10000)

ggplot(simulation_results, aes(x = sim_rounds_elapsed)) + geom_histogram(binwidth = 25)
