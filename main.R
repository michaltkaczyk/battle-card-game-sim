cards <- list(
    cards_1 = sample(rep(1:13, 2), 26, FALSE),
    cards_2 = sample(rep(1:13, 2), 26, FALSE),
    stash_1 = c(),
    stash_2 = c(),
    winner = 0)

play_cards <- function(cards) {
    if (cards$winner == 0) {
        if (cards$cards_1[1] > cards$cards_2[1]) {
            if (length(cards$cards_2) == 1) {
                cards$winner <- 1
                cat("Player 1 wins the game!\n")
            } else {
                cat("Player 1 wins:", cards$cards_1[1], "beats", cards$cards_2[1], "\n")
                cards$cards_1 <- c(cards$cards_1[-1], cards$stash_1, cards$stash_2, cards$cards_1[1], cards$cards_2[1])
                cards$cards_2 <- cards$cards_2[-1]
                cards$stash_1 <- c()
                cards$stash_2 <- c()
            }
        } else {
            if (cards$cards_1[1] < cards$cards_2[1]) {
                if (length(cards$cards_1) == 1) {
                    cat("Player 2 wins the game!\n")
                    cards$winner <- 2
                } else {
                    cat("Player 2 wins:", cards$cards_2[1], "beats", cards$cards_1[1], "\n")
                    cards$cards_2 <- c(cards$cards_2[-1], cards$stash_2, cards$stash_1, cards$cards_2[1], cards$cards_1[1])
                    cards$cards_1 <- cards$cards_1[-1]
                    cards$stash_1 <- c()
                    cards$stash_2 <- c()
                }
            } else {
                cat("Battle!\n")
                if (length(cards$cards_2) == 1) {
                    cat("Player 1 wins the game!\n")
                    cards$winner <- 1
                } else {
                    if (length(cards$cards_1) == 1) {
                        cat("Player 2 wins the game!\n")
                        cards$winner <- 2
                    } else {
                        cards$stash_1 <- c(cards$stash_1, cards$cards_1[1:2])
                        cards$stash_2 <- c(cards$stash_2, cards$cards_2[1:2])
                        cards$cards_1 <- cards$cards_1[-(1:2)]
                        cards$cards_2 <- cards$cards_2[-(1:2)]
                        play_cards(cards)
                    }
                }
            }
        }
    }
    return(cards)
}

for (rounds in 1:1000) {
    cards <- play_cards(cards)
}
