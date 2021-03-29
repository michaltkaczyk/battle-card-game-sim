deck <- sample(rep(1:13, 4))

cards <- list(
    cards_1 = deck[1:26],
    cards_2 = deck[27:52],
    stash_1 = c(),
    stash_2 = c(),
    winner = 0,
    counter = 0)

report_victory <- function(cards) {
    cat("Player", cards$winner, "wins the game after", cards$counter, "rounds!\n") 
}

play_cards <- function(cards) {
    if (cards$winner == 0) {
        if (cards$cards_1[1] > cards$cards_2[1]) {
            if (length(cards$cards_2) == 1) {
                cards$winner <- 1
                report_victory(cards)
            } else {
                cat("Player 1 wins:", cards$cards_1[1], "beats", cards$cards_2[1], "\n")
                cards$counter <- cards$counter + 1
                cards$cards_1 <- c(cards$cards_1[-1], cards$stash_1, cards$stash_2, cards$cards_1[1], cards$cards_2[1])
                cards$cards_2 <- cards$cards_2[-1]
                cards$stash_1 <- c()
                cards$stash_2 <- c()
            }
        } else {
            if (cards$cards_1[1] < cards$cards_2[1]) {
                if (length(cards$cards_1) == 1) {
                    cards$winner <- 2
                    report_victory(cards)
                } else {
                    cat("Player 2 wins:", cards$cards_2[1], "beats", cards$cards_1[1], "\n")
                    cards$counter <- cards$counter + 1
                    cards$cards_2 <- c(cards$cards_2[-1], cards$stash_2, cards$stash_1, cards$cards_2[1], cards$cards_1[1])
                    cards$cards_1 <- cards$cards_1[-1]
                    cards$stash_1 <- c()
                    cards$stash_2 <- c()
                }
            } else {
                cat("Battle!\n")
                if (length(cards$cards_2) == 1) {
                    cards$winner <- 1
                    report_victory(cards)
                } else {
                    if (length(cards$cards_1) == 1) {
                        cards$winner <- 2
                        report_victory(cards)
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
