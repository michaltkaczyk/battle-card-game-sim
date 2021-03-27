cards <- list(
    cards_1 = sample(rep(1:13, 2), 26, FALSE),
    cards_2 = sample(rep(1:13, 2), 26, FALSE),
    stash_1 = c(),
    stash_2 = c())

play_cards <- function(cards) {
    if (cards$cards_1[1] > cards$cards_2[1]) {
        print("Player 1 wins")
        cards$cards_1 <- c(cards$cards_1[-1], cards$stash_1, cards$stash_2, cards$cards_1[1], cards$cards_2[1])
        cards$cards_2 <- cards$cards_2[-1]
        cards$stash_1 <- c()
        cards$stash_2 <- c()
    } else {
        if (cards$cards_1[1] < cards$cards_2[1]) {
            print("Player 2 wins")
            cards$cards_2 <- c(cards$cards_2[-1], cards$stash_2, cards$stash_1, cards$cards_2[1], cards$cards_1[1])
            cards$cards_1 <- cards$cards_1[-1]
            cards$stash_1 <- c()
            cards$stash_2 <- c()
        } else {
            print("Battle")
            cards$stash_1 <- c(cards$stash_1, cards$cards_1[1:2])
            cards$stash_2 <- c(cards$stash_2, cards$cards_2[1:2])
            cards$cards_1 <- cards$cards_1[-(1:2)]
            cards$cards_2 <- cards$cards_2[-(1:2)]
            play_cards(cards)
        }
    }
    return(cards)
}

for (rounds in 1:1000) {
    cards <- play_cards(cards)
}
