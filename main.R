cards_1 <- sample(rep(1:13, 2), 26, FALSE)
cards_2 <- sample(rep(1:13, 2), 26, FALSE)

stash_1 <- c()
stash_2 <- c()

play_cards <- function(cards_1, cards_2) {
    if (cards_1[1] > cards_2[1]) {
        print("Player 1 wins")
        cards_1 <<- c(cards_1[-1], cards_1[1], cards_2[1])
        cards_2 <<- cards_2[-1]
    } else {
        if (cards_1[1] < cards_2[1]) {
            print("Player 2 wins")
            cards_2 <<- c(cards_2[-1], cards_2[1], cards_1[1])
            cards_1 <<- cards_1[-1]
        } else {
            print("Battle")
            stash_1 <<- c(stash_1, cards_1[1:2])
            stash_2 <<- c(stash_2, cards_2[1:2])
            cards_1 <<- cards_1[-(1:2)]
            cards_2 <<- cards_2[-(1:2)]
            play_cards(cards_1, cards_2)
        }
    }
    stash_1 <- c()
    stash_2 <- c()
}

for (rounds in 1:100) {
    play_cards(cards_1, cards_2)
}
