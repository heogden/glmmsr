library(glmmsr)

test_that("modifies subexpr for model frame indexing correctly", {
  subexpr1 <- quote(ability[player1] - ability[player2])
  subexpr2 <- quote(ability[player1] + stuff[id])
  expect_equal(modify_subexpr(subexpr1, "ability"),
               quote(`-fr`(`[fr`(ability, player1), `[fr`(ability, player2))))
  expect_equal(modify_subexpr(subexpr2, "ability"),
               quote(`+fr`(`[fr`(ability, player1), stuff[id])))
})
