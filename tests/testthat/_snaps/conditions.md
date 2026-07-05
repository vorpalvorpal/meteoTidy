# abort_meteo() / renders a multi-bullet cli message stably

    Code
      abort_meteo(c("Bad thing happened.", x = "the value was {.val 42}", i = "try {.code met_help()}"),
      class = "demo")
    Condition
      Error:
      ! Bad thing happened.
      x the value was "42"
      i try `met_help()`

