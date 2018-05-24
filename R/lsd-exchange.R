## lsd conversions ##

# Exchange rate by shillings
# This is set up to go from sterling to vlaams
deb_lsd_exchange <- function(l, s, d,
                             rate = 31,
                             round = 3,
                             vector = FALSE) {
  denarii <- deb_lsd_d(l, s, d) * rate/20
  deb_d_lsd(denarii, round, vector)
}

# Exchange rate of ducats
# From ducats to lsd with rate as denarii per ducat
deb_ducats_lsd <- function(ducats,
                           deniers = 0,
                           rate = 96,
                           round = 3,
                           vector = FALSE) {
  deniers <- deniers/24
  deb_d_lsd((ducats + deniers) * rate, round, vector)
}

# From lsd to ducats with rate as denarii per ducat
deb_lsd_ducats <- function(l, s, d, rate = 96) {
  denari <- deb_lsd_d(l, s, d) * 24/rate

  tibble(ducats = denari %/% 24,
         denari = denari %% 24)
}
