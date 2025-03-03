#structural breaks
varname200101_200511 <- "ESA.Q.I2.S.0000.B1QG00.1000.TTTT.Q.U.A"
varname200512_200705 <- "ESA.Q.I2.S.0000.B1QG00.1000.TTTT.L.U.A"
varname200706_200805 <- "ESA.Q.I3.S.0000.B1QG00.1000.TTTT.L.U.A"
varname200806_200901 <- "ESA.Q.I4.S.0000.B1QG00.1000.TTTT.L.U.A" #euro area 16
varname200902_201101 <- "ESA.Q.I5.S.0000.B1QG00.1000.TTTT.L.U.A" # euro area 17
varname201102_201401 <- "ESA.Q.I6.Y.0000.B1QG00.1000.TTTT.L.U.A" # euro area 17
varname201402_201410 <- "ESA.Q.I7.Y.0000.B1QG00.1000.TTTT.L.U.A" # euro area 18
varname201410_201412 <- "MNA.Q.Y.I7.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.LR.N"


varname_dat <-
  data.table(
    year = c(rep(2001:2004, each = 12), rep(2005, 11)),
    month = c(rep(1:12, 4), 1:11),
    varname = varname200101_200511
) |>
  rbind(
    data.table(
      year = c(2005, rep(2006, 12), rep(2007, 5)),
      month = c(12, 1:12, 1:5),
      varname = varname200512_200705
    )
  ) |>
  rbind(
    data.table(
      year = c(rep(2007, 7), rep(2008, 5)),
      month = c(6:12, 1:5),
      varname = varname200706_200805
    )
  ) |>
  rbind(
    data.table(
      year = c(rep(2008, 7), rep(2009, 1)),
      month = c(6:12, 1),
      varname = varname200806_200901
    )
  ) |>
  rbind(
    data.table(
      year = c(rep(2009, 11), rep(2010, 12), 2011),
      month = c(2:12,1:12, 1),
      varname = varname200902_201101
    )
  ) |>
  rbind(
    data.table(
      year = c(rep(2011, 11), rep(2012, 12), rep(2013, 12), 2014),
      month = c(2:12,1:12, 1:12, 1),
      varname = varname201102_201401
    )
  ) |>
  rbind(
    data.table(
      year = c(rep(2014, 9)),
      month = c(2:10),
      varname = varname201402_201410
    )
  ) |>
  rbind(
    data.table(
      year = c(rep(2014, 2)),
      month = c(11:12),
      varname = varname201410_201412
    )
  )

data.table::fwrite(varname_dat, here("data", "rtd_varnames.csv"))
