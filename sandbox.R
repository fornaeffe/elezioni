sum(
  duplicated(
    unique(
      camera_2018[, c(
        "CIRCOSCRIZIONE",
        "COLLEGIOPLURINOMINALE",
        "COLLEGIOUNINOMINALE",
        "COMUNE",
        "ELETTORI",
        "VOTANTI"
      )]
    )[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "COLLEGIOUNINOMINALE",
      "COMUNE"
    )]
  )
)

sum(
  duplicated(
    unique(
      amministrative[, c(
        "REGIONE",
        "PROVINCIA",
        "COMUNE",
        "ELETTORI",
        "VOTANTI",
        "ELEZIONE"
      )]
    )[, c(
      "REGIONE",
      "PROVINCIA",
      "COMUNE",
      "ELEZIONE"
    )]
  )
)
