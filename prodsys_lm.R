reg <- train (
          SALARY ~ YEARSLV2 + YEARSLV3 + FEMALE + SPECLTY1 + SPECLTY2,
          PRODSYS,
          method="lm",
          trControl = trainControl(
            method="cv",
            number=10,
            verboseIter=TRUE
            )
          )
