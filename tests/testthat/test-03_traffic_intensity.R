


test_that("Check if calc_traffic_intensity gives plausible return values",
          {
            ### Arrange
            street_lengths <- data.frame(
              id = c(1, 2),
              x = c(1, 2),
              y = c(3, 4),
              strassenklasse_0 = c(0, 1),
              strassenklasse_I = c(1, 2),
              strassenklasse_II = c(1, 1),
              strassenklasse_III = c(0, 1),
              strassenklasse_IV = c(0, 2),
              strassenklasse_V = c(1, 0),
              kfz_0 = rep(1000, 2),
              kfz_I = rep(2000, 2),
              kfz_II = rep(1000, 2),
              kfz_III = rep(2000, 2),
              kfz_IV = rep(1000, 2),
              kfz_V = rep(3000, 2)
            )
            # street_lengths
            
            scaling_factor <-
              get_overall_avg_traffic_intensity_unscaled(
                "kfz",
                street_lengths %>% select(starts_with("strassenklasse"))
                )
            # scaling_factor
            
            expected_kfz <- street_lengths %>%
              mutate(
                expected_unscaled =
                  strassenklasse_0 * kfz_0 +
                  strassenklasse_I * kfz_I +
                  strassenklasse_II * kfz_II +
                  strassenklasse_III * kfz_III +
                  strassenklasse_IV * kfz_IV +
                  strassenklasse_V * kfz_V,
                expected = expected_unscaled / scaling_factor
              ) %>%
              pull(expected)
            
            ### Act
            res_kfz <- calc_traffic_intensity("kfz", street_lengths)
            res_lkw <- calc_traffic_intensity("lkw", street_lengths)
            
            ### Assert
            expect_equal(res_kfz, expected_kfz) # calculation OK?
            expect_true("numeric" %in% class(res_lkw)) # class OK?
            expect_true(length(res_lkw) == nrow(street_lengths)) # length OK?
            
          })


test_that("Check if get_street_length_avgs gives plausible return values",
          {
            ### Arrange
            street_lengths <- data.frame(
              id = c(1, 2),
              x = c(1, 2),
              y = c(3, 4),
              strassenklasse_0 = c(0, 1),
              strassenklasse_I = c(1, 2),
              strassenklasse_II = c(1, 1),
              strassenklasse_III = c(0, 1),
              strassenklasse_IV = c(0, 2),
              strassenklasse_V = c(1, 0),
              kfz_0 = rep(1000, 2),
              kfz_I = rep(2000, 2),
              kfz_II = rep(1000, 2),
              kfz_III = rep(2000, 2),
              kfz_IV = rep(1000, 2),
              kfz_V = rep(3000, 2)
            )
            # street_lengths
            
            expected_res <- data.frame(
              strassenklasse = c("0", "I", "II", "III", "IV", "V"),
              length = c(0.5, 1.5, 1, 0.5, 1, 0.5)
            )
            
            ### Act
            res <- get_street_length_avgs(street_lengths)
            
            ### Assert
            expect_equal(as.data.frame(res), expected_res)
            
          })
