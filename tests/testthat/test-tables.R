test_that("a basic call to 'forecast' gets all the things we need", {
  tst <- vc.forecast('PC3XUM7KALTRECYQKTNFM9CCR', 'Fort Wayne,IN',
                     forecastDays = '2', shortColumnNames = 'true')

  #Number of rows
  expect_equal(nrow(tst), 2)

  #Names of temperature columns and date columns
  expect_true(all(c('datetime', 'mint', 'maxt', 'temp') %in% names(tst)))
})

test_that("a basic call to 'history' gets all the things we need", {
  tst <- vc.history('PC3XUM7KALTRECYQKTNFM9CCR', 'Fort Wayne,IN',
                     startDateTime = '1983-11-06', endDateTime = '1983-11-08',
                    shortColumnNames = 'true')

  #Number of rows
  expect_equal(nrow(tst), 3)

  #Names of temperature columns and date columns
  expect_true(all(c('datetime', 'mint', 'maxt', 'temp') %in% names(tst)))
})

#ERROR RESPONSES FROM THE VISUAL CROSSING API ARE CURRENTLY VERY UNSTABLE
#WILL UPDATE IF THEY EVER BECOME CONSISTENT

# test_that("a bad call to 'forecast' reacts the way we intend", {
#   #Get correct warning
#   expect_warning(tst <- vc.forecast('nah', locations = 'Fort Wayne,IN'),
#                  regexp = 'No\\.account\\.found\\.with\\.provided\\.API\\.key')
#
#   #Get nothing returned
#   expect_equal(nrow(tst), 0)
#
#   #Get correct warning
#   expect_warning(tst <- vc.forecast('nah', locations = 'Fort Wayne,IN', contentType = 'json'),
#                  regexp = "No account found with provided API key 'nah'")
# })
#
# test_that("a bad call to 'history' reacts the way we intend", {
#   #Get correct warning
#   expect_warning(tst <- vc.history('nah', locations = 'Fort Wayne,IN',
#                                    startDateTime = '1983-11-06', endDateTime = '1983-11-08'),
#                  regexp = 'No\\.account\\.found\\.with\\.provided\\.API\\.key')
#
#   #Get nothing returned
#   expect_equal(nrow(tst), 0)
#
#   #Get correct warning
#   expect_warning(tst <- vc.history('nah', locations = 'Fort Wayne,IN',
#                                     startDateTime = '1983-11-06', endDateTime = '1983-11-08',
#                                     contentType = 'json'),
#                  regexp = "No account found with provided API key 'nah'")
# })
#
# test_that("an unauthorized call to 'historysummary' works as intended", {
#   #Get correct warning
#   expect_warning(tst <- vc.historySummary('PC3XUM7KALTRECYQKTNFM9CCR',
#                                           locations = 'Fort Wayne,IN',
#                                           minYear = '1983', maxYear = '1988',
#                                           contentType = 'csv'),
#                  regexp = 'Your API key does not have access')
#
#   #Current formatting of VC API does not allow us to provide a similar warning for
#   #json values. Which is annoying.
# })
