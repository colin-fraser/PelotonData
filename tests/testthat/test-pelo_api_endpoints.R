


test_that("api endpoints work", {
  expect_equal(pelo_api_endpoints('pg', workout_id = 12345), 'https://api.onepeloton.com/api/workout/12345/performance_graph?every_n=1')
})
