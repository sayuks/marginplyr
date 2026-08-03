retail_sales <- data.frame(
  year = rep(c(2025L, 2026L), each = 12L),
  month = rep(rep(c("Jan", "Feb"), each = 6L), times = 2L),
  region = rep(
    c("East", "East", "East", "West", "West", "West"),
    times = 4L
  ),
  store = rep(
    c("Boston", "New York", NA, "San Francisco", "Seattle", "Seattle"),
    times = 4L
  ),
  product = rep(
    c("Laptop", "Monitor", "Headphones", "Laptop", "Monitor", "Headphones"),
    times = 4L
  ),
  channel = c(
    "Store", "Store", "Online", "Store", "Online", "Store",
    "Online", "Store", "Online", "Store", "Store", "Online",
    "Store", "Online", "Online", "Store", "Online", "Store",
    "Online", "Store", "Online", "Online", "Store", "Online"
  ),
  units = c(
    3L, 8L, 15L, 4L, 6L, 11L,
    4L, 7L, 18L, 5L, 9L, 13L,
    5L, 10L, 22L, 6L, 11L, 16L,
    7L, 12L, 25L, 8L, 14L, 19L
  ),
  revenue = c(
    3600, 2400, 1500, 4800, 1800, 1100,
    4800, 2100, 1800, 6000, 2700, 1300,
    6000, 3000, 2200, 7200, 3300, 1600,
    8400, 3600, 2500, 9600, 4200, 1900
  ),
  stringsAsFactors = FALSE
)

dir.create("data", showWarnings = FALSE)
save(retail_sales, file = "data/retail_sales.rda", compress = "xz")
