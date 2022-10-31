devtools::load_all()

input_data <- readr::read_csv(file.path("data-raw", "raw_example_data.csv"))
prepared_data <- prepare_example_data(input_data)
readr::write_csv(prepared_data, file.path("data-raw", "example_data.csv"))
