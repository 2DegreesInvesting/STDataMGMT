devtools::load_all()

input_path_stress_test_inputs <- fs::path(
  r2dii.utils::dbox_port_00(), "07_AnalysisInputs", "ST_INPUTS_MASTER"
)

abcd_full_path <- r2dii.utils::path_dropbox_2dii("PortCheck", "00_Data", "07_AnalysisInputs", "ST_INPUTS_MASTER", "abcd_stress_test_input.csv")

abcd_full <- read.csv(abcd_full_path)

output_path_stress_test_inputs <- fs::path(
  r2dii.utils::dbox_port_00(), "07_AnalysisInputs", "ST_INPUTS_MASTER"
)

sample_names <- c(
  "Mbuyelo Investment Holdings",
  "Solid Fuel, Inc.",
  "Talbot Group Holdings Pty Ltd.",
  "Nika Energy Co",
  "Omega Energy Developer Pvt Ltd",
  "Sofoensa",
  "Star Metallics And Power Private Limited",
  "Tangent Energy Solutions, Inc."
)

abcd_full_test <- abcd_full %>% dplyr::filter(company_name %in% sample_names)

abcd_full_test %>%
  readr::write_csv(
    file.path(
      output_path_stress_test_inputs, "abcd_stress_test_input_test.csv"
    )
  )
