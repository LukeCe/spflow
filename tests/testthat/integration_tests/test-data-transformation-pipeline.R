# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - integration test for data transformation steps
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# Test if the top level formula is interpreted corrextly and all the data
# required for model estimation is constructed.
# - - - - - - - - - - - - - - - - - - -
# Date: May 2020


library("spflow")
data("multi_net_usa_ge")
data("germany_net")

# ---- setup test examples ----------------------------------------------------
origin_key <- id(germany_net)
destination_key <- id(germany_net)


# ---- formula and control-----------------------------------------------------
context("Define control object and formula")

test_flow_control <- spflow::spflow_control()
test_flow_formula <-
  log(y1) ~                     # 1
  O_(. + log(invented_gdp))   + # 2
  D_(. + sqrt(invented_gdp))  + # 3
  I_(.) +                       # 4
  G_(distance)                  # 5

flow_formula_pair_part <-
  ~ Species + Sepal.Length - 1

flow_formula_origin_part <-
  ~ . + log(Petal.Length) + I(Petal.Length + Sepal.Width) - 1

formulas_by_role <-
  decompose_roles_and_cases(test_flow_formula,
                            test_flow_control)

formulas_by_case <-
  formulas_by_role %>% translist()

pair_data_cases <- c("Pair_", "interactions")
origin_data_cases <- c("Orig_","Dest_","Intra_")

describe("The multi-target formula is interpreted correctly", {

  it("Is possible to switch between by-case and by-role representation", {
    expect_equal(object = formulas_by_case %>% translist(),
                 expected = formulas_by_role)
  })

  it("Is possible to collapse formulas", {
    expect_equal(
      object = {
        formulas_by_case[pair_data_cases] %>%
          flatlist() %>%
          combine_formulas( )},
      expected = flow_formula_pair_part)

    expect_equal(
      object = {
        formulas_by_case[origin_data_cases] %>%
          flatlist() %>%
          combine_formulas()},
      expected = flow_formula_origin_part)
  })

})

# ---- interaction model frame ------------------------------------------------
context("Derive the interaction model matrix")
origin_data_model_matrix <- expand_model_matrix(
  case_formulas = formulas_by_case[origin_data_cases],
  case_data = dat(multi_network, network_id = key1))

pair_data_model_matrix <- expand_model_matrix(
  case_formulas = formulas_by_case[pair_data_cases],
  case_data = dat(multi_network, network_pair_id = pair_key11))

origin_data <- dat(multi_network, network_id = key1)
destination_data <- dat(multi_network, network_pair_id = pair_key11)

describe("Model matrix expansion", {

  it("Derives the model matrix correctly for each source", {
    expect_equal(origin_data_model_matrix,
                 fix_contrast_model_matrix(flow_formula_origin_part,
                                           origin_data))

    expect_equal(pair_data_model_matrix,
                 fix_contrast_model_matrix(flow_formula_origin_part,
                                           origin_data))
  })

})




# ---- derive the interaction model moments -----------------------------------
context("Derive the interaction model moments")


# ---- estimation -------------------------------------------------------------
context("S2SLS estimation")

