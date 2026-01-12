
test "unique_test_case_1" {
  azimuth::assert_eq(10, azimuth::add(3, 7))
}

test "unique_test_case_2" {
  azimuth::assert_eq(15, azimuth::multiply(3, 5))
}

test "unique_test_case_3" {
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
}