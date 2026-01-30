// Copyright 2026 International Digital Economy Academy
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

///|
test "basic_arithmetic_operations" {
  // Test addition
  inspect(1.0 + 2.0, content="3.0")
  inspect(0.5 + 0.5, content="1.0")
  inspect(-1.0 + 1.0, content="0.0")
  
  // Test subtraction
  inspect(5.0 - 3.0, content="2.0")
  inspect(1.0 - 1.0, content="0.0")
  inspect(-1.0 - 1.0, content="-2.0")
  
  // Test multiplication
  inspect(3.0 * 4.0, content="12.0")
  inspect(2.5 * 2.0, content="5.0")
  inspect(-2.0 * 3.0, content="-6.0")
  
  // Test division
  inspect(10.0 / 2.0, content="5.0")
  inspect(1.0 / 2.0, content="0.5")
  inspect(-6.0 / 2.0, content="-3.0")
}

///|
test "float_comparison_operations" {
  // Test equality
  assert_eq(1.0 == 1.0, true)
  assert_eq(1.0 == 2.0, false)
  assert_eq(0.0 == 0.0, true)
  
  // Test inequality
  assert_eq(1.0 != 2.0, true)
  assert_eq(1.0 != 1.0, false)
  
  // Test less than
  assert_eq(1.0 < 2.0, true)
  assert_eq(2.0 < 1.0, false)
  
  // Test greater than
  assert_eq(2.0 > 1.0, true)
  assert_eq(1.0 > 2.0, false)
  
  // Test less than or equal
  assert_eq(1.0 <= 2.0, true)
  assert_eq(1.0 <= 1.0, true)
  assert_eq(2.0 <= 1.0, false)
  
  // Test greater than or equal
  assert_eq(2.0 >= 1.0, true)
  assert_eq(1.0 >= 1.0, true)
  assert_eq(1.0 >= 2.0, false)
}

///|
test "float_special_values" {
  // Test infinity
  assert_eq(@float.infinity.is_inf(), true)
  assert_eq(@float.infinity.is_finite(), false)
  assert_eq(@float.infinity.is_nan(), false)
  
  // Test negative infinity
  assert_eq(@float.neg_infinity.is_inf(), true)
  assert_eq(@float.neg_infinity.is_finite(), false)
  assert_eq(@float.neg_infinity.is_nan(), false)
  
  // Test NaN
  assert_eq(@float.not_a_number.is_nan(), true)
  assert_eq(@float.not_a_number.is_finite(), false)
  assert_eq(@float.not_a_number.is_inf(), false)
  
  // Test normal values
  assert_eq(1.0.is_nan(), false)
  assert_eq(1.0.is_inf(), false)
  assert_eq(1.0.is_finite(), true)
}

///|
test "float_basic_functions" {
  // Test absolute value
  inspect(Float::abs(5.0), content="5.0")
  inspect(Float::abs(-5.0), content="5.0")
  inspect(Float::abs(0.0), content="0.0")
  
  // Test signum
  inspect(Float::signum(5.0), content="1.0")
  inspect(Float::signum(-5.0), content="-1.0")
  inspect(Float::signum(0.0), content="0.0")
  
  // Test round
  inspect(Float::round(3.2), content="3.0")
  inspect(Float::round(3.8), content="4.0")
  inspect(Float::round(-3.2), content="-3.0")
  inspect(Float::round(-3.8), content="-4.0")
  
  // Test floor
  inspect(Float::floor(3.2), content="3.0")
  inspect(Float::floor(3.8), content="3.0")
  inspect(Float::floor(-3.2), content="-4.0")
  inspect(Float::floor(-3.8), content="-4.0")
  
  // Test ceil
  inspect(Float::ceil(3.2), content="4.0")
  inspect(Float::ceil(3.8), content="4.0")
  inspect(Float::ceil(-3.2), content="-3.0")
  inspect(Float::ceil(-3.8), content="-3.0")
}

///|
test "float_power_operations" {
  // Test basic powers
  inspect(Float::pow(2.0, 0.0), content="1.0")
  inspect(Float::pow(2.0, 1.0), content="2.0")
  inspect(Float::pow(2.0, 2.0), content="4.0")
  inspect(Float::pow(2.0, 3.0), content="8.0")
  
  // Test fractional powers
  inspect(Float::pow(4.0, 0.5), content="2.0")
  inspect(Float::pow(9.0, 0.5), content="3.0")
  
  // Test negative powers
  inspect(Float::pow(2.0, -1.0), content="0.5")
  inspect(Float::pow(2.0, -2.0), content="0.25")
}

///|
test "float_square_root_operations" {
  // Test basic square roots
  inspect(Float::sqrt(0.0), content="0.0")
  inspect(Float::sqrt(1.0), content="1.0")
  inspect(Float::sqrt(4.0), content="2.0")
  inspect(Float::sqrt(9.0), content="3.0")
  inspect(Float::sqrt(16.0), content="4.0")
  
  // Test decimal square roots
  inspect(Float::sqrt(2.0), content="1.4142135623730951")
  inspect(Float::sqrt(3.0), content="1.7320508075688772")
}

///|
test "float_trigonometric_basic_values" {
  // Test basic trigonometric values
  let pi = 3.141592653589793
  
  // Test sin values
  inspect(Float::sin(0.0), content="0.0")
  inspect(Float::sin(pi / 2.0), content="1.0")
  inspect(Float::sin(pi), content="0.0")
  
  // Test cos values
  inspect(Float::cos(0.0), content="1.0")
  inspect(Float::cos(pi / 2.0), content="0.0")
  inspect(Float::cos(pi), content="-1.0")
}

///|
test "float_logarithmic_operations" {
  // Test natural logarithm
  inspect(Float::ln(1.0), content="0.0")
  inspect(Float::ln(@float.e), content="1.0")
  
  // Test base-10 logarithm
  inspect(Float::log10(1.0), content="0.0")
  inspect(Float::log10(10.0), content="1.0")
  inspect(Float::log10(100.0), content="2.0")
  
  // Test base-2 logarithm
  inspect(Float::log2(1.0), content="0.0")
  inspect(Float::log2(2.0), content="1.0")
  inspect(Float::log2(4.0), content="2.0")
}

///|
test "float_exponential_operations" {
  // Test exponential function
  inspect(Float::exp(0.0), content="1.0")
  inspect(Float::exp(1.0), content="2.718281828459045")
  
  // Test exp2 (2^x)
  inspect(Float::exp2(0.0), content="1.0")
  inspect(Float::exp2(1.0), content="2.0")
  inspect(Float::exp2(2.0), content="4.0")
  inspect(Float::exp2(3.0), content="8.0")
}