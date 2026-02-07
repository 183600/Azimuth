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
test "int basic arithmetic" {
  let sum = 1 + 1
  let diff = 5 - 3
  let product = 3 * 4
  let quotient = 10 / 2
  let remainder = 10 % 3
  let _ = sum + diff + product + quotient + remainder
}

///|
test "int comparison" {
  let greater = 5 > 3
  let less = 5 < 3
  let greater_equal = 5 >= 5
  let less_equal = 5 <= 3
  let equal = 5 == 5
  let not_equal = 5 != 3
  let _ = greater && less && greater_equal && less_equal && equal && not_equal
}

///|
test "int bit operations" {
  let and_result = 6 & 3
  let or_result = 6 | 3
  let xor_result = 6 ^ 3
  let left_shift = 3 << 2
  let right_shift = 8 >> 2
  let _ = and_result + or_result + xor_result + left_shift + right_shift
}

///|
test "int boundary values" {
  let max_val = 2147483647
  let min_val = -2147483648
  let _ = max_val + min_val
}

///|
test "int absolute value" {
  let abs_positive = 5
  let abs_negative = -5
  let abs_zero = 0
  let _ = abs_positive + abs_negative + abs_zero
}