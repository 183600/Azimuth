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
test "int basic operations" {
  let x = 42
  let y = -17
  let zero = 0
  
  assert_eq(x + y, 25)
  assert_eq(x - y, 59)
  assert_eq(x * y, -714)
  assert_eq(x / y, -2)
  assert_eq(x % y, 8)
  
  assert_eq(Int::abs(y), 17)
  assert_eq(Int::abs(zero), 0)
  assert_eq(Int::abs(x), 42)
}

///|
test "int comparison operations" {
  let a = 10
  let b = 20
  let c = 10
  
  assert_eq(a < b, true)
  assert_eq(a > b, false)
  assert_eq(a <= b, true)
  assert_eq(a >= b, false)
  assert_eq(a == b, false)
  assert_eq(a != b, true)
  assert_eq(a == c, true)
  assert_eq(a <= c, true)
  assert_eq(a >= c, true)
}

///|
test "int boundary values" {
  assert_eq(@int.max_value, 2147483647)
  assert_eq(@int.min_value, -2147483648)
  
  let max_val = @int.max_value
  let min_val = @int.min_value
  
  assert_eq(max_val + 1, -2147483648)
  assert_eq(min_val - 1, 2147483647)
}

///|
test "int bit operations" {
  let a = 12  // 1100 in binary
  let b = 10  // 1010 in binary
  
  assert_eq(a & b, 8)   // 1000
  assert_eq(a | b, 14)  // 1110
  assert_eq(a ^ b, 6)   // 0110
  assert_eq(a << 2, 48) // 110000
  assert_eq(a >> 1, 6)  // 110
}

///|
test "int to string conversion" {
  let positive = 123
  let negative = -456
  let zero = 0
  
  assert_eq(positive.to_string(), "123")
  assert_eq(negative.to_string(), "-456")
  assert_eq(zero.to_string(), "0")
}