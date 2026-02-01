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
test "int absolute value function" {
  inspect(Int::abs(5), content="5")
  inspect(Int::abs(-5), content="5")
  inspect(Int::abs(0), content="0")
  inspect(Int::abs(@int.min_value), content="-2147483648")  // Edge case: min_value
}

///|
test "int max and min values" {
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  // Test that adding 1 to max_value overflows
  let overflow_result = @int.max_value + 1
  inspect(overflow_result, content="-2147483648")
  
  // Test that subtracting 1 from min_value overflows
  let underflow_result = @int.min_value - 1
  inspect(underflow_result, content="2147483647")
}

///|
test "int bit operations" {
  let x = 12  // 1100 in binary
  let y = 10  // 1010 in binary
  
  inspect(x & y, content="8")    // 1000
  inspect(x | y, content="14")   // 1110
  inspect(x ^ y, content="6")    // 0110
  inspect(x << 2, content="48")  // 110000
  inspect(y >> 1, content="5")   // 101
}

///|
test "int comparison operations" {
  inspect(5 > 3, content="true")
  inspect(3 > 5, content="false")
  inspect(5 >= 5, content="true")
  inspect(3 < 5, content="true")
  inspect(5 < 3, content="false")
  inspect(3 <= 3, content="true")
  inspect(5 == 5, content="true")
  inspect(5 == 3, content="false")
}