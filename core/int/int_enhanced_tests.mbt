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
  
  inspect(x + y, content="25")
  inspect(x - y, content="59")
  inspect(x * y, content="-714")
  inspect(x / y, content="-2")
  inspect(x % y, content="8")
  
  inspect(Int::abs(y), content="17")
  inspect(Int::abs(zero), content="0")
  inspect(Int::abs(x), content="42")
}

///|
test "int comparison operations" {
  let a = 10
  let b = 20
  let c = 10
  
  inspect(a < b, content="true")
  inspect(a > b, content="false")
  inspect(a <= b, content="true")
  inspect(a >= b, content="false")
  inspect(a == b, content="false")
  inspect(a != b, content="true")
  inspect(a == c, content="true")
  inspect(a <= c, content="true")
  inspect(a >= c, content="true")
}

///|
test "int boundary values" {
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  let max_val = @int.max_value
  let min_val = @int.min_value
  
  inspect(max_val + 1, content="-2147483648")
  inspect(min_val - 1, content="2147483647")
}

///|
test "int bit operations" {
  let a = 12  // 1100 in binary
  let b = 10  // 1010 in binary
  
  inspect(a & b, content="8")   // 1000
  inspect(a | b, content="14")  // 1110
  inspect(a ^ b, content="6")   // 0110
  inspect(a << 2, content="48") // 110000
  inspect(a >> 1, content="6")  // 110
}

///|
test "int to string conversion" {
  let positive = 123
  let negative = -456
  let zero = 0
  
  inspect(positive.to_string(), content="123")
  inspect(negative.to_string(), content="-456")
  inspect(zero.to_string(), content="0")
}