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
test "basic integer arithmetic" {
  inspect(1 + 2, content="3")
  inspect(5 - 3, content="2")
  inspect(4 * 3, content="12")
  inspect(10 / 2, content="5")
  inspect(10 % 3, content="1")
}

///|
test "string concatenation" {
  let str1 = "Hello"
  let str2 = " World"
  let combined = str1 + str2
  inspect(combined, content="Hello World")
}

///|
test "array basic operations" {
  let arr = [1, 2, 3, 4, 5]
  inspect(arr.length(), content="5")
  inspect(arr[0], content="1")
  inspect(arr[4], content="5")
}

///|
test "boolean logic operations" {
  inspect(true && false, content="false")
  inspect(true || false, content="true")
}

///|
test "comparison operations" {
  inspect(10 > 5, content="true")
  inspect(5 < 10, content="true")
  inspect(10 >= 10, content="true")
  inspect(5 <= 10, content="true")
  inspect(10 == 10, content="true")
  inspect(5 != 10, content="true")
}

///|
test "bitwise operations" {
  inspect(5 & 3, content="1")
  inspect(5 | 3, content="7")
  inspect(5 ^ 3, content="6")
  inspect(5 << 1, content="10")
  inspect(5 >> 1, content="2")
}

///|
test "integer boundary values" {
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
}

///|
test "absolute value function" {
  inspect(Int::abs(5), content="5")
  inspect(Int::abs(-5), content="5")
  inspect(Int::abs(0), content="0")
}

///|
test "power function" {
  inspect(Int::pow(2, 0), content="1")
  inspect(Int::pow(2, 1), content="2")
  inspect(Int::pow(2, 3), content="8")
  inspect(Int::pow(3, 2), content="9")
}

///|
test "integer overflow behavior" {
  inspect(@int.max_value + 1, content="-2147483648")
  inspect(@int.min_value - 1, content="2147483647")
}