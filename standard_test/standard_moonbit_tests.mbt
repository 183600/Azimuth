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
test "integer basic arithmetic" {
  inspect(1 + 2, content="3")
  inspect(10 - 3, content="7")
  inspect(4 * 5, content="20")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
}

///|
test "integer comparison operations" {
  inspect(5 > 3, content="true")
  inspect(5 < 3, content="false")
  inspect(5 >= 5, content="true")
  inspect(5 <= 3, content="false")
  inspect(5 == 5, content="true")
  inspect(5 != 3, content="true")
}

///|
test "string basic operations" {
  let str1 = "Hello"
  let str2 = "World"
  let combined = str1 + " " + str2
  inspect(combined, content="Hello World")
  inspect(str1.length(), content="5")
  inspect(combined.length(), content="11")
}

///|
test "array basic operations" {
  let numbers = [1, 2, 3, 4, 5]
  let empty = []
  inspect(numbers.length(), content="5")
  inspect(empty.length(), content="0")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
}

///|
test "boolean logic operations" {
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false || true, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
}

///|
test "integer bit operations" {
  inspect(5 & 3, content="1")
  inspect(5 | 3, content="7")
  inspect(5 ^ 3, content="6")
  inspect(5 << 2, content="20")
  inspect(5 >> 1, content="2")
}

///|
test "string comparison and indexing" {
  let text = "MoonBit"
  inspect(text[0], content="'M'")
  inspect(text[3], content="'n'")
  inspect(text.length(), content="7")
  inspect(text > "Apple", content="true")
  inspect(text < "Zebra", content="true")
  inspect(text == "MoonBit", content="true")
  inspect(text != "moonbit", content="true")
}

///|
test "array operations with different types" {
  let numbers = [10, 20, 30]
  let strings = ["a", "b", "c"]
  inspect(numbers[1] + numbers[2], content="50")
  inspect(strings[0] + strings[1], content="ab")
  inspect(numbers.length(), content="3")
  inspect(strings.length(), content="3")
}

///|
test "integer boundary values" {
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  inspect(@int.max_value + 1, content="-2147483648")
  inspect(@int.min_value - 1, content="2147483647")
}

///|
test "type conversion and casting" {
  let int_val = 42
  let bool_true = true
  let bool_false = false
  inspect(int_val.to_string(), content="42")
  inspect(bool_true.to_string(), content="true")
  inspect(bool_false.to_string(), content="false")
}