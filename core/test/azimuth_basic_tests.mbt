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
test "int_addition_basic" {
  inspect(1 + 1, content="2")
  inspect(10 + 5, content="15")
  inspect(-3 + 7, content="4")
  inspect(-5 + -3, content="-8")
}

///|
test "int_subtraction_basic" {
  inspect(10 - 5, content="5")
  inspect(5 - 10, content="-5")
  inspect(-3 - 7, content="-10")
  inspect(-5 - -3, content="-2")
}

///|
test "int_multiplication_basic" {
  inspect(2 * 3, content="6")
  inspect(5 * 0, content="0")
  inspect(-2 * 3, content="-6")
  inspect(-2 * -3, content="6")
}

///|
test "int_division_basic" {
  inspect(10 / 2, content="5")
  inspect(7 / 2, content="3")
  inspect(-10 / 2, content="-5")
  inspect(-7 / 2, content="-3")
}

///|
test "int_modulo_basic" {
  inspect(10 % 3, content="1")
  inspect(7 % 2, content="1")
  inspect(-10 % 3, content="-1")
  inspect(-7 % 2, content="-1")
}

///|
test "int_comparison_operations" {
  inspect(1 > 0, content="true")
  inspect(0 < 1, content="true")
  inspect(1 >= 1, content="true")
  inspect(1 <= 1, content="true")
  inspect(1 == 1, content="true")
  inspect(1 != 0, content="true")
}

///|
test "int_boundary_values" {
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  inspect(@int.max_value + 1, content="-2147483648")
  inspect(@int.min_value - 1, content="2147483647")
}

///|
test "string_concatenation" {
  inspect("" + "", content="")
  inspect("Hello" + " " + "World", content="Hello World")
  inspect("a" + "b" + "c", content="abc")
  inspect("123" + "456", content="123456")
}

///|
test "string_equality" {
  inspect("hello" == "hello", content="true")
  inspect("hello" == "world", content="false")
  inspect("" == "", content="true")
  inspect("a" == "A", content="false")
}

///|
test "boolean_basic_operations" {
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false || true, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
}