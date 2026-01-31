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
test "int_basic_arithmetic" {
  inspect(10 + 5, content="15")
  inspect(10 - 3, content="7")
  inspect(6 * 7, content="42")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
}

///|
test "int_boundary_values" {
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  assert_eq(@int.max_value + 1, @int.min_value)
  assert_eq(@int.min_value - 1, @int.max_value)
}

///|
test "int_bitwise_operations" {
  inspect(12 & 10, content="8")
  inspect(12 | 10, content="14")
  inspect(12 ^ 10, content="6")
  inspect(8 << 2, content="32")
  inspect(32 >> 2, content="8")
}

///|
test "string_basic_operations" {
  let hello = "Hello"
  let world = "World"
  let greeting = hello + " " + world
  inspect(greeting, content="Hello World")
  assert_eq(hello.length(), 5)
  assert_eq(world.length(), 5)
  assert_eq(greeting.length(), 11)
}

///|
test "string_comparison" {
  let str1 = "apple"
  let str2 = "apple"
  let str3 = "banana"
  assert_true(str1 == str2)
  assert_true(str1 != str3)
  assert_false(str1 == str3)
}

///|
test "array_basic_operations" {
  let empty : Array[Int] = []
  let numbers = [1, 2, 3, 4, 5]
  inspect(empty.length(), content="0")
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
}

///|
test "array_zip_with_function" {
  let arr1 = [1, 2, 3]
  let arr2 = [10, 20, 30]
  let result = @array.zip_with(arr1, arr2, (a, b) => a * b)?
  inspect(result, content="[10, 40, 90]")
}

///|
test "array_creation_methods" {
  let manual = [1, 2, 3]
  let from_make = Array::make(3, 42)
  let from_makei = Array::makei(3, i => i * 2)
  inspect(manual.length(), content="3")
  inspect(from_make.length(), content="3")
  inspect(from_makei.length(), content="3")
  inspect(from_makei[1], content="2")
}

///|
test "mixed_type_operations" {
  let num = 42
  let str_num = num.to_string()
  let back_to_num = Int::parse(str_num)?
  inspect(str_num, content="42")
  inspect(back_to_num, content="42")
}