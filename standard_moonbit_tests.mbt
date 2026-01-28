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

// Standard MoonBit Test Suite
// This file contains standard test cases for core MoonBit modules

///|
test "int absolute value edge cases" {
  inspect(Int::abs(0), content="0")
  inspect(Int::abs(1), content="1")
  inspect(Int::abs(-1), content="1")
  inspect(Int::abs(@int.max_value), content="2147483647")
  inspect(Int::abs(@int.min_value), content="-2147483648")  // min_value.abs() stays negative due to overflow
}

///|
test "int comparison operations" {
  assert_eq(10 > 5, true)
  assert_eq(10 < 5, false)
  assert_eq(10 >= 10, true)
  assert_eq(10 <= 5, false)
  assert_eq(10 == 10, true)
  assert_eq(10 != 5, true)
}

///|
test "int signed shift operations" {
  inspect(10 >> 2, content="2")
  inspect(-10 >> 2, content="-3")
  inspect(10 << 2, content="40")
  inspect(-10 << 2, content="-40")
}

///|
test "list basic operations" {
  let empty_list : @list.List[Int] = @list.empty()
  let single_element = @list.singleton(42)
  let multi_element = @list.from_array([1, 2, 3])
  
  inspect(empty_list.length(), content="0")
  inspect(single_element.length(), content="1")
  inspect(multi_element.length(), content="3")
  
  assert_eq(@list.is_empty(empty_list), true)
  assert_eq(@list.is_empty(single_element), false)
}

///|
test "list element access" {
  let ls = @list.from_array([10, 20, 30, 40, 50])
  
  inspect(ls[0], content="10")
  inspect(ls[2], content="30")
  inspect(ls[4], content="50")
  
  inspect(ls.first(), content="Some(10)")
  inspect(ls.last(), content="Some(50)")
  
  let empty_ls : @list.List[Int] = @list.empty()
  inspect(empty_ls.first(), content="None")
  inspect(empty_ls.last(), content="None")
}

///|
test "list transformations" {
  let ls = @list.from_array([1, 2, 3, 4, 5])
  
  let doubled = ls.map(fn(x) { x * 2 })
  inspect(doubled, content="@list.from_array([2, 4, 6, 8, 10])")
  
  let evens = ls.filter(fn(x) { x % 2 == 0 })
  inspect(evens, content="@list.from_array([2, 4])")
  
  let sum = ls.fold(0, fn(acc, x) { acc + x })
  inspect(sum, content="15")
}

///|
test "string basic operations" {
  let empty_str = ""
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  
  inspect(empty_str.length(), content="0")
  inspect(hello.length(), content="5")
  inspect(combined, content="Hello World")
  inspect(combined.length(), content="11")
}

///|
test "string character operations" {
  let str = "MoonBit"
  
  inspect(str[0], content="'M'")
  inspect(str[3], content="'B'")
  inspect(str[6], content="'t'")
  
  inspect(str.char_at(0), content="Some('M')")
  inspect(str.char_at(10), content="None")
}

///|
test "string case conversions" {
  let lower = "moonbit"
  let upper = "MOONBIT"
  let mixed = "MoonBit"
  
  inspect(lower.to_uppercase(), content="MOONBIT")
  inspect(upper.to_lowercase(), content="moonbit")
  inspect(mixed.to_uppercase(), content="MOONBIT")
  inspect(mixed.to_lowercase(), content="moonbit")
}