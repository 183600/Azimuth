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
  inspect(1 + 1, content="2")
  inspect(5 - 3, content="2")
  inspect(3 * 4, content="12")
  inspect(10 / 2, content="5")
  inspect(-5, content="-5")
  inspect(Int::abs(-5), content="5")
  inspect(Int::abs(0), content="0")
}

///|
test "int comparison" {
  inspect(5 > 3, content="true")
  inspect(5 < 3, content="false")
  inspect(5 == 5, content="true")
  inspect(5 != 3, content="true")
  inspect(5 >= 5, content="true")
  inspect(3 <= 5, content="true")
}

///|
test "string operations" {
  let hello = "Hello"
  let world = " World"
  let combined = hello + world
  inspect(combined, content="Hello World")
  inspect(combined.length(), content="11")
  inspect(hello.length(), content="5")
  inspect("".length(), content="0")
}

///|
test "string character access" {
  let text = "MoonBit"
  inspect(text[0], content="'M'")
  inspect(text[3], content="'n'")
  inspect(text[6], content="'t'")
}

///|
test "array operations" {
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  inspect([1, 2, 3].length(), content="3")
}

///|
test "boolean operations" {
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(!true, content="false")
  inspect(!false, content="true")
  inspect(true == true, content="true")
  inspect(false != true, content="true")
}

///|
test "option type operations" {
  let some_value = Some(42)
  let none_value : Option[Int] = None
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
}

///|
test "result type operations" {
  let ok_value = Ok(42)
  let err_value = Err("error")
  inspect(ok_value.is_ok(), content="true")
  inspect(ok_value.is_err(), content="false")
  inspect(err_value.is_ok(), content="false")
  inspect(err_value.is_err(), content="true")
}

///|
test "char operations" {
  let char_a = 'a'
  let char_A = 'A'
  inspect(char_a, content="'a'")
  inspect(char_A, content="'A'")
  inspect('0', content="'0'")
}

///|
test "arithmetic precedence" {
  inspect(2 + 3 * 4, content="14")
  inspect((2 + 3) * 4, content="20")
  inspect(10 - 2 * 3, content="4")
  inspect((10 - 2) * 3, content="24")
}