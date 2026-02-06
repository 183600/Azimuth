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
  inspect(5 - 3, content="2")
  inspect(3 * 4, content="12")
  inspect(8 / 2, content="4")
  inspect(10 % 3, content="1")
}

///|
test "integer comparisons" {
  inspect(5 > 3, content="true")
  inspect(3 >= 3, content="true")
  inspect(5 < 10, content="true")
  inspect(10 <= 10, content="true")
  inspect(7 == 7, content="true")
  inspect(8 != 9, content="true")
}

///|
test "boolean logic" {
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(!true, content="false")
  inspect(!false, content="true")
}

///|
test "string concatenation" {
  let hello = "Hello"
  let world = "World"
  let space = " "
  inspect(hello + space + world, content="Hello World")
  inspect(hello.length(), content="5")
}

///|
test "tuple operations" {
  let pair = (42, "answer")
  inspect(pair.0, content="42")
  inspect(pair.1, content="answer")
  
  let (first, second) = pair
  inspect(first, content="42")
  inspect(second, content="answer")
}

///|
test "option type basics" {
  let some_value = Some(100)
  let none_value = None
  inspect(some_value, content="Some(100)")
  inspect(none_value, content="None")
}

///|
test "result type basics" {
  let success = Ok("success")
  let failure = Err("error")
  inspect(success, content="Ok(\"success\")")
  inspect(failure, content="Err(\"error\")")
}

///|
test "character operations" {
  let char_a = 'a'
  let char_A = 'A'
  inspect(char_a.to_string(), content="a")
  inspect(char_A.to_string(), content="A")
  inspect(char_a.to_byte(), content="97")
}

///|
test "array operations" {
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
}