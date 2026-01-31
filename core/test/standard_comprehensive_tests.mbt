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
test "integer arithmetic operations" {
  inspect(1 + 2, content="3")
  inspect(5 - 3, content="2")
  inspect(4 * 3, content="12")
  inspect(10 / 2, content="5")
  inspect(10 % 3, content="1")
  inspect(Int::abs(-5), content="5")
  inspect(Int::abs(0), content="0")
  inspect(Int::abs(7), content="7")
}

///|
test "integer comparison operations" {
  inspect(1 < 2, content="true")
  inspect(2 > 1, content="true")
  inspect(3 <= 3, content="true")
  inspect(4 >= 4, content="true")
  inspect(5 == 5, content="true")
  inspect(5 != 6, content="true")
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
test "array basic operations" {
  let empty : Array[Int] = []
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["hello", "world", "moonbit"]
  
  inspect(empty.length(), content="0")
  inspect(numbers.length(), content="3")
  inspect(strings.length(), content="3")
  inspect(numbers[0], content="1")
  inspect(strings[2], content="moonbit")
}

///|
test "string concatenation and operations" {
  let str1 = "hello"
  let str2 = "world"
  let combined = str1 + " " + str2
  inspect(combined, content="hello world")
  inspect(str1.length(), content="5")
  inspect(combined.length(), content="11")
}

///|
test "array map transformation" {
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="[2, 4, 6, 8, 10]")
}

///|
test "array filter operation" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens, content="[2, 4, 6, 8, 10]")
}

///|
test "option type operations" {
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  
  inspect(some_value.is_some(), content="true")
  inspect(none_value.is_none(), content="true")
  inspect(some_value.unwrap(), content="42")
}

///|
test "result type operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  inspect(ok_result.is_ok(), content="true")
  inspect(err_result.is_err(), content="true")
  inspect(ok_result.unwrap(), content="42")
}