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

// 标准 MoonBit 测试用例
// 包含核心功能的测试，使用标准的 MoonBit 测试语法

test "int basic arithmetic operations" {
  inspect(1 + 1, content="2")
  inspect(5 - 3, content="2")
  inspect(3 * 4, content="12")
  inspect(10 / 2, content="5")
  inspect(7 % 3, content="1")
}

test "int comparison operations" {
  inspect(5 > 3, content="true")
  inspect(5 < 3, content="false")
  inspect(5 == 5, content="true")
  inspect(5 != 3, content="true")
  inspect(5 >= 5, content="true")
  inspect(3 <= 5, content="true")
}

test "string concatenation and length" {
  let hello = "Hello"
  let world = "World"
  let greeting = hello + " " + world
  inspect(greeting, content="Hello World")
  inspect(greeting.length(), content="11")
}

test "string comparison and operations" {
  let apple = "apple"
  let banana = "banana"
  inspect(apple == "apple", content="true")
  inspect(apple != banana, content="true")
  inspect(apple < banana, content="true")
  inspect("test".length(), content="4")
}

test "boolean logical operations" {
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
}

test "array basic operations" {
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
}

test "option type operations" {
  let some_value = Some(42)
  let none_value = None
  inspect(some_value is Some(_), content="true")
  inspect(none_value is None, content="true")
  inspect(some_value.unwrap(), content="42")
}

test "result type operations" {
  let ok_result = Ok("success")
  let err_result = Err("error")
  inspect(ok_result is Ok(_), content="true")
  inspect(err_result is Err(_), content="true")
  inspect(ok_result.unwrap(), content="success")
}

test "char operations" {
  let char_a = 'a'
  let char_z = 'z'
  inspect(char_a.to_string(), content="a")
  inspect(char_z.to_string(), content="z")
}

test "unit type operations" {
  let unit_value = ()
  inspect(unit_value, content="()")
}