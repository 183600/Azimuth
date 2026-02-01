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

test "integer arithmetic operations" {
  let a = 42
  let b = 17
  inspect(a + b, content="59")
  inspect(a - b, content="25")
  inspect(a * b, content="714")
  inspect(a / b, content="2")
  inspect(a % b, content="8")
}

///|
test "integer arithmetic operations" {
  let a = 42
  let b = 17
  assert_eq(a + b, 59)
  assert_eq(a - b, 25)
  assert_eq(a * b, 714)
  assert_eq(a / b, 2)
  assert_eq(a % b, 8)
}

///|
test "string operations and concatenation" {
  let hello = "Hello"
  let world = "World"
  let combined = hello + ", " + world + "!"
  inspect(combined, content="Hello, World!")
  inspect(hello.length(), content="5")
  inspect(world.length(), content="5")
}

///|
test "option type basic operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  inspect(some_value.unwrap_or(0), content="42")
  inspect(none_value.unwrap_or(0), content="0")
  inspect(some_value.map(x => x * 2), content="Some(84)")
  inspect(none_value.map(x => x * 2), content="None")
}

///|
test "boolean logic operations" {
  let true_val = true
  let false_val = false
  inspect(true_val && false_val, content="false")
  inspect(true_val || false_val, content="true")
  inspect(!true_val, content="false")
  inspect(!false_val, content="true")
}

///|
test "array basic operations" {
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="[2, 4, 6, 8, 10]")
}

///|
test "comparison operations" {
  let a = 10
  let b = 20
  let c = 10
  inspect(a < b, content="true")
  inspect(b > a, content="true")
  inspect(a <= c, content="true")
  inspect(a >= c, content="true")
  inspect(a == c, content="true")
  inspect(a != b, content="true")
}

///|
test "type conversion operations" {
  let int_val = 42
  let string_val = int_val.to_string()
  inspect(string_val, content="\"42\"")
  let char_val = 'A'
  let char_string = char_val.to_string()
  inspect(char_string, content="\"A\"")
}

///|
test "result type operations" {
  let success : Result[Int, String] = Ok(42)
  let failure : Result[Int, String] = Err("error")
  inspect(success.unwrap_or(0), content="42")
  inspect(failure.unwrap_or(0), content="0")
  inspect(success.map(x => x * 2), content="Ok(84)")
  inspect(failure.map(x => x * 2), content="Err(\"error\")")
}

///|
test "tuple operations" {
  let pair = (42, "hello")
  let (num, str) = pair
  inspect(num, content="42")
  inspect(str, content="\"hello\"")
  let triple = (1, 2, 3)
  inspect(triple.0, content="1")
  inspect(triple.1, content="2")
  inspect(triple.2, content="3")
}

///|
test "math functions and constants" {
  let x = -42
  let abs_x = @int.abs(x)
  inspect(abs_x, content="42")
  let max_val = @int.max_value
  let min_val = @int.min_value
  inspect(max_val > 0, content="true")
  inspect(min_val < 0, content="true")
}