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
test "int basic arithmetic" {
  let a = 15
  let b = 4
  let c = -7
  
  inspect(a + b, content="19")
  inspect(a - b, content="11")
  inspect(a * b, content="60")
  inspect(a / b, content="3")
  inspect(a % b, content="3")
  inspect(@int.abs(c), content="7")
}

///|
test "int boundary values" {
  let max_val = @int.max_value
  let min_val = @int.min_value
  
  inspect(max_val, content="2147483647")
  inspect(min_val, content="-2147483648")
  inspect(max_val > 0, content="true")
  inspect(min_val < 0, content="true")
}

///|
test "string basic operations" {
  let s1 = "Hello"
  let s2 = "World"
  let combined = s1 + " " + s2
  let empty = ""
  
  inspect(combined, content="Hello World")
  inspect(s1, content="Hello")
  inspect(s2, content="World")
  inspect(empty, content="")
  inspect(s1 == "Hello", content="true")
  inspect(s1 != s2, content="true")
}

///|
test "string length operations" {
  let short = "Hi"
  let medium = "MoonBit"
  let long = "Programming Language"
  let empty = ""
  
  inspect(short.length(), content="2")
  inspect(medium.length(), content="7")
  inspect(long.length(), content="20")
  inspect(empty.length(), content="0")
}

///|
test "array basic operations" {
  let numbers = [1, 2, 3, 4, 5]
  let first = numbers[0]
  let last = numbers[4]
  let length = numbers.length()
  
  inspect(first, content="1")
  inspect(last, content="5")
  inspect(length, content="5")
  inspect(numbers, content="[1, 2, 3, 4, 5]")
}

///|
test "array transformations" {
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(x => x * 2)
  let evens = numbers.filter(x => x % 2 == 0)
  let sum = numbers.fold(0, (acc, x) => acc + x)
  
  inspect(doubled, content="[2, 4, 6, 8, 10]")
  inspect(evens, content="[2, 4]")
  inspect(sum, content="15")
}

///|
test "boolean logic operations" {
  let t = true
  let f = false
  
  inspect(t && f, content="false")
  inspect(t || f, content="true")
  inspect(t && t, content="true")
  inspect(f || f, content="false")
  inspect(!t, content="false")
  inspect(!f, content="true")
  inspect(t == f, content="false")
  inspect(t != f, content="true")
}

///|
test "result type operations" {
  let success : Result[String, String] = Ok("success")
  let failure : Result[String, String] = Err("error")
  
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(failure.is_ok(), content="false")
  inspect(failure.is_err(), content="true")
  inspect(success.unwrap(), content="success")
  inspect(failure.unwrap_err(), content="error")
}

///|
test "char operations" {
  let char_a = 'a'
  let char_z = 'z'
  let char_num = '5'
  
  inspect(char_a.to_string(), content="a")
  inspect(char_z.to_string(), content="z")
  inspect(char_num.to_string(), content="5")
  inspect(char_a < char_z, content="true")
  inspect(char_a == 'a', content="true")
  inspect(char_a != 'b', content="true")
}

///|
test "tuple operations" {
  let pair = (42, "answer")
  let triple = (1, 2, 3)
  
  inspect(pair.0, content="42")
  inspect(pair.1, content="answer")
  inspect(triple.0, content="1")
  inspect(triple.1, content="2")
  inspect(triple.2, content="3")
}