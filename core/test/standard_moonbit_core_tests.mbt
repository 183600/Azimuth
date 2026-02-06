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

// 导入必要的函数
using @builtin { inspect }

///|
test "integer arithmetic" {
  inspect(1 + 2, content="3")
  inspect(10 - 3, content="7")
  inspect(4 * 5, content="20")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
}

///|
test "integer comparisons" {
  inspect(5 > 3, content="true")
  inspect(2 < 4, content="true")
  inspect(7 == 7, content="true")
  inspect(9 != 5, content="true")
  inspect(6 >= 6, content="true")
  inspect(3 <= 8, content="true")
}

///|
test "string operations" {
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  inspect(combined, content="Hello World")
  inspect(combined.length(), content="11")
  inspect(hello == "Hello", content="true")
  inspect(hello != world, content="true")
}

///|
test "string comparisons" {
  let a = "apple"
  let b = "banana"
  let c = "apple"
  
  inspect(a == c, content="true")
  inspect(a == b, content="false")
  inspect(a < b, content="true")
  inspect(b > a, content="true")
}

///|
test "array operations" {
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  inspect(sum, content="15")
}

///|
test "array string operations" {
  let strings = ["a", "b", "c"]
  inspect(strings.length(), content="3")
  inspect(strings[0], content="a")
  inspect(strings[2], content="c")
  let combined = strings[0] + strings[1] + strings[2]
  inspect(combined, content="abc")
}

///|
test "boolean operations" {
  let t = true
  let f = false
  inspect(t && f, content="false")
  inspect(t || f, content="true")
  inspect(!t, content="false")
  inspect(!f, content="true")
}

///|
test "conditional expressions" {
  let x = 5
  let result = if x > 0 { "positive" } else { "non-positive" }
  inspect(result, content="positive")
  
  let y = -3
  let result2 = if y > 0 { "positive" } else { "non-positive" }
  inspect(result2, content="non-positive")
}

///|
test "function definitions" {
  let add = (a, b) => a + b
  let multiply = (a, b) => a * b
  
  inspect(add(2, 3), content="5")
  inspect(multiply(4, 5), content="20")
  
  let square = x => x * x
  inspect(square(7), content="49")
}

///|
test "nested expressions" {
  let a = 2
  let b = 3
  let c = 4
  
  let result = (a + b) * c
  inspect(result, content="20")
  
  let nested = if a > b { a * c } else { b * c }
  inspect(nested, content="12")
}