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
test "integer arithmetic" {
  assert_eq(1 + 2, 3)
  assert_eq(10 - 3, 7)
  assert_eq(4 * 5, 20)
  assert_eq(15 / 3, 5)
  assert_eq(17 % 5, 2)
}

///|
test "integer comparisons" {
  assert_eq(5 > 3, true)
  assert_eq(2 < 4, true)
  assert_eq(7 == 7, true)
  assert_eq(9 != 5, true)
  assert_eq(6 >= 6, true)
  assert_eq(3 <= 8, true)
}

///|
test "string operations" {
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  assert_eq(combined, "Hello World")
  assert_eq(combined.length(), 11)
  assert_eq(hello == "Hello", true)
  assert_eq(hello != world, true)
}

///|
test "string comparisons" {
  let a = "apple"
  let b = "banana"
  let c = "apple"
  
  assert_eq(a == c, true)
  assert_eq(a == b, false)
  assert_eq(a < b, true)
  assert_eq(b > a, true)
}

///|
test "array operations" {
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  assert_eq(sum, 15)
}

///|
test "array string operations" {
  let strings = ["a", "b", "c"]
  assert_eq(strings.length(), 3)
  assert_eq(strings[0], "a")
  assert_eq(strings[2], "c")
  let combined = strings[0] + strings[1] + strings[2]
  assert_eq(combined, "abc")
}

///|
test "boolean operations" {
  let t = true
  let f = false
  assert_eq(t && f, false)
  assert_eq(t || f, true)
  assert_eq(!t, false)
  assert_eq(!f, true)
}

///|
test "conditional expressions" {
  let x = 5
  let result = if x > 0 { "positive" } else { "non-positive" }
  assert_eq(result, "positive")
  
  let y = -3
  let result2 = if y > 0 { "positive" } else { "non-positive" }
  assert_eq(result2, "non-positive")
}

///|
test "function definitions" {
  let add = (a, b) => a + b
  let multiply = (a, b) => a * b
  
  assert_eq(add(2, 3), 5)
  assert_eq(multiply(4, 5), 20)
  
  let square = x => x * x
  assert_eq(square(7), 49)
}

///|
test "nested expressions" {
  let a = 2
  let b = 3
  let c = 4
  
  let result = (a + b) * c
  assert_eq(result, 20)
  
  let nested = if a > b { a * c } else { b * c }
  assert_eq(nested, 12)
}