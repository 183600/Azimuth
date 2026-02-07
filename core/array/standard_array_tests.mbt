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
test "array creation and basic operations" {
  let empty = []
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["a", "b", "c"]
  
  assert_eq(empty.length(), 0)
  assert_eq(numbers.length(), 5)
  assert_eq(strings.length(), 3)
  assert_eq(numbers[0], 1)
  assert_eq(strings[2], "c")
}

///|
test "array push and pop" {
  let mut arr = [1, 2, 3]
  arr.push(4)
  assert_eq(arr.length(), 4)
  assert_eq(arr[3], 4)
  
  let popped = arr.pop()
  assert_eq(popped, Some(4))
  assert_eq(arr.length(), 3)
}

///|
test "array map" {
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled, [2, 4, 6, 8, 10])
}

///|
test "array filter" {
  let numbers = [1, 2, 3, 4, 5, 6]
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(evens, [2, 4, 6])
}

///|
test "array fold" {
  let numbers = [1, 2, 3, 4, 5]
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  assert_eq(sum, 15)
}

///|
test "array contains" {
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(numbers.contains(3), true)
  assert_eq(numbers.contains(6), false)
}

///|
test "array slice" {
  let numbers = [1, 2, 3, 4, 5, 6]
  let slice = numbers.slice(1, 4)
  assert_eq(slice, [2, 3, 4])
}

///|
test "array sort" {
  let mut numbers = [3, 1, 4, 1, 5, 9, 2, 6]
  numbers.sort()
  assert_eq(numbers, [1, 1, 2, 3, 4, 5, 6, 9])
}

///|
test "array reverse" {
  let mut numbers = [1, 2, 3, 4, 5]
  numbers.reverse()
  assert_eq(numbers, [5, 4, 3, 2, 1])
}