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
test "array basic operations" {
  let empty = []
  let single = [1]
  let numbers = [1, 2, 3, 4, 5]
  
  assert_eq(empty.length(), 0)
  assert_eq(single.length(), 1)
  assert_eq(numbers.length(), 5)
  
  assert_eq(empty.is_empty(), true)
  assert_eq(single.is_empty(), false)
  assert_eq(numbers.is_empty(), false)
}

///|
test "array access operations" {
  let numbers = [10, 20, 30, 40, 50]
  
  assert_eq(numbers[0], 10)
  assert_eq(numbers[2], 30)
  assert_eq(numbers[4], 50)
  
  assert_eq(numbers.first(), Some(10))
  assert_eq(numbers.last(), Some(50))
  
  let empty = []
  assert_eq(empty.first(), None[Int])
  assert_eq(empty.last(), None[Int])
}

///|
test "array slicing operations" {
  let numbers = [1, 2, 3, 4, 5]
  
  assert_eq(numbers[0..3], [1, 2, 3])
  assert_eq(numbers[1..4], [2, 3, 4])
  assert_eq(numbers[2..5], [3, 4, 5])
  assert_eq(numbers[0..0], [])
  assert_eq(numbers[5..5], [])
  assert_eq(numbers[0..5], [1, 2, 3, 4, 5])
}

///|
test "array search operations" {
  let numbers = [1, 2, 3, 4, 5, 3, 2, 1]
  
  assert_eq(numbers.index_of(3), Some(2))
  assert_eq(numbers.index_of(1), Some(0))
  assert_eq(numbers.index_of(5), Some(4))
  assert_eq(numbers.index_of(6), None[Int])
  
  assert_eq(numbers.last_index_of(3), Some(5))
  assert_eq(numbers.last_index_of(1), Some(7))
  assert_eq(numbers.last_index_of(5), Some(4))
  assert_eq(numbers.last_index_of(6), None[Int])
  
  assert_eq(numbers.contains(3), true)
  assert_eq(numbers.contains(6), false)
}

///|
test "array modification operations" {
  let mut numbers = [1, 2, 3]
  
  numbers.push(4)
  assert_eq(numbers, [1, 2, 3, 4])
  
  let popped = numbers.pop()
  assert_eq(popped, Some(4))
  assert_eq(numbers, [1, 2, 3])
  
  let popped_from_empty = [].pop()
  assert_eq(popped_from_empty, None[Int])
}

///|
test "array insertion operations" {
  let mut numbers = [1, 2, 4, 5]
  
  numbers.insert(2, 3)
  assert_eq(numbers, [1, 2, 3, 4, 5])
  
  numbers.insert(0, 0)
  assert_eq(numbers, [0, 1, 2, 3, 4, 5])
  
  numbers.insert(6, 6)
  assert_eq(numbers, [0, 1, 2, 3, 4, 5, 6])
}

///|
test "array removal operations" {
  let mut numbers = [1, 2, 3, 4, 5]
  
  let removed = numbers.remove(2)
  assert_eq(removed, 3)
  assert_eq(numbers, [1, 2, 4, 5])
  
  let removed_first = numbers.remove(0)
  assert_eq(removed_first, 1)
  assert_eq(numbers, [2, 4, 5])
  
  let removed_last = numbers.remove(2)
  assert_eq(removed_last, 5)
  assert_eq(numbers, [2, 4])
}

///|
test "array concatenation" {
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let empty = []
  
  let result1 = arr1 + arr2
  assert_eq(result1, [1, 2, 3, 4, 5, 6])
  
  let result2 = arr1 + empty
  assert_eq(result2, [1, 2, 3])
  
  let result3 = empty + arr2
  assert_eq(result3, [4, 5, 6])
  
  let result4 = empty + empty
  assert_eq(result4, [])
}

///|
test "array iteration" {
  let numbers = [1, 2, 3, 4, 5]
  let mut sum = 0
  
  for num in numbers {
    sum = sum + num
  }
  assert_eq(sum, 15)
  
  // Test with empty array
  let mut empty_sum = 0
  for num in [] {
    empty_sum = empty_sum + num
  }
  assert_eq(empty_sum, 0)
}

///|
test "array map operation" {
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(fn(x) { x * 2 })
  
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let empty = []
  let mapped_empty = empty.map(fn(x) { x * 2 })
  assert_eq(mapped_empty, [])
}

///|
test "array filter operation" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  
  assert_eq(evens, [2, 4, 6, 8, 10])
  
  let odds = numbers.filter(fn(x) { x % 2 == 1 })
  assert_eq(odds, [1, 3, 5, 7, 9])
  
  let empty = []
  let filtered_empty = empty.filter(fn(x) { x % 2 == 0 })
  assert_eq(filtered_empty, [])
}

///|
test "array fold operation" {
  let numbers = [1, 2, 3, 4, 5]
  let sum = numbers.fold(0, fn(acc, x) { acc + x })
  
  assert_eq(sum, 15)
  
  let product = numbers.fold(1, fn(acc, x) { acc * x })
  assert_eq(product, 120)
  
  let empty = []
  let empty_sum = empty.fold(0, fn(acc, x) { acc + x })
  assert_eq(empty_sum, 0)
}

///|
test "array reverse operation" {
  let numbers = [1, 2, 3, 4, 5]
  let reversed = numbers.reverse()
  
  assert_eq(reversed, [5, 4, 3, 2, 1])
  
  let empty = []
  let reversed_empty = empty.reverse()
  assert_eq(reversed_empty, [])
  
  let single = [42]
  let reversed_single = single.reverse()
  assert_eq(reversed_single, [42])
}

///|
test "array sort operation" {
  let numbers = [5, 2, 8, 1, 9, 3]
  let sorted = numbers.sort()
  
  assert_eq(sorted, [1, 2, 3, 5, 8, 9])
  
  let strings = ["banana", "apple", "cherry", "date"]
  let sorted_strings = strings.sort()
  
  assert_eq(sorted_strings, ["apple", "banana", "cherry", "date"])
  
  let empty = []
  let sorted_empty = empty.sort()
  assert_eq(sorted_empty, [])
}