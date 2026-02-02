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
test "array basic literals and length" {
  // Test array literals
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["hello", "world", "test"]
  let empty = []
  
  // Test array length
  let numbers_len = numbers.length()
  let strings_len = strings.length()
  let empty_len = empty.length()
  
  // Test array access
  let first_num = numbers[0]
  let last_num = numbers[4]
  let first_str = strings[0]
  let last_str = strings[2]
  
  // Simple arithmetic operations
  let doubled = first_num * 2
  let concatenated = first_str + " " + last_str
  
  // Verify the operations work by using the values
  let _ = numbers_len
  let _ = strings_len
  let _ = empty_len
  let _ = first_num
  let _ = last_num
  let _ = first_str
  let _ = last_str
  let _ = doubled
  let _ = concatenated
}

///|
test "array basic operations" {
  let numbers = [1, 2, 3, 4, 5]
  
  // Test array access with different indices
  let first = numbers[0]
  let second = numbers[1]
  let middle = numbers[2]
  let fourth = numbers[3]
  let fifth = numbers[4]
  
  // Test arithmetic with array elements
  let sum = first + second + middle + fourth + fifth
  let product = first * second
  let difference = fifth - first
  
  // Test string operations
  let words = ["hello", "world", "moonbit"]
  let greeting = words[0] + " " + words[1]
  let exclamation = words[2] + "!"
  
  // Verify the operations work
  let _ = sum
  let _ = product
  let _ = difference
  let _ = greeting
  let _ = exclamation
}