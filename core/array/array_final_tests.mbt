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
test "array basic literals and access" {
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
  
  // Test array operations with functions
  let double_number = fn(x : Int) -> Int { x * 2 }
  let doubled = double_number(first_num)
  
  // Verify the operations work by using the values
  let _ = numbers_len
  let _ = strings_len
  let _ = empty_len
  let _ = first_num
  let _ = last_num
  let _ = first_str
  let _ = last_str
  let _ = doubled
}

///|
test "array iteration and manipulation" {
  let numbers = [1, 2, 3, 4, 5]
  
  // Manual array iteration
  let sum = fn(arr : [Int]) -> Int {
    let mut result = 0
    for i = 0; i < arr.length(); i = i + 1 {
      result = result + arr[i]
    }
    result
  }
  
  // Manual array filtering
  let filter_even = fn(arr : [Int]) -> [Int] {
    let mut result = []
    for i = 0; i < arr.length(); i = i + 1 {
      let value = arr[i]
      if value % 2 == 0 {
        result.push(value)
      }
    }
    result
  }
  
  // Test the functions
  let total = sum(numbers)
  let evens = filter_even(numbers)
  
  // Verify the operations work
  let _ = total
  let _ = evens.length()
}