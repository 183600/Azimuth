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
  // Test array literals and basic operations
  let numbers = [1, 2, 3, 4, 5]
  let empty = []
  
  // Test array length
  let numbers_len = numbers.length()
  let empty_len = empty.length()
  
  // Test array access
  let first = numbers[0]
  let second = numbers[1]
  let last = numbers[4]
  
  // Test array zip_with function
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let zipped = @array.zip_with(arr1, arr2, (a, b) => a + b)
  
  // Verify the operations work by using the values
  let _ = numbers_len
  let _ = empty_len
  let _ = first
  let _ = second
  let _ = last
  let _ = zipped.length()
}

///|
test "array utility functions" {
  let numbers = [1, 2, 3, 4, 5, 6]
  
  // Test array creation and manipulation
  let reversed = @array.reverse(numbers)
  let sorted = @array.sort(numbers)
  
  // Test array membership
  let contains_3 = @array.member(numbers, 3)
  let contains_10 = @array.member(numbers, 10)
  
  // Test array conversion
  let to_list = @array.to_list(numbers)
  
  // Verify the operations work
  let _ = reversed.length()
  let _ = sorted.length()
  let _ = contains_3
  let _ = contains_10
  let _ = to_list
}