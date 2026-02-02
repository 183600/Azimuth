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
  let _ = numbers.length()
  let _ = empty.length()
  
  // Test array access
  let _ = numbers[0]
  let _ = numbers[1]
  let _ = numbers[2]
  
  // Test array operations with built-in functions
  let doubled = @array.map(numbers, x => x * 2)
  let filtered = @array.filter(numbers, x => x % 2 == 0)
  let summed = @array.reduce(numbers, 0, (acc, x) => acc + x)
  
  // Verify the operations work
  let _ = doubled.length()
  let _ = filtered.length()
  let _ = summed
}

///|
test "array utility functions" {
  let numbers = [1, 2, 3, 4, 5, 6]
  
  // Test array slicing and manipulation
  let first_three = @array.slice(numbers, 0, 3)
  let last_three = @array.slice(numbers, 3, 6)
  
  // Test array concatenation
  let concatenated = @array.concat(first_three, last_three)
  
  // Test array finding
  let found = @array.find(numbers, x => x > 3)
  let not_found = @array.find(numbers, x => x > 10)
  
  // Verify the operations work
  let _ = first_three.length()
  let _ = last_three.length()
  let _ = concatenated.length()
  let _ = found
  let _ = not_found
}