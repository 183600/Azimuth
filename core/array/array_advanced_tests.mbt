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
test "array zip_with different lengths" {
  let longer = [1, 2, 3, 4, 5]
  let shorter = [10, 20, 30]
  let single = [100]
  let empty : Array[Int] = []
  
  // Test with different array lengths
  let result1 = @array.zip_with(longer, shorter, (a, b) => a + b)?
  inspect(result1, content="[11, 22, 33]")
  
  let result2 = @array.zip_with(shorter, longer, (a, b) => a * b)?
  inspect(result2, content="[10, 40, 90]")
  
  let result3 = @array.zip_with(single, longer, (a, b) => a - b)?
  inspect(result3, content="[99]")
  
  let result4 = @array.zip_with(empty, longer, (a, b) => a + b)?
  inspect(result4, content="[]")
}

///|
test "array zip_with complex operations" {
  let strings = ["hello", "world", "moonbit"]
  let numbers = [1, 2, 3]
  
  // Test string concatenation with numbers
  let result1 = @array.zip_with(strings, numbers, (s, n) => s + n.to_string())?
  inspect(result1, content="['hello1', 'world2', 'moonbit3']")
  
  // Test tuple creation
  let result2 = @array.zip_with(strings, numbers, (s, n) => (s, n))?
  inspect(result2, content="[('hello', 1), ('world', 2), ('moonbit', 3)]")
  
  // Test conditional logic
  let result3 = @array.zip_with(numbers, numbers, (a, b) => if a > b { a } else { b })?
  inspect(result3, content="[1, 2, 3]")
}

///|
test "array comprehensive operations" {
  let mut arr = [1, 2, 3, 4, 5]
  
  // Test push and pop operations
  arr.push(6)
  inspect(arr, content="[1, 2, 3, 4, 5, 6]")
  
  let popped = arr.pop()?
  inspect(popped, content="6")
  inspect(arr, content="[1, 2, 3, 4, 5]")
  
  // Test insert at different positions
  arr.insert(0, 0)
  inspect(arr, content="[0, 1, 2, 3, 4, 5]")
  
  arr.insert(3, 99)
  inspect(arr, content="[0, 1, 2, 99, 3, 4, 5]")
  
  arr.insert(arr.length(), 100)
  inspect(arr, content="[0, 1, 2, 99, 3, 4, 5, 100]")
}

///|
test "array remove operations" {
  let mut arr = [10, 20, 30, 40, 50]
  
  // Remove from middle
  let removed1 = arr.remove(2)?
  inspect(removed1, content="30")
  inspect(arr, content="[10, 20, 40, 50]")
  
  // Remove from beginning
  let removed2 = arr.remove(0)?
  inspect(removed2, content="10")
  inspect(arr, content="[20, 40, 50]")
  
  // Remove from end
  let removed3 = arr.remove(arr.length() - 1)?
  inspect(removed3, content="50")
  inspect(arr, content="[20, 40]")
}

///|
test "array mapping and chaining" {
  let numbers = [1, 2, 3, 4, 5]
  
  // Chain multiple operations
  let result = numbers
    .map(x => x * 2)
    .filter(x => x > 4)
    .map(x => x + 1)
  
  inspect(result, content="[7, 9, 11]")
  
  // Test with string operations
  let words = ["hello", "world", "moonbit"]
  let transformed = words
    .map(s => s.to_upper())
    .filter(s => s.length() > 4)
    .map(s => s + "!")
  
  inspect(transformed, content="['HELLO!', 'WORLD!', 'MOONBIT!']")
}

///|
test "array reduction operations" {
  let numbers = [1, 2, 3, 4, 5]
  
  // Test sum
  let sum = numbers.fold((acc, x) => acc + x, init=0)
  inspect(sum, content="15")
  
  // Test product
  let product = numbers.fold((acc, x) => acc * x, init=1)
  inspect(product, content="120")
  
  // Test finding maximum
  let max = numbers.fold((acc, x) => if x > acc { x } else { acc }, init=numbers[0])
  inspect(max, content="5")
  
  // Test string concatenation
  let words = ["hello", " ", "world", "!"]
  let sentence = words.fold((acc, word) => acc + word, init="")
  inspect(sentence, content="hello world!")
}

///|
test "array edge cases" {
  // Test with empty array
  let empty : Array[Int] = []
  inspect(empty.length(), content="0")
  
  // Test with single element
  let single = [42]
  inspect(single.length(), content="1")
  inspect(single[0], content="42")
  
  // Test array of arrays
  let nested = [[1, 2], [3, 4], [5, 6]]
  inspect(nested.length(), content="3")
  inspect(nested[1], content="[3, 4]")
  
  // Test array with mixed types (using tuples)
  let mixed = [(1, "one"), (2, "two"), (3, "three")]
  inspect(mixed.length(), content="3")
  inspect(mixed[1], content="(2, 'two')")
}

///|
test "array sorting operations" {
  let numbers = [5, 2, 8, 1, 9, 3]
  let mut sorted = numbers
  
  sorted.sort()
  inspect(sorted, content="[1, 2, 3, 5, 8, 9]")
  
  // Test with strings
  let mut words = ["zebra", "apple", "moonbit", "banana"]
  words.sort()
  inspect(words, content="['apple', 'banana', 'moonbit', 'zebra']")
  
  // Test that original array is unchanged
  inspect(numbers, content="[5, 2, 8, 1, 9, 3]")
}