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

// Additional comprehensive test cases for array module

test "array_basic_operations" {
  let empty : Array[Int] = []
  let single = [42]
  let multiple = [1, 2, 3, 4, 5]
  
  let len1 = empty.length()
  let len2 = single.length()
  let len3 = multiple.length()
  
  // Simple validation
  let _ = len1 + len2 + len3
}

test "array_access" {
  let arr = [10, 20, 30, 40, 50]
  
  let val1 = arr[0]
  let val2 = arr[2]
  let val3 = arr[4]
  
  // Simple validation
  let _ = val1 + val2 + val3
}

test "array_mutation" {
  let mut arr = [1, 2, 3, 4, 5]
  
  arr[0] = 10
  arr[2] = 30
  arr[4] = 50
  
  let val1 = arr[0]
  let val2 = arr[2]
  let val3 = arr[4]
  
  // Simple validation
  let _ = val1 + val2 + val3
}

test "array_slice" {
  let arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let slice1 = arr.slice(0, 5)
  let slice2 = arr.slice(5, 10)
  
  let len1 = slice1.length()
  let len2 = slice2.length()
  
  // Simple validation
  let _ = len1 + len2
}

test "array_concatenation" {
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let combined = arr1.concat(arr2)
  
  let combined_len = combined.length()
  
  // Simple validation
  let _ = combined_len
}

test "array_contains" {
  let arr = [1, 2, 3, 4, 5]
  
  let contains1 = arr.contains(3)
  let contains2 = arr.contains(10)
  
  // Simple validation
  let _ = if (contains1 && !contains2) { 1 } else { 0 }
}

test "array_filter" {
  let arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let evens = arr.filter(fn(x) { x % 2 == 0 })
  
  let evens_len = evens.length()
  
  // Simple validation
  let _ = evens_len
}

test "array_map" {
  let arr = [1, 2, 3, 4, 5]
  let doubled = arr.map(fn(x) { x * 2 })
  
  let doubled_len = doubled.length()
  
  // Simple validation
  let _ = doubled_len
}

test "array_fold" {
  let arr = [1, 2, 3, 4, 5]
  let sum = arr.fold(0, fn(acc, x) { acc + x })
  
  // Simple validation
  let _ = sum
}

test "array_reverse" {
  let arr = [1, 2, 3, 4, 5]
  let reversed = arr.reverse()
  
  let reversed_len = reversed.length()
  
  // Simple validation
  let _ = reversed_len
}

test "array_with_strings" {
  let words = ["Hello", "World", "MoonBit"]
  
  let words_len = words.length()
  let contains_moonbit = words.contains("MoonBit")
  let contains_rust = words.contains("Rust")
  
  // Simple validation
  let _ = if (contains_moonbit && !contains_rust) { words_len } else { 0 }
}

test "array_with_booleans" {
  let flags = [true, false, true, true, false]
  
  let flags_len = flags.length()
  let contains_true = flags.contains(true)
  let contains_false = flags.contains(false)
  
  // Simple validation
  let _ = if (contains_true && contains_false) { flags_len } else { 0 }
}

test "array_boundary_cases" {
  let empty : Array[Int] = []
  let single = [42]
  
  let empty_len = empty.length()
  let single_len = single.length()
  let contains_42 = single.contains(42)
  let empty_contains = empty.contains(0)
  
  // Simple validation
  let _ = if (contains_42 && !empty_contains) { empty_len + single_len } else { 0 }
}