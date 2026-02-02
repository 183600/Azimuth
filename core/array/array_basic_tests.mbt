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
test "array utility functions" {
  // Test partition function
  let partition = fn(arr : Array[Int], predicate : (Int) -> Bool) -> (Array[Int], Array[Int]) {
    let evens = Array::empty()
    let odds = Array::empty()
    for i = 0; i < arr.length(); i = i + 1 {
      let value = arr[i]
      if predicate(value) {
        evens.push(value)
      } else {
        odds.push(value)
      }
    }
    (evens, odds)
  }
  
  // Test find function
  let find = fn(arr : Array[Int], predicate : (Int) -> Bool) -> Int? {
    for i = 0; i < arr.length(); i = i + 1 {
      let value = arr[i]
      if predicate(value) {
        return Some(value)
      }
    }
    None
  }
  
  // Test chunk function
  let chunk = fn(arr : Array[Int], size : Int) -> Array[Array[Int]] {
    let result = Array::empty()
    for i = 0; i < arr.length(); i = i + size {
      let end = if i + size > arr.length() { arr.length() } else { i + size }
      let chunk = Array::empty()
      for j = i; j < end; j = j + 1 {
        chunk.push(arr[j])
      }
      result.push(chunk)
    }
    result
  }
  
  // Test the functions with various inputs
  let numbers = [1, 2, 3, 4, 5, 6, 7]
  
  // Function calls to verify they work
  let _ = partition(numbers, x => x % 2 == 0)
  let _ = find(numbers, x => x % 20 == 0)
  let _ = find(numbers, x => x > 100)
  let _ = find(numbers, x => x == 5)
  
  let _ = chunk(numbers, 2)
  let _ = chunk(numbers, 3)
}