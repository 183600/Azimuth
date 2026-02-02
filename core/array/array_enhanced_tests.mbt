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
test "array partition functionality" {
  let numbers = [1, 2, 3, 4, 5, 6]
  
  let partition = fn(arr : Array[Int], predicate : Int -> Bool) -> (Array[Int], Array[Int]) {
    let evens = []
    let odds = []
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
  
  let (evens, odds) = partition(numbers, x => x % 2 == 0)
  inspect(evens, content="[2, 4, 6]")
  inspect(odds, content="[1, 3, 5]")
}

///|
test "array find functionality" {
  let numbers = [10, 20, 30, 40, 50]
  let strings = ["apple", "banana", "cherry"]
  
  let find = fn[T](arr : Array[T], predicate : T -> Bool) -> Option[T] {
    for i = 0; i < arr.length(); i = i + 1 {
      let value = arr[i]
      if predicate(value) {
        return Some(value)
      }
    }
    None
  }
  
  let found_even = find(numbers, x => x % 20 == 0)
  let found_large = find(numbers, x => x > 100)
  let found_cherry = find(strings, s => s == "cherry")
  let found_orange = find(strings, s => s == "orange")
  
  assert_eq(found_even, Some(20))
  assert_eq(found_large, None)
  assert_eq(found_cherry, Some("cherry"))
  assert_eq(found_orange, None)
}

///|
test "array chunk functionality" {
  let numbers = [1, 2, 3, 4, 5, 6, 7]
  
  let chunk = fn(arr : Array[Int], size : Int) -> Array[Array[Int]] {
    let result = []
    for i = 0; i < arr.length(); i = i + size {
      let end = if i + size > arr.length() { arr.length() } else { i + size }
      let chunk = []
      for j = i; j < end; j = j + 1 {
        chunk.push(arr[j])
      }
      result.push(chunk)
    }
    result
  }
  
  let chunks_of_2 = chunk(numbers, 2)
  let chunks_of_3 = chunk(numbers, 3)
  
  inspect(chunks_of_2, content="[[1, 2], [3, 4], [5, 6], [7]]")
  inspect(chunks_of_3, content="[[1, 2, 3], [4, 5, 6], [7]]")
}