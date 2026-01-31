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

using @builtin { inspect }

///|
test "array_basic_operations" {
  let empty = []
  let single = [42]
  let multiple = [1, 2, 3, 4, 5]
  
  inspect(empty.length(), content="0")
  inspect(single.length(), content="1")
  inspect(multiple.length(), content="5")
}

///|
test "array_element_access" {
  let arr = [10, 20, 30, 40, 50]
  
  inspect(arr[0], content="10")
  inspect(arr[2], content="30")
  inspect(arr[4], content="50")
}

///|
test "array_element_modification" {
  let mut arr = [1, 2, 3, 4, 5]
  arr[0] = 10
  arr[2] = 30
  
  inspect(arr[0], content="10")
  inspect(arr[2], content="30")
  inspect(arr[1], content="2")
}

///|
test "array_concatenation" {
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let combined = arr1.concat(arr2)
  
  inspect(combined.length(), content="6")
  inspect(combined[0], content="1")
  inspect(combined[5], content="6")
}

///|
test "array_contains_operations" {
  let arr = [1, 2, 3, 4, 5]
  
  inspect(arr.contains(3), content="true")
  inspect(arr.contains(6), content="false")
  inspect([].contains(1), content="false")
}

///|
test "array_index_operations" {
  let arr = [10, 20, 30, 40, 50]
  
  inspect(arr.index_of(30), content="2")
  inspect(arr.index_of(10), content="0")
  inspect(arr.index_of(50), content="4")
  inspect(arr.index_of(60), content="-1")
}

///|
test "array_filter_operations" {
  let arr = [1, 2, 3, 4, 5, 6]
  let evens = arr.filter(fn(x) { x % 2 == 0 })
  
  inspect(evens.length(), content="3")
  inspect(evens[0], content="2")
  inspect(evens[1], content="4")
  inspect(evens[2], content="6")
}

///|
test "array_map_operations" {
  let arr = [1, 2, 3, 4, 5]
  let doubled = arr.map(fn(x) { x * 2 })
  
  inspect(doubled.length(), content="5")
  inspect(doubled[0], content="2")
  inspect(doubled[4], content="10")
}

///|
test "array_reduce_operations" {
  let arr = [1, 2, 3, 4, 5]
  let sum = arr.reduce(0, fn(acc, x) { acc + x })
  
  inspect(sum, content="15")
  
  let product = arr.reduce(1, fn(acc, x) { acc * x })
  inspect(product, content="120")
}