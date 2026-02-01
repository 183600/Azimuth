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
  let empty_array : Array[Int] = []
  let numbers = [1, 2, 3, 4, 5]
  
  inspect(empty_array.length(), content="0")
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
}

///|
test "array creation and initialization" {
  let filled_array = Array::make(3, 42)
  inspect(filled_array.length(), content="3")
  inspect(filled_array[0], content="42")
  inspect(filled_array[1], content="42")
  inspect(filled_array[2], content="42")
  
  let indexed_array = Array::makei(4, i => i * 2)
  inspect(indexed_array.length(), content="4")
  inspect(indexed_array[0], content="0")
  inspect(indexed_array[1], content="2")
  inspect(indexed_array[2], content="4")
  inspect(indexed_array[3], content="6")
}

///|
test "array push and pop operations" {
  let mutable_array = [1, 2, 3]
  
  mutable_array.push(4)
  inspect(mutable_array.length(), content="4")
  inspect(mutable_array[3], content="4")
  
  let popped_value = mutable_array.pop()
  inspect(popped_value, content="Some(4)")
  inspect(mutable_array.length(), content="3")
  
  let empty_pop = ([] : Array[Int]).pop()
  inspect(empty_pop, content="None")
}

///|
test "array zip_with function" {
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let add = (a, b) => a + b
  
  let zipped = @array.zip_with(arr1, arr2, add)
  inspect(zipped.length(), content="3")
  inspect(zipped[0], content="5")
  inspect(zipped[1], content="7")
  inspect(zipped[2], content="9")
  
  // Test with different length arrays
  let short_arr1 = [1, 2]
  let long_arr2 = [10, 20, 30, 40]
  let zipped_unequal = @array.zip_with(short_arr1, long_arr2, add)
  inspect(zipped_unequal.length(), content="2")
  inspect(zipped_unequal[0], content="11")
  inspect(zipped_unequal[1], content="22")
}

///|
test "array iteration and mapping" {
  let numbers = [1, 2, 3, 4, 5]
  
  // Test map operation
  let doubled = numbers.map(x => x * 2)
  inspect(doubled.length(), content="5")
  inspect(doubled[0], content="2")
  inspect(doubled[4], content="10")
  
  // Test filter operation
  let evens = numbers.filter(x => x % 2 = 0)
  inspect(evens.length(), content="2")
  inspect(evens[0], content="2")
  inspect(evens[1], content="4")
  
  // Test fold operation
  let sum = numbers.fold(0, (acc, x) => acc + x)
  inspect(sum, content="15")
}

///|
test "array slice operations" {
  let numbers = [10, 20, 30, 40, 50]
  
  let slice1 = numbers.slice(1, 3)
  inspect(slice1.length(), content="2")
  inspect(slice1[0], content="20")
  inspect(slice1[1], content="30")
  
  let slice2 = numbers.slice(0, 5)
  inspect(slice2.length(), content="5")
  inspect(slice2[0], content="10")
  inspect(slice2[4], content="50")
  
  let empty_slice = numbers.slice(2, 2)
  inspect(empty_slice.length(), content="0")
}