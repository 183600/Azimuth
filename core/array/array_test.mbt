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
test "zip_with" {
  let left = [1, 2, 3]
  let right = [10, 20]
  inspect(@array.zip_with(left, right, (a, b) => a + b), content="[11, 22]")
}

///|
test "array basic operations enhanced" {
  let empty = []
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["a", "b", "c"]
  inspect(empty.length(), content="0")
  inspect(numbers.length(), content="5")
  inspect(strings.length(), content="3")
  inspect(numbers[0], content="1")
  inspect(strings[2], content="c")
}

///|
test "array zip_with operation enhanced" {
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let sum = (a, b) => a + b
  let result = @array.zip_with(arr1, arr2, sum)?
  inspect(result, content="[5, 7, 9]")
}

///|
test "array creation and initialization" {
  let empty : Array[Int] = []
  let single = [42]
  let multiple = [1, 2, 3, 4, 5]
  
  inspect(empty.length(), content="0")
  inspect(single.length(), content="1")
  inspect(multiple.length(), content="5")
  inspect(single[0], content="42")
  inspect(multiple[2], content="3")
}

///|
test "array modification operations" {
  let mut arr = [1, 2, 3]
  arr.push(4)
  inspect(arr.length(), content="4")
  inspect(arr[3], content="4")
  
  let popped = arr.pop()?
  inspect(popped, content="4")
  inspect(arr.length(), content="3")
  
  arr.insert(1, 99)
  inspect(arr, content="[1, 99, 2, 3]")
  
  let removed = arr.remove(1)?
  inspect(removed, content="99")
  inspect(arr, content="[1, 2, 3]")
}

///|
test "array mapping operations" {
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="[2, 4, 6, 8, 10]")
  
  let strings = ["a", "b", "c"]
  let uppercased = strings.map(s => s.to_upper())
  inspect(uppercased, content="['A', 'B', 'C']")
}

///|
test "array filtering operations" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens, content="[2, 4, 6, 8, 10]")
  
  let odds = numbers.filter(x => x % 2 != 0)
  inspect(odds, content="[1, 3, 5, 7, 9]")
  
  let greater_than_five = numbers.filter(x => x > 5)
  inspect(greater_than_five, content="[6, 7, 8, 9, 10]")
}

///|
test "array reduction operations" {
  let numbers = [1, 2, 3, 4, 5]
  let sum = numbers.fold(0, (acc, x) => acc + x)
  inspect(sum, content="15")
  
  let product = numbers.fold(1, (acc, x) => acc * x)
  inspect(product, content="120")
  
  let strings = ["hello", "world", "moonbit"]
  let concatenated = strings.fold("", (acc, s) => acc + s + " ")
  inspect(concatenated.trim(), content="hello world moonbit")
}

///|
test "array searching operations" {
  let numbers = [10, 20, 30, 40, 50]
  let found = numbers.find(x => x > 25)
  inspect(found, content="Some(30)")
  
  let not_found = numbers.find(x => x > 100)
  inspect(not_found, content="None")
  
  let contains_thirty = numbers.contains(30)
  inspect(contains_thirty, content="true")
  
  let contains_sixty = numbers.contains(60)
  inspect(contains_sixty, content="false")
}

///|
test "array sorting operations" {
  let mut numbers = [5, 2, 8, 1, 9, 3]
  numbers.sort()
  inspect(numbers, content="[1, 2, 3, 5, 8, 9]")
  
  let mut strings = ["zebra", "apple", "cherry", "banana"]
  strings.sort()
  inspect(strings, content="['apple', 'banana', 'cherry', 'zebra']")
}

///|
test "array slice operations" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let first_three = numbers.slice(0, 3)
  inspect(first_three, content="[1, 2, 3]")
  
  let middle = numbers.slice(3, 7)
  inspect(middle, content="[4, 5, 6, 7]")
  
  let last_three = numbers.slice(7, 10)
  inspect(last_three, content="[8, 9, 10]")
}

///|
test "array concatenation" {
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let combined = arr1 + arr2
  inspect(combined, content="[1, 2, 3, 4, 5, 6]")
  
  let empty : Array[Int] = []
  let with_empty = empty + arr1
  inspect(with_empty, content="[1, 2, 3]")
}
