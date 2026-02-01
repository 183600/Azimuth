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
test "int basic arithmetic operations" {
  inspect(1 + 2, content="3")
  inspect(10 - 3, content="7")
  inspect(4 * 5, content="20")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
}

///|
test "int comparison operations" {
  inspect(10 > 5, content="true")
  inspect(3 < 8, content="true")
  inspect(7 >= 7, content="true")
  inspect(4 <= 9, content="true")
  inspect(6 == 6, content="true")
  inspect(2 != 5, content="true")
}

///|
test "list basic operations" {
  let empty_list : @list.List[Int] = @list.empty()
  inspect(empty_list.length(), content="0")
  inspect(empty_list.is_empty(), content="true")
  
  let list1 = @list.from_array([1, 2, 3])
  inspect(list1.length(), content="3")
  inspect(list1.head(), content="Some(1)")
  inspect(list1.last(), content="Some(3)")
}

///|
test "list functional operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="@list.from_array([2, 4, 6, 8, 10])")
  
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens, content="@list.from_array([2, 4])")
  
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  inspect(sum, content="15")
}

///|
test "bool logical operations" {
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(!true, content="false")
}

///|
test "bool bitwise operations" {
  inspect(true & false, content="false")
  inspect(true | false, content="true")
  inspect(true ^ false, content="true")
}

///|
test "array operations" {
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let summed = @array.zip_with(arr1, arr2, (a, b) => a + b)
  inspect(summed, content="[5, 7, 9]")
  
  let multiplied = @array.zip_with(arr1, arr2, (a, b) => a * b)
  inspect(multiplied, content="[4, 10, 18]")
}

///|
test "string operations" {
  let empty_str = ""
  inspect(empty_str.length(), content="0")
  
  let hello = "Hello"
  let world = "World"
  let greeting = hello + ", " + world + "!"
  inspect(greeting, content="\"Hello, World!\"")
}

///|
test "type conversion operations" {
  inspect(42.to_string(), content="\"42\"")
  inspect((-17).to_string(), content="\"-17\"")
  inspect(true.to_string(), content="\"true\"")
  inspect(false.to_string(), content="\"false\"")
  inspect(true.to_int(), content="1")
  inspect(false.to_int(), content="0")
}

///|
test "list advanced operations" {
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  
  let combined = list1 + list2
  inspect(combined, content="@list.from_array([1, 2, 3, 4, 5, 6])")
  
  let reversed = combined.rev()
  inspect(reversed, content="@list.from_array([6, 5, 4, 3, 2, 1])")
  
  inspect(combined.contains(4), content="true")
  inspect(combined.contains(7), content="false")
}