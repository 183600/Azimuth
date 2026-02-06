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
test "list basic operations" {
  let empty_list : @list.List[Int] = @list.empty()
  assert_eq(empty_list.length(), 0)
  assert_eq(empty_list.is_empty(), true)
  
  let list1 = @list.from_array([1, 2, 3, 4, 5])
  assert_eq(list1.length(), 5)
  assert_eq(list1.is_empty(), false)
  assert_eq(list1.head(), Some(1))
  assert_eq(list1.last(), Some(5))
}

///|
test "list map and filter operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled, @list.from_array([2, 4, 6, 8, 10]))
  
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(evens, @list.from_array([2, 4]))
  
  let sum = numbers.fold(0, (acc, x) => acc + x)
  assert_eq(sum, 15)
}

///|
test "list concatenation and reversal" {
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  
  let combined = list1.concat(list2)
  assert_eq(combined, @list.from_array([1, 2, 3, 4, 5, 6]))
  
  let reversed = combined.rev()
  assert_eq(reversed, @list.from_array([6, 5, 4, 3, 2, 1]))
}

///|
test "option type operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  assert_eq(some_value.is_some(), true)
  assert_eq(some_value.is_none(), false)
  assert_eq(none_value.is_some(), false)
  assert_eq(none_value.is_none(), true)
  
  let mapped = some_value.map(x => x * 2)
  assert_eq(mapped, Some(84))
  
  let default_value = none_value.unwrap_or(0)
  assert_eq(default_value, 0)
}

///|
test "boolean logical operations" {
  let t = true
  let f = false
  
  assert_eq(t && f, false)
  assert_eq(t || f, true)
  assert_eq(!t, false)
  assert_eq(!f, true)
  
  assert_eq(t == t, true)
  assert_eq(t == f, false)
  assert_eq(t != f, true)
}

///|
test "integer arithmetic operations" {
  let a = 15
  let b = 4
  
  assert_eq(a + b, 19)
  assert_eq(a - b, 11)
  assert_eq(a * b, 60)
  assert_eq(a / b, 3)
  assert_eq(a % b, 3)
  
  assert_eq(a < b, false)
  assert_eq(a > b, true)
  assert_eq(a <= b, false)
  assert_eq(a >= b, true)
}

///|
test "string operations" {
  let str1 = "Hello"
  let str2 = "World"
  
  let combined = str1 + " " + str2
  assert_eq(combined, "Hello World")
  
  assert_eq(str1.length(), 5)
  assert_eq(combined.length(), 11)
}

///|
test "list zip and unzip operations" {
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array(["a", "b", "c"])
  
  let zipped = @list.zip(list1, list2)
  assert_eq(zipped, @list.from_array([(1, "a"), (2, "b"), (3, "c")]))
  
  let (unzipped1, unzipped2) = zipped.unzip()
  assert_eq(unzipped1, @list.from_array([1, 2, 3]))
  assert_eq(unzipped2, @list.from_array(["a", "b", "c"]))
}

///|
test "list find and contains operations" {
  let numbers = @list.from_array([10, 20, 30, 40, 50])
  
  assert_eq(numbers.contains(30), true)
  assert_eq(numbers.contains(25), false)
  
  let found = numbers.find(x => x > 25)
  assert_eq(found, Some(30))
  
  let not_found = numbers.find(x => x > 100)
  assert_eq(not_found, None)
}

///|
test "list take and drop operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  
  let taken = numbers.take(5)
  assert_eq(taken, @list.from_array([1, 2, 3, 4, 5]))
  
  let dropped = numbers.drop(5)
  assert_eq(dropped, @list.from_array([6, 7, 8, 9, 10]))
  
  let all_taken = numbers.take(20)
  assert_eq(all_taken, numbers)
  
  let all_dropped = numbers.drop(20)
  assert_eq(all_dropped, @list.empty())
}