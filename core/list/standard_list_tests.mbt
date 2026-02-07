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
test "list creation and basic operations" {
  let empty_list : @list.List[Int] = @list.empty()
  assert_eq(empty_list.length(), 0)
  
  let single_element = @list.cons(1, @list.empty())
  assert_eq(single_element.length(), 1)
  
  let list_from_array = @list.from_array([1, 2, 3])
  assert_eq(list_from_array.length(), 3)
}

///|
test "list head and tail" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  assert_eq(list.head(), Some(1))
  assert_eq(list.last(), Some(5))
  
  let empty_list : @list.List[Int] = @list.empty()
  assert_eq(empty_list.head(), None)
  assert_eq(empty_list.last(), None)
}

///|
test "list map and filter" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  
  let doubled = list.map(x => x * 2)
  assert_eq(doubled, @list.from_array([2, 4, 6, 8, 10]))
  
  let evens = list.filter(x => x % 2 == 0)
  assert_eq(evens, @list.from_array([2, 4]))
}

///|
test "list fold operations" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  
  let sum = list.fold(init=0, (acc, x) => acc + x)
  assert_eq(sum, 15)
  
  let product = list.fold(init=1, (acc, x) => acc * x)
  assert_eq(product, 120)
}

///|
test "list concatenation" {
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  
  let combined = list1.concat(list2)
  assert_eq(combined, @list.from_array([1, 2, 3, 4, 5, 6]))
}

///|
test "list reverse" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let reversed = list.rev()
  assert_eq(reversed, @list.from_array([5, 4, 3, 2, 1]))
}

///|
test "list contains" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  assert_eq(list.contains(3), true)
  assert_eq(list.contains(6), false)
}

///|
test "list take and drop" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  
  let first_three = list.take(3)
  assert_eq(first_three, @list.from_array([1, 2, 3]))
  
  let all = list.take(10)
  assert_eq(all, @list.from_array([1, 2, 3, 4, 5]))
}