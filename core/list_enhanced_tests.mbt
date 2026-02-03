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
  let empty : @list.List[Int] = @list.empty()
  let single = @list.cons(1, empty)
  let multiple = @list.from_array([1, 2, 3, 4, 5])
  
  assert_eq(empty.length(), 0)
  assert_eq(single.length(), 1)
  assert_eq(multiple.length(), 5)
  assert_eq(single.head(), Some(1))
  assert_eq(multiple.head(), Some(1))
}

///|
test "list cons and prepend" {
  let empty : @list.List[Int] = @list.empty()
  let list1 = @list.cons(1, empty)
  let list2 = list1.prepend(2)
  let list3 = @list.cons(3, list2)
  
  assert_eq(list1, @list.from_array([1]))
  assert_eq(list2, @list.from_array([2, 1]))
  assert_eq(list3, @list.from_array([3, 2, 1]))
}

///|
test "list map operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  let doubled = numbers.map(x => x * 2)
  let squared = numbers.map(x => x * x)
  
  assert_eq(doubled, @list.from_array([2, 4, 6, 8, 10]))
  assert_eq(squared, @list.from_array([1, 4, 9, 16, 25]))
}

///|
test "list filter operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  let evens = numbers.filter(x => x % 2 == 0)
  let odds = numbers.filter(x => x % 2 != 0)
  let greater_than_five = numbers.filter(x => x > 5)
  
  assert_eq(evens, @list.from_array([2, 4, 6, 8, 10]))
  assert_eq(odds, @list.from_array([1, 3, 5, 7, 9]))
  assert_eq(greater_than_five, @list.from_array([6, 7, 8, 9, 10]))
}

///|
test "list fold operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  let product = numbers.fold(init=1, (acc, x) => acc * x)
  
  assert_eq(sum, 15)
  assert_eq(product, 120)
}

///|
test "list concat and reverse" {
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  let combined = list1.concat(list2)
  let reversed = combined.rev()
  
  assert_eq(combined, @list.from_array([1, 2, 3, 4, 5, 6]))
  assert_eq(reversed, @list.from_array([6, 5, 4, 3, 2, 1]))
}

///|
test "list searching operations" {
  let numbers = @list.from_array([10, 20, 30, 40, 50])
  let contains_thirty = numbers.contains(30)
  let contains_sixty = numbers.contains(60)
  let all_positive = numbers.all(x => x > 0)
  let any_greater_than_25 = numbers.any(x => x > 25)
  
  assert_eq(contains_thirty, true)
  assert_eq(contains_sixty, false)
  assert_eq(all_positive, true)
  assert_eq(any_greater_than_25, true)
}

///|
test "list nth and last operations" {
  let numbers = @list.from_array([10, 20, 30, 40, 50])
  let first = numbers.nth(0)
  let third = numbers.nth(2)
  let last = numbers.last()
  let out_of_bounds = numbers.nth(10)
  
  assert_eq(first, Some(10))
  assert_eq(third, Some(30))
  assert_eq(last, Some(50))
  assert_eq(out_of_bounds, None)
}