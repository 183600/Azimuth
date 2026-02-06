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
  let empty = @list.empty()
  let single = @list.cons(1, empty)
  let multi = @list.from_array([1, 2, 3, 4, 5])
  
  // Test length
  assert_eq(empty.length(), 0)
  assert_eq(single.length(), 1)
  assert_eq(multi.length(), 5)
  
  // Test is_empty
  assert_true(empty.is_empty())
  assert_false(single.is_empty())
  assert_false(multi.is_empty())
}

///|
test "list prepend operations" {
  let empty = @list.empty()
  let list1 = empty.prepend(1)
  let list2 = list1.prepend(2)
  let list3 = list2.prepend(3)
  
  assert_eq(list1, @list.from_array([1]))
  assert_eq(list2, @list.from_array([2, 1]))
  assert_eq(list3, @list.from_array([3, 2, 1]))
}

///|
test "list head and tail operations" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let empty = @list.empty()
  
  // Test head
  assert_eq(list.head(), Some(1))
  assert_eq(empty.head(), None)
  
  // Test last
  assert_eq(list.last(), Some(5))
  assert_eq(empty.last(), None)
  
  // Test unsafe_head and unsafe_last
  assert_eq(list.unsafe_head(), 1)
  assert_eq(list.unsafe_last(), 5)
}

///|
test "list nth element access" {
  let list = @list.from_array([10, 20, 30, 40, 50])
  
  // Test nth
  assert_eq(list.nth(0), Some(10))
  assert_eq(list.nth(2), Some(30))
  assert_eq(list.nth(4), Some(50))
  assert_eq(list.nth(5), None)
  assert_eq(list.nth(-1), None)
  
  // Test unsafe_nth
  assert_eq(list.unsafe_nth(0), 10)
  assert_eq(list.unsafe_nth(2), 30)
  assert_eq(list.unsafe_nth(4), 50)
}

///|
test "list concatenation operations" {
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  let empty = @list.empty()
  
  // Test concat
  assert_eq(list1.concat(list2), @list.from_array([1, 2, 3, 4, 5, 6]))
  assert_eq(list1.concat(empty), @list.from_array([1, 2, 3]))
  assert_eq(empty.concat(list2), @list.from_array([4, 5, 6]))
  assert_eq(empty.concat(empty), @list.from_array([]))
  
  // Test + operator
  assert_eq(list1 + list2, @list.from_array([1, 2, 3, 4, 5, 6]))
}

///|
test "list reverse operations" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let empty = @list.empty()
  
  // Test rev
  assert_eq(list.rev(), @list.from_array([5, 4, 3, 2, 1]))
  assert_eq(empty.rev(), @list.from_array([]))
  
  // Test rev_concat
  assert_eq(list.rev_concat(empty), @list.from_array([5, 4, 3, 2, 1]))
  assert_eq(empty.rev_concat(list), @list.from_array([1, 2, 3, 4, 5]))
}

///|
test "list fold operations" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let empty = @list.empty()
  
  // Test fold
  assert_eq(list.fold(0, (acc, x) => acc + x), 15)
  assert_eq(list.fold(1, (acc, x) => acc * x), 120)
  assert_eq(empty.fold(0, (acc, x) => acc + x), 0)
  
  // Test foldi
  assert_eq(list.foldi(0, (i, acc, x) => acc + i * x), 40) // 0*1 + 1*2 + 2*3 + 3*4 + 4*5 = 40
  assert_eq(empty.foldi(0, (i, acc, x) => acc + i * x), 0)
}

///|
test "list map operations" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let empty = @list.empty()
  
  // Test map
  assert_eq(list.map(x => x * 2), @list.from_array([2, 4, 6, 8, 10]))
  assert_eq(list.map(x => x + 1), @list.from_array([2, 3, 4, 5, 6]))
  assert_eq(empty.map(x => x * 2), @list.from_array([]))
  
  // Test mapi
  assert_eq(list.mapi((i, x) => i + x), @list.from_array([1, 3, 5, 7, 9]))
  assert_eq(empty.mapi((i, x) => i + x), @list.from_array([]))
  
  // Test rev_map
  assert_eq(list.rev_map(x => x * 2), @list.from_array([10, 8, 6, 4, 2]))
  assert_eq(empty.rev_map(x => x * 2), @list.from_array([]))
}

///|
test "list filter operations" {
  let list = @list.from_array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  let empty = @list.empty()
  
  // Test filter
  assert_eq(list.filter(x => x % 2 == 0), @list.from_array([2, 4, 6, 8, 10]))
  assert_eq(list.filter(x => x > 5), @list.from_array([6, 7, 8, 9, 10]))
  assert_eq(list.filter(x => x < 0), @list.from_array([]))
  assert_eq(empty.filter(x => x > 0), @list.from_array([]))
  
  // Test all
  assert_true(list.all(x => x > 0))
  assert_true(list.all(x => x <= 10))
  assert_false(list.all(x => x % 2 == 0))
  assert_true(empty.all(x => false)) // vacuously true
  
  // Test any
  assert_true(list.any(x => x % 2 == 0))
  assert_true(list.any(x => x > 9))
  assert_false(list.any(x => x > 10))
  assert_false(empty.any(x => true))
}

///|
test "list flat_map operations" {
  let list = @list.from_array([1, 2, 3])
  let empty = @list.empty()
  
  // Test flat_map
  assert_eq(list.flat_map(x => @list.from_array([x, x * 2])), @list.from_array([1, 2, 2, 4, 3, 6]))
  assert_eq(list.flat_map(x => @list.from_array([])), @list.from_array([]))
  assert_eq(empty.flat_map(x => @list.from_array([x, x * 2])), @list.from_array([]))
}

///|
test "list filter_map operations" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let empty = @list.empty()
  
  // Test filter_map
  assert_eq(list.filter_map(x => if x % 2 == 0 { Some(x * 2) } else { None }), @list.from_array([4, 8]))
  assert_eq(list.filter_map(x => if x > 10 { Some(x) } else { None }), @list.from_array([]))
  assert_eq(empty.filter_map(x => Some(x * 2)), @list.from_array([]))
}

///|
test "list zip and unzip operations" {
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  let longer = @list.from_array([1, 2, 3, 4, 5])
  let empty = @list.empty()
  
  // Test zip
  assert_eq(@list.zip(list1, list2), @list.from_array([(1, 4), (2, 5), (3, 6)]))
  assert_eq(@list.zip(list1, longer), @list.from_array([(1, 1), (2, 2), (3, 3)]))
  assert_eq(@list.zip(list1, empty), @list.from_array([]))
  
  // Test unzip
  let (a, b) = @list.from_array([(1, 4), (2, 5), (3, 6)]).unzip()
  assert_eq(a, @list.from_array([1, 2, 3]))
  assert_eq(b, @list.from_array([4, 5, 6]))
}

///|
test "list contains operations" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let empty = @list.empty()
  
  // Test contains
  assert_true(list.contains(1))
  assert_true(list.contains(3))
  assert_true(list.contains(5))
  assert_false(list.contains(0))
  assert_false(list.contains(6))
  assert_false(empty.contains(1))
}

///|
test "list take and drop operations" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let empty = @list.empty()
  
  // Test take
  assert_eq(list.take(0), @list.from_array([]))
  assert_eq(list.take(3), @list.from_array([1, 2, 3]))
  assert_eq(list.take(5), @list.from_array([1, 2, 3, 4, 5]))
  assert_eq(list.take(10), @list.from_array([1, 2, 3, 4, 5]))
  assert_eq(empty.take(3), @list.from_array([]))
}