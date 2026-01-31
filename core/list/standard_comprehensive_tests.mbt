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
test "list_basic_construction" {
  let empty = @list.empty()
  let single = @list.singleton(42)
  let from_array = @list.from_array([1, 2, 3])
  
  inspect(empty.length(), content="0")
  inspect(single.length(), content="1")
  inspect(from_array.length(), content="3")
}

///|
test "list_cons_operations" {
  let empty = @list.empty()
  let list1 = @list.cons(1, empty)
  let list2 = @list.cons(2, list1)
  let list3 = @list.cons(3, list2)
  
  inspect(list1.length(), content="1")
  inspect(list2.length(), content="2")
  inspect(list3.length(), content="3")
}

///|
test "list_head_tail_operations" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  
  inspect(list.head(), content="1")
  inspect(list.tail().length(), content="4")
  inspect(list.tail().head(), content="2")
}

///|
test "list_length_operations" {
  inspect(@list.empty().length(), content="0")
  inspect(@list.singleton(1).length(), content="1")
  inspect(@list.from_array([1, 2, 3, 4, 5]).length(), content="5")
}

///|
test "list_append_operations" {
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  let combined = list1.append(list2)
  
  inspect(combined.length(), content="6")
  inspect(combined.head(), content="1")
}

///|
test "list_reverse_operations" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let reversed = list.reverse()
  
  inspect(reversed.length(), content="5")
  inspect(reversed.head(), content="5")
}

///|
test "list_map_operations" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let doubled = list.map(fn(x) { x * 2 })
  
  inspect(doubled.length(), content="5")
  inspect(doubled.head(), content="2")
}

///|
test "list_filter_operations" {
  let list = @list.from_array([1, 2, 3, 4, 5, 6])
  let evens = list.filter(fn(x) { x % 2 == 0 })
  
  inspect(evens.length(), content="3")
  inspect(evens.head(), content="2")
}

///|
test "list_fold_operations" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let sum = list.fold(0, fn(acc, x) { acc + x })
  
  inspect(sum, content="15")
  
  let product = list.fold(1, fn(acc, x) { acc * x })
  inspect(product, content="120")
}

///|
test "list_contains_operations" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  
  inspect(list.contains(3), content="true")
  inspect(list.contains(6), content="false")
  inspect(@list.empty().contains(1), content="false")
}