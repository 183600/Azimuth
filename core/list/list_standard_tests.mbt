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
  let empty_list : @list.List[Int] = @list.new()
  inspect(empty_list.is_empty(), content="true")
  inspect(empty_list.length(), content="0")
  
  let list_with_one = @list.cons(1, empty_list)
  inspect(list_with_one.is_empty(), content="false")
  inspect(list_with_one.length(), content="1")
  inspect(list_with_one.head(), content="Some(1)")
}

///|
test "list from_array and to_array" {
  let arr = [1, 2, 3, 4, 5]
  let list = @list.from_array(arr)
  let result_arr = list.to_array()
  inspect(arr, content="[1, 2, 3, 4, 5]")
  inspect(result_arr, content="[1, 2, 3, 4, 5]")
}

///|
test "list prepend" {
  let list = @list.from_array([2, 3, 4])
  let new_list = list.prepend(1)
  inspect(new_list, content="@list.from_array([1, 2, 3, 4])")
}

///|
test "list map" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let doubled = list.map(x => x * 2)
  inspect(doubled, content="@list.from_array([2, 4, 6, 8, 10])")
}

///|
test "list filter" {
  let list = @list.from_array([1, 2, 3, 4, 5, 6])
  let evens = list.filter(x => x % 2 == 0)
  inspect(evens, content="@list.from_array([2, 4, 6])")
}

///|
test "list fold" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let sum = list.fold(init=0, (acc, x) => acc + x)
  inspect(sum, content="15")
  
  let product = list.fold(init=1, (acc, x) => acc * x)
  inspect(product, content="120")
}

///|
test "list head and tail" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  inspect(list.head(), content="Some(1)")
  inspect(list.unsafe_tail(), content="@list.from_array([2, 3, 4, 5])")
  
  let empty_list : @list.List[Int] = @list.empty()
  inspect(empty_list.head(), content="None")
}

///|
test "list concat" {
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  let combined = list1.concat(list2)
  inspect(combined, content="@list.from_array([1, 2, 3, 4, 5, 6])")
}

///|
test "list reverse" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let reversed = list.rev()
  inspect(reversed, content="@list.from_array([5, 4, 3, 2, 1])")
}

///|
test "list contains" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  inspect(list.contains(3), content="true")
  inspect(list.contains(6), content="false")
}