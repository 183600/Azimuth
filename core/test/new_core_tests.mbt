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

// 导入必要的函数
using @builtin { inspect }

// 标准 MoonBit 核心测试用例
// 测试核心模块的基本功能

test "int_max_value_test" {
  inspect(@int.max_value, content="2147483647")
}

test "int_min_value_test" {
  inspect(@int.min_value, content="-2147483648")
}

test "int_abs_test" {
  inspect(@int.abs(5), content="5")
  inspect(@int.abs(-5), content="5")
  inspect(@int.abs(0), content="0")
}

test "list_cons_test" {
  let empty_list = @list.empty()
  let list1 = @list.cons(1, empty_list)
  let list2 = @list.cons(2, list1)
  inspect(list2.length(), content="2")
  inspect(list2.head(), content="Some(2)")
}

test "list_map_test" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let doubled = list.map(x => x * 2)
  inspect(doubled.to_array(), content="[2, 4, 6, 8, 10]")
}

test "list_filter_test" {
  let list = @list.from_array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  let evens = list.filter(x => x % 2 == 0)
  inspect(evens.to_array(), content="[2, 4, 6, 8, 10]")
}

test "list_fold_test" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  let sum = list.fold(init=0, (acc, x) => acc + x)
  inspect(sum, content="15")
  
  let product = list.fold(init=1, (acc, x) => acc * x)
  inspect(product, content="120")
}

test "list_concat_test" {
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  let combined = list1.concat(list2)
  inspect(combined.to_array(), content="[1, 2, 3, 4, 5, 6]")
  inspect(combined.length(), content="6")
}

test "string_default_test" {
  let default_string = @string.default()
  inspect(default_string, content="")
}

test "list_contains_test" {
  let list = @list.from_array([1, 2, 3, 4, 5])
  inspect(list.contains(3), content="true")
  inspect(list.contains(6), content="false")
}

test "list_min_max_test" {
  let list = @list.from_array([3, 1, 4, 1, 5, 9, 2, 6])
  inspect(list.minimum(), content="Some(1)")
  inspect(list.maximum(), content="Some(9)")
  
  let empty_list = @list.empty()
  inspect(empty_list.minimum(), content="None")
  inspect(empty_list.maximum(), content="None")
}