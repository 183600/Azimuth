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

// 高质量核心模块综合测试用例
// 测试覆盖 int、list、string 等核心模块的关键功能

test "int_max_min_values" {
  // 测试 Int 模块的最大值和最小值
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  // 测试边界值的绝对值
  inspect(Int::abs(@int.max_value), content="2147483647")
  inspect(Int::abs(@int.min_value), content="2147483647")
  inspect(Int::abs(0), content="0")
  inspect(Int::abs(-42), content="42")
}

test "list_basic_operations" {
  // 测试 List 模块的基本操作
  let empty_list : @list.List[Int] = @list.empty()
  inspect(empty_list.length(), content="0")
  inspect(empty_list.is_empty(), content="true")
  
  let list1 = @list.from_array([1, 2, 3, 4, 5])
  inspect(list1.length(), content="5")
  inspect(list1.is_empty(), content="false")
  inspect(list1.head(), content="Some(1)")
  inspect(list1.last(), content="Some(5)")
  inspect(list1.nth(2), content="Some(3)")
  inspect(list1.nth(10), content="None")
}

test "list_functional_operations" {
  // 测试 List 的函数式操作
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // map 操作
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="@list.from_array([2, 4, 6, 8, 10])")
  
  // filter 操作
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens, content="@list.from_array([2, 4])")
  
  // fold 操作
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  inspect(sum, content="15")
  
  // any/all 操作
  let has_greater_than_3 = numbers.any(x => x > 3)
  inspect(has_greater_than_3, content="true")
  let all_positive = numbers.all(x => x > 0)
  inspect(all_positive, content="true")
  let all_greater_than_3 = numbers.all(x => x > 3)
  inspect(all_greater_than_3, content="false")
}

test "list_advanced_operations" {
  // 测试 List 的高级操作
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  
  // concat 操作
  let combined = list1.concat(list2)
  inspect(combined, content="@list.from_array([1, 2, 3, 4, 5, 6])")
  
  // reverse 操作
  let reversed = combined.rev()
  inspect(reversed, content="@list.from_array([6, 5, 4, 3, 2, 1])")
  
  // zip 操作
  let zipped = @list.zip(list1, list2)
  inspect(zipped, content="@list.from_array([(1, 4), (2, 5), (3, 6)])")
  
  // take 操作
  let taken = combined.take(4)
  inspect(taken, content="@list.from_array([1, 2, 3, 4])")
}

test "list_search_operations" {
  // 测试 List 的搜索操作
  let fruits = @list.from_array(["apple", "banana", "cherry", "date"])
  
  // contains 操作
  let has_banana = fruits.contains("banana")
  inspect(has_banana, content="true")
  let has_grape = fruits.contains("grape")
  inspect(has_grape, content="false")
  
  // maximum/minimum 操作
  let numbers = @list.from_array([3, 1, 4, 1, 5, 9, 2])
  inspect(numbers.maximum(), content="Some(9)")
  inspect(numbers.minimum(), content="Some(1)")
  
  let empty_numbers : @list.List[Int] = @list.empty()
  inspect(empty_numbers.maximum(), content="None")
  inspect(empty_numbers.minimum(), content="None")
}

test "string_basic_operations" {
  // 测试 String 模块的基本操作
  let empty_string = @string.default()
  inspect(empty_string, content="")
  
  // 测试字符串长度
  let test_string = "Hello, MoonBit!"
  inspect(test_string.length(), content="14")
}

test "list_edge_cases" {
  // 测试 List 的边界情况
  let single_element = @list.from_array([42])
  inspect(single_element.head(), content="Some(42)")
  inspect(single_element.last(), content="Some(42)")
  inspect(single_element.unsafe_tail(), content="@list.empty()")
  
  // 测试重复元素
  let repeated = @list.repeat(3, "x")
  inspect(repeated, content="@list.from_array([\"x\", \"x\", \"x\"])"
  
  // 测试 intersperse
  let interspersed = @list.from_array([1, 2, 3]).intersperse(0)
  inspect(interspersed, content="@list.from_array([1, 0, 2, 0, 3])")
}

test "list_flat_map_operations" {
  // 测试 List 的 flat_map 操作
  let numbers = @list.from_array([1, 2, 3])
  
  // flat_map 展开操作
  let expanded = numbers.flat_map(x => @list.from_array([x, x * 10]))
  inspect(expanded, content="@list.from_array([1, 10, 2, 20, 3, 30])")
  
  // filter_map 操作
  let filter_mapped = numbers.filter_map(x => if x % 2 == 0 { Some(x * x) } else { None })
  inspect(filter_mapped, content="@list.from_array([4])")
}

test "list_unfold_operations" {
  // 测试 List 的 unfold 操作
  // 生成序列 [0, 1, 2, 3, 4]
  let sequence = @list.unfold(i => if i >= 5 { None } else { Some((i, i + 1)) }, init=0)
  inspect(sequence, content="@list.from_array([0, 1, 2, 3, 4])")
  
  // 反向生成序列 [4, 3, 2, 1, 0]
  let rev_sequence = @list.rev_unfold(i => if i >= 5 { None } else { Some((i, i + 1)) }, init=0)
  inspect(rev_sequence, content="@list.from_array([4, 3, 2, 1, 0])")
}

test "complex_list_transformations" {
  // 测试复杂的 List 转换操作
  let data = @list.from_array([
    ("Alice", 25),
    ("Bob", 30),
    ("Charlie", 35),
    ("Diana", 28)
  ])
  
  // 解构并重新组合
  let (names, ages) = data.unzip()
  inspect(names, content="@list.from_array([\"Alice\", \"Bob\", \"Charlie\", \"Diana\"])"
  inspect(ages, content="@list.from_array([25, 30, 35, 28])")
  
  // 复杂的 map-filter 组合
  let adults_over_30 = data
    .filter_map(pair => match pair {
      (name, age) => if age > 30 { Some((name, age)) } else { None }
    })
  
  inspect(adults_over_30, content="@list.from_array([(\"Bob\", 30), (\"Charlie\", 35)])")
}