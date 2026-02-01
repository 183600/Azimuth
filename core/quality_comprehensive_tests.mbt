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
  assert_eq(@int.max_value, 2147483647)
  assert_eq(@int.min_value, -2147483648)
  
  // 测试边界值的绝对值
  assert_eq(Int::abs(@int.max_value), @int.max_value)
  assert_eq(Int::abs(@int.min_value), @int.max_value)
  assert_eq(Int::abs(0), 0)
  assert_eq(Int::abs(-42), 42)
}

test "list_basic_operations" {
  // 测试 List 模块的基本操作
  let empty_list : @list.List[Int] = @list.empty()
  assert_eq(empty_list.length(), 0)
  assert_eq(empty_list.is_empty(), true)
  
  let list1 = @list.from_array([1, 2, 3, 4, 5])
  assert_eq(list1.length(), 5)
  assert_eq(list1.is_empty(), false)
  assert_eq(list1.head(), Some(1))
  assert_eq(list1.last(), Some(5))
  assert_eq(list1.nth(2), Some(3))
  assert_eq(list1.nth(10), None)
}

test "list_functional_operations" {
  // 测试 List 的函数式操作
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // map 操作
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled, @list.from_array([2, 4, 6, 8, 10]))
  
  // filter 操作
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(evens, @list.from_array([2, 4]))
  
  // fold 操作
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  assert_eq(sum, 15)
  
  // any/all 操作
  assert_eq(numbers.any(x => x > 3), true)
  assert_eq(numbers.all(x => x > 0), true)
  assert_eq(numbers.all(x => x > 3), false)
}

test "list_advanced_operations" {
  // 测试 List 的高级操作
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  
  // concat 操作
  let combined = list1.concat(list2)
  assert_eq(combined, @list.from_array([1, 2, 3, 4, 5, 6]))
  
  // reverse 操作
  let reversed = combined.rev()
  assert_eq(reversed, @list.from_array([6, 5, 4, 3, 2, 1]))
  
  // zip 操作
  let zipped = @list.zip(list1, list2)
  assert_eq(zipped, @list.from_array([(1, 4), (2, 5), (3, 6)]))
  
  // take 操作
  let taken = combined.take(4)
  assert_eq(taken, @list.from_array([1, 2, 3, 4]))
}

test "list_search_operations" {
  // 测试 List 的搜索操作
  let fruits = @list.from_array(["apple", "banana", "cherry", "date"])
  
  // contains 操作
  assert_eq(fruits.contains("banana"), true)
  assert_eq(fruits.contains("grape"), false)
  
  // maximum/minimum 操作
  let numbers = @list.from_array([3, 1, 4, 1, 5, 9, 2])
  assert_eq(numbers.maximum(), Some(9))
  assert_eq(numbers.minimum(), Some(1))
  
  let empty_numbers : @list.List[Int] = @list.empty()
  assert_eq(empty_numbers.maximum(), None)
  assert_eq(empty_numbers.minimum(), None)
}

test "string_basic_operations" {
  // 测试 String 模块的基本操作
  let empty_string = @string.default()
  assert_eq(empty_string, "")
  
  // 测试字符串长度（如果可用）
  let test_string = "Hello, MoonBit!"
  assert_eq(test_string.length(), 14)
}

test "list_edge_cases" {
  // 测试 List 的边界情况
  let single_element = @list.from_array([42])
  assert_eq(single_element.head(), Some(42))
  assert_eq(single_element.last(), Some(42))
  assert_eq(single_element.unsafe_tail(), @list.empty())
  
  // 测试重复元素
  let repeated = @list.repeat(3, "x")
  assert_eq(repeated, @list.from_array(["x", "x", "x"]))
  
  // 测试 intersperse
  let interspersed = @list.from_array([1, 2, 3]).intersperse(0)
  assert_eq(interspersed, @list.from_array([1, 0, 2, 0, 3]))
}

test "list_flat_map_operations" {
  // 测试 List 的 flat_map 操作
  let numbers = @list.from_array([1, 2, 3])
  
  // flat_map 展开操作
  let expanded = numbers.flat_map(x => @list.from_array([x, x * 10]))
  assert_eq(expanded, @list.from_array([1, 10, 2, 20, 3, 30]))
  
  // filter_map 操作
  let filter_mapped = numbers.filter_map(x => if x % 2 == 0 { Some(x * x) } else { None })
  assert_eq(filter_mapped, @list.from_array([4]))
}

test "list_unfold_operations" {
  // 测试 List 的 unfold 操作
  // 生成序列 [0, 1, 2, 3, 4]
  let sequence = @list.unfold(i => if i >= 5 { None } else { Some((i, i + 1)) }, init=0)
  assert_eq(sequence, @list.from_array([0, 1, 2, 3, 4]))
  
  // 反向生成序列 [4, 3, 2, 1, 0]
  let rev_sequence = @list.rev_unfold(i => if i >= 5 { None } else { Some((i, i + 1)) }, init=0)
  assert_eq(rev_sequence, @list.from_array([4, 3, 2, 1, 0]))
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
  assert_eq(names, @list.from_array(["Alice", "Bob", "Charlie", "Diana"]))
  assert_eq(ages, @list.from_array([25, 30, 35, 28]))
  
  // 复杂的 map-filter 组合
  let adults_over_30 = data
    .filter_map(pair => match pair {
      (name, age) => if age > 30 { Some((name, age)) } else { None }
    })
  
  assert_eq(adults_over_30, @list.from_array([("Bob", 30), ("Charlie", 35)]))
}