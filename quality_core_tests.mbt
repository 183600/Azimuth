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
  assert_eq(2147483647, @int.max_value)
  assert_eq(-2147483648, @int.min_value)
  
  // 测试边界值的绝对值
  assert_eq(2147483647, Int::abs(@int.max_value))
  assert_eq(2147483647, Int::abs(@int.min_value))
  assert_eq(0, Int::abs(0))
  assert_eq(42, Int::abs(-42))
}

test "int_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, 10 + 5)
  assert_eq(5, 10 - 5)
  assert_eq(50, 10 * 5)
  assert_eq(3, 10 / 3)
  assert_eq(1, 10 % 3)
  
  // 测试负数运算
  assert_eq(-5, -10 + 5)
  assert_eq(-15, -10 - 5)
  assert_eq(-50, -10 * 5)
  assert_eq(-3, -10 / 3)
}

test "list_basic_operations" {
  // 测试 List 模块的基本操作
  let empty_list : @list.List[Int] = @list.empty()
  assert_eq(0, empty_list.length())
  assert_eq(true, empty_list.is_empty())
  
  let list1 = @list.from_array([1, 2, 3, 4, 5])
  assert_eq(5, list1.length())
  assert_eq(false, list1.is_empty())
  assert_eq(true, list1.length() > 0)
}

test "list_functional_operations" {
  // 测试 List 的函数式操作
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // fold 操作 - 计算总和
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  assert_eq(15, sum)
  
  // fold 操作 - 计算乘积
  let product = numbers.fold(init=1, (acc, x) => acc * x)
  assert_eq(120, product)
  
  // any/all 操作
  assert_eq(true, numbers.any(x => x > 3))
  assert_eq(true, numbers.all(x => x > 0))
  assert_eq(false, numbers.all(x => x > 3))
}

test "list_search_operations" {
  // 测试 List 的搜索操作
  let numbers = @list.from_array([3, 1, 4, 1, 5, 9, 2])
  
  // maximum/minimum 操作
  assert_eq(true, numbers.maximum() == Some(9))
  assert_eq(true, numbers.minimum() == Some(1))
  
  let empty_numbers : @list.List[Int] = @list.empty()
  assert_eq(true, empty_numbers.maximum() == None)
  assert_eq(true, empty_numbers.minimum() == None)
}

test "string_basic_operations" {
  // 测试 String 模块的基本操作
  let empty_string = @string.default()
  assert_eq_string("", empty_string)
  
  // 测试字符串长度
  let test_string = "Hello"
  assert_eq(true, test_string.length() > 0)
  assert_eq(5, test_string.length())
  
  // 测试字符串拼接
  let greeting = "Hello, " + "MoonBit!"
  assert_eq_string("Hello, MoonBit!", greeting)
}

test "list_edge_cases" {
  // 测试 List 的边界情况
  let single_element = @list.from_array([42])
  assert_eq(true, single_element.head() == Some(42))
  assert_eq(true, single_element.last() == Some(42))
  
  // 测试重复元素
  let repeated = @list.repeat(3, 1)
  let sum_repeated = repeated.fold(init=0, (acc, x) => acc + x)
  assert_eq(3, sum_repeated)
  
  // 测试空列表
  let empty_list : @list.List[Int] = @list.empty()
  assert_eq(0, empty_list.length())
  assert_eq(true, empty_list.is_empty())
}

test "list_transformation_operations" {
  // 测试 List 的转换操作
  let numbers = @list.from_array([1, 2, 3])
  
  // map 操作 - 每个元素乘以2
  let doubled = numbers.map(x => x * 2)
  let doubled_sum = doubled.fold(init=0, (acc, x) => acc + x)
  assert_eq(12, doubled_sum)
  
  // filter 操作 - 只保留偶数
  let evens = numbers.filter(x => x % 2 == 0)
  let evens_count = evens.length()
  assert_eq(1, evens_count)
}

test "complex_data_operations" {
  // 测试复杂数据操作
  let data = @list.from_array([
    (10, 25),
    (20, 30),
    (30, 35)
  ])
  
  // 计算第一个元素的总和
  let first_sum = data.fold(init=0, (acc, pair) => {
    let (first, _) = pair
    acc + first
  })
  assert_eq(60, first_sum)
  
  // 计算第二个元素的总和
  let second_sum = data.fold(init=0, (acc, pair) => {
    let (_, second) = pair
    acc + second
  })
  assert_eq(90, second_sum)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 13
  
  // 加法交换律
  assert_eq(x + y, y + x)
  
  // 乘法交换律
  assert_eq(x * y, y * x)
  
  // 分配律
  assert_eq(x * (y + 1), x * y + x)
  
  // 结合律
  assert_eq((x + y) + 5, x + (y + 5))
}

test "boundary_value_testing" {
  // 测试边界值
  assert_eq(0, 0)
  assert_eq(1, 1)
  assert_eq(-1, -1)
  
  // 测试大数
  let large_num = 1000000
  assert_eq(2000000, large_num + large_num)
  assert_eq(1000000000000, large_num * large_num)
  
  // 测试零的性质
  assert_eq(0, 0 * 12345)
  assert_eq(12345, 12345 + 0)
  assert_eq(12345, 12345 - 0)
}