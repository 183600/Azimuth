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

// Azimuth 项目标准 MoonBit 测试用例
// 包含 10 个核心功能测试，使用标准 MoonBit 测试语法

test "subtract_function_basic" {
  // 测试基本减法功能
  assert_eq(7, subtract(10, 3))?
  assert_eq(0, subtract(5, 5))?
  assert_eq(-10, subtract(0, 10))?
  assert_eq(-2, subtract(-5, -3))?
}

test "divide_with_ceil_mixed_signs" {
  // 测试混合符号的向上取整除法
  assert_eq(-3, divide_with_ceil(15, -4))?
  assert_eq(-3, divide_with_ceil(-15, 4))?
  assert_eq(4, divide_with_ceil(-15, -4))?
  assert_eq(0, divide_with_ceil(0, -5))?
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值
  let a = 8
  let b = 3
  let c = 2
  
  // 计算 (a * b) + (a / c) - b
  let result = add(
    multiply(a, b),
    subtract(divide_with_ceil(a, c), b)
  )
  assert_eq(25, result)?  // (8*3) + ceil(8/2) - 3 = 24 + 4 - 3 = 25
}

test "greet_function_with_special_chars" {
  // 测试包含特殊字符的问候功能
  assert_eq("Hello, C++!", greet("C++"))?
  assert_eq("Hello, Java/C#!", greet("Java/C#"))?
  assert_eq("Hello, HTML&CSS!", greet("HTML&CSS"))?
  assert_eq("Hello, Python@3.9!", greet("Python@3.9"))?
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算：等比数列前n项和
  let first_term = 2
  let ratio = 3
  
  // 计算 2 + 6 + 18 + 54
  let sum = add(
    add(
      first_term,
      multiply(first_term, ratio)
    ),
    add(
      multiply(first_term, multiply(ratio, ratio)),
      multiply(first_term, multiply(multiply(ratio, ratio), ratio))
    )
  )
  assert_eq(80, sum)?
}

test "business_profit_calculation" {
  // 测试商业利润计算
  let revenue = 5000
  let cost_per_unit = 30
  let units_sold = 100
  let fixed_costs = 800
  
  let total_cost = add(multiply(cost_per_unit, units_sold), fixed_costs)
  let profit = subtract(revenue, total_cost)
  
  assert_eq(3800, total_cost)?
  assert_eq(1200, profit)?
}

test "array_index_simulation" {
  // 模拟数组索引计算
  let row_size = 5
  let row = 2
  let col = 3
  
  // 计算二维数组的一维索引
  let index = add(multiply(row, row_size), col)
  assert_eq(13, index)?  // 2*5 + 3 = 13
}

test "time_conversion_calculation" {
  // 测试时间转换计算
  let hours = 3
  let minutes = 45
  let total_minutes = add(multiply(hours, 60), minutes)
  
  // 转换为15分钟的时间块数量
  let time_blocks = divide_with_ceil(total_minutes, 15)
  
  assert_eq(225, total_minutes)?
  assert_eq(15, time_blocks)?
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_resources = 47
  let team_size = 5
  let min_per_team = 8
  
  // 计算每个团队的基本分配
  let base_allocation = multiply(team_size, min_per_team)
  
  // 计算剩余资源
  let remaining = subtract(total_resources, base_allocation)
  
  // 计算每个团队的额外分配
  let extra_per_team = divide_with_ceil(remaining, team_size)
  
  // 计算每个团队的总分配
  let total_per_team = add(min_per_team, extra_per_team)
  
  assert_eq(40, base_allocation)?
  assert_eq(7, remaining)?
  assert_eq(2, extra_per_team)?
  assert_eq(10, total_per_team)?
}

test "error_boundary_validation" {
  // 测试错误边界验证
  // 测试极值运算
  let large_positive = 1000000
  let large_negative = -1000000
  
  assert_eq(0, add(large_positive, large_negative))?
  assert_eq(0, multiply(large_positive, 0))?
  assert_eq(1000000, divide_with_ceil(large_positive, 1))?
  assert_eq(-1000000, divide_with_ceil(large_negative, 1))?
  
  // 测试边界除法
  assert_eq(1, divide_with_ceil(1, 1000000))?
  assert_eq(-1, divide_with_ceil(-1, 1000000))?
  assert_eq(0, divide_with_ceil(0, 1000000))?
}