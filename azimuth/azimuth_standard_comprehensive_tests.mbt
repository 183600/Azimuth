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

// Azimuth 库的标准测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-2, add(-10, 8))
  assert_eq(0, add(5, -5))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-80, multiply(-10, 8))
  assert_eq(0, multiply(5, 0))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-15, subtract(5, 20))
  assert_eq(0, subtract(8, 8))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法综合场景测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(15, 0))
  
  // 负数场景
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_laws_and_properties" {
  // 数学定律和性质测试
  let a = 12
  let b = 7
  
  // 加法交换律和结合律
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(add(a, b), 3), add(a, add(b, 3)))
  
  // 乘法交换律和结合律
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(multiply(a, b), 2), multiply(a, multiply(b, 2)))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_logistics_calculation" {
  // 真实世界物流计算测试
  let total_packages = 127
  let truck_capacity = 30
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  
  assert_eq(5, trucks_needed)
  
  // 验证总运输能力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(150, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  assert_eq(23, remaining_space)
}

test "financial_budget_allocation" {
  // 财务预算分配测试
  let total_budget = 50000
  let departments = 8
  let base_allocation = divide_with_ceil(total_budget, departments)
  
  assert_eq(6250, base_allocation)
  
  // 计算总分配金额
  let total_allocated = multiply(base_allocation, departments)
  assert_eq(50000, total_allocated)
  
  // 验证每个部门实际获得的金额
  let final_allocation = subtract(total_allocated, multiply(base_allocation, departments - 1))
  assert_eq(6250, final_allocation)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
}

test "complex_workflow_integration" {
  // 复杂工作流集成测试
  // 场景：项目管理资源计算
  let team_members = 15
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let tasks_per_day = 6
  let sprint_capacity = multiply(sprint_duration, tasks_per_day)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  let total_days = multiply(sprints_needed, sprint_duration)
  
  assert_eq(120, total_tasks)  // 15 * 8 = 120
  assert_eq(12, sprint_capacity)  // 2 * 6 = 12
  assert_eq(10, sprints_needed)  // ceil(120/12) = 10
  assert_eq(20, total_days)  // 10 * 2 = 20
}