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

// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "add_function_positive_numbers" {
  // 测试正数加法
  inspect(add(3, 7), content="10")
  inspect(add(0, 5), content="5")
  inspect(add(100, 200), content="300")
}

test "add_function_negative_numbers" {
  // 测试负数加法
  inspect(add(-3, -7), content="-10")
  inspect(add(-5, 2), content="-3")
  inspect(add(-10, 20), content="10")
}

test "multiply_function_various_cases" {
  // 测试乘法函数的各种情况
  inspect(multiply(4, 5), content="20")
  inspect(multiply(-3, 7), content="-21")
  inspect(multiply(-4, -6), content="24")
  inspect(multiply(8, 0), content="0")
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  inspect(divide_with_ceil(10, 3), content="4")
  inspect(divide_with_ceil(9, 3), content="3")
  inspect(divide_with_ceil(1, 5), content="1")
  inspect(divide_with_ceil(0, 7), content="0")
  inspect(divide_with_ceil(15, 0), content="0")
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  inspect(divide_with_ceil(-10, 3), content="-3")
  inspect(divide_with_ceil(10, -3), content="-4")
  inspect(divide_with_ceil(-10, -3), content="4")
  inspect(divide_with_ceil(-1, 2), content="-1")
}

test "greet_function_comprehensive" {
  // 测试问候函数的综合情况
  inspect(greet("World"), content="Hello, World!")
  inspect(greet("MoonBit"), content="Hello, MoonBit!")
  inspect(greet(""), content="Hello, !")
  inspect(greet("123"), content="Hello, 123!")
  inspect(greet("中文"), content="Hello, 中文!")
}

test "subtract_function_basic" {
  // 测试减法函数的基本功能
  inspect(subtract(10, 3), content="7")
  inspect(subtract(3, 10), content="-7")
  inspect(subtract(5, 5), content="0")
  inspect(subtract(0, -5), content="5")
}

test "complex_business_workflow" {
  // 测试复杂业务工作流
  // 场景：计算项目资源需求
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  inspect(total_tasks, content="48")
  inspect(sprints_needed, content="4")
}