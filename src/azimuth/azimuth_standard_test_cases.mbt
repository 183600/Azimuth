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

// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含 8 个高质量测试用例，覆盖核心功能

///|
test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  inspect(add(3, 5), content="8")
  inspect(add(0, 0), content="0")
  inspect(add(-5, 3), content="-2")
  inspect(add(50, 50), content="100")
  inspect(add(-50, -50), content="-100")
}

///|
test "multiply_function_comprehensive" {
  // 测试乘法函数的综合运算
  inspect(multiply(3, 5), content="15")
  inspect(multiply(5, 0), content="0")
  inspect(multiply(-3, 5), content="-15")
  inspect(multiply(-3, -5), content="15")
  inspect(multiply(100, 100), content="10000")
}

///|
test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  inspect(divide_with_ceil(8, 3), content="3")
  inspect(divide_with_ceil(6, 3), content="2")
  inspect(divide_with_ceil(1, 5), content="1")
  inspect(divide_with_ceil(0, 10), content="0")
  inspect(divide_with_ceil(10, 0), content="0")
}

///|
test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数处理
  inspect(divide_with_ceil(-8, 3), content="-2")
  inspect(divide_with_ceil(8, -3), content="-3")
  inspect(divide_with_ceil(-8, -3), content="3")
  inspect(divide_with_ceil(-1, 2), content="-1")
  inspect(divide_with_ceil(-1, -2), content="1")
}

///|
test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  inspect(greet("MoonBit"), content="Hello, MoonBit!")
  inspect(greet("World"), content="Hello, World!")
  inspect(greet(""), content="Hello, !")
  inspect(greet("123"), content="Hello, 123!")
  inspect(greet("中文"), content="Hello, 中文!")
}

///|
test "subtract_function_operations" {
  // 测试减法函数运算
  inspect(subtract(10, 5), content="5")
  inspect(subtract(5, 10), content="-5")
  inspect(subtract(15, 15), content="0")
  inspect(subtract(10, -10), content="20")
  inspect(subtract(-10, 10), content="-20")
}

///|
test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 12
  let b = 7
  
  // 加法交换律
  inspect(add(a, b), content="19")
  inspect(add(b, a), content="19")
  
  // 乘法交换律
  inspect(multiply(a, b), content="84")
  inspect(multiply(b, a), content="84")
  
  // 验证具体数值
  inspect(add(12, 7), content="19")
  inspect(multiply(12, 7), content="84")
}

///|
test "business_packaging_scenario" {
  // 测试业务包装场景
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  inspect(packages_needed, content="5")
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  inspect(total_capacity, content="40")
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  inspect(remaining_space, content="5")
}