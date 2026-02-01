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

// 标准 Azimuth 测试用例
// 包含 10 个核心功能测试，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  inspect(@azimuth.add(5, 3), content="8")
  inspect(@azimuth.multiply(4, 6), content="24")
  inspect(@azimuth.subtract(10, 7), content="3")
  inspect(@azimuth.divide_with_ceil(10, 3), content="4")
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法交换律
  inspect(@azimuth.add(7, 13), content="20")
  inspect(@azimuth.add(13, 7), content="20")
  
  // 乘法交换律
  inspect(@azimuth.multiply(8, 9), content="72")
  inspect(@azimuth.multiply(9, 8), content="72")
  
  // 零元素性质
  inspect(@azimuth.multiply(15, 0), content="0")
  inspect(@azimuth.add(25, 0), content="25")
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  // 正数情况
  inspect(@azimuth.divide_with_ceil(10, 3), content="4")  // 3.333... -> 4
  inspect(@azimuth.divide_with_ceil(9, 3), content="3")   // 3 -> 3
  inspect(@azimuth.divide_with_ceil(1, 100), content="1") // 0.01 -> 1
  
  // 负数情况
  inspect(@azimuth.divide_with_ceil(-10, 3), content="-3")  // -3.333... -> -3
  inspect(@azimuth.divide_with_ceil(-10, -3), content="4")  // 3.333... -> 4
  
  // 除数为零的情况
  inspect(@azimuth.divide_with_ceil(100, 0), content="0")
}

test "greeting_function_comprehensive" {
  // 测试问候函数的全面功能
  inspect(@azimuth.greet("World"), content="Hello, World!")
  inspect(@azimuth.greet("MoonBit"), content="Hello, MoonBit!")
  inspect(@azimuth.greet(""), content="Hello, !")
  inspect(@azimuth.greet("世界"), content="Hello, 世界!")
  inspect(@azimuth.greet("🚀"), content="Hello, 🚀!")
}

test "negative_number_operations" {
  // 测试负数运算
  inspect(@azimuth.add(-5, -3), content="-8")
  inspect(@azimuth.add(-8, 3), content="-5")
  inspect(@azimuth.multiply(-4, 5), content="-20")
  inspect(@azimuth.multiply(-4, -5), content="20")
  inspect(@azimuth.subtract(5, 10), content="-5")
}

test "combined_operations" {
  // 测试组合运算
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(5, 3), 4)
  inspect(result1, content="32")  // (5 + 3) * 4 = 8 * 4 = 32
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(6, 7), 8)
  inspect(result2, content="50")  // 6 * 7 + 8 = 42 + 8 = 50
  
  // 复杂表达式
  let result3 = @azimuth.add(
    @azimuth.multiply(@azimuth.divide_with_ceil(25, 4), 3),
    @azimuth.multiply(@azimuth.divide_with_ceil(17, 3), 2)
  )
  inspect(result3, content="85")  // ceil(25/4)*3 + ceil(17/3)*2 = 7*3 + 6*2 = 21 + 12 = 33
}

test "business_calculation_scenario" {
  // 测试业务计算场景
  // 计算订单总价：商品价格 × 数量 + 运费
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  inspect(subtotal, content="200")  // 25 * 8 = 200
  inspect(total, content="215")     // 200 + 15 = 215
}

test "packaging_calculation_scenario" {
  // 测试包装计算场景
  // 计算需要的包装箱数量
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  
  inspect(boxes_needed, content="4")      // ceil(47/12) = 4
  inspect(total_capacity, content="48")   // 4 * 12 = 48
  inspect(remaining_space, content="1")   // 48 - 47 = 1
}

test "extreme_boundary_values" {
  // 测试极端边界值
  // 大数运算
  inspect(@azimuth.add(1000000, 1000000), content="2000000")
  inspect(@azimuth.multiply(1000, 1000), content="1000000")
  
  // 边界情况
  inspect(@azimuth.add(2147483646, 1), content="2147483647")  // 接近Int最大值
  inspect(@azimuth.add(-2147483647, -1), content="-2147483648")  // 接近Int最小值
  inspect(@azimuth.divide_with_ceil(999999, 1000), content="1000")  // 999.999 -> 1000
}

test "string_processing_with_greet" {
  // 测试字符串处理与问候函数结合
  // 构建复杂字符串
  let first_name = "John"
  let last_name = "Doe"
  let full_name = first_name + " " + last_name
  let greeting = @azimuth.greet(full_name)
  
  inspect(greeting, content="Hello, John Doe!")
  
  // 测试特殊字符和表情符号
  inspect(@azimuth.greet("Alice & Bob"), content="Hello, Alice & Bob!")
  inspect(@azimuth.greet("😊"), content="Hello, 😊!")
  inspect(@azimuth.greet("test@example.com"), content="Hello, test@example.com!")
}