// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法

test "azimuth_basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-3, add(-10, 7))
  assert_eq(0, add(5, -5))
  assert_eq(24, multiply(6, 4))
  assert_eq(-18, multiply(-3, 6))
  assert_eq(0, multiply(0, 100))
}

test "azimuth_divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "azimuth_greet_function_variations" {
  // 问候函数变体测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中国!", greet("中国"))
}

test "azimuth_mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律 a * (b + c) = a * b + a * c
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "azimuth_business_calculation_scenario" {
  // 业务计算场景测试
  let unit_price = 45
  let quantity = 6
  let tax_rate = 10
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(27, tax)
  assert_eq(312, total)
}

test "azimuth_edge_cases_and_boundary_values" {
  // 边界值和极端情况测试
  // 零值运算
  assert_eq(0, multiply(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位运算
  assert_eq(7, multiply(7, 1))
  assert_eq(7, add(7, 0))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // 负数运算
  assert_eq(14, multiply(-2, -7))
  assert_eq(-3, add(-10, 7))
}

test "azimuth_complex_nested_operations" {
  // 复杂嵌套运算测试
  // 计算：(a + b) * c - d / e
  let a = 8
  let b = 12
  let c = 5
  let d = 20
  let e = 3
  
  let result = add(multiply(add(a, b), c), -divide_with_ceil(d, e))
  assert_eq(97, result)  // (8 + 12) * 5 - ceil(20/3) = 20 * 5 - 7 = 100 - 7 = 93
  
  // 嵌套函数调用
  let nested = divide_with_ceil(multiply(add(3, 4), add(5, 2)), 6)
  assert_eq(9, nested)  // ceil((3 + 4) * (5 + 2) / 6) = ceil(7 * 7 / 6) = ceil(49/6) = 9
}

test "azimuth_real_world_applications" {
  // 实际应用场景测试
  // 库存管理
  let items_per_box = 24
  let total_items = 65
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(3, boxes_needed)
  
  // 时间计算
  let tasks_per_day = 7
  let total_tasks = 30
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  assert_eq(5, days_needed)
  
  // 价格计算
  let price_per_item = 15
  let quantity = 8
  let discount = 20
  let final_price = add(multiply(price_per_item, quantity), -discount)
  assert_eq(100, final_price)
}

test "azimuth_string_and_numeric_integration" {
  // 字符串和数值集成测试
  let user_id = 12345
  let user_name = "Alice"
  
  // 创建用户标识
  let user_id_str = "User" + user_id.to_string()
  let greeting = greet(user_name + " (" + user_id_str + ")")
  
  assert_eq_string("Hello, Alice (User12345)!", greeting)
  
  // 数值验证
  assert_eq(12345, user_id)
}

test "azimuth_error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零安全性
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  
  // 空字符串处理
  assert_eq_string("Hello, !", greet(""))
}