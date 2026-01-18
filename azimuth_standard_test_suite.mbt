// Azimuth 项目标准测试用例集
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
}

test "greet_function_basic" {
  // 测试greet函数基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_expressions" {
  // 测试数学表达式
  // (2 + 3) * 4 = 20
  let result1 = multiply(add(2, 3), 4)
  assert_eq(20, result1)
  
  // 5 * (6 + 7) = 65
  let result2 = multiply(5, add(6, 7))
  assert_eq(65, result2)
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试最小值加法
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试最大值乘法
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(max_val, multiply(1, max_val))
  
  // 测试最小值乘法
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(1, min_val))
}

test "string_operations_edge_cases" {
  // 测试字符串操作边界情况
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, Test@123!", greet("Test@123"))
  assert_eq_string("Hello, 用户测试!", greet("用户测试"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced  "))
  
  // 测试包含引号的字符串
  assert_eq_string("Hello, \"Quoted\"!", greet("\"Quoted\""))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算矩形周长：2 * (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(30, perimeter)
  
  // 计算梯形面积：(上底 + 下底) * 高 / 2
  let top_base = 6
  let bottom_base = 10
  let height = 4
  let trapezoid_area = multiply(add(top_base, bottom_base), height) / 2
  assert_eq(32, trapezoid_area)
}

test "sequential_operations" {
  // 测试连续操作
  // 连续加法：1 + 2 + 3 + 4 + 5 = 15
  let sum_result = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum_result)
  
  // 连续乘法：1 * 2 * 3 * 4 = 24
  let product_result = multiply(multiply(multiply(1, 2), 3), 4)
  assert_eq(24, product_result)
  
  // 混合操作：(1 + 2) * (3 + 4) = 21
  let mixed_result = multiply(add(1, 2), add(3, 4))
  assert_eq(21, mixed_result)
}

test "real_world_scenarios" {
  // 测试真实世界场景
  // 场景1：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let cart_total = add(total1, total2)
  
  assert_eq(697, cart_total)  // 199*2 + 299*1 = 398 + 299 = 697
  
  // 场景2：温度转换（摄氏度转华氏度，简化版：F = C * 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
}

test "assert_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试数学运算的断言
  assert_true(add(2, 3) == 5)
  assert_true(multiply(4, 5) == 20)
  assert_false(add(2, 3) == 6)
}