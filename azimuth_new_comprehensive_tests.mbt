// Azimuth 项目新增综合测试用例
// 使用标准 MoonBit 测试语法

// 导入 azimuth 模块
use azimuth

test "add_function_zero_properties" {
  // 测试加法函数的零值特性
  assert_eq(42, azimuth.add(42, 0))
  assert_eq(42, azimuth.add(0, 42))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(-17, azimuth.add(-17, 0))
  assert_eq(-17, azimuth.add(0, -17))
}

test "multiply_function_commutative_property" {
  // 测试乘法函数的交换律
  assert_eq(azimuth.multiply(3, 7), azimuth.multiply(7, 3))
  assert_eq(azimuth.multiply(-4, 5), azimuth.multiply(5, -4))
  assert_eq(azimuth.multiply(-6, -8), azimuth.multiply(-8, -6))
  assert_eq(azimuth.multiply(0, 10), azimuth.multiply(10, 0))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @#$%!", azimuth.greet("@#$%"))
  assert_eq_string("Hello, 中文!", azimuth.greet("中文"))
  assert_eq_string("Hello, 🌙!", azimuth.greet("🌙"))
  assert_eq_string("Hello, line\nbreak!", azimuth.greet("line\nbreak"))
}

test "add_function_large_numbers" {
  // 测试加法函数的大数相加
  let large_num1 = 1000000
  let large_num2 = 2000000
  assert_eq(3000000, azimuth.add(large_num1, large_num2))
  
  let large_num3 = 100000000
  let large_num4 = -50000000
  assert_eq(50000000, azimuth.add(large_num3, large_num4))
}

test "multiply_function_negative_numbers" {
  // 测试乘法函数的负数乘法
  assert_eq(-25, azimuth.multiply(-5, 5))
  assert_eq(36, azimuth.multiply(-6, -6))
  assert_eq(0, azimuth.multiply(-7, 0))
  assert_eq(49, azimuth.multiply(-7, -7))
}

test "greet_function_long_strings" {
  // 测试问候函数的长字符串处理
  let long_name = "This is a very long name that should be handled properly by the greet function"
  assert_eq_string("Hello, This is a very long name that should be handled properly by the greet function!", azimuth.greet(long_name))
  
  let empty_name = ""
  assert_eq_string("Hello, !", azimuth.greet(empty_name))
}

test "combined_operations" {
  // 测试 add 和 multiply 函数的组合运算
  // 计算 (2 + 3) * (4 + 5) = 5 * 9 = 45
  let result1 = azimuth.multiply(azimuth.add(2, 3), azimuth.add(4, 5))
  assert_eq(45, result1)
  
  // 计算 10 * (5 + 3) + 2 = 10 * 8 + 2 = 82
  let result2 = azimuth.add(azimuth.multiply(10, azimuth.add(5, 3)), 2)
  assert_eq(82, result2)
}

test "boundary_value_special_cases" {
  // 测试边界值的特殊情况
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近边界的加法
  assert_eq(max_int, azimuth.add(max_int - 1, 1))
  assert_eq(max_int, azimuth.add(max_int - 100, 100))
  
  // 测试接近边界的乘法
  assert_eq(max_int, azimuth.multiply(46340, 46340))  // sqrt(max_int) 的平方
  assert_eq(0, azimuth.multiply(min_int, 0))
}

test "assertion_helper_functions" {
  // 测试断言辅助函数
  azimuth.assert_eq(10, 10)
  azimuth.assert_eq_string("test", "test")
  azimuth.assert_true(true)
  azimuth.assert_false(false)
  
  // 测试组合断言
  let result = azimuth.add(3, 4)
  azimuth.assert_eq(7, result)
  azimuth.assert_true(result > 0)
  
  let greeting = azimuth.greet("Test")
  azimuth.assert_eq_string("Hello, Test!", greeting)
  azimuth.assert_true(String.length(greeting) > 5)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1：计算购物车总价
  let item1_price = 199
  let item2_price = 299
  let shipping = 20
  let discount = -50
  let total = azimuth.add(azimuth.add(azimuth.add(item1_price, item2_price), shipping), discount)
  assert_eq(468, total)
  
  // 场景2：计算矩形面积和周长
  let length = 12
  let width = 8
  let area = azimuth.multiply(length, width)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  assert_eq(96, area)
  assert_eq(40, perimeter)
  
  // 场景3：计算复利（简化版）
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  // 简化计算：principal + principal * rate * years / 100
  let interest = azimuth.multiply(azimuth.multiply(principal, rate), years) / 100
  let final_amount = azimuth.add(principal, interest)
  assert_eq(1150, final_amount)
}