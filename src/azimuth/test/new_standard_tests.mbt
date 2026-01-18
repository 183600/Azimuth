// 新增的标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "mathematical_operations_combination" {
  // 测试数学运算组合：(a + b) * c - d
  let a = 8
  let b = 12
  let c = 3
  let d = 15
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  let result = add(product, -d)
  
  assert_eq(45, result)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, 测试123!", greet("测试123"))
}

test "overflow_protection_verification" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -100))
  
  // 乘法溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
}

test "practical_business_calculation" {
  // 测试实际业务计算：订单总价
  let unit_price = 199
  let quantity = 5
  let discount = 50
  let shipping = 20
  
  let subtotal = multiply(unit_price, quantity)
  let discounted_total = add(subtotal, -discount)
  let final_total = add(discounted_total, shipping)
  
  assert_eq(1025, final_total)
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let x = 15
  let y = 25
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律：(x + y) * z = x * z + y * z
  let z = 4
  let left_side = multiply(add(x, y), z)
  let right_side = add(multiply(x, z), multiply(y, z))
  assert_eq(left_side, right_side)
}

test "sequential_calculation_chain" {
  // 测试连续计算链
  let initial = 10
  let step1 = add(initial, 5)
  let step2 = multiply(step1, 3)
  let step3 = add(step2, -15)
  let step4 = multiply(step3, 2)
  
  assert_eq(60, step4)
}

test "special_value_handling" {
  // 测试特殊值处理
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 零值特性
  assert_eq(5, add(5, zero))
  assert_eq(0, multiply(5, zero))
  
  // 单位元特性
  assert_eq(5, multiply(5, one))
  assert_eq(5, multiply(one, 5))
  
  // 负一特性
  assert_eq(-5, multiply(5, neg_one))
  assert_eq(-5, multiply(neg_one, 5))
}

test "complex_string_scenarios" {
  // 测试复杂字符串场景
  let unicode_name = "张三李四"
  let mixed_name = "User2023@Example"
  let symbolic_name = "!@#$%^&*()"
  
  assert_eq_string("Hello, 张三李四!", greet(unicode_name))
  assert_eq_string("Hello, User2023@Example!", greet(mixed_name))
  assert_eq_string("Hello, !@#$%^&*()!", greet(symbolic_name))
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 边界值与普通值的运算
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
  assert_eq(0, multiply(max_val, 0))
}

test "real_world_application_scenario" {
  // 测试实际应用场景：温度转换
  // 摄氏度转华氏度（简化版）：F = C * 2 + 30
  let celsius_temps = [0, 10, 20, 30, 40]
  let expected_fahrenheit = [30, 50, 70, 90, 110]
  
  // 测试第一个温度点
  let celsius = celsius_temps[0]
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(expected_fahrenheit[0], fahrenheit)
  
  // 测试中间温度点
  let celsius_mid = celsius_temps[2]
  let fahrenheit_mid = add(multiply(celsius_mid, 2), 30)
  assert_eq(expected_fahrenheit[2], fahrenheit_mid)
  
  // 测试最高温度点
  let celsius_high = celsius_temps[4]
  let fahrenheit_high = add(multiply(celsius_high, 2), 30)
  assert_eq(expected_fahrenheit[4], fahrenheit_high)
}