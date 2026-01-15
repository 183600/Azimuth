// 新增的 MoonBit 测试用例 - 不超过10个
// 使用标准的 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(4, 6))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-12, multiply(-3, 4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "edge_case_calculations" {
  // 测试边界情况计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  // 计算 (a + b) * c - d
  let a = 8
  let b = 7
  let c = 3
  let d = 10
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  let result = add(product, -d)
  
  assert_eq(35, result)
}

test "conditional_logic_with_math" {
  // 测试条件逻辑与数学运算结合
  let x = 25
  let y = 30
  
  // 如果 x < y，计算 x + y，否则计算 x * y
  let condition = x < y
  let result = if condition { add(x, y) } else { multiply(x, y) }
  
  assert_eq(55, result)
  
  // 反向测试
  let condition2 = x > y
  let result2 = if condition2 { add(x, y) } else { multiply(x, y) }
  
  assert_eq(750, result2)
}

test "real_world_shopping_cart" {
  // 测试实际购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  // 计算总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let final_total = add(add(total1, total2), shipping)
  
  assert_eq(717, final_total)
}

test "temperature_conversion_formula" {
  // 测试温度转换公式（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30
  let celsius = 18
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  assert_eq(66, fahrenheit)
  
  // 测试冰点
  assert_eq(30, add(multiply(0, 2), 30))
  
  // 测试沸点
  assert_eq(230, add(multiply(100, 2), 30))
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算平均值
  let numbers_sum = add(add(add(add(85, 90), 75), 95), 80)
  let count = 5
  let average = numbers_sum / count
  
  assert_eq(85, average)
  
  // 计算总和
  assert_eq(425, numbers_sum)
}

test "string_processing_with_special_chars" {
  // 测试特殊字符字符串处理
  let email = "user@example.com"
  let phone = "123-456-7890"
  let mixed = "Test_123@Project"
  
  assert_eq_string("Hello, user@example.com!", greet(email))
  assert_eq_string("Hello, 123-456-7890!", greet(phone))
  assert_eq_string("Hello, Test_123@Project!", greet(mixed))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律 a × (b + c) = a × b + a × c
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}