// Azimuth 项目新增标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法

// 导入 azimuth 模块
use azimuth

test "addition_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值相加
  @assertion.assert_eq(-1, azimuth.add(max_val, min_val))?
  
  // 测试最大值与0相加
  @assertion.assert_eq(max_val, azimuth.add(max_val, 0))?
  
  // 测试最小值与0相加
  @assertion.assert_eq(min_val, azimuth.add(min_val, 0))?
  
  // 测试正负数相加
  @assertion.assert_eq(0, azimuth.add(100, -100))?
}

test "multiplication_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与1相乘
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 1))?
  
  // 测试最小值与1相乘
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 1))?
  
  // 测试最大值与-1相乘
  @assertion.assert_eq(min_val, azimuth.multiply(max_val, -1))?
  
  // 测试最小值与-1相乘（特殊情况）
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))?
}

test "greet_function_variations" {
  // 测试 greet 函数的各种情况
  // 测试空字符串
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
  
  // 测试单字符
  @assertion.assert_eq_string("Hello, A!", azimuth.greet("A"))?
  
  // 测试包含空格的字符串
  @assertion.assert_eq_string("Hello, John Doe!", azimuth.greet("John Doe"))?
  
  // 测试包含特殊字符的字符串
  @assertion.assert_eq_string("Hello, test@example.com!", azimuth.greet("test@example.com"))?
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum = azimuth.add(a, b)
  let product = azimuth.multiply(sum, c)
  let result = azimuth.add(product, -d)
  
  @assertion.assert_eq(75, result)?
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 测试加法交换律
  @assertion.assert_eq(azimuth.add(x, y), azimuth.add(y, x))?
  
  // 测试乘法交换律
  @assertion.assert_eq(azimuth.multiply(x, y), azimuth.multiply(y, x))?
  
  // 测试分配律
  let z = 5
  @assertion.assert_eq(
    azimuth.multiply(x, azimuth.add(y, z)),
    azimuth.add(azimuth.multiply(x, y), azimuth.multiply(x, z))
  )?
}

test "overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @assertion.assert_eq(max_val, azimuth.add(max_val, 1))?
  @assertion.assert_eq(max_val, azimuth.add(max_val, max_val))?
  
  // 测试减法溢出保护
  @assertion.assert_eq(min_val, azimuth.add(min_val, -1))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, min_val))?
  
  // 测试乘法溢出保护
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 2))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 2))?
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let tax_rate = 8  // 8%
  
  let subtotal = azimuth.multiply(item_price, quantity)
  let tax = azimuth.multiply(subtotal, tax_rate) / 100
  let total = azimuth.add(azimuth.add(subtotal, tax), shipping)
  
  @assertion.assert_eq(659, total)?
  
  // 温度转换（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  @assertion.assert_eq(80, fahrenheit)?
}

test "string_processing" {
  // 测试字符串处理
  // 测试数字字符串
  @assertion.assert_eq_string("Hello, 123!", azimuth.greet("123"))?
  
  // 测试混合字符串
  @assertion.assert_eq_string("Hello, User2023!", azimuth.greet("User2023"))?
  
  // 测试包含Unicode字符的字符串
  @assertion.assert_eq_string("Hello, 测试!", azimuth.greet("测试"))?
  
  // 测试包含emoji的字符串
  @assertion.assert_eq_string("Hello, 🌍!", azimuth.greet("🌍"))?
}

test "performance_patterns" {
  // 测试性能模式
  let base = 100
  
  // 测试重复加法 vs 乘法
  let repeated_add = azimuth.add(azimuth.add(azimuth.add(base, base), base), base)
  let multiply_result = azimuth.multiply(base, 4)
  
  @assertion.assert_eq(repeated_add, multiply_result)?
  
  // 测试嵌套计算
  let nested = azimuth.multiply(azimuth.add(10, 5), azimuth.add(20, 10))
  @assertion.assert_eq(450, nested)?
}

test "error_handling" {
  // 测试错误处理
  // 测试断言函数
  @assertion.assert_true(true)?
  @assertion.assert_false(false)?
  @assertion.assert_eq(42, 42)?
  @assertion.assert_eq_string("test", "test")?
  
  // 测试边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  @assertion.assert_true(azimuth.add(max_val, 1) == max_val)?
  @assertion.assert_true(azimuth.add(min_val, -1) == min_val)?
  @assertion.assert_true(azimuth.multiply(max_val, 2) == max_val)?
}