// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  let result1 = azimuth.add(2, 3)
  if (result1 != 5) {
    @builtin.abort("Expected 5, got " + result1.to_string())
  }
  
  let result2 = azimuth.add(-2, 3)
  if (result2 != 1) {
    @builtin.abort("Expected 1, got " + result2.to_string())
  }
  
  let result3 = azimuth.add(0, 0)
  if (result3 != 0) {
    @builtin.abort("Expected 0, got " + result3.to_string())
  }
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值溢出保护
  let result1 = azimuth.add(max_val, 1)
  if (result1 != max_val) {
    @builtin.abort("Expected overflow protection for max value")
  }
  
  // 测试最小值溢出保护
  let result2 = azimuth.add(min_val, -1)
  if (result2 != min_val) {
    @builtin.abort("Expected overflow protection for min value")
  }
  
  // 测试零值加法
  let result3 = azimuth.add(max_val, 0)
  if (result3 != max_val) {
    @builtin.abort("Expected max_val when adding 0")
  }
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  let result1 = azimuth.multiply(3, 4)
  if (result1 != 12) {
    @builtin.abort("Expected 12, got " + result1.to_string())
  }
  
  let result2 = azimuth.multiply(-2, 3)
  if (result2 != -6) {
    @builtin.abort("Expected -6, got " + result2.to_string())
  }
  
  let result3 = azimuth.multiply(0, 5)
  if (result3 != 0) {
    @builtin.abort("Expected 0, got " + result3.to_string())
  }
}

test "multiply_special_cases" {
  // 测试乘法特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试乘以1
  let result1 = azimuth.multiply(42, 1)
  if (result1 != 42) {
    @builtin.abort("Expected 42 when multiplying by 1")
  }
  
  // 测试乘以-1
  let result2 = azimuth.multiply(42, -1)
  if (result2 != -42) {
    @builtin.abort("Expected -42 when multiplying by -1")
  }
  
  // 测试最小值乘以-1的特殊情况
  let result3 = azimuth.multiply(min_val, -1)
  if (result3 != min_val) {
    @builtin.abort("Expected min_val when multiplying min_val by -1")
  }
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  let result1 = azimuth.greet("World")
  if (result1 != "Hello, World!") {
    @builtin.abort("Expected 'Hello, World!', got '" + result1 + "'")
  }
  
  let result2 = azimuth.greet("")
  if (result2 != "Hello, !") {
    @builtin.abort("Expected 'Hello, !', got '" + result2 + "'")
  }
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  let result1 = azimuth.greet("测试")
  if (result1 != "Hello, 测试!") {
    @builtin.abort("Expected 'Hello, 测试!', got '" + result1 + "'")
  }
  
  let result2 = azimuth.greet("123")
  if (result2 != "Hello, 123!") {
    @builtin.abort("Expected 'Hello, 123!', got '" + result2 + "'")
  }
  
  let result3 = azimuth.greet("user@example.com")
  if (result3 != "Hello, user@example.com!") {
    @builtin.abort("Expected 'Hello, user@example.com!', got '" + result3 + "'")
  }
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：矩形周长计算
  let length = 10
  let width = 5
  
  // 周长 = 2 * (length + width)
  let sum = azimuth.add(length, width)
  let perimeter = azimuth.multiply(2, sum)
  
  if (perimeter != 30) {
    @builtin.abort("Expected perimeter 30, got " + perimeter.to_string())
  }
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：简单利息计算
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 利息 = 本金 * 利率 * 年数
  let interest_product = azimuth.multiply(rate, years)
  let interest = azimuth.multiply(principal, interest_product)
  
  // 总额 = 本金 + 利息
  let total = azimuth.add(principal, interest)
  
  if (total != 16000) {
    @builtin.abort("Expected total 16000, got " + total.to_string())
  }
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 测试加法交换律
  let sum1 = azimuth.add(a, b)
  let sum2 = azimuth.add(b, a)
  if (sum1 != sum2) {
    @builtin.abort("Addition commutative property failed")
  }
  
  // 测试乘法交换律
  let product1 = azimuth.multiply(a, b)
  let product2 = azimuth.multiply(b, a)
  if (product1 != product2) {
    @builtin.abort("Multiplication commutative property failed")
  }
}

test "error_handling_validation" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护
  let overflow_add = azimuth.add(max_val, max_val)
  if (overflow_add != max_val) {
    @builtin.abort("Overflow protection failed for addition")
  }
  
  let overflow_mul = azimuth.multiply(max_val, 2)
  if (overflow_mul != max_val) {
    @builtin.abort("Overflow protection failed for multiplication")
  }
  
  // 验证零值特性
  let zero_mul1 = azimuth.multiply(0, max_val)
  let zero_mul2 = azimuth.multiply(0, min_val)
  if (zero_mul1 != 0 || zero_mul2 != 0) {
    @builtin.abort("Zero multiplication property failed")
  }
}