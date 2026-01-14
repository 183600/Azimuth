// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_basic" {
  // 测试基本的加法功能
  let result = @azimuth.add(10, 20)
  if (result != 30) {
    @builtin.abort("Test failed")
  }
}

test "add_function_with_negative" {
  // 测试包含负数的加法
  let result = @azimuth.add(15, -5)
  if (result != 10) {
    @builtin.abort("Test failed")
  }
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  let result = @azimuth.multiply(7, 8)
  if (result != 56) {
    @builtin.abort("Test failed")
  }
}

test "multiply_function_with_zero" {
  // 测试与零相乘的情况
  let result1 = @azimuth.multiply(123, 0)
  let result2 = @azimuth.multiply(0, 456)
  if (result1 != 0 || result2 != 0) {
    @builtin.abort("Multiplication with zero should return 0")
  }
}

test "greet_function_standard" {
  // 测试标准的问候功能
  let result = @azimuth.greet("MoonBit")
  if (result != "Hello, MoonBit!") {
    @builtin.abort("Test failed")
  }
}

test "greet_function_with_spaces" {
  // 测试包含空格的字符串
  let result = @azimuth.greet("John Doe")
  if (result != "Hello, John Doe!") {
    @builtin.abort("Test failed")
  }
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：矩形的面积和周长
  let length = 10
  let width = 5
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  if (area != 50) {
    @builtin.abort("Expected area 50")
  }
  
  if (perimeter != 30) {
    @builtin.abort("Expected perimeter 30")
  }
}

test "overflow_protection_add" {
  // 测试加法的溢出保护
  let max_val = 2147483647
  let result = @azimuth.add(max_val, 100)
  
  // 应该返回最大值而不是溢出
  if (result != max_val) {
    @builtin.abort("Expected overflow protection to return max value")
  }
}

test "overflow_protection_multiply" {
  // 测试乘法的溢出保护
  let max_val = 2147483647
  let result = @azimuth.multiply(max_val, 2)
  
  // 应该返回最大值而不是溢出
  if (result != max_val) {
    @builtin.abort("Expected overflow protection to return max value")
  }
}

test "financial_calculation_simple" {
  // 测试简单的金融计算场景
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 计算简单利息：本金 + (本金 × 利率 × 年数)
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  // 1000 + (1000 × 5 × 3) = 16000
  if (total != 16000) {
    @builtin.abort("Expected total 16000")
  }
}