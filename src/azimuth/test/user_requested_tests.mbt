// 用户请求的 MoonBit 测试用例
// 包含10个标准测试用例，测试项目的核心功能

test "add_function_basic_test" {
  // 测试基本的加法功能
  let result = @azimuth.add(10, 20)
  if (result != 30) {
    @builtin.abort("Expected 30, got " + result.to_string())
  }
}

test "add_function_with_negative_numbers" {
  // 测试包含负数的加法
  let result = @azimuth.add(-15, 25)
  if (result != 10) {
    @builtin.abort("Expected 10, got " + result.to_string())
  }
}

test "multiply_function_basic_test" {
  // 测试基本的乘法功能
  let result = @azimuth.multiply(7, 8)
  if (result != 56) {
    @builtin.abort("Expected 56, got " + result.to_string())
  }
}

test "multiply_function_with_zero" {
  // 测试与0相乘的情况
  let result1 = @azimuth.multiply(123, 0)
  let result2 = @azimuth.multiply(0, 456)
  if (result1 != 0 || result2 != 0) {
    @builtin.abort("Multiplication with zero should return 0")
  }
}

test "multiply_function_with_negative_numbers" {
  // 测试包含负数的乘法
  let result1 = @azimuth.multiply(-5, 6)
  let result2 = @azimuth.multiply(7, -3)
  let result3 = @azimuth.multiply(-4, -9)
  
  if (result1 != -30 || result2 != -21 || result3 != 36) {
    @builtin.abort("Negative number multiplication failed")
  }
}

test "greet_function_basic_test" {
  // 测试基本的问候功能
  let result = @azimuth.greet("MoonBit")
  if (result != "Hello, MoonBit!") {
    @builtin.abort("Expected 'Hello, MoonBit!', got '" + result + "'")
  }
}

test "greet_function_with_empty_string" {
  // 测试空字符串的问候
  let result = @azimuth.greet("")
  if (result != "Hello, !") {
    @builtin.abort("Expected 'Hello, !', got '" + result + "'")
  }
}

test "greet_function_with_special_characters" {
  // 测试包含特殊字符的字符串
  let result = @azimuth.greet("Test@123")
  if (result != "Hello, Test@123!") {
    @builtin.abort("Expected 'Hello, Test@123!', got '" + result + "'")
  }
}

test "complex_calculation_test" {
  // 测试复合计算：计算矩形的面积和周长
  let length = 10
  let width = 5
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  if (area != 50 || perimeter != 30) {
    @builtin.abort("Rectangle calculation failed: area=" + area.to_string() + ", perimeter=" + perimeter.to_string())
  }
}

test "boundary_value_test" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加1（应该返回最大值，防止溢出）
  let result1 = @azimuth.add(max_val, 1)
  // 测试最小值减1（应该返回最小值，防止溢出）
  let result2 = @azimuth.add(min_val, -1)
  // 测试最大值与-1相乘
  let result3 = @azimuth.multiply(max_val, -1)
  
  if (result1 != max_val || result2 != min_val || result3 != min_val) {
    @builtin.abort("Boundary value handling failed")
  }
}