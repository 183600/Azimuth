// 新的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，补充测试 azimuth 模块的核心功能

test "basic_addition_functionality" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
  assert_eq(10, add(-5, 15))
  assert_eq(-10, add(5, -15))
}

test "basic_multiplication_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(15, multiply(-3, -5))
}

test "addition_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 测试负数溢出
  let min_val = -2147483648
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(min_val, add(-1000000000, -1500000000))
}

test "multiplication_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数乘法溢出
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(max_val, multiply(100000, 30000))
  
  // 测试负数乘法溢出
  assert_eq(min_val, multiply(-46341, 46341))
  assert_eq(min_val, multiply(46341, -46341))
  
  // 测试最小值特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(-1, min_val))
}

test "greet_function_basic_tests" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let test_values = [0, 1, -1, 42, -42, 1000, -1000]
  
  for value in test_values {
    // 加法恒等式
    assert_eq(value, add(value, 0))
    assert_eq(value, add(0, value))
    
    // 乘法恒等式
    assert_eq(value, multiply(value, 1))
    assert_eq(value, multiply(1, value))
    assert_eq(0, multiply(value, 0))
    assert_eq(0, multiply(0, value))
    
    // 负数恒等式（跳过最小值）
    if value != -2147483648 {
      assert_eq(-value, multiply(value, -1))
      assert_eq(-value, multiply(-1, value))
    }
  }
}

test "special_case_handling" {
  // 测试特殊情况处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法中的特殊情况
  assert_eq(max_val, add(max_val, 0))  // 最大值加0
  assert_eq(min_val, add(min_val, 0))  // 最小值加0
  assert_eq(max_val, add(0, max_val))  // 0加最大值
  assert_eq(min_val, add(0, min_val))  // 0加最小值
  
  // 测试乘法中的特殊情况
  assert_eq(0, multiply(max_val, 0))   // 最大值乘0
  assert_eq(0, multiply(min_val, 0))   // 最小值乘0
  assert_eq(0, multiply(0, max_val))   // 0乘最大值
  assert_eq(0, multiply(0, min_val))   // 0乘最小值
  
  assert_eq(max_val, multiply(max_val, 1))  // 最大值乘1
  assert_eq(min_val, multiply(min_val, 1))  // 最小值乘1
  assert_eq(max_val, multiply(1, max_val))  // 1乘最大值
  assert_eq(min_val, multiply(1, min_val))  // 1乘最小值
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算多项式值 f(x) = 2x² + 3x + 1
  let x = 5
  let x_squared = multiply(x, x)  // x²
  let term1 = multiply(2, x_squared)  // 2x²
  let term2 = multiply(3, x)  // 3x
  let polynomial_result = add(add(term1, term2), 1)  // 2x² + 3x + 1
  assert_eq(66, polynomial_result)  // 2*25 + 3*5 + 1 = 50 + 15 + 1 = 66
  
  // 场景2：计算平均值
  let num1 = 85
  let num2 = 92
  let num3 = 78
  let sum = add(add(num1, num2), num3)
  let average = sum / 3  // 使用整数除法
  assert_eq(85, average)  // (85 + 92 + 78) / 3 = 255 / 3 = 85
  
  // 场景3：计算折扣价格
  let original_price = 1200
  let discount_percent = 15  // 15% 折扣
  let discount_amount = multiply(original_price, discount_percent) / 100
  let final_price = add(original_price, -discount_amount)
  assert_eq(1020, final_price)  // 1200 - 1200*15/100 = 1200 - 180 = 1020
}

test "string_edge_cases" {
  // 测试字符串边界情况
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, \$100!", greet("\$100"))
  
  // 测试包含数字和字母混合的字符串
  assert_eq_string("Hello, test123!", greet("test123"))
  assert_eq_string("Hello, 123test!", greet("123test"))
  assert_eq_string("Hello, t1e2s3t!", greet("t1e2s3t"))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello,   !", greet("   "))  // 三个空格
  assert_eq_string("Hello, leading space!", greet(" leading space"))
  assert_eq_string("Hello, trailing space !", greet("trailing space "))
  assert_eq_string("Hello,  both  spaces !", greet(" both  spaces "))
}