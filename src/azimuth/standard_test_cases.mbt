// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
}

test "add_function_overflow_handling" {
  // 测试 add 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试混合溢出
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(5, 3))
}

test "multiply_function_overflow_handling" {
  // 测试 multiply 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, multiply(46341, 46341))
  
  // 测试负数溢出
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 测试特殊边界情况
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "divide_with_ceil_function" {
  // 测试 divide_with_ceil 函数
  // 正常除法（无余数）
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(3, divide_with_ceil(9, 3))
  
  // 向上取整（有余数）
  assert_eq(6, divide_with_ceil(11, 2))
  assert_eq(4, divide_with_ceil(10, 3))
  
  // 负数测试
  assert_eq(-5, divide_with_ceil(-10, 2))
  assert_eq(-4, divide_with_ceil(-11, 3))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算表达式：(a + b) * c - d / e
  let a = 10
  let b = 5
  let c = 3
  let d = 12
  let e = 4
  
  let sum = add(a, b)  // 15
  let product = multiply(sum, c)  // 45
  let quotient = d / e  // 3
  let result = add(product, -quotient)  // 42
  
  assert_eq(42, result)
}

test "real_world_application_test" {
  // 测试实际应用场景：计算折扣价格
  let original_price = 1000
  let discount_percent = 20  // 20% 折扣
  let tax_rate = 8  // 8% 税率
  
  // 计算折扣金额
  let discount_amount = multiply(original_price, discount_percent) / 100  // 200
  // 计算折后价格
  let discounted_price = add(original_price, -discount_amount)  // 800
  // 计算税费
  let tax_amount = multiply(discounted_price, tax_rate) / 100  // 64
  // 计算最终价格
  let final_price = add(discounted_price, tax_amount)  // 864
  
  assert_eq(864, final_price)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试特殊字符
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringThatTestsBoundaryConditions"
  assert_eq_string("Hello, " + long_string + "!", greet(long_string))
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}