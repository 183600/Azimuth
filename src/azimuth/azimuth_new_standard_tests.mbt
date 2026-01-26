// Azimuth 库的新增标准测试用例
// 包含10个高质量的测试，覆盖核心功能和边界情况

test "binary_operations_commutativity" {
  // 测试二进制运算的交换律性质
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 验证具体数值
  assert_eq(59, add(42, 17))
  assert_eq(714, multiply(42, 17))
}

test "divide_with_ceil_mixed_signs" {
  // 测试混合符号的向上取整除法
  // 正数除以负数
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(-2, divide_with_ceil(5, -3))
  assert_eq(-1, divide_with_ceil(2, -3))
  
  // 负数除以正数
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
}

test "greet_function_unicode_variations" {
  // 测试各种 Unicode 字符的问候功能
  assert_eq_string("Hello, 👋!", greet("👋"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 🌺🌸🌼!", greet("🌺🌸🌼"))
  assert_eq_string("Hello, ßøü!", greet("ßøü"))
}

test "complex_nested_calculations" {
  // 测试复杂的嵌套计算
  let a = 5
  let b = 12
  let c = 7
  let d = 3
  
  // 计算: (a + b) * (c - d) + a * d
  let result1 = add(multiply(add(a, b), add(c, -d)), multiply(a, d))
  assert_eq(83, result1)  // (5+12)*(7-3) + 5*3 = 17*4 + 15 = 68 + 15 = 83
  
  // 计算: a * b + c * d - (a + c)
  let result2 = add(add(multiply(a, b), multiply(c, d)), add(a, -c))
  assert_eq(69, result2)  // 5*12 + 7*3 - (5+7) = 60 + 21 - 12 = 69
}

test "extreme_boundary_conditions" {
  // 测试极端边界条件
  // 接近零的值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  
  // 大数与小数的组合
  assert_eq(1000001, add(1000000, 1))
  assert_eq(999999, add(1000000, -1))
  assert_eq(2000000, multiply(1000000, 2))
}

test "mathematical_series_summation" {
  // 测试数学级数求和
  // 等差数列: 3, 7, 11, 15, 19 (公差为4)
  let term1 = 3
  let term2 = add(term1, 4)
  let term3 = add(term2, 4)
  let term4 = add(term3, 4)
  let term5 = add(term4, 4)
  
  let series_sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(55, series_sum)  // 3 + 7 + 11 + 15 + 19 = 55
  
  // 验证等差数列求和公式: n/2 * (first + last)
  let formula_sum = divide_with_ceil(multiply(5, add(term1, term5)), 2)
  assert_eq(55, formula_sum)
}

test "practical_business_calculations" {
  // 测试实际业务计算场景
  // 计算订单总价：商品单价 × 数量 + 税费 - 折扣
  let unit_price = 89
  let quantity = 6
  let tax_rate = 8  // 8%
  let discount = 45
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算最终总价
  let final_total = add(add(subtotal, tax_amount), -discount)
  
  assert_eq(534, subtotal)      // 89 * 6 = 534
  assert_eq(43, tax_amount)     // ceil(534 * 8 / 100) = ceil(42.72) = 43
  assert_eq(532, final_total)   // 534 + 43 - 45 = 532
}

test "string_processing_with_numbers" {
  // 测试包含数字的字符串处理
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, v1.0.0!", greet("v1.0.0"))
  assert_eq_string("Hello, test123!", greet("test123"))
  assert_eq_string("Hello, 42!", greet("42"))
  assert_eq_string("Hello, 3.14159!", greet("3.14159"))
}

test "error_resilience_operations" {
  // 测试错误恢复能力
  // 除数为零的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 零参与的运算
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(0, multiply(-2147483648, 0))
  assert_eq(2147483647, add(2147483647, 0))
  assert_eq(-2147483648, add(-2147483648, 0))
}

test "algorithmic_pattern_recognition" {
  // 测试算法模式识别
  // 模拟计算阶乘: 5! = 5 * 4 * 3 * 2 * 1 = 120
  let factorial_5 = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial_5)
  
  // 模拟计算平方数序列: 1², 2², 3², 4², 5²
  let square_1 = multiply(1, 1)
  let square_2 = multiply(2, 2)
  let square_3 = multiply(3, 3)
  let square_4 = multiply(4, 4)
  let square_5 = multiply(5, 5)
  
  let squares_sum = add(add(add(add(square_1, square_2), square_3), square_4), square_5)
  assert_eq(55, squares_sum)  // 1 + 4 + 9 + 16 + 25 = 55
  
  // 验证平方和公式: n(n+1)(2n+1)/6，其中n=5
  let formula_result = divide_with_ceil(multiply(multiply(5, 6), 11), 6)
  assert_eq(55, formula_result)  // 5*6*11/6 = 55
}