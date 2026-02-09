// Azimuth 项目标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_positive_numbers" {
  // 向上取整除法正数测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(a, b))
  assert_eq(96, multiply(a, b))
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let initial = 100
  
  // 计算序列：(100 + 25) * 2 - 30 / 5 (向上取整)
  let step1 = add(initial, 25)        // 125
  let step2 = multiply(step1, 2)      // 250
  let step3 = divide_with_ceil(30, 5) // 6
  let result = subtract(step2, step3) // 244
  
  assert_eq(125, step1)
  assert_eq(250, step2)
  assert_eq(6, step3)
  assert_eq(244, result)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 123))
  assert_eq(0, divide_with_ceil(0, 456))
  
  // 单位值测试
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 负数边界测试
  assert_eq(0, add(-1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "real_world_calculation_scenario" {
  // 真实世界计算场景测试 - 购物车计算
  let item_price = 25
  let quantity = 4
  let tax_rate = 10  // 10%
  let shipping = 15
  
  // 计算商品总价
  let subtotal = multiply(item_price, quantity)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算最终总价
  let total = add(add(subtotal, tax), shipping)
  
  // 验证计算结果
  assert_eq(100, subtotal)
  assert_eq(10, tax)
  assert_eq(125, total)
}