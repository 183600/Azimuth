// 综合测试套件 - 覆盖 azimuth 模块的核心功能
// 使用标准的 MoonBit 测试语法

test "add_function_comprehensive" {
  // 测试基本加法运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
  assert_eq(10, add(15, -5))
  
  // 测试与零相加
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
}

test "add_function_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, add(min_val, 1))
}

test "multiply_function_comprehensive" {
  // 测试基本乘法运算
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  
  // 测试与1和-1相乘
  assert_eq(7, multiply(7, 1))
  assert_eq(-7, multiply(7, -1))
}

test "multiply_function_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_function_comprehensive" {
  // 测试 greet 函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, user@123!", greet("user@123"))
  assert_eq_string("Hello, spaced name !", greet("spaced name "))
}

test "complex_mathematical_operations" {
  // 测试复合数学运算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let sum = add(a, b)
  let result = multiply(sum, c)
  assert_eq(900, result)
  
  // a * b + c
  let product = multiply(a, b)
  let result2 = add(product, c)
  assert_eq(230, result2)
}

test "mathematical_properties_validation" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(70, fahrenheit)
  
  // 简单贷款计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  let total = multiply(principal, add(1, multiply(rate, years)))
  assert_eq(160000, total)  // 10000 × (1 + 5×3) = 10000 × 16 = 160000
}

test "assert_functions_validation" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试实际值
  assert_true(add(5, 3) == 8)
  assert_false(multiply(3, 4) == 13)
  assert_eq(12, multiply(3, 4))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}