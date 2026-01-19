// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 123))
}

test "multiply_special_cases" {
  // 测试乘法特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与1和-1相乘
  assert_eq(100, multiply(100, 1))
  assert_eq(-100, multiply(100, -1))
  
  // 测试最小值的特殊处理
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_functionality" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_special_characters" {
  // 测试 greet 函数处理特殊字符
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculations" {
  // 测试复合计算
  // 计算 (2 + 3) * 4 - 5
  let sum = add(2, 3)
  let product = multiply(sum, 4)
  let result = add(product, -5)
  assert_eq(15, result)
}

test "mathematical_properties" {
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

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价：商品价格 × 数量 + 运费
  let price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = multiply(price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(612, total)
}

test "error_handling" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(add(max_val, 100) == max_val)
  assert_true(add(min_val, -100) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  
  // 测试正常情况
  assert_true(add(5, 3) == 8)
  assert_true(multiply(4, 6) == 24)
}