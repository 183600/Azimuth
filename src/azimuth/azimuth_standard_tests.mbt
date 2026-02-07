// Azimuth 项目标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(60, 40))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(7, 0))
  assert_eq(-21, multiply(-7, 3))
  assert_eq(25, multiply(-5, -5))
}

test "division_with_ceil_basic" {
  // 测试向上取整除法基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(6, 2))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 8))
}

test "division_with_ceil_edge_cases" {
  // 测试向上取整除法边界情况
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-11, -3))
}

test "subtraction_functionality" {
  // 测试减法功能
  assert_eq(7, subtract(10, 3))
  assert_eq(-7, subtract(3, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(5, -15))
}

test "greet_function_standard_cases" {
  // 测试问候函数标准情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, User!", greet("User"))
}

test "greet_function_edge_cases" {
  // 测试问候函数边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, A!", greet("A"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let base = 10
  let multiplier = 3
  let offset = 7
  let divisor = 4
  
  let result = divide_with_ceil(add(multiply(base, multiplier), offset), divisor)
  assert_eq(11, result)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 8
  let y = 12
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 单位元性质
  assert_eq(add(x, 0), x)
  assert_eq(multiply(x, 1), x)
}

test "real_world_calculation" {
  // 测试真实世界计算场景
  // 模拟购物计算：商品价格 × 数量 + 税费
  let price = 25
  let quantity = 4
  let tax_rate = 10
  
  let subtotal = multiply(price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax)
  
  assert_eq(100, subtotal)
  assert_eq(10, tax)
  assert_eq(110, total)
}