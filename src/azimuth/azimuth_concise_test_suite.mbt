// Azimuth 简洁测试套件
// 包含10个标准的 MoonBit 测试用例

test "basic_addition_functionality" {
  // 基本加法功能测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(5, 5))
}

test "basic_subtraction_functionality" {
  // 基本减法功能测试
  assert_eq(3, subtract(8, 5))
  assert_eq(-3, subtract(5, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(15, subtract(5, -10))
}

test "divide_with_ceil_basic_cases" {
  // 向上取整除法基本情况测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
}

test "greet_function_standard_cases" {
  // 问候函数标准情况测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_function_special_cases" {
  // 问候函数特殊情况测试
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_arithmetic_operations" {
  // 复杂算术运算测试
  let a = 10
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(26, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(32, result2)
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 7
  let y = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
}

test "real_world_calculation_scenario" {
  // 真实世界计算场景测试
  // 购物车计算场景
  let item_price = 25
  let quantity = 4
  let tax_rate = 10
  
  // 计算小计
  let subtotal = multiply(item_price, quantity)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(subtotal, tax)
  
  assert_eq(100, subtotal)
  assert_eq(10, tax)
  assert_eq(110, total)
}