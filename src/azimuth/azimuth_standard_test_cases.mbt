// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含基本功能、边界情况和实际应用场景的测试

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 7))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(5, 5))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "edge_cases" {
  // 测试边界情况
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(0, -1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
}

test "practical_business_calculation" {
  // 实际业务计算场景
  let unit_price = 25
  let quantity = 4
  let tax_rate = 8
  let shipping = 10
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(100, subtotal)
  assert_eq(8, tax)
  assert_eq(118, total)
}

test "complex_operations" {
  // 复杂运算测试
  let x = 10
  let y = 15
  let z = 20
  
  // 测试 (x + y) * z - x
  let result = add(multiply(add(x, y), z), -x)
  assert_eq(490, result)  // (10 + 15) * 20 - 10 = 25 * 20 - 10 = 500 - 10 = 490
  
  // 测试嵌套运算
  let nested = divide_with_ceil(multiply(add(x, multiply(y, 2)), 3), 4)
  assert_eq(23, nested)  // ceil((10 + 15*2) * 3 / 4) = ceil(40 * 3 / 4) = ceil(120/4) = ceil(30) = 30
}