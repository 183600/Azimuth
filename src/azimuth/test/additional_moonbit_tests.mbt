// 额外的 MoonBit 测试用例
// 测试 azimuth 包的核心功能和边界情况

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  assert_eq(5, azimuth::add(2, 3))
  assert_eq(0, azimuth::add(0, 0))
  assert_eq(-5, azimuth::add(-2, -3))
  assert_eq(10, azimuth::add(15, -5))
}

test "multiply_function_basic_cases" {
  // 测试 multiply 函数的基本情况
  assert_eq(6, azimuth::multiply(2, 3))
  assert_eq(0, azimuth::multiply(5, 0))
  assert_eq(-6, azimuth::multiply(-2, 3))
  assert_eq(15, azimuth::multiply(-3, -5))
}

test "greet_function_variations" {
  // 测试 greet 函数的各种变化
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
}

test "boundary_value_addition" {
  // 测试边界值的加法运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(-1, azimuth::add(max_val, min_val))
  assert_eq(max_val, azimuth::add(max_val, 1))
}

test "boundary_value_multiplication" {
  // 测试边界值的乘法运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(0, azimuth::multiply(min_val, 0))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c
  let sum = azimuth::add(a, b)
  let product = azimuth::multiply(sum, c)
  assert_eq(900, product)
  
  // 计算 a * b + c
  let product2 = azimuth::multiply(a, b)
  let sum2 = azimuth::add(product2, c)
  assert_eq(230, sum2)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, azimuth::add(-5, -5))
  assert_eq(0, azimuth::add(5, -5))
  assert_eq(-15, azimuth::multiply(-3, 5))
  assert_eq(15, azimuth::multiply(-3, -5))
}

test "large_number_operations" {
  // 测试大数运算
  let large1 = 1000000
  let large2 = 2000000
  
  assert_eq(3000000, azimuth::add(large1, large2))
  assert_eq(2000000000000, azimuth::multiply(large1, large2))
}

test "string_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, @#$%!", azimuth::greet("@#$%"))
  assert_eq_string("Hello,   !", azimuth::greet("  "))
  assert_eq_string("Hello, \n\t!", azimuth::greet("\n\t"))
  assert_eq_string("Hello, 🌍!", azimuth::greet("🌍"))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 15
  let y = 25
  
  // 测试加法交换律: x + y = y + x
  assert_eq(azimuth::add(x, y), azimuth::add(y, x))
  
  // 测试乘法交换律: x * y = y * x
  assert_eq(azimuth::multiply(x, y), azimuth::multiply(y, x))
  
  // 测试分配律: x * (y + z) = x * y + x * z
  let z = 10
  let left = azimuth::multiply(x, azimuth::add(y, z))
  let right = azimuth::add(azimuth::multiply(x, y), azimuth::multiply(x, z))
  assert_eq(left, right)
}