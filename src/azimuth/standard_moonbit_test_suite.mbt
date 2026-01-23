// 标准 MoonBit 测试套件
// 包含10个高质量的测试用例，覆盖 azimuth 库的核心功能

test "add_function_boundary_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, add(1, -1))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_zero_properties" {
  // 测试乘法函数的零性质
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(-10, 0))
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(5, -5))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let x = 10
  let y = 20
  let z = 5
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(150, result1)
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(205, result2)
  
  // 测试 (x / z) + y（使用向上取整）
  let result3 = add(divide_with_ceil(x, z), y)
  assert_eq(22, result3)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(a, add(a, 0))
  
  // 乘法单位元
  assert_eq(a, multiply(a, 1))
}

test "real_world_business_calculations" {
  // 实际业务计算场景
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  assert_eq(200, subtotal)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  assert_eq(16, tax)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  assert_eq(231, total)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
}

test "large_number_operations" {
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(999999, add(500000, 499999))
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "string_and_number_integration" {
  // 测试字符串和数字的集成使用
  let age = 25
  let name = "Alice"
  
  // 创建包含年龄的问候
  let age_string = "Age: " + age.to_string()
  let greeting = greet(name)
  
  assert_eq_string("Hello, Alice!", greeting)
  
  // 验证年龄计算
  let age_in_decades = divide_with_ceil(age, 10)
  assert_eq(3, age_in_decades)
  
  // 计算未来年龄
  let future_age = add(age, 5)
  assert_eq(30, future_age)
}