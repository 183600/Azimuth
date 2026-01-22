// 新的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-1, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
}

test "multiply_function_boundary_values" {
  // 测试乘法函数的边界值
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(1, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))  // 10/3 = 3.33... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3)) // -10/3 = -3.33... -> -3
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))    // 除数为0时返回0
}

test "greet_function_various_scenarios" {
  // 测试问候函数的各种场景
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 7
  let b = 13
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))        // 加法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a)) // 乘法交换律
}

test "mathematical_associative_properties" {
  // 测试数学结合律性质
  let x = 5
  let y = 8
  let z = 12
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))  // 加法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), z), @azimuth.multiply(x, @azimuth.multiply(y, z))) // 乘法结合律
}

test "distributive_property_verification" {
  // 验证分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 9
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)  // 4 * (6 + 9) = 4 * 6 + 4 * 9 = 60
}

test "real_world_business_calculation" {
  // 真实世界业务计算场景
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping_cost = 15
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  // 计算税费（向上取整）
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_cost = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping_cost)
  
  @azimuth.assert_eq(200, subtotal)     // 25 * 8 = 200
  @azimuth.assert_eq(16, tax_amount)    // ceil(200 * 8 / 100) = ceil(16) = 16
  @azimuth.assert_eq(231, total_cost)   // 200 + 16 + 15 = 231
}

test "complex_arithmetic_expressions" {
  // 复杂算术表达式测试
  let x = 10
  let y = 15
  let z = 20
  
  // 测试 (x + y) * z
  let result1 = @azimuth.multiply(@azimuth.add(x, y), z)
  @azimuth.assert_eq(500, result1)  // (10 + 15) * 20 = 25 * 20 = 500
  
  // 测试 x * y + z
  let result2 = @azimuth.add(@azimuth.multiply(x, y), z)
  @azimuth.assert_eq(170, result2)  // 10 * 15 + 20 = 150 + 20 = 170
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合测试
  let first_name = "Alice"
  let last_name = "Smith"
  let age = "30"
  
  // 构建完整信息字符串
  let full_info = first_name + " " + last_name + " (Age: " + age + ")"
  let greeting = @azimuth.greet(full_info)
  
  @azimuth.assert_eq_string("Hello, Alice Smith (Age: 30)!", greeting)
  
  // 测试特殊字符组合
  let special_chars = "Test@123#$%"
  let special_greeting = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, Test@123#$%!", special_greeting)
}