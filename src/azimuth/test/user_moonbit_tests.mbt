// 用户自定义 MoonBit 测试用例
// 包含10个实用的测试用例，使用标准 MoonBit 测试语法

test "integer_addition_variations" {
  // 测试整数加法的各种情况
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
}

test "integer_multiplication_variations" {
  // 测试整数乘法的各种情况
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(64, @azimuth.multiply(8, 8))
}

test "greet_function_edge_cases" {
  // 测试greet函数的边界情况
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "mathematical_expressions" {
  // 测试数学表达式计算
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(16, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(17, result2)
  
  // a + b * c
  let result3 = @azimuth.add(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(11, result3)
}

test "boundary_value_operations" {
  // 测试边界值操作
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "real_world_calculations" {
  // 测试实际应用场景的计算
  // 计算总价：单价 × 数量
  let price = 25
  let quantity = 8
  let total = @azimuth.multiply(price, quantity)
  @azimuth.assert_eq(200, total)
  
  // 计算找零：支付金额 - 总价
  let payment = 300
  let change = @azimuth.add(payment, -total)
  @azimuth.assert_eq(100, change)
  
  // 计算折扣价：原价 - 折扣金额
  let original_price = 150
  let discount = 30
  let discounted_price = @azimuth.add(original_price, -discount)
  @azimuth.assert_eq(120, discounted_price)
}

test "string_processing_scenarios" {
  // 测试字符串处理场景
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet("user123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced  "))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形的面积和周长
  let length = 12
  let width = 8
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 计算三角形的面积（底 × 高 ÷ 2）
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "assertion_function_usage" {
  // 测试断言函数的使用
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  
  // 测试断言函数与实际功能结合
  let add_result = @azimuth.add(15, 25)
  @azimuth.assert_eq(40, add_result)
  @azimuth.assert_true(add_result > 30)
  @azimuth.assert_false(add_result < 30)
  
  let multiply_result = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, multiply_result)
  @azimuth.assert_true(multiply_result >= 40)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a × b = b × a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let c = 5
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律: (a × b) × c = a × (b × c)
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}