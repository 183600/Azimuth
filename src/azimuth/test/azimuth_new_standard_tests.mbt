// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，全面覆盖核心功能

test "add_function_positive_numbers" {
  // 测试正数加法
  let result1 = @azimuth.add(3, 7)
  @azimuth.assert_eq(10, result1)
  
  let result2 = @azimuth.add(50, 50)
  @azimuth.assert_eq(100, result2)
  
  let result3 = @azimuth.add(0, 0)
  @azimuth.assert_eq(0, result3)
}

test "add_function_negative_numbers" {
  // 测试负数加法
  let result1 = @azimuth.add(-3, -7)
  @azimuth.assert_eq(-10, result1)
  
  let result2 = @azimuth.add(-5, 5)
  @azimuth.assert_eq(0, result2)
  
  let result3 = @azimuth.add(-50, -50)
  @azimuth.assert_eq(-100, result3)
}

test "multiply_function_various_cases" {
  // 测试乘法函数的各种情况
  let result1 = @azimuth.multiply(3, 5)
  @azimuth.assert_eq(15, result1)
  
  let result2 = @azimuth.multiply(5, 0)
  @azimuth.assert_eq(0, result2)
  
  let result3 = @azimuth.multiply(-3, 5)
  @azimuth.assert_eq(-15, result3)
  
  let result4 = @azimuth.multiply(-3, -5)
  @azimuth.assert_eq(15, result4)
}

test "greet_function_different_inputs" {
  // 测试问候函数的不同输入
  let result1 = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result1)
  
  let result2 = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result2)
  
  let result3 = @azimuth.greet("123")
  @azimuth.assert_eq_string("Hello, 123!", result3)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  @azimuth.assert_eq(result1, result2)
  
  // 乘法交换律
  let result3 = @azimuth.multiply(a, b)
  let result4 = @azimuth.multiply(b, a)
  @azimuth.assert_eq(result3, result4)
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算：(a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 15
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum_ab, c)  // 45
  let result = @azimuth.add(product, -d)  // 30
  
  @azimuth.assert_eq(30, result)
}

test "boundary_value_tests" {
  // 测试边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 边界值加法
  let result1 = @azimuth.add(max_int, 0)
  @azimuth.assert_eq(max_int, result1)
  
  let result2 = @azimuth.add(min_int, 0)
  @azimuth.assert_eq(min_int, result2)
  
  // 边界值乘法
  let result3 = @azimuth.multiply(max_int, 0)
  @azimuth.assert_eq(0, result3)
  
  let result4 = @azimuth.multiply(min_int, 0)
  @azimuth.assert_eq(0, result4)
}

test "practical_scenarios" {
  // 测试实际应用场景
  // 场景1：计算总价（单价 × 数量）
  let unit_price = 25
  let quantity = 4
  let total_price = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(100, total_price)
  
  // 场景2：计算折扣价（原价 - 折扣额）
  let original_price = 120
  let discount = 20
  let discounted_price = @azimuth.add(original_price, -discount)
  @azimuth.assert_eq(100, discounted_price)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  let result1 = @azimuth.greet("test@example.com")
  @azimuth.assert_eq_string("Hello, test@example.com!", result1)
  
  // 测试包含空格的字符串
  let result2 = @azimuth.greet("John Doe")
  @azimuth.assert_eq_string("Hello, John Doe!", result2)
  
  // 测试包含Unicode字符的字符串
  let result3 = @azimuth.greet("测试")
  @azimuth.assert_eq_string("Hello, 测试!", result3)
}

test "chained_operations" {
  // 测试链式操作
  // 计算：a * b + c * d
  let a = 5
  let b = 3
  let c = 2
  let d = 4
  
  let product1 = @azimuth.multiply(a, b)  // 15
  let product2 = @azimuth.multiply(c, d)  // 8
  let result = @azimuth.add(product1, product2)  // 23
  
  @azimuth.assert_eq(23, result)
}