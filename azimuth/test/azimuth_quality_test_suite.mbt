// Azimuth 高质量测试套件
// 使用标准的 MoonBit 测试语法，包含10个精选测试用例

test "basic_addition_operations" {
  // 测试基本加法运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
}

test "basic_multiplication_operations" {
  // 测试基本乘法运算
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
}

test "greeting_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "ceil_division_basic_cases" {
  // 测试向上取整除法的基本情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  // 计算 (2 * 3) + (4 * 5) = 6 + 20 = 26
  let part1 = @azimuth.multiply(2, 3)
  let part2 = @azimuth.multiply(4, 5)
  let result = @azimuth.add(part1, part2)
  @azimuth.assert_eq(26, result)
  
  // 计算 10 * (3 + 2) = 10 * 5 = 50
  let sum = @azimuth.add(3, 2)
  let result2 = @azimuth.multiply(10, sum)
  @azimuth.assert_eq(50, result2)
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  // 负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  @azimuth.assert_eq(-5, @azimuth.add(10, -15))
  
  // 负数乘法
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(-20, @azimuth.multiply(4, -5))
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
}

test "boolean_assertions_complex" {
  // 测试复杂的布尔断言
  @azimuth.assert_true(10 > 5 && 3 < 8)
  @azimuth.assert_true(-5 < 0 || 10 > 20)
  @azimuth.assert_true(5 == 5 && 5 != 3)
  
  @azimuth.assert_false(10 < 5 && 3 > 8)
  @azimuth.assert_false(-5 > 0 && 10 < 20)
  @azimuth.assert_false(5 == 3 || 5 != 5)
}

test "mathematical_properties" {
  // 测试数学性质验证
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(123, 456), @azimuth.add(456, 123))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(12, 34), @azimuth.multiply(34, 12))
  
  // 分配律 a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 2
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "practical_application_scenarios" {
  // 测试实际应用场景
  // 场景1：计算订单总价
  let item1_price = 299
  let item2_price = 399
  let quantity = 2
  let total = @azimuth.multiply(@azimuth.add(item1_price, item2_price), quantity)
  @azimuth.assert_eq(1396, total)
  
  // 场景2：计算平均分数（向上取整）
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let total_score = @azimuth.add(@azimuth.add(score1, score2), score3)
  let average = @azimuth.divide_with_ceil(total_score, 3)
  @azimuth.assert_eq(86, average)
  
  // 场景3：生成个性化问候
  let user_name = "Alice"
  let greeting = @azimuth.greet(user_name)
  @azimuth.assert_eq_string("Hello, Alice!", greeting)
}