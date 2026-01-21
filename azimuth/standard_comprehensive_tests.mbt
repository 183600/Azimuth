// 标准 MoonBit 测试用例
// 涵盖 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "multiply_basic_operations" {
  // 测试基本乘法运算
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
}

test "multiply_special_cases" {
  // 测试乘法特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46341))
}

test "greet_function_tests" {
  // 测试问候函数
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(5, -3))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(45, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(53, result2)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法单位元
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  
  // 乘法单位元
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1：计算购物车总价
  let item1 = 199
  let item2 = 299
  let item3 = 99
  let total = @azimuth.add(@azimuth.add(item1, item2), item3)
  @azimuth.assert_eq(597, total)
  
  // 场景2：计算订单数量（单价 × 数量）
  let unit_price = 25
  let quantity = 8
  let order_total = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(200, order_total)
  
  // 场景3：生成个性化问候
  let customer_name = "李四"
  let greeting = @azimuth.greet(customer_name)
  @azimuth.assert_eq_string("Hello, 李四!", greeting)
  
  // 场景4：计算平均分（向上取整）
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let total_score = @azimuth.add(@azimuth.add(score1, score2), score3)
  let average = @azimuth.divide_with_ceil(total_score, 3)
  @azimuth.assert_eq(86, average)
}