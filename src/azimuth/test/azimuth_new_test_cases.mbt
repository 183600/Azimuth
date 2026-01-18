// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

test "complex_addition_scenarios" {
  // 复杂加法场景测试
  let a = 1000000
  let b = 2000000
  let c = -500000
  
  // 测试大数相加
  let result1 = @azimuth.add(a, b)
  @azimuth.assert_eq(3000000, result1)
  
  // 测试正负数相加
  let result2 = @azimuth.add(a, c)
  @azimuth.assert_eq(500000, result2)
  
  // 测试链式加法
  let result3 = @azimuth.add(@azimuth.add(a, b), c)
  @azimuth.assert_eq(2500000, result3)
}

test "advanced_multiplication_cases" {
  // 高级乘法案例测试
  let a = 12345
  let b = 6789
  let c = -100
  
  // 测试大数相乘
  let result1 = @azimuth.multiply(a, b)
  @azimuth.assert_eq(83810205, result1)
  
  // 测试与负数相乘
  let result2 = @azimuth.multiply(a, c)
  @azimuth.assert_eq(-1234500, result2)
  
  // 测试链式乘法
  let result3 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(-838102050, result3)
}

test "overflow_boundary_conditions" {
  // 溢出边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  let overflow_add = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, overflow_add)
  
  // 测试负数溢出
  let underflow_add = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, underflow_add)
  
  // 测试乘法溢出
  let overflow_mul = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, overflow_mul)
  
  // 测试最小值乘以-1的特殊情况
  let min_times_neg_one = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, min_times_neg_one)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 测试空字符串
  let empty_result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", empty_result)
  
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameThatMightCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let long_result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, " + long_name + "!", long_result)
  
  // 测试包含特殊字符的字符串
  let special_name = "John@Doe#123$%^&*()"
  let special_result = @azimuth.greet(special_name)
  @azimuth.assert_eq_string("Hello, John@Doe#123$%^&*()!", special_result)
  
  // 测试包含Unicode字符的字符串
  let unicode_name = "张三李四🌍"
  let unicode_result = @azimuth.greet(unicode_name)
  @azimuth.assert_eq_string("Hello, 张三李四🌍!", unicode_result)
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 15
  let b = 7
  let c = 3
  
  // 验证加法交换律
  let commutative_add1 = @azimuth.add(a, b)
  let commutative_add2 = @azimuth.add(b, a)
  @azimuth.assert_eq(commutative_add1, commutative_add2)
  
  // 验证乘法交换律
  let commutative_mul1 = @azimuth.multiply(a, b)
  let commutative_mul2 = @azimuth.multiply(b, a)
  @azimuth.assert_eq(commutative_mul1, commutative_mul2)
  
  // 验证分配律
  let distributive1 = @azimuth.multiply(a, @azimuth.add(b, c))
  let distributive2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distributive1, distributive2)
}

test "real_world_financial_calculations" {
  // 实际金融计算测试
  let principal = 10000  // 本金
  let rate = 5          // 利率（百分比）
  let years = 3         // 年数
  
  // 简单利息计算：利息 = 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  
  // 本息和 = 本金 + 利息
  let total = @azimuth.add(principal, interest)
  
  // 验证计算结果：10000 + 10000×5×3 = 160000
  @azimuth.assert_eq(160000, total)
  
  // 测试复利计算（简化版）：本息和 = 本金 × (1 + 利率×年数)
  let compound_total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(160000, compound_total)
}

test "geometry_and_trigonometry" {
  // 几何与三角学计算测试
  let length = 12  // 矩形长
  let width = 8    // 矩形宽
  let radius = 5   // 圆半径
  
  // 计算矩形周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 计算矩形面积：长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 计算圆周长（简化）：2 × π × r ≈ 2 × 3 × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(30, circumference)
  
  // 计算圆面积（简化）：π × r² ≈ 3 × r²
  let circle_area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, circle_area)
}

test "temperature_conversion_calculations" {
  // 温度转换计算测试
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  // 测试室温
  let room_celsius = 20
  let room_fahrenheit = @azimuth.add(@azimuth.multiply(room_celsius, 2), 30)
  @azimuth.assert_eq(70, room_fahrenheit)
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
}