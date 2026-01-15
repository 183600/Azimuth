// 新的标准 MoonBit 测试用例
// 包含10个精心设计的测试用例，覆盖各种场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-24, @azimuth.multiply(4, -6))
  @azimuth.assert_eq(36, @azimuth.multiply(-4, -9))
}

test "string_processing_comprehensive" {
  // 测试字符串处理的综合情况
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, special@#$%!", @azimuth.greet("special@#$%"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
}

test "boundary_value_comprehensive" {
  // 测试边界值的综合情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 最小值测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 边界值交互
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "mathematical_properties_verification" {
  // 测试数学性质的验证
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1：计算折扣价格
  let original_price = 200
  let discount_percent = 15
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(170, final_price)
  
  // 场景2：计算简单利息
  let principal = 5000
  let rate = 8
  let time = 3
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time)) / 100
  @azimuth.assert_eq(1200, interest)
  
  // 场景3：计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 78
  let average = @azimuth.add(@azimuth.add(score1, score2), score3) / 3
  @azimuth.assert_eq(84, average)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -100) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -2) == min_val)
  
  // 测试特殊值处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
}

test "performance_related_operations" {
  // 测试性能相关的操作
  // 大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(2000000000, @azimuth.multiply(large_num1, large_num2))
  
  // 连续运算测试
  let result = @azimuth.add(@azimuth.multiply(10, 20), @azimuth.multiply(30, 40))
  @azimuth.assert_eq(1400, result)
  
  // 嵌套运算测试
  let nested = @azimuth.multiply(@azimuth.add(5, 10), @azimuth.add(3, 7))
  @azimuth.assert_eq(150, nested)
  
  // 复合运算测试
  let complex = @azimuth.add(@azimuth.multiply(@azimuth.add(2, 3), @azimuth.add(4, 5)), 10)
  @azimuth.assert_eq(47, complex)
}

test "complex_calculation_patterns" {
  // 测试复杂计算模式
  // 模式1：等差数列求和 1 + 2 + ... + 10 = 55
  let n = 10
  let arithmetic_sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, arithmetic_sum)
  
  // 模式2：等比数列求和 2 + 4 + 8 + 16 = 30
  let geometric_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, geometric_sum)
  
  // 模式3：平方和 1² + 2² + 3² + 4² = 30
  let sum_of_squares = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.multiply(4, 4))
  @azimuth.assert_eq(30, sum_of_squares)
  
  // 模式4：阶乘计算 5! = 120
  let factorial = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial)
}

test "conditional_logic_simulation" {
  // 测试条件逻辑的模拟
  let x = 15
  let y = 20
  let z = 25
  
  // 模拟 if-else 逻辑
  let condition1 = x < y
  let result1 = if condition1 { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
  @azimuth.assert_eq(35, result1)
  
  let condition2 = x > y
  let result2 = if condition2 { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
  @azimuth.assert_eq(300, result2)
  
  // 模拟嵌套条件逻辑
  let condition3 = x < y
  let condition4 = y < z
  let result3 = if condition3 {
    if condition4 { @azimuth.add(@azimuth.add(x, y), z) } else { @azimuth.multiply(x, y) }
  } else {
    @azimuth.add(x, z)
  }
  @azimuth.assert_eq(60, result3)
}

test "data_conversion_and_validation" {
  // 测试数据转换和验证
  // 字符串到数字的模拟转换
  let num_str = "42"
  let converted_num = @azimuth.add(@azimuth.multiply(4, 10), 2)
  @azimuth.assert_eq(42, converted_num)
  
  // 布尔值到字符串的模拟转换
  let bool_true = true
  let bool_false = false
  let true_str = "true"
  let false_str = "false"
  
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet(true_str))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet(false_str))
  
  // 温度转换：摄氏度转华氏度（简化版）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 时间单位转换：小时转分钟
  let hours = 3
  let minutes = @azimuth.multiply(hours, 60)
  @azimuth.assert_eq(180, minutes)
  
  // 距离单位转换：公里转米
  let kilometers = 5
  let meters = @azimuth.multiply(kilometers, 1000)
  @azimuth.assert_eq(5000, meters)
}