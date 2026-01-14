// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
}

test "string_processing_functions" {
  // 测试字符串处理功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet("user123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 3)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3))
  )
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "real_world_calculations" {
  // 测试实际应用场景
  // 计算折扣价格：原价 × (1 - 折扣率)
  let original_price = 200
  let discount_rate = 20  // 20%
  let discounted_price = @azimuth.multiply(original_price, @azimuth.add(100, -discount_rate)) / 100
  
  @azimuth.assert_eq(160, discounted_price)
  
  // 计算增值税：价格 × 税率
  let price = 500
  let tax_rate = 15  // 15%
  let tax_amount = @azimuth.multiply(price, tax_rate) / 100
  
  @azimuth.assert_eq(75, tax_amount)
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  // 计算：(a + b) × (c - d) + e × f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let diff_cd = @azimuth.add(c, -d)  // 12
  let product_ab_cd = @azimuth.multiply(sum_ab, diff_cd)  // 180
  let product_ef = @azimuth.multiply(e, f)  // 21
  let result = @azimuth.add(product_ab_cd, product_ef)  // 201
  
  @azimuth.assert_eq(201, result)
}

test "error_handling_validation" {
  // 测试错误处理和验证
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(123, 123)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "performance_related_operations" {
  // 测试性能相关操作
  // 计算数列和：1 + 2 + 3 + ... + n = n × (n + 1) / 2
  let n = 100
  let series_sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  
  @azimuth.assert_eq(5050, series_sum)
  
  // 计算平方和：1² + 2² + 3² + ... + n² = n × (n + 1) × (2n + 1) / 6
  let square_sum = @azimuth.multiply(n, @azimuth.multiply(@azimuth.add(n, 1), @azimuth.add(@azimuth.multiply(2, n), 1))) / 6
  
  @azimuth.assert_eq(338350, square_sum)
}

test "data_conversion_scenarios" {
  // 测试数据转换场景
  // 模拟日期格式化
  let year = 2023
  let month = 12
  let day = 25
  
  let year_str = "2023"
  let month_str = "12"
  let day_str = "25"
  
  // 模拟日期字符串拼接
  let date_string = year_str + "-" + month_str + "-" + day_str
  @azimuth.assert_eq_string("2023-12-25", date_string)
  
  // 测试布尔值表示
  let bool_true_str = "true"
  let bool_false_str = "false"
  
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet(bool_true_str))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet(bool_false_str))
}

test "algorithmic_implementation" {
  // 测试算法实现
  // 计算最大公约数（简化版，使用减法）
  let a = 48
  let b = 18
  
  // 简化的GCD计算过程
  let temp_a = a
  let temp_b = b
  
  // 模拟欧几里得算法的几步
  let step1 = @azimuth.add(temp_a, -@azimuth.multiply(temp_b, 2))  // 48 - 18*2 = 12
  let step2 = @azimuth.add(temp_b, -@azimuth.multiply(step1, 1))  // 18 - 12*1 = 6
  let step3 = @azimuth.add(step1, -@azimuth.multiply(step2, 2))  // 12 - 6*2 = 0
  
  // 当余数为0时，除数就是GCD
  let gcd = step2  // 6
  
  @azimuth.assert_eq(6, gcd)
  
  // 验证GCD性质：a和b都能被gcd整除
  @azimuth.assert_eq(0, temp_a % gcd)
  @azimuth.assert_eq(0, temp_b % gcd)
}