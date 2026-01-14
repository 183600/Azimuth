// MoonBit 测试套件
// 为 azimuth 包添加新的测试用例

test "string_concatenation_advanced" {
  // 测试高级字符串拼接功能
  let name1 = "Alice"
  let name2 = "Bob"
  let combined = name1 + " and " + name2
  
  @azimuth.assert_eq_string("Hello, Alice and Bob!", @azimuth.greet(combined))
  
  // 测试包含特殊字符的字符串
  let special_name = "John@ Doe #123"
  @azimuth.assert_eq_string("Hello, John@ Doe #123!", @azimuth.greet(special_name))
}

test "calculation_precision" {
  // 测试计算精度
  let large_num = 1000000
  let small_num = 1
  
  // 大数加小数
  let result1 = @azimuth.add(large_num, small_num)
  @azimuth.assert_eq(1000001, result1)
  
  // 大数乘小数
  let result2 = @azimuth.multiply(large_num, small_num)
  @azimuth.assert_eq(1000000, result2)
  
  // 连续运算
  let result3 = @azimuth.add(@azimuth.multiply(large_num, 2), @azimuth.multiply(small_num, 100))
  @azimuth.assert_eq(2000100, result3)
}

test "negative_number_operations" {
  // 测试负数运算
  let neg1 = -15
  let neg2 = -25
  let pos = 30
  
  // 负数相加
  let result1 = @azimuth.add(neg1, neg2)
  @azimuth.assert_eq(-40, result1)
  
  // 负数相乘
  let result2 = @azimuth.multiply(neg1, neg2)
  @azimuth.assert_eq(375, result2)
  
  // 正负数混合运算
  let result3 = @azimuth.add(neg1, pos)
  @azimuth.assert_eq(15, result3)
  
  let result4 = @azimuth.multiply(neg1, pos)
  @azimuth.assert_eq(-450, result4)
}

test "zero_value_operations" {
  // 测试零值运算
  let zero = 0
  let positive = 42
  let negative = -17
  
  // 零加任何数
  @azimuth.assert_eq(positive, @azimuth.add(zero, positive))
  @azimuth.assert_eq(negative, @azimuth.add(zero, negative))
  @azimuth.assert_eq(zero, @azimuth.add(zero, zero))
  
  // 零乘任何数
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, positive))
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, negative))
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, zero))
}

test "sequential_operations" {
  // 测试连续运算
  let a = 5
  let b = 10
  let c = 15
  let d = 20
  
  // 连续加法
  let sum1 = @azimuth.add(@azimuth.add(a, b), @azimuth.add(c, d))
  @azimuth.assert_eq(50, sum1)
  
  // 连续乘法
  let product1 = @azimuth.multiply(@azimuth.multiply(a, b), @azimuth.multiply(c, d))
  @azimuth.assert_eq(15000, product1)
  
  // 混合运算
  let result = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(c, d))
  @azimuth.assert_eq(350, result)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  let mid_val = 1000000
  
  // 最大值与中等值相加
  let result1 = @azimuth.add(max_val, mid_val)
  @azimuth.assert_eq(max_val, result1)
  
  // 最小值与中等值相加
  let result2 = @azimuth.add(min_val, mid_val)
  @azimuth.assert_eq(-1147483648, result2)
  
  // 最大值与1相乘
  let result3 = @azimuth.multiply(max_val, 1)
  @azimuth.assert_eq(max_val, result3)
  
  // 最小值与1相乘
  let result4 = @azimuth.multiply(min_val, 1)
  @azimuth.assert_eq(min_val, result4)
}

test "unicode_string_handling" {
  // 测试 Unicode 字符串处理
  let chinese_name = "张三"
  let emoji_name = "🌟"
  let mixed_name = "Test测试123🎉"
  
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet(chinese_name))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet(emoji_name))
  @azimuth.assert_eq_string("Hello, Test测试123🎉!", @azimuth.greet(mixed_name))
  
  // 测试空字符串和特殊空白字符
  let whitespace_name = "  \t\n  "
  @azimuth.assert_eq_string("Hello,  	\n  !", @azimuth.greet(whitespace_name))
}

test "mathematical_formulas" {
  // 测试数学公式计算
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 计算矩形周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 计算平均数：(a + b + c) / 3
  let num1 = 85
  let num2 = 90
  let num3 = 95
  let sum = @azimuth.add(@azimuth.add(num1, num2), num3)
  let average = sum / 3
  @azimuth.assert_eq(90, average)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 299
  let item1_qty = 2
  let item2_price = 199
  let item2_qty = 3
  let discount = 50
  let shipping = 20
  
  // 计算商品总价
  let subtotal = @azimuth.add(
    @azimuth.multiply(item1_price, item1_qty),
    @azimuth.multiply(item2_price, item2_qty)
  )
  
  // 计算最终价格
  let final_total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  @azimuth.assert_eq(1174, final_total)
  
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let overflow_result = @azimuth.add(max_val, 100)
  @azimuth.assert_eq(max_val, overflow_result)
  
  let underflow_result = @azimuth.add(min_val, -100)
  @azimuth.assert_eq(min_val, underflow_result)
  
  // 测试乘法溢出
  let mul_overflow = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, mul_overflow)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(123, 123)
  @azimuth.assert_eq_string("success", "success")
}