// Azimuth 项目增强 MoonBit 测试用例集
// 包含10个全面的测试用例，使用标准 MoonBit 测试语法

test "number_series_summation" {
  // 测试数列求和：计算等差数列 3, 7, 11, 15, 19 的和
  // 使用公式：n × (首项 + 末项) / 2
  let n = 5
  let first = 3
  let last = 19
  
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(55, sum)  // 5 × (3 + 19) / 2 = 55
  
  // 验证手动计算结果
  let manual_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(3, 7), 11), 15), 19)
  @azimuth.assert_eq(sum, manual_sum)
}

test "financial_compound_interest" {
  // 测试金融复利计算（简化版）
  // 本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate_percent = 8  // 8% 年利率
  let years = 5
  
  let growth_factor = @azimuth.add(100, @azimuth.multiply(rate_percent, years))
  let total_amount = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(140000, total_amount)  // 10000 × (100 + 8×5) = 10000 × 140 = 140000
  
  // 计算利息收益
  let interest = @azimuth.add(total_amount, -principal)
  @azimuth.assert_eq(130000, interest)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result1 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result1)
  
  // 测试包含空格的字符串
  let spaced_name = "  John  Doe  "
  let result2 = @azimuth.greet(spaced_name)
  @azimuth.assert_eq_string("Hello,   John  Doe  !", result2)
  
  // 测试数字字符串
  let numeric_string = "1234567890"
  let result3 = @azimuth.greet(numeric_string)
  @azimuth.assert_eq_string("Hello, 1234567890!", result3)
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  let x = 42
  
  // 加法恒等：x + 0 = x
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等：x × 1 = x
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  
  // 乘法零元：x × 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, x))
  
  // 负数性质：x + (-x) = 0
  @azimuth.assert_eq(0, @azimuth.add(x, -x))
}

test "nested_calculation_scenarios" {
  // 测试嵌套计算场景：计算表达式 ((a + b) × (c - d)) + e
  let a = 15
  let b = 25
  let c = 40
  let d = 10
  let e = 100
  
  // 分步计算
  let sum_ab = @azimuth.add(a, b)           // 15 + 25 = 40
  let diff_cd = @azimuth.add(c, -d)         // 40 - 10 = 30
  let product = @azimuth.multiply(sum_ab, diff_cd)  // 40 × 30 = 1200
  let result = @azimuth.add(product, e)     // 1200 + 100 = 1300
  
  @azimuth.assert_eq(1300, result)
  
  // 验证分配律：(a + b) × (c - d) = a × (c - d) + b × (c - d)
  let a_term = @azimuth.multiply(a, diff_cd)  // 15 × 30 = 450
  let b_term = @azimuth.multiply(b, diff_cd)  // 25 × 30 = 750
  let distributive = @azimuth.add(@azimuth.add(a_term, b_term), e)
  @azimuth.assert_eq(result, distributive)
}

test "boundary_overflow_conditions" {
  // 测试边界溢出条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试最小值加法溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试最大值与负数相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值与-1相乘的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "practical_shipping_calculation" {
  // 测试实际运费计算场景
  let base_weight = 1000  // 基础重量（克）
  let additional_weight = 500  // 附加重量（克）
  let base_rate = 10  // 基础费率
  let additional_rate = 5  // 附加费率
  let handling_fee = 15  // 处理费
  
  // 计算基础运费：基础重量 × 基础费率
  let base_shipping = @azimuth.multiply(base_weight, base_rate) / 1000
  @azimuth.assert_eq(10, base_shipping)
  
  // 计算附加运费：附加重量 × 附加费率
  let additional_shipping = @azimuth.multiply(additional_weight, additional_rate) / 1000
  @azimuth.assert_eq(2, additional_shipping)
  
  // 计算总运费
  let total_shipping = @azimuth.add(@azimuth.add(base_shipping, additional_shipping), handling_fee)
  @azimuth.assert_eq(27, total_shipping)
}

test "algorithmic_pattern_recognition" {
  // 测试算法模式识别：计算斐波那契数列第10项（简化版）
  // F(1) = 1, F(2) = 1, F(n) = F(n-1) + F(n-2)
  // 我们手动计算前10项：1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  
  let f1 = 1
  let f2 = 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  let f9 = @azimuth.add(f7, f8)  // 34
  let f10 = @azimuth.add(f8, f9)  // 55
  
  @azimuth.assert_eq(55, f10)
  
  // 验证黄金比例近似：F(10) / F(9) ≈ 1.618
  let ratio_approx = @azimuth.multiply(f10, 1000) / f9
  @azimuth.assert_eq(1617, ratio_approx)  // 55 × 1000 / 34 ≈ 1617
}

test "data_validation_scenarios" {
  // 测试数据验证场景
  // 测试断言函数的组合使用
  let test_value = @azimuth.add(17, 23)
  
  // 验证计算结果
  @azimuth.assert_eq(40, test_value)
  @azimuth.assert_true(test_value > 30)
  @azimuth.assert_false(test_value < 30)
  @azimuth.assert_true(test_value == 40)
  
  // 测试字符串验证
  let test_string = @azimuth.greet("Validator")
  @azimuth.assert_eq_string("Hello, Validator!", test_string)
  @azimuth.assert_true(@azimuth.add(20, 20) == 40)
  
  // 测试边界条件验证
  let boundary_test = @azimuth.multiply(1000, 2000)
  @azimuth.assert_eq(2000000, boundary_test)
  @azimuth.assert_true(boundary_test > 1000000)
  @azimuth.assert_false(boundary_test < 1000000)
}

test "complex_business_logic" {
  // 测试复杂业务逻辑：计算订单折扣和最终价格
  let item_price = 299
  let quantity = 4
  let discount_threshold = 1000
  let discount_rate = 15  // 15% 折扣
  let tax_rate = 8  // 8% 税率
  let shipping = 25
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  @azimuth.assert_eq(1196, subtotal)  // 299 × 4 = 1196
  
  // 判断是否满足折扣条件
  let discount_amount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  @azimuth.assert_eq(179, discount_amount)  // 1196 × 15 / 100 = 179（整数除法）
  
  // 计算折后价格
  let discounted_total = @azimuth.add(subtotal, -discount_amount)
  @azimuth.assert_eq(1017, discounted_total)  // 1196 - 179 = 1017
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate) / 100
  @azimuth.assert_eq(81, tax_amount)  // 1017 × 8 / 100 = 81（整数除法）
  
  // 计算最终价格
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax_amount), shipping)
  @azimuth.assert_eq(1123, final_total)  // 1017 + 81 + 25 = 1123
}