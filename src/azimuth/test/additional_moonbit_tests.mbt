// 额外的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，测试 azimuth 模块的各种功能

test "large_number_operations" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  // 大数相加
  let sum_result = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(3000000, sum_result)
  
  // 大数相乘
  let product_result = @azimuth.multiply(1000, 1000)
  @azimuth.assert_eq(1000000, product_result)
  
  // 大数与边界值组合
  let max_val = 2147483647
  let large_sum = @azimuth.add(max_val, large_num1)
  @azimuth.assert_eq(max_val, large_sum)  // 应该溢出到最大值
}

test "boundary_condition_combinations" {
  // 测试边界条件组合
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 最大值与各种值的组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, one))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, neg_one))
  
  // 最小值与各种值的组合
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, one))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, neg_one))
  
  // 连续边界运算
  let chain_result = @azimuth.add(@azimuth.multiply(@azimuth.add(max_val, min_val), one), max_val)
  @azimuth.assert_eq(max_val, chain_result)
}

test "ecommerce_business_logic" {
  // 测试电商业务逻辑
  let product_price = 199
  let quantity = 3
  let discount_threshold = 500
  let discount_rate = 10
  let shipping_fee = 20
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(product_price, quantity)
  
  // 计算折扣（如果超过阈值）
  let discount_amount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = @azimuth.add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax_amount), shipping_fee)
  
  @azimuth.assert_eq(1076, final_total)
  
  // 测试小额订单（无折扣）
  let small_price = 50
  let small_quantity = 2
  let small_subtotal = @azimuth.multiply(small_price, small_quantity)
  let small_final = @azimuth.add(@azimuth.add(small_subtotal, @azimuth.multiply(small_subtotal, tax_rate) / 100), shipping_fee)
  
  @azimuth.assert_eq(127, small_final)
}

test "advanced_string_processing" {
  // 测试高级字符串处理
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./"
  let result1 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-={}[]|\\:;\"'<>?,.//!", result1)
  
  // 测试包含Unicode字符的字符串
  let unicode_text = "Hello世界🌍"
  let result2 = @azimuth.greet(unicode_text)
  @azimuth.assert_eq_string("Hello, Hello世界🌍!", result2)
  
  // 测试包含数字和字母混合的字符串
  let alphanumeric = "User2023ProjectX"
  let result3 = @azimuth.greet(alphanumeric)
  @azimuth.assert_eq_string("Hello, User2023ProjectX!", result3)
  
  // 测试包含空格的字符串
  let spaced_text = "  Leading and trailing spaces  "
  let result4 = @azimuth.greet(spaced_text)
  @azimuth.assert_eq_string("Hello,   Leading and trailing spaces  !", result4)
}

test "mathematical_formula_validation" {
  // 测试数学公式验证
  let a = 6
  let b = 4
  
  // 验证分配律：a × (b + c) = a × b + a × c
  let c = 5
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 验证平方差公式：(a + b) × (a - b) = a² - b²
  let sum = @azimuth.add(a, b)
  let diff = @azimuth.add(a, -b)
  let product = @azimuth.multiply(sum, diff)
  let square_diff = @azimuth.add(@azimuth.multiply(a, a), @azimuth.multiply(-b, b))
  @azimuth.assert_eq(product, square_diff)
  
  // 验证结合律：(a + b) + c = a + (b + c)
  let d = 3
  let left_associative = @azimuth.add(@azimuth.add(a, b), c)
  let right_associative = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_associative, right_associative)
}

test "performance_calculations" {
  // 测试性能相关计算
  let base_value = 100
  let iterations = 1000
  
  // 模拟累加计算：base_value + base_value*2 + ... + base_value*iterations
  // 使用公式：base_value × (1 + 2 + ... + iterations) = base_value × iterations × (iterations + 1) / 2
  let sum_formula = @azimuth.multiply(base_value, @azimuth.multiply(iterations, @azimuth.add(iterations, 1)) / 2)
  
  // 验证前几项的计算
  let sum_first_10 = @azimuth.multiply(base_value, @azimuth.multiply(10, 11) / 2)
  @azimuth.assert_eq(5500, sum_first_10)
  
  // 验证总和
  @azimuth.assert_eq(500500, sum_formula)
  
  // 模拟指数增长计算
  let growth_factor = 2
  let periods = 8
  let initial_amount = 100
  
  // 计算：initial_amount × growth_factor^periods
  let exponential_growth = @azimuth.multiply(initial_amount, @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)))))
  @azimuth.assert_eq(25600, exponential_growth)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试各种溢出情况的保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊边界情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 测试断言函数的容错性
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_false(@azimuth.add(5, 3) == 9)
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  @azimuth.assert_eq_string("Hello, ErrorTest!", @azimuth.greet("ErrorTest"))
  
  // 测试复杂条件下的错误处理
  let complex_result = @azimuth.add(@azimuth.multiply(@azimuth.add(max_val, -max_val), 100), max_val)
  @azimuth.assert_eq(max_val, complex_result)
}

test "data_conversion_scenarios" {
  // 测试数据转换场景
  // 模拟整数到字符串的转换（通过greet函数）
  let number = 2024
  let number_as_string = "2024"
  let converted_greeting = @azimuth.greet(number_as_string)
  
  @azimuth.assert_eq_string("Hello, 2024!", converted_greeting)
  
  // 模拟布尔值到字符串的转换
  let true_as_string = "true"
  let false_as_string = "false"
  
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet(true_as_string))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet(false_as_string))
  
  // 模拟浮点数的简化表示（使用整数表示小数部分）
  let pi_approx_string = "3.14"
  
  @azimuth.assert_eq_string("Hello, 3.14!", @azimuth.greet(pi_approx_string))
  
  // 模拟日期时间字符串
  let date_string = "2024-01-15 14:30:00"
  @azimuth.assert_eq_string("Hello, 2024-01-15 14:30:00!", @azimuth.greet(date_string))
}

test "algorithm_implementation" {
  // 测试算法实现
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let factorial_result = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  @azimuth.assert_eq(120, factorial_result)
  
  // 计算斐波那契数列第10项（简化版）
  // F(10) = F(9) + F(8) = 34 + 21 = 55
  let fib_8 = 21
  let fib_9 = 34
  let fib_10 = @azimuth.add(fib_9, fib_8)
  @azimuth.assert_eq(55, fib_10)
  
  // 计算最大公约数（使用欧几里得算法的简化版本）
  // gcd(48, 18) = gcd(18, 48 % 18) = gcd(18, 12) = gcd(12, 6) = 6
  let a = 48
  let b = 18
  let remainder = a % b  // 48 % 18 = 12
  let next_remainder = b % remainder  // 18 % 12 = 6
  let gcd = next_remainder  // 简化版本，假设已经找到最大公约数
  @azimuth.assert_eq(6, gcd)
  
  // 计算幂：2^10 = 1024
  let power_result = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)))), 4)
  @azimuth.assert_eq(1024, power_result)
}

test "real_world_simulation" {
  // 测试真实世界模拟场景
  // 场景1：计算汽车旅程
  let distance = 300  // 公里
  let fuel_efficiency = 15  // 每升汽油行驶的公里数
  let fuel_price = 8  // 每升汽油价格
  
  // 计算所需燃料
  let fuel_needed = distance / fuel_efficiency  // 20升
  let fuel_cost = @azimuth.multiply(fuel_needed, fuel_price)  // 160元
  
  @azimuth.assert_eq(20, fuel_needed)
  @azimuth.assert_eq(160, fuel_cost)
  
  // 场景2：计算BMI（简化版，避免浮点数）
  let weight_kg = 70  // 体重（公斤）
  let height_cm = 175  // 身高（厘米）
  let height_m_squared = @azimuth.multiply(height_cm / 100, height_cm / 100)  // 身高米的平方
  let bmi_index = @azimuth.multiply(weight_kg, 10000) / height_m_squared  // BMI × 100（避免小数）
  
  @azimuth.assert_eq(22857, bmi_index)  // 实际BMI为22.857，这里存储为22857
  
  // 场景3：计算贷款月供（简化版）
  let loan_amount = 100000  // 贷款金额
  let annual_rate = 6  // 年利率百分比
  let loan_years = 20  // 贷款年限
  
  // 简化的月供计算：总金额 / 总月数
  let total_months = @azimuth.multiply(loan_years, 12)
  let total_interest = @azimuth.multiply(loan_amount, @azimuth.multiply(annual_rate, loan_years)) / 100
  let total_payment = @azimuth.add(loan_amount, total_interest)
  let monthly_payment = total_payment / total_months
  
  @azimuth.assert_eq(240, total_months)
  @azimuth.assert_eq(120000, total_interest)
  @azimuth.assert_eq(220000, total_payment)
  @azimuth.assert_eq(916, monthly_payment)  // 简化计算的月供
}