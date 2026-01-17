// 用户综合测试用例
// 测试 azimuth 模块的核心功能和边界情况

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(-2, @azimuth.add(5, -7))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, 特殊字符@#$!", @azimuth.greet("特殊字符@#$"))
}

test "integer_overflow_boundary" {
  // 整数溢出边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "mathematical_laws_verification" {
  // 数学定律验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "real_world_financial_calculation" {
  // 实际金融计算场景测试
  let principal = 10000  // 本金
  let rate = 5           // 利率 5%
  let years = 3          // 年数
  
  // 简单利息计算: 本金 + 本金 * 利率 * 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
  
  // 复利计算（简化版）: 本金 * (1 + 利率 * 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let compound_total = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(160000, compound_total)
}

test "error_handling_validation" {
  // 错误处理验证测试
  // 测试断言函数本身
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试边界条件的正确处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  // 计算: (a + b) * (c + d) - e * f
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  let e = 8
  let f = 12
  
  let sum1 = @azimuth.add(a, b)        // 30
  let sum2 = @azimuth.add(c, d)        // 20
  let product1 = @azimuth.multiply(sum1, sum2)  // 600
  let product2 = @azimuth.multiply(e, f)        // 96
  let result = @azimuth.add(product1, -product2) // 504
  
  @azimuth.assert_eq(504, result)
}

test "special_value_combinations" {
  // 特殊值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 测试零的运算特性
  @azimuth.assert_eq(0, @azimuth.multiply(zero, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(zero, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(zero, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(zero, min_val))
  
  // 测试一的运算特性
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, one))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, one))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, neg_one))
  
  // 测试边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
}

test "performance_related_operations" {
  // 性能相关操作测试
  // 测试大量连续运算
  let base = 1000
  let count = 100
  
  // 计算: base + base*2 + base*3 + ... + base*count
  // 使用公式: base * (1 + 2 + ... + count) = base * count * (count + 1) / 2
  let sum = @azimuth.multiply(base, @azimuth.multiply(count, @azimuth.add(count, 1)) / 2)
  @azimuth.assert_eq(5050000, sum)
  
  // 测试连续乘法: 2^8 = 256
  let power_of_2 = @azimuth.multiply(
    @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)),
    @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2))
  )
  @azimuth.assert_eq(256, power_of_2)
}

test "comprehensive_application_scenario" {
  // 综合应用场景测试：电商订单计算
  let item_price = 299
  let quantity = 3
  let shipping = 15
  let tax_rate = 10  // 10%
  let discount_threshold = 500
  let discount_rate = 10  // 10%
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)  // 897
  
  // 计算折扣（如果超过阈值）
  let discount_amount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100  // 89
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = @azimuth.add(subtotal, -discount_amount)  // 808
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate) / 100  // 80
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax_amount), shipping)  // 903
  
  @azimuth.assert_eq(903, final_total)
  
  // 测试小额订单（无折扣）
  let small_price = 50
  let small_quantity = 2
  let small_subtotal = @azimuth.multiply(small_price, small_quantity)  // 100
  let small_tax = @azimuth.multiply(small_subtotal, tax_rate) / 100  // 10
  let small_final = @azimuth.add(@azimuth.add(small_subtotal, small_tax), shipping)  // 125
  
  @azimuth.assert_eq(125, small_final)
}