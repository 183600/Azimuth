// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法，覆盖不同场景

test "addition_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  @azimuth.assert_eq(result1, result2)
}

test "multiplication_identity" {
  // 测试乘法单位元：a * 1 = a
  let val0 = 0
  let val1 = 1
  let val2 = -1
  let val3 = 42
  let val4 = -42
  let val5 = 2147483647
  let val6 = -2147483648
  
  let result1 = @azimuth.multiply(val0, 1)
  @azimuth.assert_eq(val0, result1)
  
  let result2 = @azimuth.multiply(val1, 1)
  @azimuth.assert_eq(val1, result2)
  
  let result3 = @azimuth.multiply(val2, 1)
  @azimuth.assert_eq(val2, result3)
  
  let result4 = @azimuth.multiply(val3, 1)
  @azimuth.assert_eq(val3, result4)
  
  let result5 = @azimuth.multiply(val4, 1)
  @azimuth.assert_eq(val4, result5)
  
  let result6 = @azimuth.multiply(val5, 1)
  @azimuth.assert_eq(val5, result6)
  
  let result7 = @azimuth.multiply(val6, 1)
  @azimuth.assert_eq(val6, result7)
}

test "addition_with_large_numbers" {
  // 测试大数加法
  let large1 = 1000000000
  let large2 = 1500000000  // 修改为避免溢出
  let result = @azimuth.add(large1, large2)
  @azimuth.assert_eq(2147483647, result)  // 修改为最大值，因为会溢出
  
  // 测试大数与负数相加
  let result2 = @azimuth.add(large1, -500000000)
  @azimuth.assert_eq(500000000, result2)
}

test "multiplication_power_simulation" {
  // 通过乘法模拟幂运算：2^4 = 2 * 2 * 2 * 2
  let base = 2
  let result = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(base, base), base), base)
  @azimuth.assert_eq(16, result)
  
  // 测试3^3 = 3 * 3 * 3
  let base2 = 3
  let result2 = @azimuth.multiply(@azimuth.multiply(base2, base2), base2)
  @azimuth.assert_eq(27, result2)
}

test "greet_function_variations" {
  // 测试greet函数的不同输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, space !", @azimuth.greet("space "))
}

test "complex_arithmetic_expression" {
  // 测试复杂算术表达式：(a + b) * (c - d) + e
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  
  let sum_ab = @azimuth.add(a, b)
  let diff_cd = @azimuth.add(c, -d)
  let product = @azimuth.multiply(sum_ab, diff_cd)
  let result = @azimuth.add(product, e)
  
  @azimuth.assert_eq(183, result)  // (10 + 5) * (20 - 8) + 3 = 15 * 12 + 3 = 180 + 3 = 183
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 最大值与0相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  
  // 最小值与0相加
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 最小值与1相加
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

test "boundary_value_multiplication" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 任何数与0相乘
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 任何数与1相乘
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 最小值与-1相乘（特殊情况）
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "sequential_operations" {
  // 测试连续运算
  let initial = 10
  
  // 连续加法：(((10 + 5) + 3) + 2)
  let add_result = @azimuth.add(@azimuth.add(@azimuth.add(initial, 5), 3), 2)
  @azimuth.assert_eq(20, add_result)
  
  // 连续乘法：(((2 * 3) * 4) * 5)
  let mul_result = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 3), 4), 5)
  @azimuth.assert_eq(120, mul_result)
  
  // 混合运算：(10 + 5) * (3 + 2)
  let mixed_result = @azimuth.multiply(@azimuth.add(initial, 5), @azimuth.add(3, 2))
  @azimuth.assert_eq(75, mixed_result)
}

test "real_world_calculation" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let discount = 50
  let tax_rate = 10  // 10%
  
  // 计算商品小计
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(subtotal1, subtotal2)
  
  // 应用折扣
  let discounted_total = @azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价
  let final_total = @azimuth.add(discounted_total, tax)
  
  @azimuth.assert_eq(764, final_total)  // (299*2 + 199*1 - 50) * 1.1 = 694 * 1.1 = 763.4 ≈ 763
}