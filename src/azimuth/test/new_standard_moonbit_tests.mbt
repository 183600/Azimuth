// 新增的 MoonBit 标准测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_commutative_property" {
  // 测试加法交换律
  @azimuth.assert_eq(@azimuth.add(12, 8), @azimuth.add(8, 12))
  @azimuth.assert_eq(@azimuth.add(-5, 15), @azimuth.add(15, -5))
  @azimuth.assert_eq(@azimuth.add(0, 100), @azimuth.add(100, 0))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 7
  let c = 3
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(7, 3))  // 2.333... -> 3
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(14, 3)) // 4.666... -> 5
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 2))  // 2 -> 2
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 0.01 -> 1
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8  // 8%
  let discount = 25
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), -discount)
  
  @azimuth.assert_eq(270, subtotal)  // 45 * 6 = 270
  @azimuth.assert_eq(22, tax)       // ceil(270 * 8 / 100) = ceil(21.6) = 22
  @azimuth.assert_eq(267, total)    // 270 + 22 - 25 = 267
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2, @azimuth.add(-8, 10))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
}

test "edge_case_zero_operations" {
  // 测试零的边界运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(15, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -25))
  @azimuth.assert_eq(15, @azimuth.add(15, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(25, 0))
}

test "real_world_packaging_calculation" {
  // 实际应用：包装计算
  let total_products = 47
  let products_per_box = 12
  let cost_per_box = 8
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_products, products_per_box)
  // 计算总成本
  let total_cost = @azimuth.multiply(boxes_needed, cost_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  @azimuth.assert_eq(32, total_cost)   // 4 * 8 = 32
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 3
  let common_diff = 4
  let num_terms = 6
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let sum = @azimuth.divide_with_ceil(
    @azimuth.multiply(num_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(num_terms - 1, common_diff))),
    2
  )
  
  @azimuth.assert_eq(78, sum)  // 6/2 * (6 + 20) = 3 * 26 = 78
}

test "string_greeting_with_special_characters" {
  // 测试包含特殊字符的字符串问候
  @azimuth.assert_eq_string("Hello, C++!", @azimuth.greet("C++"))
  @azimuth.assert_eq_string("Hello, a@b.c!", @azimuth.greet("a@b.c"))
  @azimuth.assert_eq_string("Hello, A&B!", @azimuth.greet("A&B"))
  @azimuth.assert_eq_string("Hello, $100!", @azimuth.greet("$100"))
}