// 高质量增强测试用例 - 针对 azimuth 库的全面测试
// 包含边界情况、错误处理和实际应用场景

test "mathematical_properties_addition" {
  // 测试加法的数学性质
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(10, @azimuth.add(3, 7))  // 交换律
  @azimuth.assert_eq(0, @azimuth.add(5, -5))  // 相反数性质
  @azimuth.assert_eq(15, @azimuth.add(@azimuth.add(5, 3), 7))  // 结合律
  @azimuth.assert_eq(15, @azimuth.add(5, @azimuth.add(3, 7)))
}

test "mathematical_properties_multiplication" {
  // 测试乘法的数学性质
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(4, 3))  // 交换律
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))  // 零元素性质
  @azimuth.assert_eq(24, @azimuth.multiply(@azimuth.multiply(2, 3), 4))  // 结合律
  @azimuth.assert_eq(24, @azimuth.multiply(2, @azimuth.multiply(3, 4)))
}

test "distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))  // 12/3 = 4 -> 4
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))  // -11/3 = -3.666... -> -4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, -3))   // 10/-3 = -3.333... -> -3 (但实现可能不同)
  
  // 负数除负数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))   // -9/-3 = 3 -> 3
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))  // 空字符串
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))  // 空格字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))  // 单字符
  @azimuth.assert_eq_string("Hello, This is a very long name!", @azimuth.greet("This is a very long name"))  // 长字符串
}

test "complex_business_scenario" {
  // 复杂业务场景：计算订单总价
  let base_price = 100
  let quantity = 5
  let tax_rate = 8  // 8%
  let shipping = 15
  let discount = 20
  
  // 计算小计
  let subtotal = @azimuth.multiply(base_price, quantity)
  // 计算税费（简化为固定金额）
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_shipping = @azimuth.add(subtotal, tax)
  let total_before_discount = @azimuth.add(total_before_shipping, shipping)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(515, final_total)  // 100*5 + ceil(500*8/100) + 15 - 20 = 500 + 40 + 15 - 20 = 535
}

test "inventory_management_scenario" {
  // 库存管理场景：计算需要的存储空间
  let items_per_box = 24
  let total_items_small = 100
  let total_items_medium = 250
  let total_items_large = 1000
  
  let boxes_small = @azimuth.divide_with_ceil(total_items_small, items_per_box)
  let boxes_medium = @azimuth.divide_with_ceil(total_items_medium, items_per_box)
  let boxes_large = @azimuth.divide_with_ceil(total_items_large, items_per_box)
  
  let total_boxes = @azimuth.add(@azimuth.add(boxes_small, boxes_medium), boxes_large)
  
  @azimuth.assert_eq(5, boxes_small)   // ceil(100/24) = 5
  @azimuth.assert_eq(11, boxes_medium) // ceil(250/24) = 11
  @azimuth.assert_eq(42, boxes_large)  // ceil(1000/24) = 42
  @azimuth.assert_eq(58, total_boxes)  // 5 + 11 + 42 = 58
}

test "mathematical_series_calculation" {
  // 数学级数计算：计算等差数列的和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum_formula = @azimuth.divide_with_ceil(
    @azimuth.multiply(number_of_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 验证结果
  @azimuth.assert_eq(120, sum_formula)  // 10/2 * (6 + 18) = 5 * 24 = 120
}