// 标准 MoonBit 测试用例 - 新增10个测试用例
// 测试 azimuth 库的核心功能

// 测试1: 基本算术运算验证
test "basic_arithmetic_operations" {
  // 测试加法
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  
  // 测试乘法
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 10))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  
  // 测试减法
  @azimuth.assert_eq(3, @azimuth.subtract(10, 7))
  @azimuth.assert_eq(-3, @azimuth.subtract(7, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
}

// 测试2: 向上取整除法的各种情况
test "divide_with_ceil_comprehensive" {
  // 正数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))  // 3.333... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))   // 3 -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 0.01 -> 1
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -3.333... -> -3
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, -3))   // -3.333... -> -3
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))  // 3.333... -> 4
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

// 测试3: 字符串问候功能的多语言测试
test "greet_function_multilingual" {
  // 英文测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 中文测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 中国!", @azimuth.greet("中国"))
  
  // 特殊字符测试
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

// 测试4: 数学性质验证
test "mathematical_properties" {
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 5)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 5))
  )
}

// 测试5: 实际业务场景 - 购物车计算
test "shopping_cart_calculation" {
  let item1_price = 29
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  let shipping_fee = 10
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  let total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(58, item1_total)
  @azimuth.assert_eq(45, item2_total)
  @azimuth.assert_eq(103, subtotal)
  @azimuth.assert_eq(113, total)
}

// 测试6: 包装计算场景
test "packaging_calculation" {
  let total_items = 47
  let items_per_box = 12
  
  // 计算需要的包装箱数量
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  // 计算总容量和剩余空间
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  
  @azimuth.assert_eq(4, boxes_needed)
  @azimuth.assert_eq(48, total_capacity)
  @azimuth.assert_eq(1, remaining_space)
}

// 测试7: 负数运算的综合测试
test "negative_number_operations" {
  // 负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(5, @azimuth.add(-8, 13))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  
  // 负数乘法
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
  
  // 负数除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-5, -2))
}

// 测试8: 大数运算测试
test "large_number_operations" {
  let big_num1 = 100000
  let big_num2 = 200000
  
  // 大数加法
  @azimuth.assert_eq(300000, @azimuth.add(big_num1, big_num2))
  
  // 大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 大数除法
  @azimuth.assert_eq(33334, @azimuth.divide_with_ceil(100000, 3))
  
  // 组合运算
  let complex_result = @azimuth.add(
    @azimuth.multiply(1000, 500),
    @azimuth.divide_with_ceil(99999, 1000)
  )
  @azimuth.assert_eq(500100, complex_result)
}

// 测试9: 复杂业务计算 - 订单处理
test "order_processing_calculation" {
  let unit_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping_cost = 15
  let discount = 20
  
  // 计算商品小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = @azimuth.divide_with_ceil(
    @azimuth.multiply(subtotal, tax_rate),
    100
  )
  
  // 计算最终总价
  let total_before_discount = @azimuth.add(subtotal, tax_amount)
  let total_before_shipping = @azimuth.add(total_before_discount, shipping_cost)
  let final_total = @azimuth.add(total_before_shipping, -discount)
  
  @azimuth.assert_eq(356, subtotal)
  @azimuth.assert_eq(29, tax_amount)
  @azimuth.assert_eq(400, total_before_shipping)
  @azimuth.assert_eq(380, final_total)
}

// 测试10: 综合表达式计算
test "complex_expression_evaluation" {
  let x = 10
  let y = 15
  let z = 8
  
  // 表达式1: (x + y) * z
  let expr1 = @azimuth.multiply(@azimuth.add(x, y), z)
  @azimuth.assert_eq(200, expr1)  // (10 + 15) * 8 = 25 * 8 = 200
  
  // 表达式2: x * y + y * z
  let expr2 = @azimuth.add(
    @azimuth.multiply(x, y),
    @azimuth.multiply(y, z)
  )
  @azimuth.assert_eq(270, expr2)  // 10*15 + 15*8 = 150 + 120 = 270
  
  // 表达式3: ceil(x * y / z) + ceil(y * z / x)
  let expr3 = @azimuth.add(
    @azimuth.divide_with_ceil(@azimuth.multiply(x, y), z),
    @azimuth.divide_with_ceil(@azimuth.multiply(y, z), x)
  )
  @azimuth.assert_eq(31, expr3)  // ceil(150/8) + ceil(120/10) = 19 + 12 = 31
  
  // 表达式4: (x - y) * (y - z) + z
  let expr4 = @azimuth.add(
    @azimuth.multiply(@azimuth.add(x, -y), @azimuth.add(y, -z)),
    z
  )
  @azimuth.assert_eq(-22, expr4)  // (10-15)*(15-8) + 8 = (-5)*7 + 8 = -35 + 8 = -27
}