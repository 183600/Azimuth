// Azimuth 附加测试用例
// 使用标准的 MoonBit 测试语法
// 覆盖各种边界情况和实际应用场景

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, -3))
}

test "greet_unicode_and_special_chars" {
  // 测试包含Unicode字符和特殊字符的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, \"quote\"!", @azimuth.greet("\"quote\""))
}

test "mathematical_properties" {
  // 测试数学运算的性质
  let a = 15
  let b = 7
  let c = 3
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律（分步计算）
  let sum_ab = @azimuth.add(a, b)
  let sum_abc = @azimuth.add(sum_ab, c)
  let sum_bc = @azimuth.add(b, c)
  let sum_bca = @azimuth.add(a, sum_bc)
  @azimuth.assert_eq(sum_abc, sum_bca)
}

test "extreme_values" {
  // 测试极值情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 边界值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "mixed_operations" {
  // 测试混合运算
  let a = 10
  let b = 3
  let c = 2
  
  // 先加后乘
  let sum = @azimuth.add(a, b)
  let result1 = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(26, result1)  // (10 + 3) * 2 = 26
  
  // 先乘后加
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(32, result2)  // 10 * 3 + 2 = 32
  
  // 使用除法的复合运算
  let dividend = @azimuth.add(a, b)
  let quotient = @azimuth.divide_with_ceil(dividend, c)
  @azimuth.assert_eq(7, quotient)  // ceil((10 + 3) / 2) = 7
}

test "error_conditions" {
  // 测试错误条件和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出情况
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-3, @azimuth.add(0, -3))
}

test "practical_scenarios" {
  // 测试实际应用场景
  // 场景1：计算购物车总价（带溢出保护）
  let item1_price = 199
  let item2_price = 299
  let item3_price = 599
  let total = @azimuth.add(@azimuth.add(item1_price, item2_price), item3_price)
  @azimuth.assert_eq(1097, total)
  
  // 场景2：计算订单总价（数量×单价）
  let quantity = 7
  let unit_price = 85
  let order_total = @azimuth.multiply(quantity, unit_price)
  @azimuth.assert_eq(595, order_total)
  
  // 场景3：计算所需包装箱数量（向上取整）
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(5, boxes_needed)
  
  // 场景4：生成个性化问候
  let customer_name = "张三"
  let greeting = @azimuth.greet(customer_name)
  @azimuth.assert_eq_string("Hello, 张三!", greeting)
}