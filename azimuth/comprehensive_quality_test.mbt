// 综合质量测试用例 - 覆盖核心功能和边界情况

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  // 加法恒等元
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  
  // 乘法恒等元
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(-17, 0))
}

test "associative_laws_verification" {
  // 验证结合律
  let a = 12
  let b = 34
  let c = 56
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_add = @azimuth.add(@azimuth.add(a, b), c)
  let right_add = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_add, right_add)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let left_mult = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mult = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mult, right_mult)
}

test "distributive_law_verification" {
  // 验证分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 13
  let c = 19
  
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "negative_number_comprehensive" {
  // 负数综合测试
  let pos = 25
  let neg = -17
  
  // 加法测试
  @azimuth.assert_eq(8, @azimuth.add(pos, neg))
  @azimuth.assert_eq(-8, @azimuth.add(neg, pos))
  @azimuth.assert_eq(-34, @azimuth.add(neg, neg))
  
  // 乘法测试
  @azimuth.assert_eq(-425, @azimuth.multiply(pos, neg))
  @azimuth.assert_eq(-425, @azimuth.multiply(neg, pos))
  @azimuth.assert_eq(289, @azimuth.multiply(neg, neg))
}

test "divide_with_ceil_real_world" {
  // 向上取整除法的实际应用场景
  // 场景1：计算需要的箱子数量
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  
  // 场景2：计算需要的车辆数量
  let passengers = 23
  let seats_per_vehicle = 7
  let vehicles_needed = @azimuth.divide_with_ceil(passengers, seats_per_vehicle)
  @azimuth.assert_eq(4, vehicles_needed)  // ceil(23/7) = 4
  
  // 场景3：计算需要的页数
  let words = 3500
  let words_per_page = 800
  let pages_needed = @azimuth.divide_with_ceil(words, words_per_page)
  @azimuth.assert_eq(5, pages_needed)  // ceil(3500/800) = 5
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  // 空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符处理
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 中!", @azimuth.greet("中"))
  
  // 特殊字符处理
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello, \t\n\r!", @azimuth.greet("\t\n\r"))
  
  // 长字符串处理
  let long_string = "这是一个很长的字符串用来测试函数的处理能力"
  let expected = "Hello, " + long_string + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_string))
}

test "boundary_value_exhaustive" {
  // 边界值穷举测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值边界测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 零值边界测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}

test "complex_business_logic" {
  // 复杂业务逻辑测试
  // 场景：电商订单计算
  let unit_price = 199
  let quantity = 3
  let shipping_fee = 15
  let discount = -50
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(597, subtotal)
  
  // 加上运费
  let with_shipping = @azimuth.add(subtotal, shipping_fee)
  @azimuth.assert_eq(612, with_shipping)
  
  // 应用折扣
  let final_total = @azimuth.add(with_shipping, discount)
  @azimuth.assert_eq(562, final_total)
  
  // 生成订单确认消息
  let customer_name = "李明"
  let confirmation = @azimuth.greet(customer_name)
  @azimuth.assert_eq_string("Hello, 李明!", confirmation)
}

test "performance_stress_operations" {
  // 性能压力测试
  // 连续运算测试
  let mut result = 1
  
  // 连续乘法计算阶乘（简化版）
  result = @azimuth.multiply(result, 2)  // 2
  result = @azimuth.multiply(result, 3)  // 6
  result = @azimuth.multiply(result, 4)  // 24
  result = @azimuth.multiply(result, 5)  // 120
  @azimuth.assert_eq(120, result)
  
  // 连续加法计算累加
  let mut sum = 0
  sum = @azimuth.add(sum, 100)
  sum = @azimuth.add(sum, 200)
  sum = @azimuth.add(sum, 300)
  sum = @azimuth.add(sum, 400)
  sum = @azimuth.add(sum, 500)
  @azimuth.assert_eq(1500, sum)
  
  // 混合运算压力测试
  let mixed_result = @azimuth.multiply(@azimuth.add(10, 20), @azimuth.add(3, 4))
  @azimuth.assert_eq(210, mixed_result)  // (10+20)*(3+4) = 30*7 = 210
}