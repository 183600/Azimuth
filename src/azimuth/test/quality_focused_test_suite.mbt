// 质量导向的测试套件 - 针对 azimuth 库的高质量测试用例
// 包含边界情况、实际应用场景和数学性质验证

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(3, 7), @azimuth.add(7, 3))
  @azimuth.assert_eq(@azimuth.add(-5, 10), @azimuth.add(10, -5))
  @azimuth.assert_eq(@azimuth.add(0, 100), @azimuth.add(100, 0))
  @azimuth.assert_eq(@azimuth.add(-20, -30), @azimuth.add(-30, -20))
  @azimuth.assert_eq(@azimuth.add(999, 1), @azimuth.add(1, 999))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 7
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)  // 5 * (3 + 7) = 5 * 3 + 5 * 7 = 50
}

test "divide_with_ceil_real_world_scenarios" {
  // 测试向上取整除法的实际应用场景
  
  // 场景1：计算需要的车辆数量（每辆车最多载4人）
  let passengers = 15
  let capacity_per_vehicle = 4
  let vehicles_needed = @azimuth.divide_with_ceil(passengers, capacity_per_vehicle)
  @azimuth.assert_eq(4, vehicles_needed)  // ceil(15/4) = 4
  
  // 场景2：计算需要的房间数量（每间房最多住3人）
  let guests = 10
  let capacity_per_room = 3
  let rooms_needed = @azimuth.divide_with_ceil(guests, capacity_per_room)
  @azimuth.assert_eq(4, rooms_needed)  // ceil(10/3) = 4
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, José!", @azimuth.greet("José"))
  @azimuth.assert_eq_string("Hello, Muhammad!", @azimuth.greet("Muhammad"))
  @azimuth.assert_eq_string("Hello, Владимир!", @azimuth.greet("Владимир"))
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

test "complex_business_calculation" {
  // 复杂业务计算：计算订单总价（包括税费和折扣）
  let unit_price = 25
  let quantity = 12
  let tax_rate = 8  // 8%
  let discount_threshold = 200  // 超过200元享受折扣
  let discount_amount = 15
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let total_before_discount = @azimuth.add(subtotal, tax)
  
  // 应用折扣（如果满足条件）
  let final_total = if total_before_discount > discount_threshold {
    @azimuth.add(total_before_discount, -discount_amount)
  } else {
    total_before_discount
  }
  
  // 验证结果：25*12 + ceil(300*8/100) - 15 = 300 + 24 - 15 = 309
  @azimuth.assert_eq(309, final_total)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：计算等差数列的和
  let first_term = 2
  let common_difference = 3
  
  // 手动计算等差数列：2 + 5 + 8 + 11 + 14 + 17 = 57
  let term1 = first_term
  let term2 = @azimuth.add(term1, common_difference)  // 5
  let term3 = @azimuth.add(term2, common_difference)  // 8
  let term4 = @azimuth.add(term3, common_difference)  // 11
  let term5 = @azimuth.add(term4, common_difference)  // 14
  let term6 = @azimuth.add(term5, common_difference)  // 17
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5), term6)
  @azimuth.assert_eq(57, sum)
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  
  // 测试除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试极值情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  
  // 测试大数运算（使用较小数值避免溢出）
  let large_num = 10000
  @azimuth.assert_eq(20000, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(100000000, @azimuth.multiply(large_num, large_num))
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合测试
  
  // 测试组合姓名
  let full_name1 = "John" + " " + "Doe"
  let greeting1 = @azimuth.greet(full_name1)
  @azimuth.assert_eq_string("Hello, John Doe!", greeting1)
  
  let full_name2 = "Mary" + " " + "Smith"
  let greeting2 = @azimuth.greet(full_name2)
  @azimuth.assert_eq_string("Hello, Mary Smith!", greeting2)
  
  let full_name3 = "张" + " " + "三"
  let greeting3 = @azimuth.greet(full_name3)
  @azimuth.assert_eq_string("Hello, 张 三!", greeting3)
  
  // 测试特殊字符处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
}