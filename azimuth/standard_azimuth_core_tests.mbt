// 标准 MoonBit 测试用例 - 覆盖 azimuth 库核心功能

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-1, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

test "multiply_function_boundary_cases" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(-12345, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet("!@#$%^&*()"))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, 0))
}

test "complex_calculations" {
  // 测试复合计算
  let step1 = @azimuth.add(10, 5)
  let step2 = @azimuth.multiply(step1, 2)
  let step3 = @azimuth.divide_with_ceil(step2, 3)
  @azimuth.assert_eq(10, step3)  // ((10+5)*2)/3 = 30/3 = 10
  
  let result = @azimuth.add(@azimuth.multiply(3, 4), @azimuth.multiply(5, 2))
  @azimuth.assert_eq(22, result)  // 3*4 + 5*2 = 12 + 10 = 22
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(-2, @azimuth.add(3, -5))
  
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-5, -2))
}

test "real_world_application" {
  // 实际应用场景测试
  // 库存管理：计算需要的箱子数量
  let total_products = 47
  let capacity_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_products, capacity_per_box)
  @azimuth.assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  
  // 价格计算：商品总价
  let unit_price = 25
  let quantity = 8
  let total_price = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(200, total_price)
  
  // 折扣计算：最终价格
  let original_price = 200
  let discount_amount = 30
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(170, final_price)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  // 加法结合律
  let a = 5, b = 7, c = 3
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  // 单位元性质
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
}

test "unicode_and_special_characters" {
  // Unicode 和特殊字符测试
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, 🎉🚀🌟!", @azimuth.greet("🎉🚀🌟"))
  @azimuth.assert_eq_string("Hello, 	
!", @azimuth.greet("	
"))
  @azimuth.assert_eq_string("Hello, \"quotes\"!", @azimuth.greet("\"quotes\""))
}

test "performance_and_large_numbers" {
  // 性能和大数测试
  // 大数运算
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  
  // 连续运算
  let mut accumulator = 1
  accumulator = @azimuth.multiply(accumulator, 2)
  accumulator = @azimuth.multiply(accumulator, 3)
  accumulator = @azimuth.multiply(accumulator, 4)
  accumulator = @azimuth.multiply(accumulator, 5)
  @azimuth.assert_eq(120, accumulator)  // 1*2*3*4*5 = 120
  
  // 大数除法
  @azimuth.assert_eq(100000, @azimuth.divide_with_ceil(100000000, 1000))
  @azimuth.assert_eq(100001, @azimuth.divide_with_ceil(100000001, 1000))
}