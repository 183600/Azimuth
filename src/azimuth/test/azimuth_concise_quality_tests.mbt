// Azimuth 库的简洁高质量测试用例
// 包含10个标准 MoonBit 测试语法编写的测试用例

test "add_basic_functionality" {
  // 测试加法函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
}

test "multiply_commutative_property" {
  // 测试乘法函数的交换律性质
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(4, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 10))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "greet_multilingual_support" {
  // 测试问候函数的多语言支持
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = @azimuth.divide_with_ceil(@azimuth.add(10, 5), 4)
  @azimuth.assert_eq(4, result2)  // ceil(15/4) = 4
}

test "business_packaging_scenario" {
  // 测试业务场景：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  @azimuth.assert_eq(30, total_capacity)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 7
  let y = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "real_world_application_scenario" {
  // 测试真实世界应用场景：活动安排
  let participants = 87
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(88, total_seats)
  
  // 验证座位充足性
  @azimuth.assert_eq(1, if (total_seats >= participants) { 1 } else { 0 })
}