// 标准 MoonBit 测试用例 - 为 azimuth 项目编写
// 包含10个高质量测试用例，覆盖所有主要功能

test "basic_arithmetic_addition" {
  // 基本算术加法测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
}

test "basic_arithmetic_multiplication" {
  // 基本算术乘法测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "greet_function_functionality" {
  // 问候函数功能测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_basic" {
  // 向上取整除法基本测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "subtract_function_basic" {
  // 减法函数基本测试
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(15, @azimuth.subtract(10, -5))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 7
  let b = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(5, boxes_needed)
  
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  @azimuth.assert_eq(30, total_capacity)
  
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(5, remaining_space)
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let initial = 10
  let step1 = @azimuth.add(initial, 5)        // 15
  let step2 = @azimuth.multiply(step1, 2)     // 30
  let step3 = @azimuth.divide_with_ceil(step2, 8)  // 4
  let step4 = @azimuth.add(step3, 1)          // 5
  
  @azimuth.assert_eq(5, step4)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  
  // 单位值测试
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  
  // 除法边界测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试：活动安排
  let participants = 87
  let chairs_per_table = 10
  
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, total_chairs)
  
  let empty_chairs = @azimuth.subtract(total_chairs, participants)
  @azimuth.assert_eq(3, empty_chairs)
  
  // 验证座位充足性
  @azimuth.assert_true(total_chairs >= participants)
}