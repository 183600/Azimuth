// 标准 MoonBit 测试用例
// 为 azimuth 库添加的高质量测试用例

test "add_function_comprehensive_test" {
  // 加法函数综合测试
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
}

test "multiply_function_boundary_test" {
  // 乘法函数边界测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 999))
  @azimuth.assert_eq(999, @azimuth.multiply(1, 999))
  @azimuth.assert_eq(-999, @azimuth.multiply(-1, 999))
  @azimuth.assert_eq(999, @azimuth.multiply(-1, -999))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "divide_with_ceil_precision_test" {
  // 向上取整除法精度测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
}

test "subtract_function_verification_test" {
  // 减法函数验证测试
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
  @azimuth.assert_eq(15, @azimuth.subtract(10, -5))
  @azimuth.assert_eq(-15, @azimuth.subtract(-10, 5))
}

test "greet_function_unicode_test" {
  // 问候函数Unicode测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, empty!", @azimuth.greet(""))
}

test "mathematical_properties_test" {
  // 数学性质测试
  let x = 42
  let y = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 3)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3))
  )
}

test "business_logic_packaging_test" {
  // 业务逻辑：包装计算测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(items, capacity_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, capacity_per_box)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, items)
  @azimuth.assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline_test" {
  // 复杂计算流水线测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, addition)
  
  @azimuth.assert_eq(13, result)  // ceil(10*3/4) + 5 = 8 + 5 = 13
}

test "error_handling_boundary_test" {
  // 错误处理边界测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值验证
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}

test "real_world_application_test" {
  // 真实世界应用场景测试：活动安排
  let participants = 87
  let tables = 8
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  // 计算可用座位数
  let available_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, available_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(available_seats, participants)
  @azimuth.assert_eq(3, remaining_seats)
  
  // 验证座位充足性
  @azimuth.assert_true(available_seats >= participants)
}