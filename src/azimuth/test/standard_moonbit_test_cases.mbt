// 标准 MoonBit 测试用例 - 10个高质量测试
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(7, @azimuth.subtract(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证测试
  let a = 12
  let b = 18
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a × b = b × a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证具体数值
  @azimuth.assert_eq(30, @azimuth.add(a, b))
  @azimuth.assert_eq(216, @azimuth.multiply(a, b))
}

test "boundary_value_testing" {
  // 边界值测试
  // 零值边界
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  
  // 单位值边界
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(5, 1))
  
  // 负数边界
  @azimuth.assert_eq(-1, @azimuth.add(0, -1))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-5, 5))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial = 100
  let factor = 3
  let offset = 25
  let divisor = 7
  
  // 计算流水线
  let step1 = @azimuth.multiply(initial, factor)  // 100 * 3 = 300
  let step2 = @azimuth.add(step1, offset)         // 300 + 25 = 325
  let step3 = @azimuth.subtract(step2, 50)        // 325 - 50 = 275
  let step4 = @azimuth.divide_with_ceil(step3, divisor)  // ceil(275/7) = 40
  
  @azimuth.assert_eq(300, step1)
  @azimuth.assert_eq(325, step2)
  @azimuth.assert_eq(275, step3)
  @azimuth.assert_eq(40, step4)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let principal = 10000
  let rate = 5
  let years = 2
  
  // 简单利息计算
  let interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate), 100)
  let total = @azimuth.add(principal, interest)
  
  // 复利计算（第二年）
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(total, rate), 100)
  let final_total = @azimuth.add(total, year2_interest)
  
  @azimuth.assert_eq(500, interest)
  @azimuth.assert_eq(10500, total)
  @azimuth.assert_eq(525, year2_interest)
  @azimuth.assert_eq(11025, final_total)
}

test "error_handling_division_by_zero" {
  // 除零错误处理测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, 0))
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(2, @azimuth.add(5, -3))
  @azimuth.assert_eq(-8, @azimuth.add(-5, -3))
  
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  
  @azimuth.assert_eq(-8, @azimuth.subtract(-5, 3))
  @azimuth.assert_eq(8, @azimuth.subtract(5, -3))
  
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-5, -3))
}

test "real_world_packaging_problem" {
  // 真实世界包装问题测试
  let total_items = 47
  let capacity_per_box = 12
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_items, capacity_per_box)
  
  // 计算总容量
  let total_capacity = @azimuth.multiply(boxes_needed, capacity_per_box)
  
  // 计算剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  
  @azimuth.assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  @azimuth.assert_eq(48, total_capacity)
  @azimuth.assert_eq(1, remaining_space)
}

test "associative_properties_verification" {
  // 结合律性质验证测试
  let a = 5
  let b = 8
  let c = 12
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let add_left = @azimuth.add(@azimuth.add(a, b), c)
  let add_right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(add_left, add_right)
  
  // 乘法结合律: (a × b) × c = a × (b × c)
  let mult_left = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let mult_right = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(mult_left, mult_right)
  
  // 验证具体数值
  @azimuth.assert_eq(25, add_left)
  @azimuth.assert_eq(480, mult_left)
}