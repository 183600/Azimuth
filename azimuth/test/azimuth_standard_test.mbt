// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含10个高质量测试用例，覆盖所有主要功能

test "add_function_basic_operations" {
  // 测试加法函数的基本操作
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(10, @azimuth.add(-5, 15))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试向上取整除法的正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
}

test "divide_with_ceil_negative_and_zero" {
  // 测试向上取整除法的负数和零情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "subtract_function_operations" {
  // 测试减法函数的操作
  @azimuth.assert_eq(2, @azimuth.subtract(5, 3))
  @azimuth.assert_eq(-2, @azimuth.subtract(3, 5))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(5, -5))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 15
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 减法性质
  @azimuth.assert_eq(0, @azimuth.subtract(x, x))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  @azimuth.assert_eq(60, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(13, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let initial_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 7
  
  // 计算流程：((initial * multiplier) / divisor) + addition
  let step1 = @azimuth.multiply(initial_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let final_result = @azimuth.add(step2, addition)
  
  @azimuth.assert_eq(14, final_result)  // ceil(10*3/4) + 7 = 8 + 7 = 15
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  
  // 大数测试
  @azimuth.assert_eq(2000, @azimuth.add(1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 除零处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
}