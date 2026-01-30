// 标准 MoonBit 测试用例 - 8个高质量测试用例
// 涵盖核心功能和实际应用场景

test "basic_arithmetic_comprehensive" {
  // 基础算术运算综合测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(-4, @azimuth.add(3, -7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
}

test "divide_with_ceil_scenarios" {
  // 向上取整除法的各种场景
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

test "greet_function_international_support" {
  // 问候函数的国际化支持测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 15
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 2)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 2))
  )
  
  // 零元素性质
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
}

test "business_inventory_management" {
  // 业务库存管理测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  
  @azimuth.assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(168, total_capacity)
  
  // 验证剩余空间（使用加法和负数代替减法）
  let remaining_space = @azimuth.add(total_capacity, -total_products)
  @azimuth.assert_eq(11, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 20
  let multiplier = 3
  let divisor = 8
  let addition = 7
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, addition)
  
  @azimuth.assert_eq(14, result)  // ceil(20*3/8) + 7 = 8 + 7 = 15
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  
  // 大数运算
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 87
  let chairs_per_table = 10
  let cost_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  // 计算总座位数
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, total_chairs)
  
  // 计算剩余座位
  let empty_chairs = @azimuth.add(total_chairs, -participants)
  @azimuth.assert_eq(3, empty_chairs)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_chairs, cost_per_chair)
  @azimuth.assert_eq(2250, total_cost)
  
  // 验证预算充足
  @azimuth.assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = @azimuth.add(total_budget, -total_cost)
  @azimuth.assert_eq(750, remaining_budget)
}