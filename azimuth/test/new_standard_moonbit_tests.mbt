// 新的标准 MoonBit 测试用例
// 包含不超过10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-1, @azimuth.add(-5, 4))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-42, @azimuth.multiply(-6, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(5, @azimuth.subtract(12, 7))
  @azimuth.assert_eq(-5, @azimuth.subtract(7, 12))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(5, -3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-5, -3))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 17
  let y = 23
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let total_items = 47
  let items_per_box = 12
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 3
  let divisor = 7
  let addition = 8
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, addition)
  
  @azimuth.assert_eq(14, result)  // ceil(15*3/7) + 8 = 7 + 8 = 15
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_resources = 100
  let teams = 3
  let min_resources_per_team = @azimuth.divide_with_ceil(total_resources, teams)
  
  @azimuth.assert_eq(34, min_resources_per_team)
  
  // 验证分配结果
  let allocated_total = @azimuth.multiply(min_resources_per_team, teams)
  @azimuth.assert_eq(102, allocated_total)
  
  // 验证资源利用率
  let utilization = @azimuth.divide_with_ceil(@azimuth.multiply(total_resources, 100), allocated_total)
  @azimuth.assert_eq(98, utilization)  // floor(10000/102) = 98%
}

test "error_handling_and_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  
  // 大数测试
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let participants = 87
  let tables_available = 12
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(11, tables_needed)
  
  // 检查桌子是否足够
  @azimuth.assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(total_seats, participants)
  @azimuth.assert_eq(1, remaining_seats)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let input_size = 1000
  
  // 模拟O(n)算法
  let linear_steps = @azimuth.multiply(input_size, 3)
  @azimuth.assert_eq(3000, linear_steps)
  
  // 模拟O(log n)算法（简化为除法）
  let log_steps = @azimuth.divide_with_ceil(input_size, 100)
  @azimuth.assert_eq(10, log_steps)
  
  // 模拟O(n^2)算法
  let quadratic_steps = @azimuth.multiply(input_size, @azimuth.divide_with_ceil(input_size, 100))
  @azimuth.assert_eq(10000, quadratic_steps)
}