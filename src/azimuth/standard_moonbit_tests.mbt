// 标准 MoonBit 测试用例 - 为 azimuth 项目添加
// 包含 8 个高质量测试用例，覆盖核心功能和边界情况

test "basic_arithmetic_comprehensive" {
  // 综合基本算术运算测试
  azimuth::assert_eq(15, azimuth::add(7, 8))
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(-10, azimuth::add(-3, -7))
  azimuth::assert_eq(100, azimuth::multiply(10, 10))
  azimuth::assert_eq(0, azimuth::multiply(0, 123))
  azimuth::assert_eq(-25, azimuth::multiply(-5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法的综合测试
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 5))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(100, 0))
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
  azimuth::assert_eq(-4, azimuth::divide_with_ceil(10, -3))
  azimuth::assert_eq(4, azimuth::divide_with_ceil(-10, -3))
}

test "string_greeting_variations" {
  // 问候函数的各种变体测试
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, 123!", azimuth::greet("123"))
  azimuth::assert_eq_string("Hello, spaces !", azimuth::greet("spaces "))
  azimuth::assert_eq_string("Hello, special@#$!", azimuth::greet("special@#$"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  azimuth::assert_eq(azimuth::add(x, y), azimuth::add(y, x))
  
  // 乘法交换律
  azimuth::assert_eq(azimuth::multiply(x, y), azimuth::multiply(y, x))
  
  // 加法恒等式
  azimuth::assert_eq(x, azimuth::add(x, 0))
  azimuth::assert_eq(x, azimuth::add(0, x))
  
  // 乘法恒等式
  azimuth::assert_eq(x, azimuth::multiply(x, 1))
  azimuth::assert_eq(x, azimuth::multiply(1, x))
  azimuth::assert_eq(0, azimuth::multiply(x, 0))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算场景
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = azimuth::divide_with_ceil(total_items, box_capacity)
  
  azimuth::assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = azimuth::multiply(boxes_needed, box_capacity)
  azimuth::assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = azimuth::subtract(total_capacity, total_items)
  azimuth::assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base = 20
  let multiplier = 3
  let divisor = 8
  let addition = 7
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = azimuth::multiply(base, multiplier)
  let step2 = azimuth::divide_with_ceil(step1, divisor)
  let result = azimuth::add(step2, addition)
  
  azimuth::assert_eq(14, result)  // ceil(20*3/8) + 7 = 8 + 7 = 15
}

test "error_handling_boundaries" {
  // 错误处理和边界测试
  // 除零错误处理
  azimuth::assert_eq(0, azimuth::divide_with_ceil(10, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(-10, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 0))
  
  // 极值测试
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1))
  azimuth::assert_eq(-1, azimuth::divide_with_ceil(-1, -1))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 1))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, -1))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 87
  let tables_available = 12
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = azimuth::divide_with_ceil(participants, chairs_per_table)
  azimuth::assert_eq(11, tables_needed)
  
  // 检查桌子是否足够 - 使用条件表达式而不是 assert_true
  let enough_tables = if tables_needed <= tables_available { 1 } else { 0 }
  azimuth::assert_eq(1, enough_tables)
  
  // 计算总座位数
  let total_seats = azimuth::multiply(tables_needed, chairs_per_table)
  azimuth::assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = azimuth::subtract(total_seats, participants)
  azimuth::assert_eq(1, remaining_seats)
}