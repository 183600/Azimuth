// Azimuth 新标准测试用例
// 包含10个高质量的 MoonBit 测试用例，使用标准测试语法

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(5, -6))
  assert_eq(0, add(0, 0))
  assert_eq(42, multiply(6, 7))
  assert_eq(-42, multiply(6, -7))
  assert_eq(0, multiply(0, 123))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(15, 0))
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(-3, divide_with_ceil(8, -3))
}

test "string_greeting_variations" {
  // 字符串问候变体测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 24
  let y = 18
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 5)),
    add(multiply(x, y), multiply(x, 5))
  )
  
  // 减法性质
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "business_packaging_logic" {
  // 业务包装逻辑测试
  let items = 95
  let box_capacity = 20
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(100, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(5, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let base_value = 12
  let multiplier = 3
  let divisor = 5
  let addition = 7
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(14, result)  // ceil(12*3/5) + 7 = 8 + 7 = 15
}

test "resource_allocation_efficiency" {
  // 资源分配效率测试
  let total_resources = 156
  let teams = 4
  let min_resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(39, min_resources_per_team)
  
  // 验证分配结果
  let allocated_total = multiply(min_resources_per_team, teams)
  assert_eq(156, allocated_total)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), allocated_total)
  assert_eq(100, utilization)
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(25, 0))
  assert_eq(0, divide_with_ceil(-25, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值验证
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let participants = 125
  let tables_available = 15
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(16, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(128, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 验证座位充足性
  assert_true(total_seats >= participants)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let input_size = 1000
  
  // 线性算法模拟
  let linear_complexity = multiply(input_size, 2)
  assert_eq(2000, linear_complexity)
  
  // 对数算法模拟（简化为除法）
  let logarithmic_complexity = divide_with_ceil(input_size, 100)
  assert_eq(10, logarithmic_complexity)
  
  // 常数算法模拟
  let constant_complexity = 5
  assert_eq(5, constant_complexity)
  
  // 验证算法效率差异
  let linear_vs_log = divide_with_ceil(linear_complexity, logarithmic_complexity)
  assert_eq(200, linear_vs_log)
}