// 标准 MoonBit 测试套件 - 为 Azimuth 项目添加
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  // 测试加法、乘法、减法和向上取整除法的基本功能
  
  // 加法测试
  assert_eq(7, add(3, 4))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  
  // 乘法测试
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-12, multiply(-3, 4))
  
  // 向上取整除法测试
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 减法测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  // 验证基本的数学定律和性质
  
  let x = 15
  let y = 8
  let z = 3
  
  // 加法交换律: a + b = b + a
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律: a * (b + c) = a * b + a * c
  assert_eq(
    multiply(x, add(y, z)),
    add(multiply(x, y), multiply(x, z))
  )
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 测试各种边界条件和极值情况
  
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 单位元测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(5, subtract(5, 0))
  
  // 极值测试
  assert_eq(20000, add(10000, 10000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(9999, 10000))
  assert_eq(10000, divide_with_ceil(10000, 1))
  assert_eq(1, divide_with_ceil(10000, 10000))
}

test "negative_numbers_comprehensive" {
  // 负数综合处理测试
  // 测试涉及负数的各种运算
  
  // 负数加法
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-3, 5))
  assert_eq(-8, add(-5, -3))
  
  // 负数乘法
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(0, multiply(-5, 0))
  
  // 负数向上取整除法
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(1, divide_with_ceil(-3, -4))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  
  // 负数减法
  assert_eq(-8, subtract(-5, 3))
  assert_eq(2, subtract(-3, -5))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  // 测试问候函数和各种字符串输入
  
  // 基本字符串测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 特殊字符串测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  
  // 空白字符测试
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, leading space!", greet(" leading space"))
  assert_eq_string("Hello, trailing space !", greet("trailing space "))
  
  // 特殊字符测试
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, éspanol!", greet("éspanol"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算测试
  // 模拟真实世界的包装计算场景
  
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  // 验证需要的箱子数量
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
  
  // 验证空间利用率
  let utilization_percent = divide_with_ceil(multiply(items, 100), total_capacity)
  assert_eq(98, utilization_percent)  // floor(4700/48) = 97%，向上取整为98%
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 测试多步骤的复杂计算流程
  
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)  // 10 * 3 = 30
  assert_eq(30, step1)
  
  let step2 = divide_with_ceil(step1, divisor)  // ceil(30/4) = 8
  assert_eq(8, step2)
  
  let result = add(step2, addition)  // 8 + 5 = 13
  assert_eq(13, result)
  
  // 验证整个流程的正确性
  let expected_result = add(divide_with_ceil(multiply(base_value, multiplier), divisor), addition)
  assert_eq(result, expected_result)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 模拟资源分配和优化场景
  
  let total_resources = 100
  let teams = 3
  let min_resources_per_team = divide_with_ceil(total_resources, teams)
  
  // 验证每个团队的最少资源
  assert_eq(34, min_resources_per_team)
  
  // 验证分配结果
  let allocated_total = multiply(min_resources_per_team, teams)
  assert_eq(102, allocated_total)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), allocated_total)
  assert_eq(98, utilization)  // floor(10000/102) = 98%，向上取整为98%
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  // 测试各种错误情况和边界条件
  
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值验证
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
  
  // 边界情况验证
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数除法测试
  assert_eq(1000, divide_with_ceil(1000000, 1000))
  assert_eq(1001, divide_with_ceil(1000001, 1000))
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试：活动安排
  // 模拟活动座位安排的完整场景
  
  let participants = 87
  let tables_available = 10
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 验证桌子可用性 - 使用简单的数值比较而不是assert_true
  assert_eq(1, if(tables_needed <= tables_available) { 1 } else { 0 })
  
  // 计算可用座位数
  let available_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, available_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(available_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 验证座位充足性 - 使用简单的数值比较而不是assert_true
  assert_eq(1, if(available_seats >= participants) { 1 } else { 0 })
  
  // 计算座位利用率
  let seat_utilization = divide_with_ceil(multiply(participants, 100), available_seats)
  assert_eq(97, seat_utilization)  // floor(8700/90) = 96%，向上取整为97%
}