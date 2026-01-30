// 新增的标准 MoonBit 测试用例
// 为 azimuth 库添加高质量测试覆盖，包含8个精选测试用例

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  // 加法测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
  
  // 乘法测试
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(0, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  
  // 减法测试
  assert_eq(-4, subtract(3, 7))
  assert_eq(0, subtract(5, 5))
  assert_eq(8, subtract(-3, -11))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-4, divide_with_ceil(10, -3))
  
  // 除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  // 基本英文测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // Unicode字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  
  // Emoji测试
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  
  // 特殊情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  let z = 5
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
  
  // 恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：仓库库存管理
  let product_a_count = 156
  let product_b_count = 89
  let product_c_count = 47
  let box_capacity = 24
  
  // 计算每种产品需要的箱子数
  let boxes_a = divide_with_ceil(product_a_count, box_capacity)
  let boxes_b = divide_with_ceil(product_b_count, box_capacity)
  let boxes_c = divide_with_ceil(product_c_count, box_capacity)
  
  assert_eq(7, boxes_a)  // ceil(156/24) = 7
  assert_eq(4, boxes_b)  // ceil(89/24) = 4
  assert_eq(2, boxes_c)  // ceil(47/24) = 2
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  assert_eq(13, total_boxes)
  
  // 计算总容量
  let total_capacity = multiply(total_boxes, box_capacity)
  assert_eq(312, total_capacity)
  
  // 计算剩余容量
  let total_products = add(add(product_a_count, product_b_count), product_c_count)
  let remaining_capacity = subtract(total_capacity, total_products)
  assert_eq(20, remaining_capacity)
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 极值测试
  let large_value = 100000
  let small_value = 1
  
  assert_eq(200000, add(large_value, large_value))
  assert_eq(10000000000, multiply(large_value, large_value))
  assert_eq(100000, divide_with_ceil(large_value, small_value))
  assert_eq(1, divide_with_ceil(small_value, large_value))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 连续错误处理
  let step1 = divide_with_ceil(50, 0)  // 0
  let step2 = add(step1, 25)  // 25
  let step3 = divide_with_ceil(step2, 5)  // 5
  assert_eq(0, step1)
  assert_eq(25, step2)
  assert_eq(5, step3)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源规划
  let team_members = 8
  let project_duration_weeks = 12
  let hours_per_week = 40
  let tasks_per_hour = 2
  let total_tasks = 750
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(project_duration_weeks, hours_per_week))
  assert_eq(3840, total_hours)  // 8 * 12 * 40 = 3840
  
  // 计算总任务处理能力
  let total_capacity = multiply(total_hours, tasks_per_hour)
  assert_eq(7680, total_capacity)  // 3840 * 2 = 7680
  
  // 计算需要的项目周期（基于任务数量）
  let weeks_needed = divide_with_ceil(total_tasks, multiply(team_members, multiply(hours_per_week, tasks_per_hour)))
  assert_eq(2, weeks_needed)  // ceil(750/(8*40*2)) = 2
  
  // 计算资源利用率
  let utilization = divide_with_ceil(multiply(total_tasks, 100), total_capacity)
  assert_eq(10, utilization)  // ceil(75000/7680) = 10
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  // 场景：会议安排
  let attendees = 125
  let rooms_available = 8
  let chairs_per_room = 20
  let cost_per_chair = 45
  let total_budget = 6000
  
  // 计算需要的房间数量
  let rooms_needed = divide_with_ceil(attendees, chairs_per_room)
  assert_eq(7, rooms_needed)  // ceil(125/20) = 7
  
  // 验证房间是否足够
  assert_true(rooms_needed <= rooms_available)
  
  // 计算总座位数
  let total_seats = multiply(rooms_needed, chairs_per_room)
  assert_eq(140, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(15, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(6300, total_cost)
  
  // 检查预算是否充足（这里应该会超预算，这是测试的一部分）
  let budget_exceeded = total_cost > total_budget
  assert_true(budget_exceeded)
  
  // 计算超出的金额
  let over_budget = subtract(total_cost, total_budget)
  assert_eq(300, over_budget)
}