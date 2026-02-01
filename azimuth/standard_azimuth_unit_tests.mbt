// 标准 Azimuth 单元测试
// 包含 8 个高质量 MoonBit 测试用例

// 导入必要的函数
use azimuth

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(0, 0))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-20, multiply(-5, 4))
  assert_eq(0, multiply(0, 123))
  
  assert_eq(-1, subtract(7, 8))
  assert_eq(-9, subtract(-5, 4))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 3))
  assert_eq(0, divide_with_ceil(0, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  
  // 负数测试
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 7
  
  // 交换律验证
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 结合律验证
  let c = 5
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律验证
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：计算仓库存储需求
  let products = [150, 89, 47, 23]  // 产品数量
  let box_capacity = 20  // 每箱容量
  
  // 计算每种产品需要的箱子数
  let boxes_1 = divide_with_ceil(products[0], box_capacity)  // ceil(150/20) = 8
  let boxes_2 = divide_with_ceil(products[1], box_capacity)  // ceil(89/20) = 5
  let boxes_3 = divide_with_ceil(products[2], box_capacity)  // ceil(47/20) = 3
  let boxes_4 = divide_with_ceil(products[3], box_capacity)  // ceil(23/20) = 2
  
  let total_boxes = add(add(boxes_1, boxes_2), add(boxes_3, boxes_4))
  assert_eq(18, total_boxes)
  
  // 计算总存储容量
  let total_capacity = multiply(total_boxes, box_capacity)
  assert_eq(360, total_capacity)
  
  // 计算剩余空间
  let total_products = add(add(products[0], products[1]), add(products[2], products[3]))
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(51, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_members = 6
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  assert_eq(720, total_hours)  // 6 * 8 * 15 = 720
  
  // 计算总任务数
  let total_tasks = multiply(total_hours, tasks_per_hour)
  assert_eq(1440, total_tasks)  // 720 * 2 = 1440
  
  // 计算需要的冲刺数（每冲刺处理400个任务）
  let sprint_capacity = 400
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(4, sprints_needed)  // ceil(1440/400) = 4
  
  // 计算实际处理能力
  let actual_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(1600, actual_capacity)
  
  // 计算剩余处理能力
  let remaining_capacity = subtract(actual_capacity, total_tasks)
  assert_eq(160, remaining_capacity)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 极值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(0, add(1000000, -1000000))
  
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(-1000000000000, multiply(-1000000, 1000000))
  
  // 边界除法测试
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
  
  // 零值测试
  assert_eq(0, multiply(0, 1000000))
  assert_eq(0, divide_with_ceil(0, 1000000))
  assert_eq(1000000, add(1000000, 0))
  assert_eq(1000000, subtract(1000000, 0))
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算稳定性
  let big_num = 100000
  assert_eq(200000, add(big_num, big_num))
  assert_eq(10000000000, multiply(big_num, big_num))
  assert_eq(100000, divide_with_ceil(big_num, 1))
  assert_eq(1, divide_with_ceil(big_num, big_num))
  
  // 负数运算稳定性
  let neg_num = -50000
  assert_eq(-100000, add(neg_num, neg_num))
  assert_eq(2500000000, multiply(neg_num, neg_num))
  assert_eq(-50000, divide_with_ceil(neg_num, 1))
  assert_eq(1, divide_with_ceil(neg_num, neg_num))
}