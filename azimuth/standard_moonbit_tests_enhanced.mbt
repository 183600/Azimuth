// 标准 MoonBit 测试用例 - 增强版
// 为 Azimuth 项目添加的高质量测试用例

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_various_cases" {
  // 乘法函数各种情况测试
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(100, 0))
  assert_eq(-30, multiply(-6, 5))
  assert_eq(30, multiply(-6, -5))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "divide_with_ceil_negative_operations" {
  // 向上取整除法负数运算测试
  assert_eq(-4, divide_with_ceil(15, -4))
  assert_eq(-4, divide_with_ceil(-15, 4))
  assert_eq(4, divide_with_ceil(-15, -4))
  assert_eq(-1, divide_with_ceil(-3, 5))
  assert_eq(1, divide_with_ceil(-3, -5))
}

test "greet_function_international" {
  // 问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "subtract_function_edge_cases" {
  // 减法函数边界情况测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(20, 20))
  assert_eq(100, subtract(50, -50))
  assert_eq(-100, subtract(-50, 50))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  let total_products = 85
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(8, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(96, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源规划
  let team_members = 8
  let tasks_per_member = 7
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(56, total_tasks)  // 8 * 7 = 56
  assert_eq(4, sprints_needed)  // ceil(56/15) = 4
}

test "boundary_and_precision_tests" {
  // 边界和精度测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(10, 10))
  assert_eq(1, divide_with_ceil(-10, -10))
  assert_eq(0, divide_with_ceil(0, 100))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊字符串处理
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special#chars!", greet("special#chars"))
}