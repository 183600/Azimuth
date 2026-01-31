// 标准 MoonBit 测试套件 - 8个高质量测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和边界情况

test "add_function_various_scenarios" {
  // 加法函数多种场景测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(3, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-2, divide_with_ceil(-3, 2))
  assert_eq(-2, divide_with_ceil(3, -2))
  assert_eq(2, divide_with_ceil(-3, -2))
}

test "subtract_function_edge_cases" {
  // 减法函数边缘情况测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(8, subtract(5, -3))
  assert_eq(-8, subtract(-5, 3))
}

test "greet_function_unicode_support" {
  // 问候函数 Unicode 支持测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_inventory_calculation" {
  // 业务库存计算测试
  let products = 65
  let box_capacity = 10
  let boxes_needed = divide_with_ceil(products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(70, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, products)
  assert_eq(5, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源分配
  let team_size = 6
  let weekly_hours = 40
  let project_weeks = 5
  let total_tasks = 120
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(weekly_hours, project_weeks))
  assert_eq(1200, total_hours)
  
  // 计算每小时任务数
  let tasks_per_hour = divide_with_ceil(total_tasks, total_hours)
  assert_eq(1, tasks_per_hour)
  
  // 验证任务分配合理性
  assert_true(tasks_per_hour > 0)
}