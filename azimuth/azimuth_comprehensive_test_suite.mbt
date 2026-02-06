// Azimuth 综合测试套件
// 标准 MoonBit 测试语法 - 8个高质量测试用例

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-2, add(-5, 3))
  assert_eq(0, add(5, -5))
  
  assert_eq(42, multiply(6, 7))
  assert_eq(-24, multiply(-3, 8))
  assert_eq(0, multiply(100, 0))
  
  assert_eq(8, subtract(15, 7))
  assert_eq(-12, subtract(5, 17))
  assert_eq(0, subtract(25, 25))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(8, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法恒等式
  assert_eq(a, add(a, 0))
  assert_eq(a, add(0, a))
  
  // 乘法恒等式
  assert_eq(a, multiply(a, 1))
  assert_eq(a, multiply(1, a))
  assert_eq(0, multiply(a, 0))
}

test "business_logistics_scenario" {
  // 业务物流场景测试
  let total_items = 157
  let container_capacity = 24
  let containers_needed = divide_with_ceil(total_items, container_capacity)
  
  assert_eq(7, containers_needed)
  
  // 验证总容量
  let total_capacity = multiply(containers_needed, container_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(11, remaining_space)
}

test "edge_cases_and_boundaries" {
  // 边界情况和极值测试
  // 极小值
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 零值处理
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 单位值
  assert_eq(1, multiply(1, 1))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 项目管理场景：计算资源需求
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  // 验证计算结果
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
  
  // 计算总工作量
  let total_work_days = multiply(sprints_needed, 5)  // 假设每个冲刺5天
  assert_eq(20, total_work_days)
}

test "large_numbers_performance" {
  // 大数性能测试
  let large_num1 = 100000
  let large_num2 = 200000
  
  // 大数运算
  assert_eq(300000, add(large_num1, large_num2))
  assert_eq(20000000000, multiply(large_num1, large_num2))
  assert_eq(1, divide_with_ceil(large_num1, large_num2))
  assert_eq(2, divide_with_ceil(large_num2, large_num1))
  
  // 大数减法
  assert_eq(-100000, subtract(large_num1, large_num2))
  assert_eq(100000, subtract(large_num2, large_num1))
  
  // 极大值测试
  let max_val = 1000000
  assert_eq(2000000, add(max_val, max_val))
  assert_eq(1, divide_with_ceil(max_val, max_val))
  assert_eq(2, divide_with_ceil(add(max_val, 1), max_val))
}