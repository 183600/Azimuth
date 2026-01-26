// Azimuth 增强测试用例 - 10个高质量 MoonBit 测试用例
// 测试 azimuth 库的核心功能和边缘情况

// 导入 azimuth 库
use azimuth

test "add_function_comprehensive_boundary" {
  // 全面测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  assert_eq(0, add(2147483647, -2147483647))
}

test "multiply_function_precision_test" {
  // 测试乘法函数的精度和大数处理
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(46340, multiply(46340, 1))  // sqrt(INT_MAX)
  assert_eq(1000000000, multiply(10000, 100000))
  assert_eq(-1000000000, multiply(-10000, 100000))
}

test "divide_with_ceil_edge_scenarios" {
  // 测试向上取整除法的边缘场景
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(0, divide_with_ceil(-1, 1))
  assert_eq(1000000, divide_with_ceil(999999, 1))
  assert_eq(1, divide_with_ceil(999999, 999999))
}

test "greet_function_character_encoding" {
  // 测试问候函数的字符编码处理
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景测试
  let principal = 50000
  let interest_rate = 8  // 8%
  let years = 5
  
  // 计算复利（简化版本）
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, interest_rate), 100))
  let year4 = add(year3, divide_with_ceil(multiply(year3, interest_rate), 100))
  let year5 = add(year4, divide_with_ceil(multiply(year4, interest_rate), 100))
  
  assert_eq(54000, year1)  // 50000 + 8%
  assert_eq(58320, year2)  // 54000 + 8%
  assert_eq(62986, year3)  // 58320 + 8%
  assert_eq(68025, year4)  // 62986 + 8%
  assert_eq(73467, year5)  // 68025 + 8%
  
  // 计算月供
  let total_months = multiply(years, 12)
  let monthly_payment = divide_with_ceil(year5, total_months)
  assert_eq(1223, monthly_payment)  // ceil(73467/60)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 12345
  let departments = 7
  let min_per_dept = divide_with_ceil(total_resources, departments)
  
  assert_eq(1764, min_per_dept)  // ceil(12345/7)
  
  // 验证分配结果
  let total_allocated = multiply(min_per_dept, departments)
  assert_eq(12348, total_allocated)
  
  // 计算资源浪费
  let waste = subtract(total_allocated, total_resources)
  assert_eq(3, waste)
  
  // 优化策略：增加一个部门
  let optimized_departments = add(departments, 1)
  let optimized_per_dept = divide_with_ceil(total_resources, optimized_departments)
  assert_eq(1544, optimized_per_dept)  // ceil(12345/8)
}

test "mathematical identities_verification" {
  // 数学恒等式验证测试
  let a = 42
  let b = 17
  let c = 23
  
  // 加法恒等式
  assert_eq(a, add(a, 0))
  assert_eq(add(a, b), add(b, a))  // 交换律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))  // 结合律
  
  // 乘法恒等式
  assert_eq(a, multiply(a, 1))
  assert_eq(0, multiply(a, 0))
  assert_eq(multiply(a, b), multiply(b, a))  // 交换律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))  // 结合律
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "logistics_packaging_optimization" {
  // 物流包装优化测试
  // 产品A：156个，每箱24个
  let product_a_qty = 156
  let product_a_capacity = 24
  let boxes_a = divide_with_ceil(product_a_qty, product_a_capacity)
  
  // 产品B：89个，每箱12个
  let product_b_qty = 89
  let product_b_capacity = 12
  let boxes_b = divide_with_ceil(product_b_qty, product_b_capacity)
  
  // 产品C：234个，每箱36个
  let product_c_qty = 234
  let product_c_capacity = 36
  let boxes_c = divide_with_ceil(product_c_qty, product_c_capacity)
  
  assert_eq(7, boxes_a)   // ceil(156/24)
  assert_eq(8, boxes_b)   // ceil(89/12)
  assert_eq(7, boxes_c)   // ceil(234/36)
  
  // 计算总箱数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = add(
    multiply(boxes_a, product_a_capacity),
    add(multiply(boxes_b, product_b_capacity), multiply(boxes_c, product_c_capacity))
  )
  
  assert_eq(22, total_boxes)
  assert_eq(516, total_capacity)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极值运算
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  assert_eq(0, multiply(2147483647, 0))
  assert_eq(0, multiply(-2147483648, 0))
}

test "real_world_project_management" {
  // 真实世界项目管理测试
  let project_tasks = 127
  let team_members = 8
  let working_days = 15
  let tasks_per_person_per_day = 2
  
  // 计算团队日产能
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  assert_eq(16, daily_capacity)
  
  // 计算需要的工作日
  let days_needed = divide_with_ceil(project_tasks, daily_capacity)
  assert_eq(8, days_needed)  // ceil(127/16)
  
  // 验证项目是否能在规定时间内完成
  assert_true(days_needed <= working_days)
  
  // 计算实际完成时间
  let total_capacity = multiply(days_needed, daily_capacity)
  let buffer_tasks = subtract(total_capacity, project_tasks)
  
  assert_eq(128, total_capacity)
  assert_eq(1, buffer_tasks)
  
  // 计算资源利用率
  let utilization = divide_with_ceil(multiply(project_tasks, 100), total_capacity)
  assert_eq(100, utilization)  // ceil(12700/128) = 100%
}