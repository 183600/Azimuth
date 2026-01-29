// Azimuth 增强测试用例
// 标准 MoonBit 测试语法，包含10个高质量测试用例

test "subtract_function_basic" {
  // 测试减法函数的基本功能
  assert_eq(5, subtract(10, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(10, subtract(0, -10))
}

test "mathematical_identity_verification" {
  // 验证数学恒等式
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-4, divide_with_ceil(10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  let a = 15
  let b = 7
  let c = 3
  
  // 复合运算
  let result1 = add(multiply(a, b), divide_with_ceil(c, 2))
  assert_eq(106, result1)  // 15*7 + ceil(3/2) = 105 + 2 = 107
  
  let result2 = subtract(multiply(a, c), divide_with_ceil(b, 3))
  assert_eq(43, result2)   // 15*3 - ceil(7/3) = 45 - 3 = 42
  
  // 嵌套运算
  let result3 = divide_with_ceil(add(multiply(a, b), multiply(c, 2)), 10)
  assert_eq(11, result3)   // ceil((15*7 + 3*2)/10) = ceil(111/10) = 12
}

test "greet_function_multilingual" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Français!", greet("Français"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "business_logic_inventory_management" {
  // 库存管理业务逻辑测试
  let total_products = 157
  let box_capacity = 24
  let available_boxes = 10
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(7, boxes_needed)  // ceil(157/24) = 7
  
  // 验证箱子是否足够
  assert_true(boxes_needed <= available_boxes)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "edge_cases_and_boundary_values" {
  // 边界值和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, subtract(12345, 0))
  assert_eq(0, divide_with_ceil(0, 12345))
  
  // 单位值测试
  assert_eq(2, add(1, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(0, subtract(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 负值边界测试
  assert_eq(-2, add(-1, -1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(0, subtract(-1, -1))
}

test "performance_large_number_operations" {
  // 大数运算性能测试
  let large_num1 = 100000
  let large_num2 = 99999
  
  // 大数运算
  assert_eq(199999, add(large_num1, large_num2))
  assert_eq(9999900000, multiply(large_num1, large_num2))
  assert_eq(1, subtract(large_num1, large_num2 - 1))
  assert_eq(2, divide_with_ceil(large_num2, 50000))
  
  // 极值运算
  assert_eq(200000, add(large_num1, large_num1))
  assert_eq(10000000000, multiply(large_num1, large_num1))
}

test "real_world_project_planning" {
  // 真实世界项目规划测试
  let project_tasks = 87
  let team_members = 5
  let tasks_per_person_per_day = 2
  let working_days = 10
  
  // 计算团队每日处理能力
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  assert_eq(10, daily_capacity)
  
  // 计算需要的总天数
  let days_needed = divide_with_ceil(project_tasks, daily_capacity)
  assert_eq(9, days_needed)  // ceil(87/10) = 9
  
  // 验证是否在规定时间内完成
  assert_true(days_needed <= working_days)
  
  // 计算总处理能力
  let total_capacity = multiply(days_needed, daily_capacity)
  assert_eq(90, total_capacity)
  
  // 计算剩余处理能力
  let spare_capacity = subtract(total_capacity, project_tasks)
  assert_eq(3, spare_capacity)
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  // 特殊字符测试
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'apostrophe'!", greet("'apostrophe'"))
  assert_eq_string("Hello, \\backslash!", greet("\\backslash"))
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  
  // 数字字符串测试
  assert_eq_string("Hello, 123456!", greet("123456"))
  assert_eq_string("Hello, -987654!", greet("-987654"))
  assert_eq_string("Hello, 3.14159!", greet("3.14159"))
  
  // 空白字符测试
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, \t\n\r !", greet("\t\n\r "))
}