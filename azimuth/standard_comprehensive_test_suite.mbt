// 标准 MoonBit 综合测试套件
// 包含 8 个高质量测试用例，覆盖核心功能和边界情况

test "comprehensive_addition_edge_cases" {
  // 加法函数的边界情况全面测试
  assert_eq(0, add(0, 0))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(-1000000, add(-500000, -500000))
  assert_eq(0, add(1000000, -1000000))
  assert_eq(1, add(-999999, 1000000))
}

test "comprehensive_multiplication_properties" {
  // 乘法函数的数学性质测试
  let a = 42
  let b = 17
  
  // 交换律验证
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零乘法性质
  assert_eq(0, multiply(a, 0))
  assert_eq(0, multiply(0, b))
  
  // 单位元性质
  assert_eq(a, multiply(a, 1))
  assert_eq(b, multiply(1, b))
  
  // 负数乘法
  assert_eq(-a, multiply(a, -1))
  assert_eq(a, multiply(-a, -1))
}

test "comprehensive_divide_with_ceil_scenarios" {
  // 向上取整除法的综合场景测试
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
  
  // 特殊情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "comprehensive_greet_function_international" {
  // 问候函数的国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "comprehensive_subtract_function_properties" {
  // 减法函数的性质测试
  let a = 100
  let b = 37
  
  // 基本减法
  assert_eq(63, subtract(a, b))
  assert_eq(-63, subtract(b, a))
  
  // 自减性质
  assert_eq(0, subtract(a, a))
  
  // 零减法性质
  assert_eq(a, subtract(a, 0))
  assert_eq(-a, subtract(0, a))
  
  // 负数减法
  assert_eq(137, subtract(a, -b))
  assert_eq(-137, subtract(-a, b))
}

test "comprehensive_business_inventory_management" {
  // 库存管理业务逻辑综合测试
  let total_products = 287
  let box_capacity = 24
  let warehouse_sections = 8
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(12, boxes_needed)
  
  // 计算每个仓库区域的箱子数量
  let boxes_per_section = divide_with_ceil(boxes_needed, warehouse_sections)
  assert_eq(2, boxes_per_section)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(288, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(1, remaining_space)
}

test "comprehensive_project_resource_calculation" {
  // 项目资源计算综合测试
  let team_size = 15
  let work_days_per_sprint = 10
  let tasks_per_person_per_day = 3
  let total_project_tasks = 450
  
  // 计算每个冲刺的处理能力
  let sprint_capacity = multiply(team_size, multiply(work_days_per_sprint, tasks_per_person_per_day))
  assert_eq(450, sprint_capacity)
  
  // 计算需要的冲刺数量
  let sprints_needed = divide_with_ceil(total_project_tasks, sprint_capacity)
  assert_eq(1, sprints_needed)
  
  // 计算总工时（假设每天8小时）
  let total_work_hours = multiply(sprints_needed, multiply(team_size, multiply(work_days_per_sprint, 8)))
  assert_eq(1200, total_work_hours)
}

test "comprehensive_error_handling_boundary" {
  // 错误处理和边界情况综合测试
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  
  // 极端除法情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(999999, 1000000))
  
  // 特殊字符串处理
  assert_eq_string("Hello, special@#$%chars!", greet("special@#$%chars"))
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  
  // 数学恒等式验证
  let x = 12345
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, subtract(x, x))
}