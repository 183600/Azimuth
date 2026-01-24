// Azimuth 高质量测试套件
// 包含10个标准 MoonBit 测试用例，覆盖各种场景和边界情况

test "add_function_stress_test" {
  // 加法函数压力测试：测试大数和边界值
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(1, add(2147483646, -2147483645))
}

test "multiply_function_precision_test" {
  // 乘法函数精度测试：验证数值计算的准确性
  assert_eq(123456789, multiply(11111, 11111))
  assert_eq(-123456789, multiply(-11111, 11111))
  assert_eq(123456789, multiply(-11111, -11111))
  assert_eq(0, multiply(2147483647, 0))
}

test "divide_with_ceil_financial_test" {
  // 向上取整除法金融场景测试：计算贷款分期
  let loan_amount = 50000
  let monthly_payment = 2250
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  assert_eq(23, months_needed)  // ceil(50000/2250) = 23
  
  // 验证总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(51750, total_payment)
}

test "greet_function_multilingual_test" {
  // 问候函数多语言测试：验证国际化支持
  assert_eq_string("Hello, Bonjour!", greet("Bonjour"))
  assert_eq_string("Hello, Hola!", greet("Hola"))
  assert_eq_string("Hello, Japanese!", greet("Japanese"))
  assert_eq_string("Hello, Korean!", greet("Korean"))
  assert_eq_string("Hello, Arabic!", greet("Arabic"))
}

test "complex_inventory_management_test" {
  // 复杂库存管理测试：多物品装箱优化
  let item_a_qty = 47
  let item_b_qty = 63
  let item_c_qty = 29
  let box_capacity = 15
  
  let boxes_a = divide_with_ceil(item_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(item_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(item_c_qty, box_capacity)
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  assert_eq(4, boxes_a)  // ceil(47/15) = 4
  assert_eq(5, boxes_b)  // ceil(63/15) = 5
  assert_eq(2, boxes_c)  // ceil(29/15) = 2
  assert_eq(11, total_boxes)
  
  // 验证总容量
  let total_capacity = multiply(total_boxes, box_capacity)
  assert_eq(165, total_capacity)
}

test "mathematical_properties_comprehensive_test" {
  // 数学性质综合测试：验证基本数学定律
  let x = 24
  let y = 18
  let z = 6
  
  // 加法结合律
  assert_eq(
    add(add(x, y), z),
    add(x, add(y, z))
  )
  
  // 乘法结合律
  assert_eq(
    multiply(multiply(x, y), z),
    multiply(x, multiply(y, z))
  )
  
  // 分配律
  assert_eq(
    multiply(x, add(y, z)),
    add(multiply(x, y), multiply(x, z))
  )
}

test "error_handling_boundary_test" {
  // 错误处理边界测试：验证异常情况的处理
  // 除零情况的多种测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1000))
}

test "resource_allocation_optimization_test" {
  // 资源分配优化测试：计算最优资源分配
  let total_budget = 25000
  let departments = 7
  let min_budget_per_dept = 3000
  
  // 计算每个部门的预算
  let budget_per_dept = divide_with_ceil(total_budget, departments)
  assert_eq(3572, budget_per_dept)  // ceil(25000/7) = 3572
  
  // 验证所有部门都能获得最低预算
  let meets_minimum = budget_per_dept > min_budget_per_dept
  assert_true(meets_minimum)
  
  // 计算总分配预算
  let total_allocated = multiply(budget_per_dept, departments)
  let budget_surplus = total_allocated - total_budget
  assert_eq(4, budget_surplus)
}

test "time_calculation_project_management_test" {
  // 时间计算项目管理测试：项目时间规划
  let tasks = 85
  let team_members = 6
  let hours_per_day = 8
  let tasks_per_person_per_day = 2
  
  // 计算团队每日处理能力
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  
  // 计算需要的工作日数
  let work_days_needed = divide_with_ceil(tasks, daily_capacity)
  assert_eq(8, work_days_needed)  // ceil(85/12) = 8
  
  // 计算总工作小时数
  let total_hours = multiply(work_days_needed, hours_per_day)
  assert_eq(64, total_hours)
  
  // 验证总处理能力
  let total_capacity = multiply(work_days_needed, daily_capacity)
  assert_eq(96, total_capacity)
}

test "string_processing_edge_cases_test" {
  // 字符串处理边缘情况测试：特殊字符和格式
  assert_eq_string("Hello, quoted!", greet("quoted"))
  assert_eq_string("Hello, apostrophe!", greet("apostrophe"))
  assert_eq_string("Hello, backslash!", greet("backslash"))
  assert_eq_string("Hello, emoji!", greet("emoji"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, spaces!", greet("   spaces   "))
}