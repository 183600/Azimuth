// Azimuth 新增标准测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例
// 覆盖各种场景、边界情况和实际应用

test "add_function_comprehensive_coverage" {
  // 加法函数综合覆盖测试
  // 基本正数加法
  assert_eq(42, add(20, 22))
  
  // 零值加法
  assert_eq(25, add(25, 0))
  assert_eq(25, add(0, 25))
  
  // 负数加法
  assert_eq(-10, add(-5, -5))
  assert_eq(5, add(-10, 15))
  
  // 混合符号加法
  assert_eq(-3, add(-8, 5))
  assert_eq(0, add(-10, 10))
}

test "multiply_function_various_scenarios" {
  // 乘法函数各种场景测试
  // 基本正数乘法
  assert_eq(36, multiply(6, 6))
  
  // 零乘法
  assert_eq(0, multiply(100, 0))
  assert_eq(0, multiply(0, 100))
  
  // 负数乘法
  assert_eq(16, multiply(-4, -4))
  assert_eq(-20, multiply(-5, 4))
  
  // 单位乘法
  assert_eq(17, multiply(17, 1))
  assert_eq(17, multiply(1, 17))
}

test "divide_with_ceil_edge_cases_handling" {
  // 向上取整除法边界情况处理测试
  // 基本向上取整
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  
  // 零除数处理
  assert_eq(0, divide_with_ceil(15, 0))
  assert_eq(0, divide_with_ceil(-15, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 零被除数
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(0, -10))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-11, 3))
  assert_eq(-4, divide_with_ceil(11, -3))
  assert_eq(4, divide_with_ceil(-11, -3))
}

test "subtract_function_mathematical_properties" {
  // 减法函数数学性质测试
  // 基本减法
  assert_eq(15, subtract(25, 10))
  assert_eq(-15, subtract(10, 25))
  
  // 零减法
  assert_eq(20, subtract(20, 0))
  assert_eq(-20, subtract(0, 20))
  
  // 自减
  assert_eq(0, subtract(30, 30))
  
  // 负数减法
  assert_eq(-5, subtract(-10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "greet_function_unicode_and_special_chars" {
  // 问候函数Unicode和特殊字符测试
  // 基本英文
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // Unicode字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  
  // Emoji
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  
  // 特殊字符
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_laws_validation" {
  // 数学定律验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律: a * (b + c) = a * b + a * c
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  // 场景：仓库需要将物品装入箱子
  let total_products = 158
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 验证需要的箱子数量
  assert_eq(7, boxes_needed)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(10, remaining_space)
  
  // 验证所有产品都能装下
  assert_true(total_capacity >= total_products)
}

test "project_resource_allocation" {
  // 项目资源分配测试
  // 场景：计算项目所需的人力和时间
  let team_members = 7
  let tasks_per_member = 9
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let tasks_per_sprint = 16
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  let total_days = multiply(sprints_needed, sprint_duration)
  
  // 验证计算结果
  assert_eq(63, total_tasks)  // 7 * 9 = 63
  assert_eq(4, sprints_needed)  // ceil(63/16) = 4
  assert_eq(8, total_days)  // 4 * 2 = 8
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, tasks_per_sprint)
  assert_eq(64, total_capacity)
  
  // 验证容量充足
  assert_true(total_capacity >= total_tasks)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 25000
  let monthly_payment = 1200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  // 验证需要的月数
  assert_eq(21, months_needed)
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(25200, total_payment)
  
  // 计算额外支付
  let extra_payment = subtract(total_payment, loan_amount)
  assert_eq(200, extra_payment)
  
  // 验证总支付覆盖贷款
  assert_true(total_payment >= loan_amount)
}

test "comprehensive_boundary_testing" {
  // 综合边界测试
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(999999, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  
  // 极值减法
  assert_eq(0, subtract(1000000, 1000000))
  assert_eq(2000000, subtract(1000000, -1000000))
  
  // 复杂边界表达式
  let complex_result = add(
    multiply(1000, 1000),
    divide_with_ceil(subtract(5000, 100), 100)
  )
  assert_eq(1000040, complex_result)  // 1000000 + ceil(4900/100) = 1000000 + 49 = 1000049
}