// Azimuth 增强测试套件
// 包含10个标准 MoonBit 测试用例，覆盖各种场景和边界情况

test "enhanced_mathematical_identities" {
  // 数学恒等式综合测试
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
  assert_eq(-x, subtract(0, x))
}

test "enhanced_division_precision" {
  // 除法精度测试
  // 测试接近整数的除法情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(5, divide_with_ceil(9, 2))
  
  // 测试大数精度
  assert_eq(1000, divide_with_ceil(999999, 1000))
  assert_eq(1001, divide_with_ceil(1000001, 1000))
}

test "enhanced_string_processing" {
  // 字符串处理增强测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, @user!", greet("@user"))
  assert_eq_string("Hello, $100!", greet("$100"))
  assert_eq_string("Hello, space !", greet("space "))
  assert_eq_string("Hello,  multiple!", greet(" multiple"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, émoji 🚀!", greet("émoji 🚀"))
}

test "enhanced_negative_arithmetic" {
  // 负数算术增强测试
  // 加法负数测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(5, add(-10, 15))
  assert_eq(-15, add(-20, 5))
  
  // 乘法负数测试
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(-25, multiply(5, -5))
  assert_eq(0, multiply(-5, 0))
  
  // 除法负数测试
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(2, divide_with_ceil(-5, -3))
  assert_eq(-2, divide_with_ceil(5, -3))
}

test "enhanced_business Logistics" {
  // 物流业务场景测试
  let total_packages = 147
  let truck_capacity = 24
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  
  assert_eq(7, trucks_needed)
  
  // 计算总运输能力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  assert_eq(21, remaining_space)
  
  // 计算空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_packages, 100), total_capacity)
  assert_eq(88, utilization_percent)  // floor(14700/168) = 87%
}

test "enhanced_complex_calculations" {
  // 复杂计算链测试
  let base_value = 10
  
  // 第一步：加法
  let step1 = add(base_value, 5)  // 15
  
  // 第二步：乘法
  let step2 = multiply(step1, 3)  // 45
  
  // 第三步：除法
  let step3 = divide_with_ceil(step2, 8)  // 6
  
  // 第四步：减法
  let step4 = subtract(step3, 2)  // 4
  
  // 验证最终结果
  assert_eq(4, step4)
}

test "enhanced_boundary_conditions" {
  // 边界条件综合测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(0, -100))
  
  // 单位值测试
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(1, 2))
  
  // 大数测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(1000001, divide_with_ceil(1000000, 1))
}

test "enhanced_error_handling" {
  // 错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  assert_eq(0, divide_with_ceil(-1000000, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算测试
  let max_test = 1000000
  let min_test = -1000000
  
  assert_eq(0, multiply(max_test, 0))
  assert_eq(0, multiply(min_test, 0))
  assert_eq(max_test, add(max_test, 0))
  assert_eq(min_test, add(min_test, 0))
}

test "enhanced_project_management" {
  // 项目管理场景测试
  let team_members = 12
  let hours_per_day = 8
  let working_days = 20
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  assert_eq(1920, total_hours)  // 12 * 8 * 20 = 1920
  
  // 计算总任务数
  let total_tasks = multiply(total_hours, tasks_per_hour)
  assert_eq(3840, total_tasks)  // 1920 * 2 = 3840
  
  // 计算需要的冲刺数（每冲刺500个任务）
  let tasks_per_sprint = 500
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(8, sprints_needed)  // ceil(3840/500) = 8
  
  // 计算总冲刺容量
  let total_sprint_capacity = multiply(sprints_needed, tasks_per_sprint)
  assert_eq(4000, total_sprint_capacity)  // 8 * 500 = 4000
  
  // 计算剩余任务容量
  let remaining_capacity = subtract(total_sprint_capacity, total_tasks)
  assert_eq(160, remaining_capacity)  // 4000 - 3840 = 160
}

test "enhanced_financial_calculations" {
  // 财务计算测试
  let product_price = 99
  let quantity = 47
  let tax_rate = 8  // 8%
  let discount_threshold = 1000
  let discount_rate = 5  // 5%
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  assert_eq(4653, subtotal)  // 99 * 47 = 4653
  
  // 计算税额
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  assert_eq(373, tax_amount)  // ceil(4653 * 8 / 100) = ceil(372.24) = 373
  
  // 计算总计
  let total_before_discount = add(subtotal, tax_amount)
  assert_eq(5026, total_before_discount)  // 4653 + 373 = 5026
  
  // 检查是否符合折扣条件
  let discount_applicable = total_before_discount >= discount_threshold
  assert_true(discount_applicable)
  
  // 计算折扣金额
  let discount_amount = 0
  let final_total = 0
  
  if (discount_applicable) {
    discount_amount = divide_with_ceil(multiply(total_before_discount, discount_rate), 100)
    final_total = subtract(total_before_discount, discount_amount)
  } else {
    final_total = total_before_discount
  }
  
  assert_eq(252, discount_amount)  // ceil(5026 * 5 / 100) = ceil(251.3) = 252
  assert_eq(4774, final_total)  // 5026 - 252 = 4774
}