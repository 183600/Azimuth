// 高质量标准 MoonBit 测试用例
// 为 azimuth 库编写的综合测试，覆盖各种函数和边界情况

test "azimuth_high_quality_basic_arithmetic" {
  // 基础算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(-10, 10))
  assert_eq(1000000, add(500000, 500000))
}

test "azimuth_high_quality_multiply_function" {
  // 乘法函数综合测试
  assert_eq(42, multiply(6, 7))
  assert_eq(-42, multiply(-6, 7))
  assert_eq(42, multiply(-6, -7))
  assert_eq(0, multiply(12345, 0))
  assert_eq(1, multiply(1, 12345))
}

test "azimuth_high_quality_divide_ceil_boundary" {
  // 向上取整除法边缘情况测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "azimuth_high_quality_subtract_function" {
  // 减法函数综合测试
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(5, -10))
  assert_eq(-15, subtract(-5, 10))
}

test "azimuth_high_quality_greet_international" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "azimuth_high_quality_business_calculation" {
  // 复杂业务计算测试：项目资源规划
  let team_members = 8
  let hours_per_day = 8
  let project_days = 15
  let total_tasks = 150
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_days))
  assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  
  // 计算每人需要处理的任务数
  let tasks_per_person = divide_with_ceil(total_tasks, team_members)
  assert_eq(19, tasks_per_person)  // ceil(150/8) = 19
  
  // 计算每人需要的工时（假设每个任务需要2小时）
  let hours_per_person = multiply(tasks_per_person, 2)
  assert_eq(38, hours_per_person)  // 19 * 2 = 38
  
  // 验证工时不超过项目总工时
  assert_true(hours_per_person <= project_days * hours_per_day)
}

test "azimuth_high_quality_math_properties" {
  // 数学性质验证测试
  let x = 25
  let y = 17
  let z = 6
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "azimuth_high_quality_inventory_management" {
  // 库存管理场景测试
  let product_a = 47
  let product_b = 63
  let product_c = 29
  let box_capacity = 12
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a, box_capacity)  // ceil(47/12) = 4
  let boxes_b = divide_with_ceil(product_b, box_capacity)  // ceil(63/12) = 6
  let boxes_c = divide_with_ceil(product_c, box_capacity)  // ceil(29/12) = 3
  
  assert_eq(4, boxes_a)
  assert_eq(6, boxes_b)
  assert_eq(3, boxes_c)
  
  // 计算总箱子数量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  assert_eq(13, total_boxes)
  
  // 计算总存储容量
  let total_capacity = multiply(total_boxes, box_capacity)
  assert_eq(156, total_capacity)
  
  // 计算总产品数量
  let total_products = add(add(product_a, product_b), product_c)
  assert_eq(139, total_products)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(17, remaining_space)
}

test "azimuth_high_quality_error_handling" {
  // 错误处理和边界条件测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数测试
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000, multiply(large_num, 1))
  assert_eq(1000000, divide_with_ceil(large_num, 1))
}

test "azimuth_high_quality_financial_calculation" {
  // 金融计算场景测试：贷款和投资
  let principal = 50000
  let monthly_payment = 2200
  let annual_return_rate = 8  // 8%
  
  // 计算还款月数
  let months_needed = divide_with_ceil(principal, monthly_payment)
  assert_eq(23, months_needed)  // ceil(50000/2200) = 23
  
  // 计算总还款金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)
  
  // 计算利息
  let total_interest = subtract(total_payment, principal)
  assert_eq(600, total_interest)
  
  // 计算年化收益率（简化计算）
  let years = divide_with_ceil(months_needed, 12)
  let annual_return = divide_with_ceil(multiply(total_interest, 100), multiply(principal, years))
  assert_eq(1, annual_return)  // ceil(600*100/(50000*2)) = 1%
}