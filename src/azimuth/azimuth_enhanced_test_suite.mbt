// Azimuth 增强测试套件 - 10个高质量 MoonBit 测试用例
// 覆盖基本功能、边界情况、错误处理和实际应用场景

test "enhanced_arithmetic_precision" {
  // 测试算术运算的精度和边界情况
  assert_eq(2147483647, add(2147483646, 1))  // 接近 Int 最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近 Int 最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数相加
  
  // 测试大数乘法的精度
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
  assert_eq(1000000, multiply(-1000, -1000))
}

test "enhanced_division_ceil_comprehensive" {
  // 全面测试向上取整除法的各种情况
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(4, divide_with_ceil(12, 3))  // 12/3 = 4 -> 4
  assert_eq(1, divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -11/3 = -3.666... -> -4
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "enhanced_string_greeting_international" {
  // 测试国际化字符串问候功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "enhanced_mathematical_properties" {
  // 验证数学性质和定律
  let x = 42
  let y = 17
  let z = 8
  
  // 加法交换律和结合律
  assert_eq(add(x, y), add(y, x))  // 交换律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))  // 结合律
  
  // 乘法交换律和结合律
  assert_eq(multiply(x, y), multiply(y, x))  // 交换律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))  // 结合律
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
  
  // 单位元性质
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
}

test "enhanced_financial_calculation_scenario" {
  // 复杂金融计算场景：贷款和投资计算
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  let monthly_payment_limit = 300
  
  // 计算复利（每年计算一次）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, annual_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, annual_rate), 100))
  
  // 计算月供
  let total_months = multiply(years, 12)
  let monthly_payment = divide_with_ceil(year3_amount, total_months)
  
  // 验证结果
  assert_eq(10500, year1_amount)  // 10000 + 5%
  assert_eq(11025, year2_amount)  // 10500 + 5%
  assert_eq(11576, year3_amount)  // 11025 + 5%
  assert_eq(322, monthly_payment)  // ceil(11576/36)
  
  // 验证月供是否在限制范围内
  assert_eq(1, if monthly_payment <= monthly_payment_limit { 1 } else { 0 })
}

test "enhanced_inventory_management" {
  // 库存管理系统测试 - 简化版本不使用数组
  
  // 产品1: 25个产品，每箱8个
  let boxes_1 = divide_with_ceil(25, 8)  // ceil(25/8) = 4
  let capacity_1 = multiply(boxes_1, 8)  // 4 * 8 = 32
  
  // 产品2: 47个产品，每箱12个
  let boxes_2 = divide_with_ceil(47, 12)  // ceil(47/12) = 4
  let capacity_2 = multiply(boxes_2, 12)  // 4 * 12 = 48
  
  // 产品3: 156个产品，每箱20个
  let boxes_3 = divide_with_ceil(156, 20)  // ceil(156/20) = 8
  let capacity_3 = multiply(boxes_3, 20)  // 8 * 20 = 160
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(add(boxes_1, boxes_2), boxes_3), 3)  // 加上产品4和5的箱子数
  let total_capacity = add(add(add(capacity_1, capacity_2), capacity_3), 135)  // 加上产品4和5的容量
  let total_products = add(add(add(25, 47), 156), 102)  // 总产品数
  
  assert_eq(25, total_boxes)
  assert_eq(375, total_capacity)
  assert_eq(330, total_products)
}

test "enhanced_project_resource_planning" {
  // 项目资源规划测试
  let team_members = 8
  let tasks_per_person = 5
  let sprint_duration = 2  // 周
  let tasks_per_sprint = 15
  let project_deadline = 4  // 周
  
  // 计算总任务数
  let total_tasks = multiply(team_members, tasks_per_person)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  // 计算项目总时间
  let total_weeks = multiply(sprints_needed, sprint_duration)
  
  // 计算每人每周需要完成的任务数
  let tasks_per_person_per_week = divide_with_ceil(total_tasks, multiply(team_members, project_deadline))
  
  // 验证结果
  assert_eq(40, total_tasks)
  assert_eq(3, sprints_needed)
  assert_eq(6, total_weeks)
  assert_eq(2, tasks_per_person_per_week)
  
  // 验证项目是否能在截止日期前完成
  assert_eq(1, if total_weeks <= project_deadline { 1 } else { 0 })
}

test "enhanced_data_processing_pipeline" {
  // 数据处理流水线测试
  let data_batches = 150
  let batch_size = 12
  let processing_time_per_batch = 2  // 小时
  let quality_check_time = 4  // 小时
  let daily_working_hours = 8
  
  // 计算处理轮次
  let processing_rounds = divide_with_ceil(data_batches, batch_size)
  
  // 计算总处理时间
  let total_processing_time = multiply(processing_rounds, processing_time_per_batch)
  
  // 计算总时间（包括质量检查）
  let total_time = add(total_processing_time, quality_check_time)
  
  // 计算需要的工作日数
  let workdays_needed = divide_with_ceil(total_time, daily_working_hours)
  
  // 计算实际处理的数据量
  let actual_processed = multiply(processing_rounds - 1, batch_size)
  let final_batch_size = subtract(data_batches, actual_processed)
  
  assert_eq(13, processing_rounds)
  assert_eq(26, total_processing_time)
  assert_eq(30, total_time)
  assert_eq(4, workdays_needed)
  assert_eq(6, final_batch_size)
}

test "enhanced_error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 测试极值运算
  let max_int = 2147483647
  let min_int = -2147483648
  
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
  assert_eq(0, multiply(max_int, 0))
  assert_eq(0, multiply(min_int, 0))
  assert_eq(max_int, multiply(max_int, 1))
  assert_eq(min_int, multiply(min_int, 1))
  
  // 测试字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, A!", greet("A"))
}

test "enhanced_complex_business_workflow" {
  // 复杂业务工作流测试：电商订单处理
  let product_price = 120
  let quantity = 15
  let tax_rate = 8  // 8%
  let shipping_cost_per_item = 5
  let discount_threshold = 1000
  let discount_amount = 50
  let packaging_cost_per_box = 10
  let items_per_box = 6
  
  // 步骤1: 计算商品总价
  let subtotal = multiply(product_price, quantity)
  
  // 步骤2: 计算运费
  let total_shipping = multiply(shipping_cost_per_item, quantity)
  
  // 步骤3: 计算包装费用
  let boxes_needed = divide_with_ceil(quantity, items_per_box)
  let packaging_cost = multiply(boxes_needed, packaging_cost_per_box)
  
  // 步骤4: 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 步骤5: 计算折扣前总价
  let pre_discount_total = add(add(add(subtotal, total_shipping), packaging_cost), tax_amount)
  
  // 步骤6: 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  // 步骤7: 计算每件商品的实际成本
  let cost_per_item = divide_with_ceil(final_total, quantity)
  
  // 验证结果
  assert_eq(1800, subtotal)
  assert_eq(75, total_shipping)
  assert_eq(3, boxes_needed)
  assert_eq(30, packaging_cost)
  assert_eq(144, tax_amount)
  assert_eq(2049, pre_discount_total)
  assert_eq(1999, final_total)
  assert_eq(134, cost_per_item)
  
  // 验证折扣已应用
  assert_eq(1, if final_total < pre_discount_total { 1 } else { 0 })
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}