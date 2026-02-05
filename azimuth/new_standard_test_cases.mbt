// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含 10 个高质量测试用例，覆盖核心功能的各个方面

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}

test "distributive_property_validation" {
  // 测试乘法分配律
  let a = 6
  let b = 4
  let c = 3
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
  assert_eq(42, left_side)
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正常情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 零除数处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 零被除数
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000, divide_with_ceil(1000, 1))
}

test "subtract_function_properties" {
  // 测试减法函数性质
  let x = 25
  let y = 15
  
  // 基本减法
  assert_eq(10, subtract(x, y))
  assert_eq(-10, subtract(y, x))
  
  // 自减性
  assert_eq(0, subtract(x, x))
  assert_eq(0, subtract(y, y))
  
  // 零元素性质
  assert_eq(x, subtract(x, 0))
}

test "greet_function_international" {
  // 测试问候函数的国际化字符处理
  // 基本英文字符
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 中文字符
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  
  // Emoji字符
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  
  // 空字符串和空格
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "warehouse_inventory_scenario" {
  // 仓库库存管理业务场景测试
  let total_products = 487
  let box_capacity = 24
  let pallet_capacity = 12
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(21, boxes_needed)
  
  // 计算总容量
  let total_box_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(504, total_box_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_box_capacity, total_products)
  assert_eq(17, remaining_space)
  
  // 计算需要的托盘数量
  let pallets_needed = divide_with_ceil(boxes_needed, pallet_capacity)
  assert_eq(2, pallets_needed)
}

test "mathematical_identities" {
  // 数学恒等式验证测试
  let x = 42
  let y = 17
  
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
  
  // 负一恒等式
  assert_eq(multiply(x, -1), multiply(-1, x))
  assert_eq(add(x, multiply(x, -1)), 0)
}

test "large_number_operations" {
  // 大数运算测试
  let large_num1 = 10000
  let large_num2 = 9999
  
  // 大数加法
  assert_eq(19999, add(large_num1, large_num2))
  assert_eq(20000, add(large_num1, large_num1))
  
  // 大数乘法（调整到安全范围内）
  assert_eq(99990000, multiply(large_num1, large_num2 - 1))
  assert_eq(100000000, multiply(large_num1, large_num1))
  
  // 大数除法
  assert_eq(2, divide_with_ceil(large_num1, large_num2))
  assert_eq(1, divide_with_ceil(large_num2, large_num1))
  assert_eq(10000, divide_with_ceil(multiply(large_num1, large_num1), large_num1))
  
  // 大数减法
  assert_eq(1, subtract(large_num1, large_num2))
  assert_eq(0, subtract(large_num1, large_num1))
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算
  let max_positive = 100000
  let max_negative = -100000
  
  // 零乘法测试
  assert_eq(0, multiply(max_positive, 0))
  assert_eq(0, multiply(max_negative, 0))
  
  // 零加法测试
  assert_eq(max_positive, add(max_positive, 0))
  assert_eq(max_negative, add(max_negative, 0))
  
  // 相等数相减
  assert_eq(0, subtract(max_positive, max_positive))
  assert_eq(0, subtract(max_negative, max_negative))
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试 - 项目预算管理
  let team_members = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_duration_days = 15
  let budget_limit = 100000
  let payment_threshold = 25000
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_duration_days))
  assert_eq(960, total_hours)
  
  // 计算总成本
  let total_cost = multiply(total_hours, hourly_rate)
  assert_eq(72000, total_cost)
  
  // 计算剩余预算
  let remaining_budget = subtract(budget_limit, total_cost)
  assert_eq(28000, remaining_budget)
  
  // 计算需要的支付批次数
  let payment_batches = divide_with_ceil(total_cost, payment_threshold)
  assert_eq(3, payment_batches)
  
  // 计算每批平均支付金额
  let average_payment = divide_with_ceil(total_cost, payment_batches)
  assert_eq(24000, average_payment)
  
  // 计算项目效率指标
  let cost_per_hour = divide_with_ceil(total_cost, total_hours)
  assert_eq(75, cost_per_hour)
  
  let cost_per_person_per_day = divide_with_ceil(total_cost, multiply(team_members, project_duration_days))
  assert_eq(600, cost_per_person_per_day)
}