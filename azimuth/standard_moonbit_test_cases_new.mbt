// 标准 MoonBit 测试用例 - 为 azimuth 库添加10个高质量测试
// 使用标准的 MoonBit 测试语法

test "add_function_boundary_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  assert_eq(1, add(-100, 101))
  assert_eq(0, add(-50, 50))
}

test "multiply_function_boundary_cases" {
  // 测试乘法函数的边界情况
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(2147483647, multiply(1, 2147483647))
  assert_eq(-2147483647, multiply(-1, 2147483647))
  assert_eq(1, multiply(-1, -1))
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_negative_handling" {
  // 测试向上取整除法的负数处理
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(4, divide_with_ceil(-11, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let x = 10
  let y = 15
  let z = 20
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(500, result1)
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(170, result2)
  
  // 测试复合运算
  let result3 = add(divide_with_ceil(multiply(x, y), z), multiply(2, 3))
  assert_eq(11, result3)
}

test "inventory_management_business_scenario" {
  // 测试库存管理业务场景
  let product_a_qty = 120
  let product_b_qty = 85
  let product_c_qty = 200
  let box_capacity = 24
  
  // 计算每种产品需要的箱子数
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  assert_eq(5, boxes_a)   // ceil(120/24) = 5
  assert_eq(4, boxes_b)   // ceil(85/24) = 4
  assert_eq(9, boxes_c)   // ceil(200/24) = 9
  assert_eq(18, total_boxes) // 5 + 4 + 9 = 18
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 42
  let b = 17
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 测试多组数值
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
  assert_eq(add(-5, 10), add(10, -5))
  assert_eq(multiply(-5, 10), multiply(10, -5))
}

test "error_handling_zero_division" {
  // 测试错误处理：除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
}

test "large_number_operations" {
  // 测试大数运算
  let large_num = 100000
  
  // 大数加法
  assert_eq(200000, add(large_num, large_num))
  
  // 大数乘法
  assert_eq(10000000000, multiply(large_num, large_num))
  
  // 大数除法
  assert_eq(1, divide_with_ceil(large_num, large_num))
  assert_eq(2, divide_with_ceil(add(large_num, 1), large_num))
  assert_eq(100, divide_with_ceil(large_num, 1000))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目成本和时间
  
  let team_size = 8
  let hourly_rate = 50
  let hours_per_day = 8
  let project_days = 15
  let overhead_percentage = 20
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(hours_per_day, project_days))
  
  // 计算人力成本
  let labor_cost = multiply(total_hours, hourly_rate)
  
  // 计算管理费用
  let overhead_cost = divide_with_ceil(multiply(labor_cost, overhead_percentage), 100)
  
  // 计算总成本
  let total_cost = add(labor_cost, overhead_cost)
  
  // 计算需要的支付周期（每周期支付不超过10000）
  let payment_cycle_limit = 10000
  let payment_cycles = divide_with_ceil(total_cost, payment_cycle_limit)
  
  assert_eq(960, total_hours)     // 8 * 8 * 15 = 960
  assert_eq(48000, labor_cost)    // 960 * 50 = 48000
  assert_eq(9600, overhead_cost)  // ceil(48000 * 20 / 100) = 9600
  assert_eq(57600, total_cost)    // 48000 + 9600 = 57600
  assert_eq(6, payment_cycles)    // ceil(57600 / 10000) = 6
}