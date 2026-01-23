// 新增的高质量 MoonBit 测试用例
// 针对 azimuth 库的综合测试，包含基础功能、边界情况和实际应用场景

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与最小值相加
  assert_eq(1, add(2147483646, 1))            // 接近最大值
  assert_eq(-1, add(-2147483647, 1))           // 接近最小值
  assert_eq(0, add(1000, -1000))               // 相反数相加
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(0, multiply(2147483647, 0))        // 最大值乘零
  assert_eq(-2147483647, multiply(-1, 2147483647))  // 负一乘最大值
  assert_eq(1, multiply(-1, -1))               // 负数相乘得正
  assert_eq(1000000, multiply(1000, 1000))     // 大数相乘
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(1, divide_with_ceil(1, 100))       // 被除数小于除数
  assert_eq(100, divide_with_ceil(9999, 100))  // 接近整除
  assert_eq(-3, divide_with_ceil(-10, 3))      // 负数除法
  assert_eq(4, divide_with_ceil(-11, -3))      // 负数除负数
  assert_eq(0, divide_with_ceil(0, 100))       // 零除法
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_shopping_cart" {
  // 真实购物车计算场景
  let item1_price = 29
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  let shipping_fee = 12
  let discount = 20
  
  let subtotal = add(multiply(item1_price, item1_quantity), multiply(item2_price, item2_quantity))
  let total_before_discount = add(subtotal, shipping_fee)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(115, final_total)  // 29*2 + 15*3 + 12 - 20 = 58 + 45 + 12 - 20 = 95
}

test "inventory_packaging_optimization" {
  // 库存包装优化计算
  let total_products = 127
  let box_capacity = 15
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, box_capacity)
  let unused_space = add(total_capacity, -total_products)
  
  assert_eq(9, boxes_needed)      // ceil(127/15) = 9
  assert_eq(8, unused_space)      // 9*15 - 127 = 135 - 127 = 8
}

test "time_management_calculation" {
  // 时间管理计算
  let daily_tasks = 6
  let total_tasks = 47
  let work_days_needed = divide_with_ceil(total_tasks, daily_tasks)
  
  // 计算总工作小时数（每天8小时）
  let total_work_hours = multiply(work_days_needed, 8)
  
  assert_eq(8, work_days_needed)    // ceil(47/6) = 8
  assert_eq(64, total_work_hours)   // 8 * 8 = 64
}

test "financial_compound_interest" {
  // 金融复利计算
  let principal = 1000
  let interest_rate = 5
  
  // 简化的年复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, interest_rate), 100))
  
  assert_eq(1050, year1)   // 1000 + ceil(1000*5/100) = 1000 + 50 = 1050
  assert_eq(1103, year2)   // 1050 + ceil(1050*5/100) = 1050 + 53 = 1103
  assert_eq(1158, year3)   // 1103 + ceil(1103*5/100) = 1103 + 55 = 1158
}

test "data_processing_pipeline" {
  // 数据处理管道模拟
  let input_data = 1000
  let error_rate = 2  // 2% 错误率
  
  // 模拟处理步骤
  let step1_result = add(input_data, -divide_with_ceil(multiply(input_data, error_rate), 100))
  let step2_result = add(step1_result, -divide_with_ceil(multiply(step1_result, error_rate), 100))
  let final_result = add(step2_result, -divide_with_ceil(multiply(step2_result, error_rate), 100))
  
  assert_eq(980, step1_result)     // 1000 - ceil(1000*2/100) = 1000 - 20 = 980
  assert_eq(960, step2_result)     // 980 - ceil(980*2/100) = 980 - 20 = 960
  assert_eq(941, final_result)     // 960 - ceil(960*2/100) = 960 - 19 = 941
}