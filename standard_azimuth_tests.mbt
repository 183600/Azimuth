// 标准 MoonBit 测试用例 - 针对 Azimuth 库
// 使用标准的 MoonBit 测试语法编写

test "standard_add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  // 测试包含负数的情况
  let x = -10
  let y = 25
  assert_eq(add(x, y), add(y, x))
}

test "standard_multiply_identity_property" {
  // 测试乘法单位元性质：a * 1 = a
  assert_eq(42, multiply(42, 1))
  assert_eq(-17, multiply(-17, 1))
  assert_eq(0, multiply(0, 1))
  
  // 测试乘法零元性质：a * 0 = 0
  assert_eq(0, multiply(100, 0))
  assert_eq(0, multiply(-50, 0))
}

test "standard_divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(2, divide_with_ceil(4, 3))   // 1.333... -> 2
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  
  // 测试负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为0时返回0
}

test "standard_greet_multilingual_support" {
  // 测试多语言问候功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  
  // 测试特殊字符
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, @user!", greet("@user"))
}

test "standard_combined_arithmetic_operations" {
  // 测试组合算术运算
  let x = 8
  let y = 12
  let z = 5
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(100, result1)  // (8 + 12) * 5 = 20 * 5 = 100
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(101, result2)  // 8 * 12 + 5 = 96 + 5 = 101
  
  // 测试复杂表达式
  let complex = add(multiply(divide_with_ceil(25, 4), 3), multiply(2, 7))
  assert_eq(29, complex)  // ceil(25/4) * 3 + 2 * 7 = 7 * 3 + 14 = 21 + 14 = 35
}

test "standard_business_calculation_scenario" {
  // 测试实际业务计算场景
  let unit_price = 45
  let quantity = 8
  let tax_rate = 10  // 10%
  let shipping = 15
  
  // 计算商品总价
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算最终总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(360, subtotal)
  assert_eq(36, tax)
  assert_eq(411, total)
}

test "standard_packaging_and_shipping_calculation" {
  // 测试包装和运输计算
  let total_items = 67
  let items_per_box = 12
  let weight_per_item = 2  // kg
  let max_weight_per_box = 25  // kg
  
  // 计算需要的箱子数量（基于物品数量）
  let boxes_by_count = divide_with_ceil(total_items, items_per_box)
  
  // 计算总重量
  let total_weight = multiply(total_items, weight_per_item)
  
  // 计算基于重量的箱子数量
  let boxes_by_weight = divide_with_ceil(total_weight, max_weight_per_box)
  
  // 取两者中的较大值
  let final_boxes = if boxes_by_count > boxes_by_weight {
    boxes_by_count
  } else {
    boxes_by_weight
  }
  
  assert_eq(6, boxes_by_count)  // ceil(67/12) = 6
  assert_eq(134, total_weight)  // 67 * 2 = 134
  assert_eq(6, boxes_by_weight)  // ceil(134/25) = 6
  assert_eq(6, final_boxes)
}

test "standard_time_and_resource_management" {
  // 测试时间和资源管理
  let total_tasks = 47
  let tasks_per_day = 8
  let working_days_per_week = 5
  
  // 计算需要的工作天数
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, working_days_per_week)
  
  // 计算总工作小时数（假设每天8小时）
  let total_hours = multiply(work_days_needed, 8)
  
  assert_eq(6, work_days_needed)  // ceil(47/8) = 6
  assert_eq(2, weeks_needed)      // ceil(6/5) = 2
  assert_eq(48, total_hours)      // 6 * 8 = 48
}

test "standard_financial_compound_interest" {
  // 测试复利计算
  let principal = 1000
  let interest_rate = 5  // 5%
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, interest_rate), 100))
  
  assert_eq(1050, year1)  // 1000 + 50
  assert_eq(1103, year2)  // 1050 + ceil(1050*5/100) = 1050 + 53
  assert_eq(1158, year3)  // 1103 + ceil(1103*5/100) = 1103 + 55
}

test "standard_data_analysis_statistics" {
  // 测试数据分析统计
  let data_values = [15, 23, 18, 31, 27, 19, 22]
  let count = 7
  
  // 计算总和（手动累加）
  let sum = add(add(add(add(add(add(15, 23), 18), 31), 27), 19), 22)
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum, count)
  
  // 计算最大值和最小值的差
  let max_value = 31
  let min_value = 15
  let range = add(max_value, -min_value)
  
  assert_eq(155, sum)
  assert_eq(23, average)  // ceil(155/7) = 23
  assert_eq(16, range)    // 31 - 15 = 16
}