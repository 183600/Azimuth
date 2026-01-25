// 额外的高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行补充测试

// 测试 1: 复杂数学表达式计算
test "complex_mathematical_expression" {
  // 测试嵌套数学表达式: (a + b) * (c - d) / e
  let a = 15
  let b = 25
  let c = 100
  let d = 40
  let e = 6
  
  let step1 = add(a, b)  // 15 + 25 = 40
  let step2 = add(c, -d)  // 100 - 40 = 60
  let step3 = multiply(step1, step2)  // 40 * 60 = 2400
  let result = divide_with_ceil(step3, e)  // ceil(2400/6) = 400
  
  assert_eq(400, result)
}

// 测试 2: 边界值和极限情况
test "boundary_and极限_values" {
  // 测试零的边界情况
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(0, add(0, 0))
  
  // 测试大数运算
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
  
  // 测试除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1000))
  assert_eq(0, divide_with_ceil(1000, 0))
}

// 测试 3: 字符串处理和特殊字符
test "string_processing_special_chars" {
  // 测试各种特殊字符和Unicode
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, éàèêë!", greet("éàèêë"))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
}

// 测试 4: 实际业务场景 - 订单处理
test "business_order_processing" {
  // 模拟电商订单处理
  let unit_price = 89
  let quantity = 7
  let tax_rate = 8  // 8%
  let shipping_cost = 25
  let discount_threshold = 500
  let discount_amount = 50
  
  // 计算商品总价
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_total = add(add(subtotal, tax), shipping_cost)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_total > discount_threshold {
    add(pre_total, -discount_amount)
  } else {
    pre_total
  }
  
  assert_eq(623, subtotal)
  assert_eq(50, tax)
  assert_eq(698, pre_total)
  assert_eq(648, final_total)
}

// 测试 5: 数据分析和统计
test "data_analysis_statistics" {
  // 模拟销售数据分析
  let sales_data = [12000, 15000, 18000, 22000, 25000, 28000]
  let total_sales = add(add(add(add(add(sales_data[0], sales_data[1]), sales_data[2]), sales_data[3]), sales_data[4]), sales_data[5])
  
  // 计算平均值
  let average = divide_with_ceil(total_sales, 6)
  
  // 计算增长率
  let growth_rate = divide_with_ceil(multiply(sales_data[5] - sales_data[0], 100), sales_data[0])
  
  assert_eq(120000, total_sales)
  assert_eq(20000, average)
  assert_eq(134, growth_rate)
}

// 测试 6: 算法模拟 - 排序和搜索
test "algorithm_simulation" {
  // 模拟冒泡排序的比较次数
  let array_size = 10
  let comparisons_per_pass = add(array_size, -1)
  let total_comparisons = multiply(comparisons_per_pass, divide_with_ceil(array_size, 2))
  
  // 模拟二分查找的最大比较次数
  let max_binary_search_comparisons = divide_with_ceil(
    multiply(2, array_size), 
    100
  )
  
  assert_eq(45, total_comparisons)
  assert_eq(1, max_binary_search_comparisons)
}

// 测试 7: 工程计算
test "engineering_calculation" {
  // 模拟建筑工程计算
  let building_length = 50  // 米
  let building_width = 30   // 米
  let floor_height = 3.5    // 米（向上取整为4）
  let floors = 8
  
  // 计算建筑面积
  let floor_area = multiply(building_length, building_width)
  
  // 计算总建筑体积
  let total_volume = multiply(floor_area, multiply(floors, 4))
  
  // 计算需要的建筑材料（假设每立方米需要2单位材料）
  let materials_needed = multiply(total_volume, 2)
  
  assert_eq(1500, floor_area)
  assert_eq(48000, total_volume)
  assert_eq(96000, materials_needed)
}

// 测试 8: 金融计算
test "financial_calculation" {
  // 模拟复利计算
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let year1_balance = add(principal, year1_interest)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_balance, annual_rate), 100)
  let year2_balance = add(year1_balance, year2_interest)
  
  // 第三年
  let year3_interest = divide_with_ceil(multiply(year2_balance, annual_rate), 100)
  let final_balance = add(year2_balance, year3_interest)
  
  assert_eq(500, year1_interest)
  assert_eq(10500, year1_balance)
  assert_eq(525, year2_interest)
  assert_eq(11025, year2_balance)
  assert_eq(552, year3_interest)
  assert_eq(11577, final_balance)
}

// 测试 9: 性能基准测试
test "performance_benchmark" {
  // 模拟性能基准测试
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  
  // 计算优化后的操作数
  let optimized_ops = multiply(base_operations, optimization_factor)
  
  // 计算开销
  let overhead = divide_with_ceil(multiply(optimized_ops, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = add(optimized_ops, -overhead)
  
  // 计算性能提升倍数
  let performance_gain = divide_with_ceil(actual_performance, base_operations)
  
  assert_eq(3000, optimized_ops)
  assert_eq(450, overhead)
  assert_eq(2550, actual_performance)
  assert_eq(3, performance_gain)
}

// 测试 10: 错误处理和健壮性
test "error_handling_robustness" {
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极端数值
  let extreme_positive = 2147483647
  let extreme_negative = -2147483648
  
  // 测试极端值的加法
  let extreme_add = add(extreme_positive, -1)
  assert_eq(2147483646, extreme_add)
  
  // 测试极端值的乘法（小数值）
  let extreme_multiply = multiply(extreme_negative, 0)
  assert_eq(0, extreme_multiply)
  
  // 测试空字符串和特殊输入
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, null!", greet("null"))
}