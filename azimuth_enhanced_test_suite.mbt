// Enhanced MoonBit Test Suite
// 高质量 MoonBit 测试套件 - 8个专业测试用例

// 导入 azimuth 库
use azimuth.*

test "string_concatenation_and_validation" {
  // 字符串连接和验证测试
  let input1 = "MoonBit"
  let input2 = "Programming"
  let input3 = ""
  
  // 测试字符串拼接功能
  let result1 = greet(input1)
  let result2 = greet(input2)
  let result3 = greet(input3)
  
  // 验证结果
  assert_eq_string(result1, "Hello, MoonBit!")
  assert_eq_string(result2, "Hello, Programming!")
  assert_eq_string(result3, "Hello, !")
  
  // 验证字符串长度
  assert_eq(result1.length(), 14)
  assert_eq(result2.length(), 19)
  assert_eq(result3.length(), 8)
}

test "arithmetic_precision_and_accuracy" {
  // 算术精度和准确性测试
  let a = 1000
  let b = 3
  let c = 7
  let d = 2
  
  // 测试复合运算的精度
  let result1 = divide_with_ceil(multiply(a, b), 10)
  let result2 = divide_with_ceil(add(multiply(a, b), multiply(a, c)), 100)
  let result3 = divide_with_ceil(subtract(multiply(a, d), b), 5)
  
  // 验证结果
  assert_eq(result1, 300)
  assert_eq(result2, 100)
  assert_eq(result3, 399)
  
  // 测试负数精度
  let negative_result = divide_with_ceil(multiply(-100, 3), 7)
  assert_eq(negative_result, -42)
}

test "edge_case_boundary_conditions" {
  // 边界条件和极端情况测试
  let zero = 0
  let one = 1
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试零值运算
  assert_eq(add(zero, zero), zero)
  assert_eq(multiply(zero, max_int), zero)
  assert_eq(divide_with_ceil(zero, one), zero)
  assert_eq(divide_with_ceil(zero, zero), zero)
  
  // 测试单位值运算
  assert_eq(add(max_int, zero), max_int)
  assert_eq(multiply(one, max_int), max_int)
  assert_eq(divide_with_ceil(max_int, one), max_int)
  
  // 测试边界减法
  let boundary_sub = subtract(max_int, min_int)
  assert_eq(boundary_sub > 0, true)
}

test "mathematical_series_and_progressions" {
  // 数学数列和级数测试
  // 等差数列: 3, 7, 11, 15, 19...
  let first_term = 3
  let common_diff = 4
  let n_terms = 5
  
  // 计算等差数列各项
  let term1 = first_term
  let term2 = add(term1, common_diff)
  let term3 = add(term2, common_diff)
  let term4 = add(term3, common_diff)
  let term5 = add(term4, common_diff)
  
  // 计算等差数列和
  let series_sum = add(add(add(add(term1, term2), term3), term4), term5)
  
  // 验证结果
  assert_eq(term1, 3)
  assert_eq(term2, 7)
  assert_eq(term3, 11)
  assert_eq(term4, 15)
  assert_eq(term5, 19)
  assert_eq(series_sum, 55)
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(series_sum, n_terms)
  assert_eq(average, 11)
}

test "business_calculation_scenario" {
  // 商业计算场景测试
  let product_cost = 150
  let markup_percentage = 20
  let tax_rate = 8
  let quantity = 25
  
  // 计算加价后的价格
  let markup_amount = divide_with_ceil(multiply(product_cost, markup_percentage), 100)
  let selling_price = add(product_cost, markup_amount)
  
  // 计算含税价格
  let tax_amount = divide_with_ceil(multiply(selling_price, tax_rate), 100)
  let final_price = add(selling_price, tax_amount)
  
  // 计算总收入
  let total_revenue = multiply(final_price, quantity)
  
  // 计算利润
  let total_cost = multiply(product_cost, quantity)
  let total_profit = subtract(total_revenue, total_cost)
  
  // 计算利润率
  let profit_margin = divide_with_ceil(multiply(total_profit, 100), total_revenue)
  
  assert_eq(markup_amount, 30)
  assert_eq(selling_price, 180)
  assert_eq(tax_amount, 15)
  assert_eq(final_price, 195)
  assert_eq(total_revenue, 4875)
  assert_eq(total_cost, 3750)
  assert_eq(total_profit, 1125)
  assert_eq(profit_margin, 24)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let input_size = 100
  
  // O(1) 常数时间复杂度
  let constant_time = 5
  
  // O(n) 线性时间复杂度
  let linear_time = multiply(input_size, 2)
  
  // O(n²) 平方时间复杂度
  let quadratic_time = multiply(input_size, input_size)
  
  // O(log n) 对数时间复杂度（简化计算）
  let log_time = divide_with_ceil(input_size, 10)
  
  // O(n log n) 线性对数时间复杂度
  let linear_log_time = multiply(input_size, log_time)
  
  // 计算总复杂度
  let total_complexity = add(add(add(add(constant_time, linear_time), quadratic_time), log_time), linear_log_time)
  
  // 计算各复杂度的占比
  let linear_percentage = divide_with_ceil(multiply(linear_time, 100), total_complexity)
  let quadratic_percentage = divide_with_ceil(multiply(quadratic_time, 100), total_complexity)
  
  assert_eq(constant_time, 5)
  assert_eq(linear_time, 200)
  assert_eq(quadratic_time, 10000)
  assert_eq(log_time, 10)
  assert_eq(linear_log_time, 1000)
  assert_eq(total_complexity, 11215)
  assert_eq(linear_percentage, 2)
  assert_eq(quadratic_percentage, 89)
}

test "data_structure_operations" {
  // 数据结构操作模拟测试
  // 模拟数组操作
  let array_size = 10
  let initial_value = 5
  let increment = 3
  
  // 模拟数组初始化
  let total_initial = multiply(array_size, initial_value)
  
  // 模拟数组遍历和更新
  let total_increment = multiply(array_size, increment)
  let total_updated = add(total_initial, total_increment)
  
  // 模拟查找操作（假设查找3次）
  let search_operations = multiply(3, divide_with_ceil(array_size, 2))
  
  // 模拟排序操作（简化为O(n²)）
  let sort_operations = multiply(array_size, array_size)
  
  // 计算总操作数
  let total_operations = add(add(add(total_updated, search_operations), sort_operations), 100)
  
  // 计算平均每个元素的操作数
  let ops_per_element = divide_with_ceil(total_operations, array_size)
  
  assert_eq(total_initial, 50)
  assert_eq(total_increment, 30)
  assert_eq(total_updated, 80)
  assert_eq(search_operations, 15)
  assert_eq(sort_operations, 100)
  assert_eq(total_operations, 295)
  assert_eq(ops_per_element, 30)
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  let normal_input = 42
  let zero_input = 0
  let negative_input = -15
  let large_input = 1000000
  
  // 正常情况处理
  let normal_result = divide_with_ceil(multiply(normal_input, 2), 3)
  assert_eq(normal_result, 28)
  
  // 零值处理
  let zero_dividend = divide_with_ceil(zero_input, 5)
  let zero_divisor = divide_with_ceil(100, zero_input)
  let zero_both = divide_with_ceil(zero_input, zero_input)
  assert_eq(zero_dividend, 0)
  assert_eq(zero_divisor, 0)
  assert_eq(zero_both, 0)
  
  // 负数处理
  let negative_result = divide_with_ceil(multiply(negative_input, 3), 4)
  assert_eq(negative_result, -11)
  
  // 大数处理
  let large_result = divide_with_ceil(multiply(large_input, 2), 3)
  assert_eq(large_result, 666667)
  
  // 边界组合测试
  let boundary_test1 = add(subtract(normal_input, negative_input), large_input)
  let boundary_test2 = multiply(normal_input, divide_with_ceil(large_input, 10000))
  
  assert_eq(boundary_test1, 1000057)
  assert_eq(boundary_test2, 420000)
}