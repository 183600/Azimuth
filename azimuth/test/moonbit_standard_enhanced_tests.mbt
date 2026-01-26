// 标准 MoonBit 增强测试用例
// 为 azimuth 库添加的高质量测试用例

test "subtract_function_basic_operations" {
  // 测试减法函数的基本运算
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
  @azimuth.assert_eq(15, @azimuth.subtract(5, -10))
  @azimuth.assert_eq(-15, @azimuth.subtract(-5, 10))
}

test "subtract_function_zero_handling" {
  // 测试减法函数的零值处理
  @azimuth.assert_eq(10, @azimuth.subtract(10, 0))
  @azimuth.assert_eq(-10, @azimuth.subtract(0, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(0, 0))
  @azimuth.assert_eq(-5, @azimuth.subtract(0, 5))
  @azimuth.assert_eq(5, @azimuth.subtract(0, -5))
}

test "divide_with_ceil_precision_edge_cases" {
  // 测试向上取整除法的精度边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
  @azimuth.assert_eq(1000001, @azimuth.divide_with_ceil(1000000, 1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(1000000, 999999))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(999999, 1000000))
}

test "complex_business_calculation_inventory" {
  // 测试复杂业务计算：库存管理
  let total_products = 1234
  let boxes_per_pallet = 24
  let products_per_box = 12
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_products, products_per_box)
  
  // 计算需要的托盘数量
  let pallets_needed = @azimuth.divide_with_ceil(boxes_needed, boxes_per_pallet)
  
  // 验证计算结果
  @azimuth.assert_eq(103, boxes_needed)  // ceil(1234/12) = 103
  @azimuth.assert_eq(5, pallets_needed)   // ceil(103/24) = 5
  
  // 验证总容量
  let total_box_capacity = @azimuth.multiply(boxes_needed, products_per_box)
  let total_pallet_capacity = @azimuth.multiply(pallets_needed, @azimuth.multiply(boxes_per_pallet, products_per_box))
  
  @azimuth.assert_eq(1236, total_box_capacity)
  @azimuth.assert_eq(1440, total_pallet_capacity)
}

test "mathematical_sequence_fibonacci_style" {
  // 测试数学序列：类似斐波那契数列的计算
  let a = 1
  let b = 1
  
  // 计算类似斐波那契数列的前几项
  let term3 = @azimuth.add(a, b)  // 2
  let term4 = @azimuth.add(b, term3)  // 3
  let term5 = @azimuth.add(term3, term4)  // 5
  let term6 = @azimuth.add(term4, term5)  // 8
  
  // 验证计算结果
  @azimuth.assert_eq(2, term3)
  @azimuth.assert_eq(3, term4)
  @azimuth.assert_eq(5, term5)
  @azimuth.assert_eq(8, term6)
  
  // 测试数列的乘法性质
  let product_test = @azimuth.multiply(term5, term6)
  @azimuth.assert_eq(40, product_test)  // 5 * 8 = 40
}

test "string_processing_multilingual_greetings" {
  // 测试多语言字符串处理
  @azimuth.assert_eq_string("Hello, English!", @azimuth.greet("English"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
  @azimuth.assert_eq_string("Hello, 한국어!", @azimuth.greet("한국어"))
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
  @azimuth.assert_eq_string("Hello, Français!", @azimuth.greet("Français"))
  @azimuth.assert_eq_string("Hello, Русский!", @azimuth.greet("Русский"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
}

test "algorithm_complexity_calculation_sorting_simulation" {
  // 测试算法复杂度计算：排序算法模拟
  let array_size = 100
  
  // 模拟O(n²)排序算法的比较次数
  let bubble_sort_comparisons = @azimuth.multiply(array_size, @azimuth.subtract(array_size, 1))
  @azimuth.assert_eq(9900, bubble_sort_comparisons)  // 100 * 99
  
  // 模拟O(n log n)排序算法的比较次数（简化计算）
  let merge_sort_comparisons = @azimuth.multiply(array_size, @azimuth.divide_with_ceil(100, 10))
  @azimuth.assert_eq(1000, merge_sort_comparisons)  // 100 * 10
  
  // 模拟O(n)排序算法的比较次数
  let counting_sort_comparisons = array_size
  @azimuth.assert_eq(100, counting_sort_comparisons)
  
  // 验证算法效率差异
  let efficiency_ratio = @azimuth.divide_with_ceil(bubble_sort_comparisons, merge_sort_comparisons)
  @azimuth.assert_eq(10, efficiency_ratio)  // ceil(9900/1000) = 10
}

test "financial_compound_interest_calculation" {
  // 测试金融复利计算
  let principal = 10000  // 本金
  let annual_rate = 5     // 年利率5%
  let years = 3           // 投资年限
  
  // 计算每年的复利（简化版本，不使用幂运算）
  let year1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100)
  let year1_total = @azimuth.add(principal, year1_interest)
  
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year1_total, annual_rate), 100)
  let year2_total = @azimuth.add(year1_total, year2_interest)
  
  let year3_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year2_total, annual_rate), 100)
  let year3_total = @azimuth.add(year2_total, year3_interest)
  
  // 验证复利计算结果
  @azimuth.assert_eq(500, year1_interest)  // 10000 * 5%
  @azimuth.assert_eq(10500, year1_total)
  @azimuth.assert_eq(525, year2_interest)  // 10500 * 5%
  @azimuth.assert_eq(11025, year2_total)
  @azimuth.assert_eq(552, year3_interest)  // 11025 * 5%（向上取整）
  @azimuth.assert_eq(11577, year3_total)
  
  // 计算总收益
  let total_profit = @azimuth.subtract(year3_total, principal)
  @azimuth.assert_eq(1577, total_profit)
}

test "resource_allocation_server_load_balancing" {
  // 测试资源分配：服务器负载均衡
  let total_requests = 10000
  let servers = 8
  let max_requests_per_server = 1500
  
  // 计算每个服务器应该处理的请求数
  let requests_per_server = @azimuth.divide_with_ceil(total_requests, servers)
  
  // 验证负载分配
  @azimuth.assert_eq(1250, requests_per_server)  // ceil(10000/8) = 1250
  
  // 计算实际处理能力
  let total_capacity = @azimuth.multiply(requests_per_server, servers)
  @azimuth.assert_eq(10000, total_capacity)
  
  // 检查是否超过每台服务器的最大处理能力
  let within_capacity = requests_per_server <= max_requests_per_server
  @azimuth.assert_true(within_capacity)
  
  // 计算剩余处理能力
  let remaining_capacity_per_server = @azimuth.subtract(max_requests_per_server, requests_per_server)
  let total_remaining_capacity = @azimuth.multiply(remaining_capacity_per_server, servers)
  
  @azimuth.assert_eq(250, remaining_capacity_per_server)
  @azimuth.assert_eq(2000, total_remaining_capacity)
}

test "temperature_conversion_arithmetic" {
  // 测试温度转换的算术运算
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let celsius_0 = 0
  let celsius_100 = 100
  let celsius_minus40 = -40
  
  // 转换为华氏度（简化计算，使用整数运算）
  let fahrenheit_0 = @azimuth.add(@azimuth.divide_with_ceil(@azimuth.multiply(celsius_0, 9), 5), 32)
  let fahrenheit_100 = @azimuth.add(@azimuth.divide_with_ceil(@azimuth.multiply(celsius_100, 9), 5), 32)
  let fahrenheit_minus40 = @azimuth.add(@azimuth.divide_with_ceil(@azimuth.multiply(celsius_minus40, 9), 5), 32)
  
  // 验证转换结果
  @azimuth.assert_eq(32, fahrenheit_0)      // 0°C = 32°F
  @azimuth.assert_eq(212, fahrenheit_100)   // 100°C = 212°F
  @azimuth.assert_eq(-40, fahrenheit_minus40) // -40°C = -40°F（特殊点）
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let celsius_back_0 = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.subtract(fahrenheit_0, 32), 5), 9)
  let celsius_back_100 = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.subtract(fahrenheit_100, 32), 5), 9)
  let celsius_back_minus40 = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.subtract(fahrenheit_minus40, 32), 5), 9)
  
  // 验证反向转换
  @azimuth.assert_eq(0, celsius_back_0)
  @azimuth.assert_eq(100, celsius_back_100)
  @azimuth.assert_eq(-40, celsius_back_minus40)
}