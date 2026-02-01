// 标准 Azimuth 测试用例
// 使用标准 MoonBit 测试语法编写的高质量测试用例
// 覆盖 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 8
  let y = 12
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(0, multiply(x, 0))
  assert_eq(x, add(x, 0))
}

test "complex_business_calculations" {
  // 复杂业务计算测试
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_and_logistics" {
  // 包装和物流计算测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "financial_calculations" {
  // 金融计算测试
  let principal = 1000
  let rate_percent = 8
  let years = 2
  
  // 简化的复利计算
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  
  assert_eq(1080, year1_amount)
  assert_eq(1167, year2_amount)
}

test "data_analysis_scenario" {
  // 数据分析场景测试
  let data_points = 250
  let sample_size = 30
  let batches_needed = divide_with_ceil(data_points, sample_size)
  
  assert_eq(9, batches_needed)
  
  // 计算实际处理的样本数
  let full_batches = batches_needed - 1
  let samples_in_full_batches = multiply(full_batches, sample_size)
  let final_batch_size = subtract(data_points, samples_in_full_batches)
  
  assert_eq(240, samples_in_full_batches)
  assert_eq(10, final_batch_size)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(0, multiply(2147483647, 0))
}

test "string_processing_international" {
  // 字符串处理国际化测试
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, François!", greet("François"))
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, Ñoño!", greet("Ñoño"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
}