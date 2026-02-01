// 标准 MoonBit 测试用例
// 针对 azimuth 库的全面测试，包含核心功能和实际应用场景

test "add_function_comprehensive" {
  // 测试加法函数的综合情况
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
  @azimuth.assert_eq(-1000, @azimuth.add(-500, -500))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(10000, @azimuth.multiply(100, 100))
}

test "subtract_function_comprehensive" {
  // 测试减法函数的综合情况
  @azimuth.assert_eq(2, @azimuth.subtract(8, 6))
  @azimuth.assert_eq(-2, @azimuth.subtract(6, 8))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(5, -5))
  @azimuth.assert_eq(-10, @azimuth.subtract(-5, 5))
}

test "greet_function_comprehensive" {
  // 测试问候函数的综合情况
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的综合情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
}

test "mathematical_properties_verification" {
  // 测试数学性质的验证
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
  
  // 单位元性质
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  let product_price = 45
  let quantity = 12
  let discount = 20
  let tax_rate = 8
  
  // 计算小计
  let subtotal = @azimuth.multiply(product_price, quantity)
  
  // 计算折扣
  let discounted_total = @azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(discounted_total, tax_rate), 100)
  
  // 计算最终总价
  let final_total = @azimuth.add(discounted_total, tax)
  
  @azimuth.assert_eq(540, subtotal)
  @azimuth.assert_eq(520, discounted_total)
  @azimuth.assert_eq(42, tax)
  @azimuth.assert_eq(562, final_total)
}

test "packaging_and_shipping_calculation" {
  // 测试包装和运输计算场景
  let total_items = 87
  let items_per_box = 15
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  let shipping_cost_per_box = 8
  let total_shipping = @azimuth.multiply(boxes_needed, shipping_cost_per_box)
  
  @azimuth.assert_eq(6, boxes_needed)
  @azimuth.assert_eq(48, total_shipping)
}

test "time_and_resource_calculation" {
  // 测试时间和资源计算场景
  let tasks_per_day = 7
  let total_tasks = 50
  let workdays_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  let hours_per_day = 8
  let total_work_hours = @azimuth.multiply(workdays_needed, hours_per_day)
  
  @azimuth.assert_eq(8, workdays_needed)
  @azimuth.assert_eq(64, total_work_hours)
}

test "data_analysis_scenario" {
  // 测试数据分析场景
  let data_points = @azimuth.multiply(15, 4)  // 15个数据集，每个4个数据点
  let processed_data = @azimuth.divide_with_ceil(data_points, 10)  // 每批处理10个数据点
  let remaining_data = @azimuth.add(data_points, -@azimuth.multiply(processed_data, 10))
  
  @azimuth.assert_eq(60, data_points)
  @azimuth.assert_eq(6, processed_data)
  @azimuth.assert_eq(0, remaining_data)
}