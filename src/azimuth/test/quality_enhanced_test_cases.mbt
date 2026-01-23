// 简化的高质量测试用例
// 针对 azimuth 库的核心功能进行实际应用场景测试

test "simple_financial_calculation" {
  // 简单的金融计算测试
  let principal = 1000
  let interest_rate = 5
  
  // 计算一年的利息
  let interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, interest_rate), 100)
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(1050, total)
}

test "simple_packaging_calculation" {
  // 简单的包装计算测试
  let total_items = 25
  let items_per_box = 6
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(5, boxes_needed)
}

test "simple_area_calculation" {
  // 简单的面积计算测试
  let length = 10
  let width = 5
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  
  @azimuth.assert_eq(50, area)
}

test "simple_greeting_test" {
  // 简单的问候测试
  let name = "MoonBit"
  let greeting = @azimuth.greet(name)
  
  @azimuth.assert_eq_string("Hello, MoonBit!", greeting)
}

test "simple_batch_processing" {
  // 简单的批处理测试
  let total_items = 85
  let batch_size = 20
  
  // 计算批次数
  let batches = @azimuth.divide_with_ceil(total_items, batch_size)
  
  @azimuth.assert_eq(5, batches)
}

test "simple_percentage_calculation" {
  // 简单的百分比计算测试
  let total = 200
  let percentage = 15
  
  // 计算百分比值
  let result = @azimuth.divide_with_ceil(@azimuth.multiply(total, percentage), 100)
  
  @azimuth.assert_eq(30, result)
}

test "simple_average_calculation" {
  // 简单的平均值计算测试
  let value1 = 85
  let value2 = 90
  let value3 = 95
  
  // 计算平均值
  let sum = @azimuth.add(@azimuth.add(value1, value2), value3)
  let average = @azimuth.divide_with_ceil(sum, 3)
  
  @azimuth.assert_eq(90, average)
}

test "simple_discount_calculation" {
  // 简单的折扣计算测试
  let original_price = 120
  let discount_percentage = 20
  
  // 计算折扣后的价格
  let discount_amount = @azimuth.divide_with_ceil(@azimuth.multiply(original_price, discount_percentage), 100)
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(96, final_price)
}